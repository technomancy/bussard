-- This is the file launched in a new thread for each user session on login.

local lume = require("lume")
local utf8 = require("polywell.utf8")
local shell = require("os.orb.shell")
local rpcs = require("os.orb.rpcs")
local fs = require("os.orb.fs")

local env, command, stdin, output, hostname = ...

local function completions_for(input, dir, prefixes)
   local input_parts = lume.split(input, "/")
   if(#input_parts == 1) then
      local matches = {}
      for _,v in pairs(fs.ls(dir, "/")) do
         if(fs.exists(v, dir) and utf8.sub(v, 1, #input) == input) then
            local parts = lume.clone(prefixes)
            table.insert(parts, v)
            if(fs.isdir(v, dir)) then table.insert(parts, "") end
            table.insert(matches, table.concat(parts, "/"))
         end
      end
      return matches
   else
      local first_part = table.remove(input_parts, 1)
      table.insert(prefixes, first_part)
      return completions_for(table.concat(input_parts, "/"),
                             dir .. "/" .. first_part, prefixes)
   end
end

local function read()
   local msg = stdin:demand()
   if(msg.op == "stdin") then
      return msg.stdin
   elseif(msg.op == "complete") then
      -- we are turning read into a generic RPC dispatch point, which we
      -- will likely turn out to regret! but it's our only entry point now.
      local ok, err = pcall(function()
            local dir = msg.input:match("^/") and "" or env.CWD
            local completions = completions_for(msg.input, dir, {})
            output:push({op="rpc", fn="completions",
                         args=lume.serialize({completions, msg.input})})
      end)
      if(not ok) then
         print("OS handler error:", err)
         output:push({op="status", status="err", out=err})
      end
   elseif(msg.op == "kill") then
      return nil
   else
      print("Unknown op!", lume.serialize(msg))
   end
   return read()
end

local write = function(...)
   local out = table.concat(lume.map({...}, tostring), " ")
   output:push({ op = "stdout", out = out })
end

local add_rpc = function(sandbox, name)
   sandbox[name] = function(...)
      local chan = love.thread.newChannel()
      output:push({op="rpc", fn=name, args=lume.serialize({...}), chan=chan})
      local response = chan:demand()
      if(response[1] == "_error") then
         table.remove(response, 1)
         error(unpack(response))
      else
         return unpack(response)
      end
   end
   return sandbox
end

local ok, err = pcall(fs.init_if_needed, hostname)
if(not ok) then print("init err:", err) return false end

xpcall(shell.exec, function(e) print(e, debug.traceback()) end,
       env, command, lume.reduce(rpcs, add_rpc, {read = read, write = write}))
output:push({op="disconnect"})
