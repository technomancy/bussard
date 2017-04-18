local shell = require("os.orb.shell")
local fs = require("os.orb.fs")
local lume = require("lume")
local utf8 = require("polywell.utf8")

local pp = function(x) print(lume.serialize(x)) end

local function completions_for(input, dir, prefixes)
   local input_parts = lume.split(input, "/")
   if(#input_parts == 1) then
      local matches = {}
      for _,v in pairs(fs.ls(dir, "/")) do
         if(fs.exists(v, dir) and utf8.sub(v, 1, #input) == input) then
            local parts = lume.clone(prefixes)
            table.insert(parts, v)
            table.insert(matches, table.concat(parts, "/"))
         end
      end
      return matches
   else
      local first_part = table.remove(input_parts, 1)
      table.insert(prefixes, first_part)
      return completions_for(table.concat(input_parts, "/"),
                             dir .. first_part, prefixes)
   end
end

return {
   new_session = function(stdin, output, username, hostname)
      local env = shell.new_env(username, hostname)
      local thread = love.thread.newThread("os/orb/session.lua")
      thread:start(env, "smash", stdin, output, hostname)
      return env
   end,

   is_authorized = function(hostname, username, password)
      local ok, err = pcall(fs.init_if_needed, hostname)
      if(not ok) then print("auth err", err) return false end
      return shell.auth(username, password)
   end,

   -- This will only kill threads that are smash sessions; ugh.
   kill = function(session) session.stdin:push("logout") end,

   handlers = {
      complete = function(session, msg, channel)
         local ok, err = pcall(function()
               local dir = msg.input:match("^/") and "" or session.CWD
               local completions = completions_for(msg.input, dir, {})
               channel:push({op="rpc", fn="completions",
                             args={completions, msg.input}})
         end)
         if(not ok) then
            print("OS handler error:", err)
            channel:push({op="status", status="err", out=err})
         end
      end
   },
}
