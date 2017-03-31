local _, compiler = require("os.lisp.l2l.compat"), require("os.lisp.l2l.compiler")
local reader = require("os.lisp.l2l.reader")
local lume = require("lume")
local utils = require("utils")
local rpcs = require("os.lisp.rpcs")
require("love.timer")

local stdin, output, username, _hostname = ...

local portal_rc = love.filesystem.read("os/lisp/resources/portal.lsp")

local function repl(env, sandbox)
   local print, io = (sandbox.print or print), (sandbox.io or io)
   sandbox.set_prompt(">> ")
   while true do
      local str, complete_form, form, stream = "", false
      while not complete_form do
         local line = io.read("*line*")
         if line == nil then return end
         str = str .. " " .. (line or "")
         stream = reader.tofile(str)
         complete_form, form = pcall(reader.read, stream, true)

         if not complete_form then
            local metatable = getmetatable(form)
            if metatable ~= reader.UnmatchedLeftBraceException and
            metatable ~= reader.UnmatchedLeftParenException then
               print(form)
               break
            end
         end
      end
      if complete_form then
         local position = stream:seek("cur")
         local _, _form = pcall(reader.read, stream)
         if getmetatable(_form) ~= reader.EOFException then
            stream:seek("set", position)
            print("Unexpected input: "..stream:read("*all*"))
         else
            local _, result = pcall(compiler.eval, form, nil, env, sandbox)
            print("-> " .. tostring(result))
         end
      end
   end
end

local read = function() return stdin:demand() end

local write = function(...)
   local out = table.concat(lume.map({...}, tostring), " ")
   output:push({ op = "stdout", out = out })
end

local print = function(...) write(tostring(...) .. "\n") end

local disconnect = function()
   output:push({ op = "disconnect" })
end

local add_rpc = function(acc, name)
   acc[name] = function(...)
      local chan = love.thread.newChannel()
      output:push({op="rpc", fn=name, args={...}, chan=chan})
      return unpack(chan:demand())
   end
   return acc
end

local sb_base = {read = read, write = write, print = print,
                 sleep = love.timer.sleep, disconnect = disconnect}

local start = function()
   local env, sandbox = {}, lume.reduce(rpcs, add_rpc, sb_base)
   if(username == "root") then
      return repl(env, lume.merge(sandbox, utils.sandbox))
   else
      local stream = reader.tofile(portal_rc)
      local complete_form, form = pcall(reader.read, stream, true)
      if not complete_form then sandbox.print("Incomplete code.") return end
      compiler.eval(form, nil, env, lume.merge(sandbox, utils.sandbox))
   end
end

xpcall(start, function(e) print(e, debug.traceback()) end)
