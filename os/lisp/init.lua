local _, compiler = require("os.lisp.l2l.compat"), require("os.lisp.l2l.compiler")
local reader = require("os.lisp.l2l.reader")
local lume = require("lume")
local utils = require("utils")

local portal_rc = love.filesystem.read("os/lisp/resources/portal.lsp")

local id_for = function(p)
   return tostring(p):match(": 0x(.+)")
end

local function repl(_, env, sandbox)
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

local run = function(_, env, sandbox, code)
   local stream = reader.tofile(code)
   local complete_form, form = pcall(reader.read, stream, true)
   if not complete_form then sandbox.print("Incomplete code.") return end
   local _, result = pcall(compiler.eval, form, nil, env, sandbox)
   return result
end

local sandbox = function(ship, _, target, disconnect)
   local services = require("services")
   local sb = {}
   sb.body = ship.target
   sb.portal_target = ship.target.portal
   sb.no_trip_clearance = lume.fn(services.no_trip_clearance, ship,
                                  ship.system_name, ship.target.portal,
                                  ship.target.interportal)
   sb.set_beams = function(n)
      target.beam_count = ((n or 0) * 9) / ship.portal_time
   end
   sb.portal_activate = function()
      ship:enter(target.portal, true)
      -- if(lume.count(ship.humans) == 0) then
      --    ship.fine = ship.fine + 2460
      --    mail.deliver_msg(ship, "unauthorized-portal.msg", true)
      -- end
   end
   sb.draw_power = function(power)
      assert(ship.battery - power >= 0, "Insufficient power.")
      ship.portal_target = target
      ship.battery = ship.battery - power
   end
   sb.disconnect = disconnect
   sb.os = {time = lume.fn(utils.time, ship)}
   sb.distance = lume.fn(utils.distance, ship, ship.target)
   sb.ship = ship.api

   return lume.merge(utils.sandbox, sb)
end

return {
   shell = {
      auth = function(fs, username, password)
         if((username == "guest" and password == "") or
            (username == "root" and password == "K'chua")) then
            return fs
         end
      end,
      new_env = function(user)
         return {USER = user, LOGIN = portal_rc}
      end,
      spawn = function(fs, env, sb, command)
         local co
         if(sb.portal_target and env.USER ~= "root") then
            co = coroutine.create(lume.fn(run, fs, env, sb, portal_rc))
         else
            co = coroutine.create(lume.fn(repl, fs, env, sb, command))
         end
         local id = id_for(co)
         fs.proc[id] = { thread = co, id = id }
      end,
   },
   process = {
      scheduler = function(f)
         for k,p in pairs(f.proc) do
            if(type(p) == "table" and p.thread) then
               if(coroutine.status(p.thread) == "dead") then
                  f.proc[k] = nil
               else
                  local _, err = coroutine.resume(p.thread)
                  if err then print(err) end
               end
            end
         end
      end,
   },
   fs = {
      new_raw = function() return { proc = {} } end,
      proxy = function(x) return x end,
      dirname = function(_) end,
      mkdir = function(_, _) end,
      seed = function(fs)
         fs.bin = {
            portal = love.filesystem.read("os/lisp/resources/portal.scm"),
            repl = love.filesystem.read("os/lisp/resources/repl.scm")
         }
         fs.etc = { motd = ";; Welcome to the lisp REPL." }
      end,
   },

   login = function(fs, env, _, disconnect, ship, command)
      local buffer = {}
      local max_buffer_size = 1024
      local sb = sandbox(ship, env, ship.target, disconnect)
      local write = ship.api.write
      env.IN = function(...)
         local arg = {...}
         if(#arg == 0) then
            while #buffer == 0 do coroutine.yield() end
            return table.remove(buffer, 1)
         elseif(arg[1] == {}) then
            return buffer
         else -- write
            while(#buffer > max_buffer_size) do coroutine.yield() end
            for _,output in pairs(arg) do
               table.insert(buffer, output)
            end
         end
      end

      sb.io = sb.io or { read = env.IN, write = write }
      sb.print = ship.api.print

      ship.target.os.shell.spawn(fs, env, sb, command)
   end,

   name = "lisp",
}
