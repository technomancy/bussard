local lume = require("lume")
local utils = require("utils")
local mission = require("mission")
local body = require("body")
local services = require("services")
local serpent = require("serpent")

local sessions = {}

-- The I/O model for SSH sessions is pretty confusing, so listen up. Logging
-- in involves attempting to authenticate with the target, and storing a tuple
-- of {fs, env, fs_raw} in the sessions table. The first fs is a proxied table
-- which enforces access limitations.

-- From there it logs into orb (portal logins are different; using lisp_login)
-- by setting env.IN and env.OUT; the former to a FIFO file node, and the
-- latter to a function which writes its output to the console.

-- In the "ssh" mode of the editor, enter is rebound to `send_line`, which
-- looks up the env.IN FIFO node and puts the input into it, provided the ship
-- is within range.

-- Within remote OSes, `io.write` is bound to a function that calls env.OUT,
-- and `io.read` pulls from the env.IN FIFO. All the code that runs on the OS
-- uses the sandbox below.

local logout = function(ship, target)
   ship.api:activate_mode("console")
   ship.api.editor.set_prompt("> ")
   if(not target) then return ship.api.editor.invisible end
   local session = sessions[target.name]
   if(session) then
      local fs, env = unpack(session)
      if(env.USER == "guest") then
         for k,_ in pairs(fs["/home/guest"] or {}) do
            if(k ~= "_user" and k ~= "_group") then
               session[1]["/home/guest/" .. k] = nil
            end
         end
      end
      sessions[target.name] = nil
   else
      (ship.api or ship).print("| Not logged in.")
   end
   ship.api:activate_mode("console")
   ship.api.editor.set_prompt("> ")
   return ship.api.editor.invisible
end

local send_line = function(ship, input)
   if(not ship.comm_connected) then
      logout(ship, nil) -- shouldn't happen, but get out of ssh mode anyway
   elseif(not ship:in_range(ship.target)) then
      ship.api.print("| Out of range. Run `logout` to disconnect or move back in range.")
   elseif(not sessions[ship.target.name]) then
      ship.api.print("Not logged in to " .. ship.target.name ..
                        ". Run `logout` to disconnect.")
   else
      local fs, env = unpack(sessions[ship.target.name])
      assert(fs and env, "Not logged into " .. ship.target.name)
      if(fs[env.IN]) then
         fs[env.IN](input)
      else
         env.IN(input)
      end
   end
end

local sandbox = function(ship, target)
   local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
   local sb = {
      buy_user = lume.fn(services.buy_user, ship, ship.target, sessions),
      buy_upgrade = lume.fn(services.buy_upgrade, ship),
      sell_upgrade = lume.fn(services.sell_upgrade, ship),
      refuel = lume.fn(services.refuel, ship, ship.target),
      cargo_transfer = lume.fn(services.cargo_transfer, ship.target, ship),

      upgrade_help = ship.api.help.get,
      station = utils.readonly_proxy(ship.target),
      ship = ship.api,
      distance = lume.fn(utils.distance, ship, ship.target),
      os = {time = lume.fn(utils.time, ship)},
      term = { set_prompt = ship.api.editor.set_prompt,
               get_prompt = ship.api.editor.get_prompt },
      set_prompt = ship.api.editor.set_prompt,
      get_prompt = ship.api.editor.get_prompt,
      pps = function(x) return serpent.block(x, serpent_opts) end,
   }
   sb.pp = function(x) sb.print(serpent.block(x, serpent_opts)) end

   ship.sandbox.logout = function()
      logout(ship, target)
      ship.comm_connected = false
   end

   if(ship.target and ship.target.subnet) then
      sb.subnet = lume.fn(services.subnet.request, ship)
   end

   if(ship.target and ship.target.portal) then
      sb.body = ship.target
      sb.portal_target = ship.target.portal
      sb.no_trip_clearance = lume.fn(services.no_trip_clearance, ship,
                                     ship.system_name, ship.target.portal)
      sb.set_beams = function(n)
         target.beam_count = ((n or 0) * 9) / ship.portal_time
      end
      sb.portal_activate = function() ship:enter(target.portal, true) end
      sb.draw_power = function(power)
         assert(ship.battery - power >= 0, "Insufficient power.")
         ship.portal_target = target
         ship.battery = ship.battery - power
      end
   end
   return lume.merge(utils.sandbox, sb)
end

local lisp_login = function(fs, env, ship, command)
   local buffer = {}
   local max_buffer_size = 1024
   local sb = sandbox(ship, ship.target)
   local write = ship.api.write
   env.IN = function(...)
      local arg = {...}
      if(#arg == 0 or arg[1] == "*line*") then
         while #buffer == 0 do coroutine.yield() end
         return table.remove(buffer, 1)
      elseif(arg[1] == "*buffer") then
         return buffer
      else -- write
         while(#buffer > max_buffer_size) do coroutine.yield() end
         for _,output in pairs(arg) do
            table.insert(buffer, output)
         end
      end
   end

   sb.disconnect = function()
      ship.api.editor.with_current_buffer("*console*", function()
                                             ship.api:activate_mode("console")
                                             ship.api.editor.set_prompt("> ")
      end)
      logout(ship, ship.target)
   end

   sb.io = sb.io or { read = env.IN, write = write }
   sb.print = ship.api.print

   ship.target.os.shell.spawn(fs, env, sb, command)
end

local orb_login = function(fs, env, ship, command)
   env.IN, env.OUT = "/tmp/in", "/tmp/out"
   ship.target.os.shell.exec(fs, env, "mkfifo " .. env.IN)
   fs[env.OUT] = ship.api.write
   -- TODO: improve error handling for problems in smashrc
   ship.target.os.process.spawn(fs, env, command, sandbox(ship, ship.target))
   -- without this you can't have non-shell SSH commands
   ship.target.os.process.scheduler(fs)
end

local get_connection = function(ship, username, password)
   if(not ship:in_range(ship.target)) then
      ship.api.editor.print("| Out of communications range.")
      return
   end

   assert(ship.target.os.name == "orb", "TODO: support non-orb")
   local fs_raw = body.login(ship, ship.target, username, password)
   if(fs_raw) then
      local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
      local env = ship.target.os.shell.new_env(username)
      local session_id = tostring(love.math.random(99999999))
      local target = ship.target
      env.HOST = body.hostname(ship.target.name)
      env.OUT = "/tmp/out-" .. session_id

      env.IN = "/tmp/in-" .. session_id
      ship.target.os.shell.exec(fs, env, "mkfifo " .. env.IN)
      fs[env.IN](false, 0)

      mission.on_login(ship)
      return function(command)
         assert(fs, "Already logged out; establish a new connection.")
         if(command == "logout") then
            fs[env.OUT], fs, env = nil, nil, nil
            return
         end

         local output = ""
         fs[env.OUT] = function(...)
            for _,x in ipairs({...}) do
               output = output .. x
            end
         end

         ship.target.os.shell.exec(fs, env, command, sandbox(ship, target))
         return output
      end
   else
      return nil, "Login failed."
   end
end

return {
   connect = function(ship, username, password, command)
      ship.api.closest_cycle = 1
      if(not ship:in_range(ship.target)) then
         ship.api.editor.print("| Out of communications range.")
         return
      end

      local fs_raw = body.login(ship, ship.target, username, password)
      if(fs_raw) then
         local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = ship.target.os.shell.new_env(username)

         env.HOST = body.hostname(ship.target.name)
         sessions[ship.target.name] = {fs, env, fs_raw}
         ship.comm_connected = ship.target.name

         if(ship.target.os.name == "orb") then
            orb_login(fs, env, ship, command)
         elseif(ship.target.os.name == "lisp") then
            lisp_login(fs, env, ship, command)
         else
            error("Unknown OS: " .. ship.target.os.name)
         end

         mission.on_login(ship)
      else
         ship.api.print("Login failed.")
      end
   end,

   get_connection = get_connection,

   send_line = send_line,
   logout = logout,

   logout_all = function() end,
}
