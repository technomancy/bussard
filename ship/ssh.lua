local lume = require("lume")
local utils = require("utils")
local mission = require("mission")
local body = require("body")
local services = require("services")
local serpent = require("serpent")

local sessions = {}

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

local sandbox = function(ship)
   local target = ship.target
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
      sb.subnet = services.subnet
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

local sandbox_write = function(ship, target_name, output)
   if(output) then
      ship.api.write(output)
   else
      -- printing nil means EOF, close session
      logout(ship, target_name)
   end
end

local lisp_login = function(fs, env, ship, command)
   local buffer = {}
   local max_buffer_size = 1024
   local sb = sandbox(ship)
   local write = lume.fn(sandbox_write, ship, ship.target.name)
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
   fs[env.OUT] = lume.fn(sandbox_write, ship, ship.target.name)
   -- TODO: improve error handling for problems in smashrc
   ship.target.os.process.spawn(fs, env, command, sandbox(ship))
   -- without this you can't have non-interactive SSH commands
   ship.target.os.process.scheduler(fs)
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

   send_line = send_line,
   logout = logout,

   logout_all = function() end,
}
