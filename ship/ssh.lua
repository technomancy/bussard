local lume = require("lume")
local utils = require("utils")
local mission = require("mission")
local body = require("body")
local services = require("services")

local sessions = {}

local logout = function(ship, target)
   local session = sessions[target.name]
   if(session) then
      local fs, env = unpack(session)
      for k,_ in pairs(fs["/home/guest"] or {}) do
         if(k ~= "_user" and k ~= "_group") then
            session[1]["/home/guest/" .. k] = nil
         end
      end
      sessions[target.name] = nil
      if(not target.name:match("[Pp]ortal")) then
         ship.api.print("\nLogged out.")
      end
   else
      ship.api.print("| Not logged in.")
   end
   return ship.api.editor.invisible
end

local send_line = function(ship, input)
   if(not ship:in_range(ship.target)) then
      ship.api.print("| Out of range. Run `logout` to disconnect.")
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
   local sb = {
      buy_user = lume.fn(services.buy_user, ship, ship.target, sessions),
      buy_upgrade = lume.fn(services.buy_upgrade, ship),
      sell_upgrade = lume.fn(services.sell_upgrade, ship),
      refuel = lume.fn(services.refuel, ship, ship.target),
      cargo_transfer = lume.fn(services.cargo_transfer, ship.target, ship),
      accept_mission = lume.fn(mission.accept, ship),

      upgrade_help = ship.api.help.get,
      -- scp = lume.fn(scp, ship),
      station = utils.readonly_proxy(ship.target),
      ship = ship.api,
      distance = lume.fn(utils.distance, ship, ship.target),
      os = {time = lume.fn(utils.time, ship)},
      set_prompt = ship.api.editor.set_prompt,
   }

   ship.sandbox.logout = function()
      logout(ship, target)
      ship.comm_connected = false
   end

   if(ship.target and ship.target.portal) then
      sb.body = ship.target
      sb.portal_target = ship.target.portal
      sb.trip_cleared = lume.fn(portal_cleared, ship, target)
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

local sandbox_out = function(ship, target_name, output)
   if(output) then
      ship.api.write(output)
   else
      -- printing nil means EOF, close session
      logout(target_name, ship)
      disconnect(ship)
   end
end

local orb_login = function(fs, env, ship)
   env.IN, env.OUT = "/tmp/in", "/tmp/out"
   ship.target.os.shell.exec(fs, env, "mkfifo " .. env.IN)
   fs[env.OUT] = lume.fn(sandbox_out, ship, ship.target.name)
   fs[env.HOME .. "/ship"] = ship.api

   -- free recharge upon connect
   ship.battery = ship.battery_capacity

   -- TODO: improve error handling for problems in smashrc
   ship.target.os.process.spawn(fs, env, nil, sandbox(ship))
end

return {
   connect = function(ship, username, password)
      if(not ship:in_range(ship.target)) then
         ship.api.editor.print("| Out of range.")
      end

      local fs_raw = body.login(ship, ship.target, username, password)
      if(fs_raw) then
         local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = ship.target.os.shell.new_env(username)

         env.HOST = body.hostname(ship.target.name)
         -- env.ROWS = tostring(ship.api.editor.rows)
         -- env.COLS = tostring(ship.api.editor.cols)

         sessions[ship.target.name] = {fs, env, fs_raw}
         ship.comm_connected = ship.target.name

         if(ship.target.os.name == "orb") then
            orb_login(fs, env, ship)
         elseif(ship.target.os.name == "lisp") then
            lisp_login(fs, env, ship)
         else
            error("Unknown OS: " .. ship.target.os.name)
         end

         local default_motd = "Login succeeded. Run `logout` to disconnect."
         ship.api.print(fs_raw.etc.motd or default_motd)

         mission.check(ship)
      else
         ship.api.print("Login failed.")
      end
   end,

   send_line = send_line,
   logout = logout,

   logout_all = function(ship)
   end,
}
