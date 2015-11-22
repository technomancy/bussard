local lume = require("lume")
local utils = require("utils")
local gov = require("data/gov")
local mission = require("mission")
local body = require("body")

local services = require("services")

local sessions = {}

local scp_login = function(ship, orig_path)
   local username, pwpath = unpack(lume.split(orig_path, ":"))
   local password, path = pwpath:match("(%a+)/(.+)")
   local fs_raw = body.login(ship, ship.target, username, password or "")
   assert(fs_raw, "Incorrect login.")
   local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
   return fs, path
end

local scp_from = function(ship, from, to)
   local fs, path = scp_login(ship, from)
   local dest_components, target = lume.split(to, "."), ship.api
   local file = table.remove(dest_components)
   for _,d in pairs(dest_components) do
      if(target[d] == nil) then target[d] = {} end
      target = target[d]
   end
   target[file] = fs[path]
end

local scp_to = function(ship, from, to)
   local fs, path = scp_login(ship, to)
   local source = assert(ship:find(from), from .. " not found.")
   fs[path] = source
end

local scp = function(ship, from, to)
   assert(ship:in_range(ship.target), "| Out of range.")
   if(from:find("/")) then
      scp_from(ship, from, to)
   elseif(to:find("/")) then
      scp_to(ship, from, to)
   else
      error("Neither " .. from .. " nor " .. " to " .. "are remote files.")
   end
   ship.api.repl.print("Copied successfully.")
end

local portal_cleared = function(ship, portal_body)
   -- TODO: import duties to pay on your cargo
   local target_gov = assert(ship.systems[portal_body.portal].gov)
   local current_gov = assert(ship.systems[ship.system_name].gov)
   local now = utils.time(ship)
   if(not portal_body.interportal) then return true end
   if(gov.treaties and gov.treaties[target_gov] and
      gov.treaties[target_gov][current_gov]) then return true end
   if(gov.treaties and gov.treaties[target_gov] and
      gov.treaties[target_gov][ship.flag]) then return true end
   local visa = ship.visas[target_gov] and ship.visas[target_gov] > now
   return visa, "no visa to " .. target_gov .. "; please visit station embassy."
end

local disconnect = function(ship)
   ship.api.repl.read = nil
   ship.api.repl.prompt = nil
   ship.comm_connected = false
end

local logout = function(name)
   local session = sessions[name]
   if(session) then
      local fs, _ = unpack(session)
      for k,_ in pairs(fs["/home/guest"] or {}) do
         if(k ~= "_user" and k ~= "_group") then
            session[1]["/home/guest/" .. k] = nil
         end
      end
      sessions[name] = nil
   end
end

local sandbox = function(ship)
   local target = ship.target
   local sb = {
      buy_user = lume.fn(services.buy_user, ship, ship.target, sessions),
      buy_upgrade = lume.fn(services.buy_upgrade, ship),
      sell_upgrade = lume.fn(services.sell_upgrade, ship),
      upgrade_help = ship.api.help.get,
      buy_visa = lume.fn(services.buy_visa, ship),
      list_visas = lume.fn(services.list_visas, ship),
      refuel = lume.fn(services.refuel, ship, ship.target),
      cargo_transfer = lume.fn(services.cargo_transfer, ship.target, ship),
      scp = lume.fn(scp, ship),
      station = utils.readonly_proxy(ship.target),
      ship = ship.api,
      distance = lume.fn(utils.distance, ship, ship.target),
      os = {time = lume.fn(utils.time, ship)},
      accept_mission = lume.fn(mission.accept, ship),
      set_prompt = function(p) ship.api.repl.prompt = p end,
      disconnect = function()
         disconnect(ship)
         logout(target.name)
      end
   }
   if(ship.target and ship.target.portal) then
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

local send_input = function(ship, input)
   if(input == "logout") then
      if(sessions[ship.comm_connected]) then
         ship.api.repl.print("Logged out.")
         logout(ship.target.name)
      else
         ship.api.repl.print("| Not logged in.")
      end
      disconnect(ship)
   elseif(not ship:in_range(ship.target)) then
      ship.api.repl.print("| Out of range. Run `logout` to disconnect.")
   elseif(not sessions[ship.target.name]) then
      ship.api.repl.print("Not logged in to " .. ship.target.name ..
                             ". Run `logout` to disconnect.")
   else
      local fs, env = unpack(sessions[ship.target.name])
      assert(fs and env, "Not logged into " .. ship.target.name)
      ship.api.repl.history:append(input, true)
      ship.api.repl.print((ship.api.repl.prompt or "> ") .. input)
      if(fs[env.IN]) then
         fs[env.IN](input)
      else
         env.IN(input)
      end
   end
end

local sandbox_out = function(ship, target_name, output)
   if(output) then
      ship.api.repl.write(output)
   else
      -- printing nil means EOF, close session
      logout(target_name)
      disconnect(ship)
   end
end

local orb_login = function(fs, env, ship)
   env.IN, env.OUT = "/tmp/in", "/tmp/out"
   ship.target.os.shell.exec(fs, env, "mkfifo " .. env.IN)
   fs[env.OUT] = lume.fn(sandbox_out, ship, ship.target.name)

   -- TODO: improve error handling for problems in smashrc
   ship.target.os.process.spawn(fs, env, nil, sandbox(ship))
end

local lisp_login = function(fs, env, ship)
   local buffer = {}
   local max_buffer_size = 1024
   local box = sandbox(ship)
   local write = ship.api.repl.write
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
   box.io = box.io or { read = env.IN, write = write }
   box.print = function(...) write(unpack(lume.map({...}, tostring))) write("\n") end

   ship.target.os.shell.spawn(fs, env, box)
end


return {
   sessions = sessions, -- for debugging

   login = function(ship, username, password)
      if(not ship:in_range(ship.target)) then
         ship.api.repl.print("| Out of range.")
         return
      end

      username, password = username or "guest", password or ""
      local fs_raw = body.login(ship, ship.target, username, password)
      if(fs_raw) then
         local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = ship.target.os.shell.new_env(username)

         env.ROWS = tostring(ship.api.repl.rows)
         env.COLS = tostring(ship.api.repl.cols)

         sessions[ship.target.name] = {fs, env, fs_raw}
         ship.api.repl.read = lume.fn(send_input, ship)
         ship.comm_connected = ship.target.name

         if(ship.target.os.name == "orb") then
            orb_login(fs, env, ship)
         elseif(ship.target.os.name == "lisp") then
            lisp_login(fs, env, ship)
         else
            error("Unknown OS: " .. ship.target.os.name)
         end
         -- free recharge upon connect
         ship.battery = ship.battery_capacity

         local default_motd = "Login succeeded. Run `logout` to disconnect."
         ship.api.repl.print(fs_raw.etc.motd or default_motd)

         mission.check(ship)

         return ship.api.repl.invisible
      else
         ship.api.repl.print("Login failed.")
         return ship.api.repl.invisible
      end
   end,

   logout_all = function(ship)
      for _,session in pairs(sessions) do
         logout(session)
      end
      disconnect(ship)
   end,

   headless_login = function(ship, username, password, command)
      local fs_raw = body.login(ship, ship.target, username, password)
      local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
      local env = ship.target.os.shell.new_env(username)
      ship.target.os.shell.exec(fs, env, command or "smash", sandbox(ship))
   end,

   send_input = send_input,

   scp = scp,
}
