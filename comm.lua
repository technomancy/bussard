local body = require("body")
local utils = require("utils")
local cargo = require("cargo")

local sessions = {}

local sandbox = function(station, ship)
   return { buy = utils.partial(cargo.buy, station, ship),
            sell = utils.partial(cargo.sell, station, ship),
            station = utils.readonly_proxy(station),
            ship = ship,
   }
end

local send_input = function(ship, input)
   if(not ship.sensors.in_range(ship.sensors, ship.sensors.target)) then
      ship.repl.print("| Out of range.")
   elseif(input == "logout") then -- TODO: need to get the OS to send EOF/nil
      ship.repl.read = nil
      ship.repl.print("Logged out.")
   else
      local fs, env = unpack(sessions[ship.sensors.target.name])
      assert(fs and env and fs[env.IN], "Not logged into " ..
                ship.sensors.target.name)
      fs[env.IN](input)
   end
end

return {
   sessions = sessions, -- for debugging

   login = function(ship, target, username, password, command)
      local fs_raw = body.login(target, username, password)
      if(fs_raw) then
         local fs = target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = target.os.shell.new_env(username)
         local out_buffer = {}

         env.IN = "/tmp/in"
         env.OUT = "/tmp/out"
         target.os.shell.exec(fs, env, "mkfifo " .. env.IN)

         -- buffer output that happens when out of range
         fs[env.OUT] = function(output)
            if(output) then
               -- repl doesn't have an io:write equivalent
               ship.repl.print(output:gsub("\n$", ""))
            else
               ship.repl.read = nil -- EOF means terminate session
            end
         end

         sessions[target.name] = {fs, env, out_buffer}
         target.os.process.spawn(fs, env, command or "smash",
                                 sandbox(target, ship))
         ship.repl.read = utils.partial(send_input, ship)

         return "Login succeeded."
      else
         return "Login failed."
      end
   end,

   headless_login = function(ship, target, username, password, command)
      local fs_raw = body.login(target, username, password)
      local fs = target.os.fs.proxy(fs_raw, username, fs_raw)
      local env = target.os.shell.new_env(username)
      target.os.shell.exec(fs, env, command or "smash", sandbox(target, ship))
   end,

   send_input = send_input,

   flush = function()
      for _,v in pairs(sessions) do
         local _, _, out_buffer = unpack(v)
         for _,f in ipairs(out_buffer) do f() end
      end
   end
}
