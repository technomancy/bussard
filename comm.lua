local body = require("body")
local utils = require("utils")
local cargo = require("cargo")

local sessions = {}

local refuel = function(ship, target, amount)
   if(target.fuel_price) then
      local cost = target.fuel_price * amount
      local open_fuel_capacity = ship.fuel_capacity - ship.fuel
      if(amount > open_fuel_capacity) then
         return false, "Fuel tank only has room for " .. open_fuel_capacity .. "."
      elseif(cost < ship.credits) then
         ship.fuel = ship.fuel + amount
         ship.credits = ship.credits - cost
         return amount, "Purchased " .. amount .. " fuel for " .. cost .. "."
      else
         return false, "Insufficient credits."
      end
   else
      return false, "This station does not sell fuel."
   end
end

local sandbox = function(ship)
   return { cargo_transfer = utils.partial(cargo.transfer, ship.target, ship),
            refuel = utils.partial(refuel, ship, ship.target),
            station = utils.readonly_proxy(ship.target),
            ship = ship.api,
   }
end

local send_input = function(ship, input)
   if(input == "logout") then -- TODO: need to get the OS to send EOF/nil
      ship.api.repl.read = nil
      ship.api.repl.prompt = nil
      ship.api.repl.print("Logged out.")
      ship.comm_connected = false
      -- TODO: wipe guest account on logout
   elseif(not ship:in_range(ship.target)) then
      ship.api.repl.print("| Out of range.")
   else
      local fs, env = unpack(sessions[ship.target.name])
      assert(fs and env and fs[env.IN], "Not logged into " .. ship.target.name)
      -- TODO: insert into history
      ship.api.repl.print(input)
      fs[env.IN](input)
   end
end

return {
   sessions = sessions, -- for debugging

   login = function(ship, username, password, command)
      if(not ship:in_range(ship.target)) then
         ship.api.repl.print("| Out of range.")
         return
      end

      username, password = username or "guest", password or ""
      local fs_raw = body.login(ship.target, username, password)
      if(fs_raw) then
         local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
         local env = ship.target.os.shell.new_env(username)
         local out_buffer = {}

         env.IN = "/tmp/in"
         env.OUT = "/tmp/out"
         ship.target.os.shell.exec(fs, env, "mkfifo " .. env.IN)

         -- buffer output that happens when out of range
         fs[env.OUT] = function(output)
            if(output) then
               -- repl doesn't have an io:write equivalent
               output = output:gsub("\n\n", "")
               ship.api.repl.print(output)
            else
               ship.api.repl.read = nil -- EOF means terminate session
            end
         end

         sessions[ship.target.name] = {fs, env, out_buffer}
         ship.target.os.process.spawn(fs, env, command or "smash", sandbox(ship))
         ship.api.repl.read = utils.partial(send_input, ship)
         ship.api.repl.prompt = "$ "
         ship.comm_connected = true

         return "Login succeeded."
      else
         return "Login failed."
      end
   end,

   headless_login = function(ship, username, password, command)
      local fs_raw = body.login(ship.target, username, password)
      local fs = ship.target.os.fs.proxy(fs_raw, username, fs_raw)
      local env = ship.target.os.shell.new_env(username)
      ship.target.os.shell.exec(fs, env, command or "smash", sandbox(ship))
   end,

   send_input = send_input,

   flush = function()
      for _,v in pairs(sessions) do
         local _, _, out_buffer = unpack(v)
         for _,f in ipairs(out_buffer) do f() end
      end
   end
}
