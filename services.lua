-- These functions will be called by programs running on station OSes, but they
-- have access to functionality that isn't exposed inside the OS sandbox.

local orb = require("os/orb")

return {
   buy_user = function(ship, target, sessions, username, password)
      if(target.account_price and ship.credits >= target.account_price) then
         local session = sessions[ship.target.name]
         assert(session, "Not logged in to " .. ship.target.name)
         local fs_raw = session[3]
         local fs = target.os.fs.proxy(fs_raw, "root", fs_raw)
         orb.fs.add_user(fs, username, password)
         ship.credits = ship.credits - target.account_price
         return true
      elseif(target.account_price) then
         return false, "Insufficient credits."
      else
         return false, "This station does not sell accounts."
      end
   end,

   refuel = function(ship, target, amount)
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
   end,
}
