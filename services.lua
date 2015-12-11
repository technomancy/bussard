-- These functions will be called by programs running on station OSes, but they
-- have access to functionality that isn't exposed inside the OS sandbox.

local utils = require("utils")
local gov = require("data.gov")
local body = require("body")

local get_price = function(good, amount, prices, direction)
   local other_direction = direction == "sell" and "buy" or "sell"
   return amount * prices[good][other_direction]
end

local space_for = function(amount, ship, direction)
   return direction=="sell" or
      (ship:cargo_amount() + amount <= ship.cargo_capacity)
end

local in_stock = function(station, ship, good, amount, direction)
   local from = direction=="sell" and ship or station
   return from.cargo[good] and (from.cargo[good] >= amount)
end

return {
   buy_user = function(ship, target, sessions, username, password)
      if(target.account_price and ship.credits >= target.account_price) then
         local session = sessions[ship.target.name]
         assert(session, "Not logged in to " .. ship.target.name)
         local fs_raw = session[3]
         local fs = target.os.fs.proxy(fs_raw, "root", fs_raw)
         target.os.fs.add_user(fs, username, password)
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

   buy_upgrade = function(ship, name)
      local target = ship.target
      local price = target.upgrade_prices and target.upgrade_prices[name]
      if(not price) then
         return false, target.name .. " does not sell " .. name
      elseif(ship.credits < price) then
         return false, "Insufficient credits; need " .. price
      elseif(utils.includes(ship.upgrade_names, name)) then
         return false, "You already have this upgrade."
      else
         table.insert(ship.upgrade_names, name)
         ship:recalculate()
         ship.credits = ship.credits - price
         return price
      end
   end,

   sell_upgrade = function(ship, name)
      local target = ship.target
      local price = math.floor(((target.upgrade_prices and
                                    target.upgrade_prices[name]) or
               body.base_prices.upgrades[name]) * 0.85)
      if(not price) then
         return false, "This upgrade is not for sale."
      elseif(not utils.includes(ship.upgrade_names, name)) then
         return false, "You don't have this upgrade."
      else
         lume.remove(ship.upgrade_names, name)
         ship:recalculate()
         ship.credits = ship.credits + price
         return price
      end
   end,

   cargo_transfer = function(station, ship, direction, good, amount)
      assert(station.prices[good], station.name .. " does not trade in " .. good)
      local price = get_price(good, amount, station.prices, direction)
      if(ship.credits < price and direction == "buy") then
         return false, "Don't have " .. price .. " credits."
      elseif(not space_for(amount, ship, direction)) then
         return false, "No space for " .. amount .. " of " .. good .. "."
      elseif(not in_stock(station, ship, good, amount, direction)) then
         if(direction == "buy") then
            return false, "Sufficient " .. good .. " is not in stock."
         else
            return false, "You don't have enough " .. good .. "."
         end
      else
         if(direction == "sell") then
            station.cargo[good] = station.cargo[good] + amount
            ship:move_cargo(good, -amount)
            ship.credits = ship.credits + price
         else
            station.cargo[good] = station.cargo[good] - amount
            ship:move_cargo(good, amount)
            ship.credits = ship.credits - price
         end
         return price
      end
   end,

   buy_visa = function(ship, to_gov)
      local from = ship.systems[ship.system_name].gov
      if(not gov.adjacent[to_gov]) then
         return false, "Government not found."
      elseif(ship.flag == to_gov) then
         return false, "Your ship is already registered under a "..to_gov.." flag."
      elseif(not lume.find(ship.upgrade_names, "passponder")) then
         return false, "Need passponder for visa."
      elseif(gov.treaties and gov.treaties[to_gov] and
             gov.treaties[to_gov][ship.flag]) then
         return false, "No visa needed for in-treaty travel."
      elseif(not gov.adjacent[from][to_gov]) then
         return false, "No embassy for " .. to_gov .. " in this system."
      elseif(gov.visas[to_gov]) then
         local visa = gov.visas[to_gov]
         if(ship.credits >= visa.price) then
            ship.credits = ship.credits - visa.price
            ship.visas[to_gov] = (ship.visas[to_gov] or 0) + 1
            return true, "Success", visa.price
         else
            return false, "Insufficient credits."
         end
      else
         return false, "Travel to " .. to_gov .. " is not permitted."
      end
   end,

   list_visas = function(ship)
      local current = ship.systems[ship.system_name]
      local visas = {}
      for this_gov,_ in pairs(gov.adjacent[current.gov]) do
         visas[this_gov] = gov.visas[this_gov]
      end
      return visas
   end
}
