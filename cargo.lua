local utils = require("utils")

local get_price = function(good, amount, prices, direction)
   return amount * prices[good][direction .. "_price"]
end

local space_for = function(amount, ship, direction)
   return direction=="sell" or
      (ship:cargo_amount() + amount <= ship.cargo_capacity)
end

local in_stock = function(station, ship, good, amount, direction)
   local from = direction=="sell" and ship or station
   return from.cargo[good] and (from.cargo[good] >= amount)
end

local transfer = function(direction, station, ship, good, amount)
   assert(station.prices[good], station.name .. " does not trade in " .. good)
   local price = get_price(good, amount, station.prices)
   if(ship.credits >= price and space_for(amount, ship, direction) and
      in_stock(station, ship, good, amount, direction)) then
      if(direction == "sell") then
         station.cargo[good] = station.cargo[good] + amount
         ship.cargo[good] = ship.cargo[good] - amount
      else
         station.cargo[good] = station.cargo[good] - amount
         ship.cargo[good] = ship.cargo[good] + amount
      end
   end
end

return {
   buy = utils.partial(transfer, "buy"),
   sell = utils.partial(transfer, "sell"),
}
