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

local transfer = function(station, ship, direction, good, amount)
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
end

return { transfer = transfer }
