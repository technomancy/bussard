local utils = require("utils")

local laser_hits = function(ship, b, distance)
   -- assuming circular images
   local diameter = b.image:getWidth() / 2
   local theta = math.atan2(b.y - ship.y, b.x - ship.x)
   local angular_divergence = math.abs(ship.heading - theta)
   local divergence = math.abs(math.sin(angular_divergence) * distance)
   return divergence < diameter
end

return {
   laser = {
      stats = {
         mass = 32,
         laser_power = 64,
         scoop_range = 512,
      },
      action = function(ship, down)
         if(down == "toggle") then
            ship.laser = not ship.laser
         else
            ship.laser = down
         end
      end,
      update = function(ship, dt)
         for _,b in pairs(ship.bodies) do
            local distance = utils.distance(ship.x - b.x, ship.y - b.y)
            local power = ship.laser_power * 16
            if(ship.laser and b.asteroid and laser_hits(ship, b, distance)) then
               -- TODO: firing laser uses up power?
               b.strength = b.strength - dt * power / math.sqrt(distance)
               if(b.strength < 0) then b:split(ship) end
            end
         end
      end,
      draw = function(ship, dt)
         if(ship.laser) then
            love.graphics.push()
            love.graphics.rotate(math.pi - ship.heading)
            love.graphics.setLineWidth(3)
            love.graphics.line(0, 0, 0, -1000)
            love.graphics.pop()
         end
      end,
   },

   buy = function(ship, name)
      local target = ship.target
      local price = target.upgrade_prices and target.upgrade_prices[name]
      if(not price) then
         return false, target.name .. " does not sell " .. name
      elseif(ship.credits < price) then
         return false, "Insufficient credits; need " .. price
      else
         table.insert(ship.upgrade_names, name)
         ship:recalculate()
         ship.credits = ship.credits - price
         return price
      end
   end,
}
