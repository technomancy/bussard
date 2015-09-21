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
         laser_power=1024
      },
      action = function(ship, down)
         ship.laser = down
      end,
      update = function(ship, dt)
         for _,b in pairs(ship.bodies) do
            local distance = utils.distance(ship.x - b.x, ship.y - b.y)
            local power = ship.laser_power
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
}
