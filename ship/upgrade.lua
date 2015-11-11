local utils = require("utils")

local laser_hits = function(ship, b, distance)
   -- assuming circular images
   local diameter = b.image:getWidth() / 2
   local theta = math.atan2(b.y - ship.y, b.x - ship.x)
   local angular_divergence = math.abs(ship.heading - theta)
   local divergence = math.abs(math.sin(angular_divergence) * distance)
   return divergence < diameter
end

local portal_offsets = {
   {0, -200}, {-141, -141}, {-200, 0}, {-141, 141},
   {0, 200}, {141, 141}, {200, 0}, {141, -141},
}

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
         if(ship.laser and ship.battery > 0) then
            ship.battery = ship.battery - dt * ship.laser_power * 0.1
            for _,b in pairs(ship.bodies) do
               local dist = utils.distance(ship.x - b.x, ship.y - b.y)
               local power = ship.laser_power * 16
               if(b.asteroid and laser_hits(ship, b, dist)) then
                  b.strength = b.strength - (((dt * power * 128) / (dist*dist)))
                  if(b.strength < 0) then b:split(ship) end
               end
            end
         end
      end,
      draw = function(ship)
         if(ship.laser) then
            love.graphics.push()
            love.graphics.rotate(math.pi - ship.heading)
            love.graphics.setLineWidth(3)
            love.graphics.line(0, 0, 0, -1000)
            love.graphics.pop()
         end
      end,
   },
   -- purely stat upgrades
   engine = { stats = { engine_power = 512, burn_rate = 3 } },
   cargo_bay = { stats = { cargo_capacity = 64 } },
   fuel_tank = { stats = { fuel_capacity = 128 } }

}
