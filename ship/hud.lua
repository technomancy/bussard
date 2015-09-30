local utils = require "utils"
local body = require "body"

local hud_text = "speed: %0.2f     pos: %5.2f, %5.2f\n" ..
   "target: %s     distance: %0.2f\n" ..
   "epoch: %s     credits: %s"

local vector_size = 50
local w, h = love.graphics:getWidth(), love.graphics:getHeight()

return {
   -- TODO: data-driven hud
   render = function(ship, target)
      local speed = utils.distance(ship.dx, ship.dy)
      local formatted_time = utils.format_seconds(os.time() + ship.time_offset)
      local distance, target_name

      -- TODO: move target indicators to upper right, add mass, time to target
      if(target) then
         distance = utils.distance(ship.x - target.x, ship.y - target.y)
         target_name = target.name
      else
         target_name, distance = "none", 0
      end

      love.graphics.setColor(255, 255, 255, 150)
      love.graphics.print(string.format(hud_text, speed, ship.x, ship.y,
                                        target_name, distance,
                                        formatted_time, ship.credits), 5, 5)

      -- TODO: throttle indicator
      -- TODO: cargo indicator

      -- scale indicator
      local scale_y = math.log(ship.api.scale) * h
      love.graphics.line(w - 5, scale_y, w, scale_y)

      love.graphics.setLineWidth(1)

      -- battery
      love.graphics.setColor(20, 255, 20);
      love.graphics.rectangle("fill", 5, 75,
                              math.min(ship.battery, ship.battery_capacity), 10)
      love.graphics.setColor(200, 255, 200);
      love.graphics.rectangle("line", 5, 75, ship.battery_capacity, 10)

      -- remaining fuel
      love.graphics.setColor(255, 20, 20);
      love.graphics.rectangle("fill", 5, 60,
                              math.min(ship.fuel, ship.fuel_capacity), 10)
      love.graphics.setColor(255, 200, 200);
      love.graphics.rectangle("line", 5, 60, ship.fuel_capacity, 10)

      -- how much fuel will we use to stop?
      local fuel_to_stop = speed * ship.engine_strength * ship.burn_rate /
         -- no idea where this 20 factor comes from
         (ship.mass * 20)
      love.graphics.setColor(150, 50, 50);
      love.graphics.rectangle("fill", 5, 60, fuel_to_stop, 5)
   end,

   trajectory = function(ship, bodies, steps, step_size, color)
      local last_x, last_y
      local sim_ship = {x = ship.x, y = ship.y, dx = ship.dx, dy = ship.dy, mass = ship.mass}
      local sim_bodies = {}
      for _, b in pairs(bodies) do
         if(b ~= ship) then
            sim_bodies[#sim_bodies+1] = {x = b.x, y = b.y, dx = b.dx, dy = b.dy,
                                   mass = b.mass}
         end
      end

      love.graphics.setLineWidth(5)
      love.graphics.setColor(color)
      for _=0, steps do
         last_x, last_y = sim_ship.x, sim_ship.y
         body.gravitate_all(sim_bodies, sim_ship, step_size)
         love.graphics.line(last_x - ship.x, last_y - ship.y, sim_ship.x - ship.x, sim_ship.y - ship.y)
      end
   end,

   vector = function(x, y, at_x, at_y)
      local half = vector_size / 2
      love.graphics.push()
      love.graphics.setLineWidth(1)
      love.graphics.setColor(255, 255, 255);
      love.graphics.rectangle("line", at_x, at_y, vector_size, vector_size)
      love.graphics.setLineWidth(3)
      love.graphics.setColor(50, 255, 50);
      -- TODO: scale length of vector non-linearly
      love.graphics.line(at_x + half, at_y + half,
                         at_x + half + x, at_y + half + y)
      love.graphics.pop()
   end,

   vector_size = vector_size,
}
