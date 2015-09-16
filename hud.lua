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

      -- remaining fuel
      love.graphics.setLineWidth(1)
      love.graphics.setColor(255, 20, 20);
      love.graphics.rectangle("fill", 5, 60,
                              math.min(ship.fuel, ship.fuel_capacity), 10)
      love.graphics.setColor(255, 200, 200);
      love.graphics.rectangle("line", 5, 60, ship.fuel_capacity, 10)

      -- how much fuel will we use to stop?
      local speed = utils.distance(ship.dx, ship.dy)
      local fuel_to_stop = speed * ship.engine_strength / ship.burn_rate
      love.graphics.setColor(150, 50, 50);
      love.graphics.rectangle("fill", 5, 60, fuel_to_stop, 5)
   end,

   trajectory = function(ship, bodies, steps)
      local last_x, last_y
      local x, y, dx, dy = ship.x, ship.y, ship.dx, ship.dy
      local body_points = {}
      for _, b in pairs(bodies) do
         body_points[b.name] = {x = b.x, y = b.y, dx = b.dx, dy = b.dy, mass = b.mass}
      end

      love.graphics.setLineWidth(5)
      love.graphics.setColor(150, 150, 255)
      for _=0, steps do
         for _, b in pairs(body_points) do
            local ddx, ddy = body.gravitate(b, x, y)
            dx = dx + ddx * ship.api.trajectory_step_size
            dy = dy + ddy * ship.api.trajectory_step_size
            b.x = b.x + (b.dx * ship.api.trajectory_step_size * 100)
            b.y = b.y + (b.dy * ship.api.trajectory_step_size * 100)
         end

         for _, b2 in ipairs(body_points) do
            if(b ~= b2 and (not b2.star)) then
               local ddx2, ddy2 = body.gravitate(b, b2.x, b2.y)
               b2.dx = b2.dx + (dt * ddx2)
               b2.dy = b2.dy + (dt * ddy2)
            end
         end
         last_x, last_y = x, y
         x = x + (dx * ship.api.trajectory_step_size * 100)
         y = y + (dy * ship.api.trajectory_step_size * 100)

         love.graphics.line(last_x - ship.x, last_y - ship.y, x - ship.x, y - ship.y)
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
