local utils = require "utils"
local body = require "body"

local hud_text = "speed: %0.2f | pos: %5.2f, %5.2f\n" ..
   "target: %s | distance: %0.2f"

local vector_size = 50

return {
   render = function(ship, target)
      local speed = utils.calculate_distance(ship.dx, ship.dy)
      local distance, target_name
      if(target) then
         distance = utils.calculate_distance(ship.x - target.x, ship.y - target.y)
         target_name = target.name
      else
         target_name, distance = "none", 0
      end

      love.graphics.setColor(255, 255, 255, 150)

      love.graphics.print(string.format(hud_text, speed, ship.x, ship.y,
                                        target_name, distance),
                          5, 5)

      -- fuel readout
      love.graphics.setLineWidth(1)
      love.graphics.setColor(255, 50, 50);
      love.graphics.rectangle("fill", 5, 50,
                              math.min(ship.fuel * 2, ship.fuel_capacity*2), 20)
      love.graphics.setColor(255, 200, 200);
      love.graphics.rectangle("line", 5, 50, ship.fuel_capacity*2, 20)
   end,

   trajectory = function(ship, bodies, steps)
      local last_x, last_y = nil, nil
      local x, y, dx, dy = ship.x, ship.y, ship.dx, ship.dy
      local body_points = {}
      for _, b in pairs(bodies) do
         body_points[b.name] = {x = b.x, y = b.y, dx = b.dx, dy = b.dy, mass = b.mass}
      end

      love.graphics.setLineWidth(5)
      love.graphics.setColor(200, 200, 200, 200)
      for i=0, steps do
         for _, b in pairs(body_points) do
            local ddx, ddy = body.gravitate(b, x, y)
            dx = dx + ddx * ship.api.step_size / ship.mass
            dy = dy + ddy * ship.api.step_size / ship.mass
            b.x = b.x + (b.dx * ship.api.step_size * 100)
            b.y = b.y + (b.dy * ship.api.step_size * 100)
         end

         for _, b2 in ipairs(body_points) do
            if(b ~= b2 and (not b2.star)) then
               local ddx2, ddy2 = body.gravitate(b, b2.x, b2.y)
               b2.dx = b2.dx + (dt * ddx2 / b2.mass)
               b2.dy = b2.dy + (dt * ddy2 / b2.mass)
            end
         end
         last_x, last_y = x, y
         x = x + (dx * ship.api.step_size * 100)
         y = y + (dy * ship.api.step_size * 100)

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
