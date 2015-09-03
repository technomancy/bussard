local utils = require "utils"

local hud_text = "speed: %0.2f | pos: %5.2f, %5.2f\n" ..
   "target: %s | distance: %0.2f"

local calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end

local vector_size = 50
local w, h = love.graphics:getWidth(), love.graphics:getHeight()

return {
   render = function(ship, target)
      local speed = calculate_distance(ship.dx, ship.dy)
      local distance, target_name
      if(target) then
         distance = calculate_distance(ship.x - target.x, ship.y - target.y)
         target_name = target.name
      else
         target_name, distance = "none", 0
      end

      love.graphics.setColor(255, 255, 255)

      love.graphics.print(string.format(hud_text, speed, ship.x, ship.y,
                                        target_name, distance),
                          5, 5)

      local messages = utils.drop(#ship.api.messages - 3, ship.api.messages)
      if(#messages < 3) then table.insert(messages, 1, "") end
      if(#messages < 3) then table.insert(messages, 1, "") end
      love.graphics.print(table.concat(messages, "\n"), 15, h-90)

      -- fuel readout
      love.graphics.setColor(255, 50, 50);
      love.graphics.rectangle("fill", 5, 50,
                              math.min(ship.fuel * 2, 200), 20)
      love.graphics.setColor(255, 200, 200);
      love.graphics.rectangle("line", 5, 50, 200, 20)
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
