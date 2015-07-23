-- x, y, gravity, image
local new = function(x, y, gravity, image)
   return {x = x, y = y, gravity = gravity, image = image}
end

local is_onscreen = function(body, x, y)
   return true
end

local g = 0.02
local distance_factor = 20

return {
   load = function()
      return {new(0, 0, 12, love.graphics.newImage('assets/planet-1.png'))}
   end,

   draw = function(body, x, y)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
   end,

   gravitate = function(body, x, y)
      local dx = x
      local dy = y
      local distance = math.sqrt(dx*dx + dy*dy)
      local theta = math.atan2(dx, dy) + math.pi
      local f = (body.gravity * g) / math.log(distance * distance_factor)
      return (f * math.sin(theta)), (f * math.cos(theta))
   end,
}
