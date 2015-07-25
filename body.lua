local new = function(x, y, dx, dy, mass, scale, image, name)
   return {x = x, y = y, dx = 0, dy = 0,
           mass = mass, scale = scale,
           image = image, name = name}
end

local g = 0.02
local distance_factor = 100

return {
   load = function()
      return {new(3500, 0, 0, 50000, 5, 0.3,
                  love.graphics.newImage('assets/planet-1.png'), "Earth"),
              new(0, 0, 0, 0, 30, 1,
                  love.graphics.newImage('assets/sun.png'), "Sol")}
   end,

   draw = function(body, x, y)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
   end,

   gravitate = function(body, x, y)
      local dx = x - body.x
      local dy = y - body.x
      local distance = math.sqrt(dx*dx + dy*dy)
      local theta = math.atan2(dx, dy) + math.pi
      local f = (body.mass * g) / math.log(distance * distance_factor)
      return (f * math.sin(theta)), (f * math.cos(theta))
   end,
}
