local new = function(x, y, dx, dy, mass, image, name, description)
   return {x = x, y = y, dx = dx, dy = dy,
           mass = mass, image = image, name = name, description = description,}
end

local g = 0.002
local distance_factor = 10000

return {
   load = function()
      return {new(3500, 0, 0, 20, 5,
                  love.graphics.newImage('assets/planet-1.png'), "Earth",
                 "This is a pretty great planet."),
              new(0, 0, 0, 0, 30,
                  love.graphics.newImage('assets/sun.png'), "Sol")}
   end,

   draw = function(body, x, y)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
   end,

   gravitate = function(body, x, y, mass)
      local dx = x - body.x
      local dy = y - body.x
      local distance = math.sqrt(dx*dx + dy*dy)
      local theta = math.atan2(dx, dy) + math.pi
      local f = (body.mass * g) / math.log(distance)
      return (f * math.sin(theta)), (f * math.cos(theta)), theta
   end,
}
