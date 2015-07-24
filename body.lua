-- x, y, gravity, image
local new = function(x, y, gravity, image, name)
   return {x = x, y = y, gravity = gravity, image = image, name = name}
end

local g = 0.02
local distance_factor = 20

return {
   load = function()
      return {new(0, 0, 12, love.graphics.newImage('assets/planet-1.png'),
                  "Earth")}
   end,

   draw = function(body, x, y, selected)
      local bx = body.x - body.image:getWidth() / 2
      local by = body.y - body.image:getHeight() / 2
      love.graphics.draw(body.image, bx - x, by - y)
      love.graphics.setColor(100, 100, 100);
      if(selected) then
         love.graphics.rectangle("line", body.x, body.y,
                                 body.image:getWidth(), body.image:getHeight())
      end
   end,

   gravitate = function(body, x, y)
      local dx = x -- TODO: don't assume body lies at origin
      local dy = y
      local distance = math.sqrt(dx*dx + dy*dy)
      local theta = math.atan2(dx, dy) + math.pi
      local f = (body.gravity * g) / math.log(distance * distance_factor)
      return (f * math.sin(theta)), (f * math.cos(theta))
   end,
}
