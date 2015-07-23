-- x, y, gravity, image
local new = function(x, y, gravity, image)
   return {x = x, y = y, gravity = gravity, image = image}
end

local is_onscreen = function(body, x, y)
   return true
end

return {
   load = function()
      return {new(12, 33, 12, love.graphics.newImage('assets/planet-1.png'))}
   end,

   draw = function(body, x, y)
      love.graphics.draw(body.image, body.x - x, body.y - y)
   end
}
