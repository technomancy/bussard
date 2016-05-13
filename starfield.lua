return {
   new = function(count, factor, intensity)
      local stars = {}
      local w,h = love.graphics.getWidth(), love.graphics.getHeight()
      for _ = 1, count do
         table.insert(stars, {x = love.math.random(w),
                              y = love.math.random(h) })
      end
      return {stars = stars, width = h, height = h,
              factor = factor, intensity = intensity}
   end,

   render = function(field, x, y)
      love.graphics.setColor(field.intensity, field.intensity, field.intensity);
      for _, star in ipairs(field.stars) do
         love.graphics.circle("fill",
                              (star.x - (x * field.factor)) % field.width,
                              (star.y - (y * field.factor)) % field.height, 1)
      end
   end
}
