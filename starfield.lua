return {
   new = function(count, factor, intensity)
      local stars = {}
      for _ = 1, count do
         table.insert(stars, {x = love.math.random(16384),
                              y = love.math.random(16384) })
      end
      return {stars = stars, factor = factor, intensity = intensity}
   end,

   render = function(field, x, y, w, h)
      love.graphics.setColor(field.intensity, field.intensity, field.intensity);
      for _, star in ipairs(field.stars) do
         love.graphics.circle("fill",
                              (star.x - (x * field.factor)) % w,
                              (star.y - (y * field.factor)) % h, 1)
      end
   end
}
