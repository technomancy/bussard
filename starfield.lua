return {
   new = function(count, width, height, factor, intensity)
      local stars = {}
      for _ = 1, count do
         table.insert(stars, {x = math.random(width),
                              y = math.random(height) })
      end
      return {stars = stars, width = width, height = height,
              factor = factor, intensity = intensity}
   end,

   render = function(field, x, y)
      if(not factor) then factor = 1 end
      love.graphics.setColor(field.intensity, field.intensity, field.intensity);
      for _, star in ipairs(field.stars) do
         love.graphics.circle("fill",
                              (star.x - (x * field.factor)) % field.width,
                              (star.y - (y * field.factor)) % field.height, 1)
      end
   end
}
