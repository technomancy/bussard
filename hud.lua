local font_size = 24
local font = love.graphics.newFont("jura-demibold.ttf", font_size)
love.graphics.setFont(font)

local hud_text = "speed: %0.2f | target: %s | distance: %0.2f"
local calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end

return {
   render = function(w,h, player, target)
      local speed = calculate_distance(player.dx, player.dy)
      local distance
      if(target) then
         distance = calculate_distance(player.x - target.x, player.y - target.y)
         target = target.name
      else
         target, distance = "none", 0
      end
      love.graphics.print(string.format(hud_text, speed, target, distance),
                          5, h - (font_size + 5))
   end
}
