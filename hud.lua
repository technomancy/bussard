local hud_text = "speed: %0.2f | pos: %5.2f, %5.2f\n" ..
   "target: %s | distance: %0.2f"

local calculate_distance = function(x, y) return math.sqrt(x*x+y*y) end

return {
   render = function(player, target)
      local speed = calculate_distance(player.dx, player.dy)
      local distance, target_name
      if(target) then
         distance = calculate_distance(player.x - target.x, player.y - target.y)
         target_name = target.name
      else
         target_name, distance = "none", 0
      end

      love.graphics.print(string.format(hud_text, speed, player.x, player.y,
                                        target_name, distance),
                          5, 5)

      -- fuel readout
      love.graphics.setColor(255, 50, 50);
      love.graphics.rectangle("fill", 5, 50, player.fuel * 2, 20)
      love.graphics.setColor(255, 200, 200);
      love.graphics.rectangle("line", 5, 50, 200, 20)
   end
}
