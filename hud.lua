local hud_text = "speed: %0.2f | pos: %5.2f %5.2f\n" ..
   "target: %s | distance: %0.2f\n" ..
   "fuel: %0.2f"
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

      -- TODO: graphical fuel readout
      love.graphics.print(string.format(hud_text, speed, player.x, player.y,
                                        target_name, distance, player.fuel),
                          5, 5)
   end
}
