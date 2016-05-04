love.conf = function(t)
   t.title, t.identity = "Bussard", "bussard"

   if(os.getenv("SCREENSHOT")) then
      t.window.width, t.window.height = 615, 500
   elseif(os.getenv("BUSSARD_RES")) then
      local x, y = os.getenv("BUSSARD_RES"):match("(.+)x(.+)")
      t.window.width,t.window.height = tonumber(x or "1400"),tonumber(y or "800")
   else
      t.window.fullscreen, t.window.fullscreentype = true, "desktop"
   end

   t.modules.mouse, t.modules.joystick, t.modules.physics = false, false, false
end
