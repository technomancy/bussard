love.conf = function(t)
   t.title = "Bussard"
   t.version = "0.9.1"

   if(os.getenv("USER") == "phil") then
      t.window.width = 1400
      t.window.height = 800
      -- for screenshots
      -- t.window.width = 615
      -- t.window.height = 500
   else
      t.window.fullscreen = true
      t.window.fullscreentype = "desktop"
   end

   t.modules.mouse = false
   t.modules.joystick = false
   t.modules.physics = false
end
