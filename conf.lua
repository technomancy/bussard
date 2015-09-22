love.conf = function(t)
   t.title = "Bussard"
   t.version = "0.9.1"

   if(os.getenv("FULLSCREEN")) then
      t.window.fullscreen = true
      t.window.fullscreentype = "desktop"
   else
      -- for screenshots
      -- t.window.width = 615
      -- t.window.height = 500
      t.window.width = 1400
      t.window.height = 800
   end

   t.modules.mouse = false
   t.modules.joystick = false
   t.modules.physics = false
end
