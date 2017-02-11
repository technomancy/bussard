local lume = require("lume.init")

love.conf = function(t)
   t.gammacorrect = true
   t.title, t.identity = "Bussard", "bussard"
   t.modules.joystick, t.modules.physics = false, false
   t.modules.audio, t.modules.sound = false, false

   -- local data_arg = lume.find(arg, "--data")
   -- if(data_arg) then
   --    love.filesystem.mount(arg[data_arg+1], "data")
   -- end
   if(lume.find(arg, "--test") or lume.find(arg, "--fuzz")) then
      t.window, t.modules.window, t.modules.graphics = false, false, false
      t.identity = "bussard-test"
   end
end
