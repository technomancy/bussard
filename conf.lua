local lume = require("lume.init")

love.conf = function(t)
   t.gammacorrect = true
   t.title, t.identity = "Bussard", "bussard"
   t.modules.joystick, t.modules.physics = false, false
   t.modules.audio, t.modules.sound = false, false

   -- need to run this: luarocks install --local lua-repl
   if(lume.find(arg, "--headless")) then
      require("luarocks.loader")
      local repl = require("repl.console")
      repl:loadplugin("history")
      repl:loadplugin("autoreturn")
      repl:loadplugin("pretty_print")
      t.window, love.run = false, function() repl:run() end
   elseif(lume.find(arg, "--test") or lume.find(arg, "--fuzz")) then
      t.window, t.modules.window, t.modules.graphics = false, false, false
      t.identity = "bussard-test"
   end
end
