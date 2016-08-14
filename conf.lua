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
   elseif(lume.find(arg, "--no-game")) then
      function love.run()
         local ship = require("ship")
         love.load()
         love.keypressed = ship.api.editor.handle_key
         love.textinput = ship.api.editor.handle_textinput
         love.wheelmoved = ship.api.editor.handle_wheel
         love.keyreleased = function() end
         ship.api.editor.open(ship, "*console*")

         while true do
            love.event.pump()
            for name, a,b,c,d,e,f in love.event.wait do
               if(name == "quit") then
                  if(not love.quit()) then os.exit() end
               elseif(name == "keypressed") then
                  love.keypressed(a,b,c,d,e,f)
               elseif(name == "keyreleased") then
                  love.keyreleased(a,b,c,d,e,f)
               elseif(name == "textinput") then
                  love.textinput(a,b,c,d,e,f)
               elseif(name == "wheelmoved") then
                  love.wheelmoved(a,b,c,d,e,f)
               end

               love.graphics.clear(love.graphics.getBackgroundColor())
               love.graphics.origin()
               ship.api.editor.draw(ship)
               love.graphics.present()
            end
         end
      end
   end
end
