ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right,
   ["="] = function(d) if d then game.scale = game.scale + (ship.dt / 2) end end,
   ["-"] = function(d) if d then game.scale = game.scale - (ship.dt / 2) end end,
}

ship.commands = {
   -- TODO: support declaration of modifier keybindings
   ["`"] = function()
      if(game.keyboard.isDown("lctrl", "rctrl")) then
         ship.repl.toggle()
      else
         ship.repl.keypressed("~")
      end
   end,
   tab = ship.actions.next_target,
   ["return"] = function()
      if(ship.repl.toggled()) then
         ship.repl.keypressed("return")
      else
         ship.actions.connect()
      end
   end,
   escape = function()
      if(ship.repl.toggled()) then
         ship.repl.toggle()
      else
         game.quit()
      end
   end,
   -- p = function() game.paused = (not game.paused) end,
}
