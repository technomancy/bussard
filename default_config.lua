-- This code runs inside your ship's own computer.

ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right,
   ["lalt"] = ship.actions.laser,
   ["="] = function(d) if d then ui.scale = ui.scale - (ship.dt/2) end end,
   ["-"] = function(d) if d then ui.scale = ui.scale + (ship.dt/2) end end,
   ["["] = function(d) if d then ship.throttle = ship.throttle - (ship.dt/2) end end,
   ["]"] = function(d) if d then ship.throttle = ship.throttle - (ship.dt/2) end end,
}

ship.commands = {
   -- TODO: separate keymap for ctrl, alt
   ["`"] = function()
      if(ship.helm.isDown("lctrl", "rctrl", "capslock")) then
         ship.repl.toggle()
      else
         ship.repl.keypressed("`")
      end
   end,
   escape = function()
      if(ship.repl.toggled()) then
         ship.repl.toggle()
      else
         ui.quit()
      end
   end,
   tab = ship.actions.next_target,
   pause = function() ui.paused = (not ui.paused) end,
}

login = ship.actions.login
