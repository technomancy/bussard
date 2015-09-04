ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right
}

ship.commands = {
   tab = ship.actions.next_target,
   ["return"] = ship.actions.connect,
   escape = game.quit,
   p = function() game.paused = (not game.paused) end
}
