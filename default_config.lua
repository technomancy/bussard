ship.controls["up"] = ship.actions.forward
ship.controls["left"] = ship.actions.left
ship.controls["right"] = ship.actions.right
ship.commands["tab"] = ship.actions.next_target
ship.commands["return"] = ship.actions.connect
ship.commands["escape"] = game.quit
ship.commands["p"] = function() game.paused = (not game.paused) end
