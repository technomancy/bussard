-- This code runs inside your ship's own computer.

-- TODO: unify these with keymap stuff?
ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right,
   ["lalt"] = ship.actions.laser,
   ["="] = function(d) if d then ship.scale = ship.scale - (ship.dt/2) end end,
   ["-"] = function(d) if d then ship.scale = ship.scale + (ship.dt/2) end end,
   ["["] = function(d) if d then ship.throttle = ship.throttle - (ship.dt/2) end end,
   ["]"] = function(d) if d then ship.throttle = ship.throttle - (ship.dt/2) end end,

      -- TODO: new bindings can shadow old ones!
   h = ship.actions.left,
   t = ship.actions.forward,
   n = ship.actions.right,
   space = ship.actions.laser,
   ["/"] = function(d) if d then ship.scale = ship.scale - (ship.dt/2) end end,

}

local pause = function() ship.paused = (not ship.paused) end
local mode = function(mode)
   return function()
      ship.repl.enable(mode == "repl")
      keymap.change_mode(mode)
   end
end

-- Flight mode
keymap.define_mode("flight")
keymap.define("flight", "ctrl-`", mode("repl"))

keymap.define("flight", "escape", ui.quit)
keymap.define("flight", "tab", ship.actions.next_target)
keymap.define("flight", "ctrl-tab", ship.actions.closest_target)

keymap.modes["flight"].textinput = function(text)
   if(not ship.controls[text]) then
      ship.repl.textinput(text)
   end
end

-- Repl mode
keymap.define_mode("repl")
keymap.define({"repl", "flight"}, "pause", pause)

keymap.define({"repl", "flight"}, "return", ship.repl.eval_line)
keymap.define({"repl", "flight"}, "backspace", ship.repl.delete_backwards)
keymap.define({"repl", "flight"}, "ctrl-h", ship.repl.delete_backwards)
keymap.define({"repl", "flight"}, "delete", ship.repl.delete_forwards)
keymap.define({"repl", "flight"}, "ctrl-k", ship.repl.kill_line)

keymap.define({"repl", "flight"}, "ctrl-a", ship.repl.move_beginning_of_line)
keymap.define({"repl", "flight"}, "home", ship.repl.move_beginning_of_line)
keymap.define({"repl", "flight"}, "ctrl-e", ship.repl.move_end_of_line)
keymap.define({"repl", "flight"}, "end", ship.repl.move_end_of_line)

keymap.define({"repl", "flight"}, "up", ship.repl.history_prev)
keymap.define({"repl", "flight"}, "ctrl-p", ship.repl.history_prev)
keymap.define({"repl", "flight"}, "down", ship.repl.history_next)
keymap.define({"repl", "flight"}, "ctrl-n", ship.repl.history_next)
keymap.define({"repl", "flight"}, "left", ship.repl.backward_char)
keymap.define({"repl", "flight"}, "ctrl-b", ship.repl.backward_char)
keymap.define({"repl", "flight"}, "right", ship.repl.forward_char)
keymap.define({"repl", "flight"}, "ctrl-f", ship.repl.forward_char)
keymap.define({"repl", "flight"}, "alt-f", ship.repl.forward_word)
keymap.define({"repl", "flight"}, "alt-b", ship.repl.backward_word)

-- Not part of flight mode
keymap.define("repl", "pageup", ship.repl.scroll_up)
keymap.define("repl", "pagedown", ship.repl.scroll_down)
keymap.define("repl", "alt-v", ship.repl.scroll_up)
keymap.define("repl", "ctrl-v", ship.repl.scroll_down)

keymap.define("repl", "ctrl-`", mode("flight"))
keymap.define("repl", "escape", mode("flight"))

keymap.define("repl", "ctrl-l", ship.repl.clear)

keymap.modes["repl"].textinput = ship.repl.textinput

-- convenience functions
login = ship.actions.login
