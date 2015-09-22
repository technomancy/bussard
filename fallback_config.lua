-- This code runs inside your ship's own computer.

local mode = function(mode)
   return function()
      if(keymap.current_mode == "edit") then ship.edit.save() end
      ship.repl.on(mode == "repl")
      ship.edit.on(mode == "edit")
      keymap.change_mode(mode)
   end
end

keymap.define_mode("flight") -- basically empty
keymap.define("flight", "ctrl-return", mode("repl"))
keymap.define("flight", "escape", ship.ui.quit)

-- Repl mode
keymap.define_mode("repl")

keymap.define("repl", "return", ship.repl.eval_line)
keymap.define("repl", "backspace", ship.repl.delete_backwards)
keymap.define("repl", "delete", ship.repl.delete_forwards)
keymap.define("repl", "ctrl-k", ship.repl.kill_line)

keymap.define("repl", "home", ship.repl.move_beginning_of_line)
keymap.define("repl", "end", ship.repl.move_end_of_line)
keymap.define("repl", "left", ship.repl.backward_char)
keymap.define("repl", "right", ship.repl.forward_char)
keymap.define("repl", "up", ship.repl.history_prev)
keymap.define("repl", "down", ship.repl.history_next)
keymap.define("repl", "pageup", ship.repl.scroll_up)
keymap.define("repl", "pagedown", ship.repl.scroll_down)

keymap.define("repl", "ctrl-return", mode("flight"))
keymap.define("repl", "escape", mode("flight"))

keymap.define("repl", "ctrl-l", ship.repl.clear)

keymap.modes["repl"].textinput = ship.repl.textinput

-- Edit mode
keymap.define_mode("edit")
keymap.define("edit", "escape", mode("flight"))
keymap.define("edit", "return", ship.edit.newline)

keymap.define("edit", "backspace", ship.edit.delete_backwards)
keymap.define("edit", "delete", ship.edit.delete_forwards)
keymap.define("edit", "home", ship.edit.move_beginning_of_line)
keymap.define("edit", "end", ship.edit.move_end_of_line)
keymap.define("edit", "left", ship.edit.backward_char)
keymap.define("edit", "right", ship.edit.forward_char)
keymap.define("edit", "up", ship.edit.prev_line)
keymap.define("edit", "down", ship.edit.next_line)

keymap.define("edit", "ctrl- ", ship.edit.mark)
keymap.define("edit", "ctrl-c", ship.edit.kill_ring_save)
keymap.define("edit", "ctrl-x", ship.edit.kill_region)
keymap.define("edit", "ctrl-v", ship.edit.yank)
keymap.define("edit", "pageup", ship.edit.scroll_up)
keymap.define("edit", "pagedown", ship.edit.scroll_down)

keymap.modes["edit"].textinput = ship.edit.textinput

-- convenience functions
login = ship.actions.login

-- testing
keymap.define("flight", "ctrl-x", function() ship:e("config.lua") end)
