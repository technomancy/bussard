-- This code runs inside your ship's own computer.

local mode = function(mode)
   return function()
      if(keymap.current_mode == "edit") then ship.edit.save() end
      ship.console.on(mode == "console")
      ship.edit.on(mode == "edit")
      keymap.change_mode(mode)
   end
end

keymap.define_mode("flight") -- basically empty
keymap.define("flight", "`", mode("console"))
keymap.define("flight", "escape", ship.ui.quit)
keymap.define("flight", "ctrl-return", function() ship:e("config.lua") end)

-- Console mode
keymap.define_mode("console")

keymap.define("console", "`", mode("flight"))
keymap.define("console", "return", ship.console.eval_line)
keymap.define("console", "backspace", ship.console.delete_backwards)
keymap.define("console", "delete", ship.console.delete_forwards)
keymap.define("console", "ctrl-k", ship.console.kill_line)

keymap.define("console", "home", ship.console.move_beginning_of_line)
keymap.define("console", "end", ship.console.move_end_of_line)
keymap.define("console", "left", ship.console.backward_char)
keymap.define("console", "right", ship.console.forward_char)
keymap.define("console", "up", ship.console.history_prev)
keymap.define("console", "down", ship.console.history_next)
keymap.define("console", "pageup", ship.console.scroll_up)
keymap.define("console", "pagedown", ship.console.scroll_down)

keymap.define("console", "ctrl-return", mode("flight"))
keymap.define("console", "escape", mode("flight"))

keymap.define("console", "ctrl-l", ship.console.clear)

keymap.modes["console"].textinput = ship.console.textinput

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
