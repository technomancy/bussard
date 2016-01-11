-- This code runs inside your ship's own computer.

ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right,
   ["="] = function(d) if d then ship.scale = ship.scale - (ship.dt/2) end end,
   ["-"] = function(d) if d then ship.scale = ship.scale + (ship.dt/2) end end,
}

local pause = function() ship.paused = (not ship.paused) end

local mode = function(mode)
   return function()
      if(keymap.current_mode == "edit") then ship.edit.save() end
      ship.console.on(mode == "console")
      ship.edit.on(mode == "edit")
      keymap.change_mode(mode)
   end
end

-- Flight mode
keymap.define_mode("flight")
keymap.define("flight", "`", mode("console"))

-- You can change this so escape by itself quits if you prefer.
keymap.define("flight", "escape", function() print("Press ctrl-q to quit.") end)
keymap.define("flight", "ctrl-q", ship.ui.quit)

keymap.define("flight", "tab", ship.actions.next_target)
keymap.define("flight", "ctrl-tab", ship.actions.closest_target)
keymap.define("flight", "ctrl- ", ship.actions.login)

keymap.modes["flight"].textinput = function(text)
   if(not ship.controls[text]) then
      ship.console.textinput(text)
   end
end

-- Console mode
keymap.define_mode("console")
keymap.define({"console", "flight"}, "pause", pause)
keymap.define("console", "`", function() end)
keymap.define("console", "~", function() end)

keymap.define({"console", "flight"}, "return", ship.console.eval_line)
keymap.define({"console", "flight"}, "backspace", ship.console.delete_backwards)
keymap.define({"console", "flight"}, "ctrl-h", ship.console.delete_backwards)
keymap.define({"console", "flight"}, "delete", ship.console.delete_forwards)
keymap.define({"console", "flight"}, "ctrl-d", ship.console.delete_forwards)
keymap.define({"console", "flight"}, "ctrl-k", ship.console.kill_line)

keymap.define({"console", "flight"}, "ctrl-a", ship.console.move_beginning_of_line)
keymap.define({"console", "flight"}, "home", ship.console.move_beginning_of_line)
keymap.define({"console", "flight"}, "ctrl-e", ship.console.move_end_of_line)
keymap.define({"console", "flight"}, "end", ship.console.move_end_of_line)

keymap.define({"console", "flight"}, "ctrl-b", ship.console.backward_char)
keymap.define({"console", "flight"}, "ctrl-f", ship.console.forward_char)
keymap.define({"console", "flight"}, "alt-f", ship.console.forward_word)
keymap.define({"console", "flight"}, "alt-b", ship.console.backward_word)

keymap.define({"console", "flight"}, "ctrl-p", ship.console.history_prev)
keymap.define({"console", "flight"}, "ctrl-n", ship.console.history_next)
keymap.define({"console", "flight"}, "down", ship.console.history_next)

-- Not part of flight mode
keymap.define("console", "up", ship.console.history_prev)
keymap.define("console", "right", ship.console.forward_char)
keymap.define("console", "left", ship.console.backward_char)

keymap.define("console", "pageup", ship.console.scroll_up)
keymap.define("console", "pagedown", ship.console.scroll_down)

keymap.define("console", "tab", ship.console.complete)
keymap.define("console", "escape", mode("flight"))
keymap.define({"flight", "console"}, "ctrl-return", function() ship:e("src.config") end)

keymap.define("console", "ctrl-l", ship.console.clear)

keymap.modes["console"].textinput = ship.console.textinput

-- Edit mode
keymap.define_mode("edit", ship.edit.wrap)
keymap.define("edit", "escape", mode("flight"))
keymap.define("edit", "`", mode("console"))
keymap.define("edit", "pause", pause)
keymap.define("edit", "return", ship.edit.newline_and_indent)
keymap.define("edit", "ctrl-z", ship.edit.undo)
keymap.define("edit", "ctrl-r", ship.edit.revert)

-- "Conventional" keys
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

-- Emacs keys

keymap.define("edit", "ctrl-h", ship.edit.delete_backwards)
keymap.define("edit", "ctrl-d", ship.edit.delete_forwards)
keymap.define("edit", "ctrl-k", ship.edit.kill_line)
keymap.define("edit", "ctrl-a", ship.edit.move_beginning_of_line)
keymap.define("edit", "ctrl-e", ship.edit.move_end_of_line)
keymap.define("edit", "ctrl-b", ship.edit.backward_char)
keymap.define("edit", "ctrl-f", ship.edit.forward_char)
keymap.define("edit", "alt-f", ship.edit.forward_word)
keymap.define("edit", "alt-b", ship.edit.backward_word)
keymap.define("edit", "ctrl-p", ship.edit.prev_line)
keymap.define("edit", "ctrl-n", ship.edit.next_line)
keymap.define("edit", "alt-,", ship.edit.beginning_of_buffer)
keymap.define("edit", "alt-.", ship.edit.end_of_buffer)

keymap.define("edit", "ctrl- ", ship.edit.mark)
keymap.define("edit", "ctrl-g", ship.edit.no_mark)
keymap.define("edit", "alt-w", ship.edit.kill_ring_save)
keymap.define("edit", "ctrl-w", ship.edit.kill_region)
keymap.define("edit", "ctrl-y", ship.edit.yank)
keymap.define("edit", "alt-y", ship.edit.yank_pop)

keymap.define("edit", "ctrl-backspace", ship.edit.backward_kill_word)
keymap.define("edit", "alt-d", ship.edit.forward_kill_word)

keymap.define({"edit", "flight", "console"}, "ctrl-r",
   function()
      ship:load("src.config")
      ship.console.initialize()
      ship.edit.initialize()
end)

keymap.modes["edit"].textinput = ship.edit.textinput
keymap.define("edit", "ctrl-l", function()
                 ship.edit.newline()
                 ship.edit.textinput("\f")
                 ship.edit.newline()
end)

-- HUD
ship.hud = {
   { x=60, y=5, type="text",
     format="x: %5.0f y: %5.0f", values={"status.x", "status.y"}
   },
   { x=60, y=20, type="text",
     format="epoch: %s  credits: %s",
     values={function() return utils.format_seconds(os.time()) end,
        "status.credits"}
   },
   { x=5, y=5, type="vector",
     values={"status.dx", "status.dy"},
     width=3, color={50, 255, 50}
   },
   { x=-5, y=5, type="vector",
     values={"status.target.dx", "status.target.dy"},
     width=3, color={50, 255, 150}
   },

   { x=-70, y=5, type="text", align="right",
     format="%s\ndistance: %0.0f\nmass: %0.0f",
     values={"status.target.name", function(s)
                return s.status.target and utils.distance(s.status,
                                                          s.status.target)
     end, "status.target.mass"}
   },
   { x=5,y=60, type="bar",
     values={"status.fuel", "status.fuel_capacity", ship.fuel_to_stop},
     color={255, 20, 20}
   },
   { x=5, y=75, type="bar",
     values={"status.battery", "status.battery_capacity"},
     color={20, 255, 20}, h=6,
   },
   ship.mission.hud,
}

-- convenience functions
login = ship.actions.login
