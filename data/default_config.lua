-- This code runs inside your ship's own computer.

ship.controls = {
   up = ship.actions.forward,
   left = ship.actions.left,
   right = ship.actions.right,
   ["="] = function(d) if d then ship.scale = ship.scale - (ship.dt/2) end end,
   ["-"] = function(d) if d then ship.scale = ship.scale + (ship.dt/2) end end,
}

local pause = function() ship.paused = (not ship.paused) end

-- Flight mode
define_mode("flight")
bind("flight", "`", function() ship:change_mode("console") end)
bind("flight", "return", function()
        ship.editor.open(ship, "src.config")
        ship:change_mode("edit")
end)

-- You can change this so escape by itself quits if you prefer.
bind("flight", "escape", function() print("Press ctrl-q to quit.") end)
bind("flight", "ctrl-q", ship.ui.quit)

bind("flight", "tab", ship.actions.next_target)
bind("flight", "ctrl-tab", ship.actions.closest_target)
bind("flight", "ctrl- ", ship.actions.login)

-- ship.modes.flight.textinput = function(text)
--    if(not ship.controls[text]) then
--       ship.modes.console.textinput(text)
--    end
-- end

-- -- Console mode
-- define_mode("console")
-- bind({"console", "flight"}, "pause", pause)
-- bind("console", "`", function() end)
-- bind("console", "~", function() end)

-- bind({"console", "flight"}, "return", ship.console.eval_line)
-- bind({"console", "flight"}, "backspace", ship.console.delete_backwards)
-- bind({"console", "flight"}, "ctrl-h", ship.console.delete_backwards)
-- bind({"console", "flight"}, "delete", ship.console.delete_forwards)
-- bind({"console", "flight"}, "ctrl-d", ship.console.delete_forwards)
-- bind({"console", "flight"}, "ctrl-k", ship.console.kill_line)

-- bind({"console", "flight"}, "ctrl-a", ship.console.move_beginning_of_line)
-- bind({"console", "flight"}, "home", ship.console.move_beginning_of_line)
-- bind({"console", "flight"}, "ctrl-e", ship.console.move_end_of_line)
-- bind({"console", "flight"}, "end", ship.console.move_end_of_line)

-- bind({"console", "flight"}, "ctrl-b", ship.console.backward_char)
-- bind({"console", "flight"}, "ctrl-f", ship.console.forward_char)
-- bind({"console", "flight"}, "alt-f", ship.console.forward_word)
-- bind({"console", "flight"}, "alt-b", ship.console.backward_word)

-- bind({"console", "flight"}, "ctrl-p", ship.console.history_prev)
-- bind({"console", "flight"}, "ctrl-n", ship.console.history_next)
-- bind({"console", "flight"}, "down", ship.console.history_next)

-- -- Not part of flight mode
-- bind("console", "up", ship.console.history_prev)
-- bind("console", "right", ship.console.forward_char)
-- bind("console", "left", ship.console.backward_char)

-- bind("console", "pageup", ship.console.scroll_up)
-- bind("console", "pagedown", ship.console.scroll_down)

-- bind("console", "tab", ship.console.complete)
-- bind("console", "escape", mode("flight"))
-- bind({"flight", "console"}, "ctrl-return", function() ship:e("src.config") end)

-- bind("console", "ctrl-l", ship.console.clear_lines)

-- keymap.modes["console"].textinput = ship.console.textinput

-- Edit mode
define_mode("edit", ship.editor.textinput, ship.editor.wrap)
bind("edit", "escape", function() ship:change_mode("flight") end)
bind("edit", "`", function() ship:change_mode("console") end)

bind("edit", "pause", pause)
bind("edit", "return", ship.editor.newline_and_indent)
bind("edit", "ctrl-z", ship.editor.undo)
bind("edit", "ctrl-r", ship.editor.revert)

-- "Conventional" keys
bind("edit", "backspace", ship.editor.delete_backwards)
bind("edit", "delete", ship.editor.delete_forwards)
bind("edit", "home", ship.editor.move_beginning_of_line)
bind("edit", "end", ship.editor.move_end_of_line)
bind("edit", "left", ship.editor.backward_char)
bind("edit", "right", ship.editor.forward_char)
bind("edit", "up", ship.editor.prev_line)
bind("edit", "down", ship.editor.next_line)

bind("edit", "ctrl- ", ship.editor.mark)
bind("edit", "ctrl-c", ship.editor.kill_ring_save)
bind("edit", "ctrl-x", ship.editor.kill_region)
bind("edit", "ctrl-v", ship.editor.yank)
bind("edit", "pageup", ship.editor.scroll_up)
bind("edit", "pagedown", ship.editor.scroll_down)

-- Emacs keys

bind("edit", "ctrl-h", ship.editor.delete_backwards)
bind("edit", "ctrl-d", ship.editor.delete_forwards)
bind("edit", "ctrl-k", ship.editor.kill_line)
bind("edit", "ctrl-a", ship.editor.move_beginning_of_line)
bind("edit", "ctrl-e", ship.editor.move_end_of_line)
bind("edit", "ctrl-b", ship.editor.backward_char)
bind("edit", "ctrl-f", ship.editor.forward_char)
bind("edit", "alt-f", ship.editor.forward_word)
bind("edit", "alt-b", ship.editor.backward_word)
bind("edit", "ctrl-p", ship.editor.prev_line)
bind("edit", "ctrl-n", ship.editor.next_line)
bind("edit", "alt-,", ship.editor.beginning_of_buffer)
bind("edit", "alt-.", ship.editor.end_of_buffer)

bind("edit", "ctrl- ", ship.editor.mark)
bind("edit", "ctrl-g", ship.editor.no_mark)
bind("edit", "alt-w", ship.editor.kill_ring_save)
bind("edit", "ctrl-w", ship.editor.kill_region)
bind("edit", "ctrl-y", ship.editor.yank)
bind("edit", "alt-y", ship.editor.yank_pop)

bind("edit", "ctrl-backspace", ship.editor.backward_kill_word)
bind("edit", "alt-d", ship.editor.forward_kill_word)

bind({"edit", "flight", -- "console"
     }, "ctrl-r",
   function()
      ship:load("src.config")
      ship.editor.initialize()
end)

bind("edit", "ctrl-l", function()
        ship.editor.newline()
        ship.editor.textinput("\f")
        ship.editor.newline()
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
