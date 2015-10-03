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
      ship.repl.on(mode == "repl")
      ship.edit.on(mode == "edit")
      keymap.change_mode(mode)
   end
end

local pass_or_login = function()
   if(ship.sensors.target.portal) then
      ship.actions.passponder()
   elseif(ship.sensors.target.os) then
      ship.actions.login()
   end
end

-- Flight mode
keymap.define_mode("flight")
keymap.define("flight", "ctrl-return", mode("repl"))

keymap.define("flight", "escape", ship.ui.quit)
keymap.define("flight", "tab", ship.actions.next_target)
keymap.define("flight", "ctrl-tab", ship.actions.closest_target)
keymap.define("flight", "ctrl- ", pass_or_login)

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

keymap.define({"repl", "flight"}, "ctrl-b", ship.repl.backward_char)
keymap.define({"repl", "flight"}, "ctrl-f", ship.repl.forward_char)
keymap.define({"repl", "flight"}, "alt-f", ship.repl.forward_word)
keymap.define({"repl", "flight"}, "alt-b", ship.repl.backward_word)

keymap.define({"repl", "flight"}, "ctrl-p", ship.repl.history_prev)
keymap.define({"repl", "flight"}, "down", ship.repl.history_next)
keymap.define({"repl", "flight"}, "ctrl-n", ship.repl.history_next)

-- Not part of flight mode
keymap.define("repl", "up", ship.repl.history_prev)
keymap.define("repl", "right", ship.repl.forward_char)
keymap.define("repl", "left", ship.repl.backward_char)

keymap.define("repl", "pageup", ship.repl.scroll_up)
keymap.define("repl", "pagedown", ship.repl.scroll_down)

keymap.define("repl", "ctrl-return", mode("flight"))
keymap.define("repl", "escape", mode("flight"))

keymap.define("repl", "ctrl-l", ship.repl.clear)

keymap.modes["repl"].textinput = ship.repl.textinput

-- Edit mode
keymap.define_mode("edit")
keymap.define("edit", "escape", mode("flight"))
keymap.define("edit", "pause", pause)
keymap.define("edit", "return", ship.edit.newline)

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

-- TODO
keymap.define("edit", "ctrl-backspace", ship.edit.backward_kill_word)
keymap.define("edit", "alt-d", ship.edit.forward_kill_word)
keymap.define("edit", "ctrl-r", ship.edit.revert)

keymap.modes["edit"].textinput = ship.edit.textinput

-- HUD
ship.hud = {
   ["60:5"] = {
      type="text", format="x: %5.2f y: %5.2f", values={"sensors.x", "sensors.y"}
   },
   ["60:20"] = {
      type="text", format="epoch: %s  credits: %s",
      values={function(ship)
            return utils.format_seconds(os.time() + ship.status.time_offset) end,
         "status.credits"}
   },
   ["5:5"] = {
      type="vector", values={"sensors.dx", "sensors.dy"},
      width=3, color={50, 255, 50}
   },
   ["-5:5"] = {
      type="vector", values={"sensors.target.dx", "sensors.target.dy"},
      width=3, color={50, 255, 150}
   },

   ["-180:5"] = {
      type="text", format="target: %s\ndistance: %0.2f",
      values={"sensors.target.name", function(s)
                 return s.sensors.target and utils.distance(s.sensors,
                                                            s.sensors.target)
      end}
   },
   ["5:60"] = {
      type="bar", values={"status.fuel", "status.fuel_capacity", ship.fuel_to_stop},
      color={255, 20, 20}
   },
   ["5:75"] = {
      type="bar", values={"status.battery", "status.battery_capacity"},
      color={20, 255, 20}
   },
   -- TODO: cargo
}

-- convenience functions
login = ship.actions.login

-- testing
keymap.define("flight", "ctrl-x", function() ship:e("src.config") end)
