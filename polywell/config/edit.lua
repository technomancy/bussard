-- Edit mode -*- lua -*-
local editor = require("polywell")
local lume = require("polywell.lume")

editor.define_mode("edit")

require("polywell.config.edit_functions")

-- most of the functions these are bound to should be self-explanatory
-- just by looking at the function name.

editor.bind("edit", "ctrl-o", editor.find_file)
editor.bind("edit", "ctrl-r", editor.reload)
editor.bind("edit", "ctrl-return", lume.fn(editor.change_buffer, "*console*"))

editor.bind({"edit", "minibuffer"}, "ctrl-q", function()
      editor.save()
      os.exit()
end)

-- cycle forwards and backwards through open buffers.
editor.bind("edit", "ctrl-pageup", lume.fn(editor.next_buffer, -1))
editor.bind("edit", "ctrl-pagedown", editor.next_buffer)

-- not all keyboards have these keys, but they're handy.
editor.bind("edit", "appback", lume.fn(editor.next_buffer, -1))
editor.bind("edit", "appforward", editor.next_buffer)

-- you can editor.bind return to just editor.newline to disable auto-indent.
editor.bind("edit", "return", editor.newline_and_indent)
editor.bind("edit", "ctrl-z", editor.undo)
editor.bind("edit", "ctrl-s", editor.save)
editor.bind("edit", "alt-r", editor.revert)

-- "Conventional" keys
editor.bind("edit", "backspace", editor.delete_backwards)
editor.bind("edit", "delete", editor.delete_forwards)

editor.bind("edit", "home", editor.beginning_of_line)
editor.bind("edit", "end", editor.end_of_line)
editor.bind("edit", "left", editor.backward_char)
editor.bind("edit", "right", editor.forward_char)
editor.bind("edit", "up", editor.prev_line)
editor.bind("edit", "down", editor.next_line)
editor.bind("edit", "wheelup", editor.prev_line)
editor.bind("edit", "wheeldown", editor.next_line)

editor.bind("edit", "ctrl- ", editor.mark)
editor.bind("edit", "ctrl-space", editor.mark)
editor.bind("edit", "ctrl-c", editor.kill_ring_save)
editor.bind("edit", "ctrl-x", editor.kill_region)
editor.bind("edit", "ctrl-v", editor.yank)

editor.bind("edit", "alt-v", editor.system_yank)
editor.bind("edit", "alt-c", editor.system_copy_region)

editor.bind("edit", "pageup", editor.scroll_up)
editor.bind("edit", "pagedown", editor.scroll_down)

editor.bind("edit", "ctrl-home", editor.beginning_of_buffer)
editor.bind("edit", "ctrl-end", editor.end_of_buffer)

editor.bind("edit", "ctrl-w", editor.close)
editor.bind("edit", "ctrl-alt-b", editor.switch_buffer)

-- force close even if unsaved changes exist.
editor.bind("edit", "ctrl-alt-w", function()
               editor.close(true)
end)

-- Search
editor.bind("edit", "ctrl-f", editor.search)
editor.bind("edit", "alt-r", editor.replace)

editor.bind("edit", "alt-g", function()
               editor.read_line("Go to line: ", function(l, cancel)
                                   if(not cancel) then
                                      editor.go_to(tonumber(l))
                                   end
               end)
end)
