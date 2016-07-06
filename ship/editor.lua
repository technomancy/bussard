local utf8 = require("utf8.init")
local lume = require("lume")
local utils = require("utils")

--- Essentially a port of Emacs to Lua/Love.
-- missing features (a very limited list)
-- * search/replace
-- * syntax highlighting

local kill_ring = {}

-- TODO: move mode definitions from ship table to editor.
local make_buffer = function(fs, path, lines)
   return { fs=fs, path=path, mode = "edit",
            lines = lines or lume.split((fs and fs:find(path) or ""), "\n"),
            point = 0, point_line = 1, mark = nil, mark_line = nil,
            last_yank = nil, mark_ring = {},
            history = {}, undo_at = 0, dirty = false, needs_save = false,
            input_history = utils.buffer:new(), input_history_pos = 0,
            modeline = function(b)
               return utf8.format(" %s  %s  (%s/%s)  %s", b.needs_save and "*" or "-",
                                  b.path, b.point_line, #b.lines, b.mode)
            end
   }
end

-- how many lines do pageup/pagedown scroll?
local scroll_size = 20
-- How many pixels of padding are on either side
local PADDING = 20
-- how far down do you go before it starts to scroll?
local SCROLL_POINT = 0.8
-- How many pixels are required to display a row
local ROW_HEIGHT
-- Maximum amount of rows that can be displayed on the screen
local DISPLAY_ROWS
-- width of an m
local em
-- pattern for word breaks
local word_break = "[%s%p]"

local kill_ring_max = 32
local mark_ring_max = 32
local history_max = 128

local console = make_buffer(nil, "*console*",
                            {"This is the console. Enter any code for your " ..
                             "ship's computer to run it; run man() for help.", "> "})
console.prevent_close, console.point, console.point_line = true, 2, 2
console.mode, console.prompt, console.max_lines = "console", "> ", 512

local mb
local last_buffer -- for returning to after leaving minibuffer
local buffers = {console}
local b = nil -- default back to flight mode

local inhibit_read_only

local last_line = "Press ctrl-enter to open the console, " ..
"and run man() for help. Zoom with = and -."

local invisible = {}             -- sentinel "do not print" value

local state = function()
   return {lines = lume.clone(b.lines), point = b.point, point_line = b.point_line}
end

local undo = function()
   local prev = b.history[#b.history-b.undo_at]
   if(b.undo_at < #b.history) then b.undo_at = b.undo_at + 1 end
   if(prev) then
      b.lines, b.point, b.point_line = prev.lines, prev.point, prev.point_line
   end
end

local wrap = function(fn, ...)
   b.dirty = false
   local last_state = state()
   if(fn ~= undo) then b.undo_at = 0 end
   fn(...)
   if(b and b.dirty) then
      table.insert(b.history, last_state)
   end
   if(b and #b.history > history_max) then
      table.remove(b.history, 1)
   end
   if(b and b.max_lines) then
      for _=1,(#b.lines - b.max_lines) do
         table.remove(b.lines, 1)
         if(b.point_line >= 1) then b.point_line = b.point_line - 1 end
         if(b.mark_line) then b.mark_line = b.mark_line - 1 end
      end
   end
end

local debug = function()
   print("---------------", b.point_line, b.point)
   for _,line in ipairs(b.lines) do
      print(line)
   end
   print("---------------")
end

local get_buffer = function(path)
   return lume.match(buffers, function(bu) return bu.path == path end)
end

local with_current_buffer = function(nb, f)
   if(type(nb) == "string") then
      nb = get_buffer(nb)
   end
   local old_b = b
   b = nb
   local val = f()
   b = old_b
   return val
end

local replace_input = function(input)
   b.lines[#b.lines] = b.prompt .. input
   b.point_line, b.point = #b.lines, #b.lines[#b.lines]
end

local region = function()
   b.mark = math.min(utf8.len(b.lines[b.mark_line]), b.mark)

   if(b.point_line == b.mark_line) then
      local start, finish = math.min(b.point, b.mark), math.max(b.point, b.mark)
      local r = {utf8.sub(b.lines[b.point_line], start + 1, finish)}
      return r, b.point_line, start, b.point_line, finish
   elseif(b.mark == nil or b.mark_line == nil) then
      return {}, b.point_line, b.point, b.point_line, b.point
   else
      local start_line, start, finish_line, finish
      if(b.point_line < b.mark_line) then
         start_line, start, finish_line,finish = b.point_line,b.point,b.mark_line,b.mark
      else
         start_line, start, finish_line,finish = b.mark_line,b.mark,b.point_line,b.point
      end
      local r = {utf8.sub(b.lines[start_line], start + 1, -1)}
      for i = start_line+1, finish_line-1 do
         table.insert(r, b.lines[i])
      end
      table.insert(r, utf8.sub(b.lines[finish_line], 0, finish))
      return r, start_line, start, finish_line, finish
   end
end

-- would be nice to have a more general read-only property
local in_prompt = function(line, point, line2, _point2)
   if(not b.prompt) then return false end
   if((line2 or line) == line and line ~= #b.lines) then return false end
   if(line == #b.lines and point >= utf8.len(b.prompt)) then return false end
   return true
end

local edit_disallowed = function(line, point, line2, _point2)
   if(inhibit_read_only) then return false end
   return b.read_only or in_prompt(line, point, line2, _point2)
end

local insert = function(text, point_to_end)
   if(in_prompt(b.point_line, b.point)) then b.point = #b.prompt end
   if(edit_disallowed(b.point_line, b.point)) then return end
   b.dirty, b.needs_save = true, true
   text = lume.map(text, function(s) return utf8.gsub(s, "\t", "  ") end)
   if(not text or #text == 0) then return end
   local this_line = b.lines[b.point_line]
   local before = utf8.sub(this_line, 0, b.point)
   local after = utf8.sub(this_line, b.point + 1)
   local first_line = text[1]

   if(#text == 1) then
      b.lines[b.point_line] = (before or "") .. (first_line or "") .. (after or "")
      if(point_to_end) then
         b.point = utf8.len(before) + utf8.len(first_line)
      end
   else
      b.lines[b.point_line] = (before or "") .. (first_line or "")
      for i,l in ipairs(text) do
         if(i > 1 and i < #text) then
            table.insert(b.lines, i+b.point_line-1, l)
         end
      end
      table.insert(b.lines, b.point_line+#text-1, text[#text] .. (after or ""))
      if(point_to_end) then
         b.point = #text[#text]
         b.point_line = b.point_line+#text-1
      end
   end
end

local delete = function(start_line, start, finish_line, finish)
   start_line = math.min(start_line, finish_line)
   finish_line = math.max(start_line, finish_line)
   if(start_line == finish_line) then
      start, finish = math.min(start, finish), math.max(start, finish)
   end
   if(edit_disallowed(start_line, start, finish_line, finish)) then return end

   b.dirty, b.needs_save = true, true
   if(start_line == finish_line) then
      local line = b.lines[b.point_line]
      b.lines[b.point_line] = utf8.sub(line, 0, start) .. utf8.sub(line, finish + 1)
   else
      local after = utf8.sub(b.lines[finish_line], finish+1, -1)
      for i = finish_line, start_line + 1, -1 do
         table.remove(b.lines, i)
      end
      b.lines[start_line] = utf8.sub(b.lines[start_line], 0, start) .. after
   end
   b.point, b.point_line, b.mark, b.mark_line = start, start_line, start, start_line
end

local push = function(ring, item, max)
   table.insert(ring, item)
   if(#ring > max) then table.remove(ring, 1) end
end

local yank = function()
   local text = kill_ring[#kill_ring]
   if(text) then
      b.last_yank = {b.point_line, b.point,
                     b.point_line + #text - 1, utf8.len(text[#text])}
      insert(text, true)
   end
end

local beginning_of_buffer = function()
   return b.point == 0 and b.point_line == 1
end

local end_of_buffer = function()
   return b.point == #b.lines[b.point_line] and b.point_line == #b.lines
end

local forward_char = function(n) -- lameness: n must be 1 or -1
   n = n or 1
   if((end_of_buffer() and n > 0) or
      beginning_of_buffer() and n < 0) then return
   elseif(b.point == #b.lines[b.point_line] and n > 0) then
      b.point, b.point_line = 0, b.point_line+1
   elseif(b.point == 0 and n < 0) then
      b.point = #b.lines[b.point_line-1]
      b.point_line = b.point_line-1
   else
      b.point = b.point + n
   end
end

local point_over = function()
   return utf8.sub(b.lines[b.point_line], b.point + 1, b.point + 1)
end

local forward_word = function()
   if(utf8.find(point_over(), word_break)) then
      while(not end_of_buffer() and utf8.find(point_over(), word_break)) do
         forward_char()
      end
   end
   while(not end_of_buffer() and not utf8.find(point_over(), word_break)) do
      forward_char()
   end
end

local backward_word = function()
   if(utf8.find(point_over(), word_break)) then
      while(not beginning_of_buffer() and utf8.find(point_over(), word_break)) do
         forward_char(-1)
      end
   end
   while(not beginning_of_buffer() and not utf8.find(point_over(), word_break)) do
      forward_char(-1)
   end
end

local save = function(this_fs, this_path)
   local target = this_fs or b.fs
   if(not target) then return end
   b.needs_save = false
   if(target ~= "host") then
      local parts = lume.split(this_path or b.path, ".")
      local filename = table.remove(parts, #parts)
      for _,part in ipairs(parts) do
         target = target[part]
      end
      target[filename] = table.concat(b.lines, "\n")
   else
      local file = io.open(this_path or b.path, "w")
      if(file) then
         file:write(table.concat(b.lines, "\n"))
         file:close()
      else
         print("Could not save " .. this_path or b.path)
      end
   end
end

local newline = function(n)
   local t = {""}
   for _=1,(n or 1) do table.insert(t, "") end
   insert(t, true)
end

local save_excursion = function(f) -- TODO: discards multiple values from f
   local old_b, p, pl, m, ml = b, b and b.point, b and b.point_line, b and b.mark, b and b.mark_line
   local val, err = pcall(f)
   b = old_b
   if(b) then
      b.point, b.point_line, b.mark, b.mark_line = p, pl, m, ml
   end
   if(not val) then error(err) end
   return val
end

-- write to the current point in the current buffer
local write = function(...)
   local lines = lume.split(table.concat({...}, " "), "\n")
   local read_only = inhibit_read_only
   inhibit_read_only = true
   insert(lines, true)
   inhibit_read_only = read_only
   return lume.last(lines), #lines
end

-- write to the end of the console buffer right before the prompt
local io_write = function(...)
   local prev_b = b
   b = console
   local line_count
   local old_point, old_point_line, old_lines = b.point, b.point_line, #b.lines
   b.point, b.point_line = #b.lines[#b.lines - 1], #b.lines - 1
   last_line, line_count = write(...)
   if(old_point_line == old_lines) then
      b.point, b.point_line = old_point, #b.lines
   end
   b = prev_b
   if(b) then b.point_line = old_point_line + line_count - 1 end
end

return {
   initialize = function()
      ROW_HEIGHT = love.graphics.getFont():getHeight()
      em = love.graphics.getFont():getWidth('a')
   end,

   open = function(fs, path)
      b = get_buffer(path)
      if(not b) then
         if(fs ~= "host") then
            b = make_buffer(fs, path)
            table.insert(buffers, b)
         else -- from the host filesystem
            local lines = {}
            local file = io.open(path, "r")
            if(file) then
               for line in file:lines() do table.insert(lines, line) end
            else
               table.insert(lines, "")
            end
            if(file) then file:close() end
            b = make_buffer(fs, path, lines)
            table.insert(buffers, b)
         end
      end
   end,

   close = function(confirm)
      if(b.prevent_close) then return end
      if(confirm or not b.needs_save) then
         lume.remove(buffers, b)
         b = buffers[#buffers]
      end
   end,

   revert = function()
      b.lines = lume.split(b.fs:find(b.path), "\n")
   end,

   save = save,

   -- edit commands
   delete_backwards = function()
      if(beginning_of_buffer()) then return end
      local line, point = b.point_line, b.point
      local line2, point2
      save_excursion(function()
            forward_char(-1)
            line2, point2 = b.point_line, b.point
      end)
      delete(line2, point2, line, point)
   end,

   delete_forwards = function()
      if(end_of_buffer()) then return end
      local line, point = b.point_line, b.point
      local line2, point2
      save_excursion(function()
            forward_char()
            line2, point2 = b.point_line, b.point
      end)
      delete(line, point, line2, point2)
   end,

   kill_line = function()
      local remainder = utf8.sub(b.lines[b.point_line], b.point+1)
      if(utf8.find(remainder, "[^%s]")) then
         save_excursion(function()
               b.mark, b.mark_line = b.point, b.point_line
               b.point = #b.lines[b.point_line]
               push(kill_ring, region(), kill_ring_max)
         end)
         delete(b.point_line, b.point, b.point_line, #b.lines[b.point_line])
      elseif(b.point_line < #b.lines) then
         delete(b.point_line, b.point, b.point_line+1, 0)
      end
   end,

   beginning_of_line = function()
      b.point = 0
   end,

   end_of_line = function()
      b.point = #b.lines[b.point_line]
   end,

   prev_line = function()
      if(b.point_line > 1) then b.point_line = b.point_line - 1 end
   end,

   next_line = function()
      if(b.point_line < #b.lines) then b.point_line = b.point_line + 1 end
   end,

   scroll_up = function()
      b.point_line = math.max(0, b.point_line - scroll_size)
   end,

   scroll_down = function()
      b.point_line = math.min(#b.lines, b.point_line + scroll_size)
   end,

   forward_char = forward_char,
   backward_char = lume.fn(forward_char, -1),
   forward_word = forward_word,
   backward_word = backward_word,

   backward_kill_word = function()
      local original_point_line, original_point = b.point_line, b.point
      backward_word()
      delete(b.point_line, b.point, original_point_line, original_point)
   end,

   forward_kill_word = function()
      local original_point_line, original_point = b.point_line, b.point
      forward_word()
      delete(original_point_line, original_point, b.point_line, b.point)
   end,

   beginning_of_buffer = function()
      b.point, b.point_line = 0, 1
      return b.point, b.point_line
   end,

   end_of_buffer = function()
      b.point, b.point_line = #b.lines[#b.lines], #b.lines
      return b.point, b.point_line
   end,

   beginning_of_input = function()
      if(b.point_line == #b.lines and b.prompt) then
         b.point = #b.prompt
      else
         b.point = 0
      end
   end,

   newline = newline,

   newline_and_indent = function()
      local indentation = utf8.len(utf8.match(b.lines[b.point_line], "^ +") or "")
      newline()
      local existing_indentation = utf8.len(utf8.match(b.lines[b.point_line], "^ +") or "")
      insert({utf8.rep(" ", indentation - existing_indentation)})
      b.point = b.point + indentation
   end,

   mark = function()
      push(b.mark_ring, {b.point, b.point_line}, mark_ring_max)
      b.mark, b.mark_line = b.point, b.point_line
   end,

   unmark = function()
      b.mark, b.mark_line = nil, nil
   end,

   jump_to_mark = function()
      b.point, b.point_line = b.mark or b.point, b.mark_line or b.point_line
      if(#b.mark_ring > 0) then
         table.insert(b.mark_ring, 1, table.remove(b.mark_ring))
         b.mark, b.mark_line = unpack(b.mark_ring[1])
      end
   end,

   no_mark = function()
      b.mark, b.mark_line = nil, nil
   end,

   kill_ring_save = function()
      if(b.mark == nil or b.mark_line == nil) then return end
      push(kill_ring, region(), kill_ring_max)
   end,

   kill_region = function()
      if(b.mark == nil or b.mark_line == nil) then return end
      local _, start_line, start, finish_line, finish = region()
      push(kill_ring, region(), kill_ring_max)
      delete(start_line, start, finish_line, finish)
   end,

   yank = yank,

   yank_pop = function()
      table.insert(kill_ring, 1, table.remove(kill_ring))
      local ly_line1, ly_point1, ly_line2, ly_point2 = unpack(b.last_yank)
      delete(ly_line1, ly_point1, ly_line2, ly_point2)
      yank()
   end,

   print_kill_ring = function()
      print("Ring:")
      for i,l in ipairs(kill_ring) do
         print(i, lume.serialize(l))
      end
   end,

   eval_buffer = function()
      assert(b.fs and b.fs.load, "No loading context available.")
      b.fs:load(b.path)
   end,

   undo = undo,

   -- internal functions
   draw = function()
      if(not b) then
         love.graphics.setColor(0, 200, 0)
         if(console.lines[#console.lines] == console.prompt) then
            love.graphics.print(last_line, PADDING,
                                love.graphics:getHeight() - ROW_HEIGHT * 2)
         else
            love.graphics.print(console.lines[#console.lines], PADDING,
                                love.graphics:getHeight() - ROW_HEIGHT * 2)
         end
         return
      end

      local width, height = love.graphics:getWidth(), love.graphics:getHeight()
      DISPLAY_ROWS = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)

      -- enforce consistency
      if(b.point_line < 1) then b.point_line = 1 end
      if(b.point_line > #b.lines) then b.point_line = #b.lines end
      if(b.point < 0) then b.point = 0 end
      if(b.point > utf8.len(b.lines[b.point_line])) then
         b.point = utf8.len(b.lines[b.point_line]) end

      -- Draw background
      love.graphics.setColor(0, 0, 0, 240)
      love.graphics.rectangle("fill", 0, 0, width, height)

      -- maximum characters in a rendered line of text
      local render_line = function(ln2, y)
         if(ln2 == "\f\n" or ln2 == "\f") then
            love.graphics.line(PADDING, y + 0.5 * ROW_HEIGHT,
                               width - PADDING, y + 0.5 * ROW_HEIGHT)
         else
            love.graphics.print(ln2, PADDING, y)
         end
      end

      local edge = math.ceil(DISPLAY_ROWS * SCROLL_POINT)

      if(b.minibuffer) then
         mb, b = b, buffers[1]
      end

      local offset = (b.point_line < edge and 0) or (b.point_line - edge)
      for i,line in ipairs(b.lines) do
         if(i >= offset) then
            local y = ROW_HEIGHT * (i - offset)
            if(y >= height - ROW_HEIGHT) then break end
            -- elseif(y > height) then break end
            -- mark
            if(i == b.mark_line) then
               love.graphics.setColor(0, 125, 0)
               love.graphics.rectangle("line", PADDING+b.mark*em, y,
                                       em, ROW_HEIGHT)
            end
            if(i == b.point_line) then
               -- point_line line
               love.graphics.setColor(0, 50, 0, 190)
               love.graphics.rectangle("fill", 0, y, width, ROW_HEIGHT)
               -- point
               if(not mb) then
                  love.graphics.setColor(0, 125, 0)
                  love.graphics.rectangle("fill", PADDING+b.point*em, y,
                                          em, ROW_HEIGHT)
               end
            end
            love.graphics.setColor(0, 200, 0)
            render_line(line, y)
         end
      end

      love.graphics.setColor(0, 200, 0)
      love.graphics.rectangle("fill", 0, height - ROW_HEIGHT, width, ROW_HEIGHT)
      love.graphics.setColor(0, 0, 0)
      if(mb) then
         love.graphics.print(mb.lines[1], PADDING, height - ROW_HEIGHT)
         love.graphics.setColor(0, 225, 0)
         love.graphics.rectangle("fill", PADDING+mb.point*em,
                                 height - ROW_HEIGHT, em, ROW_HEIGHT)
      else
         love.graphics.print(b:modeline(), PADDING, height - ROW_HEIGHT)
      end

      -- draw scroll bar

      -- this only gives you an estimate since it uses the amount of
      -- lines entered rather than the lines drawn, but close enough

      -- height is percentage of the possible lines
      local bar_height = math.min(100, (DISPLAY_ROWS * 100) / #b.lines)
      -- convert to pixels (percentage of screen height, minus 10px padding)
      local bar_height_pixels = (bar_height * (height - 10)) / 100

      local sx = width - 5
      love.graphics.setColor(0, 150, 0)
      -- Handle the case where there are less actual lines than display rows
      if bar_height_pixels >= height - 10 then
         love.graphics.line(sx, 5, sx, height - 5)
      else
         -- now determine location on the screen by taking the offset in
         -- history and converting it first to a percentage of total
         -- lines and then a pixel offset on the screen
         local bar_end = (b.point_line * 100) / #b.lines
         bar_end = ((height - 10) * bar_end) / 100

         local bar_begin = bar_end - bar_height_pixels
         -- Handle overflows
         if bar_begin < 5 then
            love.graphics.line(sx, 5, sx, bar_height_pixels)
         elseif bar_end > height - 5 then
            love.graphics.line(sx, height - 5 - bar_height_pixels, sx, height - 5)
         else
            love.graphics.line(sx, bar_begin, sx, bar_end)
         end
      end
      if(mb) then b, mb = mb, nil end
   end,

   textinput = function(t)
      wrap(function() insert({t}, true) end)
   end,

   activate_minibuffer = function(prompt, callback, exit_callback)
      -- without this, the key which activated the minibuffer triggers a
      -- call to textinput, inserting it into the input
      local old_released = love.keyreleased
      love.keyreleased = function()
         love.keyreleased = old_released
         last_buffer, b = b, make_buffer(nil, nil, {prompt})
         b.mode = "minibuffer"
         b.minibuffer, b.prompt = true, prompt
         b.callback, b.exit_callback = callback, exit_callback
         b.point = #prompt
      end
   end,

   exit_minibuffer = function(cancel)
      local minibuffer = b
      b, mb = last_buffer, nil
      if(not cancel) then
         minibuffer.callback(utf8.sub(minibuffer.lines[1], #minibuffer.prompt+1))
      end
   end,

   next_buffer = function(n)
      local current = lume.find(buffers, b) - 1
      if(current + (n or 1) < 0) then current = current + #buffers end
      b = buffers[math.mod(current + (n or 1), #buffers) + 1]
   end,

   change_buffer = function(path)
      b = get_buffer(path)
   end,

   insert = insert,
   region = region,
   delete = delete,

   wrap = wrap,
   end_hook = save,
   name = "edit",

   current_mode_name = function() return b and b.mode end,

   -- normally you would use ship.api.activate_mode; this is lower-level
   set_mode = function(mode_name) if(b) then b.mode = mode_name end end,

   current_buffer_path = function() return b.path end,

   print = function(...)
      local texts, read_only = {...}, inhibit_read_only
      inhibit_read_only = true
      if(texts[1] == invisible or texts[1] == nil) then return end
      texts[1] = "\n" .. texts[1]
      io_write(unpack(lume.map(texts, tostring)))
      inhibit_read_only = read_only
   end,

   raw_write = write,
   write = io_write,

   get_line = function(n)
      if(not b) then return end
      if(n < 1) then n = #b.lines - n end
      return b.lines[n]
   end,

   get_line_number = function() return b.point_line end,

   get_max_lines = function() return b and #b.lines end,

   get_lines = function() return lume.clone(b.lines) end,

   point = function() return b.point, b.point_line end,

   is_dirty = function() return b and b.dirty end,

   invisible = invisible,

   suppress_read_only = function(f, ...)
      local read_only = inhibit_read_only
      inhibit_read_only = true
      local val = f(...)
      inhibit_read_only = read_only
      return val
   end,

   set_read_only = function(s) b.read_only = s end,

   save_excursion = save_excursion,

   prompt = function() return (b and b.prompt) or "> " end,
   get_prompt = function() return (b and b.prompt) or "> " end,
   set_prompt = function(p)
      if(not b) then return end
      local line = b.lines[#b.lines]
      b.lines[#b.lines] = p .. utf8.sub(line, utf8.len(b.prompt) + 1)
      if(b.point_line == #b.lines) then b.point = utf8.len(p) end
      b.prompt = p
   end,
   print_prompt = function()
      local read_only = inhibit_read_only
      inhibit_read_only = true
      with_current_buffer(console, function()
                             write(b.prompt)
                             b.point, b.point_line = #b.lines[#b.lines], #b.lines
      end)
      inhibit_read_only = read_only
   end,

   history_prev = function()
      if b.input_history_pos + 1 <= b.input_history.entries then
         b.input_history_pos = b.input_history_pos + 1
         replace_input(b.input_history:get(-b.input_history_pos))
      end
   end,
   history_next = function()
      if b.input_history_pos - 1 > 0 then
         b.input_history_pos = b.input_history_pos - 1
         replace_input(b.input_history:get(-b.input_history_pos))
      else
         b.input_history_pos = 0
         replace_input("")
      end
   end,
   history_push = function(input)
      b.input_history_pos = 0
      b.input_history:append(input, true)
   end,

   set_modeline = function(modeline_function)
      b.modeline = modeline_function
   end,

   with_current_buffer = with_current_buffer,

   dump_buffer = function(name)
      local to_dump = lume.pick(get_buffer(name), "prompt",
                                "path", "mode", "lines", "point", "mark",
                                "point_line", "mark_line", "input_history")
      return lume.serialize(to_dump)
   end,

   load_buffer = function(fs, dumped)
      local loaded = lume.deserialize(dumped)
      local buffer = get_buffer(loaded.path) or make_buffer(fs, loaded.path)
      lume.extend(buffer, loaded)
      buffer.input_history = utils.buffer:new(loaded.input_history)
   end,

   buffer_names = function()
      return lume.map(buffers, function(bu) return bu.path end)
   end,

   debug = debug,
}
