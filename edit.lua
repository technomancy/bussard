local lume = require("lume")

--- Essentially a port of Emacs to Lua/Love.
-- missing features (a very limited list)
-- * undo/redo
-- * search/replace
-- * indentation
-- * syntax highlighting
-- * multiple file support

-- buffer data
local fs, path
local lines = {""}
local point, point_line = 0, 1
local mark, mark_line = nil, nil
local kill_ring = {}
local mark_ring = {}

-- undo data
local history = {}
local history_max = 128
local undo_at = 0
local dirty = false

-- how many lines do pageup/pagedown scroll?
local scroll_size = 20
-- How many pixels of padding are on either side
local PADDING = 20

-- How many pixels are required to display a row
local ROW_HEIGHT
-- Maximum amount of rows that can be displayed on the screen
local DISPLAY_ROWS
-- enabled?
local on = false
-- width of an m
local em
-- pattern for word breaks
local word_break = "[%s%p]+"
-- need to replace this when cycling yank
local last_yank = nil

local kill_ring_max = 32
local mark_ring_max = 32

local state = function()
   return {lines = lume.clone(lines), point = point, point_line = point_line}
end

local undo = function()
   local prev = history[#history-undo_at]
   if(undo_at < #history) then undo_at = undo_at + 1 end
   if(prev) then
      lines, point, point_line = prev.lines, prev.point, prev.point_line
   end
end

local wrap = function(fn, ...)
   dirty = false
   local last_state = state()
   if(fn ~= undo) then undo_at = 0 end
   fn(...)
   if(dirty) then
      table.insert(history, last_state)
   end
   if(#history > history_max) then
      table.remove(history, 1)
   end
end

local region = function()
   mark = math.min(string.len(lines[mark_line]), mark)

   if(point_line == mark_line) then
      local start, finish = math.min(point, mark), math.max(point, mark)
      return {lines[point_line]:sub(start+1, finish)}, point_line, start, point_line, finish
   elseif(mark == nil or mark_line == nil) then
      return {}, point_line, point, point_line, point
   else
      local start_line, start, finish_line, finish
      if(point_line < mark_line) then
         start_line, start, finish_line,finish = point_line,point,mark_line,mark
      else
         start_line, start, finish_line,finish = mark_line,mark,point_line,point
      end
      local r = {lines[start_line]:sub(start+1, -1)}
      for i = start_line+1, finish_line-1 do
         table.insert(r, lines[i])
      end
      table.insert(r, lines[finish_line]:sub(0, finish))
      return r, start_line, start, finish_line, finish
   end
end

local insert = function(text, point_to_end)
   dirty = true
   if(not text or #text == 0) then return end
   local this_line = lines[point_line]
   local before, after = this_line:sub(0, point), this_line:sub(point + 1)
   local first_line = text[1]

   if(#text == 1) then
      lines[point_line] = (before or "") .. (first_line or "") .. (after or "")
      if(point_to_end) then
         point = before:len() + first_line:len()
      end
   else
      lines[point_line] = (before or "") .. (first_line or "")
      for i,l in ipairs(text) do
         if(i > 1 and i < #text) then
            table.insert(lines, i+point_line-1, l)
         end
      end
      table.insert(lines, point_line+#text-1, text[#text] .. (after or ""))
      if(point_to_end) then
         point = #text[#text]
         point_line = point_line+#text-1
      end
   end
end

local delete = function(start_line, start, finish_line, finish)
   dirty = true
   if(start_line == finish_line) then
      local line = lines[point_line]
      lines[point_line] = line:sub(0, start) .. line:sub(finish + 1)
   else
      local after = lines[finish_line]:sub(finish+1, -1)
      for i = finish_line, start_line + 1, -1 do
         table.remove(lines, i)
      end
      lines[start_line] = lines[start_line]:sub(0, start) .. after
   end
   point, point_line, mark, mark_line = start, start_line, start, start_line
end

local push = function(ring, text, max)
   table.insert(ring, text)
   if(#ring > max) then table.remove(ring, 1) end
end

local yank = function()
   local text = kill_ring[#kill_ring]
   if(text) then
      last_yank = {point_line, point,
                   point_line + #text - 1, string.len(text[#text])}
      insert(text, true)
   end
end

local beginning_of_buffer = function()
   return point == 0 and point_line == 1
end

local end_of_buffer = function()
   return point == #lines[point_line] and point_line == #lines
end

local forward_word = function()
   if(end_of_buffer()) then return end
   local remainder = lines[point_line]:sub(point + 1, -1)
   if(not remainder:find("%S")) then
      point, point_line = 0, point_line+1
   end
   local _, match = lines[point_line]:find(word_break, point + 2)
   point = match and match - 1 or #lines[point_line]
end

local backward_word = function()
   if(beginning_of_buffer()) then return end
   local before = lines[point_line]:sub(0, point)
   if(not before:find("%S")) then
      point_line = point_line - 1
      point = #lines[point_line]
   end
   local back_line = lines[point_line]:sub(0, math.max(point - 1, 0)):reverse()
   if(back_line and back_line:find(word_break)) then
      local _, match = back_line:find(word_break)
      point = string.len(back_line) - match + 1
   else
      point = 0
   end
end

local newline = function()
   dirty = true
   local remainder = lines[point_line]:sub(point + 1, -1)
   lines[point_line] = lines[point_line]:sub(0, point)
   point = 0
   point_line = point_line + 1
   table.insert(lines, point_line, remainder)
end

return {
   initialize = function()
      ROW_HEIGHT = love.graphics.getFont():getHeight()
      em = love.graphics.getFont():getWidth('a')
      history, undo_at, dirty = {}, 0, false
   end,

   -- TODO: need a UI for opening new files
   open = function(this_fs, this_path)
      -- reset position if opening a different file
      if(this_path ~= path) then
         point, point_line = 0, 1
      end

      mark, mark_line, on = nil, nil, true
      fs, path = this_fs, this_path
      if(fs) then
         lines = lume.split(fs:find(path) or "", "\n")
      else
         lines = lume.split(love.filesystem.read(path) or "", "\n")
      end
   end,

   revert = function()
      lines = lume.split(fs:find(path), "\n")
   end,

   save = function(this_fs, this_path)
      local target = this_fs or fs
      if(target) then -- save to ship fs
         local parts = lume.split(this_path or path, ".")
         local filename = table.remove(parts, #parts)
         for _,part in ipairs(parts) do
            target = target[part]
         end
         target[filename] = table.concat(lines, "\n")
      else -- save to real disk
         local f = io.open(this_path or path, "w")
         f:write(table.concat(lines, "\n"))
         f:close()
      end
   end,

   on = function(or_not) on = or_not ~= false end,

   -- edit commands
   delete_backwards = function()
      dirty = true
      if(point == 0) then
         point_line = point_line - 1
         point = #lines[point_line]
         lines[point_line] = lines[point_line] .. lines[point_line+1]
         table.remove(lines, point_line+1)
      else
         local l = lines[point_line]
         lines[point_line] = l:sub(0, point - 1) .. l:sub(point + 1, #l)
         if point > 0 then
            point = point - 1
         end
      end
   end,

   delete_forwards = function()
      dirty = true
      if(point == #lines[point_line]) then
         local next_line = table.remove(lines, point_line+1)
         lines[point_line] = lines[point_line] .. next_line
      else
         local l = lines[point_line]
         lines[point_line] = l:sub(0, point) .. l:sub(point + 2, #l)
      end
   end,

   kill_line = function()
      dirty = true
      if(point == #lines[point_line]) then
         local next_line = table.remove(lines, point_line+1)
         lines[point_line] = lines[point_line] .. next_line
         push(kill_ring, {""}, kill_ring_max)
      else
         local killed = lines[point_line]:sub(point + 1, -1)
         lines[point_line] = lines[point_line]:sub(0, point)
         push(kill_ring, {killed}, kill_ring_max)
      end
   end,

   move_beginning_of_line = function()
      point = 0
   end,

   move_end_of_line = function()
      point = #lines[point_line]
   end,

   prev_line = function()
      if(point_line > 1) then point_line = point_line - 1 end
   end,

   next_line = function()
      if(point_line < #lines) then point_line = point_line + 1 end
   end,

   scroll_up = function()
      point_line = math.max(0, point_line - scroll_size)
   end,

   scroll_down = function()
      point_line = math.min(#lines, point_line + scroll_size)
   end,

   forward_char = function()
      if(end_of_buffer()) then return
      elseif(point == #lines[point_line]) then
         point, point_line = 0, point_line+1
      else
         point = point + 1
      end
   end,

   backward_char = function()
      if(beginning_of_buffer()) then return
      elseif(point == 0) then
         point = #lines[point_line-1]
         point_line = point_line-1
      else
         point = point - 1
      end
   end,

   forward_word = forward_word,

   backward_word = backward_word,

   backward_kill_word = function()
      local original_point_line, original_point = point_line, point
      backward_word()
      delete(point_line, point, original_point_line, original_point)
   end,

   forward_kill_word = function()
      local original_point_line, original_point = point_line, point
      forward_word()
      delete(original_point_line, original_point, point_line, point)
   end,

   beginning_of_buffer = function()
      point, point_line = 0, 1
      mark, mark_line = nil, nil
   end,

   end_of_buffer = function()
      point, point_line = #lines[#lines], #lines
      mark, mark_line = nil, nil
   end,

   newline = newline,

   newline_and_indent = function()
      local indentation = (lines[point_line]:match("^ +") or ""):len()
      newline()
      insert({string.rep(" ", indentation)})
      point = point + indentation
   end,

   mark = function()
      push(mark_ring, {point, point_line}, mark_ring_max)
      mark, mark_line = point, point_line
   end,

   jump_to_mark = function()
      point, point_line = mark or point, mark_line or point_line
      if(#mark_ring > 0) then
         table.insert(mark_ring, 1, table.remove(mark_ring))
         mark, mark_line = unpack(mark_ring[1])
      end
   end,

   no_mark = function()
      mark, mark_line = nil, nil
   end,

   kill_ring_save = function()
      if(mark == nil or mark_line == nil) then return end
      push(kill_ring, region(), kill_ring_max)
   end,

   kill_region = function()
      if(mark == nil or mark_line == nil) then return end
      local _, start_line, start, finish_line, finish = region()
      push(kill_ring, region(), kill_ring_max)
      delete(start_line, start, finish_line, finish)
   end,

   yank = yank,

   yank_pop = function()
      table.insert(kill_ring, 1, table.remove(kill_ring))
      local ly_line1, ly_point1, ly_line2, ly_point2 = unpack(last_yank)
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
      assert(fs and fs.load, "No loading context available.")
      fs:load(path)
   end,

   undo = undo,

   -- internal functions
   draw = function()
      if not on then return end
      local width, height = love.graphics:getWidth(), love.graphics:getHeight()
      DISPLAY_ROWS = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)

      -- enforce consistency
      if(point_line < 1) then point_line = 1 end
      if(point_line > #lines) then point_line = #lines end
      if(point < 0) then point = 0 end
      if(point > string.len(lines[point_line])) then
         point = string.len(lines[point_line]) end

      -- Draw background
      love.graphics.setColor(0, 0, 0, 170)
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

      local edge = math.ceil(DISPLAY_ROWS * 0.3)
      local offset = (point_line < edge and 0) or (point_line - edge)
      for i,line in ipairs(lines) do
         if(i >= offset) then
            local y = ROW_HEIGHT * (i - offset)
            -- mark
            if(i == mark_line) then
               love.graphics.setColor(0, 125, 0)
               love.graphics.rectangle("line", PADDING+mark*em, y,
                                       em, ROW_HEIGHT)
            end
            if(i == point_line) then
               -- point_line line
               love.graphics.setColor(0, 50, 0, 150)
               love.graphics.rectangle("fill", 0, y, width, ROW_HEIGHT)
               -- point
               love.graphics.setColor(0, 125, 0)
               love.graphics.rectangle("fill", PADDING+point*em, y,
                                       em, ROW_HEIGHT)
            end
            love.graphics.setColor(0, 200, 0)
            render_line(line, y)
         end
      end

      -- draw scroll bar

      -- this only gives you an estimate since it uses the amount of
      -- lines entered rather than the lines drawn, but close enough

      -- height is percentage of the possible lines
      local bar_height = math.min(100, (DISPLAY_ROWS * 100) / #lines)
      -- convert to pixels (percentage of screen height, minus 10px padding)
      local bar_height_pixels = (bar_height * (height - 10)) / 100

      local sx = width - 5
      -- Handle the case where there are less actual lines than display rows
      if bar_height_pixels >= height - 10 then
         love.graphics.line(sx, 5, sx, height - 5)
      else
         -- now determine location on the screen by taking the offset in
         -- history and converting it first to a percentage of total
         -- lines and then a pixel offset on the screen
         local bar_end = (point_line * 100) / #lines
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
   end,

   textinput = function(t)
      wrap(function()
            local line = lines[point_line]
            dirty = true
            lines[point_line] = line:sub(0, point) .. t .. line:sub(point + 1)
            point = point + 1
      end)
   end,

   wrap = wrap
}
