local lume = require("lume")

local lines = {""}
local cursor = 0
local current = 1

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
-- where did this text come from?
local fs, path
-- pattern for word breaks
local word_break = "[%s%p]+"

return {
   initialize = function()
      ROW_HEIGHT = love.graphics.getFont():getHeight()
      em = love.graphics.getFont():getWidth('a')
   end,

   open = function(current_fs, current_path)
      -- reset position if opening a different file
      if(current_path ~= path) then
         cursor, current = 0, 1
      end

      fs, path = current_fs, current_path
      if(fs[path]) then
         lines = lume.split(fs[path], "\n")
      else
         lines = {""}
      end
      on = true
   end,

   save = function(current_fs, current_path)
      (current_fs or fs)[current_path or path] = table.concat(lines, "\n")
   end,

   on = function(or_not) on = or_not ~= false end,

   -- edit commands
   delete_backwards = function()
      if(cursor == 0) then
         current = current - 1
         cursor = #lines[current]
         lines[current] = lines[current] .. lines[current+1]
         table.remove(lines, current+1)
      else
         local l = lines[current]
         lines[current] = l:sub(0, cursor - 1) .. l:sub(cursor + 1, #l)
         if cursor > 0 then
            cursor = cursor - 1
         end
      end
   end,

   delete_forwards = function()
      if(cursor == #lines[current]) then
         local next_line = table.remove(lines, current+1)
         lines[current] = lines[current] .. next_line
      else
         local l = lines[current]
         lines[current] = l:sub(0, cursor) .. l:sub(cursor + 2, #l)
      end
   end,

   -- TODO: kill ring
   kill_line = function()
      if(cursor == #lines[current]) then
         local next_line = table.remove(lines, current+1)
         lines[current] = lines[current] .. next_line
      else
         lines[current] = lines[current]:sub(0, cursor)
      end
   end,

   move_beginning_of_line = function()
      cursor = 0
   end,

   move_end_of_line = function()
      cursor = #lines[current]
   end,

   prev_line = function()
      if(current > 1) then current = current - 1 end
   end,

   next_line = function()
      if(current < #lines) then current = current + 1 end
   end,

   -- TODO: scroll doesn't work
   scroll_up = function()
      current = math.max(0, current - scroll_size)
   end,

   scroll_down = function()
      current = math.min(#lines, current + scroll_size)
   end,

   forward_char = function()
      if(cursor == #lines[current]) then
         cursor, current = 0, current+1
      else
         cursor = cursor + 1
      end
   end,

   backward_char = function()
      if(cursor == 0) then
         cursor = #lines[current-1]
         current = current-1
      else
         cursor = cursor - 1
      end
   end,

   forward_word = function()
      local remainder = lines[current]:sub(cursor + 1, -1)
      if(not remainder:find("%S")) then
         cursor, current = 0, current+1
      end
      local _, match = lines[current]:find(word_break, cursor + 2)
      cursor = match and match - 1 or #lines[current]
   end,

   backward_word = function()
      local before = lines[current]:sub(0, cursor)
      if(not before:find("%S")) then
         current = current - 1
         cursor = #lines[current]
      end
      local back_line = lines[current]:sub(0, math.max(cursor - 1, 0)):reverse()
      if(back_line and back_line:find(word_break)) then
         local _, match = back_line:find(word_break)
         cursor = string.len(back_line) - match + 1
      else
         cursor = 0
      end
   end,

   newline = function()
      local remainder = lines[current]:sub(cursor + 1, -1)
      lines[current] = lines[current]:sub(0, cursor)
      cursor = 0
      current = current + 1
      table.insert(lines, current, remainder)
   end,

   -- internal functions
   draw = function()
      if not on then return end
      local width, height = love.graphics:getWidth(), love.graphics:getHeight()
      -- TODO: unify with repl
      DISPLAY_WIDTH = width - PADDING
      DISPLAY_ROWS = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)

      -- enforce consistency
      if(current < 1) then current = 1 end
      if(current > #lines) then current = #lines end
      if(cursor < 0) then cursor = 0 end
      if(cursor > string.len(lines[current])) then
         cursor = string.len(lines[current]) end

      -- Draw background
      love.graphics.setColor(0, 0, 0, 200)
      love.graphics.rectangle("fill", 0, 0, width, height)

      -- maximum characters in a rendered line of text
      local render_line = function(ln2, y)
         love.graphics.print(ln2, PADDING, y)
      end

      local edge = math.ceil(DISPLAY_ROWS * 0.3)
      local offset = (current < edge and 0) or (current - edge)
      for i,line in ipairs(lines) do
         if(i >= offset) then
            local y = ROW_HEIGHT * (i - offset)
            if(i == current) then
               -- current line
               love.graphics.setColor(0, 50, 0, 150)
               love.graphics.rectangle("fill", 0, y, width, ROW_HEIGHT)
               -- cursor
               love.graphics.setColor(0, 125, 0)
               love.graphics.rectangle("fill", PADDING+cursor*em, y,
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
         local bar_end = (current * 100) / #lines
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
      local line = lines[current]
      lines[current] = line:sub(0, cursor) .. t .. line:sub(cursor + 1)
      cursor = cursor + 1
   end,
}
