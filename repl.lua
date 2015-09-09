-- love-repl - an interactive lua repl for love games
-- Originally Copyright (c) 2013-2014 ioddly
-- Released under the Boost License: <http://www.boost.org/LICENSE_1_0.txt>

-- Adapted for this specific game by Phil Hagelberg

local utils = require("utils")
local love = love

-- Module
local repl = {
   toggle_key = 'escape',
   padding_left = 10,
   max_lines = 1000,
   max_history = 1000,
   font = nil,
   screenshot = true,
   background = false,
   wrapping = false
}
-- How many pixels of padding are on either side
local PADDING = 20
-- How many pixels are required to display a row
local ROW_HEIGHT
-- Maximum amount of rows that can be displayed on the screen
local DISPLAY_ROWS
-- Width of the display available for text, in pixels
local DISPLAY_WIDTH
-- True when open, false when closed
local toggled = false
-- Console contents
-- History is just a list of strings
local history
-- List of {boolean, string} where boolean is true if the string is part of user
-- -navigable history (a > will be prepended before rendering if true)
local lines
-- Line that is currently being edited
local editline = ""
-- Location in the editline
local cursor = 0
-- Current position in history
local histpos = 0
-- Line display offset (in case of scrolling up and down)
local offset = 1

-- Circular buffer functionality
local buffer = {}

function buffer:new(ob)
   local o = ob or {}
   o.entries = #o
   o.cursor = #o + 1
   o.max = 10
   setmetatable(o, self)
   self.__index = self
   return o
end

function buffer:append(entry)
   if self[self.cursor] then
      self[self.cursor] = entry
   else
      table.insert(self, entry)
   end
   self.cursor = self.cursor + 1
   if self.cursor == self.max + 1 then
      self.cursor = 1
   end
   if self.entries ~= self.max then
      self.entries = self.entries + 1
   end
end

function buffer:get(idx)
   -- Allow negative indexes
   if idx < 0 then
      idx = (self.entries + idx) + 1
   end

   if self.entries == self.max then
      local c = self.cursor + idx - 1
      if c > self.max then
         c = c - self.max
      end
      return self[c]
   else
      return self[idx]
   end
end

function repl.initialize()
   lines = buffer:new({})
   lines.max = repl.max_lines
   history = buffer:new()
   history.max = repl.max_history
   -- Expose these in case somebody wants to use them
   repl.lines = lines
   repl.history = history

   if not repl.font then
      repl.font = love.graphics.newFont(12)
   end
end

function repl.toggle()
   toggled = not toggled
   if toggled then
      if repl.screenshot then
         repl.background = love.graphics.newImage(love.graphics.newScreenshot())
      end
   else
      repl.on_close()
   end
end

function repl.toggled()
   return toggled
end

function repl.on_close() end

function repl.append(prefix, value)
   value = tostring(value)
   for _, line in pairs(utils.split(value, "\n")) do
      if(line ~= "") then repl.last_result = line end
      lines:append(prefix and ('> ' .. line) or line)
   end
end

function repl.print(text)
   repl.append(false, text)
end

local function pack(...) return {...} end

function repl.eval(text, add_to_history)
   -- Evaluate string
   local func, err = loadstring("return " .. text)
   -- Compilation error
   if not func then
      if err then
         repl.print('! Compilation error: ' .. err)
      else
         repl.print('! Unknown compilation error')
      end
      return false
   end

   if repl.sandbox then
      setfenv(func, repl.sandbox)
   end

   -- Try evaluating
   if func then
      local traceback = nil
      local err_msg = nil
      local result = pack(xpcall(func, function(e)
                                    traceback = debug.traceback()
                                    err_msg = e end))
      -- local result = pack(xpcall(func, debug.debug))
      if result[1] then
         repl.append(true, text)
         local results, i = tostring(result[2]), 3
         if add_to_history then
            if text:sub(0,1) == '=' then
               history:append('return ' .. text:sub(2))
            else
               history:append(text)
            end
         end
         while i <= #result do
            results = results .. ', ' .. tostring(result[i])
            i = i + 1
         end
         repl.print(results)
         return true
      else
         print(err_msg)
         print(traceback)
         repl.print('! Evaluation error: ' .. err_msg)
      end
   end
   return false
end

-- Line editing functionality and key handling

local function reset_editline()
   editline = ''
   cursor = 0
   prompt_prefix = ''
end

local function get_history()
   if histpos > 0 then
      editline = history:get(-histpos)
      cursor = #editline
   end
end

local function ctrlp() return love.keyboard.isDown('lctrl', 'rctrl') end

function repl.keypressed(k)
   -- Line editing
   if k == 'backspace' then
      editline = editline:sub(0, cursor - 1) .. editline:sub(cursor + 1, #editline)
      if cursor > 0 then
         cursor = cursor - 1
      end
   elseif k == 'delete' then
      editline = editline:sub(0, cursor) .. editline:sub(cursor + 2, #editline)
   elseif ctrlp() and k == 'a' then
      cursor = 0
   elseif ctrlp() and k == 'e' then
      cursor = #editline
   elseif k == 'return' then
      histpos = 0
      offset = 1
      if editline == '' then return end
      if repl.eval(editline, true) then
         reset_editline()
      end
   elseif k == 'up' then
      if histpos + 1 <= history.entries then
         histpos = histpos + 1
         get_history()
      end
      -- Navigation
   elseif k == 'home' then
      offset = math.max(1, lines.entries - DISPLAY_ROWS + 1)
   elseif k == 'end' then
      offset = 1
   elseif k == 'pageup' then
      offset = math.min(lines.entries - DISPLAY_ROWS + 1, offset + DISPLAY_ROWS)
   elseif k == 'pagedown' then
      offset = math.max(1, offset - DISPLAY_ROWS)
   elseif k == "l" and ctrlp() then
      reset_editline()
   elseif k == 'down' then
      if histpos - 1 > 0 then
         histpos = histpos - 1
         get_history()
      else
         histpos = 0
         reset_editline()
      end
   elseif k == 'left' and cursor > 0 then
      cursor = cursor - 1
   elseif k == 'right' and cursor ~= #editline then
      cursor = cursor + 1
   elseif k == repl.toggle_key then
      repl.toggle()
      assert(toggled == false)
   end
end

function repl.textinput(t)
   repl.last_result = nil
   editline = editline:sub(0, cursor) .. t .. editline:sub(cursor + 1)
   cursor = cursor + 1
end

-- Rendering

function repl.draw()
   local width, height = love.graphics:getWidth(), love.graphics:getHeight()
   local font = repl.font
   ROW_HEIGHT = font:getHeight()
   DISPLAY_WIDTH = width - PADDING
   DISPLAY_ROWS = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)

   local saved_font = love.graphics.getFont()
   love.graphics.setFont(font)

   -- Draw background
   love.graphics.setColor(0, 0, 0, 150)
   if(repl.toggled()) then
      love.graphics.rectangle("fill", 0, 0, width, height)
   else
      love.graphics.rectangle("fill", 0, height - 40, width, height)
   end
   love.graphics.setColor(0, 200, 0)

   -- Leave some room for text entry
   local limit = height - (ROW_HEIGHT * 2)

   local print_edit_line = function()
      local prefix = "> "
      local ln = prefix .. editline
      love.graphics.print(ln, repl.padding_left, limit)

      -- draw cursor
      local cx, cy = repl.padding_left + 1 +
         font:getWidth(prefix .. editline:sub(0, cursor)),
      limit + font:getHeight() + 2
      love.graphics.line(cx, cy, cx + 5, cy)
   end

   -- show edit line, unless the non-toggled repl has a last-value to show
   if(not toggled) then
      if(repl.last_result) then
         love.graphics.print(repl.last_result, repl.padding_left, limit)
      else
         print_edit_line()
      end
      return
   end

   print_edit_line()

   -- draw history
   -- maximum characters in a rendered line of text
   local render_line = function(ln2, row)
      love.graphics.print(ln2, repl.padding_left, limit -
                             (ROW_HEIGHT * (row + 1)))
   end

   local render_lines = function(ln2, row, rows)
      love.graphics.printf(ln2, repl.padding_left, limit -
                              (ROW_HEIGHT * (row + rows)), DISPLAY_WIDTH)
   end

   if repl.wrapping then
      -- max chars in a line
      local line_max = (width - (repl.padding_left * 2)) / font:getWidth('a')
      local pos, lines_drawn = offset, 0
      while lines_drawn < DISPLAY_ROWS do
         local line = lines:get(-pos)
         if line == nil then break end
         local lines_to_draw = math.ceil(#line / line_max)
         render_lines(line, lines_drawn, lines_to_draw)
         lines_drawn = lines_drawn + lines_to_draw
         pos = pos + 1
      end
   else
      for i = offset, DISPLAY_ROWS + offset do
         local line = lines:get(-i)
         if line == nil then break end
         render_line(line, i - offset)
      end
   end

   -- draw scroll bar

   -- this only gives you an estimate since it uses the amount of
   -- lines entered rather than the lines drawn, but close enough

   -- height is percentage of the possible lines
   local bar_height = math.min(100, (DISPLAY_ROWS * 100) / lines.entries)
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
      local bar_end = (offset * 100) / lines.entries
      bar_end = ((height - 10) * bar_end) / 100
      bar_end = height - bar_end

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

   -- reset font
   love.graphics.setFont(saved_font)
end

return repl
