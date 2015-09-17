-- love-repl - an interactive lua repl for love games
-- Originally Copyright (c) 2013-2014 ioddly
-- Released under the Boost License: <http://www.boost.org/LICENSE_1_0.txt>

-- Adapted for this specific game by Phil Hagelberg

local utils = require("utils")
local love = love

-- Module
local repl = {
   padding_left = 10,
   max_lines = 1000,
   max_history = 1000,
   background = false,
   wrapping = false,
}
-- How many pixels of padding are on either side
local PADDING = 20
-- How many pixels are required to display a row
local ROW_HEIGHT
-- Maximum amount of rows that can be displayed on the screen
local DISPLAY_ROWS
-- Width of the display available for text, in pixels
local DISPLAY_WIDTH
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
-- currently enabled?
local on = false
-- pattern used to determine word boundaries
local word_break = "[ _-]"
-- current font
local font

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
   font = love.graphics.getFont()
   ROW_HEIGHT = font:getHeight()
end

function repl.on(or_not)
   on = or_not ~= false
end

function repl.is_on()
   return on
end

function repl.on_close() end

function repl.append(prefix, value)
   value = tostring(value)
   for _, line in pairs(utils.split(value, "\n")) do
      if(line ~= "") then repl.last_result = line end
      lines:append(prefix and ('> ' .. line) or line)
   end
end

function repl.print(...)
   local texts = {...}
   for _,text in ipairs(texts) do repl.append(false, text) end
end

local function pack(...) return {...} end

function repl.eval(text, add_to_history)
   -- Evaluate string
   local func, err = loadstring("return " .. text)
   -- Compilation error
   if not func then
      if err then -- maybe it's a statement?
         func, err = loadstring(text)
         if not func then
            if err then
               repl.print('! Compilation error: ' .. err)
            else
               repl.print('! Unknown compilation error')
            end
            return false
         end
      end
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

function repl.delete_backwards()
   editline = editline:sub(0, cursor - 1) .. editline:sub(cursor + 1, #editline)
   if cursor > 0 then
      cursor = cursor - 1
   end
end

function repl.delete_forwards()
   editline = editline:sub(0, cursor) .. editline:sub(cursor + 2, #editline)
end

function repl.kill_line()
   editline = editline:sub(0, cursor)
end

function repl.move_beginning_of_line()
   cursor = 0
end

function repl.move_end_of_line()
   cursor = #editline
end

function repl.eval_line()
   histpos = 0
   offset = 1
   if editline == '' then return end
   if repl.read then
      repl.read(editline)
      reset_editline()
   elseif repl.eval(editline, true) then
      reset_editline()
   end
end

function repl.history_prev()
   if histpos + 1 <= history.entries then
      histpos = histpos + 1
      get_history()
   end
end

function repl.history_next()
   if histpos - 1 > 0 then
      histpos = histpos - 1
      get_history()
   else
      histpos = 0
      reset_editline()
   end
end

function repl.scroll_up()
   offset = math.min(lines.entries - DISPLAY_ROWS + 1, offset + DISPLAY_ROWS)
end

function repl.scroll_down()
   offset = math.max(1, offset - DISPLAY_ROWS)
end

function repl.clear()
   reset_editline()
end

function repl.forward_char()
   cursor = cursor + 1
end

function repl.backward_char()
   cursor = cursor - 1
end

function repl.forward_word()
   local match = editline:find(word_break, cursor + 2)
   cursor = match and match - 1 or string.len(editline)
end

function repl.backward_word()
   local back_line = editline:sub(0, math.max(cursor - 1, 0)):reverse()
   if(back_line:find(word_break)) then
      cursor = string.len(back_line) - back_line:find(word_break) + 1
   else
      cursor = 0
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
   DISPLAY_WIDTH = width - PADDING
   DISPLAY_ROWS = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)

   -- Draw background
   love.graphics.setColor(0, 0, 0, 150)
   if(on) then
      love.graphics.rectangle("fill", 0, 0, width, height)
   else
      love.graphics.rectangle("fill", 0, height - 40, width, height)
   end
   love.graphics.setColor(0, 200, 0)

   -- Leave some room for text entry
   local limit = height - (ROW_HEIGHT * 2)

   local print_edit_line = function()
      local prefix = repl.prompt or "> "
      local ln = prefix .. editline
      love.graphics.print(ln, repl.padding_left, limit)

      -- draw cursor
      local cx, cy = repl.padding_left + 1 +
         font:getWidth(prefix .. editline:sub(0, cursor)),
      limit + font:getHeight() + 2
      love.graphics.line(cx, cy, cx + 5, cy)
   end

   -- show edit line, unless the disabled repl has a last-value to show
   if(not on) then
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
end

return repl
