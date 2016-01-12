-- love-repl - an interactive lua repl for love games
-- Originally Copyright (c) 2013-2014 ioddly
-- Released under the Boost License: <http://www.boost.org/LICENSE_1_0.txt>

-- Adapted for this specific game by Phil Hagelberg

-- TODO: throw this all away and implement it as a mode inside the editor

local lume = require("lume")
local love = love
local utils = require("utils")

-- Module
local console = {
   padding_left = 10,
   max_lines = 1000,
   max_history = 1000,
   background = false,
   wrapping = false,
   -- sentinel value for suppressing printing
   invisible = {},
   rows = nil,
   cols = nil,
}
-- How many pixels of padding are on either side
local PADDING = 20
-- How many pixels are required to display a row
local ROW_HEIGHT
-- Console contents
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

function console.initialize()
   -- List of {boolean, string} where boolean is true if the string is part of user
   -- -navigable history (a > will be prepended before rendering if true)
   console.lines = utils.buffer:new()
   console.lines.max = console.max_lines
   -- List of command history entries
   console.history = utils.buffer:new()
   console.history.max = console.max_history

   font = love.graphics.getFont()
   console.font_width = font:getWidth('a')
   ROW_HEIGHT = font:getHeight()
end

function console.on(or_not)
   on = or_not ~= false
end

function console.is_on()
   return on
end

function console.on_close() end

function console.write(value)
   for line,_ in tostring(value):gmatch("([^\n]*\n?)") do
      if(line and line ~= "" and line ~= "\n") then console.display_line = line end
      console.lines:append(line)
   end
end

function console.print(...)
   local texts = lume.map({...}, tostring)
   console.write(table.concat(texts, "\t") .. "\n")
end

function console.clear_lines()
   console.lines = utils.buffer:new()
   console.lines.max = console.max_lines
   return console.invisible
end

local function pack(...) return {...} end

function console.eval(text, add_to_history)
   -- show input
   console.print("> " .. text)

   if(text == "help" or text == "help()") then
      console.print("Press ` to open the console, and run man() for more help.")
      return true
   end

   -- Evaluate string
   local func, err = loadstring("return " .. text)
   -- Compilation error
   if not func then
      if err then -- maybe it's a statement?
         func, err = loadstring(text)
         if not func then
            if err then
               console.print('! Compilation error: ' .. err)
            else
               console.print('! Unknown compilation error')
            end
            return false
         end
      end
   end

   if console.sandbox then
      setfenv(func, console.sandbox)
   end

   -- Try evaluating
   if func then
      local traceback, err_msg = nil, nil
      local result = pack(xpcall(func, function(e)
                                    traceback = debug.traceback()
                                    err_msg = e end))
      if result[1] then
         local results, i = lume.serialize(result[2], true), 3
         if add_to_history then
            local to_append
            if text:sub(0,1) == '=' then
               to_append = 'return ' .. text:sub(2)
            else
               to_append = text
            end
            if console.history:get(-1) ~= to_append then
              console.history:append(to_append, true)
            end
         end
         if result[2] == console.invisible then return true end
         while i <= #result do
            results = results .. ', ' .. lume.serialize(result[i], true)
            i = i + 1
         end
         console.print(results)
         return true
      else
         print(err_msg)
         print(traceback)
         console.print('! Evaluation error: ' .. err_msg)
      end
   end
   return false
end

local function completions_for(input, context, prefixes)
   if(type(context) ~= "table") then return {} end
   local input_parts = lume.split(input, ".")
   if(#input_parts == 1) then
      local matches = {}
      for k in pairs(context) do
         if(k:find("^" .. input)) then
            local parts = lume.clone(prefixes)
            table.insert(parts, k)
            table.insert(matches, table.concat(parts, "."))
         end
      end
      return matches
   else
      local first_part = table.remove(input_parts, 1)
      table.insert(prefixes, first_part)
      return completions_for(table.concat(input_parts, "."), context[first_part],
                             prefixes)
   end
end

console.complete = function()
   local input = lume.last(lume.split(editline:sub(0, cursor))) or ""
   local completions = completions_for(input, console.completion_context or
                                          console.sandbox, {})
   if(#completions == 1) then
      editline = editline:sub(1, cursor - input:len()) .. completions[1] ..
         editline:sub(cursor + 1)
      cursor = cursor + completions[1]:len() - input:len()
   elseif(#completions > 0) then
      console.print(table.concat(completions, " "))
   end
end

-- Line editing functionality and key handling

local function reset_editline()
   editline = ''
   cursor = 0
end

local function get_history()
   if histpos > 0 then
      editline = console.history:get(-histpos)
      cursor = #editline
   end
end

function console.delete_backwards()
   editline = editline:sub(0, cursor - 1) .. editline:sub(cursor + 1, #editline)
   if cursor > 0 then
      cursor = cursor - 1
   end
end

function console.delete_forwards()
   editline = editline:sub(0, cursor) .. editline:sub(cursor + 2, #editline)
end

function console.kill_line()
   editline = editline:sub(0, cursor)
end

function console.move_beginning_of_line()
   cursor = 0
end

function console.move_end_of_line()
   cursor = #editline
end

function console.eval_line()
   histpos = 0
   offset = 1
   if editline == '' then return end
   if console.read then
      console.read(editline)
      reset_editline()
   elseif console.eval(editline, true) then
      reset_editline()
   end
end

function console.history_prev()
   if histpos + 1 <= console.history.entries then
      histpos = histpos + 1
      get_history()
   end
end

function console.history_next()
   if histpos - 1 > 0 then
      histpos = histpos - 1
      get_history()
   else
      histpos = 0
      reset_editline()
   end
end

function console.scroll_up()
   offset = math.min(console.lines.entries - console.rows + 1, offset + console.rows)
end

function console.scroll_down()
   offset = math.max(1, offset - console.rows)
end

function console.clear()
   reset_editline()
end

function console.forward_char()
   cursor = cursor + 1
end

function console.backward_char()
   cursor = cursor - 1
end

function console.forward_word()
   local match = editline:find(word_break, cursor + 2)
   cursor = match and match - 1 or string.len(editline)
end

function console.backward_word()
   local back_line = editline:sub(0, math.max(cursor - 1, 0)):reverse()
   if(back_line:find(word_break)) then
      cursor = string.len(back_line) - back_line:find(word_break) + 1
   else
      cursor = 0
   end
end

function console.textinput(t)
   console.display_line = nil
   editline = editline:sub(0, cursor) .. t .. editline:sub(cursor + 1)
   cursor = cursor + 1
end

-- Rendering

function console.draw()
   local width, height = love.graphics:getWidth(), love.graphics:getHeight()

   console.rows = math.floor((height - (ROW_HEIGHT * 2)) / ROW_HEIGHT)
   console.cols = math.floor((width - (console.font_width * 2)) / console.font_width)

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
      local prefix = console.prompt or "> "
      local ln = prefix .. editline
      love.graphics.print(ln, console.padding_left, limit)

      -- draw cursor
      local cx, cy = console.padding_left + 1 +
         (console.font_width * string.len(prefix .. editline:sub(0, cursor))),
      limit + font:getHeight() + 2
      love.graphics.line(cx, cy, cx + 5, cy)
   end

   -- show edit line, unless the disabled console has a display_line to show
   if(not on) then
      if(console.display_line) then
         love.graphics.print(console.display_line, console.padding_left, limit)
      else
         print_edit_line()
      end
      return
   end

   print_edit_line()

   -- draw history
   local function render_line(ln2, row)
      if(ln2 == "\f\n" or ln2 == "\f") then
         local y = limit - (ROW_HEIGHT*(row+1)) + 0.5 * ROW_HEIGHT
         love.graphics.line(PADDING, y, width - PADDING, y)
      else
         love.graphics.print(ln2, console.padding_left, limit - (ROW_HEIGHT*(row+1)))
      end
   end

   for i = offset, console.rows + offset do
      local line = console.lines:get(-i)
      if(line) then render_line(line, i - offset) end
   end

   -- draw scroll bar

   -- this only gives you an estimate since it uses the amount of
   -- lines entered rather than the lines drawn, but close enough

   -- height is percentage of the possible lines
   local bar_height = math.min(100, (console.rows * 100) / console.lines.entries)
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
      local bar_end = (offset * 100) / console.lines.entries
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

return console
