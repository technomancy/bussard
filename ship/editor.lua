local utf8 = require("utf8.init")
local lume = require("lume")
local utils = require("utils")

--- Essentially a port of Emacs to Lua/Love.

-- missing features (a very limited list)
-- * search/replace
-- * syntax highlighting

local kill_ring = {}

local modes = {}

-- This is the structure of a buffer table. However, the fields it contains
-- should be considered an internal implementation detail of the editor.
-- We expose functions to make changes to a buffer, but we don't let user code
-- make any changes directly; otherwise we will not be able to change the
-- structure of the table without breaking user code.
local make_buffer = function(fs, path, lines)
   -- fs is the filesystem; right now it is always the ship table.
   return { fs=fs, path=path, mode = "edit",
            lines = lines or lume.split((fs and fs:find(path) or ""), "\n"),
            point = 0, point_line = 1, mark = nil, mark_line = nil,
            last_yank = nil, mark_ring = {}, history = {}, undo_at = 0,
            -- dirty is a per-cycle change flag (for undo tracking) while
            -- needs_save is an overall change flag.
            dirty = false, needs_save = false,
            -- console-style modes need input history tracking and a prompt
            input_history = utils.buffer:new(), input_history_pos = 0,
            prompt = nil,
            -- arbitrary key/value storage
            props = {},
            modeline = function(b)
               return utf8.format(" %s  %s  (%s/%s)  %s",
                                  b.needs_save and "*" or "-",
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
local last_buffer_before_minibuffer -- for returning to after leaving minibuffer
-- for the default value in interactive buffer switching
local last_edit_buffer = console
local buffers = {console}
local b = nil -- default back to flight mode

local inhibit_read_only = false

local last_line = "Press ctrl-enter to open the console, " ..
"and run man() for help. Zoom with = and - or scroll wheel."

local invisible = {} -- sentinel "do not print" value

local get_current_mode = function()
   return modes[b and b.mode or "flight"]
end

local get_prop = function(prop, default)
   return b.props[prop] or get_current_mode().props[prop] or default
end

local state = function()
   return {lines = lume.clone(b.lines),
           point = b.point, point_line = b.point_line}
end

local undo = function()
   local prev = b.history[#b.history-b.undo_at]
   if(b.undo_at < #b.history) then b.undo_at = b.undo_at + 1 end
   if(prev) then
      b.lines, b.point, b.point_line = prev.lines, prev.point, prev.point_line
   end
end

-- all edits (commands and insertions) run inside this function; it handles
-- tracking undo status as well as enforcing certain rules.
local wrap = function(fn, ...)
   if(not b) then return fn(...) end -- no undo tracking for flight mode
   b.dirty = false
   local last_state = b and state()
   if(fn ~= undo) then b.undo_at = 0 end
   fn(...)
   if(not b) then return end -- did we switch to flight mode?
   if(b.dirty) then
      if(b.props.on_change) then b.props.on_change() end
      table.insert(b.history, last_state)
   end
   if(#b.history > history_max) then
      table.remove(b.history, 1)
   end
   -- These should never happen, but let's be forgiving instead of asserting.
   if(b.mark_line and (b.mark_line > #b.lines or b.mark_line < 1)) then
      print("Mark out of bounds!", b.mark_line, #b.lines)
      b.mark, b.mark_line = nil, nil
   end
   if(b.point_line > #b.lines or b.point_line < 1) then
      print("Point out of bounds!", b.point_line, #b.lines)
      b.point_line = 1
   end
   -- Cycle out old content for console-like buffers.
   if(b.max_lines) then
      for _=1,(#b.lines - b.max_lines) do
         table.remove(b.lines, 1)
         if(b.point_line >= 1) then b.point_line = b.point_line - 1 end
         if(b.mark_line) then b.mark_line = b.mark_line - 1 end
      end
   end
end

local debug = function()
   print("---------------", b.path, b.point_line, b.point, b.mark_line, b.mark)
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
         start_line, start, finish_line,finish =
            b.point_line, b.point, b.mark_line, b.mark
      else
         start_line, start, finish_line,finish =
            b.mark_line, b.mark, b.point_line, b.point
      end
      local r = {utf8.sub(b.lines[start_line], start + 1, -1)}
      for i = start_line+1, finish_line-1 do
         table.insert(r, b.lines[i])
      end
      table.insert(r, utf8.sub(b.lines[finish_line], 0, finish))
      return r, start_line, start, finish_line, finish
   end
end

local in_prompt = function(line, point, line2, _point2)
   if(not b.prompt) then return false end
   if((line2 or line) == line and line ~= #b.lines) then return false end
   if(line == #b.lines and point >= utf8.len(b.prompt)) then return false end
   return true
end

local edit_disallowed = function(line, point, line2, _point2)
   if(inhibit_read_only) then return false end
   return get_prop("read_only", in_prompt(line, point, line2, _point2))
end

local insert = function(text, point_to_end)
   if(in_prompt(b.point_line, b.point)) then b.point = #b.prompt end
   if(edit_disallowed(b.point_line, b.point)) then return end
   b.dirty, b.needs_save = true, true
   text = lume.map(text, function(s) return utf8.gsub(s, "\t", "  ") end)
   if(not text or #text == 0) then return end
   local this_line = b.lines[b.point_line]
   -- TODO: this_line can be nil when yanking
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

local textinput = function(t)
   wrap(function() insert({t}, true) end)
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

local system_yank = function ()
   -- don't crash in headless mode
   local text = love.window and love.system.getClipboardText()
   if(text) then
      insert(lume.split(text, "\n"), true)
   end
end

local is_beginning_of_buffer = function()
   return b.point == 0 and b.point_line == 1
end

local is_end_of_buffer = function()
   return b.point == #b.lines[b.point_line] and b.point_line == #b.lines
end

local forward_char = function(n) -- lameness: n must be 1 or -1
   n = n or 1
   if((is_end_of_buffer() and n > 0) or
      is_beginning_of_buffer() and n < 0) then return
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
   return utf8.sub(b.lines[b.point_line], b.point + 1, b.point + 1) or ""
end

local forward_word = function()
   if(utf8.find(point_over(), word_break)) then
      -- TODO: fuzzer found an infinite loop here: 1469196339
      while(not is_end_of_buffer() and utf8.find(point_over(), word_break)) do
         forward_char()
      end
   end
   while(not is_end_of_buffer() and not utf8.find(point_over(), word_break)) do
      forward_char()
   end
end

local backward_word = function()
   if(utf8.find(point_over(), word_break)) then
      while(not is_beginning_of_buffer() and
            utf8.find(point_over(), word_break)) do
         forward_char(-1)
      end
   end
   while(not is_beginning_of_buffer() and
         not utf8.find(point_over(), word_break)) do
      forward_char(-1)
   end
end

local save = function(this_fs, this_path)
   local target = this_fs or b.fs
   if(not target or not b.needs_save) then return end
   b.needs_save = false
   if(b.path:find("^/")) then
      if(not love.filesystem.write("game" .. b.path,
                                   table.concat(b.lines, "\n"))) then
         print("Could not save " .. this_path or b.path)
      end
   else
      local parts = lume.split(this_path or b.path, ".")
      local filename = table.remove(parts, #parts)
      for _,part in ipairs(parts) do
         target = target[part]
      end
      target[filename] = table.concat(b.lines, "\n")
   end
end

local newline = function(n)
   local t = {""}
   for _=1,(n or 1) do table.insert(t, "") end
   insert(t, true)
end

local save_excursion = function(f)
   local old_b, p, pl = b, b and b.point, b and b.point_line
   local m, ml = b and b.mark, b and b.mark_line
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
   -- TODO: breaks if #b.lines == 1
   b.point, b.point_line = #b.lines[#b.lines - 1], #b.lines - 1
   last_line, line_count = write(...)
   if(old_point_line == old_lines) then
      b.point, b.point_line = old_point, #b.lines
   end
   b = prev_b
   if(b == console) then b.point_line = old_point_line + line_count - 1 end
end

local the_print = function(...)
   local texts, read_only = {...}, inhibit_read_only
   inhibit_read_only = true
   if(texts[1] == invisible or texts[1] == nil) then return end
   texts[1] = "\n" .. texts[1]
   io_write(unpack(lume.map(texts, tostring)))
   inhibit_read_only = read_only
end

local with_traceback = lume.fn(utils.with_traceback, the_print)

local function find_binding(key, the_mode)
   local mode = the_mode or get_current_mode()
   local ctrl = love.keyboard.isDown("lctrl", "rctrl", "capslock")
   local alt = love.keyboard.isDown("lalt", "ralt")
   local map = (ctrl and alt and mode["ctrl-alt"]) or
      (ctrl and mode.ctrl) or (alt and mode.alt) or mode.map

   return map[key] or map["__any"] or
      (mode.parent and find_binding(key, mode.parent))
end

local define_mode = function(name, parent_name, props)
   -- backwards-compatibility with beta-1
   if(name == "edit" and parent_name) then parent_name, props = nil, nil end
   modes[name] = { map = {}, ctrl = {}, alt = {}, ["ctrl-alt"] = {},
                   parent = modes[parent_name], name = name, props = props or {} }
   return modes[name]
end

local function bind(mode_name, keycode, fn)
   assert(keycode ~= nil, "Tried to bind to nil. Use false to unbind")
   if(type(mode_name) == "table") then
      for _,m in ipairs(mode_name) do
         bind(m, keycode, fn)
      end
   else
      -- lua regexes don't support |
      local map, key = keycode:match("(ctrl-alt)-(.+)")
      if not map then map, key = keycode:match("(ctrl)-(.+)") end
      if not map then map, key = keycode:match("(alt)-(.+)") end
      if map == "alt-ctrl" then map = "ctrl-alt" end
      assert(modes[mode_name], "No mode " .. mode_name)
      if(key == "enter") then key = "return" end
      if(keycode == "enter") then keycode = "return" end
      modes[mode_name][map or "map"][key or keycode] = fn
   end
end

local handle_textinput = function(text)
   if(b and not find_binding(text) and utf8.len(text) == 1) then
      with_traceback(textinput, text)
   end
end

local get_input = function()
   assert(b.prompt, "Buffer does not have a prompt.")
   return utf8.sub(b.lines[#b.lines], #b.prompt+1)
end

local exit_minibuffer = function(cancel)
   local input, callback = get_input(), b.callback
   b, mb = last_buffer_before_minibuffer, nil
   callback(input, cancel)
end

local delete_backwards = function()
   if(is_beginning_of_buffer()) then return end
   local line, point = b.point_line, b.point
   local line2, point2
   save_excursion(function()
         forward_char(-1)
         line2, point2 = b.point_line, b.point
   end)
   delete(line2, point2, line, point)
end

define_mode("minibuffer")
bind("minibuffer", "return", exit_minibuffer)
bind("minibuffer", "escape", lume.fn(exit_minibuffer, true))
bind("minibuffer", "ctrl-g", lume.fn(exit_minibuffer, true))
bind("minibuffer", "backspace", delete_backwards)

-- TODO: organize these better
return {
   open = function(fs, path)
      last_edit_buffer = b
      b = get_buffer(path)
      if(not b) then
         if(not path:find("^/")) then
            b = make_buffer(fs, path)
            table.insert(buffers, b)
         else -- from the host filesystem
            -- TODO: ensure that it's a valid path
            local fs_path = "game" .. path
            local lines = {}
            if(love.filesystem.exists(fs_path)) then
               for line in love.filesystem.lines(fs_path) do
                  table.insert(lines, line)
               end
            else
               table.insert(lines, "")
            end
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
      local contents = b.fs and b.fs:find(b.path)
      if(not contents) then return end
      b.lines, b.point = lume.split(contents, "\n"), 0
      if(b.point_line > #b.lines) then b.point_line = #b.lines end
      if(b.mark and (b.mark_line > #b.lines)) then b.mark_line = #b.lines end
   end,

   save = save,

   -- edit commands
   delete_backwards = delete_backwards,

   delete_forwards = function()
      if(is_end_of_buffer()) then return end
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
      b.point_line = math.max(1, b.point_line - scroll_size)
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

   system_copy_region = function()
      if(b.mark == nil or b.mark_line == nil or not love.window) then return end
      love.system.setClipboardText(table.concat(region(), "\n"))
   end,

   yank = yank,

   yank_pop = function()
      if(b.last_yank) then
         table.insert(kill_ring, 1, table.remove(kill_ring))
         local ly_line1, ly_point1, ly_line2, ly_point2 = unpack(b.last_yank)
         delete(ly_line1, ly_point1, ly_line2, ly_point2)
         yank()
      end
   end,

   system_yank = system_yank,

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
   draw = function(ship)
      local mode = get_current_mode()
      if(mode and mode.draw) then return mode.draw(ship) end

      ROW_HEIGHT = ROW_HEIGHT or love.graphics.getFont():getHeight()
      em = em or love.graphics.getFont():getWidth('a')

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
         mb, b = b, last_buffer_before_minibuffer or buffers[1]
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

   textinput = textinput,

   activate_minibuffer = function(prompt, callback, on_change)
      -- without this, the key which activated the minibuffer triggers a
      -- call to textinput, inserting it into the input
      local old_released = love.keyreleased
      love.keyreleased = function()
         love.keyreleased = old_released
         last_buffer_before_minibuffer, b = b, make_buffer(nil, nil, {prompt})
         b.mode = "minibuffer"
         b.minibuffer, b.prompt = true, prompt
         b.callback, b.props.on_change = callback, on_change
         b.point = #prompt
      end
   end,

   exit_minibuffer = exit_minibuffer,

   next_buffer = function(n)
      local current = lume.find(buffers, b) - 1
      if(current + (n or 1) < 0) then current = current + #buffers end
      b = buffers[math.mod(current + (n or 1), #buffers) + 1]
   end,

   change_buffer = function(path)
      -- Do not set last edit 'buffer' to flight mode
      if(b) then last_edit_buffer = b end
      b = get_buffer(path)
   end,

   last_buffer = function()
      return last_edit_buffer and last_edit_buffer.path
   end,

   insert = insert,
   region = region,
   delete = delete,

   end_hook = save,
   name = "edit",

   current_mode_name = function() return b and b.mode or "flight" end,

   -- normally you would use activate_mode; this is lower-level
   set_mode = function(mode_name) if(b) then b.mode = mode_name end end,

   current_buffer_path = function() return b.path end,

   print = the_print,

   raw_write = write,
   write = io_write,

   get_line = function(n)
      if(not b) then return end
      if(n < 1) then n = #b.lines - n end
      return b.lines[n]
   end,

   get_line_number = function() return b.point_line end,

   get_max_lines = function() return b and #b.lines end,

   point = function() return b.point, b.point_line end,

   invisible = invisible,

   suppress_read_only = function(f, ...)
      local read_only = inhibit_read_only
      inhibit_read_only = true
      local val = f(...)
      inhibit_read_only = read_only
      return val
   end,

   get_prop = get_prop,
   set_prop = function(prop, value) b.props[prop] = value end,

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
                             b.mark, b.mark_line = nil, nil
                             delete(#b.lines, 0, #b.lines, #b.lines[#b.lines])
                             write(b.prompt)
                             b.point, b.point_line = #b.lines[#b.lines], #b.lines
      end)
      inhibit_read_only = read_only
   end,
   get_input = get_input,

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
      if(buffer.path == "*console*") then
         buffer.prompt, buffer.mode = "> ", "console"
      end
   end,

   buffer_names = function()
      return lume.map(buffers, function(bu) return bu.path end)
   end,

   go_to_line = function(line)
      if(line > 0 and line <= #b.lines) then b.point_line = line end
   end,

   activate_mode = function(mode_name)
      if(not b) then return end
      assert(modes[mode_name], mode_name .. " mode does not exist.")
      local current_mode = get_current_mode()
      local new_mode = modes[mode_name]

      if(current_mode.deactivate) then current_mode.deactivate() end
      b.mode = mode_name
      if(new_mode.activate) then new_mode.activate() end
   end,

   define_mode = define_mode,
   bind = bind,
   handle_textinput = handle_textinput,

   handle_key = function(key)
      local fn = find_binding(key)
      if(fn) then
         with_traceback(wrap, fn)
      end
   end,

   handle_wheel = function(x, y)
      local wheel_dir = nil
      if(x < 0) then wheel_dir = "wheelleft"
      elseif(x > 0) then wheel_dir = "wheelright"
      elseif(y < 0) then wheel_dir = "wheeldown"
      elseif(y > 0) then wheel_dir = "wheelup"
      end
      local fn = find_binding(wheel_dir)
      if(fn) then
         with_traceback(wrap, fn)
      end
   end,

   debug = debug,

   -- deprecated
   initialize = function() end,
   wrap = wrap,
   modes = modes,
   mode = function()
      return modes[b and b.mode or "flight"]
   end,
   get_lines = function() return lume.clone(b.lines) end,
}
