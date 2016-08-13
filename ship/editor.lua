local utf8 = require("utf8.init")
local lume = require("lume")
local utils = require("utils")
local colorize_lua = require("ship.editor.colorize_lua")
local draw = require("ship.editor.draw")

local dprint = os.getenv("DEBUG") and print or function() end

--- Essentially a port of Emacs to Lua/Love.

local scroll_size = 20
local kill_ring_max = 32
local mark_ring_max = 32
local history_max = 128
local kill_ring = {}

local modes = {}

-- This is the structure of a buffer table. However, the fields it contains
-- should be considered an internal implementation detail of the editor.
-- We expose functions to make changes to a buffer, but we don't let user code
-- make any changes directly; otherwise we will not be able to change the
-- structure of the table without breaking user code.
local make_buffer = function(fs, path, lines, props)
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
            props = props or {},
            -- TODO: this leaks buffer structure to userspace.
            modeline = function(b)
               return utf8.format(" %s  %s  (%s/%s)  %s",
                                  b.needs_save and "*" or "-",
                                  b.path, b.point_line, #b.lines, b.mode)
            end
   }
end

-- table of classifier -> {red, green, blue} color
local colors = {mark = {0, 125, 0},
                point = {0, 125, 0},
                point_line = {0, 50, 0, 190},
                minibuffer_bg = {0, 200, 0},
                minibuffer_fg = {0, 0, 0},
                scroll_bar = {0, 150, 0},
                lua = {text={0, 180 ,0},
                       keyword={0, 255, 0},
                       str={200, 100, 0},
                       number={50, 175, 120},
                       comment={0, 100, 0}},
                -- only used when colorizer isn't active
                text = {0, 200, 0},
                background = {0, 0, 0, 240},
               }

local console = make_buffer(nil, "*console*", {"", "> "})
console.prevent_close, console.point, console.point_line = true, 2, 2
console.mode, console.prompt, console.max_lines = "console", "> ", 512

-- b here is the current buffer; when it is nil it means the editor is disabled
-- behind_minibuffer is the buffer that  is shown while the minibuffer is active
local b, behind_minibuffer = nil, nil
-- echo messages show feedback while in the editor, until a new key is pressed
local echo_message, echo_message_new = nil, false

-- for the default value in interactive buffer switching
local last_edit_buffer = console
local buffers = {console}
local splits = nil

local inhibit_read_only, printing_prompt = false, false

-- the last line printed is shown at the bottom when in flight mode; defaults
-- to a helpful message
local last_line = "Press ctrl-enter to open the console, " ..
   "Zoom with = and - or the scroll wheel."

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

local debug = function()
   if(not os.getenv("DEBUG")) then return end
   print("---------------", b.path, b.point_line, b.point, b.mark_line, b.mark)
   for _,line in ipairs(b.lines) do
      print(line)
   end
   print("---------------")
end

local out_of_bounds = function(line, point)
   return not (b.lines[line] and point >= 0 and point <= #b.lines[line])
end

local bounds_check = function()
   -- The first one is a normal occurance; the second two should never happen,
   -- but let's be forgiving instead of asserting.
   if(#b.lines[b.point_line] and b.point > #b.lines[b.point_line]) then
      b.point = #b.lines[b.point_line]
   elseif(b.mark_line and b.mark and out_of_bounds(b.mark_line, b.mark)) then
      dprint("Mark out of bounds!", b.mark_line, #b.lines)
      debug()
      b.mark, b.mark_line = nil, nil
   end
   if(out_of_bounds(b.point_line, b.point)) then
      dprint("Point out of bounds!", b.point_line, b.point,
             #b.lines, b.lines[b.point_line] and #b.lines[b.point_line])
      debug()
      b.point, b.point_line = 0, 1
   end
end

-- all edits (commands and insertions) run inside this function; it handles
-- tracking undo status as well as enforcing certain rules.
local wrap = function(fn, ...)
   if(not b) then return fn(...) end -- no undo tracking when disabled
   b.dirty = false
   local last_state = b and state()
   if(fn ~= undo) then b.undo_at = 0 end
   fn(...)

   if(echo_message_new) then
      echo_message_new = false
   else
      echo_message = nil
   end

   if(not b) then return end -- did we disable?
   if(b.dirty) then
      table.insert(b.history, last_state)
      local on_change = get_prop("on_change")
      if(type(on_change) == "function") then
         on_change()
      elseif(type(on_change) == "table") then
         for _,f in pairs(on_change) do f() end
      end
   end
   if(#b.history > history_max) then
      table.remove(b.history, 1)
   end
   bounds_check()
   -- Cycle out old content for console-like buffers.
   if(b.max_lines) then
      for _=1,(#b.lines - b.max_lines) do
         table.remove(b.lines, 1)
         if(b.point_line >= 1) then b.point_line = b.point_line - 1 end
         if(b.mark_line) then b.mark_line = b.mark_line - 1 end
      end
   end
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

local in_prompt = function(line, point, line2)
   if(printing_prompt or not b.prompt) then return false end
   if((line2 or line) == line and line ~= #b.lines) then return false end
   if(line == #b.lines and point >= utf8.len(b.prompt)) then return false end
   return true
end

local edit_disallowed = function(line, point, line2, _point2)
   if(inhibit_read_only) then return false end
   return get_prop("read_only", in_prompt(line, point, line2, _point2))
end

local echo = function(...)
   echo_message, echo_message_new = table.concat({...}, " "), true
end

local insert = function(text, point_to_end)
   if(in_prompt(b.point_line, b.point)) then b.point = #b.prompt end
   if(edit_disallowed(b.point_line, b.point)) then
      return echo("Read-only.")
   end
   if(out_of_bounds(b.point_line, b.point)) then
      dprint("Inserting out of bounds!")
      return
   end

   b.dirty, b.needs_save = true, true
   text = lume.map(text, function(s) return utf8.gsub(s, "\t", "  ") end)
   if(not text or #text == 0) then return end
   local this_line = b.lines[b.point_line] or ""
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
   if(out_of_bounds(start_line, start) or
      out_of_bounds(finish_line, finish)) then
      dprint("Deleting out of bounds!")
      return
   end

   b.dirty, b.needs_save = true, true
   if(start_line == finish_line) then
      local line = b.lines[start_line]
      b.lines[start_line] = utf8.sub(line, 0, start)..utf8.sub(line, finish + 1)
      if(b.point_line == start_line and start <= b.point) then
         b.point = start
      elseif(b.point_line == start_line and b.point <= finish) then
         b.point = b.point - (finish - start)
      end
   else
      local after = utf8.sub(b.lines[finish_line], finish+1, -1)
      for i = finish_line, start_line + 1, -1 do
         table.remove(b.lines, i)
      end
      b.lines[start_line] = utf8.sub(b.lines[start_line], 0, start) .. after
      if(b.point_line > start_line and b.point_line <= finish_line) then
         b.point, b.point_line = start, start_line
      elseif(b.point_line > finish_line) then
         b.point_line = b.point_line - (finish_line - start_line)
      end
   end
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
   bounds_check()
   return b.point == 0 and b.point_line == 1
end

local is_end_of_buffer = function()
   bounds_check()
   return b.point == #b.lines[b.point_line] and b.point_line == #b.lines
end

local forward_char = function(n) -- lameness: n must be 1 or -1
   n = n or 1
   if((is_end_of_buffer() and n > 0) or
      is_beginning_of_buffer() and n < 0) then return
   elseif(b.point >= #b.lines[b.point_line] and n > 0) then
      b.point, b.point_line = 0, b.point_line+1
   elseif(b.point <= 0 and n < 0) then
      b.point = #b.lines[b.point_line-1]
      b.point_line = b.point_line-1
   else
      b.point = b.point + n
   end
end

local point_over = function()
   return utf8.sub(b.lines[b.point_line], b.point + 1, b.point + 1) or " "
end

local moved_last_point, moved_last_line
local point_moved = function()
   local lp, ll = moved_last_point, moved_last_line
   moved_last_point, moved_last_line = b.point, b.point_line
   return not (lp == b.point and ll == b.point_line)
end

local word_break = "[%s%p]"

local forward_word = function(n)
   moved_last_point, moved_last_line = nil, nil
   if(utf8.find(point_over(), word_break)) then
      while(point_moved() and utf8.find(point_over(), word_break)) do
         forward_char(n)
      end
   end
   forward_char(n)
   while(point_moved() and not utf8.find(point_over(), word_break)) do
      forward_char(n)
   end
end

local backward_word = lume.fn(forward_word, -1)

local save = function(this_fs)
   local target = this_fs or b and b.fs
   local this_b = (b and b.path == "minibuffer") and behind_minibuffer or b
   if(not target or not this_b.needs_save) then return end
   if(this_b.lines[#this_b.lines] ~= "") then table.insert(this_b.lines, "") end
   this_b.needs_save = false
   if(this_b.path:find("^/")) then
      if(not love.filesystem.write("game" .. this_b.path,
                                   table.concat(this_b.lines, "\n"))) then
         echo("Could not save " .. this_b.path)
      end
   else
      local parts = lume.split(this_path or this_b.path, ".")
      local filename = table.remove(parts, #parts)
      for _,part in ipairs(parts) do
         target = target[part]
      end
      target[filename] = table.concat(this_b.lines, "\n")
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
   if(#b.lines > 1) then
      b.point, b.point_line = #b.lines[#b.lines - 1], #b.lines - 1
   else
      b.point, b.point_line = 0, 1
   end
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
   if(parent_name and type(parent_name) ~= "string") then parent_name = nil end
   if(type(props) ~= "table") then props = {} end

   modes[name] = { map = {}, ctrl = {}, alt = {}, ["ctrl-alt"] = {},
                   parent = modes[parent_name], name = name, props = props }
end

local function bind(mode_name, keycode, fn)
   assert(keycode ~= nil, "Tried to bind to nil. Use false to unbind.")
   if(type(mode_name) == "table") then
      for _,m in ipairs(mode_name) do
         bind(m, keycode, fn)
      end
   else
      -- lua patterns don't support |
      local map, key = keycode:match("(ctrl[-]alt)-(.+)")
      if not map then map, key = keycode:match("(ctrl)-(.+)") end
      if not map then map, key = keycode:match("(alt)-(.+)") end
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

local get_input = function(tb)
   tb = tb or b
   assert(tb.prompt, "Buffer does not have a prompt.")
   return utf8.sub(tb.lines[#tb.lines], #tb.prompt+1)
end

local exit_minibuffer = function(cancel)
   local input, callback = get_input(), b.callback
   local completer = b.props and b.props.completer
   for k in pairs(b.props and b.props.bind or {}) do
      bind("minibuffer", k, nil) -- undo one-off bindings created in read_line
   end
   b = behind_minibuffer
   if(completer) then
      local completions = completer(input)
      callback(completions[1] or input, cancel)
   else
      callback(input, cancel)
   end
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

local complete = function()
   if(b.props.completer) then
      local completions = b.props.completer(get_input())
      if(#completions == 1) then
         b.lines[#b.lines] = b.prompt .. completions[1]
         b.point = #b.lines[#b.lines]
      else
         local common = utils.longest_common_prefix(completions)
         b.lines[#b.lines] = b.prompt .. (common or "")
         b.point = #b.lines[#b.lines]
      end
   end
end

define_mode("minibuffer")
bind("minibuffer", "return", exit_minibuffer)
bind("minibuffer", "escape", lume.fn(exit_minibuffer, true))
bind("minibuffer", "ctrl-g", lume.fn(exit_minibuffer, true))
bind("minibuffer", "backspace", delete_backwards)
bind("minibuffer", "tab", complete)

local read_line = function(prompt, callback, props)
   -- without this, the key which activated the minibuffer triggers a
   -- call to textinput, inserting it into the input
   local old_released = love.keyreleased
   love.keyreleased = function()
      love.keyreleased = old_released
      if(not b or b.path ~= "minibuffer") then
         behind_minibuffer, b = b, make_buffer(nil, "minibuffer",
                                               {prompt}, props)
      end
      b.mode = "minibuffer"
      b.prompt, b.callback, b.point = prompt, callback, #prompt
      for k,f in pairs(props and props.bind or {}) do
         bind("minibuffer", k, f)
      end
      if(props and props.completer) then
         b.render = function(mini)
            local completions = props.completer(get_input(mini))
            return mini.lines[1] .. " " .. table.concat(completions, " | ")
         end
      else
         b.render = function(mini) return mini.lines[1] end
      end
   end
end

local activate_mode = function(mode_name)
   if(not b) then return end
   assert(modes[mode_name], mode_name .. " mode does not exist.")
   local current_mode = get_current_mode()
   local new_mode = modes[mode_name]

   if(current_mode.props.deactivate) then current_mode.props.deactivate() end
   b.mode = mode_name
   if(new_mode.props.activate) then new_mode.props.activate() end
   -- for backwards-compatibility; activate should be under props from now on
   if(new_mode.activate) then new_mode.activate() end
end

-- TODO: organize these better
return {
   open = function(fs, path)
      last_edit_buffer = b
      b = get_buffer(path)
      if(not b) then
         if(fs and fs:find(path) and type(fs:find(path)) ~= "string") then
            -- Could support opening tables as directories like dired?
            the_print("Tried to open a directory or something.")
         else
            b = make_buffer(fs, path)
            table.insert(buffers, b)
         end

         local parts = lume.split(b.lines[1], "-*-")
         local auto_mode = parts[2] and lume.trim(parts[2])
         if(auto_mode) then
            activate_mode(auto_mode)
         elseif(path:find("/.*lua")) then
            activate_mode("lua")
         else
            activate_mode("edit")
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

   is_end_of_buffer = is_end_of_buffer,
   is_beginning_of_buffer = is_beginning_of_buffer,

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

   eval_buffer = function()
      if(not b.fs or not b.fs.dofile) then
         echo("No loading context available.")
      else
         save()
         b.fs.dofile(b.path)
         echo("Loaded " .. b.path)
      end
   end,

   undo = undo,

   draw = function(status)
      local mode = get_current_mode()
      -- TODO/blocker: backwards-compatibility with drawless flight mode
      if(mode and mode.props.draw) then
         love.graphics.setColor(colors.text)
         mode.props.draw(status)
      else
         local w,h = love.window.getMode()
         local full = {10,10,w,h}
         local to_show = b.path == "minibuffer" and
            (behind_minibuffer or console) or b
         local buffers_where = {[full]=to_show}

         if(splits) then
            buffers_where = {}
            for pos,buffer_name in pairs(splits) do
               if(buffer_name == "current") then
                  buffers_where[pos] = to_show
               elseif(buffer_name == "last") then
                  buffers_where[pos] = last_edit_buffer
               else
                  buffers_where[pos] = get_buffer(buffer_name)
               end
            end
         end
         draw(b, buffers_where, echo_message, colors)
      end
   end,

   set_splits = function(s) splits = s end,
   get_splits = function() return splits end,

   textinput = textinput,

   read_line = read_line,

   exit_minibuffer = exit_minibuffer,

   next_buffer = function(n)
      local current = lume.find(buffers, b) - 1
      if(current + (n or 1) < 0) then current = current + #buffers end
      b = buffers[math.mod(current + (n or 1), #buffers) + 1]
   end,

   change_buffer = function(path)
      -- Do not set last edit 'buffer' to flight mode
      if(b and b ~= last_edit_buffer) then last_edit_buffer = b end
      b = get_buffer(path)
   end,

   last_buffer = function()
      return last_edit_buffer and last_edit_buffer.path
   end,

   last_line = function()
      if(console.lines[#console.lines] == console.prompt) then
         return last_line
      else
         return console.lines[#console.lines]
      end
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

   get_lines = function() return lume.clone(b.lines) end,

   get_line = function(n)
      if(not b) then return end
      if(not n) then return b.lines[b.point_line] end
      if(n < 1) then n = #b.lines - n end
      return b.lines[n]
   end,

   get_line_number = function() return b and b.point_line end,

   get_max_line = function() return b and #b.lines end,

   point = function() return b.point, b.point_line end,

   set_line = function(line, number, path)
      local buffer = get_buffer(path) or b
      buffer.lines[number] = line
   end,

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
      printing_prompt, inhibit_read_only = true, true
      with_current_buffer(console, function()
                             b.mark, b.mark_line = nil, nil
                             delete(#b.lines, 0, #b.lines, #b.lines[#b.lines])
                             write(b.prompt)
                             b.point, b.point_line = #b.lines[#b.lines], #b.lines
      end)
      printing_prompt, inhibit_read_only = false, read_only
   end,
   get_input = get_input,

   -- this is for feedback within the editor where print wouldn't make sense
   echo = echo,

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

   go_to = function(line, point, buffer_name)
      local buffer = get_buffer(buffer_name) or b
      if(type(point) == "number" and point >= 0 and
         point <= #buffer.lines[buffer.point_line]) then
         buffer.point = math.floor(point)
      end
      if(type(line) == "number" and line >= 1 and line <= #buffer.lines) then
         buffer.point_line = math.floor(line)
      end
   end,

   activate_mode = activate_mode,

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

   set_colors = function(new_colors)
      lume.extend(colors, new_colors)
   end,

   set_color = function(color, value)
      if(type(color) == "string") then
         colors[color] = value
      else -- nested set
         local target = colors
         local last = table.remove(color, #color)
         for _,v in ipairs(color) do
            target = target[v]
         end
         target[last] = value
      end
   end,

   colorize_lua = function()
      -- 0.9.x doesn't have multi-colored print
      if(love._version_major > 0 or love._version_minor < 10) then return end
      b.props.render_lines = colorize_lua(b.lines, colors.lua)
   end,

   debug = debug,

   -- deprecated
   initialize = function() end,
   wrap = wrap,
   modes = modes,
   mode = function() return modes[b and b.mode or "flight"] end,
   activate_minibuffer = read_line,
   get_max_lines = function() return b and #b.lines end,
   set_read_only = function(x) b.props["read_only"] = x end,
}
