-- This -*- lua -*- defines a console mode which is for entering Lua code
-- to run directly for experimentation.
local editor = require("polywell")
local lume = require("polywell.lume")
local has_serpent = pcall(require, "serpent")
local inspect = pcall(require, "inspect")

local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
local serpent_pps = has_serpent and function(x)
   return require("serpent").block(x, serpent_opts) end
local pack = function(...) return {...} end

require("polywell.config.edit")

editor.define_mode("console", "edit") -- inherit bindings from edit

local switch_to_last = function() editor.change_buffer(editor.last_buffer()) end
editor.bind("console", "ctrl-return", switch_to_last)
editor.bind("console", "escape", switch_to_last)

editor.bind("console", "ctrl-a", editor.beginning_of_input)
editor.bind("console", "home", editor.beginning_of_input)

editor.bind("console", "alt-p", editor.history_prev)
editor.bind("console", "alt-n", editor.history_next)
editor.bind("console", "ctrl-up", editor.history_prev)
editor.bind("console", "ctrl-down", editor.history_next)

local jump_to_error = function()
   local _, point_line = editor.point()
   local line = editor.get_line(point_line)
   -- Pattern matches "<spaces>[string "buffer-name"]:line-number"
   local buffer, line_num = line:match("%s*%[string \"([^\"]*)\"%]:(%d*)")
   if(buffer and editor.fs[buffer]) then
      editor.open(editor.fs, buffer)
      editor.go_to(tonumber(line_num))
      return true
   end
end

editor.bind("console", "return", function()
        -- if you're not on the last line, enter just bumps you down.
        if(editor.get_line_number() ~= editor.get_max_line()) then
           if(not jump_to_error()) then
              editor.end_of_buffer()
           end
        end

        local input = editor.get_input()
        editor.history_push(input)
        editor.end_of_line()
        editor.newline()
        editor.no_mark()

        -- try to compile the input.
        local ls = editor.get_prop("loadstring", loadstring)
        local chunk, err = ls("return " .. input)
        if(err and not chunk) then -- maybe it's a statement, not an expression
           chunk, err = ls(input)
           if(not chunk) then
              editor.print("! Compilation error: " .. err or "Unknown error")
              editor.print_prompt()
              editor.end_of_buffer()
              return false
           end
        end

        -- try runnig the compiled code in protected mode.
        local trace
        local result = pack(xpcall(chunk, function(e)
                                      trace = debug.traceback()
                                      err = e end))
        local pps = editor.get_prop("pps", serpent_pps or inspect or
                                       lume.serialize)

        if(result[1]) then
           local output, i = pps(result[2]), 3
           if result[2] == editor.invisible then
              editor.print_prompt()
              return true
           end
           -- pretty-print out the values it returned.
           while i <= #result do
              output = output .. ', ' .. pps(result[i])
              i = i + 1
           end
           editor.print(output)
        else
           -- display the error and stack trace.
           editor.print('! Evaluation error: ' .. err or "Unknown")
           local lines = lume.split(trace, "\n")
           for i,l in pairs(lines) do
              -- editor infrastructure wraps 8 levels of irrelevant gunk
              if(i < #lines - 8) then editor.print(l) end
           end
        end
        editor.print_prompt()
end)

-- auto-completion saves you typing!
editor.bind("console", "tab", editor.complete)
editor.bind("console", "ctrl-i", editor.complete)
