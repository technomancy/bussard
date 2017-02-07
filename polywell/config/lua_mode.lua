-- Mode for -*- lua -*- files
local editor = require("polywell")
local lume = require("polywell.lume")

local keywords = {"and", "break", "do", "else", "elseif", "end", "false",
                  "for", "function", "if", "in", "local", "nil", "not", "or",
                  "repeat", "return", "then", "true", "until", "while", }

keywords.comment_pattern = "[-][-]"

editor.define_mode("lua", "edit",
                   {on_change = lume.fn(editor.colorize, keywords),
                    activate = lume.fn(editor.colorize, keywords)})

-- change colors:           red green blue
editor.set_color("lua", {text={0, 180 ,0},
                         keyword={0, 255, 0},
                         str={200, 100, 0},
                         number={50, 175, 120},
                         comment={0, 100, 0}})

editor.add_auto_mode(".*lua", "lua")

editor.bind("lua", "ctrl-alt-r", editor.reload)
editor.bind("lua", "tab", editor.complete)
editor.bind("lua", "alt-/", editor.complete)