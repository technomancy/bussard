-- Mode for -*- lua -*- files
local editor = require("polywell")
local lume = require("polywell.lume")

local keywords = {"def", "defn", "defn-", "defonce",
                  "do", "if", "let", "let*", "var", "fn", "loop", "loop*",
                  "recur", "throw", "try", "catch", "finally", "set!",
                  "new", "monitor-enter", "monitor-exit", "quote",
                  "letfn", "case", "cond", "cond->", "cond->>", "condp",
                  "for", "when", "when-not", "when-first", "when-some",
                  "if-let", "if-not", "if-some", "->", "->>", "as->",
                  "doto", "and", "or", "dosync", "doseq", "dotimes",
                  "dorun", "doall", "ns", "in-ns", "with-open",
                  "binding", "with-redefs", "declare", "true", "false", "nil"}

keywords.comment_pattern = ";"

editor.define_mode("clojure", "edit",
                   {on_change = lume.fn(editor.colorize, keywords),
                    activate = lume.fn(editor.colorize, keywords),})

editor.set_color("clojure", {text={0, 180 ,0},
                             keyword={0, 255, 0},
                             str={200, 100, 0},
                             number={50, 175, 120},
                             comment={0, 100, 0}})

editor.add_auto_mode(".*clj", "clojure")

editor.bind("clojure", "tab", editor.complete)
