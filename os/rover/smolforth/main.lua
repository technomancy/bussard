#!/usr/bin/env lua
local smolforth = require("init")

print("welcome to smolforth my friend.")
smolforth.repl(smolforth.make_env(io.read, io.write))
