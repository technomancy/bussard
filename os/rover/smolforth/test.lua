#!/usr/bin/env lua
local lume = require("lib.lume")
local t = require("lib.lunatest")
local f = require("init")

assert_stack = function(expected_stack, code)
   local env = f.make_env(nil, io.write)
   env.input = code
   f.eval(env)
   t.assert_equal(lume.serialize(expected_stack), lume.serialize(env.stack))
end

t.suite("test_core")

t.run(nil, {"--verbose"})
