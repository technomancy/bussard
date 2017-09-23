#!/usr/bin/env lua
local lume = require("lume")
local t = require("lunatest")
local f = require("init")
local s = require("serpent")

local s_opts = {maxlevel=8,maxnum=64,nocode=true,comment=false}
pp = function(x) print(s.block(x, s_opts)) end

assert_stack = function(expected_stack, code)
   local env = f.make_env(nil, io.write)
   f.exec(env, code)
   t.assert_equal(s.line(expected_stack, s_opts), s.line(env.stack, s_opts))
end

t.suite("test_core")

t.run(nil, {"--verbose"})
