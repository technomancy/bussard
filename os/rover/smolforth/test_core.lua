local lume = require("lume")
local t = require("lunatest")
local f = require("init")

return {test_booleans=
           function()
              assert_stack({true, false}, "true false")
              assert_stack({true}, "false not")
              assert_stack({true, false}, "true false or false false or")
              assert_stack({true, false}, "true true and false true and")
           end,
        test_string=
           function()
              assert_stack({"hey", "there"}, '"hey" "there"')
              assert_stack({"hey there"}, '"hey " "there" ..')
           end,
        test_comments=
           function()
              assert_stack({}, "( this is a comment )")
              assert_stack({3, 4}, "3 ( a comment in between ) 4")
           end,
        test_stack_manipulation=
           function()
              assert_stack({1, 2}, "2 1 swap")
              assert_stack({3, 2, 4}, "4 3 2 rot")
              assert_stack({1, 1}, "1 dup")
              assert_stack({9, 8, 9}, "9 8 over")
              assert_stack({7, 8, 7, 8}, "7 8 2dup")
              assert_stack({1, 7, 8}, "7 8 1 -rot")
           end,
        test_arithmetic=
           function()
              assert_stack({1, 5}, "1 2 3 +")
              assert_stack({-1}, "2 3 -")
              assert_stack({18}, "9 2 *")
              assert_stack({3}, "6 2 /")
              assert_stack({-2}, "6 2 3 4 5 + - * swap /")
              assert_stack({0}, "1.5 0.5 - 1 -")
           end,
        test_comparisons=
           function()
              assert_stack({true}, "2 2 =")
              assert_stack({false}, "2 3 =")
              assert_stack({true, false}, "2 3 < 2 3 >")
              assert_stack({true, false, true}, "2 3 <= 2 3 >= 1 1 >=")
           end,
        test_define=
           function()
              assert_stack({5}, ": f 3 2 + ; f ")
              assert_stack({5, 5}, ": f 3 2 + ; : g f ( lol ) f ; g")
           end,
        test_if=
           function()
              assert_stack({1, 2}, "1 : f true if 2 then ; f")
              assert_stack({1}, "1 : f false if 2 3 then ; f")
              assert_stack({2, 8}, ": f true if 2 8 else 4 then ; f")
              assert_stack({4}, ": f false if 2 1 else 4 then ; f")
              assert_stack({92},
                 "true : f if 3 2 1 + = if 92 else 2 then then ; f")
              assert_stack({32},
                 ": f false if 77 2 1 + = if 92 else 2 then else 32 then ; f")
           end,
        test_do=
           function()
              assert_stack({55}, ": f 0 10 1 do i + loop ; f")
              assert_stack({6}, ": f 1 do + loop ; 1 2 3 2 f")
              assert_stack({4, 5, 6},
                 ": inclast3 3 1 do rot 1 + loop ; 1 2 3 " ..
                    ": f 3 1 do inclast3 loop ; f ")
              assert_stack({1, 2, 3, 4}, ": f 4 1 do i loop ; f")
           end,
        test_begin=
           function()
              assert_stack({8}, ": f begin 1 - dup 4 % 0 = until ; 11 f")
           end,
        test_sandbox=
           function()
              local x = 0
              local fn = f.stack_fn(1, function(y) x = y return y end)
              local env = f.make_env(nil, io.write, {fn=fn})
              f.exec(env, "38 fn")
              t.assert_equal(38, x)
           end,
        test_save=
           function()
              local env = f.make_env(nil, io.write, {print=print})
              f.exec(env, ": f 2 1 + ; f")
              t.assert_string(f.save(env, {"print"}))
           end,
        test_load=
           function()
              local env = f.make_env(nil, io.write)
              f.exec(env, ": f 2 1 + ; f")
              local env2 = f.load(f.save(env), nil, io.write)
              f.exec(env2, "f f * +")
              t.assert_equal(1, #env2.stack)
              t.assert_equal(12, env2.stack[1])
           end,
}
