return {test_booleans=
           function()
              assert_stack({true, false}, "true false")
              assert_stack({true}, "false not")
              assert_stack({true, false}, "true false or false false or")
              assert_stack({true, false}, "true true and false true and")
           end,
        test_if=
           function()
              assert_stack({2}, "true if 2 then")
              assert_stack({}, "false if 2 3 then")
              assert_stack({2, 8}, "true if 2 8 else 4 then")
              assert_stack({4}, "false if 2 1 else 4 then")
              assert_stack({92}, "true if 3 2 1 + = if 92 else 2 then then")
           end,
        test_comments=
           function()
              assert_stack({}, "( this is a comment )")
              assert_stack({3, 4}, "3 ( a comment in between ) 4")
           end,
        test_nested_if=
           function()
              assert_stack({32}, "false if 77 2 1 + = if 92 else 2 then "..
                 " else 32 then")
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
}
