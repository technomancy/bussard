Lua Programming: Statements
===========================

*Statements* are pieces of code that can be executed and that contain an
instruction and expressions to use with it. Some statements will also contain
code inside of themselves that may, for example, be run under certain
conditions. Dissimilarly to expressions, they can be put directly in code and
will execute. Lua has few instructions, but these instructions, combined with
other instructions and with complex expressions, give a good amount of control
and flexibility to the user.

Assignment
----------

Programmers frequently need to be able to store values in the memory to be able
to use them later. This is done using variables.  *Variables* are references to
a value which is stored in the computer's memory. They can be used to access a
number later after storing it in the memory. *Assignment* is the instruction
that is used to assign a value to a variable. It consists of the name of the
variable the value should be stored in, an equal sign, and the value that should
be stored in the variable:

    variable = 43
    print(variable) --> 43

As demonstrated in the above code, the value of a variable can be accessed by
putting the variable's name where the value should be accessed.

### Identifiers

Identifiers in Lua are also called names. They can be any text composed of
letters, digits, and underscores and not beginning with a digit. They are used
to name variables and table fields, which will be covered in the chapter about
tables.

Here are some valid names:

-   `name`
-   `hello`
-   `_`
-   `_tomatoes`
-   `me41`
-   `__`
-   `_thisIs_StillaValid23name`

Here are some invalid names:

-   `2hello` : starts with a digit
-   `th$i` : contains a character that isn't a letter, a digit or an underscore
-   `hel!o` : contains a character that isn't a letter, a digit or an underscore
-   `563text` : starts with a digit
-   `82_something` : starts with a digit

Also, the following keywords are reserved by Lua and can not be used as names:
`and`, `end`, `in`, `repeat`, `break`, `false`, `local`, `return`, `do`, `for`,
`nil`, `then`, `else`, `function`, `not`, `true`, `elseif`, `if`, `or`, `until`,
`while`.

When naming a variable or a table field, you must choose a valid name for it. It
must therefore start with a letter or an underscore and only contain letters,
underscores and digits. Note that Lua is case sensitive. This means that `Hello`
and `hello` are two different names.

### Scope

The scope of a variable is the region of the code of the program where that
variable is meaningful. The examples of variables you have seen before are all
examples of global variables, variables which can be accessed from anywhere in
the program. Local variables, on the other hand, can only be used from the
region of the program in which they were defined and in regions of the program
that are located inside that region of the program. They are created exactly in
the same way as global variables, but they must be prefixed with the `local`
keyword.

The `do` statement will be used to describe them. The `do` statement is a
statement that has no other purpose than to create a new block of code, and
therefore a new scope. It ends with the `end` keyword:

    -- This defines a local variable that can be accessed from anywhere in
    -- the script since it was defined in the main region.
    local variable = 13
    do -- This statement creates a new block and also a new scope.
        -- This adds 5 to the variable, which now equals 18.
        variable = variable + 5
        -- This creates a variable with the same name as the previous variable,
        --  but this one is local to the scope created by the do statement.
        local variable = 17
        variable = variable - 1 -- This subtracts 1 from the local variable
        print(variable) --> 16
    end
    print(variable) --> 18

When a scope ends, all the variables in it are gotten rid of. Regions of code
can use variables defined in regions of code they are included in, but if they
"overwrite" them by defining a local variable with the same name, that local
variable will be used instead of the one defined in the other region of
code. This is why the first call to the print function prints 16 while the
second, which is outside the scope created by the `do` statement, prints 18.

In practice, only local variables should be used because they can be defined and
accessed faster than global variables, since they are stored in registers
instead of being stored in the environment of the current function, like global
variables. Registers are areas that Lua uses to store local variables to access
them quickly, and can only usually contain up to 200 local variables. The
processor, an important component of all computers, also has registers, but
these are not related to Lua's registers. Each function (including the main
thread, the core of the program, which is also a function) also has its own
environment, which is a table that uses indices for the variable names and
stores the values of these variables in the values that correspond to these
indices.

### Forms of assignment

Some assignment patterns are sufficiently common for syntactic sugar to have
been introduced to make their use simpler.

#### Chained assignment

Chained assignment is a type of assignment that gives a single value to many
variables. The code `a = b = c = d = 0`, for example, would set the values of a,
b, c and d to 0 in C and Python. In Lua, this code will raise an error because
Lua does not have syntactic sugar for chained assignment, so it is necessary to
write the previous example like this:

    d = 0
    c = d -- or c = 0
    b = c -- or b = 0
    a = b -- or a = 0

#### Parallel assignment

Parallel assignment, which is also called simultaneous assignment and multiple
assignment, is a type of assignment that simultaneously assigns different values
(they can also be the same value) to different variables. Unlike chained
assignment and augmented assignment, parallel assignment is available in Lua.

The example in the previous section can be rewritten to use parallel assignment:

    a, b, c, d = 0, 0, 0, 0

If you provide more variables than values, some variables will be not be
assigned any value. If you provide more values than variables, the extra values
will be ignored. More technically, the list of values is adjusted to the length
of list of variables before the assignment takes place, which means that excess
values are removed and that extra nil values are added at its end to make it
have the same length as the list of variables. If a function call is present *at
the end of the values list*, the values it returns will be added at the end of
that list, unless the function call is put between parentheses.

Moreover, unlike most programming languages Lua enables reassignment of
variables' values through permutation. For example:

    first_variable, second_variable = 54, 87
    first_variable, second_variable = second_variable, first_variable
    print(first_variable, second_variable) --> 87 54

This works because the assignment statement evaluates all the variables and
values before assigning anything. Assignments are performed as if they were
really simultaneous, which means you can assign at the same time a value to a
variable and to a table field indexed with that variable’s value before it is
assigned a new value. In other words, the following code will set
`dictionary[2]`, and not `dictionary[1]`, to 12.

    dictionary = {}
    index = 2
    index, dictionary[index] = index - 1, 12

Conditional statement
---------------------

Conditional statements are instructions that check whether an expression is true
and execute a certain piece of code if it is. If the expression is not true,
they just skip over that piece of code and the program continues. In Lua, the
only conditional statement uses the `if` instruction. False and nil are both
considered as false, while everything else is considered as true.

    local number = 6

    if number < 10 then
        print("The number " .. number .. " is smaller than ten.")
    end

    -- Other code can be here and it will execute regardless of whether the
    -- code in the conditional statement executed.

In the code above, the variable number is assigned the number 6 with an
assignment statement. Then, a conditional statement checks if the value stored
in the variable number is smaller than ten, which is the case here. If it is, it
prints "The number 6 is smaller than ten.".

It is also possible to execute a certain piece of code *only* if the expression
was not true by using the `else` keyword and to chain conditional statements
with the `elseif` keyword:

    local number = 15

    if number < 10 then
        print("The number is smaller than ten.")
    elseif number < 100 then
        print("The number is bigger than or equal to ten, but smaller than one hundred.")
    elseif number ~= 1000 and number < 3000 then
        print("The number is bigger than or equal to one hundred, smaller than three thousands and is not exactly one thousand.")
    else
        print("The number is either 1000 or bigger than 2999.")
    end

Note that the `else` block must always be the last one. There cannot be
an `elseif` block after the `else` block. The `elseif` blocks are only
meaningful if none of the blocks that preceded them was executed.

Operators used to compare two values, some of which are used in the code
above, are called relational operators. If the relation is true, they
return the boolean value `true`. Otherwise, they return the boolean
value `false`.

  ------------------------------------------------------------------------------
                          equal to   not equal to   greater than   less than

  ------------------------------------------------------------------------------
  Mathematical notation   =          ≠               >              <


  Lua operator            ==         ~=              >              <

  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
                          greater than or equal to      less than or equal to

  ------------------------------------------------------------------------------
  Mathematical notation     ≥                             ≤


  Lua operator              >=                            <=

  ------------------------------------------------------------------------------

The code above also demonstrates how the `and` keyword can be used to combine
many boolean expressions in a conditional expression.

Loops
-----

Frequently, programmers will need to run a certain piece of code or a similar
piece of code many times, or to run a certain piece of code a number of times
that may depend on user input. A loop is a sequence of statements which is
specified once but which may be carried out several times in succession.

### Condition-controlled loops

Condition-controlled loops are loops that are controlled by a condition.  They
are very similar to conditional statements, but instead of executing the code if
the condition is true and skipping it otherwise, they will keep running it while
the condition is true, or until the condition is false. Lua has two statements
for condition-controlled loops: the `while` loop and the `repeat` loop. Such
loops will run code, then check if the condition is true. If it is true, then
they run the code again, and they repeat until the condition is false. When the
condition is false, they stop repeating the code and the program flow
continues. Each execution of the code is called an iteration. The difference
between `while` and `repeat` loops is that `repeat` loops will check the
condition at the end of the loop while `while` loops will check it at the start
of the loop. This only makes a difference for the first iteration: `repeat`
loops will always execute the code at least once, even if the condition is false
at the first time the code is executed. This is not the case for `while` loops,
which will only execute the code the first time if the condition is actually
true.

    local number = 0

    while number < 10 do
        print(number)
        number = number + 1 -- Increase the value of the number by one.
    end

The code above will print 0, then 1, then 2, then 3, and so on, until 9.  After
the tenth iteration, number will no longer be smaller than ten, and therefore
the loop will stop executing. Sometimes, loops will be meant to run forever, in
which case they are called infinite loops. Renderers, software processes that
draw things on the screen, for example, will often loop constantly to redraw the
screen to update the image that is shown to the user. This is frequently the
case in video games, where the game view must be updated constantly to make sure
what the user sees is kept up-to-date. However, cases where loops need to run
forever are rare and such loops will often be the result of errors.  Infinite
loops can take a lot of computer resources, so it is important to make sure that
loops will always end even if unexpected input is received from the user.

    local number = 0

    repeat
        print(number)
        number = number + 1
    until number >= 10

The code above will do exactly the same thing as the code that used a `while`
loop above. The main differences is that, unlike `while` loops, where the
condition is put between the `while` keyword and the `do` keyword, the condition
is put at the end of the loop, after the `until` keyword. The `repeat` loop is
the only statement in Lua that creates a block and that is not closed by the
`end` keyword.

### Count-controlled loops

Incrementing a variable is increasing its value by steps, especially by steps of
one. The two loops in the previous section incremented the variable number and
used it to run the code a certain number of times. This kind of loop is so
common that most languages, including Lua, have a built-in control structure for
it. This control structure is called a count-controlled loop, and, in Lua and
most languages, is defined by the `for` statement. The variable used in such
loops is called the loop counter.

    for number = 0, 9, 1 do
        print(number)
    end

The code above does exactly the same thing as the two loops presented in the
previous section, but the number variable can only be accessed from inside the
loop because it is local to it. The first number following the variable name and
the equality symbol is the initialization. It is the value the loop counter is
initialized to. The second number is the number the loop stops at. It will
increment the variable and repeat the code until the variable reaches this
number.  Finally, the third number is the increment: it is the value the loop
counter is increased of at each iteration. If the increment is not given, it
will be assumed to be 1 by Lua. The code below would therefore print
1, 1.1, 1.2, 1.3, 1.4 and 1.5.

    for n = 1, 2, 0.1 do
        print(n)
        if n >= 1.5 then
            break -- Terminate the loop instantly and do not repeat.
        end
    end

The reason the code above does not go up to 2 and only up to 1.5 is because of
the `break` statement, which instantly terminates the loop.  This statement can
be used with any loop, including `while` loops and `repeat` loops. Note that the
`>=` operator was used here, although the `==` operator would theoretically
have done the job as well. This is because of decimal precision errors. Lua
represents numbers with the double-precision floating-point format, which stores
numbers in the memory as an approximation of their actual value. In some cases,
the approximation will match the number exactly, but in some cases, it will only
be an approximation. Usually, these approximations will be close enough to the
number for it to not make a difference, but this system can cause errors when
using the equality operator. This is why it is generally safer when working with
decimal numbers to avoid using the equality operator. In this specific case, the
code would not have worked if the equality operator had been used (it would
have continued going up until 1.9), but it works with the `>=` operator.

Blocks
------

A block is a list of statements that are executed sequentially. These statements
can include empty statements, that do not contain any instruction. Empty
statements can be used to start a block with a semicolon or write two semicolons
in sequence.

Function calls and assignments may start with a parenthesis, which can lead to
an ambiguity. This fragment is an example of this:

    a = b + c
    (print or io.write)('done')

This code could be interpreted in two ways:

    a = b + c(print or io.write)('done')
    a = b + c; (print or io.write)('done')

The current parser always sees such constructions in the first way, interpreting
the opening parenthesis as the start of the arguments to a call. To avoid this
ambiguity, it is a good practice to always precede with a semicolon statements
that start with a parenthesis:

    ;(print or io.write)('done')

### Chunks

The unit of compilation of Lua is called a chunk. A chunk can be stored in a
file or in a string inside the host program. To execute a chunk, Lua first
precompiles the chunk into instructions for a virtual machine, and then it
executes the compiled code with an interpreter for the virtual machine.

The `load` function can be used to load a chunk. If the first parameter given to
the `load` function is a string, the chunk is that string. In this case, the
string may be either Lua code or Lua bytecode. If the first parameter is a
function, `load` will call that function repeatedly to get the pieces of the
chunk, each piece being a string that will be concatenated with the previous
strings. It is then considered that the chunk is complete when nothing or the
empty string is returned.

The `load` function will return the compiled chunk as a function if there is no
syntactic error. Otherwise, it will return nil and the error message.

The second parameter of the `load` function is used to set the source of the
chunk. All chunks keep a copy of their source within them, in order to be able
to give appropriate error messages and debugging information.  By default, that
copy of their source will be the code given to `load` (if code was given; if a
function was given instead, it will be "=(load)"). This parameter can be used to
change it. This is mostly useful when compiling code to prevent people from
getting the original source back. It is then necessary to remove the source
included with the binary representation because otherwise the original code can
be obtained there.

The third parameter of the `load` function can be used to set the environment of
the generated function and the fourth parameter controls whether the chunk can
be in text or binary. It may be the string "b" (only binary chunks), "t" (only
text chunks), or "bt" (both binary and text). The default is "bt".

There is also a `loadfile` function that works exactly like `load`, but instead
gets the code from a file. The first parameter is the name of the file from
which to get the code. There is no parameter to modify the source stored in the
binary representation, and the third and fourth parameters of the `load`
function correspond to the second and third parameters of this function. The
`loadfile` function can also be used to load code from the standard input, which
will be done if no file name is given.

The `dofile` function is similar to the `loadfile` function, but instead of
loading the code in a file as a function, it immediately executes the code
contained in a source code file as a Lua chunk. Its only parameter is used to
specify the name of the file it should execute the contents of; if no argument
is given, it will execute the contents of the standard input. If the chunk
returns values, they will be provided by the call to the `dofile`
function. Because `dofile` does not run in protected mode, all errors in chunks
executed through it will propagate.
