Lua Programming: Functions
==========================

<dfn>stack</dfn> is a list of items where items can be added (*pushed*)
or removed (*popped*) that behaves on the last-in-first-out principle,
which means that the last item that was added will be the first to be
removed. This is why such lists are called stacks: on a stack, you
cannot remove an item without first removing the items that are on top
of it. All operations therefore happen at the top of the stack. An item
is above another if it was added after that item and is below it if it
was added before that item.

A (also called a subroutine, a procedure, a routine or a subprogram) is
a sequence of instructions that perform a specific task and that can be
*called* from elsewhere in the program whenever that sequence of
instructions should be executed. Functions can also receive values as
input and return an output after potentially manipulating the input or
executing a task based on the input. Functions can be defined from
anywhere in a program, including inside other functions, and they can
also be called from any part of the program that has access to them:
functions, just like numbers and strings, are values and can therefore
be stored in variables and have all the properties that are common to
variables. These characteristics make functions very useful.

Because functions can be called from other functions, the Lua
interpreter (the program that reads and executes Lua code) needs to be
able to know what function called the function it is currently executing
so that, when the function terminates (when there is no more code to
execute), it can return to execution of the right function. This is done
with a stack called the call stack: each item in the call stack is a
function that called the function that is directly above it in the
stack, until the last item in the stack, which is the function currently
being executed. When a function terminates, the interpreter uses the
stack's pop operation to remove the last function in the list, and it
then returns to the previous function.

There are two types of functions: built-in functions and user-defined
functions. <dfn>Built-in functions</dfn> are functions provided with Lua
and include functions such as the `print` function, which you already
know. Some can be accessed directly, like the `print` function, but
others need to be accessed through a library, like the `math.random`
function, which returns a random number. <dfn>User-defined
functions</dfn> are functions defined by the user. User-defined
functions are defined using a function constructor:

``` {.lua}
local func = function(first_parameter, second_parameter, third_parameter)
    -- function body (a function's body is the code it contains)
end
```

The code above creates a function with three parameters and stores it in
the variable <var>func</var>. The following code does exactly the same
as the above code, but uses syntactic sugar for defining the function:

``` {.lua}
local function func(first_parameter, second_parameter, third_parameter)
    -- function body
end
```

It should be noted that, when using the second form, it is possible to
refer to the function from inside itself, which is not possible when
using the first form. This is because `local function foo() end`
translates to `local foo; foo = function() end` rather than
`local foo = function() end`. This means that <var>foo</var> is part of
the function’s environment in the second form and not in the first,
which explains why the second form makes it possible to refer to the
function itself.

In both cases, it is possible to omit the `local` keyword to store the
function in a global variable. Parameters work like variables and allow
functions to receive values. When a function is called, arguments may be
given to it. The function will then receive them as parameters.
Parameters are like local variables defined at the beginning of a
function, and will be assigned in order depending on the order of the
arguments as they are given in the function call; if an argument is
missing, the parameter will have the value `nil`. The function in the
following example adds two numbers and prints the result. It would
therefore print <samp>5</samp> when the code runs.

``` {.lua}
local function add(first_number, second_number)
    print(first_number + second_number)
end

add(2, 3)
```

Function calls are most of the time under the form `name(arguments)`.
However, if there is only one argument and it is either a table or a
string, and it isn't in a variable (meaning it is constructed directly
in the function call, expressed as a literal), the parentheses can be
omitted:

``` {.lua}
print "Hello, world!"
print {4, 5}
```

The second line of code in the previous example would print the memory
address of the table. When converting values to strings, which the
`print` function does automatically, complex types (functions, tables,
userdata and threads) are changed to their memory addresses. Booleans,
numbers and the nil value, however, will be converted to corresponding
strings.

The terms *parameter* and *argument* are often used interchangeably in
practice. In this book, and in their proper meanings, the terms
*parameter* and *argument* mean, respectively, a name to which the value
of the corresponding argument will be assigned and a value that is
passed to a function to be assigned to a parameter.

Returning values
----------------

Functions can receive input, manipulate it and give back output. You
already know how they can receive input (parameters) and manipulate it
(function body). They can also give output by returning one or many
values of any type, which is done using the return statement. This is
why function calls are both statements and expressions: they can be
executed, but they can also be evaluated.

``` {.lua}
local function add(first_number, second_number)
    return first_number + second_number
end

print(add(5, 6))
```

The code in the above function will first define the function `add`.
Then, it will call it with 5 and 6 as values. The function will add them
and return the result, which will then be printed. This is why the code
above would print <samp>11</samp>. It is also possible for a function to
return many values by separating the expressions that evaluate to these
values with commas.

Errors
------

There are three types of errors: syntactic errors, static semantic
errors and semantic errors. Syntactic errors happen when code is plainly
invalid. The following code, for example, would be detected by Lua as
invalid:

``` {.lua}
print(5 ++ 4 return)
```

The code above doesn't make sense; it is impossible to get a meaning out
of it. Similarly, in English, "cat dog tree" is not syntactically valid
because it has no meaning. It doesn't follow the rules for creating a
sentence.

Static semantic errors happen when code has a meaning, but still doesn't
make sense. For example, if you try adding a string with a number, you
get a static semantic error because it is impossible to add a string
with a number:

``` {.lua}
print("hello" + 5)
```

The code above follows Lua's syntactic rules, but it still doesn't make
sense because it is impossible to add a string with a number (except
when the string represents a number, in which case it will be coerced
into one). This can be compared in English to the sentence "I are big".
It follows the rules for creating sentences in English, but it still
doesn't make sense because "I" is singular and "are" is plural.

Finally, semantic errors are errors that happen when the meaning of a
piece of code is not what its creator thinks it is. Those are the worst
errors because they can be very hard to find. Lua will always tell you
when there is a syntactic error or a static semantic error (this is
called throwing an error), but it cannot tell you when there is a
semantic error since it doesn't know what you think the meaning of the
code is. These errors happen more often than most people would think
they do and finding and correcting them is something many programmers
spend a lot of time doing.

The process of finding errors and correcting them is called debugging.
Most of the time, programmers will spend more time finding errors than
actually correcting them. This is true for all types of errors. Once you
know what the problem is, it is usually simple to fix it, but sometimes,
a programmer can look at a piece of code for hours without finding what
is wrong in it.

### Protected calls

Throwing an error is the action of indicating, whether it is done
manually or automatically by the interpreter (the program that reads the
code and executes it), that something is wrong with the code. It is done
automatically by Lua when the code given is invalid, but it can be done
manually with the `error` function:

``` {.lua}
local variable = 500
if variable % 5 ~= 0 then
    error("It must be possible to divide the value of the variable by 5 without obtaining a decimal number.")
end
```

The `error` function also has a second argument, which indicates the
stack level at which the error should be thrown, but this will not be
covered in this book. The `assert` function does the same thing as the
`error` function, but it will only throw an error if its first argument
evaluates to nil or false and it doesn't have an argument that can be
used to specify the stack level at which the error should be thrown. The
assert function is useful at the start of a script, for example, to
check if a library that is required for the script to work is available.

It may be hard to understand why one would desire to voluntarily throw
an error, since the code in a program stops running whenever an error is
thrown, but, often, throwing errors when functions are used incorrectly
or when a program is not running in the right environment can be helpful
to help the person who will have to debug the code to find it
immediately without having to stare at the code for a long time without
realizing what is wrong.

Sometimes, it can be useful to prevent an error from stopping the code
and instead do something like displaying an error message to the user so
he can report the bug to the developer. This is called <dfn>exception
handling</dfn> (or <dfn>error handling</dfn>) and is done by catching
the error to prevent its propagation and running an exception handler to
handle it. The way it is done in different programming languages varies
a lot. In Lua, it is done using protected calls[^1]. They are called
protected calls because a function called in protected mode will not
stop the program if an error happens. There are two functions that can
be used to call a function in protected mode:

  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Function                           Description
                                     
  ---------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  `pcall(function, ...)`             Calls the function in protected mode and returns a status code (a boolean value whose value depends on if an error was thrown or not) and the values returned by the function, or the error message if the function was stopped by an error. Arguments can be given to the function by passing them to the `pcall` function after the first argument, which is the function that should be called in protected mode.
                                     

  `xpcall(function, handler, ...)`   Does the same thing as pcall, but, when the function errors, instead of returning the same values as those pcall would return, it calls the handler function with them as parameters. The handler function can then be used, for example, to display an error message. As for the `pcall` function, arguments can be passed to the function by being given to the `xpcall` function.
                                     
  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Stack overflow
--------------

The call stack, the stack that contains all the functions that were
called in the order in which they were called, was mentioned earlier.
That call stack in most languages, including Lua, has a maximum size.
This maximum size is so big that it should not be worried about in most
cases, but functions that call themselves (this is called recursivity
and such functions are called recursive functions) can reach this limit
if there is nothing to prevent them from calling themselves over and
over indefinitely. This is called a stack overflow. When the stack
overflows, the code stops running and an error is thrown.

Variadic functions
------------------

<dfn>Variadic functions</dfn>, which are also called vararg functions,
are functions that accept a variable number of arguments. A variadic
function is indicated by three dots ("...") at the end of its parameter
list. Arguments that do not fit in the parameters in the parameter list,
instead of being discarded, are then made available to the function
through a vararg expression, which is also indicated by three dots. The
value of a vararg expression is a list of values (not a table) which can
then be put in a table to be manipulated with more ease with the
following expression: `{...}`. In Lua 5.0, instead of being available
through a vararg expression, the extra arguments were available in a
special parameter called "arg". The following function is an example of
a function that would add the first argument to all the arguments it
receives, then add all of them together and print the result:

``` {.lua}
function add_one(increment, ...)
    local result = 0
    for _, number in next, {...} do
        result = result + number + increment
    end
end
```

It is not necessary to understand the code above as it is only a
demonstration of a variadic function.

The `select` function is useful to manipulate argument lists without
needing to use tables. It is itself a variadic function, as it accepts
an indefinite number of arguments. It returns all arguments after the
argument with the number given as its first argument (if the number
given is negative, it indexes starting from the end, meaning -1 is the
last argument). It will also return the number of arguments it received,
excluding the first one, if the first argument is the string "\#". It
can be useful to discard all arguments in an argument list before a
certain number, and, more originally, to distinguish between nil values
being sent as arguments and nothing being sent as an argument. Indeed,
`select` will distinguish, when `"#"` is given as its first argument,
nil values from no value. Argument lists (and return lists as well) are
instances of tuples, which will be explored in the chapter about tables;
the `select` function works with all tuples.

``` {.lua}
print((function(...) return select('#', ...) == 1 and "nil" or "no value" end)()) --> no value
print((function(...) return select('#', ...) == 1 and "nil" or "no value" end)(nil)) --> nil
print((function(...) return select('#', ...) == 1 and "nil" or "no value" end)(variable_that_is_not_defined)) --> nil

-- As this code shows, the function is able to detect whether the value nil was passed as an argument or whether there was simply no value passed.
-- In normal circumstances, both are considered as nil, and this is the only way to distinguish them.
```
