Lua Programming: Tables
=======================

Tables are the only [data
structure](Wikipedia:data structure "wikilink") in Lua, but they are
more flexible than the data structures in many other languages. They are
also called dictionaries (because they make values correspond to
indices, as the definitions in a dictionary correspond to the terms),
associative arrays (because they associate values to indices, thus
making them be arrays of values that are associated to indices), hash
tables, symbol tables, hash maps and maps. They are created using tables
constructors, which are defined by two braces that may optionally
contain values separated by commas or semicolons. The following example
demonstrates an array (an ordered list of values) and demonstrates how
an array's length can be obtained.

``` {.lua}
local array = {5, "text", {"more text", 463, "even more text"}, "more text"}
print(#array) --> 4 ; the # operator for strings can also be used for arrays, in which case it will return the number of values in the array
-- This example demonstrates how tables can be nested in other tables. Since tables themselves are values, tables can include other tables. Arrays of tables are called multi-dimensional arrays.
```

Tables may contain values of any type except nil. This is logical: nil
represents the absence of a value. Inserting "the absence of a value" in
a table wouldn't make sense. Values in tables may be separated either by
commas or semicolons, and they may continue on many lines. It is common
to use commas for single-line table constructors and semicolons for
multi-line tables, but this is not necessary.

``` {.lua}
local array = {
    "text";
    {
        "more text, in a nested table";
        1432
    };
    true -- booleans can also be in tables
}
```

Tables are composed of fields, which are pairs of values, one of which
is a index (also called a key) the other being the value that
corresponds to that index. In arrays, the indices are always numerical
values. In dictionaries, the indices can be any value.

``` {.lua}
local dictionary = {
    "text";
    text = "more text";
    654;
    [8] = {}; -- an empty table
    func = function() print("tables can even contain functions!") end;
    ["1213"] = 1213
}
```

As the example above illustrates, values can be added to a dictionary
like they are in arrays with just the value (in which case the index
will be a number), with an identifier and an equal sign prefixing the
value (in which case the index will be the string that corresponds to
the identifier) or with a value enclosed in parentheses and an equal
sign prefixing the value (in which case the index is the value in
parentheses). The latter is the most flexible way to make an index
correspond to a value because it can be used with any value or
expression.

It is possible to access a value in a table by putting the index the
value corresponds to inside brackets, after an expression that evaluates
to the table:

``` {.lua}
local dictionary = {number = 6}
print(dictionary["number"]) --> 6
```

If the index is a string which follows the criteria for Lua identifiers
(it doesn't contain spaces, start with a number or contain anything else
than numbers, letters and underscores), it can be accessed without
brackets, by prefixing the string with a dot:

``` {.lua}
local dictionary = {["number"] = 6}
print(dictionary.number) --> 6
```

The two previous examples create an identical table and print the same
value, but define and access the indices using different notations. It
is also possible, in the case of tables that contain other tables to
access a value that is inside the nested table by first indexing the
first table to get the nested table, and then indexing the value inside
of the nested table:

``` {.lua}
local nested_table = {
    [6] = {
        func = function() print("6") end
    }
}
nested_table[6].func() -- This will access the nested table, access the function that is inside it and then call that function, which will print the number 6.
```

Foreach loop
------------

The chapter on statements described two types of loops:
condition-controlled loops and count-controlled loops. In Lua, there is
a third type of loop, the foreach loop, which is also called the
*generic for loop*. A foreach loop is a loop that allows the programmer
to execute code for each field in a table. The example below
demonstrates a foreach loop that traverses the items in an array of
numbers and prints all the indices and the sum of the values that
correspond to them with the number one:

``` {.lua}
local array = {5, 2, 6, 3, 6}

for index, value in next, array do
    print(index .. ": " .. value + 1)
end
-- Output:
-- 1: 6
-- 2: 3
-- 3: 7
-- 4: 4
-- 5: 7
```

The two loops in the following example will do the same thing as the
loop in the previous example.

``` {.lua}
local array = {5, 2, 6, 3, 6}

for index, value in pairs(array) do
    print(index .. ": " .. value + 1)
end

for index, value in ipairs(array) do
    print(index .. ": " .. value + 1)
end
```

The method shown in the first example does the same thing as the first
loop in the previous example. However, the last one (the one that uses
the `ipairs` function) will only iterate up to the first integer key
absent from the table, which means it will only iterate through
numerical values that are in order as they are in an array. There used
to be two functions called `table.foreach` and `table.foreachi`, but
they were deprecated in Lua 5.1 and removed in Lua 5.2.
<dfn>Deprecation</dfn> is a status applied to a feature or practice to
indicate that it has been removed or superseded and that it should be
avoided. Using the foreach loop to traverse tables is therefore
preferable to using these two functions.

Unpacking tables
----------------

In Lua, a distinction is made between a tuple, a simple list of values,
and a table, a data structure that maps indices to values. A function
call to a function that returns many values evaluates to a tuple. A list
of values in an assignment statement where many variables are assigned
at once is also a tuple. The vararg expression, which is used in
variadic functions, is also a tuple. Because a tuple is a list of values
and therefore not a single value, it cannot be stored in a variable,
although it can be stored in many variables. It is possible to convert a
tuple into an array by putting the expression that evaluates to the
tuple in a table constructor.

``` {.lua}
function return_tuple()
    return 5, 6 -- Because this function returns two values, a function call to it evaluates to a tuple.
end

local array = {return_tuple()} -- same as local array = {5, 6}
local a, b = return_tuple() -- same as local a, b = 5, 6
print(return_tuple()) -- same as print(5, 6)
```

It is possible to unpack an array (change it from a table to a tuple) by
using the `unpack` function of the table library with the array as an
argument:

``` {.lua}
local array = {7, 4, 2}
local a, b, c = table.unpack(array)
print(a, b, c) --> 7, 4, 2
```

In Lua versions preceding Lua 5.2, the `unpack` function was part of the
basic library. It has since been moved to the table library.

Methods
-------

Because tables can contain functions and associate a name to these
functions, they will often be used to create libraries. Lua also has
syntactic sugar that can be used to create methods, functions that are
used to manipulate an object, which will generally be represented by the
table. This might be a bit hard to understand by someone who doesn't
know about object-oriented programming, which is outside the scope of
this book, so there's nothing wrong with not understanding the purpose
of this. The two examples below do exactly the same thing:

``` {.lua}
local object = {}

function object.func(self, arg1, arg2)
    print(arg1, arg2)
end

object.func(object, 1, 2) --> 1, 2
```

``` {.lua}
local object = {}

function object:func(arg1, arg2)
    print(arg1, arg2)
end

object:func(1, 2) --> 1, 2
```

When calling a function that is in a table and that corresponds to a
index that is a string, using a colon instead of a dot will add a hidden
argument which is the table itself. Similarly, defining a function in a
table using a colon instead of a dot will add a hidden <var>self</var>
parameter in the parameter list. Usage of a colon to define the function
doesn't mean a colon must also be used to call the function and
vice-versa, because these are completely interchangeable.

Sorting
-------

Sorting tables in Lua can be relatively trivial. In most cases, the
`sort` function of the table library can be used to sort tables, which
makes everything relatively easy. The `sort` function sorts elements of
an array in a given order, in-place (i.e. without creating a new array).
If a function is provided as a second argument, it should receive two
elements of the array and return true when the first element should come
before the second in the final order. If no second argument is provided,
Lua will sort the elements in the array according to the `&lt;`
operator, which returns true when used with two numbers and when the
first number is smaller than the second, but which also works with
strings, in which case it returns true when the first string comes
before the other in an manner similar to alphabetical order but
including other characters as well.

Metatables
----------

Metatables are tables that can be used to control the behavior of other
tables. This is done through <dfn>metamethods</dfn>, fields in that
table which indicate to the Lua virtual machine what should be done when
code attempts to manipulate the table in specific ways. Metamethods are
defined by their name. The `index` metamethod, for example, tells Lua
what to do when code attempts to index in a table a field that doesn't
exist yet. A table's metatable can be set using the `setmetatable`
function, which accepts two table as arguments, the first table being
the table of which the metatable should be set, and the second being the
metatable to which the table's metatable should be set. There is also a
`getmetatable` function, which returns the metatable of a table. The
following code demonstrates how a metatable could be used to make all
non-existent fields in a table appear as if they contained a number;
these numbers are generated randomly, using the `math.random` function:

``` {.lua}
local associative_array = {
    defined_field = "this field is defined"
}

setmetatable(associative_array, {
    __index = function(self, index)
        return math.random(10)
    end
})
```

There are many things that may be noticed in the above example. One of
the things that may be noticed is that the name of the field containing
the `index` metamethod is prefixed with two underscores. This is always
the case: when Lua looks in a table's metatable for metamethods, it
looks for indices that correspond to the name of a metamethod and that
start with two underscores. Another thing that may be noticed is that
the `index` metamethod is in fact a function (most metamethods are
functions, but not all are), which takes two arguments. The first
argument, <var>self</var>, is the table of which the `index` metamethod
was invoked. In the case here, we could just refer directly to the
<var>associative\_array</var> variable, but this is useful when a single
metatable is used for many tables. The second argument is the index that
was attempted to be indexed. Finally, it can be noted that the function
tells the Lua virtual machine what should be given to the code that
indexed the table by returning a value. Most metamethods can only be
functions, but the `index` metamethod can also be a table. If it is a
table, Lua will, when a program attempts to index a field of the table
that doesn't exist, look in that table for the same index. If it finds
it, it will return the value corresponding to that index in the table
that is specified in the `index` metamethod.

newindex(self, index, value)
:   The `newindex` metamethod can be used to tell the Lua virtual
    machine what to do when a program attempts to add a new field in a
    table. It may only be a function, and it will only be called if
    there isn't already a field with the index to which the program
    attempts to write. It has three arguments: the first two are the
    same as the arguments of the `index` metamethod, and the third is
    the value that the program attempted to set the value of the field
    to.
concat(self, value)
:   The `concat` metamethod can be used to tell the Lua virtual machine
    what to do when a program attempts to concatenate a value to the
    table, using the concatenation operator (`..`).
call(self, ...)
:   The `call` metamethod can be used to specify what should happen when
    a program attempts to call the table. This makes it possible to
    indeed make a table behave as if it was a function. The arguments
    passed when the table was called are given after the <var>self</var>
    argument, which is the table itself, as always. This metamethod can
    be used to return values, in which case calling the table will
    return these values.
unm(self)
:   This metamethod can be used to specify the effect of using the unary
    minus operator on the table.
tostring(self)
:   This metamethod can be a function that returns the value the
    `tostring` function should return when it is called with the table
    as its argument.
add(self, value)\
sub(self, value)\
mul(self, value)\
div(self, value)\
mod(self, value)\
pow(self, value)
:   These metamethods can be used to tell the virtual machine what to do
    respectively when a value is added to, subtracted from, multiplied
    to or divided by the table. The last two are similar, but are
    respectively for the modulus operator (`%`) and the exponentiation
    operator (`^`).
eq(self, value)
:   This metamethod is used by the equality operator (`==`). It will
    only be used if the two values compared have the same metatable. The
    non-equality operator (`~=`) uses the opposite of the result of this
    function.
lt(self, value)\
le(self, value)
:   These metamethods are used by the "less than" and "less than or
    equal to" operators. The "greater than" and "greater than or equal
    to" operators will return the opposite of what these metamethods
    return. They will only be used if the values have the same
    metatable.
gc(self)
:   This metamethod is called by Lua before the garbage collector
    collects a value a metatable with this metamethod is attached to. It
    only works with userdata values, and cannot be used with tables.
len(self)
:   This metamethod is called by Lua when the length operator (`#`) is
    used on a userdata value. It cannot be used with tables.
metatable
:   If this metamethod is present (it can be anything), using
    `getmetatable` on the table will return this metamethod's value
    instead of the metatable, and changing the table's metatable with
    the `setmetatable` function will not be allowed.
mode
:   This metamethod should be a string that can contain the letter "k"
    or "v" (or both). If the letter "k" is present, the table's keys
    will be weak. If the letter "v" is present, the table's values will
    be weak. What this means will be explained later, along with garbage
    collection.

Metatables, although normal Lua programs can only use them with tables,
are in fact the mechanism at the core of the way Lua handles operators
and operations, and they can in fact theoretically be used with any
value. However, Lua only allows them to be used with tables and userdata
values created with the undocumented `newproxy` function. Using Lua's C
API or the debug library, it is possible to set the metatable of values
of other types like numbers and strings.

It is sometimes desirable to perform an operation on a table without
invoking metamethods. This is possible for indexing, adding fields in a
table, checking for equality and obtaining the length of a table using
the `rawget`, `rawset`, `rawequal` and `rawlen` functions, respectively.
The first returns the value corresponding to the index given to it as
its second argument in the table given to it in the first argument. The
second sets to the value given in its third argument the value
corresponding to the index given in the second argument in the table
given in the first argument. The third returns whether the two values
given to it are equal. Finally, the fourth returns the length (an
integer) of the object it is given, which must be a table or a string.

Iterators
---------

An iterator is a function that is used conjunctly with a foreach loop.
In most cases, an iterator is used to loop over or *traverse* a
data-structure. Examples of this are the iterators returned by the
`pairs` and `ipairs` functions which are used, respectively, to traverse
the elements of a table or of an array. The `pairs` function, for
example, returns the `next` function, along with the table it is given
as an argument, which explains the equivalence `in pairs(dictionary)`
with `in next, dictionary`, since the former actually evaluates to the
latter.

There is no requirement for iterators to always work with data
structures, as iterators can be designed for any case where looping is
required. For example, the `file:lines` function returns an iterator
which returns a line from a file at each iteration. Similarly, the
`string.gmatch` function returns an iterator which returns a match of a
pattern in a string at each iteration. This code, for example, would
print all the lines in a file called "file.txt".

``` {.lua}
for line in io.open("file.txt"):lines() do
    print(line)
end
```

### Creating iterators

An iterator consists of three things:

-   A transformation function
-   A state value
-   One or many loop variables

The *transformation function* is used to modify the values of the *loop
variables* (the variables that appear between the `for` and the `in`)
for each iteration of the loop. This function is invoked before every
iteration and takes, as arguments, the values the loop variables were
set to during the last iteration. The function is expected to return a
tuple (one or many values) containing the new values for these
variables. The loop variables will be set to the components of the
returned tuple and the loop will go through an iteration. Once that
iteration has completed (so long as it has not been interrupted by a
`break` or `return` statement) the transformation function will be
called again and will return another set of values, which the loop
variables will be set to for the next iteration, and so on and so forth.
This cycle of calling the transformation function and iterating over the
statements of the loop will continue until the transformation function
returns `nil`.

Along with the loop variables, the transformation function is also
passed in a state value which will remain constant throughout the loop.
The state value can be used, for example, to maintain a reference to the
data structure, file handle or resource the transformation function is
iterating over.

An example of a transformation function which will produce a sequence of
numbers up to 10 is given below. This transformation function requires
only one loop variable which will have it's value stored in
<var>value</var>.

``` {.lua}
function seq(state, value)
    if (value >= 10) then
        return nil -- Returning nil will terminate the loop
    else
        local new_value = value + 1 -- The next value to use within the loop is the current value of `value` plus 1.
        -- This value will be used as the value of `value` the next time this function is called.
        return new_value
    end
end
```

The generic for loop expects a tuple containing the transformation
function, the state value and the initial values of the loop variables.
This tuple can be included directly after the `in` keyword.

``` {.lua}
-- This will display the numbers from 1 to 10.
for value in seq, nil, 0 do
    print(value)
end
```

In most cases, however, this tuple will be returned by a function. This
allows for the use of *iterator factories* which, when called, returns a
new iterator that can be used with a generic for loop:

``` {.lua}
function count_to_ten()
    local function seq(state, value)
        if (value >= 10) then
            return nil
        else
            local new_value = value + 1
            return new_value
        end
    end
    return seq, nil, 0
end

for x in count_to_ten() do
    print(value)
end
```

Since Lua supports closures and functions as first-class objects, the
iterator factory can also take arguments which can be used within the
transformation function.

``` {.lua}
function count_to(limit)
    local function seq(state, value)
        if (value >= limit) then
            return nil
        else
            local new_value = value + 1
            return new_value
        end
    end
    return seq, nil, 0
end

for value in count_to(10) do -- Print the numbers from 1 to 10
    print(value)
end

for value in count_to(20) do -- Print the numbers from 1 to 20
    print(value)
end
```
