Lua Programming: Standard Library
=================================

Lua is a language that is said to "not be provided with batteries". This
means that its libraries are kept to the minimum necessary to do some
stuff. Lua relies on its community to create libraries that can be used
to perform more specific tasks. There are ten libraries available in
Lua. The *Lua Reference Manual* provides documentation for all the
libraries[^1], so they will only be briefly described here[^2]. All the
libraries except the basic and the package libraries provide their
functions and values as fields of a table.

Basic library
-------------

The basic library provides core functionality to Lua. All its functions
and values are directly available in the global environment, and all
functions and values available directly in the global environment by
default are part of the basic library.

### Assertion

An <dfn>assertion</dfn> is a predicate that is assumed by the developer
to be true. They are used in programs to ensure that a specific
condition is true at a specific moment of the execution of a program.
Assertions are used in [unit tests](../Unit testing "wikilink") to
verify that a program works correctly, but are also used in program
code, in which case the program will fail when an assertion is false,
either to verify that the environment in which the program is correct,
or to verify that no error was made in program code and to generate
appropriate error messages to make it easier to find bugs in the code
when something doesn't happen as expected. In Lua, assertions are made
with the `assert` function, which accepts a condition and a message
(which will default to "assertion failed!") as parameters. When the
condition evaluates to false, `assert` throws an error with the message.
When it evaluates to true, `assert` returns all its arguments.

### Garbage collection

<dfn>Garbage collection</dfn> is a form of automatic memory management
implemented by Lua and many other languages. When a program needs to
store data in a variable, it asks the operating system to allocate space
in the computer's memory to store the variable's value. Then, when it
doesn't need the space anymore (generally because the variable fell out
of scope), it tells the operating system to deallocate the space so that
another program may use it. In Lua, the actual process is much more
complex, but the basic idea is the same: the program must tell the
operating system when it doesn't need a variable's value anymore. In low
level languages, allocation is handled by the language, but deallocation
is not because the language cannot know when a programmer doesn't need a
value anymore: even if a variable that referenced the value fell out of
scope or was removed, another variable or a field in a script may still
reference it, and deallocating it would cause problems. In higher level
languages, deallocation may be handled by various automatic memory
management systems, such as garbage collection, which is the system used
by Lua. The garbage collector regularly searches through all the values
allocated by Lua for values that are not referenced anywhere. It will
collect values that the program cannot access anymore (because there is
no reference to them) and, since it knows that these values can safely
be deallocated, will deallocate them. This is all done transparently and
automatically, so the programmer does not generally need to do anything
about it. However, sometimes, the developer may want to give
instructions to the garbage collector.

#### Weak references

Weak references are references that are ignored by the garbage
collector. These references are indicated to the garbage collector by
the developer, using the `mode` metamethod. A table's `mode` metamethod
should be a string. If that string contains the letter "k", all the keys
of the table's fields are weak, and if it contains the letter "v", all
the values of the table's fields are weak. When an array of objects has
weak values, the garbage collector will collect these objects even if
they are referenced in that array, as long as they are only referenced
in that array and in other weak references. This behavior is sometimes
useful, but rarely used.

#### Manipulating the garbage collector

The garbage collector may be manipulated with the `collectgarbage`
function, which is part of the basic library and serves as an interface
to the garbage collector. Its first argument is a string that indicates
to the garbage collector what action should be performed; a second
argument is used by some actions. The `collectgarbage` function can be
used to stop the garbage collector, manually perform collection cycles
and count the memory used by Lua.

Coroutines
----------

Coroutines are components that can be created and manipulated with the
coroutine library in Lua and that allow the yielding and resuming of the
execution of a function at specific locations by calling functions that
yield the coroutine from inside of itself or that resume it from outside
of itself. Example:

1.  A function in the main thread creates a coroutine from a function
    with `coroutine.create` and resumes it with `coroutine.resume`, to
    which the number 3 is passed.
2.  The function in the coroutine executes and gets the number passed to
    `coroutine.resume` as an argument.
3.  The function arrives at a certain point in its execution where it
    calls `coroutine.yield` with, as an argument, the sum of the
    argument it received (3) and 2 (hence, 3+2=5).
4.  The call to `coroutine.resume` returns 5, because it was passed to
    `coroutine.yield`, and the main thread, now running again, stores
    that number in a variable. It resumes the coroutine again after
    having executed some code, passing to `coroutine.resume` the double
    of the value it has received from the call to `coroutine.resume`
    (i.e. it passes 5×2=10).
5.  The coroutine gets the value passed to `coroutine.resume` as the
    result of the call to `coroutine.yield` and terminates after running
    some more code. It returns the difference between the result of the
    call to `coroutine.yield` and the value it was given as a parameter
    initially (i.e. 10−3=7).
6.  The main thread gets the value returned by the coroutine as the
    result of the call to `coroutine.resume` and goes on.

This example, put in code, gives the following:

``` {.lua}
local co = coroutine.create(function(initial_value)
    local value_obtained = coroutine.yield(initial_value + 2) -- 3+2=5
    return value_obtained - initial_value -- 10-3=7
end)

local _, initial_result = coroutine.resume(co, 3) -- initial_result: 5
local _, final_result = coroutine.resume(co, initial_result * 2) -- 5*2=10
print(final_result) --> 7
```

The `coroutine.create` function creates a coroutine from a function;
coroutines are values of type "thread". `coroutine.resume` starts or
continues the execution of a coroutine. A coroutine is said to be dead
when it has encountered an error or has nothing left to run (in which
case it has terminated its execution). When a coroutine is dead, it
cannot be resumed. The `coroutine.resume` function will return `true` if
the execution of the coroutine was successful, along with all the values
returned, if the coroutine has terminated, or passed to
`coroutine.yield` if it has not. If the execution was not successful, it
will return `false` along with an error message. `coroutine.resume`
returns the running coroutine and `true` when that coroutine is the main
thread, or `false` otherwise.

The `coroutine.status` function returns the status of a coroutine as a
string:

-   "running" if the coroutine is running, which means it must be the
    coroutine which called `coroutine.status`
-   "suspended" if the coroutine is suspended in a call to yield or if
    it has not started running yet
-   "normal" if the coroutine is active but not running, which means it
    has resumed another coroutine
-   "dead" if the coroutine has finished running or has encountered an
    error

The `coroutine.wrap` function returns a function that resumes a
coroutine every time it is called. Extra arguments given to this
function will act as extra arguments to `coroutine.resume` and values
returned by the coroutine or passed to `coroutine.yield` will be
returned by a call to the function. The `coroutine.wrap` function,
unlike `coroutine.resume`, does not call the coroutine in protected mode
and propagates errors.

There are many use cases for coroutines, but describing them are outside
the scope of this book.

String matching
---------------

When manipulating strings, it is frequently useful to be able to search
strings for substrings that follow a certain pattern. Lua has a string
manipulation library that offers functions for doing this and a notation
for expressing patterns that the functions can search for in strings.
The notation offered by Lua is very similar to [regular
expressions](w:regular expression "wikilink"), a notation for expressing
patterns used by most languages and tools in the programming world.
However, it is more limited and has slightly different syntax.

The `find` function of the string library looks for the first match of a
pattern in a string. If it finds an occurrence of the pattern in the
string, it returns the indices in the string (integers representing the
position of characters in the string starting from the first character,
which is at position 1) where the occurrence starts and ends. If it
doesn't find an occurrence of the pattern, it returns nothing. The first
parameter it accepts is the string, the second being the pattern and the
third being an integer indicating the character position where the
`find` function should start searching. Finally, the `find` function can
be told to perform a simple match without patterns by being given the
value `true` as its fourth argument. It will then simply search for an
occurrence of the second string it is given in the first string. The
third argument must be given when a simple match is performed. This
example code searches for the word "lazy" in a sentence and prints the
start and end positions of the occurrence it finds of the word:

``` {.lua}
local start_position, end_position = string.find("The quick brown fox jumps over the lazy dog.", "lazy", 1, true)
print("The word \"lazy\" was found starting at position " .. start_position .. " and ending at position " .. end_position .. ".")
```

This code gives the result <samp>The word "lazy" was found starting at
position 36 and ending at position 39.</samp>. It is equivalent to the
following:

``` {.lua}
local sentence = "The quick brown fox jumps over the lazy dog."
local start_position, end_position = sentence:find("lazy", 1, true)
print("The word \"lazy\" was found starting at position " .. start_position .. " and ending at position " .. end_position .. ".")
```

This works because the `index` metamethod of strings is set to the table
containing the functions of the string library, making it possible to
replace `string.a(b, ...)` by `b:a(...)`.

Functions in the string library that accept indices to indicate
character position or that return such indices consider the first
character as being at position 1. They accept negative numbers and
interpret them as indexing backwards, from the end of the string, with
the last character being at position -1.

Patterns are strings that follow a certain notation to indicate a
pattern that a string may match or not. For this purpose, patterns
contain character classes, combinations that represent sets of
characters.

  ---------------------------------------------------------------------------
  Character combination   Description
                          
  ----------------------- ---------------------------------------------------
  .                       All characters
                          

  %a                      Letters (uppercase and lowercase)
                          

  %c                      Control characters
                          

  %d                      Digits
                          

  %g                      Printable characters (except the space character)
                          

  %l                      Lowercase letters
                          

  %p                      Punctuation characters
                          

  %s                      Space characters
                          

  %u                      Uppercase letters
                          

  %w                      Alphanumeric characters (digits and letters)
                          

  %x                      Hexadecimal digits
                          
  ---------------------------------------------------------------------------

All characters that are not special represent themselves and special
characters (all characters that are not alphanumeric) can be escaped by
being prefixed by a percentage sign. Character classes can be combined
to create bigger character classes by being put in a set. Sets are noted
as character classes noted between square brackets (i.e. `[%xp]` is the
set of all hexadecimal characters plus the letter "p"). Ranges of
characters can be noted by separating the end characters of the range,
in ascending order, with a hyphen (i.e. `[0-9%s]` represents all the
digits from 0 to 9 plus space characters). If the caret ("\^") character
is put at the start of the set (right after the opening square bracket),
the set will contain all characters except those it would have contained
if that caret had not been put at the start of the set.

The complement of all classes represented by a letter preceded of a
percentage sign can be noted as a percentage sign followed by the
corresponding uppercase letter (i.e. `%S` represents all characters
except space characters).

Patterns are sequences of pattern items that represent what sequences
should be found in the pattern for a string to match it. A pattern item
can be a character class, in which case it matches any of the characters
in the class once, a character class followed by the "\*" character, in
which case it matches 0 or more repetitions of characters in the class
(these repetition items will always match the longest possible
sequence), a character class followed by the addition ("+") character,
in which case it matches 1 or more repetitions of characters in the
class (these repetition items will also always match the longest
possible sequence), a character class followed by the minus ("-")
character, in which case it matches 0 or more repetitions of characters
in the class, but matches the shortest possible sequence or a character
class followed by an interrogation mark, in which case it matches one or
no occurrence of a character in the class.

It is possible to match substrings equivalent to previously captured
substrings: `%1` will match the first captured substring, `%2` the
second, and so on until `%9`. Captures are described below. There are
two other functionalities offered by patterns, as described by the
reference manual:

Patterns are sequences of pattern items, optionally preceded by a caret,
which indicates that the pattern can only match at the beginning of the
string, and optionally followed by a dollar sign, which indicates that
the pattern can only match at the end of the string. These symbols are
said to <dfn>anchor</dfn> the match at the beginning or the end of the
string. These two characters only have a special meaning when at the
beginning or at the end of a pattern.

Sub-patterns can be enclosed inside parentheses inside patterns to
indicate captures. When a match succeeds, the substrings of the string
that match captures are stored for future use, for example to be
returned by `gmatch`. They are always numbered starting from the
position of their left parenthesis. Two empty parentheses denote the
<dfn>empty capture</dfn>, which captures the current string position
(which is a number and is not a part of the string).

The `gmatch` function can be used to iterate through the occurrences of
a pattern in a string; it is not possible, unlike with the `find`
function, to specify an initial position to start searching or to
perform simple matching. The `gmatch` function returns an iterator that,
when called, returns the next captures from the given pattern in the
string. The whole match is given instead if there are no captures
specified in the pattern. The following example shows how to iterate
through the words in a sentence and print them one by one:

``` {.lua}
local sentence = "The quick brown fox jumps over the lazy dog."
for word in sentence:gmatch('%a+') do
    print(word)
end
```

In this example, the entire match is given by the only value returned by
the iterator, <var>word</var>.

The `gsub` function can be used to replace all occurrences of a pattern
in a string by something else. Its first two arguments are the string
and the pattern, while the third is the string to replace occurrences by
and the fourth is the maximum number of occurrences that should be
replaced. The third argument, instead of being a string, can also be a
table or a function.

When the third argument is a string, it is called the replacement string
and it replaces occurrences of the pattern in the string. Captures
stored by the pattern can be embedded in the replacement string; they
are noted by a percentage sign followed by a digit representing the
number of the capture. The match itself can be represented by `%0`.
Percentage signs in replacement strings must be escaped as `%%`.

When the third argument is a table, the first capture is used as a key
to index that table and the replacement string is the value
corresponding to that key in the table. When it is a function, that
function is called for every match, with all captures passed as
arguments. In both cases, if there is no capture, the entire match is
used instead. If the function or table gives the value `false` or `nil`,
no replacement is done.

Here are some examples taken directly from the Lua Reference Manual:

Lua offers other functions for manipulating strings than those for
pattern matching. These include the `reverse` function, which returns a
string with the order of the characters reversed, the `lower` function,
which returns the lowercase equivalent of a string, the `upper`
function, which returns the uppercase equivalent of a string, the `len`
function, which returns the length of a string and the `sub` function,
which returns the substring of a string that starts at and ends at the
two character positions given as arguments. There are more, and their
documentation can be found in the Lua Reference Manual.
