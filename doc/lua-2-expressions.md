Lua Programming: Expressions
============================

As explained before, expressions are pieces of code that have a value and that
can be evaluated. They cannot be executed directly (with the exception of
function calls), and thus, a script that would contain only the following code,
which consists of an expression, would be erroneous:

    3 + 5
    -- The code above is erroneous because all it contains is an expression.
    -- The computer cannot execute '3 + 5', since that does not make sense.

Normally code in files must be comprised of a sequence of statements. These
statements can contain expressions which will be values the statement has to
manipulate or use to execute the instruction. Note that you can enter bare
expressions into the console and they will be printed without trouble.

Types
-----

To evaluate an expression is to compute it to find its value. The value a given
expression evaluates to might be different from one context to another, since it
can depend on the environment and stack level. This value will sometimes be a
number, sometimes text and the other times any of many other data types, which
is why it is said to have a type.

In Lua, and in programming in general, expressions will usually consist of one
or more values with zero or more operators. Some operators can only be used with
some types (it would be illogical, for example, to try to divide text, while it
makes sense to divide numbers). There are two kinds of operators: unary
operators and binary operators. Unary operators are operators that only take one
value. For example, the unary - operator only takes one number as a parameter:
-5, -3, -6, etc. It takes one number as a parameter and negates that number. The
binary - operator, however, which is not the same operator, takes two values and
subtracts the second from the first: 5 - 3, 8 - 6, 4 - 9, etc.

It is possible to obtain a number's type as a string with the `type` function:

    print(type(32425)) --> number

### Numbers

Numbers generally represent quantities, but they can be used for many other
things. The number type in Lua works mostly in the same way as real
numbers. Numbers can be constructed as integers, decimal numbers, decimal
exponents or even in hexadecimal. Here are some valid numbers:

-   3
-   3.0
-   3.1416
-   314.16e-2
-   0.31416E1
-   0xff
-   0x56

#### Arithmetic operations

The operators for numbers in Lua are the following:

  ------------------------------------------------------------------------------
  Operation             Syntax   Description (Example)
  ------------------------------------------------------------------------------
  Arithmetic negation   -a       Changes the sign of a and returns the value
                                 (-3.14159)

  Addition              a + b    Returns the sum of a and b
                                 (5.2 + 3.6)

  Subtraction           a - b    Subtracts b from a and returns the result
                                 (6.7 - 1.2)

  Multiplication        a \* b   Returns the product of a and b
                                 (3.2 \* 1.5)

  Exponentiation        a \^ b   Returns a to the power b, or the exponentiation
                                 of a by b (5 \^ 2)

  Division              a / b    Divides a by b and returns the result (6.4 / 2)


  Modulo operation      a % b    Returns the remainder of the division of a by b
                                 (5 % 3)

  ------------------------------------------------------------------------------

You probably already know all of these operators (they are the same as basic
mathematical operators) except the last. The last is called the modulo operator,
and simply calculates the remainder of the division of one number by another. 5
% 3, for example, would give 2 as a result because 2 is the remainder of the
division of 5 by 3. The modulo operator is less common than the other operators,
but it has multiple uses.

### Nil

Nil is the type of the value nil, whose main property is to be different from
any other value; it usually represents the absence of a useful value. A function
that would return nil, for example, is a function that has nothing useful to
return (we'll talk later about functions).

### Booleans

A boolean value can be either true or false, but nothing else. This is literally
written in Lua as `true` or `false`, which are reserved keywords. The following
operators are often used with boolean values, but can also be used with values
of any data type:

  ------------------------------------------------------------------------------
  Operation             Syntax                          Description

  --------------------- ------------------------------- ------------------------
  Boolean negation      not a     If a is false or nil, returns true. Otherwise,
                                  returns false.

  Logical conjunction   a and b   Returns the first argument if it is false or
                                  nil. Otherwise, returns the second argument.

  Logical disjunction   a or b    Returns the first argument if it is neither
                                  false nor nil. Otherwise, returns the second
                                  argument.

  ------------------------------------------------------------------------------

Essentially, the `not` operator just negates the boolean value (makes it false
if it is true and makes it true if it is false), the `and` operator returns true
if both are true and false if not and the `or` operator returns true if either
of arguments is true and false otherwise. This is however not exactly how they
work, as the exact way they work is explained in the table above. In Lua, the
values false and nil are both considered as false, while everything else is
considered as true, and if you do the logic reasoning, you'll realize that the
definitions presented in this paragraph correspond with those in the table,
although those in the table will not always return a boolean value.

The relational operators introduced in the next chapter (`<`, `>`, `<=`, `>=`,
`~=`, `==`) do not necessarily take boolean values as operands, but will always
give a boolean value as a result.

### Strings

Strings are sequences of characters that can be used to represent text.  They
can be written in Lua by being contained in double quotes, single quotes or long
brackets. Strings that aren't contained in long brackets will only continue for
one line. Because of this, the only way to make a string that contains many
lines without using long brackets is to use escape sequences. This is also the
only way to insert single or double quotes in certain cases. Escape sequences
consist of two things: an escape character, which will always be a backslash
('\\') in Lua, and an identifier that identifies the character to be escaped.

  ------------------------------------------------------------------------------
  Escape sequence   Description

  ------------------------------------------------------------------------------
  \\n               A new line

  \\"               A double quote

  \\'               A single quote (or apostrophe)

  \\\\              A backslash

  \\t               A horizontal tab

  \\\#\#\#          1.  1.  must be a number from 0 to 255. The result will be 
                    the corresponding ASCII character.

  ------------------------------------------------------------------------------

  : Escape sequences in Lua

Escape sequences are used when putting the character directly in the string
would cause a problem. For example, if you have a string of text that is
enclosed in double quotes and must contain double quotes, then you need to
enclose the string in different characters or to escape the double
quotes. Escaping characters in strings delimited by long brackets is not
necessary, and this is true for all characters. All characters in a string
delimited with long brackets will be taken as-is. The `%` character is used in
string patterns to escape magic characters, but the term *escaping* is then used
in another context.

    "This is a valid string."

    'This is also a valid string.'

    "This is a valid \" string 'that contains unescaped single quotes and escaped double quotes."

    "This is a valid string that contains tabs \t, double quotes \" and backlashes \\"

    "This is " not a valid string because there is an unescaped double quote in the middle of it."

It is possible to get the length of a string, as a number, by using the
unary length operator ('\#'):

    print(#("This is a string")) --> 16

#### Concatenation

The string concatenation operator in Lua is denoted by two dots ('..').
Here is an example of concatenation that concatenates "snow" and "ball"
and prints the result:

    print("snow" .. "ball") --> snowball

This code will concatenate "snow" and "ball" and will print the result.

### Other types

The four basic types in Lua (numbers, booleans, nil and strings) have been
described in the previous sections, but four types are missing: functions,
tables, userdata and threads. Functions are pieces of code that can be called,
receive values and return values back.  Tables are data structures that can be
used for data manipulation. Userdata are used internally by applications Lua is
embedded in to allow Lua to communicate with that program through objects
controlled by the application. Finally, threads are used by coroutines, which
allow many functions to run at the same time.  These will all be described
later, so you only need to keep in mind that there are other data types.

Literals
--------

Literals are notations for representing fixed values in source code. All values
can be represented as literals in Lua except threads and userdata. String
literals (literals that evaluate to strings), for example, consist of the text
that the string must represent enclosed into single quotes, double quotes or
long brackets. Number literals, on the other hand, consist the number they
represent expressed using decimal notation (ex: `12.43`), scientific notation
(ex: `3.1416e-2` and `0.31416E1`) or hexadecimal notation (ex: `0xff`).

Coercion
--------

Coercion is the conversion of a value of one data type to a value of another
data type. Lua provides automatic coercion between string and number values. Any
arithmetic operation applied to a string will attempt to convert this string to
a number. Conversely, whenever a string is expected and a number is used
instead, the number will be converted to a string. This applies both to Lua
operators and to default functions (functions that are provided with the
language).

    print("122" + 1) --> 123
    print("The number is " .. 5 .. ".") --> The number is 5.

Coercion of numbers to strings and strings to numbers can also be done manually
with the `tostring` and `tonumber` functions. The former accepts a number as an
argument and converts it to a string, while the second accepts a string as an
argument and converts it to a number (a different base than the default decimal
one can optionally be given in the second argument).

Operator precedence
-------------------

Operator precedence works the same way in Lua as it typically does in
mathematics. Certain operators will be evaluated before others, and
parentheses can be used to arbitrarily change the order in which
operations should be executed. The priority in which operators are
evaluated is in the list below, from higher to lower priority. Some of
these operators were not discussed yet, but they will all be covered at
some point in this book.

1.  Exponentiation: `^`
2.  Unary operators: `not`, `#`, `-`, `~`
3.  Level 2 mathematical operators: `*`, `/`, `//`, `%`
4.  Level 1 mathematical operators: `+`, `-`
5.  Concatenation: `..`
6.  Bit shifts: `&lt;&lt;`, `&gt;&gt;`
7.  Bitwise AND: `&`
8.  Bitwise XOR: `~`
9.  Bitwise OR: `|`
10. Relational operators: `&lt;`, `&gt;`, `&lt;=`, `&gt;=`, `~=`, `==`
11. Boolean and: `and`
12. Boolean or: `or`

Proceed to the next chapter with man("luabook3")
