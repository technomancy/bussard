Lua Programming: Introduction
=============================

Lua was designed and is being maintained at the Pontifical Catholic University
of Rio de Janeiro, which is located in Brazil. Its creators are Roberto
Ierusalimschy, Waldemar Celes and Luiz Henrique de Figueiredo.

Lua comes from two languages that were designed by TeCGraf (a laboratory at the
Pontifical Catholic University of Rio de Janeiro): DEL and Sol.  DEL means "data
entry language", while Sol means "simple object language" and also means sun in
Portuguese, which is why the name Lua was chosen, since it means "moon" in
Portuguese. It was created for Petrobras, a Brazilian oil company, but was also
used in many other projects in TeCGraf, and is now used in a multitude of
projects world-wide. Lua is one of the leading languages in the field of
embedded game development.

One of the main advantages of Lua is its simplicity. Some companies use it
exclusively because of that advantage: they think their employees would be able
to work better if they could use a programming language to perform certain
tasks, but they cannot afford to give to their employees a full course on a
complicated programming language. Some very simple languages like Bash or Batch
here would not be powerful enough to perform these tasks, but Lua is both
powerful and simple. Another of the important advantages of Lua is its
capability to be embedded, which was one of the most important characteristics
of it throughout all of its development. Games like or World of Warcraft or
ROBLOX need to be able to embed Lua in their application so users of the
application can use it.

Programming, which is also sometimes called scripting in the case of programs
that run inside embedded applications, is the process of writing computer
programs. A programming language is a language used to give instructions to a
computer through computer code that is contained in a computer program. A
programming language consists of two things: a syntax, which is like grammar in
English, and libraries, basic functions provided with the language. These
libraries could be compared with vocabulary in English.

Hello, world!
-------------

Lua can either be used embedded in an application or by itself. This book will
not describe the process to install Lua on your computer, but you can execute
code using whatever console you have available. The first example of Lua code in
this book will be the basic and traditional hello world program.

    print("Hello, world!")

The code above prints the text "Hello, world!" to the output, printing referring
to displaying text in the output, not to printing something on paper. It does so
by calling the `print` function with the string "Hello, world!" as an
argument. This will be explained in the chapter about functions.

Comments
--------

A comment is a code annotation that is ignored by the programming
language. Comments can be used to describe one or many lines of code, to
document a program, to temporarily disable code, or for any other reason. They
need to be prefixed by two hyphens to be recognized by Lua and they can be put
either on their own line or at the end of another line:

    print("This is normal code.")
    -- This is a comment
    print("This is still normal code.") -- This is a comment at the end of a line of code.

Syntax
------

The syntax of a programming language defines how statements and expressions must
be written in that programming language, just like grammar defines how sentences
and words must be written. Statements and expressions can be respectively
compared to sentences and words.  Expressions are pieces of code that have a
value and that can be evaluated, while statements are pieces of code that can be
executed and that contain an instruction and one or many expressions to use that
instruction with. For example, `3 + 5` is an expression and `variable = 3 + 5`
is a statement that sets the value of variable to that expression.

The entire syntax of Lua can be found in extended Backus–Naur form on the Lua
website, but you wouldn't understand anything if you read it. Extended
Backus–Naur Form is a metalanguage, a language used to describe another
language, just like a metawebsite is a website about a website, and just like
metatables, in Lua, are tables that define the behavior of other tables (you'll
learn about metatables and tables later in this book). But you're not going to
have to learn extended Backus–Naur form in this book, because, while a language
like Lua can be described using a metalanguage, it can also be described using
words and sentences, in English, and this is exactly what this book is going to
do.

Since English can be used to describe another language, then it must itself be a
metalanguage (because it corresponds to the definition of a metalanguage). This
is indeed the case. And since the purpose of a programming language is to
describe instructions, and you can do that with English, English must also be a
programming language. This, *in a way*, is also the case. In fact, English is a
language that can be used for many things. But extended Backus–Naur form is a
specialized language, and programming languages are also specialized
languages. Specialization is the characteristic of being very good at doing
something in particular, but not being capable of doing other things. Extended
Backus–Naur form is very good at describing other languages, but it cannot be
used to write instructions or to communicate a message. Programming languages
are very good at giving instructions, but they cannot be used to describe
languages or to communicate messages.

English is capable of doing everything: describing languages, giving
instructions and communicating messages. But it is not very good at doing some
of these. In fact, it is so bad at giving instructions that, if it is used to
give instructions to a computer, the computer won't understand anything. That's
because computers need the instructions to be very precise and unambiguous.

Proceed to the next chapter with man("luabook2")
