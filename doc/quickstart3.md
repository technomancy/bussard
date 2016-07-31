# Quickstart, continued (code)

As you have seen, pressing ctrl-enter opens the ship's editor, which
starts with the console that allows you to interact with the onboard
computer. You do this by entering code in the Lua programming language.
You can type anything directly into the console, but for longer functions
or functions you will be running often, it's better to put them in a file.
The editor helps with this.

Press ctrl-o to open a file in the editor. Your ship starts by loading
the code in the "src.config" file, so that's a good place to look for
examples of what you can do. If you have a new function you want to
run every time you press a key (say, ctrl-f), you can put something
like this at the bottom of that file:

    bind("flight", "ctrl-f", function() print("hello, world") end)

You will need to reload that code by pressing ctrl-r before that key
binding is active.

In order to copy and paste, press ctrl-space to activate the mark, and
move the editing point. Everything between your current point and the
mark (shown by an empty box) will be copied when you press ctrl-c or
cut when you press ctrl-x. If you uncomment the dofile("src.emacs") at
the end of "src.edit" then most Emacs key bindings will be available.

In the editor, you can press ctrl-pageup and ctrl-pagedown to cycle
through open files. Take a look at the file src.edit to see other
commands that are available and which keys they are bound to.

That's all for the quick start, but there's much more you can do. Once
you get comfortable with your ship, you'll want to read the full
reference manual to better understand what your ship is capable of:

    man("manual")
