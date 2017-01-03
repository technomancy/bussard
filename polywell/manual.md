# Polywell Manual

Polywell is at its core a library for creating programmable textual
and graphical interfaces where keystrokes are bound to Lua functions
and operate on buffers.

It also ships with some code which uses this functionality to
construct a text editor and Lua console, (in the `config` dir) but you
can just as well replace those bits with your own code or define a new
mode which defines a
[simulated SSH client](https://gitlab.com/technomancy/bussard/blob/master/data/src/ssh) or
[platformer level editor](https://gitlab.com/technomancy/liquid-runner/blob/master/level_edit.lua).

## Usage

Polywell has a bunch of handlers which need to be wired into the
`love` table; usually in your `love.load` function:

```lua
love.keyreleased = polywell.handle_key_up
love.keypressed = polywell.handle_key
love.textinput = polywell.handle_textinput
-- if you want polywell to handle mouse events too:
love.wheelmoved = polywell.handle_wheel
love.mousepressed = polywell.handle_mouse_pressed
love.mousereleased = polywell.handle_mouse_released
love.mousemoved = polywell.handle_mouse_moved
love.mousefocus = polywell.handle_mouse_focus
```

But of course you can set the `love` handlers to your own functions
which can intercept events before they are sent to the Polywell
handlers too:

```lua
love.keypressed = function(key)
   if(key == "escape") then
      do_escape()
   else
      polywell.handle_key(key)
   end
end
```

You also will need to either set `love.draw` to `polywell.draw` or
call `polywell.draw` from within your own drawing function. You can
optionally pass `polywell.draw` a function which will be called if no
buffer is currently active.

Your `love.load` function will probably also want to call
`love.graphics.setFont` and `love.keyboard.setKeyRepeat`, and you will
also want to load some config to create modes and key bindings. It's
recommended to first attempt to load user-level config, and then fall
back to the stock config if that fails:

```lua
local init_file = os.getenv("HOME") .. "/.polywell/init.lua"
local ok, err = pcall(dofile, init_file)
if(not ok) then
   local chunk = assert(love.filesystem.load("polywell/config/init.lua"))
   chunk("mygame")
end
```

Passing in the name of your game to `chunk` will allow your user to
behave differently for different games.

## Buffers and Modes

Every file you open in Polywell has a buffer associated with it, and
you can also have buffers like `*console*` which aren't backed by a
file at all. You can use the `polywell.open` function to open a new
buffer; it takes an `fs` argument which is used to look up
buffers. This makes it easy to use Polywell in-game with a simulated
filesystem, but if you're not doing that, the `polywell.fs` module
will give you a table which works for the real filesystem. (TODO: this
might deserve rethinking; Polywell was extracted from a game which
assumes a virtual in-game filesystem, but this is unlikely to be used
much outside that context.) The `polywell.open` function also takes
the path as the second argument. If you are making a buffer like
`*console*` which doesn't correspond to a file, pass nil as the first
argument, and use a path which has `*` around the name.

Every buffer has a mode associated with it which determines how
keystrokes will be interpreted. Modes are defined with the
`polywell.define_mode` function, which takes a name, an optional
parent name (textual modes will want to inherit from the "edit" mode
to get support for common text commands, while console-like
interaction modes should inherit from "console"), and an optional
properties table. (Properties are explained in detail below.)

```lua
polywell.define_mode("read_lisp", "lisp", {read_only=true})
```

Keystrokes are bound to commands using `polywell.bind`, which takes a
mode name, a keystroke descriptor, and a function:

```lua
polywell.bind("edit", "ctrl-pagedown", editor.next_buffer)
```

Often an anonymous function is used. If a key binding is not defined
in a mode, it is looked up in the parent mode map, and so on in the
parent's parent.

### Properties

Each mode has a table of properties that can contain functions or
other values which override its behaviour. Buffers can also have
properties set which take precedence over mode properties.

Currently these properties are used:

* on_change: a function which is called every time a change is made.
* read_only: a boolean indicating whether edits should be allowed; this
  can be sidestepped by the `polywell.suppress_read_only` function,
  which takes a function as an arg to run with read_only checks disabled.
* wrap: every time a command is run, it is run inside the wrap
  function which handles bookkeeping for undo; providing your own wrap
  allows you to implement your own undo.
* render_lines: a table of lines in LÖVE's print format (color,
  string, color, string, etc) which is used instead of the raw lines
  table during rendering if present; used by syntax highlighting.
* activate: (mode property only) a function called when a mode activates.
* deactivate: (mode property only) a function called when a mode deactivates.
* draw: a function to render the buffer instead of the default textual rendering.

You can get and set properties on the current buffer with
`polywell.get_prop` and `polywell.set_prop`.

## Reading Input

The `polywell.read_line` function allows you to prompt the user for a
line of input. It takes a prompt and a callback function. The callback
function takes the string value which was input as its first argument
and a boolean indicating whether the user canceled the input as its
second.

```lua
polywell.bind("edit", "alt-g", function()
        polywell.read_line("Go to line: ", function(l, cancel)
                                   if(not cancel) then
                                      polywell.go_to(tonumber(l))
                                   end
        end)
end)
```

When you read input, you can also offer live feedback as you type,
like when you invoke the buffer switcher with `ctrl-alt-b`. The `read_line`
function takes as a third argument a properties table, and if you provide
a "completer" function, it will be called with the input string whenever the
input changes. It must return a table of possible completion matches.
You can use the `polywell.utils.completions_for` function to calculate
completion candidates; it takes an input string, a table of possible
candidates, and a separator string for when completion needs to descend
into multiple levels of nested tables.

See `editor.find_file` in `config/edit_functions.lua` for an example of
how this can be used.

## Syntax Highlighting

Currently Polywell ships with syntax highlighting for both Lua code
and Clojure code. You can add support for new languages, but new
language syntax highlighting support is limited to defining a new set
of keywords and to-EOL comment markers for the language; the
colorization of strings and numbers cannot be changed, nor can new
constructs be added. See `config/lua_mode.lua` for an example.

## Useful Functions

Most text editing commands should be fairly self-explanatory; see
`config/edit.lua` and `config/emacs_keys.lua` for a listing.

The `polywell.print` and `polywell.write` functions print to the
`*console*` buffer unless it is run within a call to `with_output_to`,
which accepts an alternate buffer to which to redirect output and a
function to run with the redirection active. For messages that do not
need to stick around, use `polywell.echo` which just shows at the
bottom of the screen until the next command.

The `polywell.point` function returns the point and line number. The
line number obviously tells you which line you're on, and the point
tells you how far over the cursor is in that line. You can use
`polywell.get_line` to get a specific line as a string.

Use `polywell.activate_mode` to change the mode for the current
buffer. The `polywell.change_buffer` function will switch to an
existing buffer if it exists and do nothing if not, while
`polywell.open` will switch or create if it's not found. The
`polywell.with_current_buffer` function takes a buffer name and and
function to run with that buffer active; it switches back to the
original buffer when that function returns.
