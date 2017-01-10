# Polywell

A highly-configurable text editor / coding tool written in Lua that
runs on the [LÖVE](https://love2d.org) game engine.

Originally written for [Bussard](https://gitlab.com/technomancy/bussard),
the spaceflight programming adventure.

![screenshot](https://p.hagelb.org/polywell.png)

## Rationale

To say that Polywell is an editor is a little bit misleading; it may
be better to think of it as an interface that allows in-game coding
either to tighten the feedback loop during development or allow the
player to take their customizations to the next level by writing their
own code that calls functions from a sandboxed game API. But its
mode-based keybinding system can also be used as controls for your game.

Polywell comes with example config which defines a special editor mode
that works as a Lua console. However, none of that is hard-coded in;
the console mode simply binds the `enter` key to a function which
calls `loadstring` on the provided input and prints the return
value. But you can build many kinds of text-centric interfaces using
Polywell buffers; for instance the game Bussard includes a
[mail reading interface](https://gitlab.com/technomancy/bussard/blob/master/data/src/mail)
as well as a simulated SSH client.

The ideal means of user empowerment is to give your users the same
tools inside the program that you used to create the program yourself;
this is the vision behind Polywell.

## Features

* Syntax highlighting for Lua and Clojure
* Multiple buffers
* Rebindable key commands
* Configurable by writing Lua code
* Live completion when opening files or changing buffers
* As-you-type search
* User-definable modes which can inherit
* Easy to sandbox/embed
* Comes with a console for running Lua code
* Undo
* Emacs key bindings (optional)

* TODO: smarter lua-aware indentation
* TODO: fuzzy-matching opening files and switching buffers

## Usage

There are basically three ways to use Polywell. You can use it as a
standalone Lua text editor by just running `love /path/to/polywell` in
your project directory. You can also embed it in your game in order to
allow you to write your game "from the inside out" as it were, and
experiment with changes to the code directly while running the game
and reloading. See the game
[Liquid Runner](https://gitlab.com/technomancy/liquid-runner) for an
example of a game written from inside Polywell. Finally, you can embed
it in your game inside a sandboxed evaluation context and integrate it
into your game in a way that allows the game's players to write their
own code that can control the game. See
[Bussard](https://gitlab.com/technomancy/bussard) for an example of a
game that does this. There is also a `sandbox_example.lua` file which
shows a simpler example of how to do this.

If you have a `~/.polywell/init.lua` file, then standalone Polywell
will load that first, but if you don't, it will fall back to the
defaults in the `config` directory. There are separate files for a lua
console, lua-specific code colorization, and Emacs key bindings.

Using Polywell for in-game coding can be a bit tricky because typical Lua
idioms often make reloading difficult. You have to be very intentional
about keeping all your state in a single table and have some mechanism
to replace all the functions without resetting the state. The
`lume.hotswap` function may be useful here.

See the [reference manual](manual.md) for more detailed coverage of
Polywell's API.

## Dependencies

* [lume](https://github.com/rxi/lume/)
* [utf8.lua](https://github.com/Stepets/utf8.lua)
* [luafilesystem](https://keplerproject.github.io/luafilesystem/) (optional)
* [serpent](https://github.com/pkulchenko/serpent) (recommended)

If you don't have luafilesystem then the editor will work, but the
completion when you try to open new files will be inoperable.

If you have serpent installed, you will get much nicer table output in your
console, but otherwise it will fall back to lume's serialization.

The required dependencies are already included in the Polywell source.

TODO: package properly with luarocks

## License

Copyright © 2015-2016 Phil Hagelberg

Released under the terms of the GNU Lesser General Public License
version 3 or later; see the file LICENSE.
