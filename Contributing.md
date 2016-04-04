# Contributing

Discussion happens mostly in the `#bussard` channel on Freenode, but
the  [issue tracker](https://gitlab.com/technomancy/bussard/issues) is
useful for that too.

Contributions are preferred as GitLab merge requests on feature branches.

Our goal is compatibility with LÖVE 0.9.0+, but 0.10.x is pretty close
to 0.9.x, so it should be fine to test primarily in that.

For story and plot background and guidelines, look in `spoilers/`.

## Code style

Three-space indent; don't leave out parentheses just because they're
technically optional. `local f = function() ... end` preferred to
`local function f() ... end` unless the latter is needed for
recursion. Try to keep it under 80 columns unless it would be awkward
(usually strings for output). Lume is great; learn it inside out and
use it.

Some code (primarily the lisp and forth compilers) were imported from
elsewhere and don't follow these rules all that well.

## Philosophy

The whole game is about exploring a simulated world, pushing up
against its boundaries, and breaking through those
boundaries. Allowing the user to explore without fear of screwing
something up irreparably is of paramount importance.

In-game documentation written as fictional technical manuals
contributes to the hard-science hacker realism.

As much of the game as possible should be implemented in userspace so
it's modifiable by any intrepid player, except for places where that
would result in cheating. However, in some places there are in-game
explanations for behavior that would usually be classified as
cheating; this is to be embraced.

When in doubt, do what Emacs does.

## Code Organization

The `ship` table (loaded from `ship/init.lua`) contains all game
state. In particular, `ship.bodies` is the table for all the worlds,
asteroids, and ships in the current system, and `ship.systems`
contains all systems. (I guess it doesn't make all that much sense,
but it's very convenient.) The `ship` table furthermore has a `.api`
field on it which is the part of the ship which is exposed to the
in-game sandboxed user code. Certain fields of `ship` are exposed
through the `status` metatable as read-only fields (like position or
velocity) in order to disallow cheating. The `ship.api.actions` table
contains functions likely to be bound to in-game keys. Upgrades can
introduce new functions here. The contents of the `src` and `docs`
tables in `.api` are saved.

The `data/` directory contains mostly non-code stuff like mail,
missions, and definitions of star systems, but `data/src` also
contains all the code that is loaded into the in-game computer by
default and can be edited by the player.

The worlds you SSH into mostly run the Orb operating system, which is
found in `os/orb`. All the scripts that run inside the OS are found in
`resources` in that directory. The portals run the `os/lisp` operating
system, and the `os/forth` OS will be used for the domain injector.

The onboard computer uses the `ship/editor.lua` interface for
everything, not just editing code. Lua console sessions are run in an
editor buffer, as are SSH sessions which connect you to worlds and the
messaging system. The user is free to define their own modes as
well. Key presses are translated by the editor into text insertions or
commands based on the keymap for the current mode; a system which is
largely based on Emacs. See `find_binding`, `define_mode`, and `bind`
in `ship/init.lua`, and `data/src/config` for a usage example. The
commands which the non-flight modes bind are typically defined in
`ship/editor.lua`.
