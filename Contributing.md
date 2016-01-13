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

Some code (primarily the lisp compiler and console) were imported from
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
