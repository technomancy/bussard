# Contributing

Discussion happens mostly in the `#bussard` channel on Freenode, but
the [issue tracker](https://gitlab.com/technomancy/bussard/issues) is
useful for that too.

Contributions are preferred as GitLab merge requests on feature branches.
Emailing patches is OK too.

Our goal is compatibility with LÖVE 0.9.0+, but 0.10.x is pretty close to 0.9.x,
so it should be fine to test primarily in that. A few non-essential features
that only work in 0.10.x are OK but should be noted with comments.

During development it may be expedient to run `ship.cheat.comm_range = 9999999`
in order to make testing login interaction easier.

For story, background and guidelines, look at the dev guide in `spoilers/`.

In order to skip around in the game for debugging, you can run `love . --act 1`
to set all the event flags and deliver all the messages that you would normally
get by a real play-through of the game up to that point.

Any changes made to the stock config in `data/src` will not be visible to games
begun before the changes were made. Use `ctrl-f1` to update your in-game config
with the latest stock. Your old config files will be backed up.

You may find the contents of `spoilers/solutions` useful during development.

## Self-hosting

The text editor inside Bussard is very capable and should be comfortable to use
for editing Lua by all but the most die-hard Emacs/Vim fans. It can be used not
only for editing in-game code, but also for the source to the game itself if you
are running from a checkout. When you press `ctrl-o` to open a file, put a `/`
in front of the path of any files inside the checkout of the game to open it in
the editor; for instance, `/Contributing.md` would open this file.

If you want to use Bussard solely as a text editor, launch it with the `--no-game`
argument and the update loop will be adjusted to use much less CPU by leaving
out all the game calculations.

## Code style

* Three-space indent. No tabs.
* Use `snake_case` for all the things.
* Don't leave out parentheses just because they're technically optional.
* Unused arguments should be named `_`.
* Try to keep it under 80 columns unless breaking would be awkward (usually strings for output).
* Lume is great; learn it inside out and use it.
* Pick names for locals that avoid shadowing other locals.
* The `local f = function() ... end` construct is preferred to `local function f() ... end` unless the latter is needed for recursion.

The last rule exists to make recursive functions more obvious. If you see `local
function f()`, you know to pay more attention because a recursive function is
coming.

There are three different code contexts (at the time of this writing; more may
be introduced in the future): engine code, in-ship code, and OS code. The
standards for the engine code are the strictest; no new globals may be
introduced. Inside the ship or OS sandboxes, it is often OK to introduce new
"global" functions because their scope is much more limited.

New to Lua? This
[style guide](http://kiki.to/blog/2014/03/30/a-guide-to-authoring-lua-modules/)
has some great advice. The main difference is that in our code rather than
creating the table at the top of the lib and adding to it as you go, we
construct a table at the very end that contains all the values we need to
expose, because it is more declarative and less imperative.

## Tests and Static Analysis

While a test suite technically exists, it is far from comprehensive. If you're
working on a feature that would be easy to write tests for, great! We use
[lunatest](https://github.com/silentbicycle/lunatest), which is a pretty typical
xUnit style tool. However, most work is not like that and relies on manual
playtesting. This is OK. Please do *run* the existing tests before sending a
patch though!

You should also run [luacheck](https://github.com/mpeterv/luacheck) against your
changes to check for simple mistakes that can be caught with static analysis,
like typos or accidental globals. It also catches certain style issues.

You'll need [luarocks](https://luarocks.org), which hopefully is provided by
your package manager.

    $ luarocks install --local luacheck && luarocks install --local lunatest
    [...]
    $ make check test

If you make changes to the editor, try fuzzing it a few times to ensure none of
the commands can crash if given random input:

    $ make fuzz

Note that this will only catch problems in commands which are bound to keys in
the default config.

## Philosophy

The whole game is about exploring a simulated world, pushing up against its
boundaries, and breaking through those boundaries. Allowing the user to explore
without fear of screwing something up irreparably is of paramount importance.

In particular; there should be no failure states for the game before the
spacetime junction is acquired. After the player has the junction, they can
recover from failure states by activating it and jumping back to an earlier
point in time. However, as much as possible, (prior to the finale sequence)
progress that happens after the junction is acquired should be measured in terms
of information acquired (which persists across junction activation), not about
events which occur outside the ship and get undone by the junction.

In-game documentation written as fictional technical manuals contributes to the
hard-science hacker realism.

As much of the game as possible should be implemented in userspace so it's
modifiable by any intrepid player, except for places where that would result in
cheating. The other exception is that sometimes we avoid exposing things to
userspace (like the table structure behind editor buffers) because it would make
it impossible to know if it's safe to make changes to the table structure
without breaking user code.

When in doubt, do what Emacs does.

## Code Organization

The [overview diagram](https://p.hagelb.org/bussard-overview.jpg) shows how the
different parts of the codebase are related to each other.

The `ship` table (loaded from `ship/init.lua`) contains all game state. In
particular, `ship.bodies` is the table for all the worlds, asteroids, and ships
in the current system, and `ship.systems` contains all systems. (I guess it
doesn't make all that much sense, but it's very convenient.) The `ship.systems`
table is loaded from the `data/systems.lua` file.

The `ship` table furthermore has a `.api` field on it which is the part of the
ship which is exposed to the in-game sandboxed user code. Certain fields of
`ship` are exposed through the `status` metatable as read-only fields (like
position or velocity) in order to disallow cheating. The `ship.api.actions`
table contains functions likely to be bound to in-game keys. Upgrades can
introduce new functions here.

The `ship.sandbox` table contains all the functions that are exposed as
top-level values in the in-game Lua environment. Generic Lua functionality comes
from `utils.sandbox`, while ship-specific things are added in the `sandbox`
function in `ship/init.lua`; for instance, `print` is defined to be
`ship.api.editor.print`, which places its output in the console.

The player's progress through the game is tracked in the `ship.events` table,
which keys strings to numbers which indicate when a specific event
occurred. Events mostly affect mail and missions.

Note that from an in-game perspective, the `ship.api` table is referred to as
just `ship`.

### Data

The `data/` directory contains mostly non-code stuff like mail, missions, and
definitions of star systems, but `data/src` also contains all the code that is
loaded into the in-game computer by default and can be edited by the player.

Theoretically, replacing the `assets`, `data`, and `doc` directories would give
you a different game using the Bussard engine. Try to keep anything specific to
the worlds and story of Bussard in these directories, and everything elsewhere
should be agnostic, dealing only with the engine.

#### Missions

Missions are found in the `data/missions` directory. You accept them by replying
to mail. The fields that a mission may have are all documented at the top of
`mission.lua`. See `data/missions/passenger2` for an example of a complex
mission. Usually completing a mission sets events, which allows for other
missions to come available through new mail.

The `ship.active_missions` table stores a record about each mission that is
currently ongoing, containing at least its start time, required destinations,
and messages to show upon reaching said destinations. This table is persisted,
and missions can write arbitrary data to it in order to track mission progress
in `mission.update` and other functions.

#### Mail

Mail is stored in-game in `ship.api.docs.mail`. There are basically 3 things
that trigger mail delivery: timed events based on `data/msgs/timed.lua`,
replying to a message, or something happening within a mission. Messages are in
`data/msgs`.

Certain messages are replyable. Pressing `alt-enter` when viewing them will
either accept a mission (in `data/missions/` and named after the message-id of
the original message), or cause an event to be triggered
(`data/msgs/event.lua`), or cause another message to be sent to you, if there is
a file in `data/msgs` whose filename is the message-id of the original message
you reply to in order to get it.

Files in `data/missions` and `data/msgs` that are named after message-id headers
should also have symlinks to them for human-readable names.

Mail should stick to the typical header/body pattern; roughly RFC 822.

### OS and SSH

The worlds you SSH into mostly run the Orb unix-like operating system, which is
found in `os/orb`. All the scripts that run inside the OS are found in
`resources` in that directory. The portals run the `os/lisp` operating system,
and the `os/forth` OS will be used for the domain injector in the finale.

The code that runs inside SSH connections is sandboxed similarly to code that
runs on the ship's computer, but a different set of functions is exposed; see
`sandbox` in `ship/ssh.lua`. Each world you log into offers services you can
access using shell commands (cargo, upgrades, refueling, etc.) when you log in;
the underlying (out-of-userspace, and therefore cheat-proof) functionality for
these is implemented in `services.lua` and placed in the sandbox table so
scripts in `/bin` can call them.

The UI side of the SSH client is defined in `data/src/ssh`; it is based on the
ship's computer's console mode, but it calls `ssh_send_line` from the ship's own
sandbox rather than eval when you press enter.

See the comments at the top of `ship/ssh.lua` for details about how I/O works.

### Editor

The onboard computer uses the `ship/editor.lua` interface for everything, not
just editing code. Lua console sessions are run in an editor buffer, as are SSH
sessions which connect you to worlds and the messaging system. The user is free
to define their own modes as well. Key presses are translated by the editor into
text insertions or commands based on the keymap for the current mode; a system
which is largely based on Emacs. See `find_binding`, `define_mode`, and `bind`
in `ship/init.lua`, and `data/src/config` for a usage example. The commands
which the non-flight modes bind are typically defined in `ship/editor.lua`.

### Save

Certain fields of `ship` are saved off when you quit, but there is a whitelist;
not everything is saved between exits. (See `ship_fields` and `body_fields` in
`save.lua`.) Within user data of `ship.api`, the fields that get persisted are
listed in `ship.api.persist`, allowing the player to declare additional fields
as persistent.

Ship fields go in `ship_data.lua` in Love's save directory, while status of the
current system goes in `system_data.lua`. The OS filesystems and editor buffers
are also saved off into their own files. Nothing is saved from the status of
systems other than the current one except the filesystems. Filesystems are
created on-demand, which means that bodies which haven't been logged into yet
aren't saved.

## License

By making contributions, you agree to allow your material to be
distributed under the terms of the GNU General Public License, version
3 or later; see the file LICENSE for details.
