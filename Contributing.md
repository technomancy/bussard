# Contributing

Discussion happens mostly in the `#bussard` channel on Freenode, but
the [issue tracker](https://gitlab.com/technomancy/bussard/issues) is
useful for that too.

Contributions are preferred as GitLab merge requests on feature branches.
Emailing patches is OK too.

During development you can enable cheating (via the `ship.cheat` field) to make
debugging easier. To enable cheating run Bussard with `--cheat` argument; you
can disable it again by running with `--no-cheat` argument. The state of
cheating is saved when you restart the game without parameters, resetting the
game switches cheating off again. The `ship.cheat` variable provides access to
the real, read-write unfiltered `ship` object. For instance, it may be expedient
to run `ship.cheat.comm_range = 9999999` in order to make testing login
interaction easier.

You can also use `ship.cheat.realdofile` to reload some file from the game
source and `ship.cheat.realrequire` to load modules. Note that certain modules
like `ship` and `polywell` are stateful and will behave unpredictably if
reloaded.

## Coding

If you have never coded Lua before, don't fret! It's a very simple language;
once you learn how [tables](doc/lua-5-tables.md) work it's very much like any
other imperative dynamic language that leans heavily on closures. The only parts
that use advanced language features (metatables and coroutines) are the OS
filesystem and the read-only proxy tables like `ship.api.status` and certain
mission status checks.

The stock config in `data/src` is copied into the save directory when a new game
is started. Therefore changes to the source will not be visible to in-progress
games unless you use `ctrl-f1` to update your in-game config with the latest
stock files. Your old config files will be backed up. The `ship.host` table
is populated with the contents of the `host_fs` directory inside Bussard's save
dir; it will persist between wiping save games. This is useful mostly during
development. You can trigger the load of `host_fs/config` with `ctrl-tilde`.

You may find the code in `spoilers/solutions` useful during development.

## Self-hosting

The text editor inside Bussard is very capable and should be comfortable to use
for editing Lua by all but the most die-hard Emacs/Vim fans. It can be used not
only for editing in-game code, but also for the source to the game itself if you
are running from a checkout. Symlink your checkout to `game` inside the save
directory. When you press `ctrl-o` to open a file, put a `/` in front of the
path of any files inside the checkout of the game to open it in the editor; for
instance, `/Contributing.md` would open this file.

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
function f()`, you know to pay more attention because a recursive function (or
a function that needs self-reference for other reasons) is coming.

There are four different code contexts (at the time of this writing; more may
be introduced in the future): engine code, in-ship code, rover code, and OS code. The
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

We also run [luacheck](https://github.com/mpeterv/luacheck) to check for
simple mistakes that can be caught with static analysis, like typos or
accidental globals. It also catches certain style issues. You can install it
via your package manager or [luarocks](https://luarocks.org) To use it thru
luarocks, ensure the `bin` directory of luarocks, typically
`~/.luarocks/bin`, is on your `$PATH`, then run `luarocks install --local luacheck`.

Then do a full test run:

    $ make ci

This will also run the [fuzz tests](https://technomancy.us/180). You can run
individual test phases with `make luacheck`, `make test`, or `make fuzz`.

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
different parts of the codebase are related to each other. (The OS/services
parts are a bit out of date, so see the stuff about RPCs below.)

The `ship` table (loaded from `ship/init.lua`) contains all game state apart
from host OSes like station computers and rovers. In particular, `ship.bodies`
is the table for all the worlds, asteroids, and ships in the current system,
and `ship.systems` contains all systems. (I guess it doesn't make all that
much sense, but it's very convenient.) The `ship.systems` table is loaded from
the `data/systems.lua` file; `ship.bodies` is set with the worlds of the
current system when you enter it, plus asteroids and ships as needed.

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

Time in the game is represented as seconds since epoch, with the game set in
the year 2431. There's a time factor meaning in-game time passes 10x faster
than real time.

Note that from an in-game perspective, the `ship.api` table is referred to as
just `ship`.

### Data

The `data/` directory contains mostly non-code stuff like mail, missions, and
definitions of star systems, but `data/src` also contains all the code that is
loaded into the onboard computer by default and can be edited by the
player. The `host_src` directory contains source which exists on certain host
computer systems in-game, while `os/orb/resources` contains code which is
loaded onto all Orb OS systems.

Theoretically, replacing the `assets`, `data`, and `doc` directories would give
you a different game using the Bussard engine. Try to keep anything specific to
the worlds and story of Bussard in these directories, and everything elsewhere
should be agnostic, dealing only with the engine.

#### Missions

Missions are found in the `data/missions` directory. You accept them by replying
to mail. The fields that a mission may have are all documented at the top of
`mission.lua`. Usually completing a mission sets events, which allows for other
missions to come available through new mail.

The `ship.active_missions` table stores a record about each mission that is
currently ongoing, containing at least its start time, required destinations,
and messages to show upon reaching said destinations. This table is persisted,
and missions can write arbitrary data to it in order to track mission progress
in `mission.update` and other functions.

#### Mail

Mail is stored in-game in `ship.api.docs.mail`. There are basically 3 things
that can trigger mail delivery: timed events based on `data/msgs/timed.lua`,
replying to a message, or something happening within a mission. Messages are
stored on disk in `data/msgs`.

Certain messages are replyable. Pressing `alt-enter` when viewing them will
either accept a mission (in `data/missions/` and named after the message-id of
the original message), or cause an event to be triggered
(`data/msgs/event.lua`), or cause another message to be sent to you, if there is
a file in `data/msgs` whose filename is the message-id of the original message
you reply to in order to get it.

Files in `data/missions` and `data/msgs` that are named after message-id headers
should also have symlinks to them for human-readable names.

Mail should stick to the typical header/body pattern; roughly RFC 822. Use the
"In-Reply-To" headers for message threads.

### OS and SSH

The worlds you SSH into mostly run the Orb unix-like operating system, which
is found in `os/orb`. All the scripts that run inside the OS (userspace) are
found in `resources` in that directory. The portals run the `os/lisp`
operating system, and the `os/rover` OS will be used for rovers.

All OS code is isolated by running it in separate LÖVE threads. Each thread
can communicate with others only by channels. The `os.client` module is the
bit which is exposed to your ship's sandbox and the main thread, and the
`os.server` module runs in the isolated thread and delegates to the
appropriate OS for whatever is on the other side. Each world has an
`os.server` thread for accepting connections, and when a new connection is
made, a session-specific thread (running `os.rover.session`, etc) is started.

Mostly the communication consists of "standard IO"--lines of text sent and
received. (`op="stdin"` or `op="stdout"`) However, the protocol is simply
tables sent and received, and other operations can be invoked. The code
running on the remote OS must occasionally invoke functions on the main
thread, (such as cargo transfers or refueling) and this is done by sending
`op="rpc"` messages across the channels, which invoke a set of whitelisted
RPC-able functions in the top-level `rpcs` module. All RPC functions have the
first two args locked to `ship` and `port`, which is the table for whatever
world on which the OS is running. Each OS exposes a different set of RPC
functions to code running in its threads.

Note that versions of LÖVE prior to 0.10.2 would not allow tables to be sent
over channels if they included other tables inside them. Dropping the flat
tables requirement for channels was done by accident. We want to avoid sending
non-flat tables over channels if possible for compatibility with 0.10.1.

The UI side of the SSH client is defined in `data/src/ssh`; it is based on the
ship's computer's console mode, but it sends input across the SSH channel
rather than eval when you press enter.

### Editor

The onboard computer uses [polywell](https://gitlab.com/technomancy/polywell) as
the interface for everything, not
just editing code. Lua console sessions are run in an editor buffer, as are SSH
sessions which connect you to worlds and the messaging system. The user is free
to define their own modes as well. Key presses are translated by the editor into
text insertions or commands based on the keymap for the current mode; a system
which is largely based on Emacs. See `find_binding`, `define_mode`, and `bind`
in `polywell/init.lua`, and `data/src/config` for a usage example.

Note that changes to polywell should be contributed to the polywell repository,
not this one. It is brought in using `git subtree`.

### Save

Certain fields of `ship` and various planets/stations/other ships are saved
off when you quit, but there is a whitelist; not everything is saved between
exits. (See `ship_fields` and `body_fields` in `save.lua`.) Within user data
of `ship.api`, the fields that get persisted are listed in `ship.api.persist`,
allowing the player to declare additional fields as persistent.

Ship fields go in `ship_data.lua` in Love's save directory, while status of
the current system goes in `system_data.lua`. The editor buffers are also
saved off into their own files. Nothing is saved from the status of systems
other than the current one except the filesystems.

## License

By making contributions, you agree to allow your material to be
distributed under the terms of the GNU General Public License, version
3 or later; see the file LICENSE for details.

