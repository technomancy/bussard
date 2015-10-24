# Bussard

<img src="http://p.hagelb.org/bussard.png" alt="screenshot" />

A space flight open-world exploration game, with a programmable space
ship and space stations. Mine, trade, upgrade, and unlock the potential of your
spacecraft by hacking on the code that makes it tick.

Read the [in-game manual](manual.md) for a taste of how the game works.

## Playing

When you start the game, your first priority is to rendezvous with the
nearby station. Press `tab` until your targeting indicator in your HUD
shows the station. The grey line will always point in the direction of
your target; the blue striped line indicates your current
trajectory. Head towards the station and try to make your trajectory
match its orbit, but keep an eye on your velocity and fuel supply. If
you accelerate too much, you may not have enough fuel to match
velocity with the station. Once you get close, it will be easier to
match velocity if you zoom in with `+`.

Once you are in orbit around the station, and are in range, the grey
line pointing towards the station will turn green. Press
control-backtick and type `login()` to establish a connection. You can
see all the commands available on the station by typing `ls /bin`, but
at this time you only need to concern yourself with the `upgrade`
command. Run `upgrade buy laser`, then `logout` followed by
`man("laser")` to learn how to use the laser. You will need to edit
your config file (with `ctrl-enter`) to add a key binding to turn on the
laser, as explained on the laser's manual page.

From there it's off to find an asteroid to mine, and then the galaxy
is yours to explore. To jump to another system, find a portal and
press `ctrl-space` when you are within range. You'll want to check out
the ship's manual with `man()` at some point though.

Recommended soundtrack:
[Contingency](http://music.biggiantcircles.com/album/contingency) by
[Big Giant Circles](http://www.biggiantcircles.com/) though the
[FTL soundtrack](https://benprunty.bandcamp.com/album/ftl) is a great
fit too.

## Installation

Releases for each platform are [on itch.io](https://technomancy.itch.io/bussard).
Windows and Mac OS X releases are standalone, but Linux releases require having
[LÖVE](http://love2d.org) 0.9.x installed.

When running from source, type `love .` from a checkout.

<img src="http://p.hagelb.org/bussard-repl.png" alt="repl screenshot" />

One problem when running from source is that when new features are
added, key bindings for them are added to the default config, but
existing saved games will continue on using the same config. You can
replace your ship's config with the current default config using
`ship.src.config = default_config`.

## Status

Currently the game supports flying around, mining asteroids, trading
cargo, and running some code in the in-game REPL. You can also edit
your ship's config file in order to reprogram it, change key bindings,
and add convenience functions.

The interaction with the planet and station computers is somewhat
limited at this time. Basically just running vanilla Unix commands and
purchasing cargo, refueling, and ship upgrades. Run `ls /bin` when
logged in to see a listing of all available commands. The `cargo`,
`refuel`, and `upgrade` commands are of particular interest.

See the list of
[open issues](https://gitlab.com/technomancy/bussard/issues) to see
upcoming features.

<img src="http://p.hagelb.org/bussard-edit.png" alt="edit screenshot" />

Planned features:

* accept missions at stations
* many, many more systems under various governments
* many more ship upgrades
* tutorials for writing autopilot code
* exploits to break into accounts you don't own
* stations that use different OSes
* virtualization to allow you to run station code on your ship
* abandoned civilizations and artifacts

During development it may be expedient to run `ship.cheat.comm_range = 9999999`
in order to make testing login interaction easier.

## Influences

* [Escape Velocity](http://www.ambrosiasw.com/games/ev/) (gameplay)
* [Kerbal Space Program](https://kerbalspaceprogram.com/en/) (mechanics)
* [Marathon Trilogy](http://marathon.bungie.org/story/) (story)
* [A Fire upon the Deep](http://www.tor.com/2009/06/11/the-net-of-a-million-lies-vernor-vinges-a-fire-upon-the-deep/) (story)
* [Anathem](http://www.nealstephenson.com/anathem.html) (story, philosophy)
* [Mindstorms](https://www.goodreads.com/book/show/703532.Mindstorms) (philosophy)
* [GNU Emacs](https://www.gnu.org/software/emacs/) (architecture)
* [Unix](https://en.wikipedia.org/wiki/Unix) (architecture)
* [Atomic Rockets](http://www.projectrho.com/public_html/rocket/) (science)

## Licenses

Code copyright © 2015 Phil Hagelberg and contributors

Distributed under the GNU General Public License version 3; see file COPYING.

[Some planet graphics](http://opengameart.org/content/planets-and-stars-set-high-res) © Rawdanitsu, CC0 licensed.

[Other planet graphics](http://opengameart.org/content/27-planets-in-hi-res) © Shaber, CC-BY licensed.

[Still more planet graphics](http://opengameart.org/content/more-planets) © cemkalyoncu, CC-BY licensed.

[Solar system graphics](http://www.solarsystemscope.com/nexus/resources/planet_images/) © Inove, CC-BY licensed.

Station graphics by [MillionthVector](http://millionthvector.blogspot.de/p/free-sprites_12.html), CC-BY licensed.

Mensch font by [Robey Pointer](http://robey.lag.net/2010/06/21/mensch-font.html), distributed under the [Bitstream Vera](https://www.gnome.org/fonts/) license.
