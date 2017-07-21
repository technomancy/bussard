# Bussard changelog: history of user-visible changes

## beta-3 / ???

* Add orbital lock function to make it easier to remain in orbit.
* Warn when loading save games from older versions.
* Tab completion works on remote hosts.
* Editor uses [Polywell](https://gitlab.com/technomancy/polywell)
* Add rovers you can log into with vector-graphics sensor rendering.
* Include in-game tetris.
* OSes on remote hosts now run in independent threads, can't hang main game.
* New intro mission.
* Dramatically improved HUD trajectory.
* Add underclocker upgrade.

## beta-2 / 2016-08-13

* Added alt-up key binding to go up a folder in mail mode.
* All editor colors can be customized.
* Syntax highlighting for Lua code. (requires LÖVE 0.10.x or higher)
* Emacs bindings are more accurate, but off by default; uncomment to load.
* Add replace command to editor; defaults to alt-r.
* Add incremental search command to editor; defaults to ctrl-f.
* Add command to select buffer to switch to; defaults to ctrl-alt-b.
* Live-feedback when opening files.
* Many reliability fixes for the editor.
* Mouse wheel zooms and scrolls; can be rebound.
* Add `go_to` command for jumping to line; defaults to to alt-g.
* Add API for connecting over SSH and getting output back programmatically.
* Make it possible to edit the game from within the game during development.
* Fix a bug where contents of proxy tables wouldn't print.
* Integrate with system clipboard (alt-c and alt-v).
* Persist console history between runs.
* New challenge: connect to Subnet.
* Add socket repl so you can play the game from another process.
* New coding challenge: rot13 decryption.
* Messages can be delivered at any time, not just after portal jumps.
* Improved graphical effect during portal jump.
* Further mission progression.
* Tab completion in the console.
* Support toggling fullscreen, resizing game window.
* Add archive feature to mail system.
* Allow the font to be changed.

## beta-1 / 2016-05-17

* Better display of tables from in-game console.
* Allow reverting to stock config as fallback recovery.
* Add life-support system upgrade.
* Some support for UTF-8 characters.
* Various editor bugfixes.
* Repeated presses of ctrl-tab select next-closest target.
* Format times as years:seconds.
* Simplify gravitation to avoid certain bugs.
* Descriptions for Bohk and Katilay worlds.
* Pause screen.
* Added mail client with unread message tracking.
* Fix some gravitation bugs between planets.
* Major rewrite of console to unify it with the editor.
* Editor supports multiple files open at once.
* Spawn asteroids in mostly-stable orbits.
* Rename repl to console.
* Automatically adjust trajectory settings to achieve decent frame rate.
* Change default quit key to ctrl-q.
* Save state of asteroids.
* AI-controlled ships.

## alpha-6 / 2015-12-24

* A few more missions.
* Tab completion (only for the in-ship repl).
* Add a map upgrade.
* (Most) worlds tell you a bit about them when you log in.
* Editor can undo/redo.
* Added Comm Boost and Solar Panel upgrades.
* Finish adding the rest of the worlds.
* Offer to save the game in case it crashes.
* Add lisp OS which runs on portals and some stations.
* Allow reading manual pages before buying.
* Add ability to sell upgrades.

## alpha-5 / 2015-11-16

* Add limited support for glob patterns in shell.
* Create fuel charge booster upgrade.
* Performance increases.
* Add mission system and a handful of missions.
* Fix a bug where upgrades would not be available.
* Populated systems for Katilay and Bohk.
* Implemented clearance system for travel between governments.
* Portals now function by their own onboard computers too.
* Fix a bug where multibyte input would crash the game.

## alpha-4 / 2015-10-29

* Show readable table return values in repl.
* Change some key bindings invoking for repl and editor.
* Add tutorial.
* Create news system.
* Add in-game independent clock.
* Add portal animation.
* Fix a bug where flying would scramble repl input.
* Add power mechanics.
* Fix kill ring commands.
* Add API for in-game functions to run periodically.
* Add mark ring and pop-mark command to editor.
* Add Sol system.
* Change trajectory plot indicators to striped lines.
* Require portals be activated before traveling through them.
* Move HUD definitions to config file; allow reconfiguration.

## alpha-3 / 2015-09-27

* Boost range at which you can log in to stations.
* Fix a bug where files in station OSes couldn't load from releases.
* Other minor bugfixes.

## alpha-2 / 2015-09-25

* Rudimentary text editor for onboard config.
* Add onboard help system and some manual pages.
* Refuel at stations.
* Station filesystems get saved/restored upon restart.
* Enable buying non-guest accounts on station computers.
* Allow copying of files to/from station accounts.
* Customizable keyboard commands.
* Upgrades available for purchasing from station computers.
* Increased ship mass makes acceleration more sluggish.

## alpha-1 / 2015-09-15

* Initial alpha functionality.
* Flight works with realistic gravity/thrust mechanics.
* Portals to jump between systems.
* Onboard REPL with sandboxing.
* Logging into space station computers with faux-unix.
* Asteroid mining using lasers.
* Trade cargo on stations.

