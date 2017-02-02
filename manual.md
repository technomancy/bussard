# Operating manual for Kosaga Shipyards Bussard-class spacecraft

Congratulations on the purchase of your spacecraft. Your ship is
equipped with state of the art components as well as the capacity to
dynamically reconfigure to add new capabilities.

Please review this manual to ensure you have a complete understanding
of your ship's operation, and you will enjoy many safe travels on board.
There are manuals on other topics as well, use `man("list")` to see them all.
Most upgrades that you purchase will come with new manuals too.

## Display

The first thing you'll see when you launch your ship is its
heads-up-display. A targeting indicator will point from your ship in the
direction of your current target (changed by pressing tab). If you are within
range of a target you can act upon, the indicator will change color depending
on the type of the target. The upper left shows basic velocity, local
coordinates, fuel and battery readout, credits, and a clock, while the upper
right shows target details if a target is selected. One trajectory shows your
ship's projected orbit around the star, while another shows your target's
projected path, if applicable. If you're close enough to your target to orbit
it, your ship's trajectory will shift to show your orbit around your target
instead.

## Helm control

Your ship comes pre-configured with a keymap which allows piloting the ship
using the up, right, and left arrow keys on your helm keyboard. The = and -
keys or your mouse's scroll wheel also allow you to zoom in and out of your
view. You can fly around the system as your fuel supply permits. The ship's
onboard collectors will replenish the fuel supply, though slowly.

In accordance with interstellar law, your ship is equipped with
Collision Avoidance System (CAS) which prevent you from plotting a
course which would result in a collision with another body. If you set
yourself on a course towards another ship, a station, planet,
asteroid, or star, the CAS will automatically adjust your course so
that you will fly harmlessly over it.

## Computer

The heart of your ship is its onboard computer. The Bussard-class
spacecraft is equipped with a LuaJIT-based control unit, offering a
high degree of flexibility and customization without sacrificing
performance.

Pressing ctrl-enter toggles the ship's computer. While it is active, the
piloting controls for your ship will be unavailable until you go back to
flight mode with the esc key. The computer starts off in console mode. While
in the console, enter Lua code into the console to have it evaluated and
the result shown. See the API section below to learn how to control your
ship from code.

The console is just one of the buffers you can open in the ship's
computer (it is called `*console*` as you can see at the bottom of the
screen). When you activate the computer it will take you to the last
active buffer; you may need to use `ctrl-pageup` or `ctrl-pagedown` a
few times to find the console.

Note that some of your ship's functions will also display messages to
the console. The last line of the console output will be visible at
the bottom of the screen during flight mode.

When your ship starts, it will load its configuration file (called
"src.config") in order to create key bindings, define helper
functions, and perform any other setup. This file also loads other
files (like "src.edit" or "src.mail") which define other specialized
modes of interaction.

### Editor

You can make changes to your configuration files using the onboard
editor. Press ctrl-o to open a file; start with "src.config". You can
see where the flight mode is defined and how the flight controls and
other commands are bound to keystrokes. The bind function can attach
any function to a keystroke.

Once you're done with your edits, press esc to go back to flight mode
and load your changes with ctrl-r. You can edit other files anywhere in
your ship, but by default only files in the "ship.src" and "ship.docs"
tables will stay after your ship is restarted. You can configure it to
save other tables by adding their names to the "ship.persist" table.

You can open as many files in the editor as you need with ctrl-o. Use
ctrl-pageup and ctrl-pagedown to cycle through them.

### Communication system

When you are in range of a planet or space station with an active
computer, you can initiate a login session using your ship's
communication system. This will allow you to access any of the station's
services, including refueling, cargo transactions, purchasing upgrades,
downloading new code, renting cargo storage, and communicating with
others.

Your targeting indicator will turn light green when you are within
range of a station that allows logins.

Sessions are initiated using the ssh() function, which will log you
into the currently-targeted planet or station with the provided
username and password if you are within communications range. While
you are logged in and stay within range, the port can send automated
loaders to your ship to transfer cargo and passengers.

Most stations allow limited access via a "guest" account with an empty
password, which is the default if no username and password are
provided. Files on guest accounts are wiped upon logout. Once you are
logged in as a guest, you may have the option to purchase an account
on that station or planet's computer system, which will grant you
access to the rest of the services as well as persistent file
storage. Please note that attempting to access accounts of others is
strictly forbidden by interstellar law.

Services offered on stations vary by location, but most stations at
least offer to buy and sell cargo. The cargo program takes list,
buy, and sell subcommands; see its online help ("cargo --help")
for usage details. You can run "ls /bin" to see a list of built-in
programs on most computer systems.

While your login session is active, you will not be able to enter any
code into your ship's computer, and the console prompt will change to
`$`. Enter `logout` to terminate your session and return to your
ship's computer.

For non-interactive sessions, you can use `ssh_get_connection`, which
takes a username and password and returns a function that takes a
command to run and returns its output.  This can be useful for writing
programs to automate common tasks. Call the resulting function with
"logout" to terminate the connection.

### Portals

Your ship can travel to other star systems using the portal
network. If your battery has enough charge, fly within range of a
portal. You'll know when you are close enough because the targeting
indicator will turn blue. Press ctrl-p to begin the portal
activation sequence. You will need to stay within range for a few
seconds for it to complete.

Certain portals which allow travel between systems of different
governments require you to receive clearance before you may travel
through them.

### API list

In your onboard computer, `ship` is a table representing the data and
functionality that can be accessed by code you write.

#### ship.status

These read-only fields primarily convey the status of your ship's components and
its upgrades as well as its flight through space and the objects nearby.

* `x`, `y`, `dx`, `dy`: position and velocity.
* `heading`: direction pointing, in radians.
* `target`: table representing the body (planet, star, station, portal, etc.) targeted.
* `system_name`: the name of the current system.
* `bodies`: an array of bodies present in the current system.
* `fuel`, `fuel_capacity`: current and maximum fuel levels.
* `mass`: current total mass of the ship, its upgrades, and its cargo.
* `engine_on`: boolean indicating whether the engine is firing.
* `turning_right`, `turning_left`: booleans for turning thrusters.
* `credits`: how many credits you have in the ship.
* `cargo`: a table of good names to tons of that good in your hold.
* `cargo_capacity`: the maximum total tonnage you can fit in your hold.
* `engine_strength`, `turning_speed`: capabilities of engine/turn thrusters.
* `burn_rate`: how quickly your engine uses fuel; turn thrusters use negligible fuel.
* `recharge_rate`: how quickly your collector replenishes your fuel.
* `solar`: how quickly your battery charges (based on distance from star).
* `comm_connected`: whether or not the comm system is connected.
* `comm_range`: the maximum distance at which your comm is effective.
* `scoop_range`: the maximum distance at which you can collect mined asteroid ore.
* `in_range`: function (takes ship and body) returning boolean for
  whether the body is in range of the communication system. An optional
  third argument can determine whether it's within an arbitrary range.

#### ship.actions

These functions affect changes to the ship's systems.

* `forward`, `left`, `right`: accepts a boolean indicating whether to
  fire the given engine/thruster.
* `next_target`: cycle sequentially through all the targets in the system.
  tab by default, shift-tab to cycle in reverse.
* `closest_target`: select the closest available target. ctrl-tab by default.
* `ssh`: accepts optional username/password, attempts to establish an ssh
  session with the target if possible.

Upgrades often add new functions to this table. Consult the manual for
any given upgrade for details.

#### controls (flight)

This is a table of keys to functions; usually functions in the
`ship.actions` table above. These function slightly differently from
the keymap functionality described below. Keymaps can only be used for
commands that are triggered once per key press. Controls must be used
when the ship will behave differently based on how long the key is
held. All piloting functionality should use this table, since holding
down the turn key will turn further than just pressing it quickly. The
zoom functions need to use it too.

#### scale

A numeric indicator of how far out the display should be zoomed. Zoom
scales out exponentially with this number, so it should usually be
between 1 and 2.

#### updaters

Functions in the `ship.updaters` table will be run periodically;
typically several times per second. They are passed the ship table as
well as an argument indicating how many seconds it has been since the
last time they ran. This can be useful for implementing automated
piloting or any kind of functionality that needs to check the current
status of the ship as it changes in flight.

As an example, here is a function which adds a counter to keep track
of how many total seconds the engine is engaged:

    ship.updaters.engine_time = function(ship, dt)
      if(ship.status.engine_on) then
        ship.engine_time = (ship.engine_time or 0) + dt
      end
    end

In addition, there is a `ship.on_enter` function you can set; this will be
called once for every time you enter a new system. It will receive as an
argument the name of the system you are entering.

#### hud

The heads-up-display is configurable by setting the `ship.hud`
table. It is set by default in the "src.hud" file. It should be
an array of instrument tables. There are three different valid types
of instruments, "text", "vector", and "bar"; each needs at least
`type`, `x`, and `y`. Negative `x` and `y` values will count backwards
from the right or bottom of the screen.

First you have just plain `"text"`. These instruments look like this:

    { type="text", x=5, y=5, format="x: %5.2f y: %5.2f", values={"sensors.x", "sensors.y"} }

The `format` is the template for the text shown on screen, with the
`values` looked up in the ship table and then spliced into the format
template. Values with dots in them indicate that they are nested
inside tables. Include `align="right"` to justify the text along the
right side instead of the left.

Second is the two-dimensional `"vector"` type instrument, like this:

    { type="vector", x=5, y=60, values={"sensors.dx", "sensors.dy"} }

Here the `values` are just the X and Y values displayed by the vector
indicator; again they are looked up in the ship table before being
printed. The example above shows the ship's velocity.

Finally we have `"bar"` for percentages:

    { type="bar", x=20,y=-15, values={"status.battery", "status.battery_capacity"} }

The first value is the actual value to show, and the second is the
maximum possible value; it shows a bar that can be fully or partly
filled in.

Any type can have a `color` field set to customize its color.
See the default configuration file for an example.

The `values` can also include functions instead of strings. The
function will be passed the ship table, and their return values used.

### Upgrades

Certain stations will sell you upgrades to your ship. These can either
introduce completely new components or improve the effectiveness of
existing components.

Once you have logged into a station that sells upgrades, you can run
the `upgrade` script to see their prices. Run `upgrade buy map`, for
instance, to purchase a map, provided you have enough credits. Once
you have purchased an upgrade, you can usually view its documentation
by logging out of the station and accessing its manual page with
`man("map")`, etc. Many upgrades will require making changes to your
ship's config code to use; the changes will be described in the manual.

### Mail

Your ship also comes with a mail client allowing you to read and
respond to messages you receive. Press ctrl-m from flight mode to
activate it. You'll be shown a list of folders, each with an
unread/total message count. Pressing enter on a folder opens it up and
shows the date, sender, and subject of each message.

If a message offers a mission or makes some request of you, you can
indicate your acceptance or acknowledgment by pressing
alt-enter. Pressing alt-a will move the message to the "archive" folder.

### Missions

Your HUD is preconfigured with a listing for active missions. To get
more details, you can run `ship.missions.list()`, while you can abort
a given mission with `ship.missions.abort("MISSION_ID")`, though this
will forfeit any cargo from the mission.

### Keymaps and Modes

The currently active mode and its keymaps will dictate how your ship
will react to keys on your helm keyboard. Once a mode is defined, you
can add key bindings to it like so: `keymap.define("flight", "ctrl-k",
function() print(ship.status.dx, ship.status.dy) end)`. The first
argument is the name of the mode you're adding the key binding to, the
second argument is a string describing the key to bind, and the third
argument is the function to call when the key is pressed.

By default your ship has a "flight" mode active normally and a "console"
mode for entering code directly. There is also an "edit" mode where the
editor key bindings are defined. The "ssh" mode is active when
connecting to station or planet computers, and the "mail" mode is active
when reading your messages. You can check the current mode with
`ship:mode()`.

See the `keycodes` manual page for a detailed listing of available key
names. You can run `man("keycodes")` to view it.

Take a look at the default config code that came with your ship for an
example of how to create modes and bind new keys in them; it will come
in very handy to add in new keys when operating your ship; in particular
when you purchase an upgrade that adds new capabilities.  You can also
define new modes that are based on existing modes; see "src.ssh", which
shows that the "ssh" mode has a parent mode of "console".

### Config Reset

It's possible for your changes to the ship's configuration to
introduce bugs into the editor or console. In some cases, the bugs may
interfere with your ability to fix those bugs.

If this happens, you can perform a config reset with ctrl-f1. Your
own configuration files will be moved to "ship.src.bak.config" etc,
and "ship.src.config" will be replaced with the working stock code.
From there you can continue your debugging. Note that this only
affects the configuration files that came with your ship; your other
files will be untouched.

## Footer

Copyright © 2365-2372 Kosaga Shipyards, All Rights Reserved.

If you are viewing this manual in your ship's onboard computer, press
the pageup key a few times to scroll to the top and begin reading there.
