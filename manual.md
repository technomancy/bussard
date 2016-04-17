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
heads-up-display. A targeting indicator will point from your ship in
the direction of your current target. If you are within range of a
target you can act upon, the indicator will change color depending on
the type of the target. The upper left shows basic velocity, local
coordinates, fuel and battery readout, credits, and a clock, while the
upper right shows target details if a target is selected. A light blue
striped path plots a trajectory estimate of where you're headed, while
a darker blue striped path plots one for your target, if applicable.

## Helm control

Your ship comes pre-configured with a keymap which allows piloting the
ship using the up, right, and left arrow keys on your helm
keyboard. The = and - keys also allow you to zoom in and out of your
view. You can fly around the system as your fuel supply permits. The
ship's onboard collectors will replenish the fuel supply, though
slowly.

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

At the bottom of the screen is the interactive code console
where you can interact directly with your ship's onboard computer. By
default a single line is shown, which displays either the current line
of input (if applicable) or the last line of output. Pressing ` (backtick)
toggles the full console, which allows you to scroll through the previous
input and output. While the full console is active, the piloting controls
for your ship will be unavailable until you leave the console with the
esc key. Enter any Lua code into the console to have it evaluated. See
the API section below to learn how to control your ship from code.

When your ship starts, it will load its init file (called src.config) in
order to create key bindings, define helper functions, and perform any
other setup.

You can make changes to your init file using the onboard editor. Run
this code in your REPL: `ship:e("src.config")`. (Also bound to
ctrl-enter) Once you're done with your edits, press `esc` and load
your changes with `ship:load("src.config")` (Also bound to
ctrl-r). You can edit other files in your ship by passing another
filename to `ship:e`, but by default only files in the `ship.src` and
`ship.docs` tables will stay after your ship is restarted. You can
configure it to save other tables by adding their names to the
`ship.persist` table.

### Communication system

When you are in range of a planet or space station with an active
computer, you can initiate a login session using your ship's
communication system. This will allow you to access any of the
station's services, including refueling, cargo transactions,
purchasing upgrades, downloading new code, renting cargo storage,
and communicating with others.

Your targeting indicator will turn light green when you are within
range of a station that allows logins.

Sessions are initiated using the `ship.actions.login` function, which
will log you into the currently-targeted planet or station with the
provided username and password if you are within communications range.

Most stations allow limited access via a `guest` account with an empty
password, which is the default if no username and password are
provided. Files on guest accounts are wiped upon logout. Once you are
logged in as a guest, you may have the option to purchase an account
on that station or planet's computer system, which will grant you
access to the rest of the services as well as persistent file
storage. Please note that attempting to access accounts of others is
strictly forbidden by interstellar law.

Services offered on stations vary by location, but most stations at
least offer to buy and sell cargo. The `cargo` program takes `list`,
`buy`, and `sell` subcommands; see its online help (`cargo --help`)
for usage details. You can run `ls /bin` to see a list of built-in
programs on most computer systems.

While your login session is active, you will not be able to enter any
code into your ship's computer, and the console prompt will change to
`$`. Enter `logout` to terminate your session and return to your
ship's computer.

If you have an account on a station server, you can copy files to and
from the targeted station using
`ship.comm.scp("username:password/path/to/file", "path.in.ship")`
and `ship.comm.scp("path.in.ship", "username:password/path/to/file")`.

### Portals

Your ship can travel to other star systems using the portal
network. If your battery has enough charge, fly within range of a
portal. You'll know when you are close enough because the targeting
indicator will turn blue. Press ctrl-space to begin the portal
activation sequence. You will need to stay within range for two
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
* `login`: accepts optional username/password, attempts to establish a
  login session with the target if possible.

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

#### hud

The heads-up-display is configurable by setting `ship.hud`. It should
be an array of instrument tables. There are three different valid
types of instruments, "text", "vector", and "bar"; each needs at least
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
the `upgrade` script to see their prices. Run `upgrade buy laser`, for
instance, to purchase a laser, provided you have enough credits. Once
you have purchased an upgrade, you can usually view its documentation
by logging out of the station and accessing its manual page with
`man("laser")`, etc.

### Missions

Often on computers at various worlds you can accept missions from the
people there, usually by browsing the newsgroups in `/usr/news` and
replying to requests for help.

Your HUD is preconfigured with a listing for active missions. To get
more details, you can run `ship.missions.list()`, while you can abort
a given mission with `ship.missions.abort("MISSION_ID")`, though this
will forfeit any cargo from the mission.

### Keymaps and Modes

The currently active mode and its keymaps will dictate how your ship
will react to keys on your helm keyboard. Once a mode is defined, you
can add key bindings to it like so: `keymap.define("flight", "ctrl-p",
function() ship.ui.paused = true end)`.  The first argument is the
name of the mode you're adding the key binding to, the second argument
is a string describing the key to bind, and the third argument is the
function to call when the key is pressed. This function is passed a
boolean indicating whether it is a repeated key or not.

By default your ship has a "flight" mode active normally and a
"console" mode active when the console is full-screen. Note that
"flight" mode does have the simple one-line console available, but not
all characters can be entered in this mode as some (like the zoom
keys) have other flight-related functions. There is also an "edit"
mode where the editor key bindings are defined. You can check the
current mode with `keymap.current_mode`.

See the `keycodes` manual page for a detailed listing of available key
names. You can run `man("keycodes")` to view it.

Take a look at the default config code that came with your ship for an
example of how to create modes and bind new keys in them; it will come
in very handy to add in new keys when operating your ship; in
particular when you purchase an upgrade that adds new capabilities.

## Footer

Copyright © 2365-2372 Kosaga Shipyards, All Rights Reserved.

If you are viewing this manual in your ship's onboard computer, press
the pageup key a few times to scroll to the top and begin reading there.
