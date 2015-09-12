# Operating manual for Kosaga shipyards Bussard-class spacecraft

Congratulations on the purchase of your spacecraft. Your ship is
equipped with state of the art components as well as the capacity to
dynamically reconfigure to add new capabilities.

Please review this manual to ensure you have a complete understanding
of your ship's operation, and you will enjoy many safe travels on board.

## Display

The first thing you'll see when you launch your ship is its
heads-up-display. At the right you'll see a readout indicating your
current velocity vector. Once you set a target, (`tab` by default)
you'll see below the velocity of your target, and below that the
gravitation acceleration your target is applying to your ship. A
targeting indicator will also point from your ship in the direction of
your current target. The upper left shows basic speed, coordinates,
target details, fuel readout, credits, and a clock.

## Helm control

Your ship comes pre-configured with a keymap which allows piloting the
ship using the `up`, `right`, and `left` arrow keys on your helm
keyboard. The `+` and `-` keys also allow you to zoom in and out of
your view.

TODO: describe navigating to portals

## Computer

The heart of your ship is its onboard computer. The Bussard-class
spacecraft is equipped with a LuaJIT-based control unit, offering a
high degree of flexibility and customization without sacrificing
performance.

At the bottom of the screen is the REPL where you can interact directly
with your ship's onboard computer. By default a single line is shown,
which displays either the current line of input (if applicable) or the
last line of output. Pressing `ctrl-backtick` toggles the full
repl, which allows you to scroll through the previous output. While
the full repl is active, the piloting controls for your ship will be
unavailable. Enter any Lua code into the REPL to have it
evaluated. See the API section below to learn how to control your ship
from code.

When your ship starts, it will load its init file in order to create
key bindings, define helper functions, and perform any other setup.

TODO: document init file

### Communication system

When you are in range of a planet or space station with an active
computer, you can initiate a login session using your ship's
communication system. This will allow you to access any of the
station's services, including refueling, cargo transactions,
purchasing upgrades, downloading new libraries, renting cargo storage,
and communicating with others on board the station.

Sessions are initiated using the `ship.actions.login` function, which
will log you into the currently-targeted planet or station with the
provided username and password if you are within communications range.

Most stations allow limited access via a `guest` account with an empty
password, which is the default if no username and password are
provided. Files on guest accounts are wiped upon logout. Once you are
logged in as a guest, you will have the option to purchase an account
on that station or planet's computer system, which will grant you
access to the rest of the services as well as persistent file
storage. Please note that attempting to access accounts of others is
strictly forbidden by interstellar law.

Services offered on stations vary by locale, but most stations at
least offer to buy and sell cargo. The `cargo` executable takes
`list`, `buy`, and `sell` subcommands; see its online help for usage
details.

TODO: copying files to/from your ship

### API list

* repl
* sensors
* actions
 * forward
 * left
 * right
 * next_target
* controls
* commands
* comm
* helm
* cargo

### Upgrades

Certain stations will sell you upgrades to your ship. These can either
introduce completely new components or improve the effectiveness of
existing components.

TODO: describe how to install upgrades, read documentation for them
