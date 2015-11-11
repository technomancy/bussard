# Challenges

## Missions

Fields

* name
* id
* objectives (list of world names)
* events (on success)
* credits
* cargo
* time limit
* updater function

## Auto-miner

A mission to mine an asteroid with a particularly rare ore. This
asteroid is in an extremely low, erratic orbit. It's basically
impossible to match orbit with it, but you can write code that will
fire your laser when the target is in front of you, and then just
enter another wild low orbit and wait for the shot to line up enough
times to mine out the asteroid.

You can also use the Eye of Harmony to slow down time in order to get
close enough to mine the asteroid, but you don't get that till later.

## Breaking into the portal

The activation of the portal relies on the .smashrc file in the guest
account. If you try to run a shell as a guest, it bumps you straight
into the portal activation routine, which disconnects you as soon as
it determines whether or not you're authorized to make a jump.

However, when logging in, you can provide an alternate command to run
other than "smash". Replacing your login command with "lua" allows you
to run arbitrary code on the portal's onboard computer.

## Yueh servers

Yueh servers don't run Orb; they have a Scheme implementation as their OS.

Or should this be the portals?

## Running Causal Domain Injector

The "OS" of the lab in which the Causal Domain Injector is found (as
well as the one in which the portals were discovered, but you don't
get access to that one) is a Forth.
