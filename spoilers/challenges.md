# Challenges

## Get a passponder

### Jinod missions

* [ ] Deliver sensor equipment to Mars
* [ ] Take sensors out for a spin; spend 1ksec within 3000 of Wolf 294
* [ ] Then study New Phobos with sensor (gives you passponder)

Maybe add multiple ways to get this?

## Auto-miner

A mission to mine an asteroid with a particularly rare ore. This
asteroid is in an extremely low, erratic orbit. It's basically
impossible to match orbit with it, but you can write code that will
fire your laser when the target is in front of you, and then just
enter another wild low orbit and wait for the shot to line up enough
times to mine out the asteroid.

You can also use the Eye of Harmony to slow down time in order to get
close enough to mine the asteroid, but you don't get that till later.

## Blacklisted from portal access

Late in the game, some nefarious conduct gets you blacklisted by
Aperture; the portals all refuse you access.

### Breaking into the portal

The activation of the portal relies on the .smashrc file in the guest
account. If you try to run a shell as a guest, it bumps you straight
into the portal activation routine, which disconnects you as soon as
it determines whether or not you're authorized to make a jump.

However, when logging in, you can provide an alternate command to run
other than "smash". Replacing your login command with "lua" allows you
to run arbitrary code on the portal's onboard computer.

### Scheme OS

The portals don't run orb, they use a scheme-based OS instead. You
have to learn this in order to find the access-control code inside the
portal and override it to give you clearance.

## Endgame

### Finding out about the multiportal

One of the Yueh portals is different; it connects to multiple systems.

### Multiportal Credentials

### Names of secret colonies

### Disable portal

### Running Causal Domain Injector

The "OS" of the lab in which the Causal Domain Injector is found (as
well as the one in which the portals were discovered, but you don't
get access to that one) is a Forth.
