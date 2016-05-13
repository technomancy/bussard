# PTMC Mining Laser Mk. I

The PTMC Mining Laser Mk. I is a heavy-duty deuterium-flouride laser
capable of operating at 64 megawatts. Its most popular application is
mining of metallic asteroids.

## Control

In order to fire your laser, it's recommended to bind it in the
`ship.controls` table. Left Alt is a common choice. Add this to your
ship's configuration by pressing ctrl-o followed by src.config to
open the ship config file. Add this line below the `ship.controls = {
[...] }` section.

    ship.controls["lalt"] = ship.actions.laser

In order to reload your config with the changes you've made, go back
to flight mode and press ctrl-r. Now you will will fire the laser as
long as the alt key is held down in flight mode. However, if you'd
like a key to toggle it on and off, you can use something like this
instead:

    keymap.define("flight", "lalt", function() ship.actions.laser("toggle") end)

The range of the laser is somewhat limited; if you are zoomed out it
may not be visible. Note that the laser consumes power while active;
it will shut off if your power reserves run too low.

## Mining

Once you have targeted and approached an asteroid, fire your laser to
cut away at the asteroid. For larger asteroids, once the laser has cut
all the way through it, it may split in two. Otherwise if it is small
enough and your ship is within scoop range, it will be loaded into
your cargo bay, assuming there is room for it. The scoop range is
typically indicated by a change in the color of your targeting
indicator once you get close enough to the asteroid.

The safety features of this laser include a mechanism to automatically
drop the power levels to a harmless range when it detects that it is
pointed at a space station, planet, or spacecraft.

Copyright © 2422 Post-Terran Mining Company, All Rights Reserved.
