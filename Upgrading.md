# Upgrade guide

## From beta-1

Many of the new features in beta-2 are in the editor config, so if you take a
game from beta-1 and run it in beta-2, it will be missing all these nice things!
So it's recommended to run a config reset by pressing `ctrl-f1`; this moves your
existing config files to `src.bak` and replaces them with fresh files that have
the new features enabled.

If you do want to keep your existing config, you will at least want to add a
scale factor of `0.1` to the `values` table in the two vector readouts of
`src.hud` like so:

     { x=5, y=5, type="vector",
       values={"status.dx", "status.dy", 0.1},
       width=3, color={50, 255, 50}
     }

You will also want to change your zoom functions to work more slowly:

    ["-"] = function(d) if d then ship.scale = ship.scale + (ship.dt/20) end end,
    ["="] = function(d) if d then ship.scale = ship.scale - (ship.dt/20) end end,

Changing the divisor from 2 to 20 in the zoom functions will do this.
