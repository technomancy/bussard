# Orolo Communication Range Booster

Thanks for purchasing the Orolo Communication Range Booster. By
following the recommendations in this manual, it will provide you with
many years of safe, reliable functioning.

This device is capable of quadrupling the range of the communications
system on any standard spacecraft at the cost of some increased power
consumption.

## Usage

You can enable the Communications Range Booster by running the
following code in your onboard computer:

    ship.actions.comm_boost("on")

This code will disable the Booster and make it cease drawing power.

    ship.actions.comm_boost("off")

If you call it without an argument, it will simply toggle its
status. This can be useful for binding to a keystroke:

    keymap.define("flight", "f1", ship.actions.comm_boost)

Note that if the ship battery runs out of power while the device is
turned on, it will automatically disengage, allowing the battery to
recharge over time.

Copyright © 2431 Orolo Research, All Rights Reserved.
