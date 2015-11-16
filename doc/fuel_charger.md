# Aperture Technology Fuel Charge Booster

The Aperture Technology Fuel Charge Booster is a device that diverts
power from your ship's onboard reserves in order to temporarily
increase the effectiveness of the fuel collector. The rate at which
fuel is collected is quadrupled while it is active, provided there is
sufficient power.

## Control

Because it incurs a significant power drain on your ship, it's not
recommended to leave the Fuel Charge Booster on. Instead, bind it to a
key in the `controls` section of your ship's configuration:

    ship.controls["ralt"] = ship.actions.fuel_charger

This will activate it when the right alt key is held, but you can bind
it to any other key you wish. See `man("keycodes")` for a full list of
possibilities. As always, don't forget to reload your config before
using it:

    ship:load("src.config")

Copyright © 2426 Aperture Technology, All Rights Reserved
