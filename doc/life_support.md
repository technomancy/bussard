# Tenagra Enterprises Life Support System

The Tenagra Enterprises Life Support System ensures safe, reliable
conditions for the life forms on board your ship. Capable of
maintaining an Earth-temperate climate with adjustable temperature and
relative humidity levels, it also recycles carbon dioxide into oxygen
and filters out harmful fumes that may accumulate from onboard
equipment.

You can use the set_climate function to change the climate settings,
and the get_climate function to read the current settings:

    ship.set_climate(22, 0.45) -- Set to 22 degrees celcius and 20% humidity
    ship.get_climate() --> { temp = 22, humidity = 0.45 }

Built-in safeguards prevent the setting of temperature level outside
the boundaries of what is safe for human life, while multiple levels
of redundancy and battery backup ensure that the system will continue
to operate in the event of a malfunction.

Copyright © 2422 Tenagra Enterprises, All Rights Reserved
