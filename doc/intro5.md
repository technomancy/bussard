# Trade

After you've mined an asteroid or two, check your cargo bay by running
this in the console:

    ship.status.cargo

In your onboard computer, "ship" is a table which contains data
regarding your ship as well as functions to activate the ship's
features. In console mode, type "ship." and press tab to see all the
fields for this table. You can start typing "ship.status.h", and
pressing tab will auto-complete it to "ship.status.heading", which is
helpful for exploring the contents of any table in the ship's computer.

TODO: completion currently non-functional; need to re-enable

You'll also want to check the total carrying capacity of your cargo
bay so you know when it's filling up:

    ship.status.cargo_capacity

You can bring this cargo to Mirduka Station, but you'll get a better
price for it in a system that isn't a mining hub. It's time to head to
the portal. Press tab a few times until the portal is targeted, and
make your way over. You'll need to match orbit, but not as precisely
as with the station, since you'll be on your way shortly. Once you're
close enough for the targeting indicator to turn blue, hit ctrl-s. If
you can stay near enough for the few seconds it takes the portal to
activate, you'll soon be on your way to the next system.

The Tana system has two planets in addition to the space
station. Approach whichever planet is closer and log in once you've
got a reasonably stable orbit. You can sell the ore you've mined with
the "cargo" command once you're logged into the planet's computer:

    cargo sell ore 10

You can use this money to buy some other goods or check out what
upgrades are available. A cargo bay upgrade will let you carry more
ore if you decide to go into trading. From here the portal to Luyten's
Star will get you closer to Sol, while the one to Wolf 294 will take
you to another asteroid-rich system.

The galaxy is yours to explore! But don't forget to explore your
system's onboard computer as well. The ship's primary manual can be
accessed by running this:

    man("manual")
