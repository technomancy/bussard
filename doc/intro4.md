# Mining

You've bought some fancy new hardware for your ship, so let's get it
ready to use. All the flying you've been doing so far has just been
done by having your keystrokes translated into engine burns by your
onboard computer; this is set up by your ship's config file. You'll
need to add a new key to fire the laser you've just purchased. The
laser's manual explains how to do this:

    man("laser")

Once you've made the changes to the ship's config file described in
the manual, (using ctrl-enter to enter the editor) reload for the
changes to take effect:

    ship:load("src.config")

Now your laser will fire when you hold the left alt key down, so use
your targeting system to find the nearest asteroid. Your laser is more
effective at close ranges, though you can still destroy asteroids at a
distance. However, if you're not close enough for the targeting
indicator to turn red, (scoop range) any ore from the asteroids you
destroy will not be collected into your cargo hold.

The HUD will tell you the mass of your target; asteroids of higher
mass will result in more ore being mined, but they take longer.

To continue, run:

    man("intro5")
