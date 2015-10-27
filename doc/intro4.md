# Mining

You've bought some fancy new hardware for your ship, so let's get it
ready to use. All the flying you've been doing so far has just been
done by having your keystrokes translated into engine burns by your
onboard computer; this is set up by your ship's config file. You'll
need to add a new key to fire the laser you've just purchased. The
laser's manual explains how to do this:

    man("laser")

The main manual also has a lot more details about the technical
details of your ship:

    man("manual")

Once you've made the changes to the ship's config file described in
the manual, (using ctrl-enter) reload for the changes to take effect:

    ship:load("src.config")

Now your laser will fire when you hold the alt key down, so use your
targeting system to find the nearest asteroid. Get close enough for
the targeting indicator to turn red, then fire your laser at them to
mine them up.

To continue, run:

    man("intro5")
