# Next steps

You're going to want to rendezvous with the station in this system as
your first step. Pressing tab will cycle through available targets in
the system; use shift-tab to go backwards. Target the station, and
follow the line, which points in the direction of your current
target. Fire your engine to match your own trajectory with its trajectory.

Remember that inertia carries your ship forward even when your engine
is disengaged. You can turn your ship, but it's not till you apply
thrust that your trajectory will be affected.

As you approach the station, zoom in to make precision maneuvers
easier. Once you're close enough to the station, the line will
turn bright green. This means you are within communications range, and you
can log into the station's computer as a guest by running this function:

    ssh()

At this point you can explore the station's computer as long as you
stay within comm range. However, unless your orbit is stable, you will
drift out of range. If you are in console mode you can go back to
flight mode with ESC to make course corrections and you will stay
logged in.

While you're logged into the station, you can't run anything on your
ship's own computer, so in order to continue, run this:

    logout
    man("intro3")
