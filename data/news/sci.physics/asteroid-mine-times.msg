From: Ganiedi Linabur
Subject: Asteroid mine times
To: sci.physics
Message-Id: dfea3dfe-b3d2-30a9-aa8c-dbab7dec64b2
Content-Type: text/plain; charset=UTF-8

One thing that makes mining a bit of a drag is that you can never really
tell how long it's going to take to mine through an asteroid. This
should depend on the megawatt rating of the laser, right? Is there any
way to determine how close to mining through an asteroid you are?



From: Tusuro Gollis
Subject: Re: Asteroid mine times
To: sci.physics
Message-Id: a027dbb2-98b1-9c4d-8d87-076f62950d3d
In-Reply-To: dfea3dfe-b3d2-30a9-aa8c-dbab7dec64b2
Content-Type: text/plain; charset=UTF-8

This isn't really a physics question, so it would probably be more
appropriate to discuss elsewhere, but I'll give it a shot. The problem
with determining how long it will take to mine through an asteroid is
that the cutting power of the laser varies based on the square of the
distance, not just on the power output of the laser.

Unless you take care to maintain a constant distance, there's really no
way to say, and maintaining a constant distance is almost impossible
with a fixed front-mounted laser. Since the asteroid's gravity pulls you
in, you'd need to be oriented facing away from the asteroid, in which
case you couldn't hit the asteroid with your laser.



From: Cosib Renoussog
Subject: Re: Asteroid mine times
To: sci.physics
Message-Id: 470a26f6-f363-5ac9-9964-6730eef686cd
In-Reply-To: a027dbb2-98b1-9c4d-8d87-076f62950d3d
Content-Type: text/plain; charset=UTF-8

> The problem with determining how long it will take to mine through an
> asteroid is that the cutting power of the laser varies based on the
> square of the distance, not just on the power output of the laser.

This is technically true, but really the problem he's asking about is
not knowing how the progress on mining is coming. Most ships can use
their sensors to determine asteroid strength. Try running this instead:

  ship.sensors.target.strength / ship.sensors.target.mass

That will show you how much further you have to go till the target is
destroyed. You probably want to bind it to a key that will display it.



From: Ganiedi Linabur
Subject: Re: Asteroid mine times
To: sci.physics
Message-Id: 7c9aa766-b7a1-146c-a3ad-170ac2907052
In-Reply-To: 470a26f6-f363-5ac9-9964-6730eef686cd
Content-Type: text/plain; charset=UTF-8

> That will show you how much further you have to go till the target is
> destroyed. You probably want to bind it to a key that will display it.

Good point. That's actually much more useful than my original question;
thanks. I made some changes to my HUD config, and now this is visible
there as well:

    {x=-70, y=70, type="text", format="Strength: %0.0f",
     values={function(ship) return ship.sensors.target.strength end}}

That way it shows up clearly under my target indicator.



From: Ditoun Deriesucoum
Subject: Re: Asteroid mine times
To: sci.physics
Message-Id: f0ba9efa-6212-bfaf-93b5-178e222f8460
In-Reply-To: 7c9aa766-b7a1-146c-a3ad-170ac2907052
Content-Type: text/plain; charset=UTF-8

>    {x=-70, y=70, type="text", format="Strength: %0.0f",
>     values={"sensors.target.strength"}}

Not bad, but why not make it a bar readout instead of just text?

    {x=-70, y=70, type="bar", color = {50, 50, 50},
     values = {"sensors.target.strength", "sensors.target.mass"}}

That's a bit more readable at a glance. Also, for a nested lookup in the
ship table you can use a string instead of a function in the values
section; just put the keys separated by dots and it will look them up.
