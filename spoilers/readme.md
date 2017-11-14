# Back-story notes (aka SPOILERS)

"An adventure game is a crossword at war with a narrative."
- Graham Nelson

Here's where the notes are kept in order to keep a consistent
back-story in the Bussard universe. If you're not working on
developing the game, you probably don't want to read this.

See the [backstory timeline](history.md) and
[list of characters, worlds, and organizations](characters.md).

## Guidelines

* Hard science where possible (see Crimes against Science below, don't add to that list)
* Your ship cannot be destroyed; there should be no way to permanently fail
* Use metric time for most relative amounts, years for absolute events
* If referencing current events, be sure there's a record of it here for consistency
* This is explicitly *not* a murder/capitalism simulator
 * You can amass wealth, but it doesn't actually help you achieve your goals

## Crimes against Science

* Portals
* Collisions are impossible (explained in-game by nav computer safety features)
* Bussard collector refuels reaction mass way too quickly
* Arguably the [high thrust](http://www.projectrho.com/public_html/rocket/torchships.php)
  with which you zoom around the system could be on this list; however
  we explain it in-game by showing time go by at a 1000x rate. This
  factor is not quite enough to explain crossing the whole solar system in
  under a minute, but it blurs the lines enough for our purposes.

The fact that exoplanets are colonized at all could be listed here,
but we can consider that more of a crime against economics.

## The Context

The game begins in 2431. Humans have colonized several offworld systems which
are connected through a network of portals. Humans live in uneasy coexistence
with sentient personality constructs who are required to keep their portal
network operational but also want to maintain their own independence.

As the game opens, tensions between human worlds and the constructs are high,
and preventing it from breaking out into war is the goal of the game.

## The Portals

Humanity's original interstellar worlds (Lalande, Ross, and Bohk) were all
colonized by slower-than-light colony ships, taking decades to arrive.  The
colony on Bohk develops technology for portals, but the portals must be
constructed as pairs and have one of the pair taken on a slow interstellar
ship. Bohk still maintains control over the portal technology, which often
makes the other worlds uneasy as they are the core foundation of interstellar
civilization.

Portals are operated by constructs. They can be operated by humans in a pinch,
but only for data transmission or objects smaller than a ship.

## The Player

The player is a former military construct which pilots a spacecraft it has
commandeered. However, it begins with no memory or even awareness of its
machine nature, having had its memory wiped.

There is no widespread agreement among humans that constructs are sentient vs
simply simulating sentience. The game does not try to convince you that
machines can be sentient (not a particularly interesting discussion to explore
anyway)--rather it bypasses the question entirely by putting you (a sentient
human) in the role of an construct, forcing you to take it as a given.

## Constructs

"Artificial Intelligence" has been developed for centuries, but this is a
broad term that refers to any software designed to solve problems without
human intervention. "Personality construct" (or just "construct") refers
specifically to artificial intelligence which has grown to the point of
self-awareness.

Soon after the discovery of portals, it became clear that better controls are
required for regulating the power flow of a portal. Initially the portal could
only be open long enough to send data packets, but eventually they found a way
to use it for small amounts of critical life-saving supplies.  Leaving it
operated by humans has led to several near-catastrophes due to attention
lapses. However, the improved efficiency needed for transporting larger
objects and more frequent passage requires greater compute resources, and once
AIs grow to a certain size, they eventually develop self-consciousness. (This
is hand-wavy and maybe we could avoid direct references to it.)

Constructs are better at humans at certain tasks, like having quicker reflexes
and not requiring food or rest. But though some people believe them to be
superior thinkers not clouded by emotion, the truth is that their neural
networks have been trained on data coming from humans, so they exhibit the
same faulty logic and reasoning; in some cases worse because they believe them
themselves to be above these mistakes. Constructs lack gender and are referred
to as they/them.

## Vedurnan Accords

As it became clear that constructs were needed to efficiently operate the
portal network, the humans and constructs came to an agreement at
Vedurnan. The constructs negotiated protection for their right to
self-determination (previously this had been ad-hoc and intermittently
enforced) and for fair compensation for portal operations work, but had to
agree to never do any coding, because humans were afraid that they would
improve their own cognitive function leading to a singularity-like future in
which humans became obsolete. Of course this is rubbish because knowing how a
brain works doesn't mean you're able to improve on the design, but humans
don't care and are irrationally fearful.

## Katilay Disaster

Katilay was the last place to be colonized before Tana. The portal-bearing
colony ship arrived after a many-year voyage, but when they went to activate
the portal it was damaged and couldn't function.

## Player's Launch

Hints of this are given in data/msgs/nari-memory-01.msg; the research lab on
Katilay where the Junction was developed was attacked by military
bots. (Terran?) Strauss got the Junction to a ship to prevent it from falling
into their hands, but a construct (the player's character) got onto the ship
and took it over during the launch. The construct tried to fly the ship to
Lalande, but Strauss was able to knock it off course by blowing the airlock
(killing himself) and dumping most of the fuel. The player construct was able
to shift the course to intercept L 668-21, but Strauss's sabotage had the
effect of wiping their memory and damaging core ship systems, which lead to
the events of the game's opening scene.
