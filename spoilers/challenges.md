# Challenges

The progression is fairly linear up to the point where you get access to the
Spacetime Junction, at which point it branches out with a number of "failed
timelines" that you explore, and then reset back to the Junction point till you
know enough to get to LHS 451 and finish the game.

There will be coding challenges. They can't be too contrived. Few of them should
involve writing from scratch. In real life it's much more common to scrounge
together solutions from existing snippets you find.

We can structure it around progressing from language to language. Obviously you
start with Lua, and the Sol/Tana worlds should all use the Orb OS, which is
written in Lua.

## Decrypt journals (Lua)

This is the first challenge; you write the solution in Lua. Nari gives you a
fair bit of hints if you go for too long without solving it.

It confirms that you are a machine consciousness, introduces the character of
Traxus, and indicates that you are connected to him.

## Connect to Subnet (Lua)

This opens up a new discussion forum and allows you to accept missions posted to
subnet. It involves either writing a bencode decoder or purchasing an upgrade
that can do it, which would require some cargo trading or mining in order to get
enough cash for the upgrade. The code to sync from subnet's API to your mail
client can be found on a posting to the "soc_tana" subnet group.

You can read subnet threads without doing bencode, but in order to sync to your
mail client you need a decoder. You could accept missions manually without
syncing to your mail client if you call the `reply` function directly with the
message-id header you find from dumping the subnet data directly to the
console. (Easiest solution; most hacky.)

## Get off travel blacklist (SQL)

## Mess with portal code (Lisp)

## Control derelict spaceship (Lily)

## Light curve analysis (Any language?)

## Reactivate mining robot (Forth)

## Add password logger to compromised account (Moonscript?)

## Password dictionary attack (???)

## Buffer overflow attack (???)

## Leave a rover on a planet in order to impersonate and MITM

## Convince a portal that you have a human on board in order to allow passage
