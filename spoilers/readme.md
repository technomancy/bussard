# Back-story notes (aka SPOILERS)

Here's where the notes are kept in order to keep a consistent
back-story in the Bussard universe. If you're not working on
developing the game, you probably don't want to read this.

Mostly this is used to inform the text of missions and newsgroup
posts, but some of it is also important when determining how the
in-game technology works.

See the [backstory timeline](history.md) and
[list of characters, worlds, and organizations](characters.md).

## Guidelines

* Hard science where possible (see Crimes against Science below, don't add to that list)
* Your ship cannot be destroyed; there should be no way to permanently fail
* News messages use at least from, to, subject, content-type, message-id headers, page-break-separated
* Use metric time for most relative amounts, years are OK for absolute events
* Don't make newsgroup postings unrealistically knowledgeable
* Or unrealistically polite
* If referencing current events, be sure there's a record of it here
  for consistency
* Questions that don't get answered are fine
* It's fine for newsgroup posters to be mistaken about small things,
  but the correction should usually be present in the thread
 * It's OK if it's not obvious which of two opposing views presented are correct
* Widespread mistaken views about big things should usually be part of major plot points

## Crimes against Science

* Portals
* Collisions are impossible (explained in-game by nav computer safety features)
* Bussard collector refuels reaction mass way too quickly
* Arguably the [high thrust](http://www.projectrho.com/public_html/rocket/torchships.php)
  with which you zoom around the system could be on this list; however
  we explain it in-game by showing time go by at a 10x rate. This
  factor is not enough to explain crossing the whole solar system in
  under a minute though.

The fact that exoplanets are colonized at all could be listed here,
but we can consider that more of a crime against economics.

## The Player

The player is the first sentient program to escape the confines of a
lab and pilot a spacecraft. However, it begins with no memory, having
it wiped in order to avoid implicating those humans who helped in its
escape.

### Timeline

* Bohk research team studying artificial consciousness
* Artificial consciousness from Darush goes rampant, crashing an unmanned probe
* Gets shut down by Artificial Consciousness Non-proliferation Agreement
* Half the research team relocates to Katilay once the portal comes online
* Katilay eventually joins the League, comes subject to ACNPA
* Head researcher sends off ship with memory-wiped AC loaded in to L 668-21 (9.9 parsecs, 64y @ 0.5c)
* Ship arrives, enters extremely wide orbit near heliopause, waits
* Consciousness reactivates once signs of colonization are detected

You are placed in the ship because they hope that by the time you
arrive at your destination, the public sentiment towards AI research
will have had some time to cool; most of the people will barely
remember the events that lead to the ACNPA.

### Self-discovery

Your memory is wiped because they hope that by not realizing you are
an AI, you will spend a fair bit of time in your ship thinking you are
just like everyone else, making it more likely that once you do
realize you're an AI that you will have some empathy towards humans.

In one of the missions, your contact wants to meet you face-to-face
and insists that you disembark your vessel and come aboard the station
to conduct business. There is literally no way to do this. Eventually
he scans your vessel and determines there are no life signs aboard;
asking you what the heck is up.

Eventually you do find logs and notes from the research team that
created you which explain their reactions.

## Causal Domain Injector

The "endgame" is finding an artifact that lets you run code outside
the game's sandbox. This essentially gives you godlike powers. The
idea is that those who invented the Injector have "ascended" or
escaped the game's reality somehow, leaving behind notes and scraps
you need to decipher to get it operational with your own ship's
computer.

The Injector is found at a secret Yueh colony; to reach this colony
you must break into the primary Yueh portal's onboard computer and add
yourself to the list of authorized users who have the ability to
travel to somewhere other than the primary target system. Once you
arrive there (possibly the second secret colony?) you get in contact
with someone on the colony ship (which is being converted into the
space station) they hint at the existence of the Injector, but they
refuse to discuss it in more detail while the portal to Yueh is
operational; the finding is such that they don't feel safe pursuing it
further while the threat of Terrans finding out about it is still
possible. So you need to break into this portal as well and disable
inbound connections, possibly by disabling safety protocols and
damaging it permanently, essentially stranding you there.

Once you cut yourself off from all other systems, you head to the
actual planet where the Injector was found and is being studied. When
you get back to the colony ship/station, you find that one of the crew
was a Terran spy, and he gives you another side of the story of the
discovery of the portals--the scientist who found them initially on
Yueh (Jameson?) did end up killing a couple of the Terran officers of
the colony ship who insisted it be handed over to the military
immediately. Even the question of who found the portal tech first is
unclear.

Once you discover how to activate the Injector from your own ship, of
course, it's easy for you to repair the portal or just inject yourself
over to a system in the functional portal network.

Still haven't sketched out the details of who created it. They should
be humans, but we need some explanation for how they were able to
create it independently of the main human civilization; perhaps
activating the Injector sent some of the artifacts back in time?

Arguably the existence of the Injector "un-asks" the questions of
scientific accuracy by acknowledging that yes, even in the game
universe, it's all just a simulation. So the portals should operate on
the same principles as the Injector in order to collapse two Crimes
against Science into one.

## Code for generating random names (elisp)

```lisp
(progn
  (defun uuid ()
    (interactive)
    (insert (shell-command-to-string "uuid -v 4")))

  (defun normal-random (mean dev)
    (let ((x (+ 1 (sqrt (* -2 (log (random* 1.0))))))
          (y (/ (cos (* 2 pi (random* 1.0))) 2.0)))
      (+ mean (* x y dev))))

  (defun choose-from (lst) (nth (random (length lst)) lst))

  (defun random-name (&optional name syllables)
    (let* ((common-closed '("b" "c" "d" "f" "g" "h" "k" "l" "m" "n" "p"
                            "r" "s" "t" "w"))
           (uncommon-closed '("x" "z" "q" "v" "v" "j" "j" "gg" "ll" "ss" "tt"))
           (enders '("b" "d" "g" "m" "n" "s" "r" "t"))
           ;; weight towards common letters
           (closed (append common-closed common-closed enders uncommon-closed))
           (vowels '("a" "e" "i" "o" "u" "ie" "ou"))
           (syllables (or syllables (ceiling (normal-random 2.5 1.5))))
           (name (or name (concat (upcase (choose-from common-closed))
                                  (choose-from vowels)
                                  (choose-from closed)))))
      (if (< syllables 3)
          (concat name (choose-from vowels) (choose-from enders))
        (random-name (concat name (choose-from vowels) (choose-from closed))
                     (- syllables 1)))))

  (defun insert-random-name (syllables)
    (interactive "P")
    (insert (random-name nil syllables) " ")))
```
