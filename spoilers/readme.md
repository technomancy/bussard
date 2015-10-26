# Back-story notes (aka SPOILERS)

Here's where the notes are kept in order to keep a consistent
back-story in the Bussard universe. If you're not working on
developing the game, you probably don't want to read this.

Mostly this is used to inform the text of missions and newsgroup
posts, but some of it is also important when determining how the
in-game technology works.

## Guidelines

* Hard science where possible (see Crimes against Science below, don't add to that list)
* News messages use at least from, to, subject, message-id headers, page-break-separated
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
* Bussard collector refuels reaction mass way too quickly
* Arguably the [high thrust](http://www.projectrho.com/public_html/rocket/torchships.php)
  with which you zoom around the system could be on this list; however
  we explain it in-game by showing time go by at a 10x rate. This
  factor is not enough to explain crossing the whole solar system in
  under a minute though.

The fact that exoplanets are colonized at all could be listed here,
but we can consider that more of a crime against economics.

### Causal Domain Injector

The "endgame" is finding an artifact that lets you run code outside
the game's sandbox. This essentially gives you godlike powers. The
idea is that those who invented the Injector have "ascended" or
escaped the game's reality somehow, leaving behind notes and scraps
you need to decipher to get it operational with your own ship's
computer.

Still haven't sketched out the details of who created it. They should
be humans, but we need some explanation for how they were able to
create it independently of the main human civilization; perhaps
activating the Injector sent some of the artifacts back in time?

Arguably the existence of the Injector "un-asks" the questions of
scientific accuracy by acknowledging that yes, even in the game
universe, it's all just a simulation. So the portals should operate on
the same principles as the Injector.

## Code for generating random names (elisp)

```lisp
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
  (insert (random-name nil syllables) " "))
```