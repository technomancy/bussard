# Backstory notes (aka SPOILERS)

Here's where the notes are kept in order to keep a consistent
backstory in the Bussard universe.

If you're not working on developing the game, you probably don't want
to read this; it's basically just pure spoilers.

Mostly this is used to inform the text of missions and newsgroup
posts, but some of it is also important when determining how the
in-game technology works.

## Guidelines

* Hard science where possible
 * Obviously portals break this, as does the dramatic reduction of solar system distance
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
