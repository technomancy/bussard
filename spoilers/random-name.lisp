(defun random* (&optional (mul 1.0)) (* mul (/ (random (expt 2 32)) (expt 2 32))))

(defun normal-random (mean dev)
    (let ((x (+ 1 (sqrt (* -2 (log (random* 1.0))))))
          (y (/ (cos (* 2 pi (random* 1.0))) 2.0)))
      (+ mean (* x y dev))))

(defun choose-from (lst) (nth (random (length lst)) lst))
(defun random-name (&optional name syllables)
  (setf *random-state* (make-random-state t))

  (let* ((common-closed '("b" "c" "d" "f" "g" "h" "k" "l" "m" "n" "p"
                          "r" "s" "t" "w"))
         (uncommon-closed '("x" "z" "q" "v" "v" "j" "j" "gg" "ll" "ss" "tt"))
         (enders '("b" "d" "g" "m" "n" "s" "r" "t"))
         ;; weight towards common letters
         (closed (append common-closed common-closed enders uncommon-closed))
         (vowels '("a" "e" "i" "o" "u" "ie" "ou"))
         (syllables (or syllables (ceiling (normal-random 2.5 1.5))))
         (name (or name (concatenate 'string
                                (string-upcase (choose-from common-closed))
                                (choose-from vowels)
                                (choose-from closed)))))
    (if (< syllables 3)
      (concatenate 'string name (choose-from vowels) (choose-from enders))
      (random-name (concatenate 'string 
                      name (choose-from vowels) (choose-from closed))
                   (- syllables 1)))))

