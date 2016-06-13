;;; bussard.el --- Interface for Bussard game

;;; status buffer
;; x,y, time, credits, current system
;; target: name, distance, mass, type
;; targets: name, distance, type

;;; repl buffer

(require 'monroe)

(defvar bussard-status-message "x,y: %s,%s speed: %s
credits: %s    epoch: %s ")

(defvar bussard-status-buffer-name "*bussard-status*")

(defun bussard-insert-target (target)
  (let ((name (car target))
        (type (cadr target))
        (distance (caddr target))
        (selected (cadddr target)))
    (let ((target-string (format "%s (%s): %s\n" name type distance)))
      (when selected
        (put-text-property 0 (length target-string) 'face 'bold target-string))
      (insert target-string))))

(defun bussard-status-handler (msg)
  (monroe-dbind-response msg (x y speed credits time fuel battery targets)
    (with-current-buffer (get-buffer-create bussard-status-buffer-name)
      (delete-region (point-min) (point-max))
      (insert (format bussard-status-message x y speed credits time))
      (insert "\n\n")
      (dolist (ta targets)
        (bussard-insert-target ta)))))

(defun bussard ()
  (interactive)
  (delete-other-windows)
  (monroe "localhost:7888")
  (split-window-below)
  (switch-to-buffer bussard-status-buffer-name)
  (other-window 1))

(puthash "bussard/status" 'bussard-status-handler monroe-custom-handlers)
