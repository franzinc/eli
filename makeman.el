
(require 'cl)

(defvar fi::clman-big-oblist)

(do ((ll fi::clman-big-oblist (cdr ll))
     xx)
    ((null ll) (kill-emacs 0))
  (setq xx (car ll))
  (unless (string-match ".*:" (car xx))
    (message "%s" (car xx))
    (dolist (x (cdr xx))
      (message "%s" x))
    (message "")))
