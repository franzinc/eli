;; See the file LICENSE for the full license governing this code.

;; Emacs 18 back compatibility hacks for the Franz Inc. emacs-lisp interface

(defun fi::ensure-buffer-visible (buffer)
  nil)

(defun fi::ensure-minibuffer-visible ()
  nil)

(defun fi::defontify-string (str)
  str)

(defun set-menubar-dirty-flag ()
  nil)
