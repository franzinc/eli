;; Copyright (c) 1987-1991 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-lze.el,v 1.17 1991/10/10 18:53:15 layer Exp $
;;
;; Code the implements evaluation in via the backdoor

(make-variable-buffer-local 'fi::show-compilation-status)
(setq fi::show-compilation-status nil)

(defvar fi::mode-line-note-for-compile " COMPILING")
(defvar fi::mode-line-note-for-eval " EVALUATING")

(defun fi::note-background-request (compiling)
  (let ((message1 (if compiling "Compiling" "Evaluating"))
	(message (if compiling
		     fi::mode-line-note-for-compile
		   fi::mode-line-note-for-eval)))
    (message "%s..." message1)
    (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
      (setq layer item)
      (or (and item (not (string= "" (car (cdr item)))))
	  (or (and item (rplacd item (list message)))
	      (setq minor-mode-alist
		(cons (list 'fi::show-compilation-status message)
		      minor-mode-alist)))))
    (setq fi::show-compilation-status
      ;; this is so we can tell when lisp has died and been restarted:
      (fi::connection-process fi::*connection*))))

(defun fi::note-background-reply (&optional compiling)
  (let ((message (when compiling
		   (if (car compiling) "Compiling" "Evaluating"))))
    (if compiling (message "%s...done." message))
    (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
      (and item (rplacd item (list ""))))
    (setq fi::show-compilation-status nil)))

(defun fi::error-if-request-in-progress ()
  (and fi::show-compilation-status
       (eq (fi::connection-process fi::*connection*)
	   fi::show-compilation-status)
       (error "A background eval/compile request is pending, please wait...")))

(defun fi::eval-region-internal (start end compilep &optional ignore-package)
  (fi::error-if-request-in-progress)
  (fi::note-background-request compilep)
  (let ((buffer (current-buffer)))
    (fi::make-request
     (lep::evaluation-request
      :text (buffer-substring start end)
      :echo fi:echo-evals-from-buffer-in-listener-p
      :partialp (not (and (eq (max start end) (point-max))
			  (eq (min start end) (point-min))))
      :pathname (buffer-file-name)
      :compilep (if compilep t nil))
     ((buffer compilep) (results)
      (let ((save-buffer (current-buffer)))
	(set-buffer buffer)
	(if results
	    (fi:show-some-text nil results)
	  (fi::note-background-reply (list compilep)))
	(if (not (eq save-buffer buffer))
	    (set-buffer save-buffer))))
     (() (error)
      (message "Error occurred during evaluation: %s" error))
     ignore-package)))

(defun fi:lisp-eval-defun (compilep)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess associated with this buffer.  A `top-level' form is one that
starts in column 1.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi::eval-region-internal start end compilep)))

(defun fi:lisp-eval-region (compilep)
  "Send the text in the region to the Lisp subprocess associated with this
buffer, one expression at a time if there is more than one complete
expression.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (fi::eval-region-internal (min (point) (mark))
			     (max (point) (mark))
			     compilep))

(defun fi:lisp-eval-last-sexp (compilep)
  "Send the sexp before the point to the Lisp subprocess associated with
this buffer.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-region-internal start (point)
			       compilep)))

(defun fi:lisp-eval-current-buffer (compilep)
  "Send the entire buffer to the Lisp subprocess associated with this
buffer.  If a Lisp subprocess has not been started, then one is started.
With a prefix argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-region-internal (point-min) (point-max) compilep t))
