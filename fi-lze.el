;; Copyright (c) 1987-1991 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-lze.el,v 1.26 1992/07/20 10:09:47 layer Exp $
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
  (and nil ;; turn off this feature for now...
       fi::show-compilation-status
       (eq (fi::connection-process fi::*connection*)
	   fi::show-compilation-status)
       (error "A background eval/compile request is pending, please wait...")))

(defun fi:reset-background-request ()
  (setq fi::show-compilation-status nil))

;;;;;;;;;;;;;

(defvar fi:lisp-evals-always-compile nil
  "*This variable controls whether or not the fi:lisp-eval-or-compile-*
functions will compile or evaluated when those functions are not given a
prefix argument.  If non-nil, then compilation is the default, otherwise
evaluation is the default.  When one is the default, the other
functionality can be invoked by using a prefix argument.")

(defun fi:lisp-eval-or-compile-defun (compilep)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess associated with this buffer.  A `top-level' form is one that
starts in column 1.  With a prefix argument, the source sent to the
subprocess is compiled."
  (interactive "P")
  (if (or (and fi:lisp-evals-always-compile (null compilep))
	  (and (null fi:lisp-evals-always-compile) compilep))
      (fi:lisp-compile-defun)
    (fi:lisp-eval-defun)))

(defun fi:lisp-eval-or-compile-region (compilep)
  "Send the text in the region to the Lisp subprocess associated with this
buffer, one expression at a time if there is more than one complete
expression.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (if (or (and fi:lisp-evals-always-compile (null compilep))
	  (and (null fi:lisp-evals-always-compile) compilep))
      (fi:lisp-compile-region)
    (fi:lisp-eval-region)))

(defun fi:lisp-eval-or-compile-last-sexp (compilep)
  "Send the sexp before the point to the Lisp subprocess associated with
this buffer.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (if (or (and fi:lisp-evals-always-compile (null compilep))
	  (and (null fi:lisp-evals-always-compile) compilep))
      (fi:lisp-compile-last-sexp)
    (fi:lisp-eval-last-sexp)))

(defun fi:lisp-eval-or-compile-current-buffer (compilep)
  "Send the entire buffer to the Lisp subprocess associated with this
buffer."
  (interactive "P")
  (if (or (and fi:lisp-evals-always-compile (null compilep))
	  (and (null fi:lisp-evals-always-compile) compilep))
      (fi:lisp-compile-current-buffer)
    (fi:lisp-eval-current-buffer)))

;;;;;;;;;;

(defun fi:lisp-eval-defun ()
  "Send for evaluation the current top-level (or nearest previous) form to
the Lisp subprocess associated with this buffer.  A `top-level' form is one
that starts in column 1."
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi::eval-region-internal start end nil)))

(defun fi:lisp-compile-defun ()
  "Send for compilation the current top-level (or nearest previous) form to
the Lisp subprocess associated with this buffer.  A `top-level' form is one
that starts in column 1."
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi::eval-region-internal start end t)))

(defun fi:lisp-eval-region ()
  "Send for evaluation the region to the Lisp subprocess associated with
this buffer."
  (interactive)
  (fi::eval-region-internal (min (point) (mark))
			    (max (point) (mark))
			    nil))

(defun fi:lisp-compile-region ()
  "Send for compilation the region to the Lisp subprocess associated with
this buffer."
  (interactive)
  (fi::eval-region-internal (min (point) (mark))
			    (max (point) (mark))
			    t))

(defun fi:lisp-eval-last-sexp ()
  "Send for evaluation the sexp before the point to the Lisp subprocess
associated with this buffer."
  (interactive)
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-region-internal start (point) nil)))

(defun fi:lisp-compile-last-sexp ()
  "Send for compilation the sexp before the point to the Lisp subprocess
associated with this buffer."
  (interactive)
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-region-internal start (point) t)))

(defun fi:lisp-eval-current-buffer ()
  "Send for evaluation the entire buffer to the Lisp subprocess associated
with this buffer."
  (interactive)
  (fi::eval-region-internal (point-min) (point-max) nil t))

(defun fi:lisp-compile-current-buffer ()
  "Send for compilation the entire buffer to the Lisp subprocess associated
with this buffer."
  (interactive)
  (fi::eval-region-internal (point-min) (point-max) t t))

;;;;;;;;;;;;;

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
	(save-excursion
	  (set-buffer buffer)
	  (fi::note-background-reply (list compilep))
	  (when results
	    (fi:show-some-text nil results)))
	(when fi:pop-to-sublisp-buffer-after-lisp-eval ; bug2683
	  (pop-to-buffer fi:common-lisp-buffer-name)
	  (goto-char (point-max))))
      ((buffer compilep) (error)
	(save-excursion
	  (set-buffer buffer)
	  (fi::note-background-reply (list compilep))
	  (message "Error during %s: %s"
		   (if compilep "compile" "eval")
		   error)))
      ignore-package)))
