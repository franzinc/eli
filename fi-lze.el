;;
;; copyright (C) 1991 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are the property 
;; of Franz, Inc.  
;;
;; This file (or any derivation of it) may be distributed without 
;; further permission from Franz Inc. as long as:
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, other than a tape
;;	  fee, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this interface, please feel
;; free to contact Franz Inc. at
;;	Franz Inc.
;;	Attn: Kevin Layer
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;;	(415) 548-3600
;; or
;;	emacs-info@franz.com
;;	uunet!franz!emacs-info
;;
;; $Header: /repo/cvs.copy/eli/fi-lze.el,v 1.13 1991/09/16 14:54:53 layer Exp $
;;
;; Code the implements evaluation in via the backdoor

(make-variable-buffer-local 'fi::show-compilation-status)
(setq fi::show-compilation-status nil)

(defun fi::note-background-request (message)
  (message "%s..." message)
  (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
    (setq layer item)
    (or (and item (not (string= "" (car (cdr item)))))
	(or (and item (rplacd item (list (concat " " message))))
	    (setq minor-mode-alist
	      (cons (list 'fi::show-compilation-status message)
		    minor-mode-alist)))))
  (setq fi::show-compilation-status t))

(defun fi::note-background-reply (&optional message)
  (if message (message "%s...done." message))
  (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
    (and item (rplacd item (list ""))))
  (setq fi::show-compilation-status nil))

(defun fi::error-if-request-in-progress ()
  (and fi::show-compilation-status
       (error "A background eval/compile request is pending, please wait...")))

(defun fi::eval-region-internal (start end compilep &optional ignore-package)
  (fi::error-if-request-in-progress)
  (fi::note-background-request (if compilep "Compiling" "Evaluating"))
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
	  (fi::note-background-reply (if compilep "Compiling" "Evaluating")))
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
