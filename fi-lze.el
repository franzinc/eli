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
;; $Header: /repo/cvs.copy/eli/fi-lze.el,v 1.3 1991/02/13 10:20:43 layer Exp $
;;
;; Code the implements evaluation in via the backdoor

(defun lep::eval-region-internal (start end compilep)
  "Evaluate the region using the backdoor, print the results of the
minibuffer, and create a listener if there is any output (which might be
due to an error)."
  (interactive "r\np")
  (message "Evaluating...")
  (make-request (lep::evaluation-request
		 :text (buffer-substring start end)
		 :partialp (not 
			    (and (eq (max start end) (point-max))
				 (eq (min start end) (point-min))))
		 :pathname (buffer-file-name)
		 :compilep (if compilep t nil))
		(() (results)
		 (when results
		   (show-some-short-text results)))
		(() (error)
		 (message "Error occurred during evaluation: %s" error))))

(defun fi:lisp-eval-defun (compilep)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess associated with this buffer.  A `top-level' form is one that
starts in column 1.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (lep::eval-region-internal start end compilep)))

(defun fi:lisp-eval-region (compilep)
  "Send the text in the region to the Lisp subprocess associated with this
buffer, one expression at a time if there is more than one complete
expression.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (lep::eval-region-internal (min (point) (mark))
			     (max (point) (mark))
			     compilep))
