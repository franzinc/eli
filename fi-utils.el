;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and stored only in accordance with the terms of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure by the Government are subject to
;; restrictions of Restricted Rights for Commercial Software developed
;; at private expense as specified in DOD FAR 52.227-7013 (c) (1) (ii).
;;
;; This file may be distributed without further permission from
;; Franz Inc. as long as
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this package, please feel
;; free to contact Franz Inc. at
;;
;;	Franz Inc.
;;	Attn: Emacs Group Manager
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-utils.el,v 1.1 1988/05/12 10:14:26 layer Exp $

;;; Misc utilities

(defun fi::symbol-value-in-buffer (symbol buffer)
  "Return the value of the local binding of SYMBOL in BUFFER, or
nil if non-exists.  Yes, a value of nil and no local value are the same."
  (save-excursion
    ;; the `set-buffer' non-sense is because there is a cache which is only
    ;; updated when a `set-buffer' is done.
    (set-buffer buffer)
    (cdr (assoc symbol (buffer-local-variables buffer)))))

(defun fi::set-in-buffer (symbol value buffer)
  "Set the value of the local binding of SYMBOL to VALUE in BUFFER, or
nil if non-exists.  Yes, a value of nil and no local value are the same."
  (save-excursion
    ;; the `set-buffer' non-sense is because there is a cache which is only
    ;; updated when a `set-buffer' is done.
    (set-buffer buffer)
    (make-local-variable symbol)
    (set symbol value)))

(defun fi::file-name-sans-type (name)
  "Return FILENAME sans file extension or type."
  (substring name 0
 	     (or (string-match "\\.cl$" name)
 		 (string-match "\\.lisp$" name)
 		 (string-match "\\.l$" name)
 		 (length name))))

(defun fi::substitute-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (char-to-string (cdr pair))
		   (char-to-string char)))
	       string
	       nil)))

(defun fi::remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
	       (if (memq char char-list)
		   nil
		 (char-to-string char)))
	     string
	     nil))

(defun fi::process-running (buffer-name)
  (let (temp)
    (and (setq temp (get-buffer buffer-name))
	 (setq temp (get-buffer-process temp))
	 (setq temp (process-status temp))
	 (or (eq 'run temp) (eq 'open temp)))))

(defun fi::find-other-end-of-list (&optional arg)
  (if (null arg) (setq arg 1))
  (save-excursion
    (cond ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
	  ((= (following-char) ?\() (scan-sexps (point) arg))
	  ((= (following-char) ?\))
	   (forward-char 1) (scan-sexps (point) (- arg)))
	  (t (error "not on the beginning or end of a list")))))
