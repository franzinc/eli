;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
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
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-utils.el,v 1.4 1989/05/19 11:43:16 layer Exp $

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

(defun fi::find-path (file)
  "Find FILE in load-path, return the full pathname."
  (let ((p load-path)
	(done nil) res)
    (while (and (not done) p)
      (if (file-exists-p (setq res (concat (car p) "/" file)))
	  (setq done t)
	(setq res nil))
      (setq p (cdr p)))
    res))

(defun fi::fast-parse-partial-sexp (from to targetdepth stopbefore state
				    &optional result)
  "Fast version of fi::parse-partial-sexp which doesn't cons if sixth arg
is given, which should be a list of length seven.  This requires a hacked
version of parse-partial-sexp.  This function is automagically selected
based on whether calling parse-partial-sexp gives an error when called with
six arguments."
  (if result
      (let ((parse-partial-sexp-result result))
	(parse-partial-sexp from to targetdepth stopbefore state))
    (parse-partial-sexp from to targetdepth stopbefore state)))

(defun fi::slow-parse-partial-sexp (from to targetdepth stopbefore state
				    &optional result)
  "Slow version of fi::parse-partial-sexp which conses like mad, no matter
what the optional sixth argument is.  This is used if parse-partial-sexp
hasn't been hacked.  This function is automagically selected based on
whether calling parse-partial-sexp gives an error when called with six
arguments."
  (if result
      (let ((res result)
	    (xx (parse-partial-sexp from to targetdepth stopbefore state)))
	(while res
	  (setcar res (car xx))
	  (setq xx (cdr xx))
	  (setq res (cdr res)))
	result)
    (parse-partial-sexp from to targetdepth stopbefore state)))

(if (boundp 'parse-partial-sexp-result)
    (fset 'fi::parse-partial-sexp
	  (symbol-function 'fi::fast-parse-partial-sexp))
  (fset 'fi::parse-partial-sexp
	(symbol-function 'fi::slow-parse-partial-sexp)))
