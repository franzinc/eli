;;
;; copyright (C) 1987, 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-utils.el,v 1.20 1991/09/16 14:55:10 layer Exp $

;;; Misc utilities

(defun fi::lisp-find-char (char string &optional from-end)
  (let* ((l (length string))
	 (i (if from-end (1- l) 0))
	 (e (if from-end -1 l))
	 (d (if from-end -1 1))
	 (n nil))
    (while (and (not n) (not (= i e)))
      (if (= char (elt string i))
	  (setq n i)
	(setq i (+ i d))))
    n))

(defconst space (string-to-char " "))

(defun fi::listify-string (string)
  "Take a string \"a b c\" and turn it into a list of \"a\" \"b\" and
\"c\".  nil is represented by the null string."
  (let ((res nil) n)
    (while (setq n (fi::lisp-find-char space string t))
      (setq res (cons (substring string (+ n 1)) res))
      (setq string (substring string 0 n)))
    (if (/= 0 (length string))
	(setq res (cons string res)))))

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
		     (if (null (cdr pair))
			 nil
		       (char-to-string (cdr pair)))
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

(defun fi::find-other-end-of-list (&optional arg)
  (if (null arg) (setq arg 1))
  (save-excursion
    (cond ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
	  ((= (following-char) ?\() (scan-sexps (point) arg))
	  ((= (following-char) ?\))
	   (forward-char 1) (scan-sexps (point) (- arg)))
	  (t (error "not on the beginning or end of a list")))))

(defun fi::read-password ()
  (let ((echo-keystrokes 0)
	(result "")
	(xxx nil))
    (while (not (or (= (setq xxx (read-char)) ?\^m)
		    (= xxx ?\n)))
      (setq result (concat result (char-to-string xxx))))
    result))

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

(defun fi::fast-parse-partial-sexp (from to
				    &optional targetdepth stopbefore state
					      result)
  "Fast version of fi::parse-partial-sexp which doesn't cons if sixth arg
is given, which should be a list of length seven.  This requires a hacked
version of parse-partial-sexp.  This function is automagically selected
based on whether calling parse-partial-sexp gives an error when called with
six arguments."
  (if result
      (let ((parse-partial-sexp-result result))
	(parse-partial-sexp from to targetdepth stopbefore state))
    (parse-partial-sexp from to targetdepth stopbefore state)))

(defun fi::slow-parse-partial-sexp (from to
				    &optional targetdepth stopbefore state
					      result)
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

(defun fi::fast-search-string (char string)
  (let ((index 0)
	(max+1 (length string))
	(found nil))
    (while (and (not found) (< index max+1))
      (if (= char (aref string index))
	  (setq found t)
	(setq index (+ index 1))))
    found))

(defun fi:process-running-p (thing)
  (let ((running-states '(run stop open)) temp)
    (cond ((processp thing)
	   (memq (process-status thing) running-states))
	  ((stringp thing)
	   (and (setq temp (get-buffer buffer-name))
		(setq temp (get-buffer-process temp))
		(memq (process-status temp) running-states)))
	  (t nil))))

(defvar fi:filename-frobber-hook
    'fi::discombobulate-automounter-lint
  "*If non-nil, then name of a function which transforms filenames received
from Lisp.  This exists solely for the purpose of removing /tmp_mnt/net
from the beginning of filenames that are on automounted filesystems.")

(defun fi::discombobulate-automounter-lint (name)
  ;; remove /tmp_mnt/net from the beginning of NAME
  (if (string-match "^\\(/tmp_mnt/net\\)?\\(.*\\)$" name)
      (substring name (match-beginning 2) (match-end 2))
    (error "discombobulate-automounter-lint: internal error on %s" name)))

(defun fi::canonicalize-filename (file)
  "If FILE starts with user's home directory, then turn it into a filename
that starts with ~."
  (if (string-match (format "^\\(%s\\)\\(.*\\)" (getenv "HOME"))
		    file)
      (concat "~" (substring file (match-beginning 2) (match-end 2)))
    file))

(defun fi::frob-case-from-lisp (arg)
  (let ((string (if (symbolp arg)
		    (symbol-name arg)
		  arg)))
    (cond ((eq ':upper fi::lisp-case-mode)
	   (downcase string))
	  (t string))))

(defun fi::frob-case-to-lisp (arg)
  (let ((string (if (symbolp arg)
		    (symbol-name arg)
		  arg)))
    (cond ((eq ':upper fi::lisp-case-mode)
	   (upcase string))
	  (t string))))

(defun fi::getf-property (plist property &optional default)
  (while (and plist
	      (not (eq (car plist) property)))
    (setq plist (cddr plist)))
  (if plist 
      (second plist)
    default))

(defun fi::transpose-list (list)
  (let ((l (make-list (length (car list)) nil)))
    (dolist (k list)
      (let ((n 0))
	(dolist (a k)
	  (push a (nth n l))
	  (incf n))))
    l))

(defun fi::insert-file-contents-into-kill-ring (copy-file-name)
  (let ((buffer (generate-new-buffer "*temp*")))
    (save-excursion
      (set-buffer buffer)
      (insert-file copy-file-name)
      (copy-region-as-kill (point-min) (point-max)))
    (kill-buffer buffer)))

(defun fi::member-plist (prop plist)
  (and plist
       (or (eq (car plist) prop)
	   (fi::member-plist prop (cddr plist)))))

(defun fi::string-to-keyword (package)
  (and package (intern (concat ":" package))))

(defun fi::listify (x)
  (and x
       (if (atom x)
	   (list x)
	 x)))

(defun fi::quote-every-other-one (list)
  (and list
       (list* (list 'quote (first list)) (second list)
	      (fi::quote-every-other-one (cddr list)))))

(defun fi:verify-emacs-support ()
  "A function used to test the GNU Emacs in which it is run to see if the
minimum require support for the Emacs-Lisp interface exists."
  (interactive)
  (condition-case condition
      (accept-process-output 1 2)
    (wrong-number-of-arguments
     (fi:error "
accept-process-output does not accept two arguments.  This means that the C
kernel of this version of emacs does not have the necessary modifications
to process.c to run the Franz Inc. emacs-lisp interface.  Please
refer to the installation guide for further information."))
    (wrong-type-argument
     nil))
  (if (not (boundp 'process-environment))
      (fi:error
       "
process-environment is does not have a value.  This means that the C
pre-processor variable MAINTAIN_ENVIRONMENT was defined in some header file
when emacs was built (maybe config.h).  Certain features of the emacs-lisp
interface rely on process-environment being maintained by emacs.  Please
rebuild emacs with MAINTAIN_ENVIRONMENT undefined."))
  (if (interactive-p)
      (message "everything looks fine!")))

(defun fi:error (format-string &rest args)
  (let ((string (apply 'format format-string args)))
    (delete-other-windows)
    (switch-to-buffer "*Help*")
    (erase-buffer)
    (insert string)
    (beginning-of-buffer)
    (beep)
    (signal 'error (list ""))))
