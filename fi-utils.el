;; Copyright (c) 1987-1991 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-utils.el,v 1.29 1991/11/01 00:12:22 layer Exp $

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

(defun fi:map-lines (function &rest args)
  "Apply FUNCTION to ARGS once for every line in buffer, with point always
at the beginning of the line."
  (beginning-of-buffer)
  (while (and (= 0 (progn (beginning-of-line) (forward-line 1)))
	      (not (eobp)))
    (apply function args)))

;; This is a pretty bad hack but it appears that within completing-read
;; fi:package has the wrong value so we bind this variable to get around
;; the problem.
(defvar fi::original-package nil)

(defun fi::get-default-symbol (prompt &optional up-p ignore-keywords)
  (let* ((symbol-at-point (fi::get-symbol-at-point up-p))
	 (read-symbol
	  (let ((fi::original-package fi:package))
	    (if (fboundp 'epoch::mapraised-screen)
		(epoch::mapraised-screen (minibuf-screen)))
	    (completing-read
	     (if symbol-at-point
		 (format "%s: (default %s) " prompt symbol-at-point)
	       (format "%s: " prompt))
	     'fi::minibuffer-complete))))
    (list (if (string= read-symbol "")
	      symbol-at-point
	    read-symbol))))

(defun fi::minibuffer-complete (pattern predicate what)

  ;; HACK HACK HACK HACK
  ;;   ignore-keywords must be bound in the dynamic context in which this
  ;;   function is called (just above in fi::get-default-symbol).
  ;; HACK HACK HACK HACK
  
  (let ((fi:package fi::original-package))
    (let (package deletion)
      (if (string-match ":?:" pattern)
	  (setq package
	    (concat
	     ":" (substring pattern 0 (match-beginning 0)))
	    deletion (substring pattern 0 (match-end 0))
	    pattern
	    (substring pattern (match-end 0))))
      (let* ((alist (fi::lisp-complete-1 pattern package nil ignore-keywords))
	     (completion (and alist (try-completion pattern alist))))
	(ecase what
	  ((nil) (cond ((eq completion t) t)
		       ((and (null completion) (null alist))
			;; no match
			nil)
		       ((and (null completion) alist (null (cdr alist)))
			;; one match for abbrev
			(cdr (car alist)))
		       ((and (null completion) alist)
			;; more than one match for abbrev
			(fi::abbrev-to-symbol pattern alist))
		       ((null (string= pattern completion))
			;; we can complete further than pattern
			(let ((new (cdr (assoc completion alist))))
			  (if new
			      new
			    completion)))
		       ((and alist (null (cdr alist)))
			;; one match
			(if (string-match "::" (cdr (car alist)))
			    (cdr (car alist))
			  (concat deletion (cdr (car alist)))))
		       (t
			;; more than one match, just return completion,
			;; with possible package prefix
			(if (and completion (string-match "::" completion))
			    completion
			  (concat deletion completion)))))
	  ((t) (mapcar (function cdr) alist))
	  (lambda (eq completion t)))))))

(defun fi::get-symbol-at-point (&optional up-p)
  (let ((symbol
	 (cond
	  ((looking-at "\\sw\\|\\s_")
	   (while (looking-at "\\sw\\|\\s_")
	     (forward-char 1))
	   (buffer-substring
	    (point)
	    (progn (forward-sexp -1)
		   (while (looking-at "\\s'")
		     (forward-char 1))
		   (point))))
	  (t
	   (condition-case ()
	       (save-excursion
		 (if up-p
		     (let ((opoint (point)))
		       (cond ((= (following-char) ?\()
			      (forward-char 1))
			     ((= (preceding-char) ?\))
			      (forward-char -1)))
		       (up-list -1)
		       (forward-char 1)
		       (if (looking-at "def")
			   (goto-char opoint)
			 (if (looking-at "funcall\\|apply")
			     (progn
			       (forward-sexp 2)
			       (backward-sexp 1)
			       (if (looking-at "#'")
				   (forward-char 2)
				 (if (looking-at "(function")
				     (progn
				       (forward-char 1)
				       (forward-sexp 2)
				       (backward-sexp 1)))))))))
		 (while (looking-at "\\sw\\|\\s_")
		   (forward-char 1))
		 (if (re-search-backward "\\sw\\|\\s_" nil t)
		     (progn (forward-char 1)
			    (buffer-substring
			     (point)
			     (progn (forward-sexp -1)
				    (while (looking-at "\\s'")
				      (forward-char 1))
				    (point))))
		   nil))
	     (error nil))))))
    (or symbol
	(if (and up-p (null symbol))
	    (fi::get-symbol-at-point)))))

(defun fi::abbrev-to-symbol (pattern alist)
  (let* ((suffix (and (string-match ".*-\\(.*\\)" pattern)
		      (substring pattern
				 (match-beginning 1)
				 (match-end 1))))
	 (nwords
	  (let ((n 0) (i 0) (max (length pattern)))
	    (while (< i max)
	      (if (= ?- (aref pattern i))
		  (setq n (+ n 1)))
	      (setq i (+ i 1)))
	    n))
	 (n 0)
	 (words nil)
	 abbrev-word
	 expanded-word
	 xx)
    (while (< n nwords)
      (setq abbrev-word (fi::word pattern n))
      (setq xx
	(mapcar (function (lambda (x) (fi::word (car x) n)))
		alist))
      (setq expanded-word (car xx))
      (if (let (done)
	    (while (and (not done) xx)
	      (if (and (cdr xx)
		       (not (string= (car xx) (car (cdr xx)))))
		  (setq done t))
	      (setq xx (cdr xx)))
	    done)
	  ;; words aren't the same
	  (setq words (cons abbrev-word words))
	(setq words (cons expanded-word words)))
      (setq n (+ n 1)))
    
    (format "%s-%s" (mapconcat 'identity (nreverse words) "-") suffix)))

(defun fi::word (string word)
  ;; (fi::word "foo-bar-baz" 0) returns "foo"
  ;; (fi::word "foo-bar-baz" 1) returns "bar"
  ;; (fi::word "foo-bar-baz" 2) returns "baz"
  ;; (fi::word "foo-bar-baz" 3) returns nil
  (let* ((n 0)
	 (i 0)
	 (res nil)
	 (max (length string))
	 c
	 done)
    (while (and (not done) (< n word))
      (if (= i max)
	  (setq done t)
	(if (= ?- (aref string i))
	    (setq n (+ n 1)))
	(setq i (+ i 1))))
    
    (while (and (< i max)
		(not (= ?- (setq c (aref string i)))))
      (setq res (concat res (char-to-string c)))
      (setq i (+ i 1)))

    res))

;;;;

(defun fi::display-pop-up-window (buffer &optional hook args)
  (fi:lisp-push-window-configuration)
  (cond ((eq 'split (car fi:pop-up-temp-window-behavior))
	 (fi::display-pop-up-window-split buffer hook args))
	((eq 'other (car fi:pop-up-temp-window-behavior))
	 (fi::display-pop-up-window-other buffer hook args))
	((eq 'replace (car fi:pop-up-temp-window-behavior))
	 (fi::display-pop-up-window-replace buffer hook args))
	(t (error "bad value for car of fi:pop-up-temp-window-behavior: %s"
		  (car fi:pop-up-temp-window-behavior)))))

(defun fi::display-pop-up-window-replace (buffer hook args)
  (switch-to-buffer buffer)
  (when hook (apply hook args)))

(defun fi::display-pop-up-window-other (buffer hook args)
  (cond ((one-window-p)
	 (split-window)
	 (other-window 1)
	 (switch-to-buffer buffer))
	((eq (current-buffer) buffer))
	(t
	 (other-window 1)
	 (switch-to-buffer buffer)))

  (when hook (apply hook args))
    
  (bury-buffer buffer)
  (other-window 1))

(defun fi::display-pop-up-window-split (buffer hook args)
  (let* ((from-window (selected-window))
	 (real-from-window nil)
	 (from-window-orig-height (1- (window-height))) ; minus mode line
	 (buffer-window (get-buffer-window buffer))
	 (lines nil))
    
    (save-excursion
      (set-buffer buffer)
      (setq lines (count-lines (point-min) (point-max))))

    ;; get to the proper window
    (cond (buffer-window
	   (when (not (eq (selected-window) buffer-window))
	     (select-window buffer-window)))
	  ((eq (current-buffer) buffer))
	  ((one-window-p)
	   (setq from-window-orig-height (1- (window-height)))
	   (split-window)
	   (save-window-excursion
	     (other-window 1)
	     (setq from-window (selected-window)))
	   (switch-to-buffer buffer))
	  (t
	   (setq real-from-window (selected-window))
	   (select-window (get-largest-window))
	   (if (eq real-from-window (selected-window))
	       (setq real-from-window nil))
	   (setq from-window-orig-height (1- (window-height)))
	   (split-window)
	   (save-window-excursion
	     (other-window 1)
	     (setq from-window (selected-window)))
	   (switch-to-buffer buffer)))

    (unless (one-window-p)
      (let* ((window-min-height 2)
	     (target-size
	      (max window-min-height
		   (min lines (/ from-window-orig-height 2)))))
	(if (< target-size (window-height))
	    (shrink-window (- (window-height) target-size 1))
	  (if (> target-size (window-height))
	      (enlarge-window (- target-size (window-height) -1))))))
    
    (when hook (apply hook args))
    
    (bury-buffer buffer)
    (select-window (or real-from-window from-window))))

(defvar fi::shell-buffer-for-common-lisp-interaction-host-name nil)

(defun fi::setup-shell-buffer-for-common-lisp-interaction (process)
  "Internal function use to start an emacs-lisp interface in a buffer not
created by fi:common-lisp."
  (interactive (list (get-buffer-process (current-buffer))))
  (when (fi::lep-open-connection-p)
    (error "an emacs-lisp interface is already running in this emacs."))
  (save-excursion
    (set-buffer (process-buffer process))
    (unless process
      (error "current buffer doesn't have a process associated with it"))
    (setq fi::common-lisp-backdoor-main-process-name
      (setq fi::process-name (buffer-name (current-buffer))))
    (setq fi::lisp-host
      (or fi::shell-buffer-for-common-lisp-interaction-host-name
	  (setq fi::shell-buffer-for-common-lisp-interaction-host-name
	    (read-string "host on which lisp is running: "))))
    (set-process-filter process 'fi::common-lisp-subprocess-filter)))
