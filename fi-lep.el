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
;; $Header: /repo/cvs.copy/eli/fi-lep.el,v 1.32 1991/06/27 15:25:34 layer Exp $
;;

(defvar fi:always-in-a-window nil)

(defun fi:show-some-text (package text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let ((n (string-match "\n$" text)))
    (when n (setq text (substring text 0 n))))
  (fi:lisp-push-window-configuration)
  (if fi:always-in-a-window
      (fi::show-some-text-1 text (or package fi:package))
    (let* ((window (minibuffer-window))
	   (height (1- (window-height window)))
	   (width (window-width window))
	   (text-try
	    (cond (fi:package (format "[package: %s] %s" fi:package text))
		  (t text)))
	   (lines/len (fi::frob-string text-try)))
      (if (and (< (car lines/len) 2)
	       (<= (second lines/len) width))
	  (message "%s" text-try)
	(fi::show-some-text-1
	 (cond (fi:package (format "[package: %s]\n%s" fi:package text))
	       (t text))
	 (or package fi:package))))))

(defun fi::show-some-text-1 (text package &optional hook &rest args)
  "Display TEXT in a temporary buffer putting that buffer setting that
buffers package to the package of PACKAGE."
  (let* ((from-window (selected-window))
	 (real-from-window nil)
	 (from-window-orig-height (1- (window-height))) ; minus mode line
	 (buffer (get-buffer-create "*CL-temp*"))
	 (buffer-window (get-buffer-window buffer))
	 (lines nil))
    
    ;; fill the buffer
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (fi:common-lisp-mode)
      (setq fi:package package)
      (insert text)
      (beginning-of-buffer)
      (setq lines (count-lines (point-min) (point-max))))

    ;; get to the proper window
    ;;
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

(defun fi::frob-string (text)
  (let ((start 0)
	(lines 0)
	(length (length text))
	(max-length 0)
	last
	m)
    (while (and (setq m (string-match "$" text start))
		(< m length))
      (setq last m)
      (let ((len (- m start)))
	(if (> len max-length) (setq max-length len)))
      (setq lines (+ lines 1)
	    start (1+ m)))  
    (if (not (eq m last)) (setq lines (1+ lines)))
    (let ((len (- length start)))
      (if (> len max-length) (setq max-length len)))
    (list lines max-length)))
    
	
;;;; Implementation of arglist

(defun fi:lisp-arglist (string)
  "Dynamically determine, in the Common Lisp environment, the arglist for
STRING.  fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Arglist for"))
  (make-request (lep::arglist-session :fspec string)
		;; Normal continuation
		(() (what arglist)
		 (fi:show-some-text nil
				    "%s's arglist: %s"
				    what arglist))
		;; Error continuation
		((string) (error)
		 (message "Cannot get the arglist of %s: %s" string error))))



(defun fi:lisp-apropos (string &optional regexp)
  "In the Common Lisp environment evaluate lisp:apropos on STRING.
With prefix arg REGEXP, STRING is a regular expression for which matches
are sought.  fi:package is used to determine from which Common Lisp package
the operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive
   (list (car (fi::get-default-symbol
	       (if current-prefix-arg "Apropos (regexp)" "Apropos")))
	 (if current-prefix-arg t nil)))
  (make-request (lep::apropos-session :string string :regexp regexp)
		;; Normal continuation
		(() (text)
		 (fi:show-some-text nil text))
		;; Error continuation
		((string) (error)
		 (message "error during apropos of %s: %s" string error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Metadot implementation

(defvar lep::meta-dot-session nil)
(defvar lep::meta-dot-string nil)
(defvar lep::meta-dot-what nil)
(defvar lep::meta-dot-from-fspec nil)

(defun fi:lisp-find-definition (tag &optional next)
  "Find TAG using information in the Common Lisp environment, in the current
window.  With prefix arg NEXT, find the next occurance of the last tag.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive
   (if current-prefix-arg
       '(nil t)
     (list (car (fi::get-default-symbol "Lisp locate source"))
	   nil)))
  (if next
      (fi:lisp-find-next-definition)
    (fi::lisp-find-definition-common tag nil)))


(defun fi:lisp-find-definition-other-window (tag &optional next)
  "Find TAG in the other window using information in the Common Lisp
environment, in the current window.  With prefix arg NEXT, find the next
occurance of the last tag. fi:package is used to determine from which
Common Lisp package the operation is done. In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is parsed
at file visit time."
  (interactive
   (if current-prefix-arg
       '(nil t)
     (list (car (fi::get-default-symbol "Lisp locate source other window"))
	   nil)))
  (if next
      (fi:lisp-find-next-definition)
    (fi::lisp-find-definition-common tag t)))

(defun fi::lisp-find-definition-common (something other-window-p
				 &optional what from-fspec)
  (when (not (fi::lep-open-connection-p))
    (error "connection to ACL is down--can't find tag"))
  (setq lep::meta-dot-string something)
  (setq lep::meta-dot-what (or what "definition"))
  (setq lep::meta-dot-from-fspec from-fspec)
  (fi::delete-metadot-session)

  (message "Finding %s..." lep::meta-dot-what)
  (setq lep::meta-dot-session
    (make-complex-request 
     (scm::metadot-session :package (fi::string-to-keyword fi:package)
			   :type t	; used to be (or type t), but
					; `type' is not bound in this
					; context
			   :fspec something)
     ((something other-window-p what from-fspec)
      (pathname point n-more)
      (fi::show-found-definition (if (symbolp something)
				     (symbol-name something)
				   something)
				 pathname point n-more other-window-p))
     (() (error)
      (fi::delete-metadot-session)
      (message "%s" error)
      ))))

(defun fi:lisp-find-next-definition ()
  "Continue last tags search, started by fi:lisp-find-definition.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive)
  (message "Finding next %s..." lep::meta-dot-what)
  (if (not lep::meta-dot-session) (error "No more definitions"))
  (fi::make-request-in-existing-session 
   lep::meta-dot-session
   (:next)
   (() (pathname point n-more)
    (fi::show-found-definition lep::meta-dot-string pathname point n-more))
   (() (error)
    (fi::delete-metadot-session)
    (message "%s" error)		; lep::meta-dot-string???
    )))

(defun fi::delete-metadot-session ()
  (if lep::meta-dot-session (lep::kill-session lep::meta-dot-session))
  (setq lep::meta-dot-session nil))

(defun fi::show-found-definition (thing pathname point n-more
				  &optional other-window-p)
  (if pathname
      (if (eq pathname ':top-level)
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess "")
	      (xb nil))
	  (if fi:filename-frobber-hook
	      (setq pathname (funcall fi:filename-frobber-hook pathname)))
	  ;;
	  (setq xb (get-file-buffer pathname))
	  (if other-window-p
	      (find-file-other-window pathname)
	    (find-file pathname))
	  (if xb (set-mark (point)))
	  (if (null point)
	      (progn
		(setq mess
		  (format "The definition of %s is somewhere in this file! "
			  thing))
		(beginning-of-buffer))
	    (progn
	      (goto-char (1+ point))
	      (if (not xb) (set-mark (point)))))
	  (cond ((eq n-more 0)
		 (if lep::meta-dot-from-fspec
		     (message (concat mess "%ss of %s")
			      lep::meta-dot-what lep::meta-dot-from-fspec)
		   (message (concat mess "No more %ss of %s")
			    lep::meta-dot-what thing)))
		(n-more
 		 (message (concat mess "%d more %ss of %s")
			  n-more
			  lep::meta-dot-what
			  (or lep::meta-dot-from-fspec thing))))))
    (message "cannot find file for %s" point)))



;; We need to get hold of something

(defun scm::make-and-initialize-metadot-session (something &optional
							   what
							   from-fspec)
  ;; Put the session into the waiting state
  (if lep::meta-dot-session (lep::kill-session lep::meta-dot-session))
  (setq lep::meta-dot-session session)
  (setq lep::meta-dot-string something)
  (setq lep::meta-dot-what (or what "definition"))
  (setq lep::meta-dot-from-fspec from-fspec)

  (fi::modify-session-continuation
   session
   (list (function (lambda (pathname point n-more)
		     (fi::show-found-definition lep::meta-dot-string
						pathname point n-more))))
   (list (function (lambda (error something)
		     (setq lep::meta-dot-session nil)
		     (message "%s: %s" something error)))
	 lep::meta-dot-string)))

(defun scm::return-buffer-status (pathname write-if-modified)
  "This returns information about the status of the buffer: whether it
exists, if it is modified, last tick (when implemented), and optionally
return the pathname of temp file."
  (let ((buffer (get-file-buffer pathname)))
    (if buffer
	(list ':exists 
	      (buffer-modified-p buffer)
	      (and write-if-modified
		   (or (not (integerp write-if-modified))
		       (not (fboundp 'buffer-modified-tick))
		       (not (equal (buffer-modified-tick) write-if-modified)))
		   (buffer-modified-p buffer)
		   (save-excursion
		     (set-buffer buffer)
		     (let* ((file (concat
				   fi:emacs-to-lisp-transaction-directory
					  (make-temp-name "/foo")))
			    (buffer (get-file-buffer file)))
		       (when buffer (kill-buffer buffer))
		       (write-region (point-min) (point-max) file nil
				     'no-message)
		       file)))
	      (lep::buffer-modified-tick))
      (list ':does-not-exist))))

(defun lep::buffer-modified-tick ()
  "Get the buffer tick if it is supported"
  (and (fboundp 'buffer-modified-tick) (buffer-modified-tick)))

(defun fi:bug-report ()
  "Create a mail buffer which contains information about the Common Lisp
environment in which the bug occurs.  A :zoom and other related information
is obtained from the \"Initial Lisp Listener\".  See M-x mail for more
information on how to send the mail." 
  (interactive)
  (make-request (lep::bug-report-session
		 :process-name (fi::read-lisp-process-name
				"Process for stack :zoom: "))
		;; Normal continuation
		(() (error-message stack lisp-info)
		 (mail)
		 (mail-to)
		 (insert "bugs@franz.com")
		 (mail-subject)
		 (insert "Bug-report")
		 (end-of-buffer)
		 (save-excursion
		   (insert "\n")
		   (insert error-message)
		   (insert "------------------------------\n")
		   (insert stack)
		   (insert "------------------------------\n")	     
		   (insert lisp-info)))
		;; Error continuation
		(() (error)
		 (message "Cannot do a backtrace because: %s" error))))

;;; Macroexpansion and walking

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive)
  (message "Macroexpanding...")
  (fi::lisp-macroexpand-common 'lisp:macroexpand-1 "macroexpand"))

(defun fi:lisp-macroexpand-recursively (arg)
  "Print the full, recursive macroexpansion the form at the point.
With prefix arg, recursively macroexpand the code as the compiler would.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive "P")
  (message "Recursively macroexpanding...")
  (fi::lisp-macroexpand-common
   (if arg 'excl::compiler-walk 'clos::walk-form) "walk"))

(defun fi::lisp-macroexpand-common (expander type)
  (make-request
   (lep::macroexpand-session
    :expander expander :package
    (fi::string-to-keyword fi:package)
    :form (let ((start (condition-case ()
			   (fi::find-other-end-of-list)
			 (error nil))))
	    (if start
		(buffer-substring start (point))
	      (read-string (format "form to %s: " type)))))
		(() (expansion)
		 (fi:show-some-text fi:package expansion))
		(() (error)
		 (message "Cannot macroexpand: %s" error))))


;;; Symbol completion

(defun fi:lisp-complete-symbol ()
  "Perform completion on the Common Lisp symbol preceding the point.  That
symbol is compared to symbols that exist in the Common Lisp environment.
If the symbol starts just after an open-parenthesis, then only symbols (in
the Common Lisp) with function definitions are considered.  Otherwise all
symbols are considered.  fi:package is used to determine from which Common
Lisp package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time.

Abbreviations are also expanded.  For example, in the initial `user'
package, which inherits symbols from the `common-lisp' package, ``m-p-d-''
will expand to ``most-positive-double-float''.  The hyphen (-) is a
separator that causes the substring before the hyphen to be matched at the
beginning of words in target symbols."
  (interactive)
  (let* ((end (point))
	 package real-beg
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(setq real-beg (point))
		(let ((opoint (point)))
		  (if (re-search-forward ":?:" end t)
		      (setq package
			(concat
			 ":" (buffer-substring opoint (match-beginning 0))))))
		(point)))
	 (pattern (buffer-substring beg end))
	 (functions-only (if (eq (char-after (1- real-beg)) ?\() t nil))
	 (alist
	  (fi::lisp-complete-1 pattern package functions-only))
	 (completion (if alist (try-completion pattern alist))))
    (cond ((eq completion t))
	  ((and (null completion) (null alist))
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((and (null completion) alist (null (cdr alist)))
	   (delete-region beg end)
	   (insert (car (car alist))))
	  ((and (null completion) alist)
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Help*"
	     (display-completion-list (mapcar 'car alist)))
	   (message "Making completion list...done"))
	  ((not (string= pattern completion))
	   (let ((new (cdr (assoc completion alist))))
	     (if new
		 (progn
		   (delete-region real-beg end)
		   (insert new))
	       (delete-region beg end)
	       (insert completion))))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Help*"
	     (display-completion-list
	      (all-completions pattern alist)))
	   (message "Making completion list...done")))))

(defun fi::lisp-complete-1 (pattern package functions-only)
  (let ((completions
	 (progn
	   (car (lep::eval-session-in-lisp 
		 'lep::list-all-completions-session
		 ':pattern (fi::frob-case-to-lisp pattern)
		 ':buffer-package (fi::string-to-keyword fi:package)
		 ':package (progn
			     (if (equal ":" package)
				 (setq package "keyword"))
			     (intern (fi::frob-case-to-lisp
				      package)))
		 ':functions-only-p (intern
				     (fi::frob-case-to-lisp
				      functions-only)))))))
    (fi::lisp-complete-2 completions)))

(defun fi::lisp-complete-2 (completions &optional dont-strip-package)
  (if (consp completions)
      (apply 'list
	     (mapcar
	      (function
	       (lambda (x)
		 (let* ((whole-name (if (symbolp x) (symbol-name x) x))
			(name (if dont-strip-package
				  whole-name
				(progn
				  (string-match "^\\(.*::?\\)?\\(.*\\)$"
						whole-name)
				  (substring whole-name
					     (match-beginning 2)
					     (match-end 2))))))
		   (cons name whole-name))))
	      completions))))

(defun lep::my-find-file (filename)
  (find-file filename))


(defun lep::display-string-in-buffer (string buffer)
  "Display a string in buffer"
  (fi:lisp-push-window-configuration)
  (switch-to-buffer (get-buffer-create buffer))
  (erase-buffer)
  (insert string))


(defun lep::prompt-for-values (what prompt options)
  (list (case what
	  (:symbol
	   (let* ((string (read-string
			   prompt (fi::getf-property options ':initial-input)))
		  (colonp (string-match ":?:" string nil))
		  (package (or (fi::getf-property options ':package)
			       fi:package)))
	     ;; symbol-point
	     (if colonp
		 string
	       (if package
		   (concat fi:package "::" string)
		 string))))
	  (:file-name (read-file-name 
		       prompt
		       (fi::getf-property options ':directory)
		       (fi::getf-property options ':default)
		       (fi::getf-property options ':mustmatch)))
	  (t (read-string
		    prompt (fi::getf-property options ':initial-input))))))

(defun lep::completing-read (prompt require-match initial-input)
  (list (completing-read 
	 prompt
	 'lep::completing-read-complete
	 nil
	 require-match
	 initial-input)))

(defun lep::completing-read-complete (pattern predicate what)
  (let* ((inhibit-quit nil)
	 (alist
	  (fi::lisp-complete-2
	   (car
	    (lep::make-request-in-session-and-wait
	     session
	     ':complete
	     pattern))
	   t))
	 (completion (and alist (try-completion pattern alist))))
    (ecase what
      ((nil) completion)
      ((t) (mapcar (function cdr) alist))
      (lambda (not (not alist))))))

(defun lep::show-clman (string)
  (if string 
      (fi:clman string)
    (call-interactively 'fi:clman)))
  
(defun lep::buffer-region (buffer start end)
  (set-buffer buffer)
  (list (buffer-substring (or start (point-min)) (or end (point-max)))))

(defun fi:kill-definition (do-kill)
  "Insert a form to kill, or undefine, the definition that starts at the
point.  The undefining form is inserted after the form to undefine.
With prefix arg DO-KILL, then actually undefine the form in the Common Lisp
environment instead of inserted the undefining form.  fi:package is used to
determine from which Common Lisp package the operation is done.  In a
subprocess buffer, the package is tracked automatically.  In source buffer,
the package is parsed at file visit time."
  (interactive "P")
  (message "Killing definition...")
  (make-request (lep::undefine-reply :buffer (buffer-name) 
				     :start-point (point)
				     :end-point (save-excursion
						  (forward-sexp)
						  (point))
				     :doit do-kill)
		((do-kill) (ok form)
		 (if (not do-kill)
		     (progn (end-of-defun) 
			    (save-excursion
			      (insert form)
			      (insert "\n"))))
		 (message "Killing definition...done."))
		(() (error)
		 (message "Cannot kill current definition %s" error))))


(defun fi:toggle-trace-definition (string)
  "Dynamically toggle, in the Common Lisp environment, tracing for STRING.
If tracing is turned on, then it will be turned off for STRING.  If it is
turned off, then it will be turned on for STRING.  With a prefix arg, cause
the debugger to be invoked, via a call to BREAK, when the function is called.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "(un)trace"))
  (make-request (lep::toggle-trace :fspec string :break current-prefix-arg)
		;; Normal continuation
		(() (what tracep)
		 (message (if tracep
			      "%s is now traced"
			    "%s is now untraced")
			  what))
		;; Error continuation
		((string) (error)
		 (message "Cannot (un)trace %s: %s" string error))))


;;;; list and edit somethings

(defun fi:list-who-calls (&optional fspec)
  "List all the callers of FSPEC.  `List' means to show them in a buffer in
definition mode.  The source for each definition can be easily found via
key bindings in definition mode.  The default FSPEC is taken from the text
surrounding the point.  fi:package is used to determine from which Common
Lisp package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time."
  (interactive (fi::get-default-symbol "List who calls"))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding callers of %s..." fspec)
  (lep::list-fspecs-common fspec
			   'lep::who-calls
			   "Cannot find the callers: %s"
			   "caller"))

(defun fi:list-who-is-called-by (fspec)
  "List all the functions called by FSPEC.  `List' means to show them in a
buffer in definition mode.  The source for each definition can be easily
found via key bindings in definition mode.  The default FSPEC is taken from
the text surrounding the point.  fi:package is used to determine from which
Common Lisp package the operation is done.  In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is parsed
at file visit time."
  (interactive (fi::get-default-symbol "List who is called by"))
  (message "Finding who is called by %s..." fspec)
  (lep::list-fspecs-common fspec
			   'lep::who-is-called-by
			   "Cannot find who is called by: %s"
			   "callee"))

(defun fi:list-generic-function-methods (&optional fspec)
  "List all the generic function methods of FSPEC.  `List' means to show
them in a buffer in definition mode.  The source for each definition can be
easily found via key bindings in definition mode.  The default FSPEC is
taken from the text surrounding the point.  fi:package is used to determine
from which Common Lisp package the operation is done.  In a subprocess
buffer, the package is tracked automatically.  In source buffer, the
package is parsed at file visit time."
  (interactive (fi::get-default-symbol "List generic function methods of"))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding generic function methods of %s..." fspec)
  (lep::list-fspecs-common fspec
			   'scm::generic-function-methods-function-specs
			   "Cannot find the generic function methods: %s"
			   "generic function method"))


(defun fi:edit-who-calls (fspec)
  "Edit all the callers of the function named by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit who calls"))
  (message "Editing callers...")
  (lep::edit-somethings fspec 'lep::who-calls nil "caller"))

(defun fi:edit-who-is-called-by (fspec)
  "Edit all functions called by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit who is called by"))
  (message "Editing callees...")
  (lep::edit-somethings fspec 'lep::who-is-called-by nil "callee"))

(defun fi:edit-generic-function-methods (fspec)
  "Edit all the methods of the generic function named by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit generic function methods of"))
  (message "Editing generic function methods...")
  (lep::edit-somethings fspec
			'scm::generic-function-methods-function-specs
			nil
			"generic function method"))


(defun lep::list-fspecs-common (fspec function msg &optional what)
  (make-request (lep::list-fspecs-session
		 :function function :fspec (fi::frob-case-to-lisp fspec))
		((fspec fi:package what) (the-definitions)
		 (lep:display-some-definitions fi:package
					       the-definitions
					       (list 'lep::find-a-definition
						     what
						     fspec)))
		((msg) (error)
		 (message msg error))))

(defun lep::find-a-definition (string type list-buffer what from-fspec)
  (fi::lisp-find-definition-common string t what from-fspec))

(defun lep::edit-somethings (fspec generator &optional other-window-p what)
  (if lep::meta-dot-session (lep::kill-session lep::meta-dot-session))
  (setq lep::meta-dot-session
    (make-complex-request
     (scm::edit-sequence-session :generator generator
				 :package (fi::string-to-keyword fi:package)
				 :fspec fspec)
     ((other-window-p fspec what) (pathname point n-more)
      (setq lep::meta-dot-what (or what "definition"))
      (setq lep::meta-dot-from-fspec fspec)
      (fi::show-found-definition fspec pathname point n-more other-window-p))
     ((fspec) (error)
      (fi::delete-metadot-session)
      (error "Cannot edit %s: %s" fspec error)))))

;;; describing something

(defun fi:describe-symbol (fspec)
  "Dynamically, in the Common Lisp environment, describe the symbol named
by FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (lep::describe-something fspec 'identity))

(defun fi:describe-class (fspec)
  "Dynamically, in the Common Lisp environment, describe the class named by
FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Class name"))
  (lep::describe-something fspec 'clos::find-class))


(defun fi:describe-function (fspec)
  "Dynamically, in the Common Lisp environment, describe the function named
by FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Function spec"))
  (lep::describe-something fspec 'fdefinition))


(defun lep::describe-something (fspec function)
  (make-request (lep::describe-something-session
		 :fspec fspec :function function)
		;; Normal continuation
		(() (what)
		 (fi:show-some-text nil what))
		;; Error continuation
		((fspec) (error)
		 (message "Cannot describe %s: %s" fspec error))))



;;; Function documentation

(defun fi:lisp-function-documentation (symbol)
  "Dynamically, in the Common Lisp environment, determine the function
documentation for SYMBOL.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (make-request (lep::function-documentation-session :package fi:package
						     :fspec symbol)
		;; Normal continuation
		((symbol) (documentation)
		 (if documentation
		     (fi:show-some-text fi:package documentation)
		   (message "There is no documentation for %s" symbol)))
		;; Error continuation
		((symbol) (error)
		 (message "Cannot find documentation for %s: %s"
			  symbol error))))


(defun fi:compile-file (file)
  "Compile FILE and load the result of this compilation into the Lisp
environment."
  (interactive "fFile to compile and load:")
  (fi::compile-or-load-file file ':compile-and-load))

(defun fi:load-file (file)
  "Load FILE into the Lisp environment."
  (interactive "fFile to load:")
  (fi::compile-or-load-file file ':load))

(defun fi::compile-or-load-file (file operation)
  (make-request (lep::compile/load-file-request :pathname file
						:operation operation)
		(() ())
		(() (error)
		 (message "Could not :%s" error))))


(defun fi:list-undefined-functions ()
  "Using the cross referencing database in the Lisp environment and
inverse-definition mode, find and display all the functions which are
called but not defined.  See the documentation for
fi:inverse-definition-mode for more information on finding the callers of
the undefined functions.  See the Allegro CL variable
EXCL:*RECORD-XREF-INFO*."
  (interactive)
  (message "Finding undefined functions...")
  (make-request (lep::list-undefined-functions-session)
		((fi:package) (undeffuncs)
		 (message "Finding undefined functions...done.")
		 (lep:display-some-inverse-definitions
		  fi:package
		  undeffuncs
		  (list 'lep::edit-undefined-function-callers)))
		(() (error)
		 (message "error: %s" error))))

(defun lep::edit-undefined-function-callers (fspec &rest ignore)
  (lep::edit-somethings fspec 'lep::who-calls t))

(defun lep::eval-from-lisp (string)
  (list (eval (car (read-from-string string)))))
