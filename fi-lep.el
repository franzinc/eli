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
;; $Header: /repo/cvs.copy/eli/fi-lep.el,v 1.16 1991/03/12 18:29:33 layer Exp $
;;

(defvar fi:always-in-a-window nil)

(defun fi:show-some-text (package text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let ((n (string-match "\n$" text)))
    (when n (setq text (substring text 0 n))))
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
	  (message text-try)
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
STRING."
  (interactive (fi::get-default-symbol "Arglist for"))
  (make-request (lep::arglist-session :fspec string)
		;; Normal continuation
		(() (what arglist)
		 (fi:show-some-text nil
				    "The arglist of %s is %s"
				    what arglist))
		;; Error continuation
		((string) (error)
		 (message "Cannot get the arglist of %s: %s" string error))))



(defun fi:lisp-apropos (string &optional regexp)
  "In the Common Lisp environment evaluate lisp:apropos on STRING.  STRING
is taken to be a regular expression if a prefix argument is given."
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

;;; Metadot implementation

(defvar lep:*meta-dot-session* nil)
(defvar *meta-dot-string* nil)

;; Both of these should take a next argument but what does it do?

(defun fi:lisp-find-tag (something &optional next)
  "Find TAG using information in the Common Lisp environment, in the current
window.  If NEXT or a prefix argument is given, then find the next
occurance of the last tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (car (fi::get-default-symbol "Lisp locate source")) nil)))
  (fi::lisp-find-tag-common something next nil))


(defun fi:lisp-find-tag-other-window (tag &optional next)
  "Find TAG using information in the Common Lisp environment, in the other
window.  If NEXT or a prefix argument is given, then find the next
occurance of the last tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (car (fi::get-default-symbol "Lisp locate source")) nil)))
  (fi::lisp-find-tag-common tag next t))

(defun delete-metadot-session ()
  (if lep:*meta-dot-session* (lep::kill-session lep:*meta-dot-session*))
  (setq lep:*meta-dot-session* nil))

(defun fi::lisp-find-tag-common (something next other-window-p)
  (message "Finding definition...")
  (setq  *meta-dot-string* something)
  (if (lep::lep-open-connection-p)
      (progn
	(delete-metadot-session)
	(setq lep:*meta-dot-session*
	  (make-complex-request 
	   (scm::metadot-session :package (string-to-keyword fi:package)
				 :fspec something)
	   ((something other-window-p) (pathname point n-more)
	    (show-found-definition (if (symbolp something)
				       (symbol-name something)
				     something)
				   pathname point n-more other-window-p))
	   ((something other-window-p) (error)
	    (delete-metadot-session)
	    (find-definition-using-find-tag (if (symbolp something)
						(symbol-name something)
					      something)
					    other-window-p error)))))
    (progn
      (setq lep:*meta-dot-session* nil)
      (find-definition-using-find-tag something other-window-p))))

(defun fi:lisp-tags-loop-continue (&optional first-time)
  "Find the next definition found by a previous fi:lisp-find-tag."
  (interactive "P")
  (if (or tags-loop-form (not lep:*meta-dot-session*))
      (progn
	(when lep:*meta-dot-session*
	  (delete-metadot-session))
	(tags-loop-continue first-time))
    (make-request-in-existing-session 
     lep:*meta-dot-session*
     (:next)
     (() (pathname point n-more)
      (show-found-definition  *meta-dot-string* pathname point n-more))
     (() (error)
      (delete-metadot-session)
      (find-definition-using-find-tag *meta-dot-string* error)))))

(defun show-found-definition (thing pathname point n-more
			      &optional other-window-p)
  (if pathname
      (if (eq pathname ':top-level)
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess ""))
	  (if fi:filename-frobber-hook
	      (setq pathname (funcall fi:filename-frobber-hook pathname)))
	  (if (or other-window-p
		  fi:subprocess-mode
		  ;; Perhaps there is already a window for this file
		  )
	      (find-file-other-window pathname)
	    (find-file pathname))
	  (if (null point)
	      (progn
		(setq mess
		  (format "The definition of %s is somewhere in this file! : "
			  thing))
		(beginning-of-buffer))
	    (goto-char (1+ point)))
	  (cond ((eq n-more 0)
		 (message (concat mess "No more definitions of %s") thing))
		(n-more
		 (message (concat mess "%d more definitions of %s")
			  n-more thing)))))
    (message "cannot find file for %s" point)))


;;; When we have no success with asking the lisp start using the tags mechanism
;;;; We need to strip of the package prefix here

(defun find-definition-using-find-tag (tag other-window-p &optional error)
  (if error 
      (if fi:source-info-not-found-hook
	  (condition-case nil
	      (find-definition-using-find-tag-1 tag other-window-p)
	    (error (message "The source location of `%s' is unknown: %s"
			    tag error)))
	(message "Cannot find the definition of %s: %s" tag error))
    (if fi:source-info-not-found-hook
	(find-definition-using-find-tag-1 tag other-window-p)
      (message "Cannot find the definition of %s" tag))))

(defun find-definition-using-find-tag-1 (tag other-window-p)
  (let* ((tag (if (not (stringp tag))
		  (prin1-to-string tag)
		tag))
	 (colonp (string-match ":?:" tag nil))
	 (sym (fi::frob-case-to-lisp
	       (if colonp
		   (substring tag (match-end 0))
		 tag))))
    (funcall fi:source-info-not-found-hook sym)))


;; We need to get hold of something

(defun scm::make-and-initialize-metadot-session (something)
  "Put the session into the waiting state"
  (if lep:*meta-dot-session* (lep::kill-session lep:*meta-dot-session*))
  (setq lep:*meta-dot-session* session)
  (setq  *meta-dot-string* something)
  (modify-session-continuation
   session
   (list (function (lambda (pathname point n-more)
		     (show-found-definition *meta-dot-string*
					    pathname point n-more))))
   (list (function (lambda (error something)
		     (setq lep:*meta-dot-session* nil)
		     (find-definition-using-find-tag
		      (if (symbolp something)
			  (symbol-name something)
			something)
		      error)))
	 *meta-dot-string*)))

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
	      (and (fboundp 'buffer-tick) (buffer-tick)))
      (list ':does-not-exist))))

;;; This is pretty horrible:
;;; Some how we need to get the name of the process for the buffer
;;; and send that

(defun bug-report ()
  (interactive)
  (make-request (lep::bug-report-session)
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



;;;; Edit compiler warnings would be pretty cool also.
;;;; Symbol completion would be quite nice

;;;; *** This is broken, because if we cannot find the source file
;;;; we should invoke the tags code on each one.

(defun edit-somethings (something generator description)
  (if lep:*meta-dot-session* (lep::kill-session lep:*meta-dot-session*))
  (setq lep:*meta-dot-session*
    (make-complex-request
     (scm::edit-sequence-session 
      :generator generator
      :package (string-to-keyword fi:package) :fspec something)
     ((something) (pathname point n-more)
      (show-found-definition something pathname point n-more))
     ((something) (error)
      (delete-metadot-session)
      (error "Cannot edit sequence %s: %s" something error)))))

(defun fi:lisp-describe (symbol)
  "Dynamically, in the Common Lisp environment, describe SYMBOL."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (make-request (lep::describe-session
		 :package (string-to-keyword fi:package) :fspec symbol)
		;; Normal continuation
		((symbol) (description)
		 (fi:show-some-text fi:package description))
		;; Error continuation
		((symbol) (error)
		 (message "Cannot describe %s: %s" symbol error))))


;;; Function documentation

(defun fi:lisp-function-documentation (symbol)
  "Dynamically determine, in the Common Lisp environment, the function
documentation for SYMBOL."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (make-request
   (lep::function-documentation-session :package fi:package :fspec symbol)
   ;; Normal continuation
   ((symbol) (documentation)
    (if documentation
	(fi:show-some-text fi:package documentation)
      (message "There is no documentation for %s" symbol)))
   ;; Error continuation
   ((symbol) (error)
    (message "Cannot find documentation for %s: %s" symbol error))))

;;; Macroexpansion and walking

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point."
  (interactive)
  (message "Macroexpanding...")
  (fi::lisp-macroexpand-common 'lisp:macroexpand "macroexpand"))

(defun fi:lisp-macroexpand-recursively (arg)
  "Print the full macroexpansion the form at the point.
With a prefix argument, macroexpand the code as the compiler would."
  (interactive "P")
  (message "Recursively macroexpanding...")
  (fi::lisp-macroexpand-common
   (if arg 'excl::compiler-walk 'clos::walk-form) "walk"))

(defun fi::lisp-macroexpand-common (expander type)
  (make-request
   (lep::macroexpand-session
    :expander expander :package
    (string-to-keyword fi:package)
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
the Common Lisp) with function defintions are considered.  Otherwise all
symbols are considered."
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
	 (completions
	  (progn
	    (car (lep::eval-in-lisp 
			     'lep::list-all-completions-session
			     ':pattern (fi::frob-case-to-lisp pattern)
			     ':buffer-package (string-to-keyword fi:package)
			     ':package (progn
					 (if (equal ":" package)
					     (setq package "keyword"))
					 (intern (fi::frob-case-to-lisp
						  package)))
			     ':functions-only-p (intern
						 (fi::frob-case-to-lisp
						  functions-only))))))
	 (alist
	  (if (consp completions)
	      (apply 'list
		     (mapcar
		      (function
		       (lambda (x)
			(let* ((whole-name (symbol-name x))
			       (name (progn
				       (string-match "^\\(.*::?\\)?\\(.*\\)$"
						     whole-name)
				       (substring whole-name
						  (match-beginning 2)
						  (match-end 2)))))
			  (cons name whole-name))))
		      completions))))
	 (completion (if alist (try-completion pattern alist))))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
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


(defun fi:lisp-who-calls (&optional symbol)
  "Print all the callers of SYMBOL, the default of which is taken from the
s-expression around the point."
  (interactive (fi::get-default-symbol "Find references to symbol"))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding callers of %s..." symbol)
  (list-fspecs-common symbol 'lep::who-calls "Cannot find the callers: %s"))

(defun list-fspecs-common (symbol function msg)
  (make-request (lep::list-fspecs-session
		 :function function :fspec (fi::frob-case-to-lisp symbol))
		((symbol) (who text)
		 ;; It might be good to make this list mouse sensitive so that
		 ;; we can mouse there
		 ;; dmode should be a minor mode???
		 (cond
		  (text (fi:show-some-text fi:package text))
		  (t
		   (beep)
		   (message "No callers of \"%s\" in package \"%s\""
			    symbol fi:package))))
		((msg) (error)
		 (message msg error))))


(defun lep::my-find-file (filename)
  (find-file filename))


(defun lep::display-string-in-buffer (string buffer)
  "Display a string in buffer"
  (switch-to-buffer (get-buffer-create buffer))
  (erase-buffer)
  (insert string))

(defun lep::create-listener-stream ()
  "Create a tcp listener in response to a request for a listener stream."
  ;; We create a tcp listener using the stream protocol which arranges for
  ;; the stream to be passed back to the session
  (let ((fi::listener-protocol ':stream))
    (send-string (fi:open-lisp-listener -1)
		 (format "%d\n" (session-id session)))))

(defun getf-property (plist property &optional default)
  (while (and plist
	      (not (eq (car plist) property)))
    (setq plist (cddr plist)))
  (if plist 
      (second plist)
    default))

(defun lep::prompt-for-values (what prompt options)
  (list (ecase what
	  (:symbol
	   (let* ((string (read-string
			   prompt (getf-property options ':initial-input)))
		  (colonp (string-match ":?:" string nil))
		  (package (or (getf-property options ':package)
			       fi:package)))
	     ;; symbol-point
	     (if colonp
		 string
	       (if package
		   (concat fi:package "::" string)
		 string))))
	  (:file-name (read-file-name 
		       prompt
		       (getf-property options ':directory)
		       (getf-property options ':default)
		       (getf-property options ':mustmatch)))
	  (:string (read-string
		    prompt (getf-property options ':initial-input))))))

(defun lep::show-clman (string)
  (if string 
      (fi:clman string)
    (call-interactively 'fi:clman)))
  
(defun lep::buffer-region (buffer start end)
  (set-buffer buffer)
  (list (buffer-substring (or start (point-min)) (or end (point-max)))))

(defun lep::kill-definition (prefix)
  "Kill the definition that starts at the point. Without a prefix argument
it inserts a form to undefine it at the end of the definition. With a
prefix argument do it"
  (interactive "P")
  (make-request (lep::undefine-reply :buffer (buffer-name) 
				     :start-point (point)
				     :end-point (save-excursion
						  (forward-sexp)
						  (point))
				     :doit prefix)
		((prefix) (ok form)
		 (if (not prefix)
		     (progn (end-of-defun) 
			    (save-excursion (insert form)
					    (insert "\n")))))
		(() (error)
		 (message "Cannot undefine current definition %s"  error))))


(defun fi:toggle-trace-definition (string)
  "Dynamically toggle, in the Common Lisp environment, tracing for STRING.
If tracing is turned on, then it will be turned off for STRING.  If it is
turned off, then it will be turned on for STRING.  When given a prefix
argument, cause the debugger to be invoked, via a call to BREAK, when the
function is called."
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

