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
;; $Header: /repo/cvs.copy/eli/fi-lep.el,v 1.3 1991/01/30 10:38:42 layer Exp $
;;
;;;;;; This LEP file redefines many of the fi:functions in the fi/keys.el file

;;;; Implementation of arglist

(defun fi:lisp-arglist (string)
  (interactive (fi::get-default-symbol "Arglist for"))
  (make-request (lep::arglist-session :fspec string)
		;; Normal continuation
		(() (what arglist)
		 (show-some-short-text "The arglist of %s is %s"
				       what arglist))
		;; Error continuation
		((string) (error)
		 (message "Cannot get the arglist of %s: %s" string error))))

(defun show-some-short-text (text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let* ((window (minibuffer-window))
	 (height (1- (window-height window)))
	 (width (window-width window))
	 (lines/len (frob-string text)))
    (if (and (< (car lines/len) 2)
	     (<= (second lines/len) width))
	(message text)
      (show-some-text text fi:package))))

(defun frob-string (text)
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
    
	
    
;;;; Mmmm... that did not seem to painful.
;;;; We should try and implement everything else this way also
;;;; Find definition is the fun one.



;;; Metadot implementation

(defvar lep:*meta-dot-session* nil)
(defvar *meta-dot-string* nil)

;; Both of these should take a next argument but what does it do?

(defun fi:lisp-find-tag (something &optional next)
  "Lep version"
  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (fi::get-default-symbol "Lisp locate source") nil)))
  (fi::lisp-find-tag-common something next nil))


(defun fi:lisp-find-tag-other-window (something &optional next)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (fi::get-default-symbol "Lisp locate source") nil)))
  (fi::lisp-find-tag-common something next nil))

(defun delete-metadot-session ()
  (if lep:*meta-dot-session* (lep::kill-session lep:*meta-dot-session*))
  (setq lep:*meta-dot-session* nil))

(defun fi::lisp-find-tag-common (something  next other-window-p)
  (message "Finding definition ...")
  (setq  *meta-dot-string* something)
  (if (condition-case nil
	  (progn (ensure-lep-connection) t)
	(error nil))
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

(defun fi:lisp-tags-loop-continue ()
  (interactive)
  (if  lep:*meta-dot-session*
      (make-request-in-existing-session 
       lep:*meta-dot-session*
       (:next)
       (() (pathname point n-more)
	(show-found-definition  *meta-dot-string* pathname point n-more))
       (() (error)
	(delete-metadot-session)
	(find-definition-using-find-tag *meta-dot-string* error)))
    (find-next-definition-using-find-tag)))

(defun show-found-definition (thing pathname point n-more
			      &optional other-window-p)
  (if pathname
      (if (eq pathname ':top-level)
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess ""))
	  (if (or other-window-p
		  ;; Perhaps if the current buffer is a listener we want to
		  ;; find it elsewhere
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

(defun find-next-definition-using-find-tag ()
  (tags-loop-continue))


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
		       (not (fboundp 'buffer-tick))
		       (not (equal (buffer-tick) write-if-modified)))
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
  "Describe a symbol, which is read from the minibuffer.  The word around
the point is used as the default."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (make-request (lep::describe-session
		 :package (string-to-keyword fi:package) :fspec symbol)
		;; Normal continuation
		((symbol) (description)
		 (show-some-text description fi:package))
		;; Error continuation
		((symbol) (error)
		 (message "Cannot describe %s: %s" symbol error))))



(defun show-some-text (text package)
  "Display TEXT in a temporary buffer putting that buffer setting that buffers package to the
package of PACKAGE"
  (let ((buffer (get-buffer-create "*CL-temp*"))
	(switched nil))
    (unless (eq (current-buffer) buffer)
      (setq switched t)
      (switch-to-buffer-other-window buffer))
    (erase-buffer)
    (fi:common-lisp-mode)
    (setq fi:package package)
    (insert text)
    (beginning-of-buffer)
    (unless (one-window-p)
      (let ((height (1- (window-height)))
	    (lines (count-lines (point-min) (point-max))))
	(if (> 4 lines) (setq lines 4))
	(if (> height lines) (shrink-window (- height lines)))))
    (when switched (other-window 1))))

;;; Function documentation

(defun fi:lisp-function-documentation (symbol)
  "Describe a symbol, which is read from the minibuffer.  The word around
the point is used as the default."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (make-request
   (lep::function-documentation-session :package fi:package :fspec symbol)
   ;; Normal continuation
   ((symbol) (documentation)
    (if documentation
	(show-some-text documentation fi:package)
      (message "There is no documentation for %s" symbol)))
   ;; Error continuation
   ((symbol) (error)
    (message "Cannot find documentation for %s: %s" symbol error))))

;;; Macroexpansion and walking

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point"
  (interactive)
  (fi::lisp-macroexpand-common 'lisp:macroexpand "macroexpand"))

(defun fi:lisp-walk (arg)
  "Print the full macroexpansion the form at the point.
With a prefix argument, macroexpand the code as the compiler would."
  (interactive "P")
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
		 (show-some-text expansion fi:package))
		(() (error)
		 (message "Cannot macroexpand: %s" error))))


;;; Symbol completion

(defun fi:lisp-complete-symbol ()
  "Perform completion on the Common Lisp symbol preceding the point.  That
symbol is compared to symbols that exist in the Common Lisp, to which there
is a TCP/IP connection (see fi:eval-in-lisp).  If the symbol starts just
after an open-parenthesis, then only symbols (in the Common Lisp) with
function defintions are considered.  Otherwise all symbols are considered."
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
  "Print all the callers of a function.  The default symbol name is taken
from the sexp around the point."
  (interactive (fi::get-default-symbol "Find references to symbol"))
  ;; Since this takes a while, tell the user that it has started.
  (message "finding callers of %s..." symbol)
  (list-fspecs-common symbol 'lep::who-calls "Cannot find the callers: %s"))

(defun list-fspecs-common (symbol function msg)
  (make-request (lep::list-fspecs-session
		 :function function :fspec (fi::frob-case-to-lisp symbol))
		(() (who text)
		 ;; It might be good to make this list mouse sensitive so that
		 ;; we can mouse there
		 ;; dmode should be a minor mode???
		 (show-some-text text  fi:package))
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
  "Create a tcp listener in response to a request for a listener stream"
  ;; We create a tcp listener using the stream protocol which arranges for
  ;; the stream to be passed back to the session
  (print 1 (get-buffer "*scratch*"))
  (let ((fi::listener-protocol ':stream))
    (send-string (fi:tcp-common-lisp -1)
		 (format "%d\n" (session-id session))))
  (print 2 (get-buffer "*scratch*")))

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
    (call-interactively 'fi::clman)))
  
(defun lep::buffer-region (buffer start end)
  (set-buffer buffer)
  (list (buffer-substring (or start (point-min)) (or end (point-max)))))

(defun lep::kill-definition (prefix)
  "Kill the definition that starts at the point"
  (interactive "P")
  (make-request (lep::undefine-reply :buffer (buffer-name) 
				     :start-point (point)
				     :end-point (save-excursion
						  (forward-sexp)
						  (point))
				     :doit (not  prefix))
		((prefix) (ok form)
		 (if prefix
		     (progn (end-of-defun) 
			    (save-excursion (insert form)
					    (insert "\n")))))
		(() (error)
		 (message "Cannot undefine current definition %s" error))))


(defun lep::toggle-trace-definition (string)
  "Trace or untrace the specified function"
  (interactive (fi::get-default-symbol "(un)trace"))
  (make-request (lep::toggle-trace :fspec string)
		;; Normal continuation
		(() (what tracep)
		 (message (if tracep
			      "%s is now traced"
			    "%s is now untraced")
			  what))
		;; Error continuation
		((string) (error)
		 (message "Cannot (un)trace %s: %s" string error))))