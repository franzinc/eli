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
;; $Header: /repo/cvs.copy/eli/fi-basic-lep.el,v 1.21 1991/09/27 00:58:48 layer Exp $
;;
;; The basic lep code that implements connections and sessions

(defvar fi::lep-debug nil)			; for debugging
(defvar fi::trace-lep-filter nil)	; for debugging

(defvar fi:pop-up-temp-window-behavior '(other . t)
  "*The value of this variable determines the behavior of the popup
temporary buffers used to display information which is the result of
queries of the Lisp environment.  The value is a cons of the style and a
boolean of whether or not the minibuffer should be used for displaying the
result.  The possible values for the CAR of the cons are the symbols
SPLIT, OTHER, and REPLACE.  SPLIT causes the largest window to be split
and the new window to be minimal in size.  OTHER causes the other window to
be used, spliting the screen if there is only one window.  REPLACE causes
the current window to be replaced with the help buffer.  The reason for
specifying a CDR of nil is so that a window is always used--messages
printed in the minibuffer can easily be erased.")

(defun fi:show-some-text (package text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let ((n (string-match "[\n]+\\'" text)))
    (when n (setq text (substring text 0 n))))
  (if (null (cdr fi:pop-up-temp-window-behavior))
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
	  (progn
	    (message "%s" text-try)
	    (fi::note-background-reply))
	(fi::show-some-text-1
	 (cond (fi:package (format "[package: %s]\n%s" fi:package text))
	       (t text))
	 (or package fi:package))))))

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

(defvar fi::show-some-text-1-first-time t)

(defun fi::show-some-text-1 (text package &optional hook &rest args)
  (fi:lisp-push-window-configuration)
  (cond ((eq 'split (car fi:pop-up-temp-window-behavior))
	 (apply 'fi::show-some-text-split text package hook args))
	((eq 'other (car fi:pop-up-temp-window-behavior))
	 (apply 'fi::show-some-text-other text package hook args))
	((eq 'replace (car fi:pop-up-temp-window-behavior))
	 (apply 'fi::show-some-text-replace text package hook args))
	(t (error "bad value for car of fi:pop-up-temp-window-behavior: %s"
		  (car fi:pop-up-temp-window-behavior:))))
  (fi::note-background-reply)
  (when fi::show-some-text-1-first-time
    (message
     "%s"
     (substitute-command-keys
      "Type \\[fi:lisp-delete-pop-up-window] to remove *CL-temp* window."))
    (setq fi::show-some-text-1-first-time nil)))

(defun fi::show-some-text-replace (text package &optional hook &rest args)
  (let ((buffer (get-buffer-create "*CL-temp*")))
    
    ;; fill the buffer
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (fi:common-lisp-mode)
      (setq fi:package package)
      (insert text)
      (beginning-of-buffer)
      (setq lines (count-lines (point-min) (point-max))))

    (switch-to-buffer buffer)

    (when hook (apply hook args))))

(defun fi::show-some-text-other (text package &optional hook &rest args)
  (let ((buffer (get-buffer-create "*CL-temp*")))
    
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
    (other-window 1)))

(defun fi::show-some-text-split (text package &optional hook &rest args)
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

;;;;;;;;;;;;;;;;;;;;;;TODO
;;; The filter should not abort because of errors. Some how the errors
;;; should be printed.

(defun fi::make-connection (host process)
  (list ':connection
	process 
	nil				; sessions
	-1				; session id counter
	host))

(defun fi::connection-process (c) (second c))

(defun fi::connection-sessions (c) (third c))
(defun fi::set-connection-sessions (c nv) (setf (third c) nv))

(defun fi::connection-session-id (c) (fourth c))
(defun fi::set-connection-session-id (c nv) (setf (fourth c) nv))

(defun fi::connection-host (c) (fifth c)) ; not used, apparently

(defvar fi::*connection* nil)

(defun fi::lep-open-connection-p ()
  (and fi::*connection*
       (fi:process-running-p (fi::connection-process fi::*connection*))
       fi::*connection*))

(defun fi::ensure-lep-connection () 
  (or (fi::lep-open-connection-p)
      (fi::try-and-start-lep-connection)
      (error "no connection")))

(defun fi:reset-lep-connection ()
  "Reset the Lisp-editor protocol connection."
  (interactive)
  (setq fi::*connection* nil))

;;; Start up a connection as soon as we know where to connect to.


(if (boundp 'fi:start-lisp-interface-hook)
    (push 'fi::auto-ensure-lep-connection fi:start-lisp-interface-hook))

(defun fi::auto-ensure-lep-connection ()
  (message "Trying to start connection...")
  (fi::ensure-lep-connection)
  (message "Trying to start connection...done."))

(defun fi::try-and-start-lep-connection ()
  (let ((buffer (process-buffer (progn
				  (fi::sublisp-select)
				  (get-process fi::process-name)))))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (fi::start-connection))
      (fi:error "
An internal error has occurred.  There is no Lisp process and the
emacs-lisp interface cannot be started.
"))))

(defun fi::find-connection-from-process (process)
  fi::*connection*)

(defun fi::find-session (connection id)
  "Search a CONNECTION looking for a session with the ID"
  (let ((sessions (fi::connection-sessions connection)))
    (while (and sessions (not (eq (fi::session-id (car sessions)) id)))
      (setq sessions (cdr sessions)))
    (and sessions (car sessions))))

(defun fi::start-connection ()
  (fi::make-connection-to-lisp fi::lisp-host
			       fi::lisp-port
			       fi::lisp-password
			       fi::lisp-ipc-version))

(defun fi::make-connection-to-lisp (host port passwd ipc-version)
  (cond ((eq ipc-version fi::required-ipc-version)
	 (let* ((proc-name (format "*LEP %s %d %d*" host port passwd))
		(buffer-name (if fi::lep-debug proc-name))
		(buffer (when buffer-name
			  (get-buffer-create buffer-name)))
		(process (open-network-stream proc-name nil host port)))
	   (when buffer
	     (bury-buffer buffer)
	     (save-excursion (set-buffer buffer) (erase-buffer))
	     (set-process-buffer process buffer))
	   (set-process-filter process 'fi::lep-connection-filter)
	   ;; new stuff to indicate that we want the lisp editor protocol
	   (send-string process ":lep\n")
	   (send-string process (format "\"%s\"\n" proc-name))
	   (send-string process (format "%d \n" passwd))
	   ;; Send the class of the editor to the lisp.
	   ;; This might affect something!
	   ;; For example, gnu 19 has some good features.
	   (send-string process (format "\"%s\"\n" (emacs-version)))
	   (setq fi::*connection* (fi::make-connection host process))))
	(t
	 (fi:error
	  "
The Allegro CL ipc version is ``%s'' (from the variable ipc::*ipc-version*
in the Lisp environment).  This version of the emacs-lisp interface
requires version ``%s''.  This mismatch would most likely be caused by the
Emacs and Lisp not being from the same distribution.  If the obtained ipc
version is `nil', then it is most likely you are using the emacs-lisp
interface from ACL 4.1 or later with an older Lisp.

See lisp/fi/examples/emacs.el for code to correctly startup different
versions of the emacs-lisp interface.
"
	  ipc-version fi::required-ipc-version))))

(defun fi::lep-connection-filter (process string)
  ;; When a complete sexpression comes back from the lisp, read it and then
  ;; handle it
  (let ((inhibit-quit t)
	(buffer (or (process-buffer process)
		    (get-buffer-create " LEP temp "))))
    (when fi::trace-lep-filter
      (print string (get-buffer "*scratch*")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (insert string))
    (let (form)
      (while 
	  (condition-case ignore
	      (save-excursion
		(set-buffer buffer)
		(and 
		 (not (eq (point-max) (point-min)))
		 (progn
		   (goto-char (point-min))
		   (forward-sexp)
		   (let ((p (point)))
		     (goto-char (point-min))
		     (condition-case ignore 
			 (progn (setq form (read (current-buffer))) t)
		       (error (setq form nil)))
		     (delete-region (point-min) p))
		   t)))
	    (error nil))
	(if form (fi::handle-lep-input process form))))))

(defun fi::handle-lep-input (process form)
  "A reply is (session-id . rest) or (nil . rest)"
  (when fi::trace-lep-filter
    (print (list process form) (get-buffer "*scratch*")))
  (let* ((id (car form))
	 (connection (fi::find-connection-from-process process))
	 (session (fi::find-session connection id)))
    (cond (session
	   (fi::handle-session-reply session (cdr form)))
	  ((car form)
	   (error "something for nonexistent session: %s" form))
	  (t (fi::handle-sessionless-reply (cdr form))))))

(defun fi::handle-session-reply (session form)
  "A session reply is (:error Message) or (nil . results)"
  (when (fi::session-oncep session)
    (fi::delete-session session))
  (if (eq (car form) ':error)
      (if (fi::session-error-function session)
	  (apply (fi::session-error-function session)
		 (second form)
		 (fi::session-error-arguments session))
	(error (second form)))
    (apply (fi::session-function session)
	   (if (fi::session-arguments session)
	     (append (cdr form) (fi::session-arguments session))
	     (cdr form)))))

(defun fi::delete-session (session)
  (let ((connection (fi::session-connection session)))
    (fi::set-connection-sessions
     connection
     (delq session (fi::connection-sessions connection)))))

(defun fi::handle-sessionless-reply (form)
  "A session less reply is either (:error message) or (nil fn . args)"
  (cond ((eq (car form) ':error)
	 (error (second form)))
	((eq (car form) ':request)
	 (condition-case error
	     (apply (fi::intern-it (second form)) (cddr form))
	   (error 
	    (message (concat "Request error: " (prin1-to-string error))))))
	(t (error "Funny request received: %s" form))))



(defun fi::make-session (id oncep &optional fn args error-fn error-args)
  (list 'session id fn args error-fn error-args oncep))

(defun fi::session-id (s) (second s))

(defun fi::session-function (s) (third s))
(defun fi::set-session-function (s nv) (setf (third s) nv))


(defun fi::session-arguments (s) (fourth s))
(defun fi::set-session-arguments (s nv) (setf (fourth s) nv))

(defun fi::session-error-function (s) (fifth s))
(defun fi::set-session-error-function (s nv) (setf (fifth s) nv))

(defun fi::session-error-arguments (s) (sixth s))
(defun fi::set-session-error-arguments (s nv) (setf (sixth s) nv))

(defun fi::session-oncep (s) (seventh s))

(defun fi::session-connection (s) fi::*connection*)

(defun fi::modify-session-continuation (session continuation-and-arguments
					error-continuation)
  (fi::set-session-function session (car continuation-and-arguments))
  (fi::set-session-arguments session (cdr continuation-and-arguments))
  (fi::set-session-error-function session (car error-continuation))
  (fi::set-session-error-arguments session (cdr error-continuation)))

(defun fi::make-new-session (connection oncep continuation-and-arguments
			     &optional error-continuation)
  (let* ((id (fi::connection-session-id connection))
	 (session (fi::make-session id 
				oncep
				(car continuation-and-arguments)
				(cdr continuation-and-arguments)
				(car error-continuation)
				(cdr error-continuation))))
    (fi::set-connection-session-id connection (1- id))
    (fi::add-session connection session)
    session))

(defun fi::remove-session (connection session)
  (fi::set-connection-sessions
   connection
   (delq session (fi::connection-sessions connection))))

(defun fi::add-session (connection session)
  (fi::set-connection-sessions
   connection
   (cons session (fi::connection-sessions connection))))



(defun lep::handle-query (session function arguments)
  (lep::send-back-reply session (apply function arguments)))


(defun lep::send-request-in-new-session (session-class oncep session-arguments 
					 continuation-and-arguments
					 &optional error-continuation-and-arguments
						   ignore-package)
  (unless (fi::lep-open-connection-p)
    (error "There is no connection to Lisp.  See fi:common-lisp documentation."))
  
  (let* ((connection (fi::ensure-lep-connection))
	 (session (fi::make-new-session connection oncep
					continuation-and-arguments
					error-continuation-and-arguments))
	 (process (fi::connection-process connection)))
    (send-string process (prin1-to-string 
			  (list* nil
				 'lep::make-session session-class 
				 ':session-id (fi::session-id session)
				 (if (or ignore-package
					 (fi::member-plist ':buffer-package
							   session-arguments))
				     session-arguments 
				   (list* ':buffer-package
					  (fi::string-to-keyword fi:package)
					  session-arguments)))))
    (send-string process "\n")
    session))

(defmacro fi::make-request (type-and-options continuation
			&optional error-continuation
				  ignore-package)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	t
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list
	       (list 'function
		     (list* 'lambda
			    (append (fi::listify (second continuation))
				    (fi::listify (first continuation)))
			    (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda (append (fi::listify
						   (second error-continuation))
						  (fi::listify
						   (first error-continuation)))
				  (cddr error-continuation)))
	       (first error-continuation))
	ignore-package))


(defmacro fi::make-complex-request (type-and-options continuation
				&optional error-continuation)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	nil
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (fi::listify (second continuation))
					  (fi::listify (first continuation)))
					 (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (fi::listify
					   (second error-continuation))
					  (fi::listify (first error-continuation)))
					 (cddr error-continuation)))
	       (first error-continuation))))

(defun lep::send-request-in-existing-session (session session-class oncep
					      session-arguments 
					      continuation-and-arguments
					      &optional error-continuation-and-arguments)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (send-string process
		 (prin1-to-string
		  (list* (fi::session-id session) session-class
			 session-arguments)))
    (send-string process "\n")))

(defun lep::kill-session (session)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (fi::remove-session connection session)
    (send-string process (prin1-to-string
			  (list nil 'lep::terminate-session
				(fi::session-id session))))
    (send-string process "\n")))

(defun lep::send-request-in-session (session session-class session-arguments 
				     continuation-and-arguments
				     &optional error-continuation-and-arguments)
  (let* ((connection (fi::ensure-lep-connection))
	 (process (fi::connection-process connection )))
    (fi::modify-session-continuation 
     session continuation-and-arguments error-continuation-and-arguments)
    (send-string process
		 (prin1-to-string (list* (fi::session-id session)
					 ':request
					 session-class 
					 session-arguments)))
    (send-string process "\n")))

(defmacro fi::make-request-in-existing-session (session type-and-options 
						continuation
						&optional error-continuation)
  (list 'lep::send-request-in-session
	session
	(list 'quote (car type-and-options))
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function 
			   (list* 'lambda 
				  (append (fi::listify (second continuation))
					  (fi::listify (first continuation)))
				  (cddr continuation)))
	       (first continuation))
	(list* 'list
	       (list 'function 
		     (list* 'lambda (append
				     (fi::listify (second error-continuation))
				     (fi::listify (first error-continuation)))
			    (cddr error-continuation)))
	       (first error-continuation))))

(defun fi::intern-it (s)
  (if (stringp s) (intern s) s))

(defun lep::make-session-for-lisp (session-id replyp oncep function &rest args)
  (let ((session (fi::make-session session-id nil))
	(done nil))
    (fi::add-session connection session)
    (unwind-protect
	(progn
	  (condition-case error
	      (let* ((done nil)
		     (result (apply (fi::intern-it function) args)))
		(when  replyp
		  (send-string process 
			       (prin1-to-string (list* (fi::session-id session)
						       ':reply
						       result)))
		  (send-string process "\n")))
	    (error 
	     (if replyp
		 (progn
		   (send-string process 
				(prin1-to-string
				 (list (fi::session-id session)
				       ':error
				       (prin1-to-string error))))
	 
		   (send-string process "\n"))
	       (message (concat "Error occurred: " (prin1-to-string error))))))
	  (setq done t))
      (unless done
	(send-string process 
		     (prin1-to-string (list (fi::session-id session)
					    ':error
					    ':aborted))))
      (if oncep (lep::kill-session session)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:lisp-evalserver-timeout 5
  "The time which fi:eval-in-lisp will wait before timing out and
signalling an error.  Without a timeout Emacs would potentially be locked
out if Lisp did not `return' a result.")

(defvar fi:lisp-evalserver-number-reads 20
  "The number of times the Lisp eval server tries to read from the
lisp-evalserver process before giving up.  Without this feature Emacs would
hang if Lisp got into an infinite loop while printing.  If the size of the
values returned to Emacs is large, then the value of this variable should
be increased.")

(defun fi:eval-in-lisp (string &rest args)
  "Apply (Emacs Lisp) format to STRING and ARGS and sychronously evaluate
the result in the Common Lisp to which we are connected.  If a
lisp-eval-server has not been started, then this function starts it."
  (if (not (fi::lep-open-connection-p))
      (let ((i 0) (max 20))
	(while (and (< i max)
		    fi::common-lisp-backdoor-main-process-name
		    (get-process fi::common-lisp-backdoor-main-process-name)
		    (not (fi::lep-open-connection-p))
		    (progn (sleep-for 3) t))
	  (setq i (+ i 1)))))
  (let ((string (if args
		    (apply 'format string args)
		  string)))
    (car (lep::eval-session-in-lisp 'lep::eval-from-emacs-session
				    ':string (fi::frob-case-to-lisp string)))))

(defun lep::eval-session-in-lisp (function &rest arguments)
  (let* ((result-cons (list nil nil nil))
	 (session (lep::send-request-in-new-session
		   function
		   t
		   arguments
		   (list (function fi::immediate-reply-continuation)
			 result-cons)
		   (list (function fi::immediate-reply-error-continuation)
			 result-cons))))
    (fi::wait-for-reply-to-come-back result-cons)))

(defun fi::immediate-reply-continuation (&rest results)
  (let ((r (butlast results))
	(c (car (last results))))
    (fi::stash-in-cons c r nil)))

(defun fi::immediate-reply-error-continuation (error result-cons)
  (fi::stash-in-cons result-cons error t))

(defun fi::stash-in-cons (c r p)
  (setf (third c) p)
  (setf (second c) r)
  (setf (first c) t))

;;; This is complicated because we have to wait for output multiple times.

(defun fi::wait-for-reply-to-come-back (result-cons)
  (when (not (car result-cons))
    (let ((count fi:lisp-evalserver-number-reads))
      (while (and (> (setq count (1- count)) 0)
		  (null (car result-cons)))
	(accept-process-output 
	 (fi::connection-process (fi::ensure-lep-connection))
	 fi:lisp-evalserver-timeout)))
    (when (not (car result-cons)) (error "Eval in lisp timed out"))
    (if (third result-cons)
	(error (second result-cons))
      (second result-cons))))

(defun lep::make-request-in-session-and-wait (session function &rest arguments)
  "Send a request to SESSION consisting of FUNCTION and ARGUMENTS and wait
for the reply to come back. It returns a list of values."
  (let* ((result-cons (list nil nil nil))
	 (session (lep::send-request-in-session
		   session
		   function
		   arguments
		   (list (function fi::immediate-reply-continuation)
			 result-cons)
		   (list (function fi::immediate-reply-error-continuation)
			 result-cons))))
    (fi::wait-for-reply-to-come-back result-cons)))
