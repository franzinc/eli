;; See the file LICENSE for the full license governing this code.

(declare-function fi::note-background-reply "fi-lze")

;; The basic lep code that implements connections and sessions

(defun fi::show-error-text (format-string &rest args)
  (if (cdr fi:pop-up-temp-window-behavior)
      (apply 'fi:show-some-text nil format-string args)
    (let ((fi:pop-up-temp-window-behavior
	   (cons (car fi:pop-up-temp-window-behavior) 't)))
      (apply 'fi:show-some-text nil format-string args))))

(defun fi:show-some-text (xpackage text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let ((n (string-match "[\n]+\\'" text)))
    (when n (setq text (substring text 0 n))))
  (if (and (not (eq 'minibuffer (car fi:pop-up-temp-window-behavior)))
	   (null (cdr fi:pop-up-temp-window-behavior)))
      (fi::show-some-text-1 text (or xpackage (fi::package)))
    (let* ((window (minibuffer-window))
	   (width (window-width window))
	   (lines/len (fi::frob-string text)))
      (if (or (and (eq 'minibuffer (car fi:pop-up-temp-window-behavior))
		   (fi::will-fit-in-minibuffer-p text))
	      (and (< (car lines/len) 2)
		   (<= (cl-second lines/len) width)))
	  (progn
	    (message "%s" text)
	    (fi::note-background-reply))
	(fi::show-some-text-1 text (or xpackage (fi::package)))))))

;; would have been nice to use display-message-or-buffer...
(defun fi::will-fit-in-minibuffer-p (string)
  (let ((lines (fi::count-lines-in-string string)))
    (if (or (<= lines 1)
	    (<= lines
		(if (and (boundp 'resize-mini-windows)
			 resize-mini-windows)
		    (cond ((floatp max-mini-window-height)
			   (* (frame-height) max-mini-window-height))
			  ((integerp max-mini-window-height)
			   max-mini-window-height)
			  (t 1))
		  1)))
	t
      nil)))

(defun fi::count-lines-in-string (string)
  (with-temp-buffer
      (progn
	(insert string)
	(count-lines (point-min) (point-max)))))

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

(defun fi::show-some-text-1 (text apackage &optional hook &rest args)
  ;;A note from cer:
  ;;  I think there is some buffer local variable called package
  ;;  Outside of the save-excursion package has the correct value.
  ;;  Inside it has the value of NIL.
  ;;  Consequently fi:package ends up as NIL in *CL-temp* buffers.
  ;;  I renamed package to apackage and that seemed to fix it.
  (let ((buffer (get-buffer-create fi::*show-some-text-buffer-name*)))
    ;; fill the buffer
    (with-current-buffer buffer
      (erase-buffer)
      (fi:common-lisp-mode)
      (setq fi:package apackage)
      (insert text)
      (goto-char (point-min)))
    (fi::display-pop-up-window buffer hook args))
  (fi::note-background-reply)
  (when fi::show-some-text-1-first-time
    (message
     "%s"
     (substitute-command-keys
      (format
       "Type \\[fi:lisp-delete-pop-up-window] to remove %s window."
       fi::*show-some-text-buffer-name*)))
    (setq fi::show-some-text-1-first-time nil)))

;;;;;;;;;;;;;;;;;;;;;;TODO
;;; The filter should not abort because of errors. Some how the errors
;;; should be printed.

(defun fi::make-connection (buffer host process)
  (list ':connection
	process
	nil				; sessions
	-1				; session id counter
	host
	buffer))

(defun fi::connection-process (c) (cl-second c))

(defun fi::connection-sessions (c) (cl-third c))
(defun fi::set-connection-sessions (c nv) (setf (cl-third c) nv))

(defun fi::connection-session-id (c) (cl-fourth c))
(defun fi::set-connection-session-id (c nv) (setf (cl-fourth c) nv))

(defun fi::connection-host (c) (cl-fifth c)) ; not used, apparently

(defun fi::connection-buffer (c) (cl-sixth c))

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
  (when (fboundp 'set-menubar-dirty-flag)
    (set-menubar-dirty-flag))
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
	(with-current-buffer buffer
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
			       fi::lisp-password))


(defun fi::coding-system-name (cs)
  (when (listp cs)
    (setq cs (car cs)))
  (when (fboundp 'coding-system-name)	; xemacs only
    (setq cs (coding-system-name cs)))
  cs)

;; spr30075
;; Notes: eli on xemacs requires the emacs-base package.  The mule-sumo should
;; also be installed for xemacs with mule to provide mule-ucs.
;;
;; Load mule-ucs if available.  This defines a good utf-8.
;; 
(condition-case nil (require 'un-define) (error nil))

(defun fi::lisp-connection-coding-system ()
  fi::default-lisp-connection-coding-system)

(defun fi::make-connection-to-lisp (host port passwd)
  (let* ((proc-name (format " *LEP %s %d %d*" host port passwd))
	 ;; buffer-name used to be non-nil only when fi::lep-debug
	 ;; was non-nil, but changes to lep::make-session-for-lisp
	 ;; depend on there being a buffer associated with the
	 ;; process.  So, we now do this in all cases.
	 (buffer-name proc-name)
	 (buffer (when buffer-name
		   (get-buffer-create buffer-name)))
	 (process (fi::open-network-stream proc-name nil host port))
	 (coding-system (fi::lisp-connection-coding-system)))
    (when buffer
      (bury-buffer buffer)
      (with-current-buffer buffer (erase-buffer))
      (set-process-buffer process buffer))
    (when coding-system 
      ;; bug12456 -mikel
      (fi::set-process-coding process coding-system))
    (set-process-filter process 'fi::lep-connection-filter)
    ;; new stuff to indicate that we want the lisp editor protocol
    (process-send-string process ":lep\n")
    (process-send-string process (format "\"%s\"\n" proc-name))
    (process-send-string process (format "%d \n" passwd))
    ;; Send the class of the editor to the lisp.
    ;; This might affect something!
    ;; For example, gnu 19 has some good features.
;;;; The following works in xemacs 20.x when this file is compiled
;;;; with emacs 19.x, but we don't want to install this hack since
;;;; there are hundreds of other places a similar hack would have to
;;;; be installed.
    (let ((version-string (remove ?\" (emacs-version))))
      (process-send-string
       process
       (if coding-system
	   (format "\"%s :external-format %s\"\n" version-string coding-system)
	 (format "\"%s\"\n" version-string))))
    (prog1
	(setq fi::*connection*
	  (fi::make-connection (current-buffer) host process))
      (when (fboundp 'set-menubar-dirty-flag)
	(set-menubar-dirty-flag)))))

(defun fi::lep-connection-filter (process string)
  ;; When a complete sexpression comes back from the lisp, read it and then
  ;; handle it
  (when fi::debug-subprocess-filter
    (push string fi::debug-subprocess-filter-output))
  (let ((inhibit-quit t)
	(buffer (or (process-buffer process)
		    (get-buffer-create " LEP temp "))))
    (when fi::trace-lep-filter
      (fi::trace-debug (format "lep-connection-filter: %s" string)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert string))
    (let (form error)
      (while
	  (condition-case nil
	      (with-current-buffer buffer
		(and
		 (null error)
		 (not (eq (point-max) (point-min)))
		 (progn
		   (goto-char (point-min))
		   (forward-sexp)
		   (let ((p (point)))
		     (goto-char (point-min))
		     (condition-case nil
			 (progn (setq form (read (current-buffer))) t)
		       (error (setq error t)))
		     (delete-region (point-min) p))
		   t)))
	    (error nil))
	(if error
	    (error "error reading return value: %s" string)
	  (fi::handle-lep-input process form))))))

(defun fi::trace-debug (string)
  (with-current-buffer (get-buffer-create "*LEP-DEBUG*")
    (goto-char (point-max))
    ;; Print the time w/microseconds:
    (insert (format-time-string "%n%T %6N: "))
    (insert string)
    (insert "\n")))

(defun fi::handle-lep-input (eli--process form)
  "A reply is (session-id . rest) or (nil . rest)"
  (when fi::trace-lep-filter
    (fi::trace-debug 
     (format "handle-lep-input:\n  process: %s\n  form: %s"
		   eli--process form)))
  (let* ((id (car form))
	 (eli--connection (fi::find-connection-from-process eli--process))
	 (eli--session (fi::find-session eli--connection id)))
    (cond (eli--session
	   (fi::handle-session-reply eli--session (cdr form)))
	  ((car form)
	   (error "something for nonexistent session: %s" form))
	  (t (fi::handle-sessionless-reply (cdr form))))))

(defun fi::handle-session-reply (eli--session form)
  "A session reply is (:error Message) or (nil . results)"
  (when (fi::session-oncep eli--session)
    (fi::delete-session eli--session))
  (if (eq (car form) ':error)
      (if (fi::session-error-function eli--session)
	  (apply (fi::session-error-function eli--session)
		 (cl-second form)
		 (fi::session-error-arguments eli--session))
	(error (cl-second form)))
    (apply (fi::session-function eli--session)
	   (if (fi::session-arguments eli--session)
	     (append (cdr form) (fi::session-arguments eli--session))
	     (cdr form)))))

(defun fi::delete-session (session)
  (let ((connection (fi::session-connection session)))
    (fi::set-connection-sessions
     connection
     (delq session (fi::connection-sessions connection)))))

(defun fi::handle-sessionless-reply (form)
  ;; A session-less reply is either (:error message) or (:request fn . args)
  (cond ((eq (car form) ':error)
	 (error (cl-second form)))
	((eq (car form) ':request)
	 (condition-case error
	     (apply (fi::intern-it (cl-second form)) (cddr form))
	   (error
	    (fi::show-error-text "Request error: %s"
				 (fi::prin1-to-string error)))))
	(t (error "Funny request received: %s" form))))


(defun fi::make-session (id oncep &optional fn args error-fn error-args)
  (list 'session id fn args error-fn error-args oncep))

(defun fi::session-id (s) (cl-second s))

(defun fi::session-function (s) (cl-third s))
(defun fi::set-session-function (s nv) (setf (cl-third s) nv))


(defun fi::session-arguments (s) (cl-fourth s))
(defun fi::set-session-arguments (s nv) (setf (cl-fourth s) nv))

(defun fi::session-error-function (s) (cl-fifth s))
(defun fi::set-session-error-function (s nv) (setf (cl-fifth s) nv))

(defun fi::session-error-arguments (s) (cl-sixth s))
(defun fi::set-session-error-arguments (s nv) (setf (cl-sixth s) nv))

(defun fi::session-oncep (s) (cl-seventh s))

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



;;(defun lep::handle-query (session function arguments)
;;  (lep::send-back-reply session (apply function arguments)))


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
    (process-send-string process (fi::prin1-to-string
			  (cl-list* nil
				    'lep::make-session session-class
				    ':session-id (fi::session-id session)
				    ':buffer-readtable (fi::string-to-keyword
							fi:readtable)
				    (if (or ignore-package
					    (fi::member-plist ':buffer-package
							      session-arguments))
					session-arguments
				      (cl-list* ':buffer-package
						(fi::string-to-keyword
						 (fi::package))
						session-arguments)))))
    (process-send-string process "\n")
    session))

(defmacro fi::make-request (type-and-options continuation
			    &optional error-continuation ignore-package)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	t
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(cl-list* 'list
		  (list 'function
			(cl-list* 'lambda (append (fi::listify (cl-second continuation))
						  (fi::listify (cl-first continuation)))
				  (cddr continuation)))
		  (cl-first continuation))
	(cl-list* 'list
		  (list 'function
			(cl-list* 'lambda
				  (append (fi::listify (cl-second error-continuation))
					  (fi::listify (cl-first error-continuation)))
				  (cddr error-continuation)))
		  (cl-first error-continuation))
	ignore-package))


(defmacro fi::make-complex-request (type-and-options continuation
				    &optional error-continuation)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	nil
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(cl-list* 'list (list 'function
			      (cl-list* 'lambda
					(append (fi::listify (cl-second continuation))
						(fi::listify (cl-first continuation)))
					(cddr continuation)))
		  (cl-first continuation))
	(cl-list* 'list
		  (list 'function
			(cl-list* 'lambda
				  (append (fi::listify (cl-second error-continuation))
					  (fi::listify (cl-first error-continuation)))
				  (cddr error-continuation)))
		  (cl-first error-continuation))))

(defun lep::send-request-in-existing-session (session session-class oncep
					      session-arguments
					      continuation-and-arguments
					      &optional error-continuation-and-arguments)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (process-send-string process
		 (fi::prin1-to-string
		  (cl-list* (fi::session-id session) session-class session-arguments)))
    (process-send-string process "\n")))

(defun lep::kill-session (session)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (fi::remove-session connection session)
    (process-send-string process (fi::prin1-to-string
			  (list nil 'lep::terminate-session
				(fi::session-id session))))
    (process-send-string process "\n")))

(defun lep::send-request-in-session (session session-class session-arguments
				     continuation-and-arguments
				     &optional error-continuation-and-arguments)
  (let* ((connection (fi::ensure-lep-connection))
	 (process (fi::connection-process connection)))
    (fi::modify-session-continuation
     session continuation-and-arguments error-continuation-and-arguments)
    (process-send-string process
		 (fi::prin1-to-string (cl-list* (fi::session-id session)
						':request
						session-class
						session-arguments)))
    (process-send-string process "\n")))

(defmacro fi::make-request-in-existing-session (session type-and-options
						continuation
						&optional error-continuation)
  (list 'lep::send-request-in-session
	session
	(list 'quote (car type-and-options))
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(cl-list* 'list
		  (list 'function
			(cl-list* 'lambda
				  (append (fi::listify (cl-second continuation))
					  (fi::listify (cl-first continuation)))
				  (cddr continuation)))
		  (cl-first continuation))
	(cl-list* 'list
		  (list 'function
			(cl-list* 'lambda
				  (append (fi::listify (cl-second error-continuation))
					  (fi::listify (cl-first error-continuation)))
				  (cddr error-continuation)))
		  (cl-first error-continuation))))

(defun fi::intern-it (s)
  (if (stringp s) (intern s) s))

(defun lep::make-session-for-lisp (session-id replyp oncep function &rest args)
  (let ((eli--session (fi::make-session session-id nil))
	(done nil)
	(dead (or (eq 'closed (process-status eli--process))
		  ;; The following test relies on
		  ;; fi::make-connection-to-lisp associating the process
		  ;; with a buffer IN ALL CASES, not just for debugging.
		  (null (process-buffer eli--process)))))
    (fi::add-session eli--connection eli--session)
    (unwind-protect
	(when (not dead)
	  (condition-case error
	      (let* ((result (apply (fi::intern-it function) args)))
		(when replyp
		  (process-send-string eli--process
				       (fi::prin1-to-string
					(cl-list* (fi::session-id eli--session)
						  ':reply
						  result)))
		  (process-send-string eli--process "\n")))
	    (error
	     (if replyp
		 (progn
		   (process-send-string eli--process
					(fi::prin1-to-string
					 (list (fi::session-id eli--session)
					       ':error
					       (fi::prin1-to-string error))))

		   (process-send-string eli--process "\n"))
	       (fi::show-error-text
		"Error %s in %s\nwith args: %s\nstack dump:\n%s"
		(fi::prin1-to-string
		 (if (and (consp error) (cdr error))
		     (cdr error)
		   error))
		function
		args
		(when (fboundp 'backtrace)
		  (with-output-to-string (backtrace)))))))
	  (setq done t))
      (when (and (not dead) (not done))
	(process-send-string eli--process
			     (fi::prin1-to-string (list (fi::session-id eli--session)
							':error
							':aborted))))
      (when (or dead oncep)
	(lep::kill-session eli--session)))))

(defun fi:send-reply (session string)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (process-send-string process
		 (fi::prin1-to-string
		  (list (fi::session-id session) ':reply string)))
    (process-send-string process "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::eval-in-lisp-wait-for-connection ()
  (if (not (fi::lep-open-connection-p))
      (let ((i 0) (max 20))
	(while (and (< i max)
		    fi::common-lisp-backdoor-main-process-name
		    (get-process fi::common-lisp-backdoor-main-process-name)
		    (not (fi::lep-open-connection-p))
		    (progn (sleep-for 3) t))
	  (setq i (+ i 1))))))

(defun fi:eval-in-lisp-asynchronous (string &rest args)
  "Apply (Emacs Lisp) format to STRING and ARGS and asychronously evaluate
the result in the Common Lisp to which we are connected."
  (fi::eval-in-lisp-wait-for-connection)
  (let ((string (if args (apply 'format string args) string)))
    (fi::make-request
	;;fi::frob-case-to-lisp removed - 18jan94 smh
	(lep::eval-from-emacs-session :string string)
      ;; Normal continuation
      (() (value)
       ;; ignore the value...
       nil)
      ((string) (error)
       (fi::show-error-text "error evaluating %s: %s" string error)))))


(defun fi:eval-in-lisp (string &rest args)
  "Apply (Emacs Lisp) format to STRING and ARGS and sychronously evaluate
the result in the Common Lisp to which we are connected."
  (fi::eval-in-lisp-wait-for-connection)
  (let ((string (fi::defontify-string
		    (if args (apply 'format string args) string))))
    ;;fi::frob-case-to-lisp removed - 18jan94 smh
    (car (lep::eval-session-in-lisp 'lep::eval-from-emacs-session
				    ':string string))))

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
  (setf (cl-third c) p)
  (setf (cl-second c) r)
  (setf (cl-first c) t))

;;; This is complicated because we have to wait for output multiple times.

(defun fi::wait-for-reply-to-come-back (result-cons)
  (when (not (car result-cons))
    (let ((count fi:lisp-evalserver-number-reads))
      (while (and (> (setq count (1- count)) 0)
		  (null (car result-cons)))
	(accept-process-output
	 (fi::connection-process (fi::ensure-lep-connection)))))
    (when (not (car result-cons)) (error "Eval in lisp timed out"))
    (if (cl-third result-cons)
	(error (cl-second result-cons))
      (cl-second result-cons))))

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
