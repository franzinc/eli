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
;; $Header: /repo/cvs.copy/eli/fi-basic-lep.el,v 1.8 1991/03/13 15:03:30 layer Exp $
;;
;; The basic lep code that implements connections and sessions

;;;;;;;;;;;;;;;;;;;;;;TODO
;;; The filter should not abort because of errors. Some how the errors
;;; should be printed.

(defun make-connection (host process)
  (list ':connection
	process 
	nil				; sessions
	-1				; session id counter
	host))

(defun connection-process (c) (second c))

(defun connection-sessions (c) (third c))
(defun set-connection-sessions (c nv) (setf (third c) nv))

(defun connection-session-id (c) (fourth c))
(defun set-connection-session-id (c nv) (setf (fourth c) nv))

(defun connection-host (c) (fifth c))

(defvar lep::*connection* nil)

(defun lep::lep-open-connection-p ()
  (and lep::*connection*
       (fi:process-running-p (connection-process lep::*connection*))
       lep::*connection*))

(defun ensure-lep-connection () 
  (or (lep::lep-open-connection-p)
      (try-and-start-lep-connection)
      (error "no connection")))

;;; Start up a connection as soon as we know where to connect to.


(if (boundp 'fi:start-lisp-interface-hook)
    (push 'auto-ensure-lep-connection fi:start-lisp-interface-hook))

(defun auto-ensure-lep-connection ()
  (message "Trying to start connection...")
  (ensure-lep-connection)
  (message "Trying to start connection...done."))

(defun try-and-start-lep-connection ()
  (let ((buffer (process-buffer (progn
				  (fi::sublisp-select)
				  (get-process fi::process-name)))))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (start-connection))
      (error "Cannot start LEP connection"))))

(defun find-connection-from-process (process)
  lep::*connection*)

(defun find-session (connection id)
  "Search a CONNECTION looking for a session with the ID"
  (let ((sessions (connection-sessions connection)))
    (while (and sessions (not (eq (session-id (car sessions)) id)))
      (setq sessions (cdr sessions)))
    (and sessions (car sessions))))

(defun start-connection ()
  (interactive)
  (make-connection-to-lisp fi::lisp-host
			   fi::lisp-port
			   fi::lisp-password
			   fi::lisp-ipc-version))

(defun make-connection-to-lisp (host port passwd ipc-version)
  (cond ((not (or  (eq ipc-version 'nil) (eq ipc-version 'NIL)))
	 (let* ((buffer-name (format "*LEP buffer %s %d %d*" host port passwd))
		(buffer (get-buffer-create buffer-name))
		(process (open-network-stream buffer-name nil host port)))
	   (save-excursion (set-buffer buffer) (erase-buffer))
	   (set-process-buffer process buffer)
	   (set-process-filter process 'connection-filter)
	   ;; new stuff to indicate that we want the lisp editor protocol
	   (send-string process ":lep\n")
	   (send-string process (format "\"%s\"\n" (buffer-name buffer)))
	   (send-string process (format "%d \n" passwd))
	   ;; Send the class of the editor to the lisp.
	   ;; This might affect something!
	   ;; For example, gnu 19 has some good features.
	   (send-string process (format "\"%s\"\n" (emacs-version)))
	   (setq lep::*connection* (make-connection host process))))
	(t (error "Cannot start LEP to this lisp - incorrect IPC version"))))

(defvar lep::trace-lep-filter nil)

(defun connection-filter (process string)
  "When a complete sexpression comes back from the lisp, read it and then
handle it"
  (when lep::trace-lep-filter
    (print string (get-buffer "*scratch*")))
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string))
  (let (form)
    (while 
	(condition-case ignore
	    (save-excursion
	      (set-buffer (process-buffer process))
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
      (if form (handle-input process form)))))

(defun handle-input (process form)
  "A reply is (session-id . rest) or (nil . rest)"
  (when lep::trace-lep-filter
    (print (list process form) (get-buffer "*scratch*")))
  (let* ((id (car form))
	 (connection (find-connection-from-process process))
	 (session (find-session connection id)))
    (cond (session
	   (handle-session-reply session (cdr form)))
	  ((car form)
	   (error "something for nonexistent session: %s" form))
	  (t (handle-sessionless-reply (cdr form))))))

(defun handle-session-reply (session form)
  "A session reply is (:error Message) or (nil . results)"
  (when (session-oncep session)
    (delete-session session))
  (if (eq (car form) ':error)
      (if (session-error-function session)
	  (apply (session-error-function session)
		 (second form)
		 (session-error-arguments session))
	(error (second form)))
    (apply (session-function session)
	   (if (session-arguments session)
	     (append (cdr form) (session-arguments session))
	     (cdr form)))))

(defun delete-session (session)
  (let ((connection (session-connection session)))
    (set-connection-sessions connection
			     (delq session (connection-sessions connection)))))

(defun handle-sessionless-reply (form)
  "A session less reply is either (:error message) or (nil fn . args)"
  (cond ((eq (car form) ':error)
	 (error (second form)))
	((eq (car form) ':request)
	 (condition-case error
	     (apply (intern-it (second form)) (cddr form))
	   (error 
	    (message (concat "Request error: " (prin1-to-string error))))))
	(t (error "Funny request received: %s" form))))



(defun make-session (id oncep &optional fn args error-fn error-args)
  (list 'session id fn args error-fn error-args oncep))

(defun session-id (s) (second s))

(defun session-function (s) (third s))
(defun set-session-function (s nv) (setf (third s) nv))


(defun session-arguments (s) (fourth s))
(defun set-session-arguments (s nv) (setf (fourth s) nv))

(defun session-error-function (s) (fifth s))
(defun set-session-error-function (s nv) (setf (fifth s) nv))

(defun session-error-arguments (s) (sixth s))
(defun set-session-error-arguments (s nv) (setf (sixth s) nv))

(defun session-oncep (s) (seventh s))

(defun session-connection (s) lep::*connection*)

(defun modify-session-continuation  (session continuation-and-arguments
				     error-continuation)
  (set-session-function session (car continuation-and-arguments))
  (set-session-arguments session (cdr continuation-and-arguments))
  (set-session-error-function session (car error-continuation))
  (set-session-error-arguments session (cdr error-continuation)))

(defun make-new-session (connection oncep continuation-and-arguments
			 &optional error-continuation)
  (let* ((id (connection-session-id connection))
	 (session (make-session id 
				oncep
				(car continuation-and-arguments)
				(cdr continuation-and-arguments)
				(car error-continuation)
				(cdr error-continuation))))
    (set-connection-session-id connection (1- id))
    (add-session  connection session)
    session))

(defun remove-session (connection session)
  (set-connection-sessions connection
			   (delq session (connection-sessions connection))))

(defun add-session (connection session)
  (set-connection-sessions connection
			   (cons session (connection-sessions connection))))



(defun lep::handle-query (session function arguments)
  (lep::send-back-reply session (apply function arguments)))


(defun lep::create-session-for-lisp (session-id type)
  (let ((session (make-session-internal session-internal)))
    (funcall (get type 'session-maker) session)
    (push session *sessions*)))


(defun lep::send-request-in-new-session (session-class oncep session-arguments 
					 continuation-and-arguments
					 &optional error-continuation-and-arguments)
  (let* ((connection (ensure-lep-connection))
	 (session (make-new-session connection oncep
				    continuation-and-arguments
				    error-continuation-and-arguments))
	 (process (connection-process connection)))
    (send-string process (prin1-to-string 
			  (list* nil
				 'lep::make-session session-class 
				 ':session-id (session-id session)
				 (if (member-plist ':buffer-package
						   session-arguments)
				     session-arguments 
				   (list* ':buffer-package
					  (string-to-keyword fi:package)
					  session-arguments)))))
    (send-string process "\n")
    session))

(defun string-to-keyword (package)
  (and package
       (intern (concat ":" package))))

(defun member-plist (prop plist)
  (and plist
       (or (eq (car plist) prop)
	   (member-plist prop (cddr plist)))))


(defmacro make-request (type-and-options continuation
			&optional error-continuation)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	t
	(cons 'list (quote-every-other-one (cdr type-and-options)))
	(list* 'list
	       (list 'function
		     (list* 'lambda
			    (append (listify (second continuation))
				    (listify (first continuation)))
			    (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda (append (listify
						   (second error-continuation))
						  (listify
						   (first error-continuation)))
				  (cddr error-continuation)))
	       (first error-continuation))))

(defun listify (x) (and x (if (atom x) (list x) x)))

(defun quote-every-other-one (list)
  (and list
       (list* (list 'quote (first list)) (second list)
	      (quote-every-other-one (cddr list)))))


;(defun lep:make-session (session-class session-arguments
;			 continuation-and-arguments)
;  "Initiate a session"
;  ;;; send a make-session message
;  )
;
;(defun lep::send-session (message continuation &rest continuation-arguments)
;  "Send a message to a session"
;  nil)


;(defun lep:send-reply (session &rest values)
;  )
;
;(defun lep:send-request (session &rest continuation-and-arguments)
;  )




(defmacro make-complex-request (type-and-options continuation
				&optional error-continuation)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	nil
	(cons 'list (quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (listify (second continuation))
					  (listify (first continuation)))
					 (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (listify
					   (second error-continuation))
					  (listify (first error-continuation)))
					 (cddr error-continuation)))
	       (first error-continuation))))

(defun lep::send-request-in-existing-session (session session-class oncep
					      session-arguments 
					      continuation-and-arguments
					      &optional error-continuation-and-arguments)
  (let* ((connection (session-connection session))
	 (process (connection-process connection)))
    (send-string process
		 (prin1-to-string
		  (list* (session-id session) session-class
			 session-arguments)))
    (send-string process "\n")))

(defun lep::kill-session (session)
  (let* ((connection (session-connection session))
	 (process (connection-process connection)))
    (remove-session connection session)
    (send-string process (prin1-to-string
			  (list nil 'lep::terminate-session
				(session-id session))))
    (send-string process "\n")))

(defun lep::send-request-in-session (session session-class session-arguments 
				     continuation-and-arguments
				     &optional error-continuation-and-arguments)
  (let* ((connection (ensure-lep-connection))
	 (process (connection-process connection )))
    (modify-session-continuation 
     session continuation-and-arguments error-continuation-and-arguments)
    (send-string process
		 (prin1-to-string (list* (session-id session)
					 ':request
					 session-class 
					 session-arguments)))
    (send-string process "\n")))

(defmacro make-request-in-existing-session (session type-and-options 
					    continuation
					    &optional error-continuation)
  (list 'lep::send-request-in-session
	session
	(list 'quote (car type-and-options))
	(cons 'list (quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function 
			   (list* 'lambda 
				  (append (listify (second continuation))
					  (listify (first continuation)))
				  (cddr continuation)))
	       (first continuation))
	(list* 'list
	       (list 'function 
		     (list* 'lambda (append
				     (listify (second error-continuation))
				     (listify (first error-continuation)))
			    (cddr error-continuation)))
	       (first error-continuation))))

(defun intern-it (s)
  (if (stringp s) (intern s) s))

(defun lep::make-session-for-lisp (session-id replyp oncep function &rest args)
  (let ((session (make-session session-id nil))
	(done nil))
    (add-session connection session)
    (unwind-protect
	(progn
	  (condition-case error
	      (let* ((done nil)
		     (result (apply (intern-it function) args)))
		(when  replyp
		  (send-string process 
			       (prin1-to-string (list* (session-id session)
						       ':reply
						       result)))
		  (send-string process "\n")))
	    (error 
	     (if replyp
		 (progn
		   (send-string process 
				(prin1-to-string
				 (list (session-id session)
				       ':error
				       (prin1-to-string error))))
	 
		   (send-string process "\n"))
	       (message (concat "Error occurred: " (prin1-to-string error))))))
	  (setq done t))
      (unless done
	(send-string process 
		     (prin1-to-string (list (session-id session)
					    ':error
					    "aborted"))))
      (if oncep (lep::kill-session session)))))

;;(defun lep::make-session-for-lisp (session-id replyp oncep function &rest args)
;;  (let ((session (make-session session-id nil)))
;;    (add-session connection session)
;;    (condition-case error
;;	(let* ((done nil)
;;	       (result (unwind-protect
;;			   (prog1 (apply (intern-it function) args)
;;			     (setq done t))
;;			 ;;
;;			 (if (not done)
;;			     (progn
;;			       (send-string process 
;;					    (prin1-to-string (list (session-id session)
;;								   ':error
;;								   "aborted")))
;;	 
;;			       (send-string process "\n"))))))
;;	  (when  replyp
;;	    (send-string process 
;;			 (prin1-to-string (list* (session-id session)
;;						 ':reply
;;						 result)))
;;	    (send-string process "\n")))
;;      (error 
;;       (if replyp
;;	   (progn
;;	     (send-string process 
;;			  (prin1-to-string (list (session-id session)
;;						 ':error
;;						 (prin1-to-string error))))
;;	 
;;	     (send-string process "\n"))
;;	 (message (concat "Error occurred: " (prin1-to-string error))))))
;;    (if oncep (lep::kill-session session))))


;;;; There are some situations where we want to wait for the result to
;;;; comeback from the lisp.  For this we can use ACCEPT-PROCESS-OUTPUT.

(defun lep::eval-in-lisp (function &rest arguments)
  (let* ((result-cons (list nil nil nil))
	 (session (lep::send-request-in-new-session
		   function
		   t
		   arguments
		   (list (function immediate-reply-continuation) result-cons)
		   (list (function immediate-reply-error-continuation)
			 result-cons))))
    (wait-for-reply-to-come-back result-cons)))

(defun immediate-reply-continuation (&rest results)
  (let ((r (butlast results))
	(c (car (last results))))
    (stash-in-cons c r nil)))

(defun immediate-reply-error-continuation (error result-cons)
  (stash-in-cons result-cons error t))

(defun stash-in-cons (c r p)
  (setf (third c) p)
  (setf (second c) r)
  (setf (first c) t))

;;; This is complicated because we have to wait for output multiple times.

(defun wait-for-reply-to-come-back (result-cons)
  (when (not (car result-cons))
    (let ((count fi:lisp-evalserver-number-reads))
      (while (and (> (setq count (1- count)) 0)
		  (null (car result-cons)))
	(accept-process-output 
	 (connection-process (ensure-lep-connection))
	 fi:lisp-evalserver-timeout)))
    (when (not (car result-cons)) (error "Eval in lisp timed out"))
    (if (third result-cons)
	(error (second result-cons))
      (second result-cons))))
