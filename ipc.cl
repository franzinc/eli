;; -*- mode: common-lisp; package: ipc -*-
;;
;; Allegro Common Lisp IPC interface
;;
;; copyright (c) 1987, 1988 Franz Inc, Berkeley, Ca.
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
;;
;; $Header: /repo/cvs.copy/eli/Attic/ipc.cl,v 1.17 1988/04/26 22:12:29 layer Exp $
;; $Locker: layer $
;;
;; This code is a preliminary IPC interface for ExCL. The functionality
;; will be extended apace, but right now it only implements a Common Lisp
;; Server.  The server can be started by a CL and it establishes a daemon
;; listening to a socket.  Any process wanting to talk to the lisp
;; can connect to the socket and a new Lisp Listener will be started.

(in-package :ipc :use '(:lisp :excl :ff :mp))

(pushnew :ipc *features*)

(export '(start-lisp-listener-daemon *unix-domain* *inet-port*))

(require :process)
(require :foreign)
(require :cstructs)

(defvar *unix-domain* t
  "If non-nil then use a UNIX domain socket, otherwise use an internet
domain port (see *inet-port* variable).")

(defparameter *inet-port* 1123
  "The internet service port number on which Lisp listens for connections.
The value is this variable is only used when *unix-domain* is non-nil, in
which case a UNIX domain socket is used.")

(defconstant *af-unix* 1
  "The AF_UNIX constant from /usr/include/sys/socket.h.")

(defconstant *af-inet* 2
  "The AF_INET constant from /usr/include/sys/socket.h.")

(defconstant *sock-stream* 1
  "The SOCK_STREAM constant from /usr/include/sys/socket.h.")

(defvar lisp-listener-daemon-ff-loaded nil)
(defvar lisp-listener-daemon nil)

(eval-when (load eval)
  (unless lisp-listener-daemon-ff-loaded
    (unless (load "" :verbose nil
		  :unreferenced-lib-names
		  (mapcar #'convert-to-lang
			  '("socket" "bind" "listen" "accept" "getsockname"
			    "bcopy" "bcmp" "bzero")))
      (error "foreign load failed"))
    (setq lisp-listener-daemon-ff-loaded t)
    (defforeign-list '((getuid)
		       (socket)
		       (bind)
		       (unix-listen :entry-point "_listen")
		       (accept)
		       (getsockname)
		       (select)
		       (fd-close :entry-point "_close")
		       (bcopy) (bzero) (bcmp)
		       (perror))
	:print nil)))

(defcstruct sockaddr-in
  (family :unsigned-short)
  (port :unsigned-short)
  (addr :unsigned-long)
  (zero 8 :unsigned-byte))

(defcstruct sockaddr-un
  (family :unsigned-short)
  (path 109 :char))

(defcstruct timeval
  (sec :long)
  (usec :long))

(defcstruct unsigned-long
  (unsigned-long :unsigned-long))

#+not-used
(defcstruct (hostent :malloc)
  (name * :char)
  (aliases * * :char)
  (addrtype :long)
  (length :long)
  (addr * char))

(defun start-lisp-listener-daemon ()
  "This function starts a process which listens to a socket for attempts to
connect, and starts a lisp listener for each connection.  If the Lisp
listener ever completes, it makes sure files are closed."
  (unless lisp-listener-daemon
    (setq lisp-listener-daemon
      (process-run-function "TCP Listener Socket Daemon"
			    'lisp-listener-socket-daemon))
    (setf (getf (process-property-list lisp-listener-daemon) ':no-interrupts)
          t)))

(defun lisp-listener-socket-daemon ()
  (let (listen-socket-fd
	(listen-sockaddr
	 (if *unix-domain*
	     (make-cstruct 'sockaddr-un)
	   (make-cstruct 'sockaddr-in)))
	(socket-pathname
	 (format nil "~a/~a" (sys:getenv "HOME") ".excl_to_emacs"))
	(timeval (make-cstruct 'timeval))
	(mask-obj (make-cstruct 'unsigned-long))
	(int (make-cstruct 'unsigned-long))
	mask
	stream
	proc-name
	fd)
    (setf (timeval-sec timeval) 0
	  (timeval-usec timeval) 0)
    (unwind-protect
	(progn
	  (if *unix-domain* (errorset (delete-file socket-pathname)))
	  (setq listen-socket-fd (socket
				  (if *unix-domain* *af-unix* *af-inet*)
				  *sock-stream*
				  0))
	  (when (< listen-socket-fd 0)
	    (perror "call to socket")
	    (setq listen-socket-fd nil)
	    (return-from lisp-listener-socket-daemon nil))
	  (mp::mpwatchfor listen-socket-fd)

	  ;; Compute a select mask for the daemon's socket.
	  (setq mask (ash 1 listen-socket-fd))

	  (if* *unix-domain*
	     then (setf (sockaddr-un-family listen-sockaddr) *af-unix*)
		  ;; Set pathname.
		  (dotimes (i (length socket-pathname)
			    (setf (sockaddr-un-path listen-sockaddr i) 0))
		    (setf (sockaddr-un-path listen-sockaddr i)
		      (char-int (elt socket-pathname i))))
	     else ;; a crock:
		  (bzero listen-sockaddr (ff::cstruct-len 'sockaddr-in))
		  (setf (sockaddr-in-family listen-sockaddr) *af-inet*
			(sockaddr-in-port listen-sockaddr) *inet-port*))
	  
	  (unless (zerop (bind listen-socket-fd
			       listen-sockaddr
			       (if *unix-domain*
				   (+ (length socket-pathname) 2)
				 (ff::cstruct-len 'sockaddr-in))))
	    (perror "bind")
	    (return-from lisp-listener-socket-daemon nil))

	  (unless (zerop (unix-listen listen-socket-fd 5))
	    (perror "listen")
	    (return-from lisp-listener-socket-daemon nil))
	  (loop
	   (process-wait "waiting for a connection"
			 #'(lambda (mask mask-obj timeout)
			     (setf (unsigned-long-unsigned-long mask-obj) mask)
			     (not (zerop (select 32 mask-obj 0 0 timeout))))
			 mask mask-obj timeval)
	   (setf (unsigned-long-unsigned-long int)
	     (if *unix-domain*
		 (ff::cstruct-len 'sockaddr-un)
	       (ff::cstruct-len 'sockaddr-in)))
	   (setq fd (accept listen-socket-fd listen-sockaddr int))
	   (when (< fd 0)
	     (perror "accept")
	     (return-from lisp-listener-socket-daemon nil))
	   
	   (setq stream
	     (excl::make-buffered-terminal-stream fd fd t t))
	   
	   ;; the first thing that comes over the stream is the name of the
	   ;; emacs buffer which was created--we name the process the same.
	   (setq proc-name (read stream))
	   
	   (if* *unix-domain*
	      then (process-run-function
		    proc-name
		    'lisp-listener-with-stream-as-terminal-io stream)
	      else (let ((hostaddr (logand
				    (sockaddr-in-addr listen-sockaddr) #xff)))
		     (format t ";;; starting listener-~d (host ~d)~%" fd
			     hostaddr)
		     (if* (and nil
			       ;; the next line checks that the connection
			       ;; is coming from the current machine
			       (not (eql 1 hostaddr))
			       )
			then (format t ";;; access denied for addr ~s~%"
				     hostaddr)
			     (refuse-connection fd)
			else (process-run-function
			      proc-name
			      'lisp-listener-with-stream-as-terminal-io
			      stream))))))
      (when listen-socket-fd
	(mp::mpunwatchfor listen-socket-fd)
	(fd-close listen-socket-fd)
	(setq lisp-listener-daemon nil)))))

(defun refuse-connection (fd &aux s)
  (setq s (excl::make-buffered-terminal-stream fd fd t t))
  (setf (excl::sm_read-char s) #'mp::stm-bterm-read-string-char-wait)
  (format s "connection refused.~%")
  (force-output s)
  (setf (excl::sm_bterm-out-pos s) 0)
  (close s))

(defun lisp-listener-with-stream-as-terminal-io (s)
  (unwind-protect
      (progn
	(setf (excl::sm_read-char s) #'mp::stm-bterm-read-string-char-wait)
	(tpl:start-interactive-top-level
	 s 'tpl:top-level-read-eval-print-loop nil))
    ;; This next crock is to prevent the force-output done by close from
    ;; signalling an error if there are characters buffered to the output
    ;; stream, which there will be if the remote client closed the connection.
    ;; This should be changed to a clear-output once that works on a buffered
    ;; terminal stream.
    (setf (excl::sm_bterm-out-pos s) 0)
    (close s)))
