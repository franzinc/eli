;;					-[Sun Sep  2 20:02:05 1990 by layer]-
;;
;; Allegro CL IPC interface
;;
;; copyright (c) 1987, 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

(eval-when (compile eval)
  (if (find-symbol (symbol-name :rcsnote) (find-package :si))
      (push :has-rcsnote *features*))
  )

#+has-rcsnote
(si::rcsnote
 "ipc"
 "$Header: /repo/cvs.copy/eli/Attic/ipc.cl,v 1.29 1990/09/02 20:07:06 layer Exp $")

(provide :ipc)

(in-package :ipc :use '(:lisp :excl :ff :mp))

(export '(start-lisp-listener-daemon open-network-stream
	  *unix-domain* *inet-port-min* *inet-port-max* *inet-port-used*))

(require :process)
(require :foreign)

#+allegro-v3.1
(progn
  (require :cstructs)
  (defmacro ff::cstruct-length (x) `(ff::cstruct-len ,x))
  )

#+allegro-v4.0
(require :defctype)

(defvar *unix-domain* nil
  "If non-nil then use a UNIX domain socket, otherwise use an internet
domain port (see *inet-port* variable).")

(defvar *socket-pathname* nil
  "When the UNIX domain is used, then if non-nil this is the pathname of
the socket file to use for the communication between GNU Emacs and Allegro
CL.")

(defparameter *inet-port-min* 1025 
  "The smallest internet service port number on which Lisp listens for
  connections.  The value is this variable is only used when
  *unix-domain* is non-nil, if it is nil a UNIX domain socket is
  used.")

(defparameter *inet-port-max* 2025
  "The largest internet service port number on which Lisp listens for
  connections.  The value is this variable is only used when
  *unix-domain* is non-nil, if it is nil a UNIX domain socket is
  used.")

(defvar *inet-port-used* 0
  "Port actually in use for lisp listeners.")

(defvar *inet-listener-password* 0
  "A magic number that mnust be supplied to verify that someone trying to 
  connect to our listener is really the person who started the lisp")



(defconstant *af-unix* 1
  "The AF_UNIX constant from /usr/include/sys/socket.h.")

(defconstant *af-inet* 2
  "The AF_INET constant from /usr/include/sys/socket.h.")

(defconstant *sock-stream* 1
  "The SOCK_STREAM constant from /usr/include/sys/socket.h.")

;; from <netinet/in.h>
(defcstruct sockaddr-in
  (family :unsigned-short)		; short sin_family
  (port :unsigned-short)		; u_short sin_port
  (addr :unsigned-long)			; struct in_addr sin_addr
  (zero 8 :char)			; char sin_zero[8]
  )

;; from <>
(defcstruct sockaddr-un
  (family :unsigned-short)
  (path 109 :char))

;; from <netdb.h>
(defcstruct (hostent :malloc)
  (name * :char)			; char *h_name
  (aliases * * :char)			; char **h_aliases
  (addrtype :long)			; int h_addrtype
  (length :long)			; int h_length
  (addr * :char)			; char *h_addr   --or--
					; char **h_addr_list (for SunOS 4.0)
  )

(defcstruct timeval
  (sec :long)
  (usec :long))

(defcstruct unsigned-long
  (unsigned-long :unsigned-long))

(defvar .lisp-listener-daemon-ff-loaded. nil)
(defvar .lisp-listener-daemon. nil)

(defparameter .needed-funcs.
  (mapcar #'convert-to-lang
	  '("socket" "bind" "listen" "accept" "getsockname" "gethostbyname"
	    "connect" "bcopy" "bcmp" "bzero")))

(defvar .junk-name. (make-array 1))
(defvar .junk-address. (make-array 1 :element-type '(unsigned-byte 32)))

(defun entry-point-exists-p (string)
  (setf (aref .junk-name. 0) string)
  (setf (aref .junk-address. 0) 0)
  (= 0 (get-entry-points .junk-name. .junk-address.)))

(eval-when (load eval)
  (unless .lisp-listener-daemon-ff-loaded.
    (excl::machine-case :host
      ((:apollo :tek4300))
      (t (unless (dolist (name .needed-funcs. t)
		   (if (not (entry-point-exists-p name))
		       (return nil)))
	   (princ ";  Loading TCP routines from C library...")
	   (force-output)
	   (unless (load "" :verbose nil :unreferenced-lib-names
			 .needed-funcs.)
	     (error "foreign load failed"))
	   (princ "done")
	   (terpri))))

    (setq .lisp-listener-daemon-ff-loaded. t)
    (defforeign-list '((getuid) (socket) (bind) (accept)
		       (getsockname) (gethostbyname) (select)
		       (connect) (bcopy) (bzero) (bcmp) (perror)
		       (unix-listen :entry-point #,(convert-to-lang "listen"))
		       (unix-close :entry-point #,(convert-to-lang "close")))
	:print nil)))

(defun start-lisp-listener-daemon ()
  "This function starts a process which listens to a socket for attempts to
connect, and starts a lisp listener for each connection.  If the Lisp
listener ever completes, it makes sure files are closed."
  (unless .lisp-listener-daemon.
    (setq .lisp-listener-daemon.
      (process-run-function "TCP Listener Socket Daemon"
			    'lisp-listener-socket-daemon))

    #+(or allegro-v3.0 allegro-v3.1)
    (progn
      (setf (getf (process-property-list .lisp-listener-daemon.)
		  ':no-interrupts)
	't)
      (setf (getf (process-property-list .lisp-listener-daemon.)
		  ':survive-dumplisp)
	't))))

(defun lisp-listener-socket-daemon ()
  (block bad-news
    (let (listen-socket-fd
	  (listen-sockaddr
	   (if *unix-domain*
	       (make-cstruct 'sockaddr-un)
	     (let ((sin (make-cstruct 'sockaddr-in)))
	       (bzero sin (ff::cstruct-length 'sockaddr-in))
	       sin)))
	  (timeval (make-cstruct 'timeval))
	  (mask-obj (make-cstruct 'unsigned-long))
	  (int (make-cstruct 'unsigned-long))
	  mask
	  stream
	  proc-name
	  password
	  fd)
    
      (unless *socket-pathname*
	(setq *socket-pathname* (format nil "/tmp/GnuToAcl~d" (getuid))))

      (setf (timeval-sec timeval) 0
	    (timeval-usec timeval) 0)
      (unwind-protect
	  (progn
	    (if *unix-domain* (errorset (delete-file *socket-pathname*)))
	    (setq listen-socket-fd (socket
				    (if *unix-domain* *af-unix* *af-inet*)
				    *sock-stream*
				    0))
	    (when (< listen-socket-fd 0)
	      (perror "socket")
	      (setq listen-socket-fd nil)
	      (return-from bad-news))
	    (mp::mpwatchfor listen-socket-fd)

	    ;; Compute a select mask for the daemon's socket.
	    (setq mask (ash 1 listen-socket-fd))

	    (if* *unix-domain*
	       then (setf (sockaddr-un-family listen-sockaddr) *af-unix*)
		    ;; Set pathname.
		    (dotimes (i (length *socket-pathname*)
			       (setf (sockaddr-un-path listen-sockaddr i) 0))
		      (setf (sockaddr-un-path listen-sockaddr i)
			(char-int (elt *socket-pathname* i))))
		    (unless (zerop (bind listen-socket-fd
					 listen-sockaddr
					 (+ (length *socket-pathname*) 2)))
		      (perror "bind")
		      (return-from bad-news))
	       else (setf (sockaddr-in-family listen-sockaddr) *af-inet*)
		    (do ((port *inet-port-min* (1+ port)))
			((progn
			   (setf (sockaddr-in-port listen-sockaddr)
			     (setq *inet-port-used* port))
			   (zerop (bind listen-socket-fd
					listen-sockaddr
					(ff::cstruct-len 'sockaddr-in))))
			 (finish-output)
			 (format t "~d ~d ~a"
				 port
				 (setq *inet-listener-password* 
				   (random 1000000 (make-random-state t)))
				 (case excl::*current-case-mode*
				   ((:case-insensitive-upper
				     :case-sensitive-upper) ":upper")
				   ((:case-insensitive-lower
				     :case-sensitive-lower) ":lower")))
			 (finish-output))
		      (if* (= port *inet-port-max*)
			 then (perror "bind")
			      (return-from bad-news))))

	    (unless (zerop (unix-listen listen-socket-fd 5))
	      (perror "listen")
	      (return-from bad-news))
	    (loop
	      (process-wait "waiting for a connection"
			    #'(lambda (mask mask-obj timeout)
				(setf (unsigned-long-unsigned-long
				       mask-obj)
				  mask)
				(not (zerop (select 32 mask-obj 0 0 timeout))))
			    mask mask-obj timeval)
	      (setf (unsigned-long-unsigned-long int)
		(if *unix-domain*
		    (ff::cstruct-length 'sockaddr-un)
		  (ff::cstruct-length 'sockaddr-in)))
	      (setq fd (accept listen-socket-fd listen-sockaddr int))
	      (finish-output *standard-output*)
	      (finish-output *error-output*)
	      (when (< fd 0)
		(perror "accept")
		(return-from bad-news))
	    
	      (setq stream (make-ipc-terminal-stream fd))
	   
	      ;; the first thing that comes over the stream is the name of the
	      ;; emacs buffer which was created--we name the process the
	      ;; same.
	      ;; For internet sockets, the next thing is the password.
	      (setq proc-name (read stream))
	      (if (null *unix-domain*)
		  (setq password (read stream)))
	   
	      (if* *unix-domain*
		 then (process-run-function
		       proc-name
		       'lisp-listener-with-stream-as-terminal-io
		       stream)
		 else (if* (not (and (numberp password)
				     (= password *inet-listener-password*)))
			 then (format
			       t
			       ";; access denied for host ~s, password ~a~%"
			       (logand (sockaddr-in-addr listen-sockaddr)
				       #xff)
			       password)
			      (refuse-connection fd)
			 else (process-run-function
			       proc-name
			       'lisp-listener-with-stream-as-terminal-io
			       stream)))))
	(when listen-socket-fd
	  (mp::mpunwatchfor listen-socket-fd)
	  (unix-close listen-socket-fd)
	  (setq .lisp-listener-daemon. nil)))))
  (error "couldn't start listener daemon"))

(defun refuse-connection (fd &aux s)
  (setq s (make-ipc-terminal-stream fd))
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

(defun open-network-stream (&key host port socket-file)
  "Open a stream to a port, which is a TCP/IP communication channel.  There
are two types of ports supported, UNIX and INTERNET domain.  The domain is
chosen based on the keyword arguments actually used: HOST and PORT are for
internet domain ports and SOCKET-FILE is for unix domain ports."
  (if (and (or (null host) (null port))
	   (null socket-file))
      (error "Must either supply HOST and PORT *or* SOCKET-FILE keywords."))
  (if* socket-file
     then ;; UNIX domain
	  (let ((server (make-cstruct 'sockaddr-un))
		socket-fd)
	    (setf (sockaddr-un-family server) *af-unix*)
	    (dotimes (i (length socket-file)
		      (setf (sockaddr-un-path server i) 0))
	      (setf (sockaddr-un-path server i)
		(char-int (elt socket-file i))))
	    (setq socket-fd (socket *af-unix* *sock-stream* 0))
	    (if (< (connect socket-fd server (+ 2 (length socket-file))) 0)
		(error "connect failed to ~s" socket-file))
	    (make-ipc-terminal-stream socket-fd))
     else ;; INTERNET domain
	  (let (sock server hostaddress)
	    ;; Open a socket
	    (when (< (setf sock (socket *af-inet* *sock-stream* 0)) 0)
	      (error "couldn't open socket"))
	    ;; construct a socket address
	    (setf server (make-cstruct 'sockaddr-in))
	    (bzero server (ff::cstruct-length 'sockaddr-in))
	    (when (= (setf hostaddress (gethostbyname host)) 0)
	      (error "unknown host: ~a" host))
	    (if (not (= 4 (hostent-length hostaddress)))
		(error "address length not 4"))

	    (setf (sockaddr-in-addr server)
	      (si:memref-int (hostent-addr hostaddress) 0 0
			     :unsigned-long))
	    (if (or (member comp::.target. '(:sgi4d :sony)
			    :test #'eq)
		    ;; only on SunOS 4.0
		    (probe-file "/lib/ld.so"))
		(setf (sockaddr-in-addr server)
		  (si:memref-int (sockaddr-in-addr server)
				 0 0 :unsigned-long)))
	    
	    (setf (sockaddr-in-family server) *af-inet*)
	    (setf (sockaddr-in-port server) port)
	    ;; open the connection
	    (when (< (connect sock server (ff::cstruct-length 'sockaddr-in)) 0)
	      (unix-close sock)
	      (error "couldn't connect to socket"))
	    ;; build and return the stream
	    (make-ipc-terminal-stream sock))))

(defun make-ipc-terminal-stream (fd)
  #+allegro-v3.1 (excl::make-buffered-terminal-stream fd fd t t)
  #+allegro-v4.0 (excl::make-double-buffered-terminal-stream fd fd t t)
  )
