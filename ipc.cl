;;					-[Mon Sep  3 00:20:27 1990 by layer]-
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

(eval-when (compile eval load)
  (if (find-symbol (symbol-name :rcsnote) (find-package :si))
      (push :has-rcsnote *features*))
  
  #-(or little-endian big-endian)
  (pushnew
   (let ((x '#(1)))
     (if (not (= 0 (sys::memref x #.(comp::mdparam 'comp::md-svector-data0-adj)
				0 :unsigned-byte)))
	 :little-endian
       :big-endian))
   *features*)
  )

#+has-rcsnote
(si::rcsnote
 "ipc"
 "$Header: /repo/cvs.copy/eli/Attic/ipc.cl,v 1.31 1990/09/03 00:20:35 layer Exp $")

(provide :ipc)

(in-package :ipc :use '(:lisp :excl :ff :mp))

(export '(start-lisp-listener-daemon open-network-stream
	  *inet-port-min* *inet-port-max* *inet-port-used*))

(require :process)
(require :foreign)

#+allegro-v3.1
(progn
  (require :cstructs)
  (defmacro ff::cstruct-length (x) `(ff::cstruct-len ,x))
  )

#+(or allegro-v3.2 allegro-v4.0)
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

;; from <??>
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

(defcstruct (servent :malloc)		; Returned by getservent
  (name * :char)
  (aliases * * :char)
  (port :signed-long)
  (proto * :char))

(defcstruct timeval
  (sec :long)
  (usec :long))

(defcstruct unsigned-long
  (unsigned-long :unsigned-long))

(defvar .lisp-listener-daemon-ff-loaded. nil)
(defvar .lisp-listener-daemon. nil)

(defparameter .needed-funcs.
    (mapcar #'convert-to-lang
	    ;; this list appears in makefile.cl, too
	    '("socket" "bind" "listen" "accept" "getsockname"
	      "gethostbyname" "getservbyname"
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
	   (unless (load ""
			 :verbose nil
			 :unreferenced-lib-names .needed-funcs.
			 #+(target sgi4d) :system-libraries
			 #+(target sgi4d) '("bsd")
			 )
	     (error "foreign load failed"))
	   (princ "done")
	   (terpri)
	   (force-output))))

    (setq .lisp-listener-daemon-ff-loaded. t)
    (defforeign-list '((getuid) (socket) (bind) (accept)
		       (getsockname) (gethostbyname) (getservbyname) (select)
		       (connect) (bcopy) (bzero) (bcmp) (perror)
		       (unix-listen :entry-point #,(convert-to-lang "listen"))
		       (unix-close :entry-point #,(convert-to-lang "close")))
	:print nil)))

(if* (entry-point-exists-p (convert-to-lang "lisp_htons"))
   then ;; Allegro CL 3.1 or later...
	(defforeign-list '((lisp_htons) (lisp_htonl) (lisp_ntohs)
			   (lisp_ntohl))
			 :print nil)
   else ;; pre-3.1 Allegro CL.  Do it the hard way...
	#+little-endian
	(progn
	  (setf (symbol-function 'lisp_htons)
	    #'(lambda (x)
		(logior (ash (logand x #x00ff) 8)
			(ash (logand x #xff00) -8))))
	  (setf (symbol-function 'lisp_ntohs)
	    #'(lambda (x) (lisp_htons x)))

	  (setf (symbol-function 'lisp_htonl)
	    #'(lambda (x)
		(logior (ash (logand x #x000000ff)  24)
			(ash (logand x #x0000ff00)   8)
			(ash (logand x #x00ff0000)  -8)
			(ash (logand x #xff000000) -24))))
	  (setf (symbol-function 'lisp_ntohl)
	    #'(lambda (x) (lisp_htonl x))))

	#+big-endian
	(progn
	  (setf (symbol-function 'lisp_htons) #'(lambda (x) x))
	  (setf (symbol-function 'lisp_htonl) #'(lambda (x) x))
	  (setf (symbol-function 'lisp_ntohs) #'(lambda (x) x))
	  (setf (symbol-function 'lisp_ntohl) #'(lambda (x) x))))

(defun start-lisp-listener-daemon ()
  "Starts a daemon process which listens to a socket for attempts to
connect, and starts a lisp listener for each connection.  If the Lisp
listener ever completes, the daemon makes sure its connection is closed."
  (unless .lisp-listener-daemon.
    (setq .lisp-listener-daemon.
      (process-run-function "TCP Listener Socket Daemon"
			    'lisp-listener-socket-daemon))
    (setf (getf (process-property-list .lisp-listener-daemon.)
		':no-interrupts)
      't)
    (setf (getf (process-property-list .lisp-listener-daemon.)
		':survive-dumplisp)
      't)))

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
			     (lisp_htons (setq *inet-port-used* port)))
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
			 then (let ((hostaddr
				     (lisp_ntohl
				      (sockaddr-in-addr listen-sockaddr))))
				(format t ";; access denied for host ")
				(format-in-addr t hostaddr)
				(format t ", password ~s~%" password))
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
  #-allegro-v4.0
  (setf (excl::sm_read-char s) #'mp::stm-bterm-read-string-char-wait)
  (format s "connection refused.~%")
  (force-output s)
  #-allegro-v4.0 (setf (excl::sm_bterm-out-pos s) 0)
  #+allegro-v4.0 (excl::clear-output-1 s)
  (close s))

(defun lisp-listener-with-stream-as-terminal-io (s)
  (unwind-protect
      #+allegro-v4.0
      (tpl:start-interactive-top-level
       s 'tpl:top-level-read-eval-print-loop nil)
      #-allegro-v4.0
      (progn
	(setf (excl::sm_read-char s) #'mp::stm-bterm-read-string-char-wait)
	(tpl:start-interactive-top-level
	 s 'tpl:top-level-read-eval-print-loop nil))
    ;; This next crock is to prevent the force-output done by close from
    ;; signalling an error if there are characters buffered to the output
    ;; stream, which there will be if the remote client closed the connection.
    ;; This should be changed to a clear-output once that works on a buffered
    ;; terminal stream.
    #-allegro-v4.0 (setf (excl::sm_bterm-out-pos s) 0)
    #+allegro-v4.0 (excl::clear-output-1 s)
    (close s)))

(defun open-network-stream (&key host port socket-file)
  "Open a stream to a port, which is a TCP/IP communication channel.  There
are two types of ports supported, UNIX and INTERNET domain.  The domain is
chosen based on the keyword arguments supplied:
For internet domain:
 HOST is the string host name or an integer internet address.
 PORT is the string service name or a port number.
For Unix domain:
 SOCKET-FILE is the string pathname of the socket."
  (when (or (and host port socket-file)
	    (and (or (null host) (null port))
		 (null socket-file)))
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
	    (if* (integerp host)
	       then (setq hostaddress (lisp_htonl host))
	     elseif (stringp host)
	       then (when (= (setq hostaddress (gethostbyname host)) 0)
		      (error "unknown host: ~a" host))
		    (if (not (= 4 (hostent-length hostaddress)))
			(error "address length not 4"))
		    (setq hostaddress
		      (let ((addr (hostent-addr hostaddress)))
			(si:memref-int
			 ;; SunOS 4.0 requires an extra indirection
			 (if (or (member comp::.target.
					 '(:sgi4d :sony :dec3100)
					 :test #'eq)
				 (probe-file "/lib/ld.so"))
			     (si:memref-int addr 0 0 :unsigned-long)
			   addr)
			 0 0 :unsigned-long)))
	       else (error "HOST not a string or integer internet address:"
			   host))
	    (setf (sockaddr-in-addr server) hostaddress)

	    (setf (sockaddr-in-port server)
	      (if* (integerp port)
		 then (lisp_htons port)
	       elseif (stringp port)
		 then (let ((serv (getservbyname port "tcp")))
			(if* (= 0 serv)
			   then (error "Unknown service name: ~s" port))
			(servent-port serv))
		 else (error "PORT not a string or integer internet service:"
			     port)))

	    (setf (sockaddr-in-family server) *af-inet*)
	    ;; open the connection
	    (when (< (connect sock server (ff::cstruct-length 'sockaddr-in)) 0)
	      (perror "Connect failure")
	      (unix-close sock)
	      (error "couldn't connect to socket"))
	    ;; build and return the stream
	    (make-ipc-terminal-stream sock))))

(defun make-ipc-terminal-stream (fd)
  #+allegro-v3.1 (excl::make-buffered-terminal-stream fd fd t t)
  #+allegro-v4.0 (excl::make-double-buffered-terminal-stream fd fd t t)
  )

(defun format-in-addr (stm addr)	; assumes host byte order
  (format stm "[~11,1,1,'.<~2,'0D~;~2,'0D~;~2,'0D~;~2,'0D~>]"
	  (ldb (byte 8 24) addr)
	  (ldb (byte 8 16) addr)
	  (ldb (byte 8  8) addr)
	  (ldb (byte 8  0) addr)))

;;; Allegro Common Lisp as the client side of telnet.
;;;
;;; This is a functional but stupid demo of how lisp can establish
;;; a client relationship with a server.
;;; As a telnet client it is a pretty inept.  In particular, it does
;;; nothing to suppress local echo.  After you login to the remote
;;; machine (and have already had your passwd echoed!) you can execute
;;; "stty -echo nl" on the remote machine.

;; When connecting to a telnet server on *some* hosts there are undocumented
;; (binary?) protocol exchanges that wedge things.
;; This client code should correctly connect to vanilla servers like the
;; lisp listener socket daemon defined above.

(defun telnet (host &optional (port "telnet"))
  (let (stm receiver)
    (flet ((receive (&aux char)
	     (loop (setq char (read-char stm nil :eof))
	       (when (eq char :eof)
		 (return))
	       (write-char char *terminal-io*)
	       (force-output *terminal-io*)))
	   (transmit (&aux char)
	     (loop (setq char (read-char *terminal-io* nil :eof))
	       (when (or (eq char :eof)
			 (eq char #\null))
		 (return))
	       (write-char char stm)
	       (force-output stm))))
      (unwind-protect
	  (progn
	    (setq stm (ipc:open-network-stream :host host :port port))
	    (unless (streamp stm)
	      (ipc::perror "lisp telnet")
	      (error "Unable to connect to telnet on host ~a~%" host))
	    (format t "Connected:  Type #\null or <eof> to end session~%")
	    (setq receiver (mp:process-run-function
			    (format nil "Telnet Receiver from ~a" host)
			    #'receive))
	    (transmit))
	(when (and (mp:process-p receiver)
		   (mp:process-active-p receiver))
	  (mp:process-kill receiver))
	(when (streamp stm)
	  (close stm))))))
