;; -*- mode: common-lisp; package: ipc -*-
;; Excl Common Lisp
;; ipc.cl
;; Interface to Unix IPC.
;;
;; copyright (c) 1987 Franz Inc, Berkeley, Ca.
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

;; Description:
;;  

;; $Header: /repo/cvs.copy/eli/Attic/ipc.cl,v 1.11 1988/02/18 08:24:48 layer Exp $
;; $Locker: layer $
;;

;; This code is a preliminary IPC interface for ExCL.
;; The functionality will be extended apace, but right now it only
;; implements a Common Lisp Server .
;; The server can be started by a CL and it establishes a daemon
;; listening to a socket.  Any process wanting to talk to the lisp
;; can connect to the socket and a new Lisp Listener will be started.

;; The listener listens to a socket at the argument address.

(in-package :ipc :use '(:lisp :excl :ff :mp))

(export '(start-lisp-listener-daemon))

(defvar lisp-listener-daemon-ff-loaded nil)
(defvar lisp-listener-daemon nil)

;; Masscomp wait(II) doesn't restart after a signal, so foreign loading is
;; chancy after the scheduler starts.

#+masscomp
(eval-when (load eval)
  (unless lisp-listener-daemon-ff-loaded
    (unless (load ""
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
		       #-never (perror)))))

(eval-when (compile load eval)
  (require :process)
  (require :foreign)
  (require :cstructs))

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

(defcstruct (hostent :malloc)
  (name * :char)
  (aliases * * :char)
  (addrtype :long)
  (length :long)
  (addr * char))

(defun start-lisp-listener-daemon ()
  "This function should be run as a separate process.
It listens to a socket for attempts to connect and starts a lisp listener
for each connection.  If the lisp listener ever completes, it makes sure
files are closed."
  #-masscomp
  (unless lisp-listener-daemon-ff-loaded
    (unless (load ""
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
		       #-never (perror))))
  (unless lisp-listener-daemon
    (setq lisp-listener-daemon
      (process-run-function "TCP Listener Socket Daemon"
			    'lisp-listener-socket-daemon))
    (setf (getf (process-property-list lisp-listener-daemon) ':no-interrupts)
	  t)))

(defparameter *port* 1123)

(defun lisp-listener-socket-daemon ()
  (let (listen-socket-fd
	(listen-sockaddr (make-cstruct 'sockaddr-in))
	(timeval (make-cstruct 'timeval))
	(mask-obj (make-cstruct 'unsigned-long))
	(int (make-cstruct 'unsigned-long))
	mask
	fd)
    (setf (timeval-sec timeval) 0
	  (timeval-usec timeval) 0)
    (unwind-protect
	(progn
	  (setq listen-socket-fd (socket 2 #|AF_INET|#
					 1 #|SOCK_STREAM|#
					 0 #|TCP|# ))
	  (when (< listen-socket-fd 0)
	    (format t "call to socket failed~%")
	    (perror "call to socket")
	    (setq listen-socket-fd nil)
	    (return-from lisp-listener-socket-daemon nil))
	  (mp::mpwatchfor listen-socket-fd)

	  ;; Compute a select mask for the daemon's socket.
	  (setq mask (ash 1 listen-socket-fd))

	  ;; Name the daemon's socket.
	  ;; This *really* should be in AF_UNIX namespace, but alas, Masscomp
	  ;; doesn't yet support it.  So as a temporary crock, we'll put it in
	  ;; the AF_INET namespace, also meaning that for right now there can
	  ;; only be one listener per host.
	  ;; This will shortly be fixed, but will do for now.

	  (bzero listen-sockaddr (ff::cstruct-len 'sockaddr-in))
	  (setf (sockaddr-in-family listen-sockaddr) 2 #|AF_INET|#
		(sockaddr-in-port listen-sockaddr) *port*)
	  (unless (zerop (bind listen-socket-fd
			       listen-sockaddr
			       (ff::cstruct-len 'sockaddr-in)))
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
		 (ff::cstruct-len 'sockaddr-in))
	   (setq fd (accept listen-socket-fd listen-sockaddr int))
	   (when (< fd 0)
	     (format t "accept returned error~%")
	     (perror "accept")
	     (return-from lisp-listener-socket-daemon nil))
	   
	   (let ((hostaddr (logand (sockaddr-in-addr listen-sockaddr) #xff)))
	     (format t ";;; starting listener-~d (host ~d)~%" fd
		     hostaddr)
	     (if* (not (eql 1 hostaddr))
		then (format t ";;; access denied for addr ~s~%" hostaddr)
		     (refuse-connection fd)
		else (process-run-function
		       (format nil "TCP Listener ~d" fd)
		       'lisp-listener-with-fd-as-terminal-io
		       fd)))))
      (when listen-socket-fd
	(mp::mpunwatchfor listen-socket-fd)
	(fd-close listen-socket-fd)
	(setq lisp-listener-daemon nil)))))

(defun refuse-connection (fd &aux s)
  (setq s (excl::make-buffered-terminal-stream fd fd t t))
  (setf (excl::sm_read-char s) #'mp::stm-bterm-read-string-char-wait)
  (format s "baaad connection, you luse.~%")
  (force-output s)
  (setf (excl::sm_bterm-out-pos s) 0)
  (close s))

(defun lisp-listener-with-fd-as-terminal-io (fd &aux s)
  (unwind-protect
      (progn
	(setq s (excl::make-buffered-terminal-stream fd fd t t))
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

#+ignore
(defun open-tcp-stream (host service &key (direction :io)
			     (element-type 'string-char) &allow-other-keys)
  "HOST may be a host name or a unsigned 32-bit integer internet address
SERVICE may be a named service or an integer port number
DIRECTION may be :io, :input, or :output
ELEMENT-TYPE may be string-char or (unsigned-byte 8)

returns the new stream if the connection is established successfully."
  )

(pushnew :ipc *features*)
