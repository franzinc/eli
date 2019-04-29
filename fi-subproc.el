;; This file has its (distant) roots in lisp/shell.el, so:
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;;
;; This file is derived from part of GNU Emacs.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;
;;; User visible functions
;;;;

(setq fi:emacs-to-lisp-transaction-directory
  (or fi:emacs-to-lisp-transaction-directory
      (if (on-ms-windows) (fi::temporary-directory) "/tmp")))

(defun fi::start-backdoor-interface (proc)
  (fi:verify-emacs-support)
  (setq fi::common-lisp-backdoor-main-process-name (process-name proc))
  (fi::reset-metadot-session))

(defun fi::set-terminal-io-external-format-string (ef)
  (if ef
      ;; then
      (progn
	(unless (stringp ef)
	  (setq ef (symbol-name ef)))
	(concat
	 "(let ((*load-verbose* nil))
            (when (and (fboundp 'excl::load-emacs-mule-ef)
                       (excl::load-emacs-mule-ef t))
               (princ \";; Setting (stream-external-format *terminal-io*) to :"
	 ef
	 ".\")
               (setf (stream-external-format *terminal-io*) :"
	 ef
	 "))
               (values))"))
    ;; else
    (concat
     "#+ics (progn (terpri t)"
     "(excl::note t \"The hosting emacs appears to have neither "
     "the emacs-mule nor mule-ucs utf-8 encodings.  Thus, Allegro CL "
     "international character support is limited in this emacs session.\")"
     "(values))")))

(defun fi::set-process-coding (process coding)
  (let* ((cs (process-coding-system process))
	 (ncs nil))
    (when (fboundp 'coding-system-name)
      (setq cs (cons (coding-system-name (car cs))
		     (coding-system-name (cdr cs)))))
    (setq ncs (mapcar
	       (function
		(lambda (x)
		  (let ((eol (string-match "-dos$\\|-unix$\\|-mac$" x)))
		    (if eol
			(intern
			 (concat (symbol-name coding)
				 (substring x eol (match-end 0))))
		      coding))))
	       (list (symbol-name (car cs)) (symbol-name (cdr cs)))))
    (set-process-coding-system process (car ncs) (cadr ncs))))

(defun fi:common-lisp (&optional buffer-name directory executable-image-name
				 image-args host image-file)
  "Create a Common Lisp subprocess and put it in the buffer named by
BUFFER-NAME, with default-directory of DIRECTORY, using EXECUTABLE-IMAGE-NAME
and IMAGE-ARGS as the binary image pathname and command line
arguments, doing the appropriate magic to execute the process on HOST.
Lastly, for machines that use image files, IMAGE-FILE is the
Common Lisp image to execute.  This is separate from the executable.

The first time this function is called and when given a prefix argument, all
the above quantities are read from the minibuffer, with defaults coming
from the variables:
	fi:common-lisp-buffer-name
	fi:common-lisp-directory
	fi:common-lisp-image-name
	fi:common-lisp-image-arguments
	fi:common-lisp-host
	fi:common-lisp-image-file
and the values read are saved in these variables for later use as defaults,
except that the directory argument does not side-effect the variable
fi:common-lisp-directory.

After the first time or when no prefix argument is given, the defaults are
used and no information is read from the minibuffer.

For backward compatibility, BUFFER-NAME can be a number, when called
programmatically, which means look for, and use if found, numbered buffers
of the form \"*common-lisp*<N>\" for N > 2.  If BUFFER-NAME < 0, then find
the first \"free\" buffer name and start a subprocess in that buffer.

Each Emacs may have at most one Emacs-Lisp connection. If a connection
already exists when fi:common-lisp is called, then the *common-lisp* buffer
will be made the current buffer, and all arguments will be ignored."
  (interactive
   (fi::get-lisp-interactive-arguments
    fi::common-lisp-first-time
    fi:common-lisp-buffer-name
    (let ((name fi:common-lisp-buffer-name))
      (if (numberp current-prefix-arg)
	  (fi::buffer-number-to-buffer name current-prefix-arg)
	(get-buffer-create name)))
    (or fi:common-lisp-directory default-directory)
    (progn
      (when (consp fi:common-lisp-image-name)
	(error "3rd argument to fi:common-lisp must \
be a string. Use the 6th argument for image file."))
      fi:common-lisp-image-name)
    fi:common-lisp-image-arguments
    (or fi:common-lisp-host (system-name))
    fi:common-lisp-image-file))
  (when fi::started-via-file
    (error "Emacs-Lisp interface already started via a file."))
  (when (and fi::shell-buffer-for-common-lisp-interaction-host-name
	     (or (y-or-n-p "A make-dist might be in progress.  Continue? ")
		 (error "fi:common-lisp aborted.")))
    (setq fi::shell-buffer-for-common-lisp-interaction-host-name nil))
  
  (let* ((process-environment process-environment)
	 (buffer-name (if (called-interactively-p 'any)
			  buffer-name
			(or buffer-name fi:common-lisp-buffer-name)))
	 (host (if (called-interactively-p 'any)
		   host
		 (or host fi:common-lisp-host (system-name))))
	 (directory
	  (progn
	    (setq fi::process-is-local (or (null host)
					   (string= "localhost" host)
					   ;; so it is case insensitive:
					   (string-match host (system-name))))
	    (when (not fi::process-is-local)
	      (when (not fi::rsh-command)
		(setq fi::rsh-command (fi::default-rsh-command))))
	    (if (called-interactively-p 'any)
		(if fi::process-is-local
		    (and directory (expand-file-name directory))
		  directory)
	      (or directory fi:common-lisp-directory))))
	 (executable-image-name
	  (if (called-interactively-p 'any)
	      executable-image-name
	    (when (consp executable-image-name)
	      (error "3rd argument to fi:common-lisp must \
be a string. Use the 6th argument for image file."))
	    (or executable-image-name
		fi:common-lisp-image-name)))
	 (image-file
	  (if (called-interactively-p 'any)
	      image-file
	    (or image-file fi:common-lisp-image-file)))
	 (image-args
	  (if (called-interactively-p 'any)
	      image-args
	    (or image-args fi:common-lisp-image-arguments)))
	 (real-args
	  (if (and fi:start-lisp-interface-arguments
		   ;; can't do this:
		   ;;  (not (fi::lep-open-connection-p))
		   ;; because there is a race condition implied here: the
		   ;; network connection may take longer to die than the
		   ;; process, so we might not startup the interface
		   ;; correctly (it seems to happen to some users more than
		   ;; others, but it still happens).
		   ;;
		   ;; What we really just want to prevent is multiple
		   ;; connections.
		   (let* ((buffer-name
			   (fi::calc-buffer-name buffer-name
						 fi::cl-process-name))
			  (buffer (get-buffer buffer-name)))
		     (or (not (fi::lep-open-connection-p))
			 (and buffer
			      (eq buffer (fi::connection-buffer
					  fi::*connection*))))))
	      (append (funcall fi:start-lisp-interface-arguments
			       fi:use-background-streams
			       image-file)
		      image-args)
	    image-args))
	 (real-args (if (on-ms-windows)
			(if fi::process-is-local
			    (fi::reorder-arguments real-args)
			  (fi::remove-windows-arguments real-args))
		      real-args))
	 (process-connection-type fi::common-lisp-connection-type) ;bug3033
	 (buffer nil)
	 (proc
	  (cond
	   ((and (on-ms-windows) fi::process-is-local)
	    (setq buffer (get-buffer-create buffer-name))
	    (cond
	     ((or (null fi::common-lisp-backdoor-main-process-name)
		  (not (fi:process-running-p
			(get-process
			 fi::common-lisp-backdoor-main-process-name)
			buffer-name)))
	      (let ((proc
		     (with-current-buffer buffer
		       (setq default-directory directory)
		       (fi::socket-start-lisp fi::cl-process-name
					      executable-image-name
					      real-args))))
		;; wait for process to start
		(sleep-for fi::common-lisp-compatibility-mode-timeout)
		(if (and fi:eli-compatibility-mode
			 (fi::eli-compat-mode-p 9666))
		    ;; A Lisp running an older version of eli was detected,
		    ;; start using the old code
		    (fi::common-lisp-1-windows-compat
		     host buffer-name directory executable-image-name
		     image-file image-args)
		  ;; The resulting *common-lisp* buffer is not Lisp's initial
		  ;; *terminal-io*, so the *terminal-io*
		  ;; *external-format setting is done in the same way as
		  ;; *fi:open-lisp-listener.
		  (fi::common-lisp-1-windows proc host buffer-name directory
					     executable-image-name image-file
					     image-args))))
	     (t (fi::goto-lisp-buffer buffer))))
	   (t ;; NOT windows...
	    (let ((p (fi::common-lisp-1-unix host buffer-name directory
					     executable-image-name image-file
					     image-args real-args)))
	      ;; The resulting *common-lisp* buffer is to Lisp's initial
	      ;; *terminal-io*, so we set the *terminal-io*
	      ;; external-format here.
	      (when fi::setup-for-mule
		(let ((cs (fi::lisp-connection-coding-system)))
		  (process-send-string
		   p
		   (fi::set-terminal-io-external-format-string cs))
		  (process-send-string p "\n")
		  (when cs
		    (fi::set-process-coding p cs))
		  (setq fi::setup-for-mule nil)))
	      p)))))
    (setq fi::common-lisp-first-time nil
	  fi:common-lisp-buffer-name buffer-name
	  fi:common-lisp-image-name executable-image-name
	  fi:common-lisp-image-file image-file
	  fi:common-lisp-image-arguments image-args
	  fi:common-lisp-host host)
    (when fi:common-lisp-directory
      ;; Only setq'd if it already had a value...
      (setq fi:common-lisp-directory directory))
    proc))

(defun fi::default-rsh-command ()
  (cond ((fi::command-exists-p "ssh") "ssh")
	((and (on-ms-windows) (fi::command-exists-p "ssh.exe"))
	 "ssh.exe")
	((fi::command-exists-p "remsh") "remsh")
	((fi::command-exists-p "rsh") "rsh")
	((and (on-ms-windows) (fi::command-exists-p "rsh.exe"))
	 "rsh.exe")
	(t (error "can't find ssh/rsh in your path"))))

(defun fi::rsh-command-remote-p ()
  (fi:member-equal (file-name-nondirectory fi::rsh-command)
		   '("ssh" "ssh.exe" "remsh" "rsh" "rsh.exe")))

(defun fi::eli-compat-mode-p (port)
  (let (proc)
    (condition-case ()
	(setq proc
	  (let ((display-warning-minimum-level
		 ;; For XEmacs -- see bug14703
		 'error))
	    (open-network-stream "*eli-compat*" nil "localhost" port)))
      (error))
    (cond (proc
	   ;; something detected on port, kill proc and return `t'
	   (delete-process proc)
	   t)
	  (t nil))))

(defun fi::common-lisp-1-windows-compat (host buffer-name directory
					 executable-image-name image-file
					 image-args)
  (let ((i 0)
	(process nil))
    (unless fi::lisp-host (setq-default fi::lisp-host host))
    (unless fi::lisp-port (setq-default fi::lisp-port 9666))
    (unless fi::lisp-password (setq-default fi::lisp-password 0))
    (fi::set-environment fi:subprocess-env-vars)
    ;; So fi::make-tcp-connection knows we've started a Lisp:
    (setq fi::common-lisp-backdoor-main-process-name fi::cl-process-name)
    (while
	(condition-case condition
	    (progn
	      (setq process
		(let ((fi::muffle-open-network-stream-errors t))
		  (fi::make-tcp-connection
		   buffer-name 1
		   'fi:lisp-listener-mode fi:common-lisp-prompt-pattern
		   fi::lisp-host fi::lisp-port fi::lisp-password
		   'fi::setup-tcp-connection)))
	      nil)
	  (error
	   (and fi::last-network-condition
		(consp fi::last-network-condition)
		(eq 'file-error (first fi::last-network-condition))
		(equal "connection failed"
		       (second fi::last-network-condition)))))
      (cond
       (fi:common-lisp-subprocess-wait-forever)
       ((and (> i 0) (zerop (mod i 10)))
	;; Every 10 iterations, ask if they still want to wait:
	(when (not (y-or-n-p "Continue waiting for ACL to startup? "))
	  (error "Connection aborted.")))
       (t
	(when (> i fi:common-lisp-subprocess-timeout)
	  (error "Couldn't make connection to existing Lisp [timeout]."))))
      (sleep-for 1)
      (setq i (+ i 1)))
    
    (when (not (fi::lep-open-connection-p))
      (fi::start-backdoor-interface process)
      (setq fi::process-name (process-name process))
      (setq fi::common-lisp-backdoor-main-process-name (process-name process))
      (fi::ensure-lep-connection)
      (condition-case ()
	  (setq fi::lisp-case-mode
	    (case (car
		   (read-from-string
		    (fi:eval-in-lisp "excl:*current-case-mode*")))
	      ((case-insensitive-lower case-sensitive-lower) ':lower)
	      ((CASE-INSENSITIVE-UPPER CASE-SENSITIVE-UPPER) ':upper)))
	(error nil))
      (fi::run-start-lisp-interface-hooks))
    
    (when (null process)
      (error "Couldn't make connection to existing Lisp [no process]."))
    (setq default-directory directory)
    process))

(defun fi::run-start-lisp-interface-hooks ()
  (cond ((consp fi:start-lisp-interface-hook)
	 (mapcar 'funcall fi:start-lisp-interface-hook))
	(fi:start-lisp-interface-hook
	 (funcall fi:start-lisp-interface-hook))))

(defun fi::common-lisp-1-windows (proc host buffer-name directory
				  executable-image-name image-file
				  image-args)
  (let* ((pid (if (cygwinp)
		  (cygwin-to-windows-process-id proc)
		(process-id proc)))
	 (connection-file
	  (format "%s%selistartup%d"
		  (fi::temporary-directory)
		  (if (cygwinp) "/" "\\") pid))
	 ;;In w32proc.c where is this Win9x lossage workaround
	 ;; /* Hack for Windows 95, which assigns large (ie negative) pids */
	 ;; if (cp->pid < 0)
	 ;;   cp->pid = -cp->pid;
	 (connection-file-win9x
	  (format "%s%selistartup%d"
		  (fi::temporary-directory)
		  (if (cygwinp) "/" "\\")
		  (- pid)))
	 (i 0))

    (while (and (not (when (file-exists-p connection-file-win9x)
		       (setf connection-file connection-file-win9x)))
		(not (file-exists-p connection-file)))
      (cond
       (fi:common-lisp-subprocess-wait-forever)
       ((and (> i 0) (zerop (mod i 10)))
	;; Every 10 iterations, ask if they still want to wait:
	(when (not (y-or-n-p "Continue waiting for ACL to startup? "))
	  (error "Connection aborted.")))
       (t (when (> i fi:common-lisp-subprocess-timeout)
	    (error "Couldn't make connection to existing Lisp."))))
      (sleep-for 1)
      (setq i (+ i 1)))

    ;; So fi::make-tcp-connection knows we've started a Lisp:
    (setq fi::common-lisp-backdoor-main-process-name fi::cl-process-name)

    (let ((proc (fi::start-interface-via-file-1 host buffer-name
						connection-file)))
      (delete-file connection-file)
      proc)))

(defun fi::common-lisp-1-unix (host buffer-name directory
			       executable-image-name image-file image-args
			       real-args)
  (let ((startup-message
	 (concat
	  "\n====================================="
	  "=========================\n"
	  (format "Starting image `%s'\n" executable-image-name)
	  (when image-file
	    (format "  with image (dxl) file `%s'\n" image-file))
	  (if image-args
	      (format "  with arguments `%s'\n" image-args)
	    "  with no arguments\n")
	  (format "  in directory `%s'\n" directory)
	  (format "  on machine `%s'.\n" host)
	  "\n")))
    (fi::make-subprocess
     startup-message
     fi::cl-process-name
     buffer-name
     directory
     'fi:inferior-common-lisp-mode
     fi:common-lisp-prompt-pattern
     (if fi::process-is-local executable-image-name fi::rsh-command)
     (if fi::process-is-local
	 real-args
       (fi::remote-lisp-args host executable-image-name real-args
			     directory))
     'fi::common-lisp-subprocess-filter
     'fi::start-backdoor-interface
     ;;
     ;; rest of the arguments are the
     ;; mode-hook function and its arguments
     (function
      (lambda (local host dir)
	(if local
	    ;; can't use "localhost" below because HP can't
	    ;; write an operating system.
	    (setq fi::lisp-host host)
	  (progn 
	    (setq fi::lisp-is-remote t)
	    (setq fi::lisp-host host)
	    (condition-case ()
		(cd dir)
	      (error nil))))))
     fi::process-is-local host directory)))

(defvar fi::windows-plus-args
    ;; List of (arg-name . number-of-companion-args)
    '(("+B" . 0)
      ("+Bp" . 0)
      ("+Bt" . 0)
      ("+Cx" . 0)
      ("+M" . 0)
      ("+N" . 1)
      ("+R" . 0)
      ("+R" . 0)
      ("+RR" . 0)
      ("+Ti" . 0)
      ("+Tx" . 0)
      ("+b" . 1)
      ("+c" . 0)
      ("+cc" . 0)
      ("+cm" . 0)
      ("+cn" . 0)
      ("+cx" . 0)
      ("+d" . 1)
      ("+m" . 0)
      ("+n" . 0)
      ("+p" . 0)
      ("+s" . 1)
      ("+t" . 1)
      ("+x" . 0)
      ))

(defun fi::remove-windows-arguments (arguments)
  ;; remove windows only + arguments
  (let ((args nil)
	(arg nil)
	temp)
    (while arguments
      (setq arg (car arguments))
      (if (setq temp (cdr (assoc arg fi::windows-plus-args)))
	  (when (and temp (numberp temp)
		     (> temp 0))
	    (setq arguments (cdr arguments)))
	(push arg args)))
    (nreverse args)))

(defun fi::reorder-arguments (arguments)
  ;; make sure the + arguments are first
  (let ((dlisp-args nil)
	(other-args nil)
	(arg nil)
	temp)
    (while arguments
      (setq arg (car arguments))
      (if (setq temp (cdr (assoc arg fi::windows-plus-args)))
	  (if (and temp (numberp temp) (> temp 0))
	      (progn
		(push arg dlisp-args)
		(setq arguments (cdr arguments))
		(push (car arguments) dlisp-args))
	    (push arg dlisp-args))
	(push arg other-args))
      (setq arguments (cdr arguments)))
    (append (nreverse dlisp-args) (nreverse other-args))))

(defun fi::socket-start-lisp (process-name image arguments)
  (let (
;;;; rms didn't like the win32- prefix, so we have to support the old and
;;;; new style:
	(win32-start-process-show-window t)
	(w32-start-process-show-window t)
	(win32-quote-process-args t)
	(w32-quote-process-args t)
	;; from Greg Klanderman:
	;; XEmacs 21 NT does this differently...
	(nt-quote-args-functions-alist '(("." . nt-quote-args-double-quote))))
    (apply (function start-process)
	   process-name
	   nil ;; no buffer name
	   image arguments)))

(defun fi:open-lisp-listener (&optional buffer-number buffer-name
					setup-function)
  "Open a connection to an existing Common Lisp process, started with the
function fi:common-lisp, and create a Lisp Listener (a top-level
interaction).  The Common Lisp can be either local or remote.  The name of
the buffer is \"*lisp-listener*\" with an optional suffix of \"<N>\", for
prefix arguments > 1.  If a negative prefix argument is given, then the
first \"free\" buffer name is found and used.  When called from a program,
the buffer name is the second optional argument."
  (interactive "p")
  (if fi::started-via-file
      (fi::ensure-lep-connection)
    (if (or (null fi::common-lisp-backdoor-main-process-name)
	    (not (fi:process-running-p
		  (get-process fi::common-lisp-backdoor-main-process-name)
		  buffer-name)))
	(error "Common Lisp must be running to open a lisp listener.")))
  (if fi::started-via-file
      (fi::make-tcp-connection (or buffer-name "lisp-listener")
			       buffer-number
			       'fi:lisp-listener-mode
			       fi:common-lisp-prompt-pattern
			       fi::lisp-host
			       fi::lisp-port
			       fi::lisp-password
			       (or setup-function 'fi::setup-tcp-connection))
    (let* ((buffer (process-buffer
		    (get-process fi::common-lisp-backdoor-main-process-name))))
      (fi::make-tcp-connection (or buffer-name "lisp-listener")
			       buffer-number
			       'fi:lisp-listener-mode
			       fi:common-lisp-prompt-pattern
			       (fi::get-buffer-host buffer)
			       (fi::get-buffer-port buffer)
			       (fi::get-buffer-password buffer)
			       (or setup-function
				   'fi::setup-tcp-connection)))))

(defun fi::setup-tcp-connection (proc)
  (let ((cs nil))
    (setq cs (fi::lisp-connection-coding-system))
    (when cs
      (fi::set-process-coding proc cs))
    (format
     (concat
      "(progn
      (setf (getf (mp:process-property-list mp:*current-process*)
                  ':emacs-listener-number) %d)
      (setf (excl::interactive-stream-p *terminal-io*) t)"
      ;; still inside progn
      (fi::set-terminal-io-external-format-string cs)
      " (values))\n")
     (fi::tcp-listener-generation proc))))

(defun fi:franz-lisp (&optional buffer-name directory executable-image-name
				image-args host)
  "Create a Franz Lisp subprocess and put it in buffer named by
BUFFER-NAME, with default-directory of DIRECTORY, using EXECUTABLE-IMAGE-NAME
and IMAGE-ARGS as the binary image pathname and command line
arguments, doing the appropriate magic to execute the process on HOST.

The first time this function is called and when given a prefix argument, all
the above quantities are read from the minibuffer, with defaults coming
from the variables:
	fi:franz-lisp-buffer-name
	fi:franz-lisp-directory
	fi:franz-lisp-image-name
	fi:franz-lisp-image-arguments
	fi:franz-lisp-host
and the values read are saved in these variables for later use as defaults.
After the first time or when no prefix argument is given, the defaults are
used and no information is read from the minibuffer.

For backward compatibility, the buffer-name can be a number, when called
programmatically, which means look for, and use if found, numbered buffers
of the form \"*franz-lisp*<N>\" for N > 2.  If BUFFER-NAME < 0, then find
the first \"free\" buffer name and start a subprocess in that buffer."
  (interactive
   (fi::get-lisp-interactive-arguments fi::franz-lisp-first-time
				       fi:franz-lisp-buffer-name
				       (let ((name fi:franz-lisp-buffer-name))
					 (if (numberp current-prefix-arg)
					     (fi::buffer-number-to-buffer
					      name current-prefix-arg)
					   (get-buffer-create name)))
				       (or fi:franz-lisp-directory
					   default-directory)
				       fi:franz-lisp-image-name
				       fi:franz-lisp-image-arguments
				       (or fi:franz-lisp-host (system-name))))
  (when (not fi::rsh-command)
    (setq fi::rsh-command (fi::default-rsh-command)))
  (setq fi::process-is-local (or (string= "localhost" host)
				 ;; so it is case insensitive:
				 (string-match host (system-name))))
  (let* ((buffer-name (if (called-interactively-p 'any)
			  buffer-name
			(or buffer-name fi:franz-lisp-buffer-name)))
	 (directory (if (called-interactively-p 'any)
			(expand-file-name directory)
		      (or directory fi:franz-lisp-directory)))
	 (executable-image-name
	  (if (called-interactively-p 'any)
	      executable-image-name
	    (or executable-image-name fi:franz-lisp-image-name)))
	 (image-args (if (called-interactively-p 'any)
			 image-args
		       (or image-args fi:franz-lisp-image-arguments)))
	 (host (if (called-interactively-p 'any)
		   host
		 (or host fi:franz-lisp-host)))
	 (proc (fi::make-subprocess
		nil
		"franz-lisp"
		buffer-name
		directory
		'fi:inferior-franz-lisp-mode
		fi:franz-lisp-prompt-pattern
		(if fi::process-is-local executable-image-name fi::rsh-command)
		(if fi::process-is-local
		    image-args
		  (fi::remote-lisp-args host executable-image-name image-args
					directory))
		nil			; use default filter
		nil			; no interface to franz-lisp
		;;
		;; rest of the arguments are the
		;; mode-hook function and its arguments
		(function
		 (lambda (local dir)
		   (unless local
		     (condition-case ()
			 (cd dir)
		       (error nil)))))
		fi::process-is-local directory)))
    (setq fi:franz-lisp-process-name (process-name proc))
    (setq fi::franz-lisp-first-time nil
	  fi:franz-lisp-buffer-name buffer-name
	  fi:franz-lisp-directory directory
	  fi:franz-lisp-image-name executable-image-name
	  fi:franz-lisp-image-arguments image-args
	  fi:franz-lisp-host host)
    proc))

;;;;
;;; Internal functions
;;;;

(defun fi::remote-lisp-args (host executable-image-name image-args directory)
  (let ((remote (fi::rsh-command-remote-p)))
    (append
     fi::rsh-args
     (list
      host
      "sh"
      "-c"
      ;; use `exec' to get rid of `sh' process
      (format
       "%s%s if test -d %s; then cd %s; fi; exec %s %s%s"
       (if remote "'" "")
       (fi::env-vars)
       directory
       directory
       executable-image-name
       (mapconcat (function (lambda (x) (if x (concat "\"" x "\"") "")))
		  image-args
		  " ")
       (if remote "'" ""))))))

(defun fi::get-lisp-interactive-arguments (first-time buffer-name buffer
					   directory exe image-args host
					   &optional dxl)
  (setq buffer-name (or (and buffer (buffer-name buffer))
			buffer-name))

  (when (or first-time
	    (and current-prefix-arg
		 (or (null buffer)
		     (not (get-buffer-process buffer))
		     (not (fi:process-running-p
			   (get-buffer-process buffer)
			   buffer-name)))))

    (setq buffer-name (read-buffer "Buffer: " buffer-name))
    (setq host (read-string "Host: " host))
      
    ;; make sure it ends in a slash:
    (setq directory
      (let ((temp (fi::read-file-name "Process directory: "
				      (or directory default-directory)
				      (or directory default-directory)
				      ;; must match only if local:
				      fi::process-is-local)))
	(if (= ?/ (aref temp (- (length temp) 1)))
	    temp
	  (concat temp "/"))))

    (setq exe (fi::read-file-name "Lisp executable program: " exe exe))
    (setq dxl (fi::read-file-name "Lisp image (dxl) file: " dxl dxl))

    (setq image-args
      (fi::listify-string
       (read-from-minibuffer "Image arguments (separate by spaces): "
			     (mapconcat 'concat image-args " ")))))

  (list buffer-name directory exe image-args host dxl))

(defun fi::read-file-name (prompt
			   &optional directory default mustmatch initial)
  (let* ((def (if (null default) "" default))
	 (temp (read-file-name prompt directory
			       def
			       mustmatch
			       (if (string= def initial) nil initial))))
    (setq temp
      (cond ((string= temp "")
	     ;; user deleted `initial' text or just hit RET
	     nil)
	    ((or (string-match "\\$" temp)
		 (string-match "^~" temp))
	     ;; expand it, since `directory' will have been expanded
	     (expand-file-name temp))
	    (t temp)))
    (cond ((and temp (string= temp directory))
	   ;; user typed RET only, return default answer:
	   default)
	  (t temp))))

(defun fi::common-lisp-subprocess-filter (process output &optional stay
								   cruft)
  (let ((val (catch 'cl-subproc-filter-foo
	       (fi::common-lisp-subprocess-filter-1 process output))))
    (case (car val)
      (normal
       (fi::subprocess-filter process (cdr val) stay cruft)
       (set-process-filter process 'fi::subprocess-filter))
      (error
       (message "%s" (cdr val))
       (fi::switch-to-buffer "*Help*"))
      (t
       (fi::subprocess-filter process output stay cruft)))))

(defun fi::common-lisp-subprocess-filter-1 (process output)
  ;; This is a temporary filter, which is used until the rendezvous with
  ;; Lisp is made.
  (with-current-buffer (process-buffer process)
    (when (and (not (fi::lep-open-connection-p))
	       (fi::fast-search-string 1 output)
	       (string-match "\\([^\0]*\\)\\(.*\\)\\([^\0]*\\)"
			     output)) 
      (let ((res (concat (substring output (match-beginning 1)
				    (match-end 1))
			 (substring output (match-beginning 3)
				    (match-end 3)))))
	
	(fi::set-connection-vars 
	 (substring output (match-beginning 2) (match-end 2)))
	
	(condition-case condition
	    (progn
	      (fi::run-start-lisp-interface-hooks)
	      (throw 'cl-subproc-filter-foo
		(cons 'normal res)))
	  (error (throw 'cl-subproc-filter-foo
		   (cons 'error condition))))))))

(defun fi::set-connection-vars (command)
  (let ((xx nil))
    (setq fi::lisp-port (car (setq xx (read-from-string command nil))))
    (setq fi::lisp-password
      (car (setq xx (read-from-string command (cdr xx)))))
    (setq fi::lisp-case-mode
      (car (setq xx (read-from-string (downcase command) (cdr xx)))))
    (setq fi::lisp-version
      ;; Older versions of the protocol didn't provide this, so protect
      ;; against it not being there:
      (condition-case ()
	  (car (setq xx (read-from-string
			 (fi::frob-case-from-lisp command)
			 (cdr xx))))
	(error nil)))
    ;; The ipc version is now ignored.  It was never used.  Ever.
    ))

(defun fi::make-subprocess (startup-message process-name buffer-name
			    directory mode-function image-prompt image-file
			    image-args
			    &optional filter
				      initial-func
				      mode-hook
			    &rest mode-hook-arguments)
  (let* ((remote (and (stringp image-file)
		      (string= fi::rsh-command image-file)))
	 (buffer-name (fi::calc-buffer-name buffer-name process-name))
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (buffer-name (buffer-name buffer))
	 (process (get-buffer-process buffer))
	 (runningp (fi:process-running-p process buffer-name))
	 start-up-feed-name)

    (if (not runningp)
	(progn				; hack image-file
	  (if (consp image-file)
	      (if (not (stringp (setq image-file (funcall image-file))))
		  (error "image-file function didn't return a string"))
	    (if (not (stringp image-file))
		(error "image-file not a string or cons: %s" image-file))
	    (setq image-file (substitute-in-file-name image-file)))
	  (if (and (not remote) (= ?~ (aref image-file 0)))
	      (setq image-file (expand-file-name image-file)))))

    (unless runningp
      (with-current-buffer buffer
	(save-window-excursion
	  (goto-char (point-max))
	  (if (stringp startup-message) (insert startup-message))
	  (when (and directory
		     (file-exists-p directory)
		     (or (not (on-ms-windows))
			 fi::process-is-local))
	    (setq default-directory directory))
	  (if process (delete-process process))
	  (fi::set-environment (fi::compute-subprocess-env-vars))
	  (let (
		;;(process-connection-type nil) ;bug3033
		)
	    (setq process
	      (apply 'start-process
		     (append (list buffer-name buffer image-file)
			     image-args))))
	  (set-process-sentinel process 'fi::subprocess-sentinel)
	  
	  ;; do the following after the sentinel is established so we don't get
	  ;; an ugly message in the subprocess buffer
	  ;;
	  (when (not (fi:process-running-p process buffer-name))
	    (error "Couldn't startup %s" image-file))

	  (set-process-filter process (or filter 'fi::subprocess-filter))
	  (setq start-up-feed-name
	    (if image-file
		(concat "~/.emacs_" (file-name-nondirectory image-file))))
	  (cond
	   ((and start-up-feed-name (file-exists-p start-up-feed-name))
	    (sleep-for 1) ;; I hope 1 second is enough!
	    (goto-char (point-max))
	    (insert-file-contents start-up-feed-name)
	    (setq start-up-feed-name (buffer-substring (point) (point-max)))
	    (delete-region (point) (point-max))
	    (process-send-string process start-up-feed-name)))
	  (goto-char (point-max))
	  (set-marker (process-mark process) (point))
	  (condition-case ()
	      (let ((saved-input-ring fi::input-ring)
		    (saved-input-ring-yank-pointer
		     fi::input-ring-yank-pointer))
		;; This usually kills all local variables:
		(apply mode-function mode-hook mode-hook-arguments)
		(setq fi::input-ring saved-input-ring)
		(setq fi::input-ring-yank-pointer
		  saved-input-ring-yank-pointer))
	    (error nil))
	  (setq fi::prompt-pattern image-prompt)
	  (fi::make-subprocess-variables)
	  (when initial-func (funcall initial-func process))
	  
	  (setq fi::setup-for-mule t) ;; new process, must do this
	  )))

    (fi::goto-lisp-buffer buffer)

    process))

(defun fi::goto-lisp-buffer (buffer)
  ;; display last so we can do proper screen creation on xemacs
  (condition-case nil
      (funcall fi:display-buffer-function buffer)
    (error (fi::switch-to-buffer buffer)))

  (goto-char (point-max))

  ;; The recenter fixes the behavior of initial display with FSF 19.23, but
  ;; needs to be checked across other Emacs.
  (recenter (- (window-height) 2))
  
  (set-marker (if (numberp fi::last-input-end)
		  (make-marker)
		fi::last-input-end)
	      (point)
	      buffer))

(defun fi::calc-buffer-name (buffer-name process-name)
  (cond ((stringp buffer-name) buffer-name)
	(t (let ((name (concat "*" process-name "*")))
	     (if (numberp buffer-name)
		 (fi::buffer-number-to-buffer name buffer-name)
	       name)))))

(defun fi::tcp-listener-generation (proc)
  (let ((gen fi::tcp-listener-generation))
    (setq fi::tcp-listener-table
      (cons (cons proc gen) fi::tcp-listener-table))
    (setq fi::tcp-listener-generation (+ 1 fi::tcp-listener-generation))
    gen))

(defun fi::make-tcp-connection (buffer-name buffer-number mode image-prompt
				&optional given-host
					  given-service
					  given-password
					  setup-function)
  (or fi::started-via-file
      fi::common-lisp-backdoor-main-process-name
      (error "A Common Lisp subprocess has not yet been started."))
  (let* ((buffer-name
	  (fi::buffer-number-to-buffer
	   (if (string-match "\\*" buffer-name)
	       buffer-name
	     (concat "*" buffer-name "*"))
	   buffer-number))
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (default-dir default-directory)
	 (buffer-name (buffer-name buffer))
	 (process-buffer
	  (when (not fi::started-via-file)
	    (and (get-process fi::common-lisp-backdoor-main-process-name)
		 (process-buffer
		  (get-process fi::common-lisp-backdoor-main-process-name)))))
	 (host (or given-host
		   (and fi::started-via-file
			(error "Via file mode, need to specify host."))
		   (fi::get-buffer-host process-buffer)))
	 (service (or given-service
		      (and fi::started-via-file
			   (error "Via file mode, need to specify service."))
		      (fi::get-buffer-port process-buffer)))
	 (password (or given-password
		       (and fi::started-via-file
			    (error "Via file mode, need to specify passwd."))
		       (fi::get-buffer-password process-buffer)))
	 (proc (get-buffer-process buffer)))

    (unless (fi:process-running-p proc buffer-name)
      (with-current-buffer buffer
	(save-window-excursion
	  (goto-char (point-max))

	  (setq default-directory default-dir)
	  (setq proc (fi::open-network-stream buffer-name buffer host service))
	  (set-process-filter proc 'fi::subprocess-filter)
	  (set-process-sentinel proc 'fi::tcp-sentinel)
	  ;;
	  ;; The first input the new (Common Lisp) process is sent is the name
	  ;; of the process.  This is so that the processes are named similarly
	  ;; in Emacs and Lisp.
	  ;;

	  (process-send-string
	   proc
	   (format "%s\n" (fi::prin1-to-string fi::listener-protocol)))
	  (process-send-string proc (format "\"%s\"\n" (buffer-name buffer)))
	  (process-send-string proc (format "%d\n" password))

	  (when setup-function
	    (process-send-string proc (funcall setup-function proc)))

	  (goto-char (point-max))
	  (set-marker (process-mark proc) (point))
	  (let ((saved-input-ring fi::input-ring)
		(saved-input-ring-yank-pointer fi::input-ring-yank-pointer))
	    (funcall mode)
	    (setq fi::input-ring saved-input-ring)
	    (setq fi::input-ring-yank-pointer saved-input-ring-yank-pointer))
	  (setq fi::prompt-pattern image-prompt)
	  (fi::make-subprocess-variables))))

    (fi::goto-lisp-buffer buffer)
    
    proc))

(defun fi::subprocess-sentinel (process status)
  ;; Sentinel for subprocesses.  The sentinel currently
  ;; does nothing, other than prevent the status change message when the
  ;; process dies.
  (when (and fi::lisp-is-remote
	     (string-match "exited abnormally" status))
    (let ((extra ""))
      (when (and (not (string= (system-name) fi::lisp-host))
		 (string-match fi::lisp-host (system-name)))
	(setq extra
	  (format "

It appears that the host you gave to fi:common-lisp (%s) was meant to
be %s--the latter name is how this host is known to GNU Emacs.
If you type:

    M-x fi:common-lisp RET

and answer \"%s\" to the \"Host: \" question, the %s will
probably succeed."
		  fi::lisp-host
		  (system-name)
		  (system-name)
		  fi::rsh-command)))
      (fi:error "
It appears that %s to host %s exited abnormally.
This is probably due to the host you specified to fi:common-lisp (%s)
being inaccessible.  Check that

    %s$ %s %s date

works--if it does not, then fi:common-lisp will fail.%s"
		fi::rsh-command
		fi::lisp-host
		fi::lisp-host
		(system-name)
		fi::rsh-command
		fi::lisp-host
		extra)))
  
  (when (and fi::started-via-file
	     (string-match "finished" status))
    ;; It's annoying to have to restart emacs to clear this.
    (setq fi::started-via-file nil))
  t)

(defun fi::tcp-sentinel (process status)
  ;; Sentinel and filter for network connections.  The sentinel currently
  ;; does nothing, other than prevent the status change message when the
  ;; connection is closed.
  (set-buffer (process-buffer process))
  (goto-char (point-max))
  (insert
   (format
    "\n---------------------------------------------------------------------\n"))
  t)

(defvar fi::subprocess-filter-output-preprocess-hook nil)
(make-variable-buffer-local 'fi::subprocess-filter-output-preprocess-hook)
(setq-default fi::subprocess-filter-output-preprocess-hook nil)

(defvar fi::subprocess-filter-insert-output-hook nil)
(make-variable-buffer-local 'fi::subprocess-filter-insert-output-hook)
(setq-default fi::subprocess-filter-insert-output-hook nil)

(defun fi::subprocess-filter (process output &optional stay cruft)
  "Filter output from processes tied to buffers.
This function implements continuous output to visible buffers."
  (condition-case nil
      (fi::subprocess-filter-1 process output stay cruft)
    (error (set-process-filter process nil))))

(defun fi::subprocess-filter-1 (process output &optional stay cruft)
  (let ((inhibit-quit t))
    (when cruft
      (setq output (fi::substitute-chars-in-string '((?\r)) output)))
    (when fi::subprocess-filter-output-preprocess-hook
      (setq output
	(funcall fi::subprocess-filter-output-preprocess-hook output)))
    (let* ((old-buffer (current-buffer))
	   (buffer (process-buffer process))
	   (in-buffer (eq buffer old-buffer))
	   (window-of-buffer (get-buffer-window buffer))
	   (no-window (or (null window-of-buffer)
			  (not (windowp window-of-buffer))))
	   (xmarker (process-mark process))
	   (marker (if (marker-position xmarker)
		       xmarker
		     (set-marker (make-marker) 0 buffer)))
	   (marker-point (marker-position marker))
	   (output-length (length output))
	   old-point
	   point-not-before-marker
	   new-point)
      ;; The three symbols below are not bound above because `(window-point)'
      ;;   for the selected window does not always return the same thing as the
      ;;   function `(point)' in that window!  [Version 18 is supposed to fix
      ;;   this bug.]
      ;; Note that there is no function that returns all of the windows that
      ;;   are currently displaying a buffer.  Because of this, not all windows
      ;;   will be updated properly by this filter function.  What should be
      ;;   done is to loop through all windows displaying the buffer and do
      ;;   `(set-window-point)' in each.
      (if (not in-buffer)
	  (progn
	    (set-buffer buffer)
	    (setq old-point
	      (if no-window
		  (point)
		(window-point window-of-buffer))))
	(setq old-point (point)))
      (setq point-not-before-marker (>= old-point marker-point))
      (setq new-point (if point-not-before-marker
			  (+ old-point output-length)
			old-point))
      (save-excursion
	;; Go to point of last output by fi::make-process and insert new
	;;   output there, preserving position of the marker.
	(goto-char marker-point)
	;; The code below works around what appears to be a display bug
	;;   in GNU Emacs 17.  If `(insert-before-markers)' is used when
	;;   the process marker (process-mark), window-start point
	;;   (window-start), and window point (point) are all coincident,
	;;   the window display `sticks' on the topmost line.  We use
	;;   `(insert)' followed by `(set-marker)' to avoid this
	;;   problem.  This also happens to be the way
	;;   `handle_process_output()' deals with this in `process.c'.
	(if fi::subprocess-filter-insert-output-hook
	    (funcall fi::subprocess-filter-insert-output-hook output marker)
	  (insert output)
	  (set-marker marker (point))))
      (if (not in-buffer)
	  (if (and fi:subprocess-continuously-show-output-in-visible-buffer
		   point-not-before-marker)
	      ;; Keep window's notion of `point' in a constant relationship to
	      ;;   the process output marker.
	      (if no-window
		  (goto-char new-point)
		(set-window-point window-of-buffer new-point))
	    (if no-window
		t ;; Still there.
	      (set-window-point window-of-buffer old-point)))
	(goto-char new-point))
      (cond
       (in-buffer
	(when fi:subprocess-max-buffer-lines (fi:truncate-buffer))      
	nil)
       (stay old-buffer)
       (t (set-buffer old-buffer))))))

(defun fi:truncate-buffer ()
  "Truncate the buffer to `fi:subprocess-max-buffer-lines'."
  (interactive)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line (- fi:subprocess-max-buffer-lines))
    (beginning-of-line)
    (delete-region (point-min) (point))))

(defun fi::buffer-number-to-buffer (name number)
  (if (string-match "^\\(.*\\)<[0-9]+>$" name)
      (setq name (substring name (match-beginning 1) (match-end 1))))
  (let ((buffer-name
	 (cond ((> number 1) (format "%s<%d>" name number))
	       ((< number 0)
		(let (n tmp)
		  (if (null (fi:process-running-p name name))
		      name
		    (setq n 2)
		    (while (fi:process-running-p
			    (setq tmp (format "%s<%d>" name n)))
		      (setq n (+ n 1)))
		    tmp)))
	       (t name))))
    (or (get-buffer buffer-name)
	(get-buffer-create buffer-name))))

(defun fi::make-subprocess-variables ()
  (setq fi::input-ring-max fi:default-input-ring-max)
  (setq fi::shell-directory-stack nil)
  (setq fi::last-input-search-string "")
  (setq fi::last-input-start (make-marker))
  (setq fi::last-input-end (make-marker)))

(defun fi::subprocess-watch-for-special-commands ()
  "Watch for special commands like, for example, `cd' in a shell.  We grok
the `cdpath' C shell environment variable, if you add the line

	setenv CDPATH \"$cdpath\"

to your `.cshrc' after the `set cdpath=(...)' in the same file."
  (if (null fi::shell-directory-stack)
      (setq fi::shell-directory-stack (list default-directory)))
  (condition-case ()
      (let ((directory nil) (directory-stack nil))
	(save-excursion
	  (goto-char fi::last-input-start)
	  (cond
	   ((and fi:in-package-regexp (looking-at fi:in-package-regexp))
	    (goto-char (match-end 0))
	    (cond
	     ((or (looking-at "[ \t]*\"\\(.*\\)\"[ \t]*)")
		  (looking-at "[ \t]*[']?[#]?[:]?\\(.*\\)[ \t]*)")
		  (looking-at "[ \t]*\\(.*\\)[ \t]*)"))
	      ;; (in-package foo)
	      (setq fi:package (fi::normalize-package
				(buffer-substring (match-beginning 1) (match-end 1)))))
	     ((looking-at "[ \t]+\\(.*\\)[ \t]*$")
	      ;; :pa foo
	      (setq fi:package (fi::normalize-package
				(buffer-substring (match-beginning 1) (match-end 1))))))
	    ;; need to do something here to force the minibuffer to
	    ;; redisplay:
	    (set-buffer-modified-p (buffer-modified-p)))
	   ((and fi:shell-popd-regexp (looking-at fi:shell-popd-regexp))
	    (goto-char (match-end 0))
	    (cond
	     ((looking-at ".*&[ \t]*$")
	      ;; "popd ... &" executes in a subshell!
	      )
	     (t
	      (let ((n (if (looking-at "[ \t]+\\+\\([0-9]*\\)")
			   (car
			    (read-from-string
			     (buffer-substring (match-beginning 1)
					       (match-end 1)))))))
		(if (null n)
		    (setq directory
		      (car (setq fi::shell-directory-stack
			     (cdr fi::shell-directory-stack))))
		  ;; pop n'th entry
		  (if (> n (length fi::shell-directory-stack))
		      (message "Directory stack not that deep.")
		    (let ((tail (nthcdr (+ n 1) fi::shell-directory-stack)))
		      (rplacd (nthcdr (- n 1) fi::shell-directory-stack)
			      nil)
		      (setq fi::shell-directory-stack
			(append fi::shell-directory-stack tail)))))))))
	   ((and fi:shell-pushd-regexp (looking-at fi:shell-pushd-regexp))
	    (goto-char (match-end 0))
	    (cond
	     ((looking-at ".*&[ \t]*$")
	      ;; "pushd ... &" executes in a subshell!
	      )
	     ((looking-at "[ \t]+\\+\\([0-9]+\\)[ \t]*[;\n]")
	      ;; pushd +n
	      (let ((n (car (read-from-string
			     (buffer-substring (match-beginning 1)
					       (match-end 1))))))
		(if (< n 1)
		    (message "Illegal stack element: %s" n)
		  (if (> n (length fi::shell-directory-stack))
		      (message "Directory stack not that deep.")
		    (let ((head (nthcdr n fi::shell-directory-stack)))
		      (rplacd (nthcdr (- n 1) fi::shell-directory-stack)
			      nil)
		      (setq fi::shell-directory-stack
			(append head fi::shell-directory-stack))
		      (setq directory (car head)))))))
	     ((looking-at "[ \t]+\\([^ \t]+\\)[;\n]")
	      ;; pushd dir
	      (let* ((xdir (buffer-substring (match-beginning 1)
					     (match-end 1)))
		     (dir
		      (if (string-match "[\\$~]" xdir)
			  (expand-file-name (substitute-in-file-name xdir))
			xdir)))
		(setq directory dir)
		(setq directory-stack 'push)))
	     ((looking-at "[ \t]*[;\n]")
	      ;; pushd
	      (if (< (length fi::shell-directory-stack) 2)
		  (message "Directory stack not that deep.")
		(setq fi::shell-directory-stack
		  (append (list (car (cdr fi::shell-directory-stack))
				(car fi::shell-directory-stack))
			  (cdr (cdr fi::shell-directory-stack))))
		(setq directory (car fi::shell-directory-stack))))))
	   ((and fi:shell-cd-regexp (looking-at fi:shell-cd-regexp))
	    (goto-char (match-end 0))
	    (cond
	     ((looking-at ".*&[ \t]*$")
	      ;; "cd foo &" executes in a subshell!
	      )
	     ((looking-at "[ \t]*[;\n]")
	      ;; cd
	      (setq directory
		(rplaca fi::shell-directory-stack (getenv "HOME"))))
	     ((looking-at "[ \t]+\\([^ \t]+\\)[ \t]*[;\n]")
	      ;; cd dir
	      (let* ((xdir
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     (dir
		      (if (string-match "[\\$~]" xdir)
			  (expand-file-name (substitute-in-file-name xdir))
			xdir)))
		(setq directory dir)
		(if (string-match "^/" dir)
		    (setq directory-stack 'replace)
		  (setq directory-stack 'append))))))))
	(when directory
	  (if (file-directory-p directory)
	      (progn
		(cd directory)
		(cond ((eq 'push directory-stack)
		       (setq fi::shell-directory-stack
			 (cons directory fi::shell-directory-stack)))
		      ((eq 'replace directory-stack)
		       (rplaca fi::shell-directory-stack directory))
		      ((eq 'append directory-stack)
		       (rplaca fi::shell-directory-stack
			       (fi::new-directory
				(car fi::shell-directory-stack)
				directory)))))

	    ;; check $cdpath
	    (unless fi::cdpath (setq fi::cdpath (fi::listify-cdpath)))
	    (let ((cdpath fi::cdpath)
		  (done nil)
		  (dir nil))
	      (while (and cdpath (not done))
		(setq dir (format "%s/%s" (car cdpath) directory))
		(when (file-directory-p dir)
		  (cd dir)
		  (cond ((eq 'push directory-stack)
			 (setq fi::shell-directory-stack
			   (cons dir fi::shell-directory-stack)))
			((eq 'replace directory-stack)
			 (rplaca fi::shell-directory-stack dir))
			((eq 'append directory-stack)
			 (rplaca fi::shell-directory-stack
				 (fi::new-directory
				  (car fi::shell-directory-stack)
				  directory))))
		  (setq done t))
		(setq cdpath (cdr cdpath)))))))
    (error nil)))

(defun fi::new-directory (old new)
  (cond ((string= "." new) old)
	((string= ".." new)
	 (file-name-directory
	  (directory-file-name old)))
	(t (concat old
		   (unless (string-match "/$" old)
		     "/")
		   new))))

(defun fi::listify-cdpath ()
  (let ((cdpath (fi::shell-command-output-to-string
		 (get-buffer-create " *cdpath parsing*")
		 "csh"
		 "-c" "echo $cdpath")))
    (fi::explode cdpath ? )))

(defun fi::get-buffer-host (buffer)
  "Given BUFFER return the value in this buffer of fi::lisp-host."
  (with-current-buffer buffer fi::lisp-host))

(defun fi::get-buffer-port (buffer)
  "Given BUFFER return the value in this buffer of fi::lisp-port."
  (with-current-buffer buffer fi::lisp-port))

(defun fi::get-buffer-password (buffer)
  "Given BUFFER returns the values in this buffer of fi::lisp-password"
  (with-current-buffer buffer fi::lisp-password))


(defun fi::env-vars ()
  (let ((env (fi::compute-subprocess-env-vars)))
    (concat (mapconcat #'(lambda (x)
			   (format "%s=%s" (car x) (eval (eval (cdr x)))))
		       env
		       " ")
	    " export "
	    (mapconcat #'(lambda (x) (car x)) env " ")
	    "; ")))

(defun fi::set-environment-use-setenv (valist)
  (let (item val)
    (while valist
      (setq item (car valist))
      (setq val (or (eval (cdr item)) ""))
      (setenv (car item) val)
      (setq valist (cdr valist)))))

(defun fi::set-environment-use-process-environment (valist)
  (dolist (pair valist)
    (cond ((cdr pair)
	   (push (concat (car pair) "=" (eval (cdr pair)))
		 process-environment))
	  (t
	   ;; This is an implicit request to remove the variable from the
	   ;; environment passed to the subprocess. <from Luca Pisati>
	   (let ((match (concat (car pair) "=")))
	     (setf process-environment
	       (remove-if (lambda (x) (string-match match x))
			  process-environment)))))))

(defun fi::compute-subprocess-env-vars ()
  (append fi:subprocess-env-vars fi:user-env-vars))

(fset 'fi::set-environment
      (if (boundp 'process-environment)
	  (symbol-function 'fi::set-environment-use-process-environment)
	(symbol-function 'fi::set-environment-use-setenv)))
