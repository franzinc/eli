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

;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.137 1992/02/21 14:36:10 layer Exp $

;; Low-level subprocess mode guts

;;;;
;;; General Subprocess Variables and Constants
;;;;

(defvar fi:shell-cd-regexp ":?cd"
  "*The regular expression matching the C shell `cd' command and the
Common Lisp :cd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-cd-regexp)

(defvar fi:shell-popd-regexp ":?popd"
  "*The regular expression matching the C shell `popd' command and the
Common Lisp :popd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-popd-regexp)

(defvar fi:shell-pushd-regexp ":?pushd"
  "*The regular expression matching the C shell `pushd' command and the
Common Lisp :pushd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-pushd-regexp)

(defvar fi:subprocess-continuously-show-output-in-visible-buffer t
  "*If t, output from a subprocess to a visible buffer is continuously
shown.  If a subprocess buffer is visible and the window point is beyond
the process output marker, output to that buffer from its associated
process will be continuously visible.  If the window point is before the
process output marker, the window is not updated.  This is a buffer-local
symbol.")
(make-variable-buffer-local
 'fi:subprocess-continuously-show-output-in-visible-buffer)

(defvar fi:subprocess-enable-superkeys nil
  "*If t, certain keys become `superkeys' in subprocess buffers--this
should be set before starting any subprocesses.  The superkeys are C-a,
C-d, C-o,C-u, C-w, C-z, and C-\\, which will behave as they would in the
current local keymap when typed at the end of a subprocess buffer.  If
typed elsewhere, these keys have their normal global binding.  This is a
buffer-local symbol.  Use setq-default to set the default value for this
symbol.")
(make-variable-buffer-local 'fi:subprocess-enable-superkeys)

(defvar fi:display-buffer-function 'switch-to-buffer
  "*If non-nil, then the value should be a function taking one argument,
a buffer, which is used to display a buffer when a subprocess is created.")

(defconst fi:subprocess-env-vars
    '(("EMACS" . "t")
      ("TERM" . "emacs")
      ("DISPLAY" . (getenv "DISPLAY"))
      ("TERMCAP" . (format "emacs:co#%d:tc=unknown:" (screen-width))))
  "*An alist containing the environment variables to pass to newly created
subprocesses.")

(defvar fi:in-package-regexp nil
  "*If non-nil, the regular expression that describes the IN-PACKAGE form,
for purposes of tracking package changes in a subprocess Lisp buffer.  The
value of this is taken from fi:default-in-package-regexp in Lisp subprocess
buffers, but is nil elsewhere.")
(make-variable-buffer-local 'fi:in-package-regexp)

(defvar fi:default-in-package-regexp
  "(in-package\\>\\|:pa\\>\\|:pac\\>\\|:pack\\>\\|:packa\\>\\|:packag\\>\\|:package\\>"
  "*The regular expression matching the Lisp expression to change the
current package.  The two things this must match are the IN-PACKAGE macro
form and all the possible instances of the :package top-level command.
If nil, no automatic package tracking will be done.")

(defvar fi:pop-to-sublisp-buffer-after-lisp-eval t
  "*If non-nil, then go to the Lisp subprocess buffer after sending
expressions to Lisp (via the functions which eval or compile the region, a
form or the entire buffer).")

(defvar fi:package nil
  "A buffer-local variable whose value should either be nil or a string
which names a package in the Lisp world (ie, in a Lisp subprocess running
as an inferior of Emacs in some buffer).  It is used when expressions are
sent from an Emacs buffer to a Lisp process so that the symbols are read
into the correct Lisp package.")

(make-variable-buffer-local 'fi:package)


;;;;
;;; Common Lisp Variables and Constants
;;;;

(defvar fi:emacs-to-lisp-transaction-directory "/tmp"
  "*The directory in which files for Emacs/Lisp communication are stored.
When using Lisp and Emacs on different machines, this directory should be
accessible on both machine with the same pathname (via the wonders of NFS).")

(defvar fi:echo-evals-from-buffer-in-listener-p nil
  "*If non-nil, forms evalutated directly in fi:common-lisp-mode by the
functions which compile or eval the region, a form or the entire buffer
will be echoed by Common Lisp.")

(defvar fi:start-lisp-interface-arguments
    (function
     (lambda (use-background-streams)
       (list "-e"
	     (format "(excl:start-emacs-lisp-interface %s)"
		     use-background-streams))))
  "*This value of this variable determines whether or not the emacs-lisp
interface is started automatically when fi:common-lisp is used to run
Common Lisp images.   If non-nil, then a the value of this variable should
be a function of one argument that returns command line argument sufficient
to start the emacs-lisp interface.  The argument is a boolean that
determines whether or not background streams are used (see
fi:use-background-streams).")

(defvar fi:use-background-streams t
  "*If non-nil, then the default function bound to
fi:start-lisp-interface-arguments will cause background streams to be
initialized in ACL (see the function excl:use-background-streams).  Roughly
speaking, background streams cause processes that do output, but which are
not associated with any particular stream, to do it in a unique listener in
an emacs buffer.  This allows MP:PROCESS-RUN-FUNCTION in the Lisp
environment to be more useful.")

(defvar fi:start-lisp-interface-hook nil
  "*A function or a list of functions to call when we get the rendezvous
info from Lisp in the Lisp subprocess buffer.  This is used to
automatically startup the Emacs-Lisp hidden communication via a socket.
fi:start-lisp-interface-arguments initiates the connection with Lisp from
the Emacs side, Lisp then starts up the daemon which listens for
connections and prints a string to the subprocess buffer (which is not
displayed by the process filter for the Common Lisp subprocess), at which
time the hooks are run.")

(defvar fi:common-lisp-buffer-name "*common-lisp*"
  "*Default buffer name used by fi:common-lisp.  This variable is set by
fi:common-lisp when a new buffer name is used.")

(defvar fi:common-lisp-directory nil
  "*Default directory in which the process started by fi:common-lisp uses.
A CD is done into this directory before the process is started.")

(defvar fi:common-lisp-image-name "cl"
  "*Default Common Lisp image used by fi:common-lisp.  If the value is a
string then it names the image file or image path that fi:common-lisp
invokes.  Otherwise, the value of this variable is given to funcall, the
result of which should yield a string which is the image name.")

(defvar fi:common-lisp-image-arguments nil
  "*Default Common Lisp image arguments when invoked from `fi:common-lisp',
which must be a list of strings.  Each element of the list is one command
line argument.")

(defvar fi:common-lisp-host "localhost"
  "*The default host on which fi:common-lisp starts the Common Lisp
subprocess.  \"localhost\" means use the host on which emacs is running.")

(defvar fi:common-lisp-prompt-pattern
    "^\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\)?\\(<[-A-Za-z]* ?[0-9]*?>\\|[-A-Za-z0-9]+([0-9]+):\\) "
  "*The regular expression which matches the Common Lisp prompt.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be a prompt.")

;;;;
;;; Franz Lisp Variables and Constants
;;;;

(defvar fi:franz-lisp-buffer-name "*franz-lisp*"
  "*Default buffer name used by fi:franz-lisp.")

(defvar fi:franz-lisp-image-name "lisp"
  "*Default Franz Lisp image to invoke from `fi:franz-lisp'.  If the value
is a string then it names the image file or image path that
`fi:franz-lisp' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:franz-lisp-image-arguments nil
  "*Default Franz Lisp image arguments when invoked from `fi:franz-lisp'.")

(defvar fi:franz-lisp-host "localhost"
  "*")

(defvar fi:franz-lisp-process-name nil)

(defvar fi:franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "*The regular expression which matches the Franz Lisp prompt, used in
Inferior Franz Lisp mode.  Anything from beginning of line up to the end
of what this pattern matches is deemed to be a prompt.")

;;;;;;;;;;;;;;;;;;;;;; general subprocess internal variables

(defvar fi::last-input-start nil
  "Marker for start of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")
(make-variable-buffer-local 'fi::last-input-start)

(defvar fi::last-input-end nil
  "Marker for end of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")
(make-variable-buffer-local 'fi::last-input-end)

(defvar fi::shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")
(make-variable-buffer-local 'fi::shell-directory-stack)

;;;;;;;;;;;;;;;;;;;;;; lisp mode specific internal variables

(defvar fi::process-name nil
  "Name of inferior lisp process.")
(make-variable-buffer-local 'fi::process-name)

(defvar fi::common-lisp-first-time t)

(defvar fi::franz-lisp-first-time t)

;;;;;;;;;;;;;;;;;;;;;; common lisp mode specific internal variables

(defvar fi::common-lisp-backdoor-main-process-name nil
  "The name of the Common Lisp process to which we have a backdoor
connection.")

(defvar fi::lisp-case-mode ':unknown
  "The case in which the Common Lisp we are connected to lives.")

(defvar fi::listener-protocol ':listener)

;;;; the rest are buffer local:

(defvar fi::lisp-host nil 
"Host that is running the lisp in this buffer.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-host)

(defvar fi::lisp-port nil 
"Port to use in getting new listeners from remote lisp.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-port)

(defvar fi::lisp-password nil 
"Password to use in getting new listeners from remote lisp.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-password)

(defvar fi::lisp-ipc-version nil
  "The version of the IPC to which will connect. Buffer local")
(make-variable-buffer-local 'fi::lisp-ipc-version)

(defvar fi::lisp-is-remote nil
  "Non-nil if the lisp process tied to the current buffer is on another
machine, which implies that it was started via an `rsh'.  This variable is
buffer local.")
(make-variable-buffer-local 'fi::lisp-is-remote)

;;;;
;;; User visible functions
;;;;

(defvar fi::rsh-command "rsh")
(defvar fi::rsh-args nil)

(defun fi::start-backdoor-interface (proc)
  (fi:verify-emacs-support)
  (setq fi::common-lisp-backdoor-main-process-name (process-name proc))
  (fi::reset-metadot-session))

(defun fi:common-lisp (&optional buffer-name directory image-name
				 image-args host)
  "Create a Common Lisp subprocess and put it in buffer named by
BUFFER-NAME, with default-directory of DIRECTORY, using IMAGE-NAME
and IMAGE-ARGS as the binary image pathname and command line
arguments, doing the appropriate magic to execute the process on HOST.

The first time this function is called and when given a prefix argument, all
the above quantities are read from the minibuffer, with defaults coming
from the variables:
	fi:common-lisp-buffer-name
	fi:common-lisp-directory
	fi:common-lisp-image-name
	fi:common-lisp-image-arguments
	fi:common-lisp-host
and the values read are saved in these variables for later use as defaults.
After the first time or when no prefix argument is given, the defaults are
used and no information is read from the minibuffer.

For backward compatibility, BUFFER-NAME can be a number, when called
programmatically, which means look for, and use if found, numbered buffers
of the form \"*common-lisp*<N>\" for N > 2.  If BUFFER-NAME < 0, then find
the first \"free\" buffer name and start a subprocess in that buffer."
  (interactive
   (fi::get-lisp-interactive-arguments fi::common-lisp-first-time
				       fi:common-lisp-buffer-name
				       (let ((name fi:common-lisp-buffer-name))
					 (if (numberp current-prefix-arg)
					     (fi::buffer-number-to-buffer
					      name current-prefix-arg)
					   (get-buffer-create name)))
				       (or fi:common-lisp-directory
					   default-directory)
				       fi:common-lisp-image-name
				       fi:common-lisp-image-arguments
				       fi:common-lisp-host))
  (when (and fi::shell-buffer-for-common-lisp-interaction-host-name
	     (or (y-or-n-p "A make-dist might be in progress.  Continue? ")
		 (error "fi:common-lisp aborted.")))
    (setq fi::shell-buffer-for-common-lisp-interaction-host-name nil))
  (let* ((buffer-name (if (interactive-p)
			  (or buffer-name fi:common-lisp-buffer-name)
			fi:common-lisp-buffer-name))
	 (directory (if (interactive-p)
			(expand-file-name directory)
		      (or directory fi:common-lisp-directory)))
	 (image-name (if (interactive-p)
			 image-name
		       (or image-name fi:common-lisp-image-name)))
	 (image-args (if (interactive-p)
			 image-args
		       (or image-args fi:common-lisp-image-arguments)))
	 (host (if (interactive-p)
		   host
		 (or host fi:common-lisp-host)))
	 
	 (local (or (string= "localhost" host)
		    (string= host (system-name))))
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
		   (let* ((buffer-name (fi::calc-buffer-name buffer-name
							     "common-lisp"))
			  (buffer (get-buffer buffer-name)))
		     (or (not (fi::lep-open-connection-p))
			 (and buffer
			      (eq buffer (fi::connection-buffer
					  fi::*connection*)))))
		   )
	      (append (funcall fi:start-lisp-interface-arguments
			       fi:use-background-streams)
		      image-args)
	    image-args))
	 (startup-message
	  (concat
	   "\n==============================================================\n"
	   (format "Starting image `%s'\n" image-name)
	   (if image-args
	       (format "  with arguments `%s'\n" image-args)
	     "  with no arguments\n")
	   (format "  in directory `%s'\n" directory)
	   (format "  on machine `%s'.\n" host)
	   "\n"))
	 (proc (fi::make-subprocess
		startup-message
		"common-lisp"
		buffer-name
		directory
		'fi:inferior-common-lisp-mode
		fi:common-lisp-prompt-pattern
		(if local image-name fi::rsh-command)
		(if local
		    real-args
		  (fi::remote-lisp-args host image-name real-args
					directory))
		'fi::common-lisp-subprocess-filter
		'fi::start-backdoor-interface
		;;
		;; rest of the arguments are the
		;; mode-hook function and its arguments
		(function
		 (lambda (local host dir)
		   (if local
		       (save-excursion
			 (set-buffer (current-buffer))
			 (setq fi::lisp-host "localhost"))
		     (save-excursion
		       (set-buffer (current-buffer))
		       (setq fi::lisp-is-remote t)
		       (setq fi::lisp-host host)
		       (condition-case ()
			   (cd dir)
			 (error nil))))))
		local host directory)))
    (when (interactive-p)
      (setq fi::common-lisp-first-time nil
	    fi:common-lisp-buffer-name buffer-name
	    fi:common-lisp-directory directory
	    fi:common-lisp-image-name image-name
	    fi:common-lisp-image-arguments image-args
	    fi:common-lisp-host host))
    proc))

(defun fi:open-lisp-listener (&optional buffer-number
					buffer-name
					setup-function)
  "Open a connection to an existing Common Lisp process, started with the
function fi:common-lisp, and create a Lisp Listener (a top-level
interaction).  The Common Lisp can be either local or remote.  The name of
the buffer is \"*lisp-listener*\" with an optional suffix of \"<N>\", for
prefix arguments > 1.  If a negative prefix argument is given, then the
first \"free\" buffer name is found and used.  When called from a program,
the buffer name is the second optional argument."
  (interactive "p")
  (if (or (null fi::common-lisp-backdoor-main-process-name)
	  (not (fi:process-running-p
		(get-process fi::common-lisp-backdoor-main-process-name))))
      (error "Common Lisp must be running to open a lisp listener."))
  (unless setup-function
    (setq setup-function 'fi::setup-tcp-connection))
  (let* ((buffer
	  (process-buffer
	   (get-process fi::common-lisp-backdoor-main-process-name)))
	 (proc (fi::make-tcp-connection (or buffer-name "lisp-listener")
					buffer-number
					'fi:lisp-listener-mode
					fi:common-lisp-prompt-pattern
					(fi::get-buffer-host buffer)
					(fi::get-buffer-port buffer)
					(fi::get-buffer-password buffer)
					(fi::get-buffer-ipc-version buffer)
					setup-function)))
    proc))

(defun fi::setup-tcp-connection (proc)
  (format
   "(progn
      (setf (getf (mp:process-property-list mp:*current-process*) %s) %d)
      (values))\n"
   ':emacs-listener-number
   (fi::tcp-listener-generation proc)))

(defun fi:franz-lisp (&optional buffer-name directory image-name
				image-args host)
  "Create a Franz Lisp subprocess and put it in buffer named by
BUFFER-NAME, with default-directory of DIRECTORY, using IMAGE-NAME
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
				       fi:franz-lisp-host))
  (let* ((buffer-name (if (interactive-p)
			  buffer-name
			(or buffer-name fi:franz-lisp-buffer-name)))
	 (directory (if (interactive-p)
			(expand-file-name directory)
		      (or directory fi:franz-lisp-directory)))
	 (image-name (if (interactive-p)
			 image-name
		       (or image-name fi:franz-lisp-image-name)))
	 (image-args (if (interactive-p)
			 image-args
		       (or image-args fi:franz-lisp-image-arguments)))
	 (host (if (interactive-p)
		   host
		 (or host fi:franz-lisp-host)) )
	 (local (or (string= "localhost" host)
		    (string= host (system-name))))
	 (proc (fi::make-subprocess
		nil
		"franz-lisp"
		buffer-name
		directory
		'fi:inferior-franz-lisp-mode
		fi:franz-lisp-prompt-pattern
		(if local image-name fi::rsh-command)
		(if local
		    image-args
		  (fi::remote-lisp-args host image-name image-args
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
		local directory)))
    (setq fi:franz-lisp-process-name (process-name proc))
    (when (interactive-p)
      (setq fi::franz-lisp-first-time nil
	    fi:franz-lisp-buffer-name buffer-name
	    fi:franz-lisp-directory directory
	    fi:franz-lisp-image-name image-name
	    fi:franz-lisp-image-arguments image-args
	    fi:franz-lisp-host host))
    proc))

;;;;
;;; Internal functions
;;;;

(defun fi::remote-lisp-args (host image-name image-args directory)
  (append
   fi::rsh-args
   (list
    host
    "sh"
    "-c"
    (format "%s%s if test -d %s; then cd %s; fi; %s %s%s"
	    (if (string= "rsh" (file-name-nondirectory fi::rsh-command))
		"'" "")
	    (fi::env-vars)
	    directory
	    directory
	    image-name
	    (mapconcat (function
			(lambda (x)
			  (if x
			      (concat "\"" x "\"")
			    "")))
		       image-args
		       " ")
	    (if (string= "rsh" (file-name-nondirectory fi::rsh-command))
		"'" "")))))

(defun fi::get-lisp-interactive-arguments (first-time buffer-name buffer
					   directory image-name image-args
					   host)
  (let ((buffer-name (or (and buffer (buffer-name buffer))
			 buffer-name))
	local)
    (if (or first-time
	    (and current-prefix-arg
		 (or (null buffer)
		     (not (get-buffer-process buffer))
		     (not (fi:process-running-p
			   (get-buffer-process buffer))))))
	(list (setq buffer-name (read-buffer "Buffer: " buffer-name))
	      (progn
		(setq host (read-string "Host: " host))
		(setq local (or (string= "localhost" host)
				(string= host (system-name))))
		(let ((dir (expand-file-name
			    (read-file-name "Process directory: "
					    (or directory
						default-directory)
					    (or directory
						default-directory)
					    local))))
		  (setq directory
		    (if (= ?/ (aref dir (- (length dir) 1)))
			dir
		      (concat dir "/")))))
	      (expand-file-name
	       (read-file-name "Image name: " image-name image-name nil))
	      (setq image-args
		(fi::listify-string
		 (read-from-minibuffer
		  "Image arguments (separate by spaces): "
		  (mapconcat 'concat image-args " "))))
	      host)
      (list buffer-name directory image-name image-args host))))

(defun fi::common-lisp-subprocess-filter (process output &optional stay
								   cruft)
  (let ((val (catch 'cl-subproc-filter-foo
	       (fi::common-lisp-subprocess-filter-1 process))))
    (case val
      (normal
       (fi::subprocess-filter process output stay cruft)
       (set-process-filter process 'fi::subprocess-filter))
      (t
       (if (and (consp val) (eq 'error (car val)))
	   (progn
	     (message "%s" (cdr val))
	     (switch-to-buffer "*Help*"))
	 (fi::subprocess-filter process output stay cruft))))))

(defun fi::common-lisp-subprocess-filter-1 (process)
  ;; This is a temporary filter, which is used until the rendezvous with
  ;; Lisp is made.
  (save-excursion
    (set-buffer (process-buffer process))
    (if (not (fi::lep-open-connection-p))
	(if (and (fi::fast-search-string 1 output)
		 (string-match "\\([^\0]*\\)\\(.*\\)\\([^\0]*\\)"
			       output))
	    (let* ((res (concat (substring output (match-beginning 1)
					   (match-end 1))
				(substring output (match-beginning 3)
					   (match-end 3))))
		   (command (substring output (match-beginning 2)
				       (match-end 2)))
		   (xx nil)
		   (host nil))
	      (setq fi::lisp-port
		(car (setq xx (read-from-string command nil))))
	      (setq fi::lisp-password
		(car (setq xx (read-from-string command (cdr xx)))))
	      (setq fi::lisp-case-mode
		(car (setq xx
		       (read-from-string (downcase command) (cdr xx)))))
	      ;; the following "argument" is optional in that a previous
	      ;; version of the ipc.cl didn't provide it
	      (if (setq host
		    (condition-case ()
			(car (setq xx (read-from-string
				       (fi::frob-case-from-lisp command)
				       (cdr xx))))
		      (error nil)))
		  nil)
	      ;; This is optional also
	      (setq fi::lisp-ipc-version
		(condition-case ()
		    (if (not (eq (cdr xx) (length command))) 
			(car (setq xx (read-from-string
				       (fi::frob-case-from-lisp command)
				       (cdr xx)))))
		  (error nil)))
	      (setq output res)

	      (condition-case condition
		  (progn
		    (cond ((consp fi:start-lisp-interface-hook)
			   (mapcar 'funcall fi:start-lisp-interface-hook))
			  (fi:start-lisp-interface-hook
			   (funcall fi:start-lisp-interface-hook)))
		    (throw 'cl-subproc-filter-foo 'normal))
		(error (throw 'cl-subproc-filter-foo
			 (cons 'error condition)))))))))

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
	 (runningp (fi:process-running-p process))
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
    
    (funcall fi:display-buffer-function buffer)
    
    (if runningp
	(goto-char (point-max))
      (goto-char (point-max))
      (if (stringp startup-message) (insert startup-message))
      (if (and directory (file-exists-p directory))
	  (setq default-directory directory))
      (if process (delete-process process))
      (fi::set-environment fi:subprocess-env-vars)
      (setq process
	(apply 'start-process
	       (append (list buffer-name buffer image-file)
		       image-args)))
      (set-process-sentinel process 'fi::subprocess-sentinel)

      ;; do the following after the sentinel is established so we don't get
      ;; an ugly message in the subprocess buffer
      ;;
      (when (not (fi:process-running-p process))
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
	 (send-string process start-up-feed-name)))
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (condition-case ()
	  (let ((saved-input-ring fi::input-ring)
		(saved-input-ring-yank-pointer fi::input-ring-yank-pointer))
	    (apply mode-function mode-hook mode-hook-arguments)
	    (setq fi::input-ring saved-input-ring)
	    (setq fi::input-ring-yank-pointer saved-input-ring-yank-pointer))
	(error nil))
      (make-local-variable 'subprocess-prompt-pattern)
      (setq subprocess-prompt-pattern image-prompt)
      (fi::make-subprocess-variables)
      (when initial-func (funcall initial-func process)))
    process))

(defun fi::calc-buffer-name (buffer-name process-name)
  (cond ((stringp buffer-name) buffer-name)
	(t (let ((name (concat "*" process-name "*")))
	     (if (numberp buffer-name)
		 (fi::buffer-number-to-buffer name buffer-name)
	       name)))))

(defvar fi::tcp-listener-table nil)
(defvar fi::tcp-listener-generation 0)

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
					  given-ipc-version
					  setup-function)
  (if (not fi::common-lisp-backdoor-main-process-name)
      (error "A Common Lisp subprocess has not yet been started."))
  (let* ((buffer-name
	  (fi::buffer-number-to-buffer (concat "*" buffer-name "*")
				     buffer-number))
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (default-dir default-directory)
	 (buffer-name (buffer-name buffer))
	 (process-buffer
	  (if (get-process fi::common-lisp-backdoor-main-process-name)
	      (process-buffer
	       (get-process fi::common-lisp-backdoor-main-process-name))))
	 (host (or given-host (fi::get-buffer-host process-buffer)))
	 (service (or given-service (fi::get-buffer-port process-buffer)))
	 (password (or given-password
		       (fi::get-buffer-password process-buffer)))
	 (proc (get-buffer-process buffer)))
    
    (funcall fi:display-buffer-function buffer)
    
    (if (fi:process-running-p proc)
	(goto-char (point-max))
      (setq default-directory default-dir)
      (setq proc (open-network-stream buffer-name buffer host service))
      (set-process-sentinel proc 'fi::tcp-sentinel)
      ;;
      ;; The first input the new (Common Lisp) process is sent is the name
      ;; of the process.  This is so that the processes are named similarly
      ;; in Emacs and Lisp.
      ;;
      (when given-ipc-version
	(process-send-string
	 proc
	 (format "%s\n" (prin1-to-string fi::listener-protocol))))
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
      (make-local-variable 'subprocess-prompt-pattern)
      (setq subprocess-prompt-pattern image-prompt)
      (fi::make-subprocess-variables))
    proc))

(defun fi::subprocess-sentinel (process status)
  ;; Sentinel and filter for subprocesses.  The sentinel currently
  ;; does nothing, other than prevent the status change message when the
  ;; process dies.
  t)

(defun fi::tcp-sentinel (process status)
  ;; Sentinel and filter for network connections.  The sentinel currently
  ;; does nothing, other than prevent the status change message when the
  ;; connection is closed.
  (set-buffer (process-buffer process))
  (goto-char (point-max))
  (insert
   (format
    "---------------------------------------------------------------------\n"))
  t)

(defun fi::subprocess-filter (process output &optional stay cruft)
  "Filter output from processes tied to buffers.
This function implements continuous output to visible buffers."
  (let ((inhibit-quit t))
    (if cruft
	(setq output (fi::substitute-chars-in-string '((?\r)) output)))
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
	;;   `(insert-string)' followed by `(set-marker)' to avoid this
	;;   problem.  This also happens to be the way
	;;   `handle_process_output()' deals with this in `process.c'.
	(insert-string output)
	(set-marker marker (point)))
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
       (in-buffer nil)
       (stay old-buffer)
       (t (set-buffer old-buffer))))))

(defun fi::buffer-number-to-buffer (name number)
  (if (string-match "^\\(.*\\)<[0-9]+>$" name)
      (setq name (substring name (match-beginning 1) (match-end 1))))
  (let ((buffer-name
	 (cond ((> number 1) (concat name "<" number ">"))
	       ((< number 0)
		(let (buffer-name n)
		  (if (not (fi:process-running-p (setq buffer-name name)))
		      buffer-name
		    (setq n 2)
		    (while (fi:process-running-p
			    (setq buffer-name (concat name "<" n ">")))
		      (setq n (+ n 1)))
		    buffer-name)))
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
  "Watch for special commands like, for example, `cd' in a shell."
  (if (null fi::shell-directory-stack)
      (setq fi::shell-directory-stack (list default-directory)))
  (condition-case ()
      ;; "To err is really not nice." -dkl 11/21/88
      (save-excursion
	(goto-char fi::last-input-start)
	(cond
	  ((and fi:in-package-regexp (looking-at fi:in-package-regexp))
	   (goto-char (match-end 0))
	   (cond
	     ((or (looking-at "[ \t]*[':]\\(.*\\)[ \t]*)")
		  (looking-at "[ \t]*\"\\(.*\\)\"[ \t]*)")
		  (looking-at "[ \t]*\\(.*\\)[ \t]*)"))
	      ;; (in-package foo)
	      (setq fi:package
		(buffer-substring (match-beginning 1) (match-end 1))))
	     ((looking-at "[ \t]+\\(.*\\)[ \t]*$")
	      ;; :pa foo
	      (setq fi:package
		(buffer-substring (match-beginning 1) (match-end 1)))))
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
		    (cd (car (setq fi::shell-directory-stack
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
		      (cd (car head)))))))
	     ((looking-at "[ \t]+\\([^ \t]+\\)[;\n]")
	      ;; pushd dir
	      (let ((dir (expand-file-name
			  (substitute-in-file-name
			   (buffer-substring (match-beginning 1)
					     (match-end 1))))))
		(if (file-directory-p dir)
		    (progn
		      (setq fi::shell-directory-stack
			(cons dir fi::shell-directory-stack))
		      (cd dir)))))
	     ((looking-at "[ \t]*[;\n]")
	      ;; pushd
	      (if (< (length fi::shell-directory-stack) 2)
		  (message "Directory stack not that deep.")
		(setq fi::shell-directory-stack
		  (append (list (car (cdr fi::shell-directory-stack))
				(car fi::shell-directory-stack))
			  (cdr (cdr fi::shell-directory-stack))))
		(cd (car fi::shell-directory-stack))))))
	  ((and fi:shell-cd-regexp (looking-at fi:shell-cd-regexp))
	   (goto-char (match-end 0))
	   (cond
	     ((looking-at ".*&[ \t]*$")
	      ;; "cd foo &" executes in a subshell!
	      )
	     ((looking-at "[ \t]*[;\n]")
	      ;; cd
	      (cd (rplaca fi::shell-directory-stack (getenv "HOME"))))
	     ((looking-at "[ \t]+\\([^ \t]+\\)[ \t]*[;\n]")
	      ;; cd dir
	      (let ((dir (expand-file-name
			  (substitute-in-file-name
			   (buffer-substring (match-beginning 1)
					     (match-end 1))))))
		(if (file-directory-p dir)
		    (progn
		      (rplaca fi::shell-directory-stack dir)
		      (cd dir)))))))))
    (error nil)))

(defun fi::get-buffer-host (buffer)
  "Given BUFFER return the value in this buffer of fi::lisp-host."
  (save-excursion
    (set-buffer buffer)
    fi::lisp-host))

(defun fi::get-buffer-port (buffer)
  "Given BUFFER return the value in this buffer of fi::lisp-port."
  (save-excursion
    (set-buffer buffer)
    fi::lisp-port))

(defun fi::get-buffer-password (buffer)
  "Given BUFFER returns the values in this buffer of fi::lisp-password"
  (save-excursion
    (set-buffer buffer)
    fi::lisp-password))


(defun fi::get-buffer-ipc-version (buffer)
  "Given BUFFER returns the values in this buffer of fi::lisp-password"
  (save-excursion
    (set-buffer buffer)
    fi::lisp-ipc-version))

(defun fi::env-vars ()
  (concat (mapconcat '(lambda (x)
		       (format "%s=%s" (car x) (eval (eval (cdr x)))))
		     fi:subprocess-env-vars
		     " ")
	  " export "
	  (mapconcat '(lambda (x) (car x)) fi:subprocess-env-vars " ")
	  "; "))

(defun fi::set-environment-use-setenv (valist)
  (let (item val)
    (while valist
      (setq item (car valist))
      (setq val (or (eval (cdr item)) ""))
      (setenv (car item) val)
      (setq valist (cdr valist)))))

(defun fi::set-environment-use-process-environment (valist)
  (let ((v valist))
    (while v
      (let ((pe process-environment)
	    (found nil))
	(while (and (not found) pe)
	  (if (string-match (concat "^" (car (car v)) "=") (car pe))
	      (progn
		(rplaca pe (format "%s=%s" (car (car v)) (eval (cdr (car v)))))
		(setq found t))
	    (setq pe (cdr pe))))
	(if (not found)
	    (setq process-environment
	      (cons (format "%s=%s" (car (car v)) (eval (cdr (car v))))
		    process-environment))))
      (setq v (cdr v)))))

(fset 'fi::set-environment
      (if (boundp 'process-environment)
	  (symbol-function 'fi::set-environment-use-process-environment)
	(symbol-function 'fi::set-environment-use-setenv)))
