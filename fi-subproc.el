;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.112 1991/06/20 20:02:38 layer Exp $

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
  "(in-package\\>\\|:\\<pa\\>\\|:\\<pac\\>\\|:\\<pack\\>\\|:\\<packa\\>\\|:\\<packag\\>\\|:\\<package\\>"
  "*The regular expression matching the Lisp expression to change the
current package.  The two things this must match are the IN-PACKAGE macro
form and all the possible instances of the :package top-level command.
If nil, no automatic package tracking will be done.")

(defvar fi:pop-to-sublisp-buffer-after-lisp-eval t
  "*If non-nil, then go to the Lisp subprocess buffer after sending
expressions to Lisp (via fi:lisp-eval-* function).")

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
fi:lisp-eval-* functions will be echoed by Common Lisp.")

(defvar fi:start-lisp-interface-function
    'fi:start-backdoor-interface
  "*If non-nil, the function which is called to startup the Emacs-Lisp
interface.  This happens automatically when the Common Lisp process is
started with fi:common-lisp.")

(defvar fi:start-lisp-interface-hook nil
  "*A function or a list of functions to call when we get the rendezvous
info from Lisp in the Lisp subprocess buffer.  This is used to
automatically startup the Emacs-Lisp hidden communication via a socket.
fi:start-lisp-interface-function initiates the connection with Lisp from
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

(defun fi:start-backdoor-interface (process)
  "Send a string to PROCESS, which should be a Lisp, that starts the
Emacs-Lisp interface.  This only works in Allegro CL, currently."
  (unless (fi::lep-open-connection-p)
    (setq fi::common-lisp-backdoor-main-process-name (process-name process))
    (send-string
     process
     "(progn
      (princ \";; Starting socket daemon\n\")
      (force-output)
      (excl::require :ipc)
      (excl::require :emacs)
      #+allegro-v4.1 (excl::require :lep)
      #+allegro-v4.1 (excl::require :scm)
      (apply
       (find-symbol (symbol-name :start-lisp-listener-daemon) :ipc)
       #+allegro-v4.1 '(:use-lep t :unix-domain nil)
       #-allegro-v4.1 nil)
      (values))\n")))

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
   (fi::get-lisp-interactive-arguments 'fi::common-lisp-first-time
				       'fi:common-lisp-buffer-name
				       (let ((name "*common-lisp*"))
					 (if (numberp current-prefix-arg)
					     (fi::buffer-number-to-buffer
					      name current-prefix-arg)
					   (get-buffer-create name)))
				       'fi:common-lisp-directory
				       'fi:common-lisp-image-name
				       'fi:common-lisp-image-arguments
				       'fi:common-lisp-host))
  (let* ((buffer-name (or buffer-name fi:common-lisp-buffer-name))
	 (directory (expand-file-name (or directory fi:common-lisp-directory)))
	 (image-name (or image-name fi:common-lisp-image-name))
	 (image-args (or image-args fi:common-lisp-image-arguments))
	 (host (or host fi:common-lisp-host))
	 (local (or (string= "localhost" host)
		    (string= host (system-name))))
	 (startup-message
	  (format "\n%s\nStarting image `%s'\n  in directory `%s'\n  on machine `%s'.\n\n"
		  "====================================================="
		  image-name directory host))
	 (proc (fi::make-subprocess
		startup-message
		"common-lisp"
		buffer-name
		directory
		'fi:inferior-common-lisp-mode
		fi:common-lisp-prompt-pattern
		(if local image-name "rsh")
		(if local
		    image-args
		  (fi::remote-lisp-args host image-name image-args
					directory))
		'fi::common-lisp-subprocess-filter
		fi:start-lisp-interface-function
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
    (setq fi::common-lisp-first-time nil)
    proc))

(defun fi:goto-common-lisp ()
  "This interactive function is meant to be bound to a global key, so that
getting to the *common-lisp* buffer can be done with a minimum of key
strokes.  You can, for example, put the following form in your ~/.emacs
file:

	(define-key global-map \"\\C-xl\" 'fi:goto-common-lisp)

and typing ``C-x l'' provide an easy way to start a Common Lisp and go to it."
  (interactive)
  (call-interactively 'fi:common-lisp))

(defun fi:open-lisp-listener (&optional buffer-number buffer-name)
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
  (let ((proc (fi::make-tcp-connection
	       (or buffer-name "lisp-listener")
	       buffer-number
	       'fi:lisp-listener-mode
	       fi:common-lisp-prompt-pattern
	       (fi::get-buffer-host fi::common-lisp-backdoor-main-process-name)
	       (fi::get-buffer-port fi::common-lisp-backdoor-main-process-name)
	       (fi::get-buffer-password fi::common-lisp-backdoor-main-process-name)
	       (fi::get-buffer-ipc-version fi::common-lisp-backdoor-main-process-name))))
    proc))

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
   (fi::get-lisp-interactive-arguments 'fi::franz-lisp-first-time
				       'fi:franz-lisp-buffer-name
				       (let ((name "*franz-lisp*"))
					 (if (numberp current-prefix-arg)
					     (fi::buffer-number-to-buffer
					      name current-prefix-arg)
					   (get-buffer-create name)))
				       'fi:franz-lisp-directory
				       'fi:franz-lisp-image-name
				       'fi:franz-lisp-image-arguments
				       'fi:franz-lisp-host))
  (let* ((buffer-name (or buffer-name fi:franz-lisp-buffer-name))
	 (directory (or directory fi:franz-lisp-directory))
	 (image-name (or image-name fi:franz-lisp-image-name))
	 (image-args (or image-args fi:franz-lisp-image-arguments))
	 (host (or host fi:franz-lisp-host))
	 
	 (local (or (string= "localhost" host)
		    (string= host (system-name))))
	 (proc (fi::make-subprocess
		nil
		"franz-lisp"
		buffer-name
		directory
		'fi:inferior-franz-lisp-mode
		fi:franz-lisp-prompt-pattern
		(if local image-name "rsh")
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
    (setq fi::franz-lisp-first-time nil)
    proc))

;;;;
;;; Internal functions
;;;;

(defun fi::remote-lisp-args (host image-name image-args directory)
  (append (list host
		(format "sh -c '%s if test -d %s; then cd %s; fi; "
			(fi::env-vars)
			directory
			directory)
		image-name)
	  image-args
	  '("'")))

(defun fi::get-lisp-interactive-arguments (s-first-time s-buffer-name
					   buffer
					   s-directory s-image-name
					   s-image-args s-host)
  (let ((first-time (symbol-value s-first-time))
	(buffer-name (or (and buffer (buffer-name buffer))
			 (symbol-value s-buffer-name)))
	(directory (symbol-value s-directory))
	(image-name (symbol-value s-image-name))
	(image-args (symbol-value s-image-args))
	(host (symbol-value s-host))
	local)
    (if (or first-time
	    (and current-prefix-arg
		 (or (null buffer)
		     (not (get-buffer-process buffer))
		     (not (fi:process-running-p
			   (get-buffer-process buffer))))))
	(prog1
	    (list (setq buffer-name
		    (or current-prefix-arg
			(read-buffer "Buffer: " buffer-name)))
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
		  (let ((image
			 (fi::canonicalize-filename
			  (expand-file-name
			   (if (stringp image-name)
			       (if (= ?/ (aref image-name 0))
				   image-name
				 (format "%s%s" directory image-name))
			     directory)))))
		    (setq image-name
		      (expand-file-name
		       (read-file-name (format "Image name [%s]: " image)
				       directory image local))))
		  (setq image-args
		    (fi::listify-string
		     (read-from-minibuffer
		      "Image arguments (separate by spaces): "
		      (mapconcat 'concat image-args " "))))
		  host)
	  (set s-buffer-name buffer-name)
	  (set s-directory directory)
	  (set s-image-name image-name)
	  (set s-image-args image-args)
	  (set s-host host))
      (list buffer-name directory image-name image-args host))))

(defun fi::common-lisp-subprocess-filter (process output &optional stay cruft)
  (save-excursion
    (set-buffer (process-buffer process))
    (if (not (fi::lep-open-connection-p))
	(setq output
	  (if (and (fi::fast-search-string 1 output)
		   (string-match
		    "\\([^\0]*\\)\\(.*\\)\\([^\0]*\\)"
		    output))
	      (let* ((res (concat
			   (substring output (match-beginning 1)
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
		(cond ((consp fi:start-lisp-interface-hook)
		       (mapcar 'funcall fi:start-lisp-interface-hook))
		      (fi:start-lisp-interface-hook
		       (funcall fi:start-lisp-interface-hook)))
		res)
	    output))))
  (fi::subprocess-filter process output stay cruft))

(defun fi::make-subprocess (startup-message process-name buffer-name
			    directory mode-function image-prompt image-file
			    image-args
			    &optional filter
				      initial-func
				      mode-hook
			    &rest mode-hook-arguments)
  (let* ((remote (and (stringp image-file) (string= "rsh" image-file)))
	 (buffer-name
	  (cond ((stringp buffer-name) buffer-name)
		(t (let ((name (concat "*" process-name "*")))
		     (if (numberp buffer-name)
			 (fi::buffer-number-to-buffer name buffer-name)
		       name)))))
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
	 ;; I hope 1 second is enough!
	 (sleep-for 1)
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
      (if initial-func (funcall initial-func process)))
    process))

(defun fi::make-tcp-connection (buffer-name buffer-number mode image-prompt
				&optional given-host
					  given-service
					  given-password
					  given-ipc-version)
  (if (not fi::common-lisp-backdoor-main-process-name)
      (error "A Common Lisp subprocess has not yet been started."))
  (let* ((buffer-name
	  (fi::buffer-number-to-buffer (concat "*" buffer-name "*")
				     buffer-number))
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (default-dir default-directory)
	 (buffer-name (buffer-name buffer))
	 (host (or given-host
		   (fi::get-buffer-host
		    fi::common-lisp-backdoor-main-process-name)))
	 (service (or given-service
		      (fi::get-buffer-port
		       fi::common-lisp-backdoor-main-process-name)))
	 (password (or given-password
		       (fi::get-buffer-password
			fi::common-lisp-backdoor-main-process-name)))
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
      (process-send-string proc (format " %d \n" password))
      
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
		  (looking-at "[ \t]*\"\\(.*\\)\"[ \t]*)"))
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

(defun fi::set-environment (valist)
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

