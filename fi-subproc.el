;;
;; copyright (C) 1987, 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.73 1990/10/17 16:44:23 layer Exp $

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
;;; Variables and Constants
;;;;

(defvar fi:start-lisp-interface-function
    'fi::start-tcp-lisp-interface
  "*If non-nil, then this function is funcalled to startup the GNU
Emacs-Lisp interface.")

(defvar fi:remote-lisp-track-image-name-directory nil
  "*If non-nil, then fi:remote-common-lisp and
fi:explicit-remote-common-lisp will cause both Emacs and Lisp to cd into
the directory where the Common Lisp image resides.  Otherwise, the remote
Common Lisp will have $HOME as its current working directory and Emacs will
have whatever directory you were in when you evaluated either function.")

(defvar fi:common-lisp-image-name "cl"
  "*Default Common Lisp image to invoke from `fi:common-lisp'.  If the
value is a string then it names the image file or image path that
`fi:common-lisp' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:default-explicit-common-lisp-image-name nil
  "*If non-nil, then the value of this variable is used as the name of the
image fi:explicit-common-lisp and fi:explicit-remote-common-lisp uses to
run Lisp, instead of reading the name from the minibuffer.")

(defvar fi:default-explicit-common-lisp-image-arguments nil
  "*If non-nil, then the value of this variable (a string) contains the
arguments given to Lisp by fi:explicit-common-lisp and
fi:explicit-remote-common-lisp when Lisp is invoked, instead of reading the
name from the minibuffer.")

(defvar fi:default-remote-common-lisp-host nil
  "*If non-nil, then the value of this variable (a string) is names the
host on which fi:remote-common-lisp and fi:explicit-remote-common-lisp
executes Lisp, instead of reading the name from the minibuffer.")

(defvar fi:default-remote-common-lisp-directory nil
  "*If non-nil, used by fi:remote-common-lisp and
fi:explicit-remote-common-lisp as the directory in which the Lisp is
started.  Note that the pathname of the Lisp image must be give relative to
this directory, or the home directory if this variable is nil.  See
fi:fi:default-explicit-common-lisp-image-name")

(defvar fi:common-lisp-image-arguments nil
  "*Default Common Lisp image arguments when invoked from `fi:common-lisp',
which must be a list of strings.")

(defvar fi:common-lisp-prompt-pattern
  "^\\(\\[[0-9]+c?\\] \\|\\[step\\] \\)?<[-A-Za-z]* ?[0-9]*?> "
  "*The regular expression which matches the Common Lisp prompt, used in
Inferior Common Lisp mode.  Anything from beginning of line up to the end
of what this pattern matches is deemed to be a prompt.")

(defvar fi:franz-lisp-image-name "lisp"
  "*Default Franz Lisp image to invoke from `fi:franz-lisp'.  If the value
is a string then it names the image file or image path that
`fi:common-lisp' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:franz-lisp-image-arguments nil
  "*Default Franz Lisp image arguments when invoked from `fi:franz-lisp'.")

(defvar fi:franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "*The regular expression which matches the Franz Lisp prompt, used in
Inferior Franz Lisp mode.  Anything from beginning of line up to the end
of what this pattern matches is deemed to be a prompt.")

(defvar fi:shell-popd-regexp ":?popd"
  "*The regular expression matching the C shell `popd' command.  If nil, no
automatic directory changes will be made.")

(make-variable-buffer-local 'fi:shell-popd-regexp)

(defvar fi:shell-pushd-regexp ":?pushd"
  "*The regular expression matching the C shell `pushd' command.  If nil,
no automatic directory changes will be made.")

(make-variable-buffer-local 'fi:shell-pushd-regexp)

(defvar fi:shell-cd-regexp ":?cd"
  "*The regular expression matching the C shell `cd' command.  If nil,
no automatic directory changes will be made.")

(make-variable-buffer-local 'fi:shell-cd-regexp)

(defvar fi:common-lisp-package-regexp
  "(in-package\\>\\|:\\<pa\\>\\|:\\<pac\\>\\|:\\<pack\\>\\|:\\<packa\\>\\|:\\<packag\\>\\|:\\<package\\>"
  "*The regular expression matching the Common Lisp expression(s) to change
packages.  If nil, no automatic package tracking will be done.")

(defvar fi:subprocess-map-nl-to-cr nil
  "*If t, then map newline to carriage-return.")

(make-variable-buffer-local 'fi:subprocess-map-nl-to-cr)

(defvar fi:subprocess-continuously-show-output-in-visible-buffer t
  "*If t, output from a subprocess to a visible buffer is continuously
shown.  If a subprocess buffer is visible and the window point is beyond
the process output marker, output to that buffer from its associated
process will be continuously visible.  If the window point is before the
process output marker, the window is not updated.  This is a buffer-local
symbol.")

(make-variable-buffer-local
 'fi:subprocess-continuously-show-output-in-visible-buffer)

(defvar fi:subprocess-write-quantum 120
  "*Maximum size in bytes of a single write request to a subprocess.")

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
  "*A function of one argument, a buffer, which is used to display a
buffer when a subprocess is created.")

;;;;;;;;;;;;;;;;;;;;;; internal vars

(defvar fi::cl-package-regexp nil
  "The real Common Lisp package regexp, which is nil in all buffer except
Inferior Common Lisp buffers.")

(make-variable-buffer-local 'fi::cl-package-regexp)

(defvar fi::last-input-start nil
  "Marker for start of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")

(make-variable-buffer-local 'fi::last-input-start)

(defvar fi::last-input-end nil
  "Marker for end of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")

(make-variable-buffer-local 'fi::last-input-end)

(defvar fi::sublisp-name nil
  "Name of inferior lisp process.")

(defvar fi::freshest-franz-sublisp-name nil
  "Name of franz lisp subprocess most recently invoked.")

(defvar fi::freshest-common-sublisp-name nil
  "Name of common lisp subprocess most recently invoked.")

(defvar fi::shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")

(make-variable-buffer-local 'fi::shell-directory-stack)

(defvar fi::lisp-is-remote nil
  "Non-nil if the lisp process tied to the current buffer is on another
machine, which implies that it was started via an `rsh'.  This variable is
buffer local.")

(make-variable-buffer-local 'fi::lisp-is-remote)

(defconst fi::remote-lisp-sh-prefix
    "sh -ec 'EMACS=t TERM=emacs TERMCAP=emacs:co#%d:tc=unknown: cd %s; "
  "A format string, which takes two arguments: the screen width (%d) and
the directory in which the remote Lisp should execute (%s).")

(defvar fi::remote-host nil 
"Host that is running the lisp in this buffer.  Buffer local.")

(make-variable-buffer-local 'fi::remote-host)

(defvar fi::remote-port nil 
"Port to use in getting new listeners from remote lisp.  Buffer local.")

(make-variable-buffer-local 'fi::remote-port)

(defvar fi::remote-password nil 
"Password to use in getting new listeners from remote lisp.  Buffer local.")

(make-variable-buffer-local 'fi::remote-password)

(defvar fi::lisp-case-mode ':unknown
  "The case in which the ACL we are connected to lives.")

;;;;
;;; User visible functions
;;;;

(defun fi:common-lisp (&optional buffer-number)
  "Start a Common Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER.  Common Lisp buffer names
start with `*common-lisp' and end with `*', with an optional `-N' in
between.  If BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER
is >= 0, then the buffer is named `*common-lisp*<BUFFER-NUMBER>'.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen.

The image file and image arguments are taken from the variables
`fi:common-lisp-image-name' and `fi:common-lisp-image-arguments'.

See fi:explicit-common-lisp."
  (interactive "p")
  (let ((proc (fi::make-subprocess
	       buffer-number
	       "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       fi:common-lisp-image-name
	       fi:common-lisp-image-arguments
	       nil
	       fi:start-lisp-interface-function)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    (fi::set-buffer-host (process-buffer proc) (system-name))
    proc))

(defun fi:explicit-common-lisp (&optional buffer-number
					  image-name image-arguments)
  "The same as fi:common-lisp, except that the image and image arguments
are read from the minibuffer."
  (interactive
   (list 
    current-prefix-arg
    (or fi:default-explicit-common-lisp-image-name
	(expand-file-name (read-file-name "Image name: " nil nil t)))
    (or fi:default-explicit-common-lisp-image-arguments
	(fi::listify-string
	 (read-from-minibuffer "Image arguments (separate by spaces): ")))))
  (let ((proc (fi::make-subprocess
	       buffer-number
	       "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       image-name
	       image-arguments
	       nil
	       fi:start-lisp-interface-function)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    (fi::set-buffer-host (process-buffer proc) (system-name))
    proc))

(defun fi:remote-common-lisp (&optional buffer-number host)
  "Start a Common Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER, where the Common Lisp
image is run on another machine.  Common Lisp buffer names start with
`*common-lisp' and end with `*', with an optional `-N' in between.  If
BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER is >= 0,
then the buffer is named `*common-lisp*<BUFFER-NUMBER>'.  If BUFFER-NUMBER
is < 0, then the first available buffer name is chosen.

The host on which the image is run is read from the minibuffer.

The image file and image arguments are taken from the variables
`fi:common-lisp-image-name' and `fi:common-lisp-image-arguments'.

See fi:explicit-remote-common-lisp."
  (interactive
   (list current-prefix-arg
	 (or fi:default-remote-common-lisp-host
	     (read-from-minibuffer "Remote host name: "))))
  (let* ((remote-dir
	  (expand-file-name
	   (if fi:remote-lisp-track-image-name-directory
	       (let ((dir (file-name-directory image-name)))
		 (if (= ?/ (aref dir 0))
		     dir
		   (format "~/%s" dir)))
	     "~/")))
	 (proc (fi::make-subprocess
		buffer-number
		"common-lisp" 
		'fi:inferior-common-lisp-mode
		fi:common-lisp-prompt-pattern
		"rsh"
		(append
		 (list host
		       (format fi::remote-lisp-sh-prefix
			       (screen-width)
			       (or fi:default-remote-common-lisp-directory
				   remote-dir))
		       (if (and fi:remote-lisp-track-image-name-directory
				(equal
				 fi:default-remote-common-lisp-directory
				 fi:remote-lisp-track-image-name-directory))
			   (file-name-nondirectory fi:common-lisp-image-name)
			 fi:common-lisp-image-name))
		 fi:common-lisp-image-arguments
		 '("'"))
		nil
		fi:start-lisp-interface-function
		(list 'lambda ()
		      '(setq fi::lisp-is-remote t)
		      (list 'cd
			    (or fi:default-remote-common-lisp-directory
				remote-dir))
		      (list 'fi::set-buffer-host '(current-buffer)
			    host)))))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:explicit-remote-common-lisp (&optional buffer-number host
						 image-name image-arguments)
  "The same as fi:remote-common-lisp, except that the image and image
arguments are read from the minibuffer."
  (interactive
   (list 
    current-prefix-arg
    (or fi:default-remote-common-lisp-host
	(read-from-minibuffer "Remote host name: "))
    (or fi:default-explicit-common-lisp-image-name
	(read-from-minibuffer "Image name (relative to home directory): "))
    (or fi:default-explicit-common-lisp-image-arguments
	(fi::listify-string
	 (read-from-minibuffer "Image arguments (separate by spaces): ")))))
  (let* ((remote-dir
	  (expand-file-name
	   (if fi:remote-lisp-track-image-name-directory
	       (let ((dir (file-name-directory image-name)))
		 (if (= ?/ (aref dir 0))
		     dir
		   (format "~/%s" dir)))
	     "~/")))
	 (proc (fi::make-subprocess
		buffer-number
		"common-lisp" 
		'fi:inferior-common-lisp-mode
		fi:common-lisp-prompt-pattern
		"rsh"
		(append
		 (list host
		       (format fi::remote-lisp-sh-prefix
			       (screen-width)
			       (or fi:default-remote-common-lisp-directory
				   remote-dir))
		       (if (and fi:remote-lisp-track-image-name-directory
				(equal
				 fi:default-remote-common-lisp-directory
				 fi:remote-lisp-track-image-name-directory))
			   (file-name-nondirectory image-name)
			 image-name))
		 image-arguments
		 '("'"))
		nil
		fi:start-lisp-interface-function
		(list 'lambda ()
		      '(setq fi::lisp-is-remote t)
		      (list 'cd (or fi:default-remote-common-lisp-directory
				    remote-dir))
		      (list 'fi::set-buffer-host '(current-buffer)
			    host)))))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:tcp-common-lisp (&optional buffer-number)
  "In a buffer whose name is determined from the optional prefix argument
BUFFER-NAME, connect to a Common Lisp using either a UNIX domain socket
file or internet port number.  Common Lisp buffer names start with
`*common-lisp' and end with `*', with an optional `-N' in between.  If
BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER is >= 0,then
the buffer is named `*common-lisp*<BUFFER-NUMBER>'.  If BUFFER-NUMBER is <
0, then the first available buffer name is chosen.

See `fi:unix-domain' and `fi:explicit-tcp-common-lisp'."
  (interactive "p")
  (if (null fi::freshest-common-sublisp-name)
      (error "must start a Lisp subprocess first"))
  (let ((proc (fi::make-tcp-connection
	       buffer-number "tcp-common-lisp" 'fi:tcp-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       (fi::get-buffer-host fi::freshest-common-sublisp-name)
	       (fi::get-buffer-port fi::freshest-common-sublisp-name)
	       (fi::get-buffer-password fi::freshest-common-sublisp-name))))
    proc))

(defun fi:explicit-tcp-common-lisp (&optional buffer-number
					      host service password)
  "The same as fi:tcp-common-lisp, except the buffer name of the Lisp onto
which the listener is disired is read from the minibuffer.  Use port 0 if
fi:unix-domain is non-nil."
  (interactive
   (let ((buffer (read-buffer "Buffer with Lisp subprocess: ")))
     (if fi:unix-domain
	 (list current-prefix-arg)
       (list current-prefix-arg
	     (fi::get-buffer-host buffer)
	     (fi::get-buffer-port buffer)
	     (fi::get-buffer-password buffer)))))
  (let ((proc (fi::make-tcp-connection
	       buffer-number "tcp-common-lisp" 'fi:tcp-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       host service password)))
    proc))

(defun fi:franz-lisp (&optional buffer-number)
  "Start a Franz Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER.  Franz Lisp buffer names
start with `*franz-lisp' and end with `*', with an optional `-N' in
between.  If BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER
is >= 0, then the buffer is named `*franz-lisp*<BUFFER-NUMBER>'.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen.

The image file and image arguments are taken from the variables
`fi:franz-lisp-image-name' and `fi:franz-lisp-image-arguments'.

See fi:explicit-franz-lisp."
  (interactive "p")
  (let ((proc (fi::make-subprocess
	       buffer-number "franz-lisp" 
	       'fi:inferior-franz-lisp-mode
	       fi:franz-lisp-prompt-pattern
	       fi:franz-lisp-image-name
	       fi:franz-lisp-image-arguments)))
    (setq fi::freshest-franz-sublisp-name (process-name proc))
    proc))

(defun fi:explicit-franz-lisp (&optional buffer-number
					 image-name image-arguments)
  "The same as fi:franz-lisp, except that the image and image arguments
are read from the minibuffer."
  (interactive
   (list 
    current-prefix-arg
    (expand-file-name (read-file-name "Image name: " nil nil t))
    (fi::listify-string
     (read-from-minibuffer "Image arguments (separate by spaces): "))))
  (let ((proc (fi::make-subprocess
	       buffer-number "franz-lisp" 
	       'fi:inferior-franz-lisp-mode
	       fi:franz-lisp-prompt-pattern
	       image-name image-arguments)))
    (setq fi::freshest-franz-sublisp-name (process-name proc))
    proc))

;;;;
;;; Internal functions
;;;;

(defun fi::make-subprocess (buffer-number process-name mode-function
			    image-prompt image-file image-arguments
			    &optional filter
				      initial-func
				      mode-hook)
  (let* ((buffer (fi::make-process-buffer process-name buffer-number))
	 (default-dir default-directory)
	 (buffer-name (buffer-name buffer))
	 (process (get-buffer-process buffer))
	 (status (if process (process-status process)))
	 (runningp (memq status '(run stop)))
	 start-up-feed-name)

    (if (not runningp)
	(progn				; hack image-file
	  (if (consp image-file)
	      (if (not (stringp (setq image-file (funcall image-file))))
		  (error "image-file function didn't return a string"))
	    (if (not (stringp image-file))
		(error "image-file not a string or cons: %s" image-file))
	    (setq image-file (substitute-in-file-name image-file)))
	  (if (= ?~ (aref image-file 0))
	      (setq image-file (expand-file-name image-file)))))
    
    (funcall fi:display-buffer-function buffer)
    
    (if runningp
	(goto-char (point-max))
      (setq default-directory default-dir)
      (if process (delete-process process))
      (setq process
	(apply 'start-process
	       (append (list buffer-name buffer
			     (concat exec-directory "env")
			     (format "TERMCAP=emacs:co#%d:tc=unknown:"
				     (screen-width))
			     "TERM=emacs"
			     "EMACS=t"
			     "-" image-file)
		       image-arguments)))
      (set-process-sentinel process 'fi::subprocess-sentinel)
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
	 (fi::send-string-split process start-up-feed-name
				fi:subprocess-map-nl-to-cr)))
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (condition-case ()
	  (let ((saved-input-ring fi::input-ring)
		(saved-input-ring-yank-pointer fi::input-ring-yank-pointer))
	    (funcall mode-function mode-hook)
	    (setq fi::input-ring saved-input-ring)
	    (setq fi::input-ring-yank-pointer saved-input-ring-yank-pointer))
	(error nil))
      (make-local-variable 'subprocess-prompt-pattern)
      (setq subprocess-prompt-pattern image-prompt)
      (fi::make-subprocess-variables)
      (if initial-func (funcall initial-func process)))
    process))

(defun fi::make-tcp-connection (buffer-number buffer-name mode image-prompt
				    &optional given-host
					      given-service
					      password)
  (if (and fi:unix-domain
	   (consp fi:unix-domain-socket)
	   (eq 'lambda (car fi:unix-domain-socket)))
      (setq fi:unix-domain-socket (funcall fi:unix-domain-socket)))
  
  (let* ((buffer (fi::make-process-buffer buffer-name buffer-number))
	 (default-dir default-directory)
	 (buffer-name (buffer-name buffer))
	 (host (if given-host
		   (if fi:unix-domain
		       (expand-file-name given-host)
		     given-host)
		 (if fi:unix-domain
		     (expand-file-name fi:unix-domain-socket)
		   (or fi:inet-host-name
		       (error "no default host--backdoor not started")))))
	 (service (or given-service
		      (error "not implemented yet")))
	 proc status)
    
    (funcall fi:display-buffer-function buffer)
    
    (setq proc (get-buffer-process buffer))
    (setq status (if proc (process-status proc)))
    (if (eq status 'run)
	(error
	 "can't start a TCP Common Lisp in a buffer which has a subprocess"))
    (if (eq status 'open)
	(goto-char (point-max))
      (setq default-directory default-dir)
      (setq proc (open-network-stream buffer-name buffer host service))
      ;;
      ;; The first input the new (Common Lisp) process is sent is the name
      ;; of the process.  This is so that the processes are named similarly
      ;; in Emacs and Lisp.
      ;;
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

(defun fi::make-process-buffer (name number)
  (let ((buffer-name
	 (cond
	   ((not (numberp number))
	    (concat "*" name "*"))
	   ((> number 1)
	    ;; just return the buffer name
	    (concat "*" name "*<" number ">"))
	   ((< number 0)
	    ;; search for the first available buffer
	    (let (buffer-name n)
	      (if (not (fi::process-running
			(setq buffer-name (concat "*" name "*"))))
		  buffer-name
		(setq n 2)
		(while (fi::process-running (setq buffer-name
					      (concat "*" name "*<" n ">")))
		  (setq n (+ n 1)))
		buffer-name)))
	   (t (concat "*" name "*")))))
    (or (get-buffer buffer-name)
	(get-buffer-create buffer-name))))

(defun fi::start-tcp-lisp-interface (process)
  (send-string
   process
   (if fi:unix-domain
       "(progn(princ \";Starting socket daemon
   \")(require :ipc)(require :emacs)(set (find-symbol (symbol-name :*unix-domain*) :ipc) t)
   (funcall (find-symbol (symbol-name :start-lisp-listener-daemon) :ipc))(values))\n"
     "(progn(princ \";Starting socket interface
   \")(require :ipc)(require :emacs)
   (funcall (find-symbol (symbol-name :start-lisp-listener-daemon) :ipc))(values))\n")))

(defun fi::make-subprocess-variables ()
  (setq fi::input-ring-max fi:default-input-ring-max)
  (setq fi::shell-directory-stack nil)
  (setq fi::last-input-search-string "")
  (setq fi::last-input-start (make-marker))
  (setq fi::last-input-end (make-marker)))

(defun fi::send-region-split (process start-position end-position
				      &optional nl-cr)
  "Send region to process in small pieces."
  (interactive "sSend region in pieces (to process): \nr")
  (let* ((start (if (markerp start-position)
		    (marker-position start-position)
		  start-position))
	 (end (if (markerp end-position)
		  (marker-position end-position)
		end-position))
	 (string (buffer-substring start end)))
    (fi::send-string-split process string nl-cr)))

(defun fi::send-string-split (process string &optional nl-cr)
  "Send string to process in small pieces using send-string."
  (interactive "sSend (to process): \nsSend to process in pieces (string): ")
  (let ((size (length string))
	(filtered-string
	 (if nl-cr
	     (fi::substitute-chars-in-string '((?\n . ?\r)) string)
	   string))
	(start 0))
    (while (and (> size 0)
		(condition-case nil
		    (progn
		      (send-string
		       process
		       (substring filtered-string
				  start
				  (+ start
				     (min size
					  fi:subprocess-write-quantum))))
		      t)
		  (error
		   (message "Error writing to subprocess.")
		   nil)))
      (setq size (- size fi:subprocess-write-quantum))
      (setq start (+ start fi:subprocess-write-quantum)))))

;;; Sentinel and filter for subprocesses.  The sentinel is currently
;;;   not used.
(defun fi::subprocess-sentinel (process status)
  t)

(defun fi::subprocess-filter (process output &optional stay cruft)
  "Filter output from processes tied to buffers.
This function implements continuous output to visible buffers."
  (let ((inhibit-quit t))
    (save-excursion
      (set-buffer (process-buffer process))
      (if (and (boundp 'fi::remote-host) fi::remote-host)
	  (setq output (fi::subprocess-control-a-frammis output))))
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

(defun fi::subprocess-control-a-frammis (string)
  (if (and (fi::fast-search-string 1 string)
	   (string-match "\\(.*\\)\\(.*\\)\\(.*\\)" string))
      (let* ((res (concat
		   (substring string (match-beginning 1) (match-end 1))
		   (substring string (match-beginning 3) (match-end 3))))
	     (command (substring string (match-beginning 2) (match-end 2)))
	     (xx nil))
	(setq fi::remote-port
	  (car (setq xx (read-from-string command (cdr xx)))))
	(setq fi::remote-password
	  (car (setq xx (read-from-string command (cdr xx)))))
	(setq fi::lisp-case-mode
	  (car (setq xx (read-from-string command (cdr xx)))))
	res)
    string))

(defun fi::subprocess-watch-for-special-commands ()
  "Watch for special commands like, for example, `cd' in a shell."
  (if (null fi::shell-directory-stack)
      (setq fi::shell-directory-stack (list default-directory)))
  (condition-case ()
      ;; "To err is really not nice." -dkl 11/21/88
      (save-excursion
	(goto-char fi::last-input-start)
	(cond
	  ((and fi::cl-package-regexp (looking-at fi::cl-package-regexp))
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

(defun fi::set-buffer-host (buffer host)
  "Set the variable fi::remote-host in buffer BUFFER to be HOST."
  (save-excursion
    (set-buffer buffer)
    (setq fi::remote-host host)))

(defun fi::get-buffer-host (buffer)
  "Given BUFFER return the value in this buffer of fi::remote-host."
  (save-excursion
    (set-buffer buffer)
    fi::remote-host))

(defun fi::get-buffer-port (buffer)
  "Given BUFFER return the value in this buffer of fi::remote-port."
  (save-excursion
    (set-buffer buffer)
    fi::remote-port))

(defun fi::get-buffer-password (buffer)
  "Given BUFFER returns the values in this buffer of fi::remote-password"
  (save-excursion
    (set-buffer buffer)
    fi::remote-password))
