;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
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
;; This file may be distributed without further permission from
;; Franz Inc. as long as
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this package, please feel
;; free to contact Franz Inc. at
;;
;;	Franz Inc.
;;	Attn: Emacs Group Manager
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.31 1988/05/12 23:08:49 layer Exp $

;; Low-level subprocess mode guts

;;;;
;;; Variables and Constants
;;;;

(defvar fi:common-lisp-image-name "cl"
  "Default Common Lisp image to invoke from (fi:common-lisp).")

(defvar fi:common-lisp-image-arguments nil
  "Default Common Lisp image arguments when invoked from (fi:common-lisp).
Should be a list of strings.")

(defvar fi:common-lisp-prompt-pattern
  "^\\(\\[[0-9]+c?\\] \\|\\[step\\] \\)?<[-A-Za-z]* ?[0-9]*?> "
  "Regexp for Newline command in inferior-common-lisp mode to match Common
Lisp prompts. Anything from beginning of line up to the end of what this
pattern matches is deemed to be a prompt.")


(defvar fi:franz-lisp-image-name "lisp"
  "Default Franz Lisp image to invoke from (fi:franz-lisp).")

(defvar fi:franz-lisp-image-arguments nil
  "Default Franz Lisp image arguments when invoked from (fi:franz-lisp).
Should be a list of strings.")

(defvar fi:franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "Regexp used by Newline command in inferior-franz-lisp mode to match
Franz Lisp prompts. Anything from beginning of line up to the end of what
this pattern matches is deemed to be prompt, and is not re-executed.")

(defvar fi:shell-popd-regexp ":?popd"
  "Regexp to match subshell commands equivalent to popd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar fi:shell-pushd-regexp ":?pushd"
  "Regexp to match subshell commands equivalent to pushd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar fi:shell-cd-regexp ":?cd"
  "Regexp to match subshell commands equivalent to cd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar fi:subprocess-map-nl-to-cr nil
  "If t, map NL (newline) to CR (carriage-return).  This is a buffer-local
symbol.")

(defvar fi:subprocess-continuously-show-output-in-visible-buffer t
  "If t, output from a subprocess to a visible buffer is continuously
shown.  If a subprocess buffer is visible and the window point is beyond
the process output marker, output to that buffer from its associated
process will be continuously visible.  If the window point is before the
process output marker, the window is not updated.  This is a buffer-local
symbol.")

(defvar fi:subprocess-write-quantum 120
  "Maximum size in bytes of a single write request to a subprocess.")

(defvar fi:subprocess-enable-superkeys nil
  "If t, certain keys become `superkeys' in subprocess buffers--this should
be set before starting any subprocesses.  The superkeys are C-a, C-d, C-o,
C-u, C-w, C-z, and C-\\, which will behave as they would in the current
local keymap when typed at the end of a subprocess buffer.  If typed
elsewhere, these keys have their normal global binding.  This is a
buffer-local symbol.  This variable should be set before starting-up the
first subprocess.  Use setq-default to set the default value for this
symbol.")

;;;;;;;;;;;;;;;;;;;;;; internal vars

(defvar fi::last-input-start nil
  "Marker for start of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")

(defvar fi::last-input-end nil
  "Marker for end of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")

(defvar fi::sublisp-name nil
  "Name of inferior lisp process.")

(defvar fi::freshest-franz-sublisp-name nil
  "Name of franz lisp subprocess most recently invoked.")

(defvar fi::freshest-common-sublisp-name nil
  "Name of common lisp subprocess most recently invoked.")

(defvar fi::shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")

;;;;
;;; User visible functions
;;;;

(defun fi:common-lisp (&optional buffer-number)
  "Start a Common Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER.  Common Lisp buffer names
start with `*common-lisp' and end with `*', with an optional `-N' in
between.  If BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER
is >= 0, then the buffer is named `*common-lisp-<BUFFER-NUMBER>*'.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen.

The image file and image arguments are taken from the variables
`fi:common-lisp-image-name' and `fi:common-lisp-image-arguments'.

See fi:explicit-common-lisp."
  (interactive "p")
  (let ((proc (fi::make-subprocess
	       buffer-number "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       fi:common-lisp-image-name
	       fi:common-lisp-image-arguments)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:explicit-common-lisp (&optional buffer-number
					  image-name image-arguments)
  "The same as fi:common-lisp, except that the image and image arguments
are read from the minibuffer."
  (interactive "p\nsImage name: \nxImage arguments (a list): ")
  (let ((proc (fi::make-subprocess
	       buffer-number "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       image-name image-arguments)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:remote-common-lisp (&optional buffer-number host)
  "Start a Common Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER, where the Common Lisp
image is run on another machine.  Common Lisp buffer names start with
`*common-lisp' and end with `*', with an optional `-N' in between.  If
BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER is >= 0,
then the buffer is named `*common-lisp-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER
is < 0, then the first available buffer name is chosen.

The host on which the image is run is read from the minibuffer.

The image file and image arguments are taken from the variables
`fi:common-lisp-image-name' and `fi:common-lisp-image-arguments'.

See fi:explicit-remote-common-lisp."
  (interactive "p\nsRemote host name: ")
  (let ((proc (fi::make-subprocess
	       buffer-number "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       "rsh"
	       (append (list host fi:common-lisp-image-name)
		       fi:common-lisp-image-arguments))))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:explicit-remote-common-lisp (&optional buffer-number host
						 image-name image-arguments)
  "The same as fi:remote-common-lisp, except that the image and image
arguments are read from the minibuffer."
  (interactive
   "p\nsRemote host name: \nsImage name: \nxImage arguments (a list): ")
  (let ((proc (fi::make-subprocess
	       buffer-number "common-lisp" 
	       'fi:inferior-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       "rsh"
	       (append (list host image-name))
	       image-arguments)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:tcp-common-lisp (&optional buffer-number)
  "In a buffer whose name is determined from the optional prefix argument
BUFFER-NAME, connect to a Common Lisp using either a UNIX domain socket
file or internet port number.  Common Lisp buffer names start with
`*common-lisp' and end with `*', with an optional `-N' in between.  If
BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER is >= 0,then
the buffer is named `*common-lisp-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is <
0, then the first available buffer name is chosen.

See `fi:unix-domain' and `fi:explicit-tcp-common-lisp'."
  (interactive "p")
  (let ((proc (fi::make-tcp-connection
	       buffer-number "tcp-common-lisp" 'fi:tcp-common-lisp-mode
	       fi:common-lisp-prompt-pattern)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:explicit-tcp-common-lisp (&optional buffer-number host service)
  "The same as fi:tcp-common-lisp, except that the host name a port number
are read from the minibuffer.  Use a port number of 0 for UNIX domain
sockets."
  (interactive
   "p\nsHost name: \nnService port number (0 for UNIX domain): ")
  (let ((proc (fi::make-tcp-connection
	       buffer-number "tcp-common-lisp" 'fi:tcp-common-lisp-mode
	       fi:common-lisp-prompt-pattern
	       host service)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:franz-lisp (&optional buffer-number)
  "Start a Franz Lisp subprocess in a buffer whose name is determined
from the optional prefix argument BUFFER-NUMBER.  Franz Lisp buffer names
start with `*franz-lisp' and end with `*', with an optional `-N' in
between.  If BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER
is >= 0, then the buffer is named `*franz-lisp-<BUFFER-NUMBER>*'.  If
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
  (interactive "p\nsImage name: \nxImage arguments (a list): ")
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
					  image-prompt image-file
					  image-arguments)
  (let* ((buffer (fi::make-process-buffer process-name buffer-number))
	 (buffer-name (buffer-name buffer))
	 start-up-feed-name process status)
    (switch-to-buffer buffer)
    (setq process (get-buffer-process buffer))
    (setq status (if process (process-status process)))
    (if (memq status '(run stop))
	(goto-char (point-max))
      (if (not (stringp image-file))
	  (setq image-file (funcall image-file)))
      (if process (delete-process process))
      (setq process (apply 'start-process
			   (append (list buffer-name buffer image-file)
				   image-arguments)))
      (set-process-sentinel process 'fi::subprocess-sentinel)
      (set-process-filter process 'fi::subprocess-filter)
      (setq start-up-feed-name
	(if image-file
	    (concat "~/.emacs_" (file-name-nondirectory image-file))))
      (cond
	((and start-up-feed-name (file-exists-p start-up-feed-name))
	 ;; This is guaranteed to wait long enough
	 ;; but has bad results if the shell or Lisp does not prompt at all
	 ;;	     (while (= size (buffer-size))
	 ;;	       (sleep-for 1))
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
      (funcall mode-function)
      (make-local-variable 'subprocess-prompt-pattern)
      (setq subprocess-prompt-pattern image-prompt)
      (fi::make-subprocess-variables))
    process))

(defun fi::make-tcp-connection (buffer-number buffer-name mode image-prompt
				    &optional given-host
					      given-service)
  (let* ((buffer (fi::make-process-buffer buffer-name buffer-number))
	 (buffer-name (buffer-name buffer))
	 (host (if given-host
		   (expand-file-name given-host)
		 (if fi:unix-domain
		     (expand-file-name fi:unix-domain-socket)
		   fi:local-host-name)))
	 (service (if given-service
		      given-service
		    (if fi:unix-domain 0 fi:excl-service-name)))
	 proc status)
    (switch-to-buffer buffer)
    (setq proc (get-buffer-process buffer))
    (setq status (if proc (process-status proc)))
    (if (eq status 'run)
	(error
	 "can't start a TCP Common Lisp in a buffer which has a subprocess"))
    (if (eq status 'open)
	(goto-char (point-max))
      (setq proc (open-network-stream buffer-name buffer host service))
      ;;
      ;; HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
      ;; The first input the new (Common Lisp) process is sent is the name
      ;; of the process.  This is so that the processes are named similarly
      ;; in Emacs and Lisp.
      ;;
      (process-send-string proc (format "\"%s\"\n" (buffer-name buffer)))

      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (funcall mode)
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
	    (concat "*" name "-" number "*"))
	   ((< number 0)
	    ;; search for the first available buffer
	    (let (buffer-name n)
	      (if (not (fi::process-running
			(setq buffer-name (concat "*" name "*"))))
		  buffer-name
		(setq n 2)
		(while (fi::process-running (setq buffer-name
					      (concat "*" name "-" n "*")))
		  (setq n (+ n 1)))
		buffer-name)))
	   (t (concat "*" name "*")))))
    (or (get-buffer buffer-name)
	(get-buffer-create buffer-name))))

(defun fi::make-subprocess-variables ()
  (if (null fi::input-ring)
      (progn
	(setq fi::input-ring-max fi:default-input-ring-max)
	(setq fi::input-ring-yank-pointer nil)
	(setq fi::shell-directory-stack nil)
	(setq fi::last-input-search-string "")))
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
	 (string (buffer-substring start end))
	 (size (- end start)))
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

(defun fi::subprocess-filter (process output &optional stay)
  "Filter output from processes tied to buffers.
This function implements continuous output to visible buffers."
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
	      t;; Still there.
	    (set-window-point window-of-buffer old-point)))
      (goto-char new-point))
    (cond
      (in-buffer nil)
      (stay old-buffer)
      (t (set-buffer old-buffer)))))

(defun fi::subprocess-hack-directory ()
  ;; Even if we get an error trying to hack the working directory,
  ;; still send the input to the subshell.
  (condition-case ()
      (save-excursion
	(goto-char fi::last-input-start)
	(cond
	  ((and (and fi:shell-popd-regexp
		     (looking-at fi:shell-popd-regexp))
		(memq (char-after (match-end 0)) '(?\; ?\n)))
	   (if fi::shell-directory-stack
	       (progn
		 (cd (car fi::shell-directory-stack))
		 (setq fi::shell-directory-stack
		   (cdr fi::shell-directory-stack)))))
	  ((and fi:shell-pushd-regexp
		(looking-at fi:shell-pushd-regexp))
	   (cond
	     ((memq (char-after (match-end 0)) '(?\; ?\n))
	      (if fi::shell-directory-stack
		  (let ((old default-directory))
		    (cd (car fi::shell-directory-stack))
		    (setq fi::shell-directory-stack
		      (cons old (cdr fi::shell-directory-stack))))))
	     ((memq (char-after (match-end 0)) '(?\  ?\t))
	      (let (dir)
		(skip-chars-forward "^ ")
		(skip-chars-forward " \t")
		(if (file-directory-p
		     (setq dir
		       (expand-file-name
			(substitute-in-file-name
			 (buffer-substring
			  (point)
			  (progn
			    (skip-chars-forward "^\n \t;")
			    (point)))))))
		    (progn
		      (setq fi::shell-directory-stack
			(cons default-directory fi::shell-directory-stack))
		      (cd dir)))))))
	  ((and fi:shell-cd-regexp
		(looking-at fi:shell-cd-regexp))
	   (cond
	     ((memq (char-after (match-end 0)) '(?\; ?\n))
	      (cd (getenv "HOME")))
	     ((memq (char-after (match-end 0)) '(?\  ?\t))
	      (let (dir)
		(skip-chars-forward "^ ")
		(skip-chars-forward " \t")
		(let ((xdir (buffer-substring
			     (point)
			     (progn (skip-chars-forward "^\n \t;")
				    (point)))))
		  (if (equal "" xdir) (setq xdir "~"))
		  (if (file-directory-p
		       (setq dir
			 (expand-file-name (substitute-in-file-name xdir))))
		      (cd dir)))))))))
    (error nil)))

;;;;
;;; Initializations
;;;;

(mapcar 'make-variable-buffer-local
	'(fi:shell-popd-regexp
	  fi:shell-pushd-regexp 
	  fi:shell-cd-regexp
	  fi:package
	  fi:subprocess-map-nl-to-cr
	  fi:subprocess-continuously-show-output-in-visible-buffer
	  fi:subprocess-enable-superkeys
	  fi:subprocess-super-key-map

	  fi::shell-directory-stack
	  fi::last-input-start
	  fi::last-input-end
	  fi::input-ring
	  fi::input-ring-max
	  fi::input-ring-yank-pointer
	  fi::last-input-search-string))
