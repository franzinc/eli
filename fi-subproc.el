;;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.21 1988/04/18 21:08:04 layer Exp $
;;;
;;; Low-level subprocess mode guts

;;;;
;;; Variables and Constants
;;;;

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
  "If t, map NL (newline) to CR (carriage-return) in input to fi::make-process.
This is a buffer-local symbol.")

(defvar fi:subprocess-continuously-show-output-in-visible-buffer t
  "If t, output from a subprocess to a visible buffer is continuously
shown.  If a subprocess buffer is visible and the window point is beyond
the process output marker, output to that buffer from its associated
process will be continuously visible.  If the window point is before the
process output marker, the window is not updated.  This is a buffer-local
symbol.")

(defvar fi:subprocess-enable-superkeys nil
  "If t, certain keys become `superkeys' in subprocess buffers.
The superkeys are C-a, C-d, C-o, C-u, C-w, C-z, and C-\\, which will behave
as they would in the current local keymap when typed at the end
of a subprocess buffer.  If typed elsewhere, these keys have their
normal global binding.  This is a buffer-local symbol.")

(defvar fi:explicit-lisp-file-name nil
  "Explicit Lisp image to invoke from (fi:lisp).")
(defvar fi:explicit-franz-lisp-file-name nil
  "Explicit Franz Lisp image to invoke from (fi:franz-lisp).")
(defvar fi:explicit-common-lisp-file-name nil
  "Explicit Common Lisp image to invoke from (fi:common-lisp).")
(defvar fi:explicit-lisp-image-arguments nil
  "Explicit Lisp image arguments when invoked from (fi:lisp).")
(defvar fi:explicit-franz-lisp-image-arguments nil
  "Explicit Franz Lisp image arguments when invoked from (fi:franz-lisp).")
(defvar fi:explicit-common-lisp-image-arguments nil
  "Explicit Common Lisp image arguments when invoked from (fi:common-lisp).")

(defvar fi:default-lisp-file-name "lisp"
  "Default Lisp image to invoke from (fi:lisp).")
(defvar fi:default-franz-lisp-file-name "lisp"
  "Default Franz Lisp image to invoke from (fi:franz-lisp).")
(defvar fi:default-common-lisp-file-name "cl"
  "Default Common Lisp image to invoke from (fi:common-lisp).")
(defvar fi:default-lisp-image-arguments nil
  "Default Lisp image arguments when invoked from (fi:lisp).")
(defvar fi:default-franz-lisp-image-arguments nil
  "Default Franz Lisp image arguments when invoked from (fi:franz-lisp).")
(defvar fi:default-common-lisp-image-arguments nil
  "Default Common Lisp image arguments when invoked from (fi:common-lisp).")

(defvar fi:subprocess-write-quantum 120
  "Maximum size in bytes of a single write request to a subprocess.")

(defvar fi:common-lisp-prompt-pattern
  "^\\(\\[[0-9]+c?\\] \\|\\[step\\] \\)?<[-A-Za-z]* ?[0-9]*?> "
  "Regexp for Newline command in inferior-lisp mode to match Common Lisp
prompts. Anything from beginning of line up to the end of what this pattern
matches is deemed to be prompt, and is not re-executed.")

(defvar fi:franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "Regexp used by Newline command in inferior-lisp mode to match Franz
Lisp prompts. Anything from beginning of line up to the end of what this
pattern matches is deemed to be prompt, and is not re-executed.")

(defvar fi:lisp-prompt-pattern fi:common-lisp-prompt-pattern
  "Regexp used by Newline command in inferior-lisp mode to match Lisp
prompts. Anything from beginning of line up to the end of what this pattern
matches is deemed to be prompt, and is not re-executed.")

;;;;
;;; User visible functions
;;;;

(defun fi:lisp ()
  "Run an inferior lisp, with input/output through buffer *lisp*.
See `fi::make-process'."
  (interactive)
  (fi::make-process "lisp" "lisp" 'fi:inferior-lisp-mode))

(defun fi:another-lisp ()
  "Run a new inferior lisp, with input/output through buffer *lisp-N*.
This function always creates a new subprocess and buffer.  See
`fi::make-process'."
  (interactive)
  (fi::make-process "lisp" "lisp" 'fi:inferior-lisp-mode nil t))

(defun fi:common-lisp (&optional tcp-lisp)
  "With no prefix arg run a Common Lisp :subprocess with input/output
through buffer *common-lisp*, otherwise try and connect to a Lisp Listener
daemon (via a unix or internet domain socket, see `open-network-stream').
Returns the name of the started subprocess."
  (interactive "P")
  (let ((proc (fi::make-process
	       "common-lisp" "common-lisp"
	       (if tcp-lisp
		   'fi:tcp-lisp-mode
		 'fi:inferior-common-lisp-mode)
	       nil nil nil tcp-lisp)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:another-common-lisp (&optional tcp-lisp)
  "Run a new Common Lisp subprocess, with i/o through buffer
*common-lisp-N*.  Returns the name of the started subprocess.  This
function always creates a new subprocess and buffer.  See
`fi::make-process'."
  (interactive "P")
  (let ((proc (fi::make-process
	       "common-lisp" "common-lisp"
	       (if tcp-lisp
		   'fi:tcp-lisp-mode
		 'fi:inferior-common-lisp-mode)
	       nil t
				nil tcp-lisp)))
    (setq fi::freshest-common-sublisp-name (process-name proc))
    proc))

(defun fi:franz-lisp ()
  "Run a Franz Lisp subprocess, with input/output through buffer
*franz-lisp*.   Returns the name of the started subprocess.  See
`fi::make-process'."
  (interactive)
  (let ((proc (fi::make-process "franz-lisp" "franz-lisp"
				'fi:inferior-franz-lisp-mode)))
    (setq fi::freshest-franz-sublisp-name (process-name proc))
    proc))

(defun fi:another-franz-lisp ()
  "Run a new Franz Lisp subprocess, with i/o through buffer *franz-lisp-N*.
Returns the name of the started subprocess.  This function always creates a
new subprocess and buffer.  See `fi::make-process'."
  (interactive)
  (let ((proc (fi::make-process "franz-lisp" "franz-lisp"
				'fi:inferior-franz-lisp-mode nil t)))
    (setq fi::freshest-franz-sublisp-name (process-name proc))
    proc))

;;;;
;;; Interactively called functions (from keymaps)
;;;;

(defun fi:subprocess-superkey (&optional special-binding)
  "This function implements superkeys in subprocess buffers.
A superkey is treated specially when at the end of a subprocess buffer,
but has its normal, global, binding when used elsewhere in the buffer.
At the end of the buffer the key has SPECIAL-BINDING.  If SPECIAL-BINDING
is not given, the key takes its binding from the
fi:subprocess-super-key-map keymap."
  (interactive)
  (if (eobp)
      (if special-binding
	  (call-interactively special-binding)
	(fi::subprocess-reprocess-keys fi:subprocess-super-key-map))
    (fi::subprocess-reprocess-keys global-map)))

(defun fi::subprocess-reprocess-keys (&optional map key)
  "Reprocess KEY or the last key sequence (which may be incomplete) in MAP.
This is used to reprocess a key sequence as if it were seen in another
context, e.g. to process global bindings of keys from a subprocess
buffer (in fi:shell-mode or fi:inferior-lisp-mode) when some keys are hit
other than at the end of the buffer."
  (if (null map) (setq map global-map))
  (let* ((last-key (if key
		       (if (integerp key)
			   (char-to-string key)
			 key)
		     (this-command-keys)))
	 (last-binding (lookup-key map last-key)))
    (while (keymapp last-binding)
      (setq last-binding
	(lookup-key last-binding
		    (setq last-key (char-to-string (read-char))))))
    (if (commandp last-binding)
	(call-interactively last-binding)
      (ding))))

(defun fi:subprocess-beginning-of-line (arg)
  "Move to beginning of line, skipping over initial prompt.
Moves point to beginning of line, just like (beginning-of-line),
except that if the pattern at the beginning of the line matches the
current subprocess prompt pattern, this function skips over it."
  (interactive "P")
  (beginning-of-line arg)
  (if (looking-at subprocess-prompt-pattern)
      (re-search-forward subprocess-prompt-pattern nil t)))

(defun fi:subprocess-backward-kill-word (words)
  "Kill previous word(s) in current subprocess input line.  This function
takes care not to delete past most recent subprocess output."
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (marker-position (process-mark (get-buffer-process (current-buffer))))
     (point))
    (backward-kill-word words)))

(defun fi:subprocess-send-input ()
  "Send input to subshell.  At end of buffer, sends all text after last
output as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of subprocess-prompt-pattern if
possible.  This regexp should start with \"^\"."
  (interactive)
  (if fi::shell-completions-window (fi::shell-completion-cleanup))
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker fi::last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(move-marker fi::last-input-end (point)))
    (let ((max (point)))
      (beginning-of-line)
      (re-search-forward subprocess-prompt-pattern max t))
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker fi::last-input-start (point))
      (insert copy)
      (move-marker fi::last-input-end (point))))
  (fi::subprocess-hack-directory)
  (let ((process (get-buffer-process (current-buffer))))
    (fi::send-region-split process fi::last-input-start fi::last-input-end
			   fi:subprocess-map-nl-to-cr)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:subprocess-send-eof ()
  "Send eof to subshell (or to the program running under it)."
  (interactive)
  (process-send-eof))

(defun fi:subprocess-kill-output ()
  "Kill all output from shell since last input."
  (interactive)
  (goto-char (point-max))
  (kill-region fi::last-input-end (point))
  (insert "[output flushed]\n"))

(defun fi:subprocess-send-flush ()
  "Send `flush output' character (^O) to subprocess."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:subprocess-show-output ()
  "Display start of this batch of shell output at top of window.
Also put cursor there."
  (interactive)
  (set-window-start (selected-window) fi::last-input-end)
  (goto-char fi::last-input-end))

(defun fi:subprocess-interrupt ()
  "Interrupt this shell's current subjob."
  (interactive)
  (interrupt-process nil t))

(defun fi:subprocess-kill ()
  "Send a `kill' signal to the subprocess in the current buffer."
  (interactive)
  (kill-process nil t))

(defun fi:subprocess-quit ()
  "Send quit signal to this shell's current subjob."
  (interactive)
  (quit-process nil t))

(defun fi:subprocess-suspend ()
  "Stop this shell's current subjob."
  (interactive)
  (stop-process nil t))

(defun fi:subprocess-kill-input ()
  "Kill all text since last stuff output by the shell or its subjobs."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

;;;;
;;; The Guts (the lowest of the low (level))
;;;;

(defmacro fi::shell-variable-bound-value (&rest name-components)
  "Macro to compose a symbol name and return its value if bound."
  (list 'let
	(list (list 'name-of-symbol
		    (append (list 'funcall ''concat) name-components)))
	'(and
	  (boundp (intern name-of-symbol))
	  (symbol-value (intern name-of-symbol)))))

(defun fi::make-process (image name mode
			       &optional number another arguments tcp)
  "Spawn a subprocess with input/output through an Emacs buffer.
The major-mode is selected by calling MODE with one argument, the prompt.
Process NAME or NAME-NUMBER is created.  Returns the name of the
subprocess buffer without the asterisks.  If the associated buffer
\"*NAME*\" or \"*NAME-NUMBER*\" exists but the subprocess is not
running, a new subprocess is started in that buffer.  If the optional
ANOTHER argument is present, a new buffer and subprocess are always
created.  The image invoked is taken from value of the Emacs symbol
`explicit-<NAME>-file-name', where <NAME> is the miniscule process name, if
this symbol is defined and non-nil.  Otherwise, the value of the symbol
`explicit-<IMAGE>-file-name' is used, where <IMAGE> is the miniscule image
name, if this symbol is defined and is non-nil.  Otherwise, the image name
will be taken from the environment variables \"E<NAME>\", \"<NAME>\",
\"E<IMAGE>\", or \"<IMAGE>\" with embedded hyphens removed and converted
to upper case, e.g. \"franz-lisp\" becomes \"EFRANZLISP\".
If these variables are not found, the value of one of the Emacs symbols
`default-<NAME>-file-name' or `default-<IMAGE>-file-name' is used.  If the
image invoked is taken from the Emacs symbol `explicit-<NAME>-file-name' or
`explicit-<IMAGE>-file-name', arguments to the invoked image are taken from
the value (a list) of `explicit-<NAME>-image-arguments' or
`explicit-<IMAGE>-image-arguments' if it has a value.  Similarly, if the
image name is obtained from the Emacs symbol `default-<NAME>-file-name' or
`default-<IMAGE>-file-name', arguments to the invoked image are taken from
the value (a list) of `default-<NAME>-image-arguments' or
`default-<IMAGE>-image-arguments'.  In either case, the arguments specified
to this function in the ARGUMENTS parameter will be appended.
If a file `~/.emacs_<FILE>' exists, where <FILE> is the image being
invoked, that file is sent to the subprocess as initial input.

The subprocess prompt is set to the value of variable
`<NAME>-prompt-pattern' if found, otherwise `<IMAGE>-prompt-pattern'.

Also see `fi::make-shell' and `fi::make-another-shell'."
  ;;(interactive "sImage name: \nsProcess name: ")
  (let* ((majuscule-image (fi::remove-chars-from-string '(?-) (upcase image)))
	 (majuscule-name (fi::remove-chars-from-string '(?-) (upcase name)))
	 (image-file (or (fi::shell-variable-bound-value
			  "fi:explicit-" name "-file-name")
			 (fi::shell-variable-bound-value
			  "fi:explicit-" image "-file-name")
			 (getenv (concat "E" majuscule-name))
			 (getenv (concat "E" majuscule-image))
			 (getenv majuscule-name)
			 (getenv majuscule-image)
			 (fi::shell-variable-bound-value
			  "fi:default-" name "-file-name")
			 (fi::shell-variable-bound-value
			  "fi:default-" image "-file-name")))
	 (image-prompt (or (fi::shell-variable-bound-value
			    "fi:" name "-prompt-pattern")
			   (fi::shell-variable-bound-value
			    "fi:" image "-prompt-pattern")))
	 (image-arguments (cond
			    ((fi::shell-variable-bound-value
			      "fi:explicit-" name "-file-name")
			     (fi::shell-variable-bound-value
			      "fi:explicit-" name "-image-arguments"))
			    ((fi::shell-variable-bound-value
			      "fi:explicit-" image "-file-name")
			     (fi::shell-variable-bound-value
			      "fi:explicit-" image "-image-arguments"))
			    ((fi::shell-variable-bound-value
			      "fi:default-" name "-file-name")
			     (fi::shell-variable-bound-value
			      "fi:default-" name "-image-arguments"))
			    ((fi::shell-variable-bound-value
			      "fi:default-" image "-file-name")
			     (fi::shell-variable-bound-value
			      "fi:default-" image "-image-arguments"))
			    (t
			     nil)))
	 (start-up-feed-name (concat "~/.emacs_"
				     (file-name-nondirectory image-file))))
    (let ((res
	   (apply
	    (if tcp
		(if another
		    'fi::make-another-tcp-connection
		  'fi::make-tcp-connection)
	      (if another 'fi::make-another-shell 'fi::make-shell))
	    (append (list name image-file mode)
		    (if another (list number) nil)
		    (list
		     (if (file-exists-p start-up-feed-name)
			 start-up-feed-name))
		    (append image-arguments arguments)))))
      (make-local-variable 'subprocess-prompt-pattern)
      (setq subprocess-prompt-pattern image-prompt)
      (fi::make-subprocess-variables)
      res)))

(defun fi::make-shell (name program mode &optional startfile &rest arguments)
  "Create shell or lisp subprocess that does input/output through buffer.
Returns the name of the created subprocess without the asterisks."
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc
	(setq status (process-status proc)))
    (switch-to-buffer buffer)
    ;;(setq size (buffer-size))
    (if (memq status '(run stop))
	nil
      (if proc (delete-process proc))
      (setq proc (apply 'start-process
			(append (list name buffer program)
				arguments)))
      (set-process-sentinel proc 'fi::subprocess-sentinel)
      (set-process-filter proc 'fi::subprocess-filter)
      (cond
	(startfile
	 ;; This is guaranteed to wait long enough
	 ;; but has bad results if the shell or Lisp does not prompt at all
	 ;;	     (while (= size (buffer-size))
	 ;;	       (sleep-for 1))
	 ;; I hope 1 second is enough!
	 (sleep-for 1)
	 (goto-char (point-max))
	 (insert-file-contents startfile)
	 (setq startfile (buffer-substring (point) (point-max)))
	 (delete-region (point) (point-max))
	 (fi::send-string-split proc startfile fi:subprocess-map-nl-to-cr)))
      (setq name (process-name proc)))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (funcall mode)
    proc))

(defun fi::make-tcp-connection (name program mode
				     &optional startfile &rest arguments)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (switch-to-buffer buffer)
    (if (eq status 'open)
	nil
      (if fi:unix-domain
	  (setq proc (open-network-stream
		      (buffer-name buffer) buffer
		      (expand-file-name fi::unix-domain-socket)
		      0))
	(setq proc (open-network-stream (buffer-name buffer) buffer
					fi:local-host-name
					fi:excl-service-name)))
;;;;;;;;;;;;;;;;;;;;;;;; HACK HACK HACK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; first, send the name of the process.  this is a crock because
      ;; the process we are connecting to has to know this is coming over
      ;; the wire or else it will possibly be very confused
      (process-send-string proc (format "\"%s\"\n" (buffer-name buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (cond
	(startfile
	 ;; This is guaranteed to wait long enough
	 ;; but has bad results if the shell or Lisp does not prompt at all
	 ;;	     (while (= size (buffer-size))
	 ;;	       (sleep-for 1))
	 ;; I hope 1 second is enough!
	 (sleep-for 1)
	 (goto-char (point-max))
	 (insert-file-contents startfile)
	 (setq startfile (buffer-substring (point) (point-max)))
	 (delete-region (point) (point-max))
	 (fi::send-string-split proc startfile fi:subprocess-map-nl-to-cr)))
      (setq name (process-name proc)))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (funcall mode)
    proc))

(defun fi::make-another-shell (name program mode
				    &optional number startfile
				    &rest arguments)
  "Create another subprocess that does input/output through a buffer."
  (apply 'fi::make-shell
	 (append (list (fi::generate-new-buffer-name name number)
		       program mode startfile)
		 arguments)))

(defun fi::make-another-tcp-connection (name program mode
					     &optional number startfile
					     &rest arguments)
  (apply 'fi::make-tcp-connection
	 (append (list (fi::generate-new-buffer-name name number)
		       program mode startfile)
		 arguments)))

(defun fi::generate-new-buffer-name (name number)
  (let* ((separator "-")
	 (name-of-buffer (if number
			     (concat name separator number)
			   name))
	 (buffer (get-buffer (concat "*" name-of-buffer "*"))))
    (cond
      ((null buffer) name-of-buffer)
      (t
       (let* ((new-number (if number (1+ number) 2))
	      (new-name (concat name separator new-number))
	      temp)
	 (while (and (setq temp (get-buffer (concat "*" new-name "*")))
		     (setq temp (get-buffer-process temp))
		     (setq temp (process-status temp))
		     (or (eq 'run temp)
			 (eq 'open temp)))
	   (setq new-number (1+ new-number))
	   (setq new-name (concat name separator new-number)))
	 new-name)))))

(defun fi::make-subprocess-variables ()
  (setq fi::shell-directory-stack nil)
  
  (make-local-variable 'fi::last-input-start)
  (setq fi::last-input-start (make-marker))
  (make-local-variable 'fi::last-input-end)
  (setq fi::last-input-end (make-marker))
  
  (make-local-variable 'fi::input-ring)
  (setq fi::input-ring nil)
  (make-local-variable 'fi::input-ring-max)
  (setq fi::input-ring-max fi:default-input-ring-max)
  (make-local-variable 'fi::input-ring-yank-pointer)
  (setq fi::input-ring-yank-pointer nil)
  (make-local-variable 'fi::last-input-search-string)
  (setq fi::last-input-search-string ""))

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

(defun fi::remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
	       (if (memq char char-list)
		   nil
		 (char-to-string char)))
	     string
	     nil))

(defun fi::substitute-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (char-to-string (cdr pair))
		   (char-to-string char)))
	       string
	       nil)))

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
;;; Misc Initializations
;;;;

(mapcar 'make-variable-buffer-local
	'(fi:shell-popd-regexp
	  fi:shell-pushd-regexp 
	  fi:shell-cd-regexp
	  fi::shell-directory-stack
	  fi:subprocess-map-nl-to-cr
	  fi:subprocess-continuously-show-output-in-visible-buffer
	  fi:subprocess-enable-superkeys
	  fi:subprocess-super-key-map))
