;;; subprocess.el
;;;   subprocess modes and functions
;;;
;;; $Header: /repo/cvs.copy/eli/fi-subproc.el,v 1.3 1987/09/07 20:47:25 layer Exp $

(provide 'subprocess)

;;;;
;;; Variables and Constants
;;;;

(defvar fi:shell-mode-map nil "Shell-mode keymap.")
(defvar fi:inferior-lisp-mode-map nil "Inferior-lisp-mode keymap.")
(defvar fi:subprocess-mode-map nil "Special subprocess modes keymap.")

(defvar last-input-start nil
  "Marker for start of last input in shell-mode or inferior-lisp-mode buffer.")

(defvar last-input-end nil
  "Marker for end of last input in shell-mode or inferior-lisp-mode buffer.")

(defvar sublisp-name nil
  "Name of inferior lisp process.")

(defvar freshest-franz-sublisp-name nil
  "Name of franz lisp subprocess most recently invoked by franz-lisp.")

(defvar freshest-common-sublisp-name nil
  "Name of common lisp subprocess most recently invoked.")

(defvar shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")

(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd.
This variable is buffer-local.  If nil, no automatic directory changes
will be made.")

(defvar subprocess-map-nl-to-cr nil
  "*If t, map NL (newline) to CR (carriage-return) in input to subprocess.
This is a buffer-local symbol.")

(defvar subprocess-continuously-show-output-in-visible-buffer t
  "*If t, output from a subprocess to a visible buffer is continuously shown.
If a subprocess buffer is visible and the window point is beyond the process
output marker, output to that buffer from its associated process will be
continuously visible.  If the window point is before the process output
marker, the window is not updated.  This is a buffer-local symbol.")

(defvar subprocess-enable-superkeys nil
  "*If t, certain keys become `superkeys' in subprocess buffers.
The superkeys are C-a, C-d, C-o, C-u, C-w, C-z, and C-\\, which will behave
as they would in the `fi:subprocess-mode-map' keymap when typed at the end
of a subprocess buffer.  If typed elsewhere, these keys have their
normal global binding.  This is a buffer-local symbol.")

(defvar explicit-shell-file-name nil
  "*Explicit Shell image to invoke from (shell).")
(defvar explicit-rlogin-file-name nil
  "*Explicit remote-login image to invoke from (rlogin).")
(defvar explicit-lisp-file-name nil
  "*Explicit Lisp image to invoke from (lisp).")
(defvar explicit-franz-lisp-file-name nil
  "*Explicit Franz Lisp image to invoke from (franz-lisp).")
(defvar explicit-common-lisp-file-name nil
  "*Explicit Common Lisp image to invoke from (common-lisp).")
(defvar explicit-shell-image-arguments nil
  "*Explicit Shell image arguments when invoked from (shell).")
(defvar explicit-rlogin-image-arguments nil
  "*Explicit remote-login image arguments when invoked from (rlogin).")
(defvar explicit-lisp-image-arguments nil
  "*Explicit Lisp image arguments when invoked from (lisp).")
(defvar explicit-franz-lisp-image-arguments nil
  "*Explicit Franz Lisp image arguments when invoked from (franz-lisp).")
(defvar explicit-common-lisp-image-arguments nil
  "*Explicit Common Lisp image arguments when invoked from (common-lisp).")

(defvar default-shell-file-name "sh"
  "*Default Shell image to invoke from (shell).")
(defvar default-rlogin-file-name "rlogin"
  "*Default remote-login image to invoke from (rlogin).")
(defvar default-lisp-file-name "lisp"
  "*Default Lisp image to invoke from (lisp).")
(defvar default-franz-lisp-file-name "lisp"
  "*Default Franz Lisp image to invoke from (franz-lisp).")
(defvar default-common-lisp-file-name "cl"
  "*Default Common Lisp image to invoke from (common-lisp).")
(defvar default-shell-image-arguments '("-i")
  "*Default Shell image arguments when invoked from (shell).")
(defvar default-rlogin-image-arguments nil
  "*Default remote-login image arguments when invoked from (rlogin).")
(defvar default-lisp-image-arguments nil
  "*Default Lisp image arguments when invoked from (lisp).")
(defvar default-franz-lisp-image-arguments nil
  "*Default Franz Lisp image arguments when invoked from (franz-lisp).")
(defvar default-common-lisp-image-arguments nil
  "*Default Common Lisp image arguments when invoked from (common-lisp).")

(defvar subprocess-write-quantum 120
  "Maximum size in bytes of a single write request to a subprocess.")

(defvar shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "*Regexp used by Newline command in inferior-lisp mode to match Lisp prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "*Regexp used by Newline command in inferior-lisp mode to match Franz Lisp prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar common-lisp-prompt-pattern
  "^\\(\\[[0-9]+c?\\] \\|\\[step\\] \\)?<?cl> "
  "*Regexp for Newline command in inferior-lisp mode to match Common Lisp prompts.
Anything from beginning of line up to the end of what this pattern
matches is deemed to be prompt, and is not re-executed.")

;;;;
;;; Macros
;;;;

(defmacro push (x y)
  (list 'setq y (list 'cons x y)))

(defmacro shell-variable-bound-value (&rest name-components)
  "Macro to compose a symbol name and return its value if bound."
  (list 'let
	(list (list 'name-of-symbol
		    (append (list 'funcall ''concat) name-components)))
	'(and
	  (boundp (intern name-of-symbol))
	  (symbol-value (intern name-of-symbol)))))

;;;;
;;; Mode definitions
;;;;

(defun fi:shell-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior shell.
Shell name is same as buffer name, sans the asterisks.
\\[fi:shell-send-input] at end of buffer sends line as input.
\\[fi:shell-send-input] not at end copies rest of line to end and sends it.

An input ring saves input sent to the shell subprocess.
\\[pop-input] recalls previous input, travelling backward in the ring.
\\[push-input] recalls previous input, travelling forward in the ring.
\\[re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression.
\\[list-input-ring] lists the contents of the input ring.

Here is a complete list of the key bindings in shell-mode:
\\{fi:shell-mode-map}
Keys that are shown above bound to `subprocess-superkey' invoke their
bindings in the inferior key map shown below when at the end of the buffer,
otherwise they invoke their globally-bound functions.
\\{fi:subprocess-mode-map}

Entry to this mode applies the values of subprocess-mode-hook and
shell-mode-hook, in this order and each with no args, if their
values are names of functions.

cd, pushd and popd commands given to the shell are watched
by Emacs to keep this buffer's default directory
the same as the shell's working directory.
Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp
are used to match these command names.

You can send text to the shell (or its subjobs) from other buffers
using the commands \\[send-region], \\[send-string] and \\[fi:eval-defun]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (setq mode-line-process '(": %s"))
  (if (null fi:shell-mode-map)
      (progn
	(setq fi:shell-mode-map (make-sparse-keymap))
	(shell-mode-commands fi:shell-mode-map)))
  (use-local-map fi:shell-mode-map)
  (make-local-variable 'shell-directory-stack)
  (setq shell-directory-stack nil)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'input-ring)
  (setq input-ring nil)
  (make-local-variable 'input-ring-max)
  (setq input-ring-max default-input-ring-max)
  (make-local-variable 'input-ring-yank-pointer)
  (setq input-ring-yank-pointer nil)
  (make-local-variable 'last-input-search-string)
  (setq last-input-search-string "")
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
	(if prompt-pattern prompt-pattern shell-prompt-pattern))
  (make-local-variable 'rlogin-subprocess-semaphore)
  (setq rlogin-subprocess-semaphore nil)
  (run-hooks 'subprocess-mode-hook 'shell-mode-hook))

(defun fi:inferior-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Lisp subprocess.
\\[inferior-lisp-send-list-input] at end of buffer sends all input since last Lisp
  subprocess output to the Lisp subprocess.
\\[inferior-lisp-send-list-input] can take numeric argument to specify that
  s-expressions are to be sent instead.
\\[inferior-lisp-send-list-input] not at end copies s-expression to end and sends it.
\\[inferior-lisp-send-list-input] can take numeric argument to specify number of
  s-expressions to copy and send.
\\[newline-and-indent] starts a new line at the proper indentation.
\\[lisp-indent-line] indents the current line.
\\[self-insert-tab] inserts a real tab.
\\[indent-sexp] indents the current s-expression.
\\[backward-delete-char-untabify] converts tabs to spaces while deleting back.

An input ring saves input sent to the Lisp subprocess.
\\[pop-input] recalls previous input, travelling backward in the ring.
\\[push-input] recalls previous input, travelling forward in the ring.
\\[re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression.
\\[list-input-ring] lists the contents of the input ring.

Here is a complete list of the key bindings in inferior-lisp-mode:
\\{fi:inferior-lisp-mode-map}
Keys that are shown above bound to `subprocess-superkey' invoke their
bindings in the inferior key map shown below when at the end of the buffer,
otherwise they invoke their globally-bound functions.
\\{fi:subprocess-mode-map}

Entry to this mode applies the values of subprocess-mode-hook,
  lisp-mode-hook, and inferior-lisp-mode-hook, in this order and
  each with no args, if their values are names of functions.

You can send text to the Lisp subprocess from other buffers
  using the commands \\[send-region], \\[send-string], \\[lisp-send-defun],
  \\[franz-lisp-send-defun], and \\[common-lisp-send-defun]."
  (interactive)
  (lisp-mode)
  (set-syntax-table lisp-mode-syntax-table)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (setq mode-line-process '(": %s"))
  (if (null fi:inferior-lisp-mode-map)
      (progn
	(setq fi:inferior-lisp-mode-map (make-sparse-keymap))
	(inferior-lisp-mode-commands fi:inferior-lisp-mode-map)))
  (use-local-map fi:inferior-lisp-mode-map)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'input-ring)
  (setq input-ring nil)
  (make-local-variable 'input-ring-max)
  (setq input-ring-max default-input-ring-max)
  (make-local-variable 'input-ring-yank-pointer)
  (setq input-ring-yank-pointer nil)
  (make-local-variable 'last-input-search-string)
  (setq last-input-search-string "")
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
	(if prompt-pattern prompt-pattern lisp-prompt-pattern))
  (make-local-variable 'rlogin-subprocess-semaphore)
  (setq rlogin-subprocess-semaphore nil)
  (run-hooks 'subprocess-mode-hook 'lisp-mode-hook 'inferior-lisp-mode-hook))

(defun subprocess-mode-commands (map)
  "Bind keys in MAP to commands for subprocess modes.
The subprocess modes are `shell-mode' and `inferior-lisp-mode'.
This function binds subprocess functions to control and escape bindings
in the MAP given as argument."
  (define-key map "\C-a" 'subprocess-beginning-of-line)
  (define-key map "\C-c" 'fi:interrupt-shell-subjob)
  (define-key map "\C-d" 'fi:shell-send-eof)
  (define-key map "\C-k" 'fi:kill-output-from-shell)
  (define-key map "\C-l" 'list-input-ring)
  (define-key map "\C-m" 'fi:shell-send-input)
  (define-key map "\C-n" 'push-input)
  (define-key map "\C-o" 'shell-send-flush)
  (define-key map "\C-p" 'pop-input)
  ;;(define-key map "\C-r" 'fi:show-output-from-shell)
  (define-key map "\C-r" 're-search-backward-input)
  (define-key map "\C-s" 're-search-forward-input)
  (define-key map "\C-u" 'fi:kill-shell-input)
  (define-key map "\C-v" 'fi:show-output-from-shell)
  (define-key map "\C-w" 'subprocess-backward-kill-word)
  (define-key map "\C-x" 'input-region)
  ;;(define-key map "\C-y" 'fi:copy-last-shell-input)
  (define-key map "\C-y" 'pop-input)
  (define-key map "\C-z" 'fi:stop-shell-subjob)
  (define-key map "\C-\\" 'fi:quit-shell-subjob)
  (define-key map "\ew" 'push-input)
  (define-key map "\ex" 'input-ring-save)
  (fset 'Subprocess-Special-prefix map)
  map)

(defun inferior-lisp-mode-commands (map)
  (shell-mode-commands map)
  (fi:lisp-mode-commands map t)
  (define-key map "\r"		'subprocess-send-input)
  (define-key map "\e\r"	'inferior-lisp-send-sexp-input)
  (define-key map "\C-x\r"	'inferior-lisp-send-list-input)
  (define-key map "\C-i"	'lisp-indent-line)
  (define-key map "\e\C-q"	'indent-sexp))

(defun shell-mode-commands (&optional map)
  (define-key map "\C-a" 'subprocess-beginning-of-line)
  (define-key map "\C-m" 'fi:shell-send-input)
  (define-key map "\C-c" 'fi:interrupt-shell-subjob)
  (if subprocess-enable-superkeys
    (progn
      (define-key map "\C-d" 'subprocess-superkey)
      (define-key map "\C-o" 'subprocess-superkey)
      (define-key map "\C-u" 'subprocess-superkey)
      (define-key map "\C-w" 'subprocess-superkey)
      (define-key map "\C-z" 'subprocess-superkey)
      (define-key map "\C-\\" 'subprocess-superkey)))
  map)

;;;;
;;; User visible functions
;;;;

(defun fi:shell (&optional number)
  "Run an inferior shell, with input/output through buffer *shell*.
See `subprocess'."
  (interactive "P")
  (subprocess "shell" "shell" number))

(defun another-shell (&optional number)
  "Run a new inferior shell, with input/output through buffer *shell-N*.
This function always creates a new subprocess and buffer.  See `subprocess'."
  (interactive "P")
  (subprocess "shell" "shell" number t))

(defun rlogin (host &optional number)
  "Run an inferior remote login, with input/output through buffer *<host>*.
See `subprocess'.  Hook function `rlogin-subprocess-hook' will be applied
in the buffer if defined."
  (interactive "sRemote login to host: \nP")
  (subprocess "rlogin" host number nil (list host))
  (setq shell-popd-regexp nil)
  (setq shell-pushd-regexp nil)
  (setq shell-cd-regexp nil)
  (setq rlogin-subprocess-semaphore t)
  (set-process-filter (get-buffer-process (current-buffer))
		      'rlogin-filter)
  (run-hooks 'rlogin-subprocess-hook))

(defun another-rlogin (host &optional number)
  "Run a new remote login, with input/output through buffer *<host>-N*.
This function always creates a new subprocess and buffer.  See `subprocess'.
Hook function `rlogin-subprocess-hook' will be applied in the newly-created
buffer if defined."
  (interactive "sRemote login to host: \nP")
  (subprocess "rlogin" host number t (list host))
  (setq shell-popd-regexp nil)
  (setq shell-pushd-regexp nil)
  (setq shell-cd-regexp nil)
  (setq rlogin-subprocess-semaphore t)
  (set-process-filter (get-buffer-process (current-buffer))
		      'rlogin-filter)
  (run-hooks 'rlogin-subprocess-hook))

(defun run-franz-lisp (&optional number)
  "Run a Franz Lisp subprocess, with input/output through buffer *franz-lisp*.
Returns the name of the started subprocess.  See `subprocess'."
  (interactive "P")
  (setq freshest-franz-sublisp-name 
    (subprocess "franz-lisp" "franz-lisp" number)))

(defun run-another-franz-lisp (&optional number)
  "Run a new Franz Lisp subprocess, with i/o through buffer *franz-lisp-N*.
Returns the name of the started subprocess.  
This function always creates a new subprocess and buffer.  See `subprocess'."
  (interactive "P")
  (setq freshest-franz-sublisp-name 
    (subprocess "franz-lisp" "franz-lisp" number t)))

(defun run-common-lisp (&optional number)
  "Run Common Lisp subprocess, with input/output through buffer *common-lisp*.
Returns the name of the started subprocess.  See `subprocess'."
  (interactive "P")
  (setq freshest-common-sublisp-name 
    (subprocess "common-lisp" "common-lisp" number)))

(defun run-another-common-lisp (&optional number)
  "Run a new Common Lisp subprocess, with i/o through buffer *common-lisp-N*.
Returns the name of the started subprocess.  
This function always creates a new subprocess and buffer.  See `subprocess'."
  (interactive "P")
  (setq freshest-common-sublisp-name 
    (subprocess "common-lisp" "common-lisp" number t)))

;;;;
;;; Interactively called functions (from keymaps)
;;;;

(defun subprocess-superkey (&optional special-binding)
  "This function implements superkeys in subprocess buffers.
A superkey is treated specially when at the end of a subprocess buffer,
but has its normal, global, binding when used elsewhere in the buffer.
At the end of the buffer the key has SPECIAL-BINDING.  If SPECIAL-BINDING
is not given, the key takes its binding from the `fi:subprocess-mode-map'
key map."
  (interactive)
  (if (eobp)
      (if special-binding
	  (call-interactively special-binding)
	(progn
	  (if (null fi:subprocess-mode-map)
	      (progn
		(setq fi:subprocess-mode-map (make-sparse-keymap))
		(subprocess-mode-commands fi:subprocess-mode-map)))
	  (subprocess-reprocess-keys fi:subprocess-mode-map)))
    (subprocess-reprocess-keys global-map)))

(defun subprocess-reprocess-keys (&optional map key)
  "Reprocess KEY or the last key sequence (which may be incomplete) in MAP.
This is used to reprocess a key sequence as if it were seen in another
context, e.g. to process global bindings of keys from a subprocess
buffer (in shell-mode or inferior-lisp-mode) when some keys are hit
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
	    (lookup-key last-binding (setq last-key
					   (char-to-string
					    (read-char))))))
    (if (commandp last-binding)
      (call-interactively last-binding)
      (ding))))

(defun subprocess-beginning-of-line (arg)
  "Move to beginning of line, skipping over initial prompt.
Moves point to beginning of line, just like (beginning-of-line),
except that if the pattern at the beginning of the line matches the
current subprocess prompt pattern, this function skips over it."
  (interactive "P")
  (beginning-of-line arg)
  (if (looking-at subprocess-prompt-pattern)
    (re-search-forward subprocess-prompt-pattern nil t)))

(defun subprocess-backward-kill-word (words)
  "Kill previous word(s) in current subprocess input line.
 This function takes care not to delete past most recent subprocess output."
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (marker-position (process-mark (get-buffer-process (current-buffer))))
     (point))
    (backward-kill-word words)))

(defun fi:shell-send-input ()
  "Send input to subshell.
At end of buffer, sends all text after last output
  as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
  after first attempting to discard any prompt at the beginning of the line
  by matching the regexp that is the value of shell-prompt-pattern if
  possible.  This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward shell-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
  ;; Even if we get an error trying to hack the working directory,
  ;; still send the input to the subshell.
  (condition-case ()
      (save-excursion
	(goto-char last-input-start)
	(cond
	 ((and (and shell-popd-regexp
		    (looking-at shell-popd-regexp))
	       (memq (char-after (match-end 0)) '(?\; ?\n)))
	  (if shell-directory-stack
	      (progn
		(cd (car shell-directory-stack))
		(setq shell-directory-stack (cdr shell-directory-stack)))))
	 ((and shell-pushd-regexp
	       (looking-at shell-pushd-regexp))
	  (cond
	   ((memq (char-after (match-end 0)) '(?\; ?\n))
	    (if shell-directory-stack
		(let ((old default-directory))
		  (cd (car shell-directory-stack))
		  (setq shell-directory-stack
		    (cons old (cdr shell-directory-stack))))))
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
		    (setq shell-directory-stack
		      (cons default-directory shell-directory-stack))
		    (cd dir)))))))
	 ((and shell-cd-regexp
	       (looking-at shell-cd-regexp))
	  (cond
	   ((memq (char-after (match-end 0)) '(?\; ?\n))
	    (cd (getenv "HOME")))
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
		  (cd dir))))))))
    (error nil))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region-split process last-input-start last-input-end
		       subprocess-map-nl-to-cr)
    (input-ring-save last-input-start (1- last-input-end))
    (set-marker (process-mark process) (point))))

(defun subprocess-send-input ()
  "Send input to subprocess.
At end of buffer, sends all text after last output
  as input to the subprocess, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
  after first attempting to discard any prompt at the beginning of the line
  by matching the regexp that is the value of subprocess-prompt-pattern if
  possible.  This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward subprocess-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region-split process last-input-start last-input-end
		       subprocess-map-nl-to-cr)
    (input-ring-save last-input-start (1- last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:shell-send-eof ()
  "Send eof to subshell (or to the program running under it)."
  (interactive)
  (if rlogin-subprocess-semaphore
      (rlogin-send-eof)
    (process-send-eof)))

(defun fi:copy-last-shell-input ()
  "Copy previous shell input, sans newline, and insert before point."
  (interactive)
  (insert (buffer-substring last-input-end last-input-start))
  (delete-char -1))

(defun fi:kill-output-from-shell ()
  "Kill all output from shell since last input."
  (interactive)
  (goto-char (point-max))
  (kill-region last-input-end (point))
  (insert "[output flushed]\n"))

(defun shell-send-flush ()
  "Send `flush output' character (^O) to subprocess."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:show-output-from-shell ()
  "Display start of this batch of shell output at top of window.
Also put cursor there."
  (interactive)
  (set-window-start (selected-window) last-input-end)
  (goto-char last-input-end))

(defun fi:interrupt-shell-subjob ()
  "Interrupt this shell's current subjob."
  (interactive)
  (if rlogin-subprocess-semaphore
      (rlogin-send-interrupt)
    (interrupt-process nil t)))

(defun fi:kill-shell-subjob ()
  "Send kill signal to this shell's current subjob."
  (interactive)
  (kill-process nil t))

(defun fi:quit-shell-subjob ()
  "Send quit signal to this shell's current subjob."
  (interactive)
  (if rlogin-subprocess-semaphore
      (rlogin-send-quit)
    (quit-process nil t)))

(defun fi:stop-shell-subjob ()
  "Stop this shell's current subjob."
  (interactive)
  (if rlogin-subprocess-semaphore
      (rlogin-send-stop)
    (stop-process nil t)))

(defun fi:kill-shell-input ()
  "Kill all text since last stuff output by the shell or its subjobs."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

(defun rlogin-send-eof ()
  "Send eof to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-d"))

(defun rlogin-send-interrupt ()
  "Send interrupt to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-c"))

(defun rlogin-send-quit ()
  "Send quit to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-\\"))

(defun rlogin-send-stop ()
  "Send stop to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun inferior-lisp-send-sexp-input (arg)
  "Send s-expression(s) to the Lisp subprocess."
  (interactive "P")
  (inferior-lisp-send-input arg 'sexp))

(defun inferior-lisp-send-list-input (arg)
  "Send list(s) to the Lisp subprocess."
  (interactive "P")
  (inferior-lisp-send-input arg 'lists))

(defun inferior-lisp-send-input (arg type)
  "Send s-expression(s) or list(s) to the Lisp subprocess.
The second parameter must be either 'sexps or 'lists, specifying
  whether lists or s-expressions should be parsed (internally,
  either `(scan-sexps)' or `(scan-lists)' is used).
If at the end of buffer, everything typed since the last output
  from the Lisp subprocess is collected and sent to the Lisp
  subprocess.  With an argument, only the specified number of
  s-expressions or lists from the end of the buffer are sent.
If in the middle of the buffer, the current s-expression(s) or
  list(s) is(are) copied to the end of the buffer and then sent.
  An argument specifies the number of s-expressions or lists to
  be sent.
If s-expressions are being parsed:
  If the cursor follows a closing parenthesis, the preceding
  s-expression(s) is(are) processed.  If the cursor is at an
  opening parenthesis, the following s-expression(s) is(are)
  processed.  If the cursor is at a closing parenthesis, the
  preceding s-expression(s) is(are) processed.  Otherwise, the
  enclosing s-expression(s) is(are) processed.
If lists are being parsed:
  The enclosing list is processed."
  (if (and (eobp) (null arg))
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(lisp-indent-line)
	(move-marker last-input-end (point)))

    ;; we are in the middle of the buffer somewhere and need to collect
    ;; and s-exp to re-send

    ;; we grab everything from the end of the current line back to the end
    ;; of the last prompt
    
    (let ((exp-to-resend "")
	  (start-resend (point))
	  (end-resend (point)))
      (if (null arg) (setq arg 1))
      (if (equal type 'sexp)
	  (setq exp-to-resend
	    (buffer-substring
	     (setq start-resend
	       (save-excursion
		 (cond
		  ((= (preceding-char) ?\))
		   (scan-sexps (point) (- arg)))
		  ((= (following-char) ?\() (point))
		  ((= (following-char) ?\))
		   (forward-char 1) (scan-sexps (point) (- arg)))
		  (t (scan-sexps (point) (- arg))))))
	     (setq end-resend
	       (save-excursion
		 (cond
		  ((= (preceding-char) ?\)) (point))
		  ((= (following-char) ?\() (scan-sexps (point) arg))
		  ((= (following-char) ?\)) (forward-char 1) (point))
		  (t (scan-sexps (point) arg)))))))
	(setq exp-to-resend
	  (buffer-substring
	   (setq start-resend (scan-lists (point) (- arg) 1))
	   (setq end-resend (scan-lists (point) arg 1)))))
      (if (eobp)
	  (progn
	    (insert "\n")
	    (lisp-indent-line)
	    (move-marker last-input-start start-resend)
	    (move-marker last-input-end (point-max)))
	(progn
	  (goto-char (point-max))
	  (move-marker last-input-start (point))
	  (insert exp-to-resend)
	  (if (not (bolp)) (insert "\n"))
	  (move-marker last-input-end (point))))))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region-split process last-input-start last-input-end
		       subprocess-map-nl-to-cr)
    (input-ring-save last-input-start (1- last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:sublisp-select ()
  "Find a sublisp for eval commands to send code to.  Result stored in
the variable sublisp-name.  
If sublisp-name is set, and there is an associated process buffer, thats that.
If sublisp-name is nil, or if there is no process buffer with that name, then
try for freshest-<franz,common>-sublisp-name, which should contain the name of
the most recently started sublisp.  If neither of these exist, runs the command
franz-lisp or common-lisp, depending on the major mode of the buffer."
  (interactive)
  ;;; see if sublisp is named yet.  if its not, name it intelligently.
  (cond (sublisp-name t)
	((eql major-mode 'franz-lisp-mode)
	 (if freshest-franz-sublisp-name
	     (setq sublisp-name freshest-franz-sublisp-name)
	   (setq sublisp-name "franz-lisp")))
	((eql major-mode 'common-lisp-mode)
	 (if freshest-common-sublisp-name
	     (setq sublisp-name freshest-common-sublisp-name)
	   (setq sublisp-name "common-lisp")))
	(t (error "Cant start a subprocess for Major mode %s." major-mode)))
  ;; start-up the sublisp process if necessary and possible
  (cond ((get-process sublisp-name) t)
	((eql major-mode 'franz-lisp-mode)
	 (if (and freshest-franz-sublisp-name 
		  (get-process freshest-franz-sublisp-name))
	     (setq sublisp-name freshest-franz-sublisp-name)
	   (setq sublisp-name (prog1
				  (run-franz-lisp nil)
				(switch-to-buffer nil)
				(sleep-for 5)))))
	((eql major-mode 'common-lisp-mode)
	 (if (and freshest-common-sublisp-name 
		  (get-process freshest-common-sublisp-name))
	     (setq sublisp-name freshest-common-sublisp-name)
	   (setq sublisp-name (prog1
				  (run-common-lisp nil)
				(switch-to-buffer nil)
				(sleep-for 1)))))
	(t (error
	    "Cant start a subprocess for sublisp-name %s."
	    sublisp-name))))

(defun fi:package-select ()
  (save-excursion
    (if (null (re-search-backward "^(in-package " nil t))
	nil
      (let ((start (point))
	    res)
	(forward-sexp)
	(setq res (buffer-substring start (point)))
	res))))

(defun fi:eval-last-sexp ()
  "Send sexp before point to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive)
  (let* ((pkg (fi:package-select))
	 (stab (syntax-table))
	 (start  (unwind-protect
		     (save-excursion
		       (set-syntax-table lisp-mode-syntax-table)
		       (forward-sexp -1)
		       (point))
		   (set-syntax-table stab))))
    (fi:eval-send start (point) pkg)))

(defun fi:eval-defun ()
  "Send the current `defun' to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive)
  (let* ((pkg (fi:package-select))
	 (end	(save-excursion
		  (end-of-defun)
		  (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (fi:eval-send start end pkg)))

(defun fi:eval-send (start end pkg)
  "Send the text substring from START to END over to the sublisp."
  (fi:sublisp-select)
  (let ((stuff (buffer-substring start end))
	(source-buffer (current-buffer))
	(sublisp-buffer (process-buffer (get-process sublisp-name))))
    (if pkg
	(send-string-split
	 sublisp-name
	 (concat
	  "(progn (setq si::%emacs-package-save% (package-name *package*))"
	  pkg "(values))\n")
	 subprocess-map-nl-to-cr))
    (send-string-split sublisp-name stuff subprocess-map-nl-to-cr)
    (if pkg
	(send-string-split
	 sublisp-name
	 "(progn (in-package si::%emacs-package-save%)(values))"
	 subprocess-map-nl-to-cr))
    (send-string-split sublisp-name "\n" subprocess-map-nl-to-cr)
    (switch-to-buffer-other-window sublisp-buffer)
    (input-ring-save-string stuff)
    (goto-char (point-max))))
  
(defun fi:eval-region  ()
  "Send the region to the Lisp subprocess sublisp-name an sexp at a time.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive)
  (fi:sublisp-select)
  (unwind-protect
      (let ((start (min (point) (mark)))
	    (end   (max (point) (mark)))
	    (source-buffer (buffer-name))
	    (sublisp-process  (get-process sublisp-name)))
	(save-excursion			;check out if point=mark case
	  ;;(set-process-filter sublisp-process 'sublisp-eval-region-filter)
	  (goto-char start)
	  (forward-list)
	  (while (>= end (point))
	    (fi:eval-last-sexp)		;note:this puts us in sublisp-buffer 
	    (switch-to-buffer-other-window  source-buffer)
	    (forward-list)
	    (if (equal (point) (point-max)) ;hack for end of file case
		(setq end 0))))
	(switch-to-buffer-other-window (process-buffer sublisp-process)))))

(defun fi:eval-current-buffer ()
  "Send the entire current buffer to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive)
  (save-window-excursion
    (beginning-of-buffer)
    (set-mark-command nil)
    (end-of-buffer)
    (fi:eval-region)))

(defun set-associated-sublisp (buffer-name)
  "Set the sublisp associated with a franz lisp or common lisp source file."
  (interactive "sSublisp name: ")
  (if (get-process buffer-name)
      (setq sublisp-name buffer-name)
    (error "No such process buffer.")))

;;;;
;;; The Guts (the lowest of the low (level))
;;;;

(defun subprocess (image name &optional number another arguments)
  "Spawn a subprocess with input/output through an Emacs buffer.
Process NAME or NAME-NUMBER is created.  Returns the name of the
subprocess buffer without the asterisks.
If the associated buffer
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
invoked, that file is sent to the subprocess as initial input.  The
subprocess buffer is put in the appropriate mode, either
`inferior-lisp-mode' if the process name contains a string
\"[Ll][Ii][Ss][Pp]\" or `shell-mode'.  The subprocess prompt
is set to the value of variable `<NAME>-prompt-pattern' if found, otherwise
`<IMAGE>-prompt-pattern'.
Also see `make-shell' and `make-another-shell'."
  (interactive "sImage name: \nsProcess name: ")
  (let* ((majuscule-image (remove-chars-from-string '(?-) (upcase image)))
	 (majuscule-name (remove-chars-from-string '(?-) (upcase name)))
	 (image-file (or (shell-variable-bound-value
			  "explicit-" name "-file-name")
			 (shell-variable-bound-value
			  "explicit-" image "-file-name")
			 (getenv (concat "E" majuscule-name))
			 (getenv (concat "E" majuscule-image))
			 (getenv majuscule-name)
			 (getenv majuscule-image)
			 (shell-variable-bound-value
			  "default-" name "-file-name")
			 (shell-variable-bound-value
			  "default-" image "-file-name")))
	 (image-prompt (or (shell-variable-bound-value
			    name "-prompt-pattern")
			   (shell-variable-bound-value
			    image "-prompt-pattern")))
	 (image-arguments (cond
			   ((shell-variable-bound-value
			     "explicit-" name "-file-name")
			    (shell-variable-bound-value
			     "explicit-" name "-image-arguments"))
			   ((shell-variable-bound-value
			     "explicit-" image "-file-name")
			    (shell-variable-bound-value
			     "explicit-" image "-image-arguments"))
			   ((shell-variable-bound-value
			     "default-" name "-file-name")
			    (shell-variable-bound-value
			     "default-" name "-image-arguments"))
			   ((shell-variable-bound-value
			     "default-" image "-file-name")
			    (shell-variable-bound-value
			     "default-" image "-image-arguments"))
			   (t
			    nil)))
	 (start-up-feed-name (concat "~/.emacs_"
				     (file-name-nondirectory image-file))))
    (apply
     (if another 'make-another-shell 'fi:make-shell)
     (append
      (list
       name
       image-file)
      (if another (list number) nil)
      (list
       (if (file-exists-p start-up-feed-name) start-up-feed-name)
       image-prompt)
      (append image-arguments arguments))) ))

(defun fi:make-shell (name program &optional startfile prompt-pattern
			&rest arguments)
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
      (set-process-sentinel proc 'subprocess-sentinel)
      (set-process-filter proc 'subprocess-filter)
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
	(send-string-split proc startfile subprocess-map-nl-to-cr)))
      (setq name (process-name proc)))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (if (string-match "[Ll][Ii][Ss][Pp]" name)
	(fi:inferior-lisp-mode prompt-pattern)
	(fi:shell-mode prompt-pattern))
    name))

(defun make-another-shell (name program
				&optional number startfile prompt-pattern
				&rest arguments)
  "Create another subprocess that does input/output through a buffer."
  (let* ((separator "-")
	 (name-of-process (if number
			    (concat name separator number)
			    name))
	 (buffer (get-buffer (concat "*" name-of-process "*"))))
    (cond
     ((null buffer)
      (apply 'fi:make-shell
	     (append (list name-of-process program startfile prompt-pattern)
		     arguments)))
     (t
      (let* ((new-number (if number (1+ number) 2))
	     (new-name (concat name separator new-number))
	     temp)
	(while (and (setq temp (get-buffer (concat "*" new-name "*")))
		    (setq temp (get-buffer-process temp))
		    (eq 'run (process-status temp)))
	  (setq new-number (1+ new-number))
	  (setq new-name (concat name separator new-number)))
	(apply 'fi:make-shell
	       (append (list new-name program startfile prompt-pattern)
		       arguments)))))))

(defun send-region-split (process start-position end-position
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
    (send-string-split process string nl-cr)))

(defun send-string-split (process string &optional nl-cr)
  "Send string to process in small pieces using send-string."
  (interactive "sSend (to process): \nsSend to process in pieces (string): ")
  (let ((size (length string))
	(filtered-string (if nl-cr
			   (substitute-chars-in-string '((?\n . ?\r)) string)
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
					  subprocess-write-quantum))))
		      t)
		  (error
		   (message "Error writing to subprocess.")
		   nil)))
      (setq size (- size subprocess-write-quantum))
      (setq start (+ start subprocess-write-quantum)))))

(defun rlogin-filter (process output)
  "Filter for `rlogin' subprocess buffers.
Watch for the first shell prompt from the remote login, then send the string
\"stty -echo nl\", and turn ourself off."
  (let ((old-buffer (subprocess-filter process output t)))
    (if (save-excursion (beginning-of-line)
			(looking-at subprocess-prompt-pattern))
	(progn
	  (set-process-filter process 'subprocess-filter)
	  (send-string-split process "stty -echo nl\n" nil)))
    (if old-buffer
	(set-buffer old-buffer))))

;;; Sentinel and filter for subprocesses.  The sentinel is currently
;;;   not used.
(defun subprocess-sentinel (process status)
  t)

(defun subprocess-filter (process output &optional stay)
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
      ;; Go to point of last output by subprocess and insert new
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
	(if (and subprocess-continuously-show-output-in-visible-buffer
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
     (t (set-buffer old-buffer)))))

(defun remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
		(if (memq char char-list)
		    nil
		  (char-to-string char)))
	     string
	     nil))

(defun substitute-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		  (if (setq pair (assq char char-assoc-list))
		      (char-to-string (cdr pair))
		    (char-to-string char)))
	       string
	       nil)))

;;;;
;;; Misc Initializations
;;;;

(mapcar 'make-variable-buffer-local
	'(shell-popd-regexp
	  shell-pushd-regexp 
	  shell-cd-regexp
	  subprocess-map-nl-to-cr
	  subprocess-continuously-show-output-in-visible-buffer
	  subprocess-enable-superkeys))
