;;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.12 1988/04/04 20:18:10 layer Exp $
;;;
;;; Mode initializations

;;;;
;;; Variables
;;;;

(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")
(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:rlogin-mode-map nil
  "The rlogin major-mode keymap.")
(defvar fi:rlogin-mode-super-key-map nil
  "Used for super-key processing in rlogin mode.")

(defvar fi:inferior-common-lisp-mode-map nil
  "The inferior-common-lisp major-mode keymap.")
(defvar fi:inferior-common-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-common-lisp mode.")

(defvar fi:inferior-lisp-mode-map nil
  "The inferior-lisp major-mode keymap.")
(defvar fi:inferior-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-lisp mode.")

(defvar fi:inferior-franz-lisp-mode-map nil
  "The inferior-franz-lisp major-mode keymap.")
(defvar fi:inferior-franz-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-franz-lisp mode.")

(defvar fi:tcp-lisp-mode-map nil
  "The tcp-lisp major-mode keymap.")
(defvar fi:tcp-lisp-mode-super-key-map nil
  "Used for super-key processing in tcp-lisp mode.")

(defvar fi:common-lisp-mode-map nil "The Common Lisp major-mode map.")
(defvar fi:franz-lisp-mode-map nil "The Franz Lisp major-mode map.")
(defvar fi:emacs-lisp-mode-map nil "Emacs Lisp major-mode map.")
(defvar fi:lisp-mode-map nil "The Lisp major-mode map.")

;;;;
;;; The Modes
;;;;

(defun fi:shell-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior shell.  PROMPT-PATTERN is
the regular expression defining the prompt.  The shell process-name is same
as the buffer name, without asterisks. \\[fi:subprocess-send-input] at end
of buffer sends line as input. \\[fi:subprocess-send-input] not at end
copies rest of line to end and sends it, starting with the prompt if is one
or the beginning of the line if there isn't.

An input ring saves input sent to the shell subprocess. \\[fi:pop-input]
recalls previous input, travelling backward in the ring. \\[fi:push-input]
recalls previous input, travelling forward in the ring.
\\[fi:re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[fi:re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression. \\[fi:list-input-ring]
lists the contents of the input ring.

The value of the variable `fi:shell-mode-map' is the local keymap used in
shell mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:shell-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:shell-mode-hook', in this order and each with no args, if their values
are the names of functions.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
the buffer's default directory the same as the shell's working directory.
Variables `fi:shell-cd-regexp', `fi:shell-pushd-regexp' and
`fi:shell-popd-regexp' are used to match these command names, and are
buffer-local variables."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:shell-mode)
  (setq mode-name "Shell")
  (fi::subprocess-mode-common)
  (if (null fi:shell-mode-super-key-map)
      (progn
	(setq fi:shell-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys fi:shell-mode-super-key-map 'shell)))
  (if (null fi:shell-mode-map)
      (setq fi:shell-mode-map
	(fi::shell-mode-commands (make-sparse-keymap)
				 fi:shell-mode-super-key-map
				 'shell)))
  (use-local-map fi:shell-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:shell-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:shell-prompt-pattern))
  
  (run-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:rlogin-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior rlogin.  PROMPT-PATTERN is
the regular expression defining the prompt.

This mode is essentially the same as `fi:shell-mode' except that:
the working directory is not tracked (in the presence of NFS mounted file
systems doing this would present problems unless the mount points where the
same on the local and remote machines) and interrupt, end-of-file, quit and
stop actions are handled specially.

The local keymap for this mode is bound to `fi:rlogin-mode-map' and
super-keys are obtained from `fi:rlogin-mode-super-key-map'.  The prompt
pattern is taken from `fi:shell-prompt-pattern'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:rlogin-mode-hook', in this order and each with no args, if their values
are the names of functions."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:rlogin-mode)
  (setq mode-name "Rlogin")
  (fi::subprocess-mode-common)
  (if (null fi:rlogin-mode-super-key-map)
      (progn
	(setq fi:rlogin-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys fi:rlogin-mode-super-key-map 'rlogin)))
  (if (null fi:rlogin-mode-map)
      (setq fi:rlogin-mode-map
	(fi::shell-mode-commands (make-sparse-keymap)
				 fi:rlogin-mode-super-key-map
				 'rlogin)))
  (use-local-map fi:rlogin-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:rlogin-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:shell-prompt-pattern))
  
  (setq fi:shell-popd-regexp nil)
  (setq fi:shell-pushd-regexp nil)
  (setq fi:shell-cd-regexp nil)

  (run-hooks 'fi:subprocess-mode-hook 'fi:rlogin-mode-hook))

(defun fi:inferior-common-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Common Lisp subprocess.
PROMPT-PATTERN is the regular expression defining the prompt.
\\[fi:inferior-lisp-newline] at end of buffer causes input to be collected
and send to the subprocess when a complete sexp has been seen, and when
seen elsewhere it causes the line on which it was typed to be sent to the
subprocess, less the prompt if there was one.

An input ring facility is also available (see `fi:shell-mode').

The value of the variable `fi:inferior-common-lisp-mode-map' is the local
keymap used in inferior-common-lisp mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:inferior-common-lisp-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:inferior-common-lisp-mode-hook', in this order and each with no args,
if their values are names of functions.

:cd, :pushd and :popd top-level commands (Allegro CL aliases, which
emmulate the C shell commands) behave as they do in `fi:shell-mode'."
  (interactive)
  (kill-all-local-variables)
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (setq major-mode 'fi:inferior-common-lisp-mode)
  (setq mode-name "Inferior Common Lisp")
  (if (null fi:inferior-common-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-common-lisp-mode-super-key-map
	  (make-sparse-keymap))
	(fi::subprocess-super-keys
	 fi:inferior-common-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-common-lisp-mode-map)
      (setq fi:inferior-common-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-sparse-keymap) fi:inferior-common-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-common-lisp-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:inferior-common-lisp-mode-super-key-map)
  
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:lisp-prompt-pattern))

  (run-hooks 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook
	     'fi:inferior-common-lisp-mode-hook))

(defun fi:inferior-franz-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Franz Lisp subprocess.
PROMPT-PATTERN is the regular expression defining the prompt.
\\[fi:inferior-lisp-newline] at end of buffer causes input to be collected
and send to the subprocess when a complete sexp has been seen, and when
seen elsewhere it causes the line on which it was typed to be sent to the
subprocess, less the prompt if there was one.

An input ring facility is also available (see `fi:shell-mode').

The value of the variable `fi:inferior-franz-lisp-mode-map' is the local
keymap used in inferior-lisp mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:inferior-franz-lisp-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:inferior-franz-lisp-mode-hook', in this order and each with no args, if
their values are names of functions."
  (interactive)
  (kill-all-local-variables)
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (setq major-mode 'fi:inferior-franz-lisp-mode)
  (setq mode-name "Inferior Franz Lisp")
  (if (null fi:inferior-franz-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-franz-lisp-mode-super-key-map
	  (make-sparse-keymap))
	(fi::subprocess-super-keys
	 fi:inferior-franz-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-franz-lisp-mode-map)
      (setq fi:inferior-franz-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-sparse-keymap) fi:inferior-franz-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-franz-lisp-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:inferior-franz-lisp-mode-super-key-map)
  
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:lisp-prompt-pattern))

  (run-hooks 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook
	     'fi:inferior-franz-lisp-mode-hook))

(defun fi:inferior-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Lisp subprocess.
PROMPT-PATTERN is the regular expression defining the prompt.
\\[fi:inferior-lisp-newline] at end of buffer causes input to be collected
and send to the subprocess when a complete sexp has been seen, and when
seen elsewhere it causes the line on which it was typed to be sent to the
subprocess, less the prompt if there was one.

An input ring facility is also available (see `fi:shell-mode').

The value of the variable `fi:inferior-lisp-mode-map' is the local keymap
used in inferior-lisp mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:inferior-lisp-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:inferior-lisp-mode-hook', in this order and each with no args, if their
values are names of functions."
  (interactive)
  (kill-all-local-variables)
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (setq major-mode 'fi:inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (if (null fi:inferior-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys
	 fi:inferior-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-lisp-mode-map)
      (setq fi:inferior-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-sparse-keymap) fi:inferior-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-lisp-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:inferior-lisp-mode-super-key-map)
  
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:lisp-prompt-pattern))

  (run-hooks 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook
	     'fi:inferior-lisp-mode-hook))

(defun fi:tcp-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with a Common Lisp over a TCP , where the
communication channel is a UNIX domain or internet socket.  The Emacs
buffer name and the lisp process have the same name, including asterisks.
The difference between this mode and inferior-lisp mode is that operations
such sending interrupt or quit signals cannot be done on sockets.  For this
reason, some of these operations are implemented via a different mechanism.
\\[fi:tcp-lisp-send-eof] does a #'db:debug-pop on the Lisp process, 
\\[fi:tcp-lisp-kill-process] does a #'mp:process-kill, and
\\[fi:tcp-lisp-interrupt-process] does a mp:process-interrupt with #'break.

An input ring facility is also available (see `fi:shell-mode').

The value of the variable `fi:tcp-lisp-mode-map' is the local keymap
used in tcp-lisp mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:tcp-lisp-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:tcp-lisp-mode-hook', in this order and each with no args,
if their values are names of functions.

:cd, :pushd and :popd top-level commands (Allegro CL aliases, which
emmulate the C shell commands) behave as they do in `fi:shell-mode'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:tcp-lisp-mode)
  (setq mode-name "TCP Lisp")
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (if (null fi:tcp-lisp-mode-super-key-map)
      (progn
	(setq fi:tcp-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys
	 fi:tcp-lisp-mode-super-key-map 'tcp-lisp)))
  (if (null fi:tcp-lisp-mode-map)
      (setq fi:tcp-lisp-mode-map
	(fi::tcp-lisp-mode-commands (make-sparse-keymap)
				fi:tcp-lisp-mode-super-key-map)))
  (use-local-map fi:tcp-lisp-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:tcp-lisp-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
	(if prompt-pattern prompt-pattern fi:lisp-prompt-pattern))

  (run-hooks 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook
	     'fi:tcp-lisp-mode-hook))

(defun fi::subprocess-mode-common ()
  (setq mode-line-process '(": %s"))
  (make-local-variable 'fi::shell-directory-stack)
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
  (setq fi::last-input-search-string "")
  (make-local-variable 'fi::rlogin-subprocess-semaphore)
  (setq fi::rlogin-subprocess-semaphore nil))

(defun common-lisp-mode ()
  "Same as fi:common-lisp-mode--for compatibility."
  (fi:common-lisp-mode))

(defun fi:common-lisp-mode ()
  "Major mode for editing Lisp code to run in Common Lisp.
The bindings are taken from the variable `fi:common-lisp-mode-map'.
Entry to this mode calls the value of `fi:lisp-mode-hook' and
`fi:common-lisp-mode-hook', in this order, if their value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:common-lisp-mode)
  (setq mode-name "Common Lisp")
  (fi::lisp-mode-common)
  (if (null fi:common-lisp-mode-map)
      (progn
	(setq fi:common-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:common-lisp-mode-map nil)))
  (use-local-map fi:common-lisp-mode-map)
  (fi::check-for-package-info)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-common-sublisp-name)
  (run-hooks 'fi:lisp-mode-hook 'fi:common-lisp-mode-hook))

(defun fi:franz-lisp-mode ()
  "Major mode for editing Lisp code to run in Franz Lisp.
The bindings are taken from the variable `fi:franz-lisp-mode-map'.
Entry to this mode calls the value of `fi:lisp-mode-hook' and
`fi:franz-lisp-mode-hook', in this order, if their value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (fi::lisp-mode-common)
  (if (null fi:franz-lisp-mode-map)
      (progn
	(setq fi:franz-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:franz-lisp-mode-map nil)))
  (use-local-map fi:franz-lisp-mode-map)
  (fi::check-for-package-info)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
  (run-hooks 'fi:lisp-mode-hook 'fi:franz-lisp-mode-hook))

(defun fi:lisp-mode ()
  "Major mode for editing Lisp code to run in Generic Lisp.
The bindings are taken from the variable `fi:lisp-mode-map'.
Entry to this mode calls the value of `fi:lisp-mode-hook' if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:lisp-mode)
  (setq mode-name "Lisp")
  (fi::lisp-mode-common)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:lisp-mode-map nil)))
  (use-local-map fi:lisp-mode-map)
  (run-hooks 'fi:lisp-mode-hook))

(defun fi:emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in GNU Emacs.
The bindings are taken from the variable `fi:emacs-lisp-mode-map'.
Entry to this mode calls the value of `fi:emacs-lisp-mode-hook' if that
value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (fi::lisp-mode-common)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:emacs-lisp-mode-map nil)))
  (use-local-map fi:emacs-lisp-mode-map)
  (run-hooks 'fi:emacs-lisp-mode-hook))

(defun fi::lisp-mode-common ()
  (set-syntax-table lisp-mode-syntax-table)
  
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)

  (make-local-variable 'fi::emacs-to-lisp-transaction-file)
  (make-local-variable 'fi::emacs-to-lisp-transaction-buf)
  (make-local-variable 'fi::emacs-to-lisp-package)
  
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'fi:lisp-comment-indent)
  (make-local-variable 'fi::comment-indent-hook-values)
  (setq fi::comment-indent-hook-values '(0 nil)))

(defun fi::check-for-package-info ()
  (interactive)
  (make-local-variable 'fi::package)
  (setq fi::package nil)
  
  (save-excursion
    ;; look for -*- ... package: xxx; .... -*-
    (let (beg end mode)
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (if (and (search-forward "-*-" (save-excursion (end-of-line) (point)) t)
	       (progn
		 (skip-chars-forward " \t")
		 (setq beg (point))
		 (search-forward "-*-"
				 (save-excursion (end-of-line) (point)) t))
	       (progn
		 (forward-char -3)
		 (skip-chars-backward " \t")
		 (setq end (point))
		 (goto-char beg)
		 (if (search-forward ":" end t)
		     (progn
		       (goto-char beg)
		       (if (let ((case-fold-search t))
			     (search-forward "package:" end t))
			   (progn
			     (skip-chars-forward " \t")
			     (setq beg (point))
			     (if (search-forward ";" end t)
				 (forward-char -1)
			       (goto-char end))
			     (skip-chars-backward " \t")
			     (setq fi::package
			       (buffer-substring beg (point)))))))
		 fi::package))
	  fi::package
	(let* ((case-fold-search t)
	       (pos (re-search-forward "^(in-package[\t ]*" nil t)))
	  ;; find the `in-package' form, and snarf the package
	  ;; that way
	  (if pos
	      (let* ((start (match-end 0))
		     (end (progn (search-forward ")" nil t)
				 (match-beginning 0)))
		     (p-string (buffer-substring start end))
		     (p (car (read-from-string p-string))))
		(setq fi::package
		  (cond ((symbolp p)
			 (if (= (elt (symbol-name p) 0) ?:)
			     (substring (symbol-name p) 1)
			   (symbol-name p)))
			((and (consp p)
			      (eq 'quote (car p))
			      (symbolp (car (cdr p))))
			 (let ((name (symbol-name (car (cdr p)))))
			   (if (= (elt name 0) ?:)
			       (substring name 1)
			     name)))
			((stringp p) p)))))
	  (if fi::package
	      fi::package
	    (setq fi::package "user")))))))

;;;;
;;; Key defs
;;;;

(defun fi::subprocess-super-keys (map mode)
  "Setup keys in MAP as a subprocess super-key map.  MODE is either
shell, rlogin, sub-lisp or tcp-lisp."
  (define-key map "\C-a" 'fi:subprocess-beginning-of-line)
  (define-key map "\C-k" 'fi:kill-output-from-shell)
  (define-key map "\C-l" 'fi:list-input-ring)
  (define-key map "\C-n" 'fi:push-input)
  (define-key map "\C-o" 'fi:shell-send-flush)
  (define-key map "\C-p" 'fi:pop-input)
  (define-key map "\C-r" 'fi:re-search-backward-input)
  (define-key map "\C-s" 'fi:re-search-forward-input)
  (define-key map "\C-u" 'fi:kill-shell-input)
  (define-key map "\C-v" 'fi:show-output-from-shell)
  (define-key map "\C-w" 'fi:subprocess-backward-kill-word)
  (define-key map "\C-x" 'fi:input-region)

  (cond
    ((memq mode '(sub-lisp shell))
     (define-key map "\C-z"	'fi:stop-shell-subjob)
     (define-key map "\C-c"	'fi:interrupt-shell-subjob)
     (define-key map "\C-d"	'fi:shell-send-eof)
     (define-key map "\C-\\"	'fi:quit-shell-subjob))
    ((eq mode 'rlogin)
     (define-key map "\C-z"	'fi:rlogin-send-stop)
     (define-key map "\C-c"	'fi:rlogin-send-interrupt)
     (define-key map "\C-d"	'fi:rlogin-send-eof)
     (define-key map "\C-\\"	'fi:rlogin-send-quit))
    ((eq mode 'tcp-lisp)
     (define-key map "\C-c"	'fi:tcp-lisp-interrupt-process)
     (define-key map "\C-d"	'fi:tcp-lisp-send-eof)
     (define-key map "\C-\\"	'fi:tcp-lisp-kill-process)))
  map)

(defun fi::shell-mode-commands (map supermap mode)
  "Define subprocess mode commands on MAP, using SUPERMAP as the supermap.
MODE is either sub-lisp, tcp-lisp, shell or rlogin."
  (define-key map "\C-m" 'fi:subprocess-send-input)
  (if (not (eq 'rlogin mode))
      (define-key map "\C-i" 'fi:shell-file-name-completion))
  (if fi:subprocess-enable-superkeys
      (progn
	(define-key map "\C-a"  'fi:subprocess-superkey)
	;; \C-c points to supermap
	(define-key map "\C-d"  'fi:subprocess-superkey)
	(define-key map "\C-o"  'fi:subprocess-superkey)
	(define-key map "\C-u"  'fi:subprocess-superkey)
	(define-key map "\C-w"  'fi:subprocess-superkey)
	(define-key map "\C-z"  'fi:subprocess-superkey)
	(define-key map "\C-\\" 'fi:subprocess-superkey)))
  (define-key map "\C-c" supermap)
  map)

(defun fi::lisp-mode-commands (map mode)
  (define-key map "\e" (make-sparse-keymap))
  (define-key map "\C-x" (make-sparse-keymap))
  (define-key map "\C-c" (make-sparse-keymap))
  
  (define-key map "\e\C-q"	'fi:indent-sexp)
  (define-key map "\C-?"	'backward-delete-char-untabify)
  
  (if fi:lisp-auto-semicolon-mode
      (progn
	(define-key map ";"	'fi:lisp-semicolon)
	(define-key map "\t"	'fi:lisp-indent-line)))
  
  (cond
    ((memq mode '(sub-lisp tcp-lisp))
     (define-key map "\r"	'fi:inferior-lisp-newline)
     (define-key map "\e\r"	'fi:inferior-lisp-send-sexp-input)
     (define-key map "\C-x\r"	'fi:inferior-lisp-send-list-input))
    (t 
     (define-key map "\r"	'fi:lisp-reindent-newline-indent)))
  
  (cond
    ((memq major-mode '(fi:common-lisp-mode fi:inferior-common-lisp-mode
			fi:tcp-lisp-mode))
     (define-key map "\e."	'fi:lisp-find-tag)
     (define-key map "\e,"	'fi:lisp-tags-loop-continue)
     (define-key map "\eA"	'fi:lisp-arglist)
     (define-key map "\eD"	'fi:lisp-describe)
     (define-key map "\eF"	'fi:lisp-function-documentation)
     (define-key map "\eM"	'fi:lisp-macroexpand)
     (define-key map "\eW"	'fi:lisp-walk)))
  (cond
    ((eq major-mode 'fi:emacs-lisp-mode)
     (define-key map "\e\C-x"	'eval-defun))
    ((memq major-mode '(fi:common-lisp-mode fi:franz-lisp-mode
			fi:lisp-mode))
     (define-key map "\e\C-x"	'fi:lisp-eval-defun)
     (define-key map "\C-c\C-b"	'fi:lisp-eval-current-buffer)
     (define-key map "\C-c\C-s" 'fi:lisp-eval-last-sexp)
     (define-key map "\C-c\C-r"	'fi:lisp-eval-region)))
  map)

(defun fi::tcp-lisp-mode-commands (map supermap)
  (fi::shell-mode-commands (fi::lisp-mode-commands map 'tcp-lisp)
			   supermap 'tcp-lisp))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::shell-mode-commands (fi::lisp-mode-commands map 'sub-lisp)
			   supermap 'sub-lisp))

;;;;
;;; Initializations
;;;;

;; the following is because the data associated with auto-mode-alist
;; is put in text space when xemacs is built, and is by default read-only.
(setq auto-mode-alist (copy-alist auto-mode-alist))

(defun fi::def-auto-mode (string mode)
  (let ((xx (assoc string auto-mode-alist)))
    (if xx
	(rplacd xx mode)
      (setq auto-mode-alist
	(cons (cons string mode) auto-mode-alist)))))

(fi::def-auto-mode "\\.cl$" 'fi:common-lisp-mode)
(fi::def-auto-mode "\\.lisp$" 'fi:common-lisp-mode)
(fi::def-auto-mode "\\.l$" 'fi:franz-lisp-mode)
(fi::def-auto-mode "\\.el$" 'fi:emacs-lisp-mode)
(fi::def-auto-mode "[]>:/]\\..*emacs" 'fi:emacs-lisp-mode)
