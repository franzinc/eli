;;;
;;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.6 1988/02/29 18:33:36 layer Exp $

;;;;
;;; Variables
;;;;

(defvar fi:shell-mode-map nil
  "Shell-mode keymap.")
(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in subprocess modes.")

(defvar fi:inferior-lisp-mode-map nil
  "Inferior-lisp-mode keymap.")
(defvar fi:inferior-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-lisp modes.")

(defvar fi:tcp-lisp-mode-map nil
  "Tcp-lisp-mode keymap.")
(defvar fi:tcp-lisp-mode-super-key-map nil
  "Used for super-key processing in TCP mode.")

(defvar fi:common-lisp-mode-map nil "The Common Lisp major-mode map.")
(defvar fi:franz-lisp-mode-map nil "The Franz Lisp major-mode map.")
(defvar fi:emacs-lisp-mode-map nil "Emacs Lisp major-mode map.")
(defvar fi:lisp-mode-map nil "The Lisp major-mode map.")

(defvar fi:lisp-mode-syntax-table nil "The syntax table for all Lisp modes.")

;;;;
;;; The Modes
;;;;

(defun fi:shell-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior shell.
The shell process-name is same as the buffer name, sans the asterisks.
\\[subprocess-send-input] at end of buffer sends line as input.
\\[subprocess-send-input] not at end copies rest of line to end and sends
it, starting with the prompt if is one or the beginning of the line if
there isn't.

An input ring saves input sent to the shell subprocess.
\\[pop-input] recalls previous input, travelling backward in the ring.
\\[push-input] recalls previous input, travelling forward in the ring.
\\[re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression.
\\[list-input-ring] lists the contents of the input ring.

Here is a complete list of local key bindings:
\\{fi:shell-mode-map}
Keys that are shown above bound to `subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:shell-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook and
shell-mode-hook, in this order and each with no args, if their
values are names of functions.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp are
used to match these command names, and are buffer-local variables."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (subprocess-mode-common)
  (if (null fi:shell-mode-super-key-map)
      (progn
	(setq fi:shell-mode-super-key-map (make-sparse-keymap))
	(subprocess-super-keys fi:shell-mode-super-key-map)))
  (if (null fi:shell-mode-map)
      (setq fi:shell-mode-map
	(shell-mode-commands (make-sparse-keymap)
			     fi:shell-mode-super-key-map)))
  (use-local-map fi:shell-mode-map)
  (make-local-variable 'subprocess-super-key-map)
  (setq subprocess-super-key-map fi:shell-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern shell-prompt-pattern))
  
  (run-hooks 'subprocess-mode-hook 'shell-mode-hook))

(defun fi:inferior-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Lisp subprocess.
\\[inferior-lisp-newline] at end of buffer causes input to be collected and
send to the subprocess when a complete sexp has been seen, and when seen
elsewhere is causes the line on which it was typed to be sent to the
subprocess, less the prompt if there was one.

An input ring facility is also available (see `fi:shell-mode').

Here is a complete list of local key bindings:
\\{fi:inferior-lisp-mode-map}
Keys that are shown above bound to `subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:inferior-lisp-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook,
lisp-mode-hook, and inferior-lisp-mode-hook, in this order and each with no
args, if their values are names of functions.

cd, pushd and popd commands (Lisp aliases, which emmulate the C shell
commands) behave as they do in shell-mode (see fi:shell-mode)."
  (interactive)
  (kill-all-local-variables)
  (lisp-mode-common)
  (subprocess-mode-common)
  (set-syntax-table fi:lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (if (null fi:inferior-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-lisp-mode-super-key-map (make-sparse-keymap))
	(subprocess-super-keys fi:inferior-lisp-mode-super-key-map)))
  (if (null fi:inferior-lisp-mode-map)
      (setq fi:inferior-lisp-mode-map
	(inferior-lisp-mode-commands (make-sparse-keymap)
				     fi:inferior-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-lisp-mode-map)
  (make-local-variable 'subprocess-super-key-map)
  (setq subprocess-super-key-map fi:inferior-lisp-mode-super-key-map)
  
  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern lisp-prompt-pattern))

  (run-hooks 'subprocess-mode-hook 'lisp-mode-hook 'inferior-lisp-mode-hook))

(defun fi:tcp-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with a Common Lisp, where the communication
channel is a UNIX domain or internet socket.  The Emacs buffer name and the
lisp process have the same name, including asterisks.  The difference
between this mode and inferior-lisp mode is that operations such sending
interrupt or quit signals cannot be done on sockets.  For this reason, some
of these operations are implemented via a different mechanism.
\\[fi:tcp-lisp-send-eof] does a db:debug-pop on the Lisp process, 
\\[fi:tcp-lisp-kill-process] does a mp:process-kill, and
\\[fi:tcp-lisp-interrupt-process] does a mp:process-interrupt with #'break.

An input ring facility is also available (see `fi:shell-mode').

Here is a complete list of local key bindings:
\\{fi:tcp-lisp-mode-map}
Keys that are shown above bound to `subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:tcp-lisp-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook,
lisp-mode-hook, and tcp-lisp-mode-hook, in this order and each with no
args, if their values are names of functions.

cd, pushd and popd commands (Lisp aliases, which emmulate the C shell
commands) behave as they do in shell-mode (see fi:shell-mode)."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tcp-lisp-mode)
  (setq mode-name "TCP Lisp")
  (lisp-mode-common)
  (subprocess-mode-common)
  (set-syntax-table fi:lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (if (null fi:tcp-lisp-mode-super-key-map)
      (progn
	(setq fi:tcp-lisp-mode-super-key-map (make-sparse-keymap))
	(subprocess-super-keys fi:tcp-lisp-mode-super-key-map)))
  (if (null fi:tcp-lisp-mode-map)
      (setq fi:tcp-lisp-mode-map
	(tcp-lisp-mode-commands (make-sparse-keymap)
				fi:tcp-lisp-mode-super-key-map)))
  (use-local-map fi:tcp-lisp-mode-map)
  (make-local-variable 'subprocess-super-key-map)
  (setq subprocess-super-key-map fi:tcp-lisp-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
	(if prompt-pattern prompt-pattern lisp-prompt-pattern))

  (run-hooks 'subprocess-mode-hook 'lisp-mode-hook 'tcp-lisp-mode-hook))

(defun subprocess-mode-common ()
  (setq mode-line-process '(": %s"))
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
  (make-local-variable 'rlogin-subprocess-semaphore)
  (setq rlogin-subprocess-semaphore nil))

(defun common-lisp-mode ()
  "Major mode for editing Lisp code to run in Common Lisp.
The bindings are
\\{fi:common-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook and
common-lisp-mode-hook, in this order, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'common-lisp-mode)
  (setq mode-name "Common Lisp")
  (lisp-mode-common)
  (if (null fi:common-lisp-mode-map)
      (progn
	(setq fi:common-lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:common-lisp-mode-map)))
  (use-local-map fi:common-lisp-mode-map)
  (fi:check-for-package-info)
  (make-local-variable 'sublisp-name)
  (setq sublisp-name freshest-common-sublisp-name)
  (run-hooks 'lisp-mode-hook 'common-lisp-mode-hook))

(defun franz-lisp-mode ()
  "Major mode for editing Lisp code to run in Franz Lisp.
The bindings are
\\{fi:franz-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook and
franz-lisp-mode-hook, in this order, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (lisp-mode-common)
  (if (null fi:franz-lisp-mode-map)
      (progn
	(setq fi:franz-lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:franz-lisp-mode-map)))
  (use-local-map fi:franz-lisp-mode-map)
  (fi:check-for-package-info)
  (make-local-variable 'sublisp-name)
  (setq sublisp-name freshest-franz-sublisp-name)
  (run-hooks 'lisp-mode-hook 'franz-lisp-mode-hook))

(defun lisp-mode ()
  "Major mode for editing Lisp code to run in Generic Lisp.
The bindings are
\\{fi:lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lisp-mode)
  (setq mode-name "Lisp")
  (lisp-mode-common)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:lisp-mode-map)))
  (use-local-map fi:lisp-mode-map)
  (run-hooks 'lisp-mode-hook))

(defun emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in GNU Emacs.
The bindings are
\\{fi:emacs-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (lisp-mode-common)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:emacs-lisp-mode-map)))
  (use-local-map fi:emacs-lisp-mode-map)
  (run-hooks 'emacs-lisp-mode-hook))

(defun lisp-mode-common ()
  (set-syntax-table fi:lisp-mode-syntax-table)
  
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)

  (make-local-variable 'emacs-to-lisp-transaction-file)
  (make-local-variable 'emacs-to-lisp-transaction-buf)
  (make-local-variable 'emacs-to-lisp-package)
  
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'lisp-comment-indent)
  (make-local-variable 'comment-indent-hook-values)
  (setq comment-indent-hook-values '(0 nil)))

(defun fi:check-for-package-info ()
  (interactive)
  (make-local-variable 'package)
  (setq package nil)
  
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
			     (setq package
			       (buffer-substring beg (point)))))))
		 package))
	  package
	(let ((pos (re-search-forward "^(in-package[\t ]*" nil t)))
	  ;; find the `in-package' form, and snarf the package
	  ;; that way
	  (if pos
	      (let* ((start (match-end 0))
		     (end (progn (search-forward ")" nil t)
				 (match-beginning 0)))
		     (p-string (buffer-substring start end))
		     (p (car (read-from-string p-string))))
		(setq package
		  (cond ((symbolp p)
			 (if (= (elt (symbol-name p) 0) ?:)
			     (substring (symbol-name p) 1)
			   (symbol-name p)))
			((and (consp p)
			      (eq 'quote (car p))
			      (symbolp (cadr p)))
			 (symbol-name (cadr p)))
			((stringp p) p)))))
	  (if package
	      package
	    (setq package "user")))))))

;;;;
;;; Key defs
;;;;

(defun subprocess-super-keys (map)
  "Bind keys in MAP to commands for subprocess modes.
The subprocess modes are `shell-mode', `inferior-lisp-mode' and
`tcp-lisp-mode'."
  (define-key map "\C-a" 'subprocess-beginning-of-line)
  (define-key map "\C-k" 'fi:kill-output-from-shell)
  (define-key map "\C-l" 'list-input-ring)
  (define-key map "\C-n" 'push-input)
  (define-key map "\C-o" 'shell-send-flush)
  (define-key map "\C-p" 'pop-input)
  (define-key map "\C-r" 're-search-backward-input)
  (define-key map "\C-s" 're-search-forward-input)
  (define-key map "\C-u" 'fi:kill-shell-input)
  (define-key map "\C-v" 'fi:show-output-from-shell)
  (define-key map "\C-w" 'subprocess-backward-kill-word)
  (define-key map "\C-x" 'input-region)

  (if (eq major-mode 'shell-mode)
      (define-key map "\C-z" 'fi:stop-shell-subjob))
  
  (if (eq major-mode 'tcp-lisp-mode)
      (progn
	(define-key map "\C-c"  'fi:tcp-lisp-interrupt-process)
	(define-key map "\C-d"  'fi:tcp-lisp-send-eof)
	(define-key map "\C-\\" 'fi:tcp-lisp-kill-process))
    (progn
      (define-key map "\C-c"	'fi:interrupt-shell-subjob)
      (define-key map "\C-d"	'fi:shell-send-eof)
      (define-key map "\C-\\"	'fi:quit-shell-subjob)))

  map)

(defun shell-mode-commands (map supermap)
  (define-key map "\C-m" 'subprocess-send-input)
  (define-key map "\C-i" 'shell-file-name-completion)
  (if subprocess-enable-superkeys
      (progn
	(define-key map "\C-a"  'subprocess-superkey)
	;; \C-c points to supermap
	(define-key map "\C-d"  'subprocess-superkey)
	(define-key map "\C-o"  'subprocess-superkey)
	(define-key map "\C-u"  'subprocess-superkey)
	(define-key map "\C-w"  'subprocess-superkey)
	(define-key map "\C-z"  'subprocess-superkey)
	(define-key map "\C-\\" 'subprocess-superkey)))
  (define-key map "\C-c" supermap)
  map)

(defun fi:lisp-mode-commands (map)
  (if lisp-auto-semicolon-mode
      (define-key map ";"		'lisp-semicolon))
  
  (define-key map "\e" (make-sparse-keymap))
  (define-key map "\C-x" (make-sparse-keymap))
  
  (cond
    ((or (eq major-mode 'common-lisp-mode)
	 (eq major-mode 'franz-lisp-mode)
	 (eq major-mode 'emacs-lisp-mode)
	 (eq major-mode 'lisp-mode))
     
     (define-key map "\e\C-q"		'indent-sexp)
     (define-key map "\C-?"		'backward-delete-char-untabify)
     (define-key map "\r"		'lisp-reindent-newline-indent)
	 
     (if lisp-auto-semicolon-mode
	 (define-key map "\t"		'lisp-indent-line))
     
     (cond
       ((eq major-mode 'emacs-lisp-mode)
	(define-key map "\e\C-x"	'eval-defun))
	   
       ((or (eq major-mode 'common-lisp-mode)
	    (eq major-mode 'franz-lisp-mode))
	(define-key map "\e\C-b"	'fi:eval-current-buffer)
	(define-key map "\C-x\e" 	'fi:eval-last-sexp)
	(define-key map "\e\C-r"	'fi:eval-region)
	(define-key map "\e\C-x"	'fi:eval-defun)
	    
	(cond ((eq major-mode 'common-lisp-mode)
	       (define-key map "\e."	'fi:lisp-find-tag)
	       (define-key map "\e,"	'fi:lisp-tags-loop-continue)
	       (define-key map "\eA"	'fi:lisp-arglist)
	       (define-key map "\eD"	'fi:lisp-describe)
	       (define-key map "\eF"	'fi:lisp-function-documentation)
	       (define-key map "\eM"	'fi:lisp-macroexpand))))))

    ((or (eq major-mode 'inferior-lisp-mode)
	 (eq major-mode 'tcp-lisp-mode))
     (define-key map "\r"		'inferior-lisp-newline)
     (define-key map "\e\r"		'inferior-lisp-send-sexp-input)
     (define-key map "\C-x"		(make-sparse-keymap))
     (define-key map "\C-x\r"		'inferior-lisp-send-list-input)
     (define-key map "\e\C-q"		'indent-sexp)))
  map)

(defun tcp-lisp-mode-commands (map supermap)
  (fi:lisp-mode-commands (shell-mode-commands map supermap)))

(defun inferior-lisp-mode-commands (map supermap)
  (fi:lisp-mode-commands (shell-mode-commands map supermap)))

;;;;
;;; Initializations
;;;;

;; the following is because the data associated with auto-mode-alist
;; is put in text space when xemacs is built, and is by default read-only.
(setq auto-mode-alist (copy-alist auto-mode-alist))

(defun def-auto-mode (string mode)
  (let ((xx (assoc string auto-mode-alist)))
    (if xx
	(rplacd xx mode)
      (setq auto-mode-alist
	(cons (cons string mode) auto-mode-alist)))))

(def-auto-mode "\\.l$" 'franz-lisp-mode)
(def-auto-mode "\\.cl$" 'common-lisp-mode)
(def-auto-mode "\\.lisp$" 'common-lisp-mode)

(if (not fi:lisp-mode-syntax-table)
    (progn
      (setq fi:lisp-mode-syntax-table
	(copy-syntax-table emacs-lisp-mode-syntax-table))
      (modify-syntax-entry ?\| "\"   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?_   "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?-   "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?.   "_   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\[  "_   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\]  "_   " fi:lisp-mode-syntax-table)))
