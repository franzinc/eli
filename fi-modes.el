;;;
;;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.1 1988/02/20 22:30:05 layer Exp $

;;;;
;;; Variables and Constants
;;;;

(defvar fi:shell-mode-map nil
  "Shell-mode keymap.")

(defvar fi:inferior-lisp-mode-map nil
  "Inferior-lisp-mode keymap.")

(defvar fi:emacs-lisp-mode-map nil
  "Emacs-lisp-mode keymap.")

(defvar fi:lisp-mode-map nil
  "Sublisp modes in common and franz.")

(defvar fi:subprocess-mode-map nil
  "Subprocess-mode map used for super-key processing.")

;;;;
;;; The Modes
;;;;

(defun fi:shell-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior shell.
Shell name is same as buffer name, sans the asterisks.
\\[subprocess-send-input] at end of buffer sends line as input.
\\[subprocess-send-input] not at end copies rest of line to end and sends it.

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
  (if (null fi:subprocess-mode-map)
      (progn
	(setq fi:subprocess-mode-map (make-sparse-keymap))
	(subprocess-mode-commands fi:subprocess-mode-map)))
  (if (null fi:shell-mode-map)
      (progn
	(setq fi:shell-mode-map (copy-keymap fi:subprocess-mode-map))
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

Entry to this mode applies the values of subprocess-mode-hook,
  lisp-mode-hook, and inferior-lisp-mode-hook, in this order and
  each with no args, if their values are names of functions.

You can send text to the Lisp subprocess from other buffers
  using the commands \\[send-region], \\[send-string], \\[lisp-send-defun],
  \\[franz-lisp-send-defun], and \\[common-lisp-send-defun]."
  (interactive)
  (lisp-mode)
  (set-syntax-table fi:lisp-mode-syntax-table)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (setq mode-line-process '(": %s"))
  (if (null fi:subprocess-mode-map)
      (progn
	(setq fi:subprocess-mode-map (make-sparse-keymap))
	(subprocess-mode-commands fi:subprocess-mode-map)))
  (if (null fi:inferior-lisp-mode-map)
      (progn
	(setq fi:inferior-lisp-mode-map
	  (copy-keymap fi:subprocess-mode-map))
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
  (define-key map "\C-m" 'subprocess-send-input)
  (define-key map "\C-n" 'push-input)
  (define-key map "\C-o" 'shell-send-flush)
  (define-key map "\C-p" 'pop-input)
  (define-key map "\C-r" 're-search-backward-input)
  (define-key map "\C-s" 're-search-forward-input)
  (define-key map "\C-u" 'fi:kill-shell-input)
  (define-key map "\C-v" 'fi:show-output-from-shell)
  (define-key map "\C-w" 'subprocess-backward-kill-word)
  (define-key map "\C-x" 'input-region)
  (define-key map "\C-y" 'pop-input)
  (define-key map "\C-z" 'fi:stop-shell-subjob)
  (define-key map "\C-\\" 'fi:quit-shell-subjob)
  (define-key map "\ew" 'push-input)
  (define-key map "\ex" 'input-ring-save)
  (fset 'Subprocess-Special-prefix map)
  map)

(defun inferior-lisp-mode-commands (map)
  (shell-mode-commands map)
  (fi:lisp-mode-commands map))

(defun shell-mode-commands (&optional map)
  (define-key map "\C-m" 'subprocess-send-input)
  (define-key map "\C-c" 'fi:interrupt-shell-subjob)
  (define-key map "\C-i" 'shell-file-name-completion)
  (if subprocess-enable-superkeys
    (progn
      (define-key map "\C-a" 'subprocess-superkey)
      (define-key map "\C-d" 'subprocess-superkey)
      (define-key map "\C-o" 'subprocess-superkey)
      (define-key map "\C-u" 'subprocess-superkey)
      (define-key map "\C-w" 'subprocess-superkey)
      (define-key map "\C-z" 'subprocess-superkey)
      (define-key map "\C-\\" 'subprocess-superkey)))
  map)

(defun emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in Emacs.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{fi:emacs-lisp-mode-map}
Entry to this mode calls the value of emacs-lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:emacs-lisp-mode-map)))
  (use-local-map fi:emacs-lisp-mode-map)
  (setq major-mode 'emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (lisp-mode-variables t)
  (run-hooks 'emacs-lisp-mode-hook))

(defun lisp-mode ()
  "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:lisp-mode-map)))
  (use-local-map fi:lisp-mode-map)
  (setq major-mode 'lisp-mode)
  (setq mode-name "Lisp")
  (lisp-mode-variables t)
  (run-hooks 'lisp-mode-hook))

(defun franz-lisp-mode ()
  "Major mode for editing Lisp code to run in Franz Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{fi:lisp-mode-map}
Entry to this mode calls the value of franz-lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:lisp-mode-map)))
  (use-local-map fi:lisp-mode-map)
  (setq major-mode 'franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (lisp-mode-variables t)
  
  (make-local-variable 'sublisp-name)
  (setq sublisp-name freshest-franz-sublisp-name)
  (run-hooks 'lisp-mode-hook 'franz-lisp-mode-hook))

(defun common-lisp-mode ()
  "Major mode for editing Lisp code to run in Common Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{fi:lisp-mode-map}
Entry to this mode calls the value of common-lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi:lisp-mode-commands fi:lisp-mode-map)))
  (use-local-map fi:lisp-mode-map)
  (setq major-mode 'common-lisp-mode)
  (setq mode-name "Common Lisp")
  (lisp-mode-variables t)			
  (make-local-variable 'sublisp-name)
  (setq sublisp-name freshest-common-sublisp-name)
  (run-hooks 'lisp-mode-hook 'common-lisp-mode-hook))

(defvar lisp-auto-semicolon-mode t
  "fancy automatic indentation of semicolons")

(defun fi:lisp-mode-commands (map)
  (if lisp-auto-semicolon-mode
      (define-key map ";"		'lisp-semicolon))
  
  (cond ((or (eq map fi:emacs-lisp-mode-map)
	     (eq map fi:lisp-mode-map))
	 (if lisp-auto-semicolon-mode 	; ...for jkf... :->
	     (define-key map "\t"	'lisp-indent-line))
	 (define-key map "\e\C-q"	'indent-sexp)
	 (define-key map "\C-?"		'backward-delete-char-untabify)
	 (define-key map "\r"		'lisp-reindent-newline-indent)
	 
	 (cond
	   ((eq map fi:emacs-lisp-mode-map)
	    (define-key map "\e\C-x"	'eval-defun))
	   
	   ((eq map fi:lisp-mode-map)
	    (define-key map "\e\C-r"	'fi:eval-region)
	    (define-key map "\e\C-x"	'fi:eval-defun)
	    (define-key map "\e\C-b"	'fi:eval-current-buffer)
	    (define-key map "\C-x\e" 	'fi:eval-last-sexp)
	    ;;
	    ;; the following use the backdoor tcp lisp listener:
	    ;;
	    (define-key map "\e."	'sublisp-display-source)
	    (define-key map "\eA"	'sublisp-arglist)
	    (define-key map "\eD" 	'sublisp-describe)
	    (define-key map "\eF" 	'sublisp-function-documentation)
	    (define-key map "\eM" 	'sublisp-macroexpand))))
  
	((eq map fi:inferior-lisp-mode-map)
	 (define-key map "\r"		'inferior-lisp-newline)
	 (define-key map "\e\r"		'inferior-lisp-send-sexp-input)
	 (define-key map "\C-x"		(make-sparse-keymap))
	 (define-key map "\C-x\r"	'inferior-lisp-send-list-input)
	 (define-key map "\e\C-q"	'indent-sexp))))

;;;;
;;; Misc functions
;;;;

(defun setup-lisp-mode-syntax-table (table)
  (modify-syntax-entry ?\| "\"   " table)
  (modify-syntax-entry ?_   "w   " table)
  (modify-syntax-entry ?-   "w   " table)
  (modify-syntax-entry ?.   "_   " table)
  (modify-syntax-entry ?\[  "_   " table)
  (modify-syntax-entry ?\]  "_   " table))

(defvar fi:lisp-mode-syntax-table nil)

(defun lisp-mode-variables (&optional lisp-syntax)
  (if lisp-syntax
      (progn
	(if (not fi:lisp-mode-syntax-table)
	    (progn
	      (setq fi:lisp-mode-syntax-table
		(copy-syntax-table emacs-lisp-mode-syntax-table))
	      (setup-lisp-mode-syntax-table fi:lisp-mode-syntax-table)))
	(set-syntax-table fi:lisp-mode-syntax-table)))
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)

  (make-local-variable 'package)
  (setq package "user")
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
  (setq comment-indent-hook-values '(0 nil))
  )

;;;;
;;; Initializations
;;;;

(defun fi:alist-parse (list string token)
  "recursively looks through list
   for an element whose car is string and makes its cdr token."
  (cond ((null list) nil)
	((equal (car (car list)) string)
	  (progn
	    (rplacd (car list) token)
	    t))
	(t (fi:alist-parse (cdr list) string token))))

;; the following is because the data associated with auto-mode-alist
;; is put in text space when xemacs is built, and is by default read-only.
(setq auto-mode-alist (copy-alist auto-mode-alist))

(defmacro push (x y)
  (list 'setq y (list 'cons x y)))

(if (not (fi:alist-parse auto-mode-alist "\\.l$" 'franz-lisp-mode))
    (push '("\\.l$" . franz-lisp-mode) auto-mode-alist))

(if (not (fi:alist-parse auto-mode-alist "\\.cl$" 'common-lisp-mode))
    (push '("\\.cl$" . common-lisp-mode) auto-mode-alist))

(fi:alist-parse auto-mode-alist "\\.el$" 'emacs-lisp-mode)
(fi:alist-parse auto-mode-alist "/\\..*emacs" 'emacs-lisp-mode)

(fi:alist-parse auto-mode-alist "\\.lisp$" 'common-lisp-mode)
(fi:alist-parse auto-mode-alist "\\.lsp$" 'common-lisp-mode)
(fi:alist-parse auto-mode-alist "\\.ml$" 'common-lisp-mode)
