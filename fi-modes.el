;;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.7 1988/03/04 09:10:22 layer Exp $
;;;
;;; Mode initializations

;;;;
;;; Variables
;;;;

(defvar fi:shell-mode-map nil
  "Fi:shell-mode keymap.")
(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in fi::subprocess modes.")

(defvar fi:inferior-lisp-mode-map nil
  "Fi:inferior-lisp-mode keymap.")
(defvar fi:inferior-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-lisp modes.")

(defvar fi:tcp-lisp-mode-map nil
  "Fi:tcp-lisp-mode keymap.")
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
\\[fi:subprocess-send-input] at end of buffer sends line as input.
\\[fi:subprocess-send-input] not at end copies rest of line to end and sends
it, starting with the prompt if is one or the beginning of the line if
there isn't.

An input ring saves input sent to the shell fi::subprocess.
\\[fi:pop-input] recalls previous input, travelling backward in the ring.
\\[fi:push-input] recalls previous input, travelling forward in the ring.
\\[fi:re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[fi:re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression.
\\[fi:list-input-ring] lists the contents of the input ring.

Here is a complete list of local key bindings:
\\{fi:shell-mode-map}
Keys that are shown above bound to `fi:subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`fi:subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:shell-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook and
shell-mode-hook, in this order and each with no args, if their
values are names of functions.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
Variables fi:shell-cd-regexp, fi:shell-pushd-regexp and fi:shell-popd-regexp are
used to match these command names, and are buffer-local variables."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:shell-mode)
  (setq mode-name "Shell")
  (fi::subprocess-mode-common)
  (if (null fi:shell-mode-super-key-map)
      (progn
	(setq fi:shell-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys fi:shell-mode-super-key-map)))
  (if (null fi:shell-mode-map)
      (setq fi:shell-mode-map
	(fi::shell-mode-commands (make-sparse-keymap)
				 fi:shell-mode-super-key-map)))
  (use-local-map fi:shell-mode-map)
  (make-local-variable 'fi:subprocess-super-key-map)
  (setq fi:subprocess-super-key-map fi:shell-mode-super-key-map)

  (make-local-variable 'subprocess-prompt-pattern)
  (setq subprocess-prompt-pattern
    (if prompt-pattern prompt-pattern fi:shell-prompt-pattern))
  
  (run-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:inferior-lisp-mode (&optional prompt-pattern)
  "Major mode for interacting with an inferior Lisp fi::subprocess.
\\[fi:inferior-lisp-newline] at end of buffer causes input to be collected and
send to the fi::subprocess when a complete sexp has been seen, and when seen
elsewhere is causes the line on which it was typed to be sent to the
fi::subprocess, less the prompt if there was one.

An input ring facility is also available (see `fi:shell-mode').

Here is a complete list of local key bindings:
\\{fi:inferior-lisp-mode-map}
Keys that are shown above bound to `fi:subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`fi:subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:inferior-lisp-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook,
lisp-mode-hook, and inferior-lisp-mode-hook, in this order and each with no
args, if their values are names of functions.

cd, pushd and popd commands (Lisp aliases, which emmulate the C shell
commands) behave as they do in fi:shell-mode (see fi:shell-mode)."
  (interactive)
  (kill-all-local-variables)
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table fi:lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (setq major-mode 'fi:inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (if (null fi:inferior-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys fi:inferior-lisp-mode-super-key-map)))
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
Keys that are shown above bound to `fi:subprocess-superkey' invoke their
bindings in the map bound (a buffer-local) called
`fi:subprocess-super-key-map' when at the end of the buffer, otherwise they
invoke their globally-bound functions.   The super-keys map is:
\\{fi:tcp-lisp-mode-super-key-map}

Entry to this mode applies the values of subprocess-mode-hook,
lisp-mode-hook, and tcp-lisp-mode-hook, in this order and each with no
args, if their values are names of functions.

cd, pushd and popd commands (Lisp aliases, which emmulate the C shell
commands) behave as they do in fi:shell-mode (see fi:shell-mode)."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:tcp-lisp-mode)
  (setq mode-name "TCP Lisp")
  (fi::lisp-mode-common)
  (fi::subprocess-mode-common)
  (set-syntax-table fi:lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (if (null fi:tcp-lisp-mode-super-key-map)
      (progn
	(setq fi:tcp-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-super-keys fi:tcp-lisp-mode-super-key-map)))
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
The bindings are
\\{fi:common-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook and
common-lisp-mode-hook, in this order, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:common-lisp-mode)
  (setq mode-name "Common Lisp")
  (fi::lisp-mode-common)
  (if (null fi:common-lisp-mode-map)
      (progn
	(setq fi:common-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:common-lisp-mode-map)))
  (use-local-map fi:common-lisp-mode-map)
  (fi::check-for-package-info)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-common-sublisp-name)
  (run-hooks 'fi:lisp-mode-hook 'fi:common-lisp-mode-hook))

(defun fi:franz-lisp-mode ()
  "Major mode for editing Lisp code to run in Franz Lisp.
The bindings are
\\{fi:franz-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook and
franz-lisp-mode-hook, in this order, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (fi::lisp-mode-common)
  (if (null fi:franz-lisp-mode-map)
      (progn
	(setq fi:franz-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:franz-lisp-mode-map)))
  (use-local-map fi:franz-lisp-mode-map)
  (fi::check-for-package-info)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
  (run-hooks 'fi:lisp-mode-hook 'fi:franz-lisp-mode-hook))

(defun fi:lisp-mode ()
  "Major mode for editing Lisp code to run in Generic Lisp.
The bindings are
\\{fi:lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:lisp-mode)
  (setq mode-name "Lisp")
  (fi::lisp-mode-common)
  (if (null fi:lisp-mode-map)
      (progn
	(setq fi:lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:lisp-mode-map)))
  (use-local-map fi:lisp-mode-map)
  (run-hooks 'fi:lisp-mode-hook))

(defun fi:emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in GNU Emacs.
The bindings are
\\{fi:emacs-lisp-mode-map}
Entry to this mode calls the value of lisp-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (fi::lisp-mode-common)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:emacs-lisp-mode-map)))
  (use-local-map fi:emacs-lisp-mode-map)
  (run-hooks 'fi:emacs-lisp-mode-hook))

(defun fi::lisp-mode-common ()
  (set-syntax-table fi:lisp-mode-syntax-table)
  
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
			      (symbolp (cadr p)))
			 (symbol-name (cadr p)))
			((stringp p) p)))))
	  (if fi::package
	      fi::package
	    (setq fi::package "user")))))))

;;;;
;;; Key defs
;;;;

(defun fi::subprocess-super-keys (map)
  "Bind keys in MAP to commands for subprocess modes.
The subprocess modes are `fi:shell-mode', `fi:inferior-lisp-mode' and
`fi:tcp-lisp-mode'."
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

  (if (eq major-mode 'fi:shell-mode)
      (define-key map "\C-z" 'fi:stop-shell-subjob))
  
  (if (eq major-mode 'fi:tcp-lisp-mode)
      (progn
	(define-key map "\C-c"  'fi:tcp-lisp-interrupt-process)
	(define-key map "\C-d"  'fi:tcp-lisp-send-eof)
	(define-key map "\C-\\" 'fi:tcp-lisp-kill-process))
    (progn
      (define-key map "\C-c"	'fi:interrupt-shell-subjob)
      (define-key map "\C-d"	'fi:shell-send-eof)
      (define-key map "\C-\\"	'fi:quit-shell-subjob)))

  map)

(defun fi::shell-mode-commands (map supermap)
  (define-key map "\C-m" 'fi:subprocess-send-input)
  (define-key map "\C-i" 'fi:shell-file-name-completion)
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

(defun fi::lisp-mode-commands (map)
  (if fi:lisp-auto-semicolon-mode
      (define-key map ";"		'fi:lisp-semicolon))
  
  (define-key map "\e" (make-sparse-keymap))
  (define-key map "\C-x" (make-sparse-keymap))
  
  (cond
    ((or (eq major-mode 'fi:common-lisp-mode)
	 (eq major-mode 'fi:franz-lisp-mode)
	 (eq major-mode 'fi:emacs-lisp-mode)
	 (eq major-mode 'fi:lisp-mode))
     
     (define-key map "\e\C-q"		'indent-sexp)
     (define-key map "\C-?"		'backward-delete-char-untabify)
     (define-key map "\r"		'fi:lisp-reindent-newline-indent)
	 
     (if fi:lisp-auto-semicolon-mode
	 (define-key map "\t"		'lisp-indent-line))
     
     (cond
       ((eq major-mode 'fi:emacs-lisp-mode)
	(define-key map "\e\C-x"	'eval-defun))
	   
       ((or (eq major-mode 'fi:common-lisp-mode)
	    (eq major-mode 'fi:franz-lisp-mode))
	(define-key map "\e\C-b"	'fi:eval-current-buffer)
	(define-key map "\C-x\e" 	'fi:eval-last-sexp)
	(define-key map "\e\C-r"	'fi:eval-region)
	(define-key map "\e\C-x"	'fi:eval-defun)
	    
	(cond ((eq major-mode 'fi:common-lisp-mode)
	       (define-key map "\e."	'fi:lisp-find-tag)
	       (define-key map "\e,"	'fi:lisp-tags-loop-continue)
	       (define-key map "\eA"	'fi:lisp-arglist)
	       (define-key map "\eD"	'fi:lisp-describe)
	       (define-key map "\eF"	'fi:lisp-function-documentation)
	       (define-key map "\eM"	'fi:lisp-macroexpand))))))

    ((or (eq major-mode 'fi:inferior-lisp-mode)
	 (eq major-mode 'fi:tcp-lisp-mode))
     (define-key map "\r"		'fi:inferior-lisp-newline)
     (define-key map "\e\r"		'fi:inferior-lisp-send-sexp-input)
     (define-key map "\C-x"		(make-sparse-keymap))
     (define-key map "\C-x\r"		'fi:inferior-lisp-send-list-input)
     (define-key map "\e\C-q"		'indent-sexp)))
  map)

(defun fi::tcp-lisp-mode-commands (map supermap)
  (fi::shell-mode-commands (fi::lisp-mode-commands map) supermap))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::shell-mode-commands (fi::lisp-mode-commands map) supermap))

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
