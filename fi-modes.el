;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Id: fi-modes.el,v 1.68 1997/01/07 01:04:10 layer Exp $

;;;; Mode initializations

;;;
;; Variables
;;;

(defvar fi:inferior-common-lisp-mode-map nil
  "The inferior-common-lisp major-mode keymap.")
(defvar fi:inferior-common-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-common-lisp mode.")

(defvar fi:inferior-franz-lisp-mode-map nil
  "The inferior-franz-lisp major-mode keymap.")
(defvar fi:inferior-franz-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-franz-lisp mode.")

(defvar fi:lisp-listener-mode-map nil
  "The tcp-lisp major-mode keymap.")
(defvar fi:lisp-listener-mode-super-key-map nil
  "Used for super-key processing in tcp-lisp mode.")

(defvar fi:common-lisp-mode-map nil
  "Major mode map used when editing Common Lisp source.")
(defvar fi:franz-lisp-mode-map nil
  "Major mode map used when editing Franz Lisp source.")
(defvar fi:emacs-lisp-mode-map nil
  "Major mode map used when editing GNU Emacs Lisp source.")

(defvar fi:lisp-mode-syntax-table nil
  "The value of which is the syntax table for all Lisp modes, except Emacs
Lisp mode.")
(defvar fi:emacs-lisp-mode-syntax-table nil
  "The value of which is the syntax table for Emacs Lisp mode.")

(defvar fi:common-lisp-file-types '(".cl" ".lisp" ".lsp")
  "*A list of the file types which are automatically put in
fi:common-lisp-mode.  NOTE: the value of this variable is only used at
interface load time.  Setting after the interface is loaded will have no
effect.")

(defvar fi:lisp-do-indentation t
  "*When non-nil, do FI-style indentation in Lisp modes.")

(defvar fi:subprocess-mode nil
  "Non-nil when buffer has a subprocess.")

(add-hook 'fi:common-lisp-mode-hook
	  (function
	   (lambda ()
	     (when (not (fi:member-equal "; pkg:" mode-line-process))
	       (setq mode-line-process
		 (append mode-line-process
			 '((fi:package ("; pkg:" fi:package))))))
	     (when (not (fi:member-equal "; rt:" mode-line-process))
	       (setq mode-line-process
		 (append mode-line-process
			 '((fi:readtable ("; rt:" fi:readtable))))))))
	  "*The initial value of this hook, which is run whenever a Lisp mode is
entered, causes the `package' and readtable (if any) to be displayed in the
mode line.  It uses MODE-LINE-PROCESS, which has no use in non-subprocess
buffers.")

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

(defvar fi::menubar-initialization nil)

;;;;
;;; The Modes
;;;;

(defun fi::kill-all-local-variables ()
  ;; don't kill the input ring, which can be very useful
  (let ((input-ring fi::input-ring)
	(input-ring-yank-pointer fi::input-ring-yank-pointer)
	(last-input-search-string fi::last-input-search-string)
	(last-command-was-successful-search
	 fi::last-command-was-successful-search))
    (kill-all-local-variables)
    (setq fi::input-ring input-ring)
    (setq fi::input-ring-yank-pointer input-ring-yank-pointer)
    (setq fi::last-input-search-string last-input-search-string)
    (setq fi::last-command-was-successful-search
      last-command-was-successful-search)))

(defun fi:inferior-common-lisp-mode (&optional mode-hook &rest mode-hook-args)
  "Major mode for interacting with Common Lisp subprocesses.
The keymap for this mode is bound to fi:inferior-common-lisp-mode-map:
\\{fi:inferior-common-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:subprocess-mode-hook
	fi:inferior-common-lisp-mode-hook

in the above order.

When calling from a program, arguments are MODE-HOOK and MODE-HOOK-ARGS,
the former is applied to the latter just after killing all local variables
but before doing any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (apply mode-hook mode-hook-args))
  (setq major-mode 'fi:inferior-common-lisp-mode)
  (setq mode-name "Inferior Common Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:inferior-common-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-common-lisp-mode-super-key-map
	  (make-keymap))
	(fi::subprocess-mode-super-keys
	 fi:inferior-common-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-common-lisp-mode-map)
      (setq fi:inferior-common-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-keymap)
	 fi:inferior-common-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-common-lisp-mode-map)
  (setq fi:subprocess-super-key-map fi:inferior-common-lisp-mode-super-key-map)
  (setq fi:lisp-indent-hook-property 'fi:common-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:subprocess-mode-hook
	     'fi:inferior-common-lisp-mode-hook))

(defun fi:inferior-franz-lisp-mode (&optional mode-hook &rest mode-hook-args)
  "Major mode for interacting with Franz Lisp subprocesses.
The keymap for this mode is bound to fi:inferior-franz-lisp-mode-map:
\\{fi:inferior-franz-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:subprocess-mode-hook
	fi:inferior-franz-lisp-mode-hook

in the above order.

When calling from a program, arguments are MODE-HOOK and MODE-HOOK-ARGS,
the former is applied to the latter just after killing all local variables
but before doing any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (apply mode-hook mode-hook-args))
  (setq major-mode 'fi:inferior-franz-lisp-mode)
  (setq mode-name "Inferior Franz Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:inferior-franz-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-franz-lisp-mode-super-key-map
	  (make-keymap))
	(fi::subprocess-mode-super-keys
	 fi:inferior-franz-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-franz-lisp-mode-map)
      (setq fi:inferior-franz-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-keymap)
	 fi:inferior-franz-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-franz-lisp-mode-map)
  (setq fi:subprocess-super-key-map fi:inferior-franz-lisp-mode-super-key-map)
  (setq fi:lisp-indent-hook-property 'fi:franz-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:subprocess-mode-hook
	     'fi:inferior-franz-lisp-mode-hook))

(defun fi:lisp-listener-mode (&optional mode-hook)
  "Major mode for interacting with Common Lisp over a socket.
The keymap for this mode is bound to fi:lisp-listener-mode-map:
\\{fi:lisp-listener-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:subprocess-mode-hook
	fi:lisp-listener-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:lisp-listener-mode)
  (setq mode-name "TCP Common Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:lisp-listener-mode-super-key-map)
      (progn
	(setq fi:lisp-listener-mode-super-key-map (make-keymap))
	(fi::subprocess-mode-super-keys
	 fi:lisp-listener-mode-super-key-map 'tcp-lisp)))
  (if (null fi:lisp-listener-mode-map)
      (setq fi:lisp-listener-mode-map
	(fi::lisp-listener-mode-commands
	 (make-keymap)
	 fi:lisp-listener-mode-super-key-map)))
  (use-local-map fi:lisp-listener-mode-map)
  (setq fi:subprocess-super-key-map fi:lisp-listener-mode-super-key-map)
  (setq fi:lisp-indent-hook-property 'fi:common-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:subprocess-mode-hook
	     'fi:lisp-listener-mode-hook))

(defun fi:common-lisp-mode (&optional mode-hook)
  "Major mode for editing Lisp code to run in Common Lisp.
The keymap for this mode is bound to fi:common-lisp-mode-map:
\\{fi:common-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:common-lisp-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:common-lisp-mode)
  (setq mode-name "Common Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-edit-mode-setup)
  (fi:parse-mode-line-and-package)
  (if (null fi:common-lisp-mode-map)
      (progn
	(setq fi:common-lisp-mode-map (make-keymap))
	(fi::lisp-mode-commands fi:common-lisp-mode-map nil nil)))
  (use-local-map fi:common-lisp-mode-map)
  (setq fi::process-name fi::common-lisp-backdoor-main-process-name)
  (setq fi:lisp-indent-hook-property 'fi:common-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:common-lisp-mode-hook))

(defun lisp-mode (&optional mode-hook)
  "See fi:common-lisp-mode.  This function is here so that set-auto-mode
will go into the FI Common Lisp mode when ``mode: lisp'' appears in
the file modeline."
  (interactive)
  (fi:common-lisp-mode mode-hook))

(defun common-lisp-mode (&optional mode-hook)
  "See fi:common-lisp-mode.  This function is here so that set-auto-mode
will go into the FI Common Lisp mode when ``mode: common-lisp'' appears in
the file modeline."
  (interactive)
  (fi:common-lisp-mode mode-hook))

(defun fi:franz-lisp-mode (&optional mode-hook)
  "Major mode for editing Lisp code to run in Franz Lisp.
The keymap for this mode is bound to fi:franz-lisp-mode-map:
\\{fi:franz-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:franz-lisp-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-edit-mode-setup)
  (fi:parse-mode-line-and-package)
  (if (null fi:franz-lisp-mode-map)
      (progn
	(setq fi:franz-lisp-mode-map (make-keymap))
	(fi::lisp-mode-commands fi:franz-lisp-mode-map nil nil)))
  (use-local-map fi:franz-lisp-mode-map)
  (setq fi::process-name fi:franz-lisp-process-name)
  (setq fi:lisp-indent-hook-property 'fi:franz-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:franz-lisp-mode-hook))

(defun fi:emacs-lisp-mode (&optional mode-hook)
  "Major mode for editing Lisp code to run in Emacs Lisp.
The keymap for this mode is bound to fi:emacs-lisp-mode-map:
\\{fi:emacs-lisp-mode-map}
Entry to this mode runs the fi:emacs-lisp-mode-hook hook.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (set-syntax-table fi:emacs-lisp-mode-syntax-table)
  (fi::lisp-edit-mode-setup)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-keymap))
	(fi::lisp-mode-commands fi:emacs-lisp-mode-map nil nil)))
  (use-local-map fi:emacs-lisp-mode-map)
  (setq fi:lisp-indent-hook-property 'fi:emacs-lisp-indent-hook)
  (run-hooks 'fi:emacs-lisp-mode-hook))

(defun fi::lisp-edit-mode-setup ()
  (fi::lisp-mode-setup-common))

(defun fi::lisp-subprocess-mode-variables ()
  (make-local-variable 'fi:subprocess-mode)
  (setq fi:subprocess-mode t)
  (fi::lisp-mode-setup-common))

(defun fi::lisp-mode-setup-common ()
  ;; not needed for Emacs Lisp mode, but ...
  (setq fi:in-package-regexp fi:default-in-package-regexp)

  (setq local-abbrev-table lisp-mode-abbrev-table)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)

  (if fi:lisp-do-indentation
      (progn
	(setq fill-column 75)
	(auto-fill-mode 1)
	
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'fi:fill-paragraph)

	(make-local-variable 'auto-fill-function)
	(setq auto-fill-function 'fi::do-auto-fill)
  
	(make-local-variable 'indent-line-function)
	(setq indent-line-function 'fi:lisp-indent-line)

	(make-local-variable 'comment-indent-hook)
	(setq comment-indent-hook 'fi:lisp-comment-indent)

	(make-local-variable 'parse-sexp-ignore-comments)
	;; It used to be true that this variable must be `nil' when
	;; comments end in newlines.  However, it seems this limitation was
	;; removed some time around the start of 1994, so now we'll try
	;; setting it true. - smh 29jun95
	(setq parse-sexp-ignore-comments t)

	(setq fi::lisp-most-recent-parse-result (list 0 0 0 0 nil nil nil 0))
	(setq fi::calculate-lisp-indent-state-temp (list 0 0 0 nil nil nil 0))
	(setq fi::lisp-indent-state-temp (list nil nil nil nil nil nil nil)))
    ;; the GNU style
    (lisp-mode-variables t))
  
  (when fi::menubar-initialization (funcall fi::menubar-initialization)))

(defvar fi:default-package
    "user"
  "*The name of the package to use as the default package, if there is no
package specification in the mode line.  See fi:parse-mode-line-and-package
for more information.")

(defun fi:parse-mode-line-and-package ()
  "Determine the current package in which the buffer is defined.
The buffer's IN-PACKAGE form and the -*- mode line are parsed for this
information.  This function is automatically called when a Common Lisp
source file is visited, but may be executed explicitly to re-parse the
package.

When using Allegro CL 4.2 or later, the ``Readtable: '' can be used to name
the readtable used for evaluations given to Lisp from emacs."
  (interactive)
  (setq fi:readtable (fi::parse-mode-line "readtable"))
  (setq fi:package
    (fi::parse-mode-line "package" fi:default-package t
			 'fi::parse-package-from-buffer t)))

(defun fi::parse-mode-line (key
			    &optional default-value messagep fail-hook
				      list-value-ok)
  (save-excursion
    (let ((case-fold-search t)
	  (search-string (format "%s:" key))
	  value found start end)
      (goto-char (point-min))
      (when (and (search-forward "-*-"
				 (save-excursion (end-of-line) (point))
				 t)
		 (progn
		   (skip-chars-forward " \t")
		   (setq start (point))
		   (search-forward "-*-"
				   (save-excursion (end-of-line) (point))
				   t)))
	(forward-char -3)
	(skip-chars-backward " \t")
	(setq end (point))
	(goto-char start)

	(when (search-forward ":" end t)
	  (goto-char start)
	  (when (search-forward search-string end t)
	    (skip-chars-forward " \t")
	    (setq start (point))
	    (if (> start end) (setq end start))
	    (if (search-forward ";" end t)
		(forward-char -1)
	      (goto-char end))
	    (skip-chars-backward " \t")
	    (cond
	     ((>= start (point))
	      (setq value default-value))
	     (t
	      (let ((val
		     (if (and list-value-ok
			      (= (string-to-char "(")
				 (char-after start)))
			 (buffer-substring (+ 1 start)
					   (point))
		       (buffer-substring start (point)))))
		(setq found t)
		(setq value
		  (downcase
		   (symbol-name (car (read-from-string val)))))))))))
      (when (and (not found) fail-hook)
	(goto-char (point-min))
	(let ((val (funcall fail-hook)))
	  (when val
	    (setq found t value val))))
      (unless found (setq value default-value))
      (when messagep
	(if found
	    (message "%s specification is `%s'" key value)
	  (message "using default %s specification of `%s'"
		   key value)))
      value)))

(defun fi::parse-package-from-buffer ()
  (goto-char (point-min))
  (let ((pos (re-search-forward "^(in-package[\t ]*#?" nil t))
	value)
    ;; find the `in-package' form, and snarf the package
    ;; that way
    (when pos
      (let* ((start (match-end 0))
	     (end (progn (search-forward ")" nil t)
			 (match-beginning 0)))
	     (p-string (buffer-substring start end))
	     (p (car (read-from-string p-string))))
	(setq value
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
    value))

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

(fi::def-auto-mode "\\.l$" 'fi:franz-lisp-mode)
;;
(let ((list fi:common-lisp-file-types))
  (while list
    (fi::def-auto-mode (concat "\\" (car list) "$")
	'fi:common-lisp-mode)
    (setq list (cdr list))))

(defvar fi:define-emacs-lisp-mode t
  "*If non-nil, then use the FI supplied mode for editing .el files.")

(when fi:define-emacs-lisp-mode
  (fi::def-auto-mode "\\.el$" 'fi:emacs-lisp-mode)
  (fi::def-auto-mode "[]>:/]\\..*emacs" 'fi:emacs-lisp-mode))

;;;; the syntax tables for Lisp and Emacs Lisp

(if (not fi:emacs-lisp-mode-syntax-table)
    (let ((i 0))
      (setq fi:emacs-lisp-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " fi:emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " fi:emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " fi:emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " fi:emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\t "    " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\f ">   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?` "'   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?' "'   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?, "'   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?. "'   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?# "'   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " fi:emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " fi:emacs-lisp-mode-syntax-table)))

(if (not fi:lisp-mode-syntax-table)
    (progn
      (setq fi:lisp-mode-syntax-table
	(copy-syntax-table fi:emacs-lisp-mode-syntax-table))
      ;;(modify-syntax-entry ?_   "w   " fi:lisp-mode-syntax-table)
      ;;(modify-syntax-entry ?-   "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?*   "w   " fi:lisp-mode-syntax-table)
      ;; The next syntax entry doesn't work with these forms:
      ;;  `,.foo
      ;;  #.foo
      ;; but it works better with variables with .'s in them
      (modify-syntax-entry ?. "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\| "\"   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\[ "_   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\] "_   " fi:lisp-mode-syntax-table)))
