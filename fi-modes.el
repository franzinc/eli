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

;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.41 1991/03/12 18:29:54 layer Exp $

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
fi:common-lisp-mode.")

(defvar fi:lisp-do-indentation t
  "*When non-nil, do FI-style indentation in Lisp modes.")

(defvar fi:subprocess-mode nil
  "Non-nil when buffer has a subprocess.")

(defvar fi:lisp-mode-hook
    '(lambda ()
      (if (and (boundp 'fi:package) fi:package)
	  (setq mode-line-process '("; package: " fi:package))))
  "*The initial value of this hook, which is run whenever a Lisp mode is
entered, causes the `package' to be displayed in the mode line.  It uses
MODE-LINE-PROCESS, which has no use in non-subprocess buffers.")

;;;;
;;; The Modes
;;;;

(defun fi:inferior-common-lisp-mode (&optional mode-hook &rest mode-hook-args)
  "Major mode for interacting with Common Lisp subprocesses.
The keymap for this mode is bound to fi:inferior-common-lisp-mode-map:
\\{fi:inferior-common-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:subprocess-mode-hook
	fi:inferior-common-lisp-mode-hook

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (apply mode-hook mode-hook-args))
  (setq major-mode 'fi:inferior-common-lisp-mode)
  (setq mode-name "Inferior Common Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:inferior-common-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-common-lisp-mode-super-key-map
	  (make-sparse-keymap))
	(fi::subprocess-mode-super-keys
	 fi:inferior-common-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-common-lisp-mode-map)
      (setq fi:inferior-common-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-sparse-keymap) fi:inferior-common-lisp-mode-super-key-map)))
  (use-local-map fi:inferior-common-lisp-mode-map)
  (setq fi:subprocess-super-key-map fi:inferior-common-lisp-mode-super-key-map)
  (setq fi:lisp-indent-hook-property 'fi:common-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:subprocess-mode-hook
	     'fi:inferior-common-lisp-mode-hook))

(defun fi:inferior-franz-lisp-mode (&optional mode-hook)
  "Major mode for interacting with Franz Lisp subprocesses.
The keymap for this mode is bound to fi:inferior-franz-lisp-mode-map:
\\{fi:inferior-franz-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:subprocess-mode-hook
	fi:inferior-franz-lisp-mode-hook

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:inferior-franz-lisp-mode)
  (setq mode-name "Inferior Franz Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:inferior-franz-lisp-mode-super-key-map)
      (progn
	(setq fi:inferior-franz-lisp-mode-super-key-map
	  (make-sparse-keymap))
	(fi::subprocess-mode-super-keys
	 fi:inferior-franz-lisp-mode-super-key-map 'sub-lisp)))
  (if (null fi:inferior-franz-lisp-mode-map)
      (setq fi:inferior-franz-lisp-mode-map
	(fi::inferior-lisp-mode-commands
	 (make-sparse-keymap) fi:inferior-franz-lisp-mode-super-key-map)))
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

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:lisp-listener-mode)
  (setq mode-name "TCP Common Lisp")
  (set-syntax-table fi:lisp-mode-syntax-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:lisp-listener-mode-super-key-map)
      (progn
	(setq fi:lisp-listener-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys
	 fi:lisp-listener-mode-super-key-map 'tcp-lisp)))
  (if (null fi:lisp-listener-mode-map)
      (setq fi:lisp-listener-mode-map
	(fi::lisp-listener-mode-commands
	 (make-sparse-keymap) fi:lisp-listener-mode-super-key-map)))
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

in the above order."
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
	(setq fi:common-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:common-lisp-mode-map nil nil)))
  (use-local-map fi:common-lisp-mode-map)
  (setq fi::process-name fi:common-lisp-process-name)
  (setq fi:lisp-indent-hook-property 'fi:common-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:common-lisp-mode-hook))

(defun fi:franz-lisp-mode (&optional mode-hook)
  "Major mode for editing Lisp code to run in Franz Lisp.
The keymap for this mode is bound to fi:franz-lisp-mode-map:
\\{fi:franz-lisp-mode-map}
Entry to this mode runs the following hooks:

	fi:lisp-mode-hook
	fi:franz-lisp-mode-hook

in the above order."
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
	(setq fi:franz-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:franz-lisp-mode-map nil nil)))
  (use-local-map fi:franz-lisp-mode-map)
  (setq fi::process-name fi:franz-lisp-process-name)
  (setq fi:lisp-indent-hook-property 'fi:franz-lisp-indent-hook)
  (run-hooks 'fi:lisp-mode-hook 'fi:franz-lisp-mode-hook))

(defun fi:emacs-lisp-mode (&optional mode-hook)
  "Major mode for editing Lisp code to run in Emacs Lisp.
The keymap for this mode is bound to fi:emacs-lisp-mode-map:
\\{fi:emacs-lisp-mode-map}
Entry to this mode runs the fi:emacs-lisp-mode-hook hook."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:emacs-lisp-mode)
  (setq mode-name "Emacs Lisp")
  (set-syntax-table fi:emacs-lisp-mode-syntax-table)
  (fi::lisp-edit-mode-setup)
  (if (null fi:emacs-lisp-mode-map)
      (progn
	(setq fi:emacs-lisp-mode-map (make-sparse-keymap))
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
  
  (make-local-variable 'fi:lisp-comment-indent-specification)
  (setq fi:lisp-comment-indent-specification (list comment-column t nil 0))
  (if fi:lisp-do-indentation
      (progn
	(mapcar
	 'make-local-variable
	 '(indent-line-function comment-indent-hook
	   parse-sexp-ignore-comments fi::lisp-most-recent-parse-result
	   fi::lisp-indent-state-temp fi::calculate-lisp-indent-state-temp))
	
	(setq indent-line-function 'fi:lisp-indent-line)
	(setq comment-indent-hook 'fi:lisp-comment-indent)
	;; This variable must be `nil' when comments end in newlines.
	(setq parse-sexp-ignore-comments nil)
	(setq fi::lisp-most-recent-parse-result (list 0 0 0 0 nil nil nil 0))
	(setq fi::calculate-lisp-indent-state-temp (list 0 0 0 nil nil nil 0))
	(setq fi::lisp-indent-state-temp
	  (list nil nil nil nil nil nil nil)))))

(defun fi:parse-mode-line-and-package ()
  "Determine the current package in which the buffer is defined.
IN-PACKAGE and the -*- mode line are parsed for this information."
  (interactive)
  (save-excursion
    ;; look for -*- ... package: xxx; .... -*-
    (let (beg end)
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
			     (let ((package
				    (if (= (string-to-char "(")
					   (char-after beg))
					(buffer-substring (+ 1 beg) (point))
				      (buffer-substring beg (point)))))
			       (setq fi:package
				 (downcase
				  (symbol-name
				   (car (read-from-string package))))))))))
		 fi:package))
	  fi:package
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
		(setq fi:package
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
			((stringp p) p)))))))))
  (if (or (not (boundp 'fi:package))
	  (null fi:package))
      (progn
	(setq fi:package "user")
	(message "using default package specification of `%s'" fi:package))
    (message "package specification is `%s'" fi:package)))

(defun set-auto-mode ()
  "Select major mode appropriate for current buffer.
May base decision on visited file name (See variable  auto-mode-list)
or on buffer contents (-*- line or local variables spec), but does not look
for the \"mode:\" local variable.  For that, use  hack-local-variables."
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let (beg end mode)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (if (and (search-forward "-*-" (save-excursion (end-of-line) (point)) t)
	       (progn
		 (skip-chars-forward " \t")
		 (setq beg (point))
		 (search-forward "-*-" (save-excursion (end-of-line) (point)) t))
	       (progn
		 (forward-char -3)
		 (skip-chars-backward " \t")
		 (setq end (point))
		 (goto-char beg)
		 (if (search-forward ":" end t)
		     (progn
		       (goto-char beg)
		       (if (let ((case-fold-search t))
			     (search-forward "mode:" end t))
			   (progn
			     (skip-chars-forward " \t")
			     (setq beg (point))
			     (if (search-forward ";" end t)
				 (forward-char -1)
			       (goto-char end))
			     (skip-chars-backward " \t")
			     (setq mode (buffer-substring beg (point))))))
		   (setq mode (buffer-substring beg end)))))
	  (progn
	    (setq mode (downcase mode))
	    (if (or (equal mode "lisp") (equal mode "common-lisp"))
		(setq mode "fi:common-lisp"))
	    (funcall (intern (concat mode "-mode"))))
	(let ((alist auto-mode-alist)
	      (name buffer-file-name))
	  (let ((case-fold-search (eq system-type 'vax-vms)))
	    ;; Remove backup-suffixes from file name.
	    (setq name (file-name-sans-versions name))
	    ;; Find first matching alist entry.
	    (while (and (not mode) alist)
	      (if (string-match (car (car alist)) name)
		  (setq mode (cdr (car alist))))
	      (setq alist (cdr alist))))
	  (if mode (funcall mode)))))))

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
(fi::def-auto-mode "\\.el$" 'fi:emacs-lisp-mode)
(fi::def-auto-mode "[]>:/]\\..*emacs" 'fi:emacs-lisp-mode)

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
      (modify-syntax-entry ?\] ")[  " fi:emacs-lisp-mode-syntax-table)
      ))

(if (not fi:lisp-mode-syntax-table)
    (progn
      (setq fi:lisp-mode-syntax-table
	(copy-syntax-table fi:emacs-lisp-mode-syntax-table))
      ;;(modify-syntax-entry ?_   "w   " fi:lisp-mode-syntax-table)
      ;;(modify-syntax-entry ?-   "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?*   "w   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\| "\"   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\[ "_   " fi:lisp-mode-syntax-table)
      (modify-syntax-entry ?\] "_   " fi:lisp-mode-syntax-table)))
