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

;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.24 1988/05/12 22:57:30 layer Exp $

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

(defvar fi:tcp-common-lisp-mode-map nil
  "The tcp-lisp major-mode keymap.")
(defvar fi:tcp-common-lisp-mode-super-key-map nil
  "Used for super-key processing in tcp-lisp mode.")

(defvar fi:common-lisp-mode-map nil
  "Major mode map used when editing Common Lisp source.")
(defvar fi:franz-lisp-mode-map nil
  "Major mode map used when editing Franz Lisp source.")

(defvar fi:indent-setup-hook nil
  "Hook called to setup local indentation in Inferior Lisp and Lisp
modes.")

;;;;
;;; The Modes
;;;;

(defun fi:inferior-common-lisp-mode ()
  "Major mode for interacting with an inferior Common Lisp subprocess."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:inferior-common-lisp-mode)
  (setq mode-name "Inferior Common Lisp")
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
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
  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook 'fi:inferior-common-lisp-mode-hook))

(defun fi:inferior-franz-lisp-mode ()
  "Major mode for interacting with an inferior Franz Lisp subprocess."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:inferior-franz-lisp-mode)
  (setq mode-name "Inferior Franz Lisp")
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
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
  
  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook 'fi:inferior-franz-lisp-mode-hook))

(defun fi:tcp-common-lisp-mode ()
  "Major mode for interacting with a Common Lisp over a TCP/IP socket."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:tcp-common-lisp-mode)
  (setq mode-name "TCP Common Lisp")
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:tcp-common-lisp-mode-super-key-map)
      (progn
	(setq fi:tcp-common-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys
	 fi:tcp-common-lisp-mode-super-key-map 'tcp-lisp)))
  (if (null fi:tcp-common-lisp-mode-map)
      (setq fi:tcp-common-lisp-mode-map
	(fi::tcp-common-lisp-mode-commands
	 (make-sparse-keymap) fi:tcp-common-lisp-mode-super-key-map)))
  (use-local-map fi:tcp-common-lisp-mode-map)
  (setq fi:subprocess-super-key-map fi:tcp-common-lisp-mode-super-key-map)

  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook 'fi:tcp-common-lisp-mode-hook))

(defun fi:common-lisp-mode ()
  "Major mode for editing Lisp code to run in Common Lisp.
The bindings are taken from the variable `fi:common-lisp-mode-map'.
Entry to this mode calls the value of `fi:common-lisp-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:common-lisp-mode)
  (setq mode-name "Common Lisp")
  (fi::lisp-edit-mode-setup)
  (fi::check-for-package-info)
  (if (null fi:common-lisp-mode-map)
      (progn
	(setq fi:common-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:common-lisp-mode-map nil nil)))
  (use-local-map fi:common-lisp-mode-map)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-common-sublisp-name)
  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:common-lisp-mode-hook))

(defun fi:franz-lisp-mode ()
  "Major mode for editing Lisp code to run in Franz Lisp.
The bindings are taken from the variable `fi:franz-lisp-mode-map'.
Entry to this mode calls the value of `fi:lisp-mode-hook' and
`fi:franz-lisp-mode-hook', in this order, if their value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:franz-lisp-mode)
  (setq mode-name "Franz Lisp")
  (fi::lisp-edit-mode-setup)
  (fi::check-for-package-info)
  (if (null fi:franz-lisp-mode-map)
      (progn
	(setq fi:franz-lisp-mode-map (make-sparse-keymap))
	(fi::lisp-mode-commands fi:franz-lisp-mode-map nil nil)))
  (use-local-map fi:franz-lisp-mode-map)
  (make-local-variable 'fi::sublisp-name)
  (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:franz-lisp-mode-hook))

(defun fi::lisp-edit-mode-setup ()
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'fi::emacs-to-lisp-transaction-file)
  (make-local-variable 'fi::emacs-to-lisp-transaction-buf)
  (make-local-variable 'fi::emacs-to-lisp-package)
  (fi::lisp-subprocess-mode-variables))

(defun fi::lisp-subprocess-mode-variables ()
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start))

(defun fi::check-for-package-info ()
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
			     (setq fi:package
			       (car (read-from-string
				     (buffer-substring beg (point)))))
			     (setq fi:package
			       (downcase
				(format "%s" (if (consp fi:package)
						 (car fi:package)
					       fi:package))))))))
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

(fi::def-auto-mode "\\.cl$" 'fi:common-lisp-mode)
(fi::def-auto-mode "\\.lisp$" 'fi:common-lisp-mode)
(fi::def-auto-mode "\\.l$" 'fi:franz-lisp-mode)

(setq fi:indent-setup-hook 'fi::indent-setup-hook)

(defun fi::indent-setup-hook ()
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'lisp-comment-indent))
