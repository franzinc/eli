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

;; $Header: /repo/cvs.copy/eli/fi-modes.el,v 1.20 1988/04/26 22:25:21 layer Exp $

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

(defvar fi:tcp-lisp-mode-map nil
  "The tcp-lisp major-mode keymap.")
(defvar fi:tcp-lisp-mode-super-key-map nil
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

(defun fi:tcp-lisp-mode ()
  "Major mode for interacting with a Common Lisp over a TCP/IP socket."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:tcp-lisp-mode)
  (setq mode-name "TCP Lisp")
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (fi::lisp-subprocess-mode-variables)
  (if (null fi:tcp-lisp-mode-super-key-map)
      (progn
	(setq fi:tcp-lisp-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys
	 fi:tcp-lisp-mode-super-key-map 'tcp-lisp)))
  (if (null fi:tcp-lisp-mode-map)
      (setq fi:tcp-lisp-mode-map
	(fi::tcp-lisp-mode-commands (make-sparse-keymap)
				    fi:tcp-lisp-mode-super-key-map)))
  (use-local-map fi:tcp-lisp-mode-map)
  (setq fi:subprocess-super-key-map fi:tcp-lisp-mode-super-key-map)

  (run-hooks 'fi:indent-setup-hook 'fi:lisp-mode-hook
	     'fi:subprocess-mode-hook 'fi:tcp-lisp-mode-hook))

(defun common-lisp-mode ()
  "Same as fi:common-lisp-mode, for -*- mode definitions."
  (fi:common-lisp-mode))

(defun lisp-mode ()
  "Same as fi:common-lisp-mode, for -*- mode definitions."
  (fi:common-lisp-mode))

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
  (interactive)
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
			     (setq fi:package
			       (buffer-substring beg (point)))))))
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
      (setq fi:package "user")))

;;;;
;;; Key defs
;;;;

(defun fi::subprocess-mode-super-keys (map mode)
  "Setup keys in MAP as a subprocess super-key map.  MODE is either
shell, rlogin, sub-lisp or tcp-lisp."
  (define-key map "\C-a" 'fi:subprocess-beginning-of-line)
  (define-key map "\C-k" 'fi:subprocess-kill-output)
  (define-key map "\C-l" 'fi:list-input-ring)
  (define-key map "\C-n" 'fi:push-input)
  (define-key map "\C-o" 'fi:subprocess-send-flush)
  (define-key map "\C-p" 'fi:pop-input)
  (define-key map "\C-r" 'fi:re-search-backward-input)
  (define-key map "\C-s" 'fi:re-search-forward-input)
  (define-key map "\C-u" 'fi:subprocess-kill-input)
  (define-key map "\C-v" 'fi:subprocess-show-output)
  (define-key map "\C-w" 'fi:subprocess-backward-kill-word)

  (cond
    ((memq mode '(sub-lisp shell))
     (if (eq mode 'shell)
	 (define-key map "\C-z"	'fi:subprocess-suspend))
     (define-key map "\C-c"	'fi:subprocess-interrupt)
     (define-key map "\C-d"	'fi:subprocess-send-eof)
     (define-key map "\C-\\"	'fi:subprocess-quit))
    ((eq mode 'tcp-lisp)
     (define-key map "\C-c"	'fi:tcp-lisp-interrupt-process)
     (define-key map "\C-d"	'fi:tcp-lisp-send-eof)
     (define-key map "\C-\\"	'fi:tcp-lisp-kill-process)))
  map)

(defun fi::subprocess-mode-commands (map supermap mode)
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
  (if supermap (define-key map "\C-c" supermap))
  map)

(defun fi::lisp-mode-commands (map supermap mode)
  (define-key map "\e" (make-sparse-keymap))
  (define-key map "\C-x" (make-sparse-keymap))

  (if supermap (define-key map "\C-c" supermap))
  
  (define-key map "\e\C-q"	'indent-sexp)
  (define-key map "\C-?"	'backward-delete-char-untabify)
  
  (cond
    ((memq mode '(sub-lisp tcp-lisp))
     (define-key map "\r"	'fi:inferior-lisp-newline))
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
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'tcp-lisp)
			  supermap
			  'tcp-lisp))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'sub-lisp)
			  supermap
			  'sub-lisp))

(defun fi:lisp-reindent-newline-indent ()
  "Indent the current line, insert a newline and indent to the proper
column."
  (interactive)
  (save-excursion (funcall indent-line-function))
  (newline)
  (funcall indent-line-function))

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

(defun fi::lisp-indent-do (path state indent-point sexp-column normal-indent)
  "A better do indenter than the standard GNU has."
  (if (>= (car path) 3)
      (let ((lisp-tag-body-indentation lisp-body-indent))
        (funcall (function lisp-indent-tagbody)
		 path state indent-point sexp-column normal-indent))
    (funcall (function lisp-indent-259)
	     '((&whole nil &rest)
	       (&whole nil &rest 1))
	     path state indent-point sexp-column normal-indent)))

(defun fi::lisp-indent-if* (path state indent-point sexp-column normal-indent)
  "An excl:if* indentation hook."
  (let ((lisp-if-then-else-indent 3)
	(lisp-if-elseif-indent 1))
    (if (not (null (cdr path)))
	normal-indent
      (save-excursion
	(goto-char indent-point)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(list (cond ((looking-at "elseif")
		     (+ sexp-column lisp-if-elseif-indent))
		    ((looking-at "then\\|else")
		     (+ sexp-column lisp-if-then-else-indent))
		    (t (+ sexp-column
			  (+ lisp-if-then-else-indent 5))))
	      (elt state 1))))))

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

(let ((l '((block 1)
	   (catch 1)
           (case (4 &rest (&whole 2 &rest 1)))
           (ccase . case)
	   (ecase . case)
           (typecase . case)
	   (etypecase . case)
	   (ctypecase . case)
           (catch 1)
	   (concatenate 1)
           (cond (&rest (&whole 2 &rest 1)))
           (block 1)
           (defvar (4 2 2))
           (defconstant . defvar)
	   (defparameter . defvar)
           (define-modify-macro (4 &body))
           (define-setf-method (4 (&whole 4 &rest 1) &body))
           (defsetf (4 (&whole 4 &rest 1) 4 &body))
           (defun (4 (&whole 4 &rest 1) &body))
           (defmacro . defun) (deftype . defun)
           (defstruct ((&whole 4 &rest (&whole 2 &rest 1))
			&rest (&whole 2 &rest 1)))
           (destructuring-bind ((&whole 6 &rest 1) 4 &body))
           (do fi::lisp-indent-do)
           (do* . do)
           (dolist ((&whole 4 2 1) &body))
           (dotimes . dolist)
           (eval-when 1)
           (flet ((&whole 4 &rest (&whole 1 (&whole 4 &rest 1) &body))
		  &body))
           (labels . flet)
           (macrolet . flet)
           (if (nil nil &body))
 	   ;;(lambda     ((&whole 4 &rest 1) &body))
           (lambda ((&whole 4 &rest 1)
		    &rest lisp-indent-function-lambda-hack))
           (let ((&whole 4 &rest (&whole 1 1 2)) &body))
           (let* . let)
           (locally 1)
 	   ;;(loop ...)
           (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
           (multiple-value-call (4 &body))
           (multiple-value-list 1)
           (multiple-value-prog1 1)
           (multiple-value-setq (4 2))
 	   ;; Combines the worst features of BLOCK, LET and TAGBODY
           (prog ((&whole 4 &rest 1) &rest lisp-indent-tagbody))
           (prog* . prog)
           (prog1 1)
           (prog2 2)
           (progn 0)
           (progv (4 4 &body))
	   (setq 1)
	   (setf 1)
           (return 0)
           (return-from (nil &body))
           (tagbody lisp-indent-tagbody)
           (throw 1)
           (unless 1)
           (unwind-protect (5 &body))
           (when 1)
	   (with-input-from-string 1)
           (with-open-file 1)
	   (if* fi::lisp-indent-if*))))
  (while l
    (put (car (car l)) 'common-lisp-indent-hook
         (if (symbolp (cdr (car l)))
             (get (cdr (car l)) 'common-lisp-indent-hook)
	   (car (cdr (car l)))))
    (setq l (cdr l))))

(setq lisp-indent-hook 'common-lisp-indent-hook)
