;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.62 1994/08/23 01:46:42 smh Exp $
;;
;; The Franz Inc. Lisp/Emacs interface.

(setq fi:emacs-lisp-interface-version "2.0.13")
(defvar fi::required-ipc-version 1)
(defvar fi::load-subprocess-files t)
(defvar fi::build-time nil)
(defvar fi::emacs-type nil)

(load "fi-version.el")

(require 'cl)

(when (or (eq fi::emacs-type 'emacs19) (eq fi::emacs-type 'lemacs19))
  ;; needed for setf expanations (on some version 19.xx) when they are
  ;; compiled with non-version 19 byte-compilers.
  (condition-case nil
      (require 'cl-compat)
    (error nil)))

(defvar fi::initialization-forms nil)
(defun fi::initialize-emacs-lisp-interface ()
  (when fi::initialization-forms
    (dolist (form (nreverse fi::initialization-forms))
      (eval form))
    (setq fi::initialization-forms nil)))

(unless (symbol-function 'add-hook)	;not in 18.59
  (defun add-hook (hook function &optional append)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (or (boundp hook) (set hook nil))
    ;; If the hook value is a single function, turn it into a list.
    (let ((old (symbol-value hook)))
      (if (or (not (listp old)) (eq (car old) 'lambda))
	  (set hook (list old))))
    (or (if (consp function)
	    (member function (symbol-value hook))
	  (memq function (symbol-value hook)))
	(set hook 
	     (if append
		 (nconc (symbol-value hook) (list function))
	       (cons function (symbol-value hook)))))))

(load "fi-modes.elc")
(when fi:lisp-do-indentation
  (load "fi-indent.elc"))
(load "fi-keys.elc")
(load "fi-utils.elc")
(load "fi-gnu.elc")

(when fi::load-subprocess-files
  (load "fi-subproc.elc")
  (load "fi-sublisp.elc")
  (load "fi-basic-lep.elc")
  (load "fi-lep.elc")
  (load "fi-dmode.elc")
  (load "fi-composer.elc")
  (load "fi-lze.elc")
  (load "fi-changes.elc")
  (load "fi-db.elc")
  (load "fi-stream.elc")

  (when (eq fi::emacs-type 'epoch)
    (load "fi-leep0.elc")
    (load "fi-leep.elc"))

  (when (eq fi::emacs-type 'lemacs19)
    (load "fi-leep0.elc")
    (load "fi-leep-lemacs.elc"))

  (load "fi-ring.elc")
  (load "fi-filec.elc")

  ;; `shell' and `rlogin' modes:
  (load "fi-shell.elc")
  (load "fi-rlogin.elc")
  (load "fi-telnet.elc")
  (load "fi-su.elc"))

(autoload 'fi:clman         "fi-clman" nil t)
(autoload 'fi:clman-mode    "fi-clman" nil t)
(autoload 'fi:clman-apropos "fi-clman" nil t)

(setq fi:package-loaded t)

;; the test for GNU Emacs 19 has to be after that for lemacs, because
;; the version of lemacs is 19.* too!

(when (eq fi::emacs-type 'lemacs19)
  (load "fi-lemacs"))

(when (eq fi::emacs-type 'emacs19)
  (load "fi-emacs19"))

(defun fi::top-level ()
  (fi::initialize-emacs-lisp-interface)
  (eval fi::build-time))

(if fi::build-time
    (progn
      (setq fi::build-time top-level)
      (setq top-level '(fi::top-level)))
  (fi::initialize-emacs-lisp-interface))
