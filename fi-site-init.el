;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.49 1993/09/10 17:41:44 layer Exp $
;;
;; The Franz Inc. Lisp/Emacs interface.

(setq fi:emacs-lisp-interface-version "2.0.9")
(defvar fi::required-ipc-version 1)
(defvar fi::load-subprocess-files t)
(defvar fi::build-time nil)
(defvar fi::emacs-type nil)

(load "fi-version.el")

(require 'cl)

(when (or (eq fi::emacs-type 'emacs19) (eq fi::emacs-type 'lemacs19))
  ;; needed for setf expanations (on some version 19.xx) when they are
  ;; compiled when non-version 19 
  ;; byte-compilers
  (condition-case nil
      (require 'cl-compat)
    (error nil)))

(defvar fi::initialization-forms nil)
(defun fi::initialize-emacs-lisp-interface ()
  (when fi::initialization-forms
    (dolist (form (nreverse fi::initialization-forms))
      (eval form))
    (setq fi::initialization-forms)))

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
