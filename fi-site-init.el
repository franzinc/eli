;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.36 1993/04/12 16:59:19 layer Exp $
;;
;; The Franz Inc. Lisp/Emacs interface.

(setq fi:emacs-lisp-interface-version "2.0.5")
(defvar fi::required-ipc-version 1)
(defvar fi::load-subprocess-files t)

(require 'cl)

(load "fi/modes.elc")
(when fi:lisp-do-indentation
  (load "fi/indent.elc"))
(load "fi/keys.elc")
(load "fi/utils.elc")

(when fi::load-subprocess-files
  (load "fi/subproc.elc")
  (load "fi/sublisp.elc")
  (load "fi/basic-lep.elc")
  (load "fi/lep.elc")
  (load "fi/dmode.elc")
  (load "fi/composer.elc")
  (load "fi/lze.elc")
  (load "fi/changes.elc")
  (load "fi/db.elc")
  (load "fi/stream.elc")

  (when (boundp 'epoch::version)
    (load "fi/leep0.elc")
    (load "fi/leep.elc"))

  (load "fi/ring.elc")
  (load "fi/filec.elc")

  ;; `shell' and `rlogin' modes:
  (load "fi/shell.elc")
  (load "fi/rlogin.elc")
  (load "fi/telnet.elc")
  (load "fi/su.elc"))

(autoload 'fi:clman         "fi/clman" nil t)
(autoload 'fi:clman-mode    "fi/clman" nil t)
(autoload 'fi:clman-apropos "fi/clman" nil t)

(setq fi:package-loaded t)
