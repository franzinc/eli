;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.28 1991/09/10 11:19:36 layer Exp $
;;
;; The Franz Inc. Lisp/Emacs interface.

(setq fi:emacs-lisp-interface-version "2.0")

(require 'cl)

(load "fi/modes.elc")
(if fi:lisp-do-indentation		; default is `t'
    (load "fi/indent.elc"))
(load "fi/keys.elc")
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
(load "fi/ring.elc")
(load "fi/filec.elc")
(load "fi/utils.elc")

(if (boundp 'epoch::version)
    (progn
      (load "fi/leep0.elc")
      (load "fi/leep.elc")))

;; `shell' and `rlogin' modes:
(load "fi/shell.elc")
(load "fi/rlogin.elc")
(load "fi/telnet.elc")
(load "fi/su.elc")

(autoload 'fi:clman         "fi/clman" nil t)
(autoload 'fi:clman-mode    "fi/clman" nil t)
(autoload 'fi:clman-apropos "fi/clman" nil t)

(setq fi:package-loaded t)
