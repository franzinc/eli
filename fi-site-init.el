;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.24 1991/04/22 13:40:38 layer Exp $
;;
;; The Franz Inc. Lisp/Emacs interface:
;;

(setq fi:emacs-lisp-interface-version "2.0alpha")

(require 'cl)

(load "fi/modes.elc")
(if fi:lisp-do-indentation		; default is `t'
    (load "fi/indent.elc"))
(load "fi/keys.elc")
(load "fi/subproc.elc")
(load "fi/sublisp.elc")
(load "fi/lep/lep-init")
(load "fi/ring.elc")
(load "fi/filec.elc")
(load "fi/utils.elc")

;; `shell' and `rlogin' modes:
(load "fi/shell.elc")
(load "fi/rlogin.elc")
(load "fi/telnet.elc")
(load "fi/su.elc")

(autoload 'fi:clman         "fi/clman" nil t)
(autoload 'fi:clman-mode    "fi/clman" nil t)
(autoload 'fi:clman-apropos "fi/clman" nil t)

(setq fi:package-loaded t)
