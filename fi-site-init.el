;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.12 1989/08/28 20:20:32 layer Rel $
;;
;; The Franz Inc. Lisp/Emacs interface:
;;
(load "fi/modes.elc")
(if fi:lisp-do-indentation		; default is `t'
    (load "fi/indent.elc"))
(load "fi/keys.elc")
(load "fi/subproc.elc")
(load "fi/sublisp.elc")
(load "fi/tcplisp.elc")
(load "fi/ltags.elc")
(load "fi/ring.elc")
(load "fi/filec.elc")
(load "fi/utils.elc")

;; `shell' and `rlogin' modes:
(load "fi/shell.elc")
(load "fi/rlogin.elc")

(autoload 'fi:clman         "fi/clman" nil t)
(autoload 'fi:clman-mode    "fi/clman" nil t)
(autoload 'fi:clman-apropos "fi/clman" nil t)

(setq fi:package-loaded t)
