
;; The Franz Inc. Lisp/Emacs interface:
;;
(load "fi/modes.elc")
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

(setq fi:package-loaded t)
