;; $Header: /repo/cvs.copy/eli/fi-site-init.el,v 1.23 1991/04/20 23:24:20 layer Exp $
;;
;; The Franz Inc. Lisp/Emacs interface:
;;

(defvar fi:use-lep t) ;; for final release this should be removed

(setq fi:emacs-lisp-interface-version "2.0alpha")

(require 'cl)

(load "fi/modes.elc")
(if fi:lisp-do-indentation		; default is `t'
    (load "fi/indent.elc"))
(load "fi/keys.elc")
(load "fi/subproc.elc")
(load "fi/sublisp.elc")
(load "fi/tcplisp.elc")
(cond (fi:use-lep (load "fi/lep/lep-init"))
      (t (load "fi/nonlep/query.elc")
	 (load "fi/nonlep/ltags.elc")))
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

(defvar fi::lep-loaded)
(setq fi::lep-loaded fi:use-lep)

(defun fi:toggle-lep ()
  (interactive)
  (if fi::lep-loaded
      (progn
	(message "Loading non-LEP version")
	(load "fi/nonlep/query.elc")
	(load "fi/nonlep/ltags.elc")
	(load "fi/keys.elc")
	(setq fi::lep-loaded nil))
    (message "Loading LEP version")
    (load "fi/lep/lep-init")
    ;; fi::lep-loaded set in lep-init
    ))

(setq fi:package-loaded t)
