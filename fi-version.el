;; $Header: /repo/cvs.copy/eli/Attic/fi-version.el,v 2.2 1995/01/10 00:43:47 smh Exp $

;; This is in a separate file so that it can be loaded at byte-compile time

(cond ((string-match "[Xx][Ee]macs" emacs-version) (setq fi::emacs-type 'xemacs19))
      ((string-match "^19\." emacs-version) (setq fi::emacs-type 'emacs19))
      ((string-match "^18\." emacs-version) (setq fi::emacs-type 'emacs18))
      ((boundp 'epoch::version) (setq fi::emacs-type 'epoch))
      (t
       (error
	"%s is not supported by this version of the emacs-lisp interface."
	emacs-version)))
