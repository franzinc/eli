;; $Header: /repo/cvs.copy/eli/Attic/fi-version.el,v 2.1 1993/08/31 23:26:14 layer Exp $

;; This is in a separate file so that it can be loaded at byte-compile time

(cond ((string-match "ucid" emacs-version) (setq fi::emacs-type 'lemacs19))
      ((string-match "^19\." emacs-version) (setq fi::emacs-type 'emacs19))
      ((string-match "^18\." emacs-version) (setq fi::emacs-type 'emacs18))
      ((boundp 'epoch::version) (setq fi::emacs-type 'epoch))
      (t
       (error
	"%s is not supported by this version of the emacs-lisp interface."
	emacs-version)))
