;; $Header: /repo/cvs.copy/eli/Attic/fi-version.el,v 2.4 1996/05/15 23:31:38 layer Exp $

;; This is in a separate file so that it can be loaded at byte-compile time

(defun on-ms-windows ()
  (memq system-type '(ms-windows ms-dos win386)))

(let ((case-fold-search t))
  (cond ((string-match "xemacs" emacs-version) (setq fi::emacs-type 'xemacs19))
	((string-match "lucid" emacs-version) (setq fi::emacs-type 'xemacs19))
	((string-match "^19\." emacs-version) (setq fi::emacs-type 'emacs19))
	((string-match "^18\." emacs-version) (setq fi::emacs-type 'emacs18))
	((boundp 'epoch::version) (setq fi::emacs-type 'epoch))
	(t
	 (error
	  "%s is not supported by this version of the emacs-lisp interface."
	  emacs-version))))
