;; $Id: fi-version.el,v 2.8 1997/10/01 21:48:29 layer Exp $

;; This is in a separate file so that it can be loaded at byte-compile time

(defun on-ms-windows ()
  (memq system-type '(windows-nt ms-windows ms-dos win386)))

(defvar *on-windows-nt*
    (and (on-ms-windows)
	 (file-exists-p (format "%s/system32" (getenv "WINDIR")))))

(let ((case-fold-search t))
  (cond ((string-match "xemacs" emacs-version) (setq fi::emacs-type 'xemacs19))
	((string-match "lucid" emacs-version) (setq fi::emacs-type 'xemacs19))
	((string-match "^20\." emacs-version) (setq fi::emacs-type 'emacs20))
	((string-match "^19\." emacs-version) (setq fi::emacs-type 'emacs19))
	((string-match "^18\." emacs-version) (setq fi::emacs-type 'emacs18))
	((boundp 'epoch::version) (setq fi::emacs-type 'epoch))
	(t
	 (error
	  "%s is not supported by this version of the emacs-lisp interface."
	  emacs-version))))
