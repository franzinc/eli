;; Sample .emacs file
;;
;; $Id: emacs.el,v 1.5 1997/12/11 00:28:32 layer Exp $

(defvar *eli-directory*)
(setq *eli-directory* (expand-file-name "~/cl-ultra/src/eli/"))

(when (and (not (string-match "xemacs" emacs-version))
	   (= emacs-major-version 20)
	   (<= emacs-minor-version 2))
  (setq load-path
    (cons *eli-directory* load-path)))

(load (format "%sfi-site-init" *eli-directory*))

(setq fi:common-lisp-image-name "/usr/fi/cl-4.3.1")
(setq fi:common-lisp-host "ultra")

;; This function starts up lisp with your defaults.
(defun run-common-lisp ()
  (interactive)
  (fi:common-lisp fi:common-lisp-buffer-name
		  fi:common-lisp-directory
		  fi:common-lisp-image-name
		  fi:common-lisp-image-arguments
		  fi:common-lisp-host))

;; Set up a keybinding for mycl.
(setq ctlx-3-map (make-keymap))
(define-key ctl-x-map "3" ctlx-3-map)
(define-key ctlx-3-map "l" 'run-common-lisp)

;; If you don't want to do the above, then this binding go to the
;; *common-lisp* buffer, causing the image to be run the first time it is
;; typed. 
(define-key global-map "\C-xl" 'fi:common-lisp)

;; Start up a lisp image.
(run-common-lisp)
