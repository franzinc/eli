;; lemacs specific hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; $Header: /repo/cvs.copy/eli/Attic/fi-lemacs.el,v 2.1 1993/07/13 18:55:01 layer Exp $

;; does lemacs have dialogs?

(defconst fi:global-menubar
    '("Allegro"
      ["Run Allegro CL" fi:common-lisp t]
      ["Run and start Allegro Composer" fi:xxxxx t]
      ;; get a listener
      ))

(defconst fi:global-composer-menubar
    '("Allegro Composer"
      ;; all the mouse line stuff should be duplicated here
      ;; when fi:common-lisp starts up it should automatically cause this
      ;; menubar to be added globally
      ))

(defconst fi:common-lisp-mode-menubar
    '("Allegro"
      ;; editing things...
      ["foo" fi:xxxx t]
      ))

(defconst fi:inferior-common-lisp-mode-menubar
    '("Allegro"
      ;; subprocess things...
      ["foo" fi:xxxx t]
      ))

(defconst fi:common-lisp-common-menubar
    '("Allegro"
      ;; common to inferior and edit modes
      ))

(defconst fi:clim-menubar
    '("Allegro CLIM"
      ))

(defun fi::install-menubar (menu-bar)
  (set-menubar (delete (assoc (car menu-bar) current-menubar)
		       (copy-sequence current-menubar)))
  (add-menu nil (car menu-bar) (cdr menu-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can't do the following when fi::build-time is non-nil!
(fi::install-menubar fi:global-menubar)
