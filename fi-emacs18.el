;; Emacs 18 back compatibility hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1993-1994 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; $Header: /repo/cvs.copy/eli/fi-emacs18.el,v 2.2 1995/01/18 05:04:08 smh Exp $

(defun fi::ensure-buffer-visible (buffer)
  nil)

(defun fi::ensure-minibuffer-visible ()
  nil)

(defun fi::defontify-string (str)
  str)

(defun set-menubar-dirty-flag ()
  nil)
