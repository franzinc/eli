;; Emacs 18 back compatibility hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1993-2001 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; $Id: fi-emacs18.el,v 2.3.44.1 2001/06/05 19:07:11 layer Exp $

(defun fi::ensure-buffer-visible (buffer)
  nil)

(defun fi::ensure-minibuffer-visible ()
  nil)

(defun fi::defontify-string (str)
  str)

(defun set-menubar-dirty-flag ()
  nil)
