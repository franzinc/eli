;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-leep0.el,v 1.3 1993/07/23 03:49:00 layer Exp $

;; The epoch side of presentations in a lisp-listener window.

;; This defstruct has to be in a separate file compiled and loaded
;; before the mail file because the cruftly compiler doesn't understand
;; a defstruct at compile time in the same file.

(defstruct presentation
  start
  end
  data
  subpresentation-vector)
