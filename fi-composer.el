;; Copyright (c) 1987-1991 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-composer.el,v 1.13 1991/09/30 11:39:17 layer Exp $

(defun composer::make-listener (new-screen-p)
  (when (and new-screen-p (fboundp 'create-screen))
    (let ((screen (create-screen "*listener*" epoch::screen-properties)))
	   (epoch::map-screen screen)
	   (epoch::select-screen screen)
	   screen))
  (fi:open-lisp-listener -1))

(defun composer::edit-file-in-editor ()
  (call-interactively 'find-file))


(defun composer::find-definition-in-editor ()
  (call-interactively 'fi:lisp-find-definition))


;;;;Todo:
;;;; fi:inspect-method
;;;; fi:inspect-generic-function
;;;; fi:inspect-method-combination

(defun fi:inspect-class (something)
  "Inspect, using the grapher, a class object."
  (interactive (fi::get-default-symbol "Class name"))
  (fi::inspect-something something 'clos::find-class "Class"))

(defun fi:inspect-function (something)
  "Inspect, using the grapher, a function object."
  (interactive (fi::get-default-symbol "Function name"))
  (fi::inspect-something something 'fdefinition "Function"))

(defun fi:inspect-value (something)
  "Inspect, using the grapher, an arbitrary Lisp object."
  (interactive (fi::get-default-symbol "Value to inspect"))
  (fi::inspect-something something 'eval "Inspect"))

(defun fi::inspect-something (something function descr)
  (fi::make-request
   (composer::inspect-something-session :fspec something :function function)
   ;; Normal continuation
   (() ())
   ;; Error continuation
   ((something) (error)
    (message "Cannot inspect %s: %s" something error))))

;;;Todo?
;;; show-callers
;;; show-callees

(defun fi:show-calls-to (function)
  "Show a graph of the calls to FUNCTION."
  (interactive (fi::get-default-symbol "Function"))
  (fi::show-calls function ':parent "Could not show calls to %s"))

(defun fi:show-calls-from (function)
  "Show a graph of the calls from FUNCTION."
  (interactive (fi::get-default-symbol "Function"))
  (fi::show-calls function  ':kid "Could not show calls from %s"))

(defun fi::show-calls (function direction msg)
  (fi::make-request
   (composer::show-calls-session
    :direction direction :fspec (fi::frob-case-to-lisp function))
   (() () ())
   ((msg) (error)
    (message msg error))))

;;;

(defun fi:show-subclasses (class)
  "Show a graph of the subclasses of CLASS."
  (interactive (fi::get-default-symbol "Subclasses of class"))
  (fi::show-subsuper-classes class ':kid "Could not show subclasses: %s"))

(defun fi:show-superclasses (class)
  "Show a graph of the superclasses of CLASS."
  (interactive (fi::get-default-symbol "Superclasses of class"))
  (fi::show-subsuper-classes class ':parent "Could not show superclasses: %s"))

(defun fi::show-subsuper-classes (class direction msg)
  (fi::make-request
   (composer::show-classes-session 
    :direction direction :fspec (fi::frob-case-to-lisp class))
   (() () ())
   ((msg) (error)
    (message msg error))))
  
;;; Perhaps with Epoch we should create buttons such that mousing invokes.
;;; Should we think about drawing classes.

