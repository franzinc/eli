;;
;; copyright (C) 1991 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are the property 
;; of Franz, Inc.  
;;
;; This file (or any derivation of it) may be distributed without 
;; further permission from Franz Inc. as long as:
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, other than a tape
;;	  fee, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this interface, please feel
;; free to contact Franz Inc. at
;;	Franz Inc.
;;	Attn: Kevin Layer
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;;	(415) 548-3600
;; or
;;	emacs-info@franz.com
;;	uunet!franz!emacs-info
;;
;; $Header: /repo/cvs.copy/eli/fi-composer.el,v 1.7 1991/03/15 12:44:01 layer Exp $
;;
;;;;;;;;;;;;;;;;;; Composer 2 related stuff
;;; Perhaps some of this is generally useful
;;; Called to create an editor.
;;; Perhaps on Epoch this should do something different, such as create a new window

(defun composer::make-listener ()
  (fi:open-lisp-listener -1))

(defun composer::edit-file-in-editor ()
  (call-interactively 'find-file))


(defun composer::find-definition-in-editor ()
  (call-interactively 'fi:lisp-find-tag))


;;; XXXX = class, method, generic-function, method-combination
;;; describe-xxx
;;; browse-xxx
;;; edit-class-methods
;;; edit-uses

;; inspecting stuff

(defun fi:inspect-something (something function descr)
  (make-request (composer::inspect-something-session
		 :fspec something :function function)
		;; Normal continuation
		(() ())
		;; Error continuation
		((something) (error)
		 (message "Cannot inspect/browse %s: %s" something error))))

(defun fi:browse-class (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Class name")))
  (inspect-something something 'clos::find-class "Class"))

(defun fi:browse-function (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Function spec")))
  (inspect-something something 'fdefinition "Function"))


(defun fi:inspect-value (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "form ")))
  (inspect-something something 'eval "Inspect"))

;;; show-callers
;;; show-callees


(defun fi:show-calls-to (function)
  (interactive (fi::get-default-symbol "Function"))
  (fi::show-calls function ':parent "Could not show calls to %s"))

(defun fi:show-calls-from (function)
  (interactive (fi::get-default-symbol "Function"))
  (fi::show-calls function  ':kid "Could not show calls from %s"))

(defun fi::show-calls (function direction msg)
  (make-request (composer::show-calls-session
		 :direction direction :fspec (fi::frob-case-to-lisp function))
		(() () ())
		((msg) (error)
		 (message msg error))))

;;;

(defun show-subclasses (class)
  (interactive (fi::get-default-symbol "Class"))
  (show-subsuper-classes class ':kid "Could not show subclasses: %s"))

(defun show-superclasses (class)
  (interactive (fi::get-default-symbol "Class"))
  (show-subsuper-classes class ':parent "Could not show superclasses: %s"))

(defun show-subsuper-classes (class direction msg)
  (make-request (composer::show-classes-session 
		 :direction direction :fspec (fi::frob-case-to-lisp class))
		(() () ())
		((msg) (error)
		 (message msg error))))
  
;;; Perhaps with Epoch we should create buttons such that mousing invokes.
;;; Should we think about drawing classes.







