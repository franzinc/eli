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
;; $Header: /repo/cvs.copy/eli/fi-composer.el,v 1.3 1991/02/12 17:17:52 layer Exp $
;;
;;;;;;;;;;;;;;;;;; Composer 2 related stuff
;;; Perhaps some of this is generally useful
;;; Called to create an editor.
;;; Perhaps on Epoch this should do something different, such as create a new window

(defun composer::make-listener ()
  (fi:tcp-common-lisp -1))

(defun composer::edit-file-in-editor ()
  (call-interactively 'find-file))


(defun composer::find-definition-in-editor ()
  (call-interactively 'fi:lisp-find-tag))


;;; XXXX = class, method, generic-function, method-combination
;;; describe-xxx
;;; browse-xxx

(defun edit-generic-function-methods (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Generic Function Name")))
  (edit-somethings something 
		   'lep::generic-function-methods-function-specs
		   "Generic Function Methods"))

(defun edit-callers (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Function name")))
  (edit-somethings something 
		   'lep::who-calls
		   "Who calls"))

(defun edit-callees (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Function name")))
  (edit-somethings something 
		   'lep::who-is-called-by
		   "Who is called by"))

;;; edit-class-methods
;;; edit-uses

;; inspecting stuff

(defun inspect-something (something function descr)
  (make-request (composer::inspect-something-session
		 :fspec something :function function)
		;; Normal continuation
		(() ())
		;; Error continuation
		((something) (error)
		 (message "Cannot inspect/browse %s: %s" something error))))

(defun browse-class (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Class name")))
  (inspect-something something 'clos::find-class "Class"))

(defun browse-function (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Function spec")))
  (inspect-something something 'fdefinition "Function"))


(defun inspect-value (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "form ")))
  (inspect-something something 'eval "Inspect"))

;;; describing something

(defun describe-something (something function descr)
  (make-request (lep::describe-something-session
		 :fspec something :function function)
		;; Normal continuation
		(() (what)
		 (show-some-short-text what))
		;; Error continuation
		((something) (error)
		 (message "Cannot describe %s: %s" something error))))

(defun lep::describe-class (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Class name")))
  (describe-something something 'clos::find-class "Class"))


(defun lep::describe-function (something)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Function spec")))
  (describe-something something 'fdefinition "Function"))

;; describe-method
;; describe-method-combination

;;; List callers

(defun list-callers (symbol)
  (interactive (fi::get-default-symbol "Find references to symbol"))
  (fi:lisp-who-calls symbol))

;;; list-callees
;;; list-class-methods
;;; list-generic-function-methods
;; .... 


;;; show-callers
;;; show-callees


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







