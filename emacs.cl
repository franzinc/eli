;;					-[Mon Aug  7 16:55:34 1989 by layer]-
;;
;; The Allegro CL part of the Emacs/Lisp interface
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca. 
;; copyright (c) 1986, 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and stored only in accordance with the terms of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure by the Government are subject to
;; restrictions of Restricted Rights for Commercial Software developed
;; at private expense as specified in DOD FAR 52.227-7013 (c) (1) (ii).
;;

(eval-when (compile)
  (if (find-symbol (symbol-name :rcsnote) (find-package :si))
      (push :has-rcsnote *features*))
  )

#+has-rcsnote
(si::rcsnote "emacs" "$Header: /repo/cvs.copy/eli/Attic/emacs.cl,v 1.2 1989/08/07 16:54:38 layer Rel $")

;; $Locker:  $

(provide :emacs)

(in-package :excl)

(eval-when (compile)

(defmacro do-package-symbols ((var &optional (package '*package*) result-form)
			      &rest forms
			      &environment env)
  (let ((pkg (gensym))
	(p (gensym))
	(body (third (parse-body forms env))))
    `(let ((,pkg ,package))
      (if (not (packagep ,pkg)) (error "non-package to do-symbols: ~s" ,pkg))
      (prog nil
	 (maphash #'(lambda (xx ,var)
		      (declare (ignore xx) (ignore-if-unused ,var))
		      ,@body)
		  (fast (package-internal-symbols ,pkg)))
	 (maphash #'(lambda (xx ,var)
		      (declare (ignore xx) (ignore-if-unused ,var))
		      ,@body)
		  (fast (package-external-symbols ,pkg)))
	 (return ,result-form)))))
)

(defun list-all-completions (string &optional package functions-only)
  "Return a list of all the accessible symbols with a print name starting
with the substring STRING. If PACKAGE is given, then the reference package
is PACKAGE."
  (declare (optimize (speed 3)))
  (let* ((substring (simple-string string))
	 (substring-length (fast (length substring)))
	 (substring-fixnums (apropos-fixnums substring substring-length))
	 result pkg packages)
    (declare (simple-string substring))
    (if* package
       then (setq pkg (or (and (packagep package) package)
			  (fasl-find-package package)))
	    (setq packages nil)
       else (setq pkg *package*)
	    (setq packages (package-use-list pkg)))
    (dolist (p packages)
      (do-external-symbols (symbol p)
	(if (and (or (null functions-only)
		     (fboundp symbol))
		 (list-all-completions-search symbol substring-fixnums
					      substring-length))
	    (push symbol result))))
    (do-package-symbols (symbol pkg)
      (if (and (or (null functions-only)
		   (fboundp symbol))
	       (list-all-completions-search symbol substring-fixnums
					    substring-length))
	  (push symbol result)))
    (delete-duplicates result :test #'eq)))

(defun list-all-completions-search (symbol fixnums flength)
  (declare (optimize (speed 3) (safety 0))
	   (list fixnums)
	   (fixnum flength))
  (let* ((name (symbol-name symbol))
	 (symbol-length (the fixnum (length name))))
    (declare (type fixnum symbol-length))
    (if (< symbol-length flength)
	(return-from list-all-completions-search nil))
    (do* ((index 0 (the fixnum (1+ index)))
	  (fixnums fixnums (cdr fixnums)))
	 ((= index flength) t)
      (declare (type simple-string name)
	       (type fixnum index))
      (unless (= (the fixnum (char-code (schar name index)))
		 (the fixnum (car fixnums)))
	(return nil)))))
