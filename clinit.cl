;; $Header: /repo/cvs.copy/eli/Attic/clinit.cl,v 1.2 1988/11/17 15:23:24 layer Exp $

#|

;; use one of the following in your .clinit.cl file: 

(defparameter *emacs-library*
  (let ((emacs-lib (si:getenv "EMACSLIBRARY")))
    (if emacs-lib (format nil "~a/lisp/fi/" emacs-lib))))

#+ipc-method1
(and *emacs-library*
     (load (merge-pathnames "clinit.cl" *emacs-library*))
     (load-and-start-ipc-package))

#+ipc-method2
(and *emacs-library*
     (find "+ipc" (system:command-line-arguments) :test #'string=)
     (load (merge-pathnames "clinit.cl" *emacs-library*))
     (load-and-start-ipc-package))

#+ipc-method3
(tpl:alias "listen" ()
  (and *emacs-library*
       (load (merge-pathnames "clinit.cl" *emacs-library*))
       (load-and-start-ipc-package)))

|#

(defmacro svalue (package symbol)
  `(let ((p ,package) (s ,symbol))
     (values (find-symbol (symbol-name s) p))))

(defun load-and-start-ipc-package ()
  (excl::handler-case 
   (let ((emacs-library
	  (format nil "~a/lisp/fi/" (si:getenv "EMACSLIBRARY"))))
     (if* emacs-library
	then (push (make-pathname :directory emacs-library :type "fasl")
		   si:*require-search-list*)
	     (require :ipc)
	     (require :emacs)
	     (ff:defforeign 'getuid)
	     (set (svalue :ipc :lisp-listener-daemon-ff-loaded) nil)
	     (let ((s (svalue :ipc :*socket-pathname*))
		   (socket-file (format nil "/tmp/~a_emacs_to_acl"
					(excl::userid-to-name (getuid)))))
	       (if (boundp s) (set s socket-file))
	       (funcall (svalue :ipc :start-lisp-listener-daemon)))
	else (format t "~%EMACSLIBRARY is not in the environment~%")))
   (error (condition) (format t "error during ipc initialization: ~a"
			      condition))))
