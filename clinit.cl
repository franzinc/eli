;; $Header: /repo/cvs.copy/eli/Attic/clinit.cl,v 1.1 1988/11/17 12:22:21 layer Exp $

(and (find "+ipc" (system:command-line-arguments) :test #'string=)
     (excl::handler-case 
      (let ((emacs-library (format nil "~a/lisp/fi"
				   (si:getenv "EMACSLIBRARY"))))
       (if* emacs-library
	  then (push (make-pathname :directory emacs-library :type "fasl")
		     si:*require-search-list*)
	       (require :ipc)
	       (require :emacs)
	       (set (svalue "ipc" "lisp-listener-daemon-ff-loaded") nil)
	       (let ((s (svalue "ipc" "*socket-pathname*")))
		 (if (boundp s)
		     (set s "/tmp/layer_emacs_to_acl"))
		 (funcall (svalue "ipc" "start-lisp-listener-daemon")))
	  else (format t "~%EMACSLIBRARY is not in the environment~%")))
      (error (condition) (format t "error during ipc initialization: ~a"
				 condition))))
