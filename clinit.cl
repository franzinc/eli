;; $Header: /repo/cvs.copy/eli/Attic/clinit.cl,v 1.10 1989/07/11 18:20:01 layer Exp $

#|

;; Put the following single line in your .login file:

setenv EMACSLIBRARY /usr/local/lib/emacs

;; Then, put the following single form in your .emacs file:

(setq fi:unix-domain-socket
  (format "/tmp/%s_emacs_to_acl" (user-login-name)))

;; Then, put the following single form in your .clinit file:

(defparameter *emacs-library*
  (let ((emacs-lib (si:getenv "EMACSLIBRARY")))
    (if emacs-lib (format nil "~a/lisp/fi/" emacs-lib))))

;; plus one of the following three forms (after the above form)

;;;;;;; Form 1
;; this will load the ipc package and unconditionally start it up
(and *emacs-library*
     (load (merge-pathnames "clinit.cl" *emacs-library*))
     (load-and-start-ipc-package))

;;;;;;; Form 2
;; this will load the ipc package and start it up when `+ipc' is given on
;; the Allegro CL command line.  For example, if you put the following in
;; your .emacs file:
;;
;;     (setq fi:common-lisp-image-arguments '("+ipc"))
;;
;; then when you startup Allegro CL from within emacs (fi:common-lisp) you
;; will automatically load and start the emacs/lisp interface.
(and *emacs-library*
     (find "+ipc" (system:command-line-arguments) :test #'string=)
     (load (merge-pathnames "clinit.cl" *emacs-library*))
     (load-and-start-ipc-package))

;;;;;;; Form 3
;; this will load the ipc package and require you to start it manually
;; with the top-level command `:listen'.
(tpl:alias "listen" ()
  (and *emacs-library*
       (load (merge-pathnames "clinit.cl" *emacs-library*))
       (load-and-start-ipc-package)))

|#

;;;;; NOTE:
;; The rest of this file will be loaded when the forms you just added
;; to your .clinit.cl are evaluated, so there is no need to put it in your
;; .clinit.cl!!!

(defmacro svalue (package symbol)
  `(let ((p ,package) (s ,symbol))
     (values (find-symbol (symbol-name s) p))))

(defun load-and-start-ipc-package (&key (unix-domain nil udp))
  (errorset
   (let ((emacs-library
	  (format nil "~a/lisp/fi/" (si:getenv "EMACSLIBRARY"))))
     (if* emacs-library
	then (let* ((first (car si:*require-search-list*))
		    (do-pop (atom first)))
	       (if do-pop (pop si:*require-search-list*))
	       (push (make-pathname :directory emacs-library :type "fasl")
		     si:*require-search-list*)
	       (push first si:*require-search-list*))
	     (if (find-package :ipc)
		 (set (svalue :ipc :.lisp-listener-daemon-ff-loaded.) nil))
	     (require :ipc)
	     (require :emacs)
	     (if udp (set (svalue :ipc :*unix-domain*) unix-domain))
	     (let ((s (svalue :ipc :*socket-pathname*))
		   (socket-file
		    (format nil "/tmp/~a_emacs_to_acl"
			    (excl::userid-to-name
			     (funcall (svalue :ipc :getuid))))))
	       (if (boundp s) (set s socket-file))
	       (funcall (svalue :ipc :start-lisp-listener-daemon)))
	else (format t "~%EMACSLIBRARY is not in the environment~%")))
   t))
