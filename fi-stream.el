;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-stream.el,v 1.10 1993/07/23 03:49:18 layer Exp $
;;

(defmacro fi::with-keywords (variables rest-arg &rest body)
  (let ((let-bindings nil))
    (dolist (var variables)
      (push (list var
		  (list 'fi::getf-property 
			rest-arg 
			(list 'quote (intern (concat ":" (symbol-name var))))))
	    let-bindings))
    (list* 'let
	   (reverse let-bindings)
	   body)))

(defun lep::create-listener-stream (&optional args)
  (fi:lisp-push-window-configuration)
  (fi::with-keywords (parent x y width height splitp name) args
    (let* ((fi::listener-protocol ':stream)
	   (proc
	    (save-window-excursion
	      (fi:open-lisp-listener
	       -1
	       (if name name "background-interaction")
	       (function
		(lambda (proc)
		  (format "%d\n%d\n"
			  (fi::session-id session)
			  (fi::tcp-listener-generation proc))))))))
      (cond ((or parent x y width height)
	     (fi::create-new-mapped-screen-for-stream parent x y width
						      height))
	    ((get-buffer-window (process-buffer proc))
	     (select-window (get-buffer-window (process-buffer proc))))
	    (splitp (split-window-vertically)))
      (switch-to-buffer (process-buffer proc)))))

(defun fi::create-new-mapped-screen-for-stream (parent x y width height)
  (and (boundp 'epoch::screen-properties)
       (let ((props epoch::screen-properties))
	 (when (stringp parent)
	   (push
	    (cons 'parent 
		  (epoch::string-to-resource parent 
					     (epoch::intern-atom "WINDOW")))
	    props))
	 ;; ** Do something with X,Y,WIDTH,HEIGHT
	 (let ((screen (create-screen "*foo*" props)))
	   (epoch::map-screen screen)
	   (epoch::select-screen screen)
	   screen))))

