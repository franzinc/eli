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
;; $Header: /repo/cvs.copy/eli/fi-stream.el,v 1.5 1991/06/19 22:18:19 layer Exp $
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
  (fi::with-keywords (parent x y width height splitp) args
    (let* ((fi::listener-protocol ':stream)
	   (proc (save-window-excursion
		   (fi:open-lisp-listener -1 "compilation-output"))))
      (cond ((or parent x y width height)
	     (fi::create-new-mapped-screen-for-stream parent x y width
						      height))
	    ((get-buffer-window (process-buffer proc))
	     (select-window (get-buffer-window (process-buffer proc))))
	    (splitp (split-window-vertically)))
      (send-string proc (format "%d\n" (fi::session-id session)))
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

