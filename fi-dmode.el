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
;; $Header: /repo/cvs.copy/eli/fi-dmode.el,v 1.5 1991/03/13 21:41:52 layer Exp $
;;

;; Create a mode in which each line is a definition and . on that
;; definition brings up the definition in another window

(defvar dmode-saved-window-configuration nil)

(defvar dmode-map nil)
  
(defvar dmode-mouse-map nil)

(defun dmode-quit ()
  (interactive)
  (bury-buffer)
  (set-window-configuration dmode-saved-window-configuration))

(defvar definitions nil)

(defun dmode ()
  (kill-all-local-variables)
  (use-local-map dmode-map)
  (setq major-mode 'dmode)
  (setq mode-name "Definition Mode")
  (make-local-variable 'definitions)
  (make-local-variable 'types)
  (make-local-variable 'other-args)
  (make-local-variable 'fi:package)
  (make-local-variable 'finding-function)
  (when dmode-mouse-map (use-local-mouse-map dmode-mouse-map)))

(defun dmode-mouse-select (info)
  (goto-char (car info))
  (beginning-of-line)
  (dmode-goto-definition))
  
  

(defun dmode-goto-definition ()
  (interactive)
  (let* ((n (count-lines (point-min)
			 (save-excursion (beginning-of-line) (point))))
	 (buffer (current-buffer))
	 (def (nth n definitions))
	 (other (nth n other-args))
	 (type (nth n types)))
    (when (and (not (equal type '(nil))) finding-function)
      (apply (car finding-function) def type buffer (append other (cdr finding-function))))))

(defun dmode-goto-next ()
  (interactive)
  (next-line 1)
  (dmode-goto-definition))

(defun dmode-goto-previous ()
  (interactive)
  (previous-line 1)
  (dmode-goto-definition))



(defun list-buffer-definitions ()
  (interactive)
  (let ((buffer (current-buffer)))
    (make-request (scm::file-definitions-session
		   :pathname (buffer-file-name buffer))
		  ;; Normal continuation
		  ((buffer fi:package) (the-definitions)
		   (lep:display-some-definitions 
		    fi:package
		    the-definitions (list 'find-buffer-definition
						 buffer)))
		  ;; Error continuation
		  ((buffer) (error)
		   (message "Cannot find the definitions of buffer %s: %s"
			    buffer error)))))


(defun find-buffer-definition (string type list-buffer buffer)
  (unless (bufferp buffer) (setq buffer (find-file-noselect buffer)))
  (make-request (scm::find-buffer-definition-session
		 :pathname (buffer-file-name buffer) 
		 :fspec string
		 :type type
		 :package (save-excursion (set-buffer buffer) 
					  (string-to-keyword fi:package)))
		  ;; Normal continuation
		  ((string list-buffer) (pathname point n-more)
		   (show-found-definition string pathname point n-more t)
		   (recenter 0)
		   (switch-to-buffer-other-window list-buffer))
		  ;; Error continuation
		  ((string buffer) (error)
		   (error "Cannot find the definition of %s in %s: %s"
			  string buffer error))))

(defun lep:display-some-definitions (package buffer-definitions
				     fn-and-arguments
				     &optional buffer-name)
  (setq dmode-saved-window-configuration
    (current-window-configuration))
  (switch-to-buffer-other-window (or buffer-name "*definitions*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapcar '(lambda (x) 
	    (princ (car x) (current-buffer))
	    (unless (equal '(nil) (second x))
	      (insert ", ")
	      (princ (second x) (current-buffer)))
	    (insert "\n"))
	  buffer-definitions)
  (dmode)
  (not-modified)
  (setq buffer-read-only t)
  (setq definitions (mapcar 'car buffer-definitions))
  (setq types (mapcar 'second buffer-definitions))
  (setq other-args (mapcar 'third buffer-definitions))
  (setq finding-function fn-and-arguments)
  (setq fi:package package)
  (let ((height (window-height)))
    (when (> height 5) (shrink-window (- height 5))))
  (beginning-of-buffer)
  ;;don't go to the first definition, by default
  ;; (dmode-goto-definition)
  )

(if dmode-map 
    nil
  (setq dmode-map (make-sparse-keymap))
  (define-key dmode-map "\c-." 'dmode-goto-definition)
  (define-key dmode-map "." 'dmode-goto-definition)
  (define-key dmode-map "p" 'dmode-goto-previous)
  (define-key dmode-map "n" 'dmode-goto-next)
  (define-key dmode-map "q" 'dmode-quit)
  (define-key dmode-map "\e."	'fi:lisp-find-tag))

;;(if dmode-mouse-map
;;    nil
;;  (setq dmode-mouse-map (create-mouse-map))
;;  (define-mouse dmode-mouse-map mouse-left mouse-down 'dmode-mouse-select))
