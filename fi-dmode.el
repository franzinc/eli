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
;; $Header: /repo/cvs.copy/eli/fi-dmode.el,v 1.6 1991/03/15 12:43:28 layer Exp $
;;

;; Create a mode in which each line is a definition and . on that
;; definition brings up the definition in another window

(defvar lep::definition-mode-saved-window-configuration nil)

(defvar fi:definition-mode-map nil)

(defvar fi:definition-mode-mouse-map nil)

(defvar fi:definition-mode-hook nil
  "*A hook run from fi:definition-mode.")

(defun fi:definition-mode ()
  "A major mode for viewing definitions of objects defined in the Common
Lisp environment.  The definitions are put in a buffer called
*definitions*, and each line contains the name and type of the definition.
The type is one of:

	:operator	for functions, methods, generic functions
				and macros,
	:type		for classes (types),
	:setf-method	for setf methods, or
	:variable	for constants and variables.

Definition mode is used by other tools, such as the changed-definition
commands, fi:lisp-who-calls, as well as fi:list-buffer-definitions.

The keymap for this mode is bound to fi:definition-mode-map:
\\{fi:definition-mode-map}
Entry to this mode runs the fi:definition-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:definition-mode)
  (setq mode-name "Definition Mode")

  (make-local-variable 'lep::definitions)
  (make-local-variable 'lep::definition-types)
  (make-local-variable 'lep::definition-other-args)
  (make-local-variable 'lep::definition-finding-function)

  (if (null fi:definition-mode-map )
      (let ((map (make-keymap)))
	(define-key map "\e."  'fi:lisp-find-tag)
	(define-key map "\C-." 'fi:definition-mode-goto-definition)
	(define-key map "."    'fi:definition-mode-goto-definition)
	(define-key map "n"    'fi:definition-mode-goto-next)
	(define-key map "p"    'fi:definition-mode-goto-previous)
	(define-key map "q"    'fi:definition-mode-quit)
	(setq fi:definition-mode-map map)))

  (use-local-map fi:definition-mode-map)

  ;;(if fi:definition-mode-mouse-map
  ;;    nil
  ;;  (setq fi:definition-mode-mouse-map (create-mouse-map))
  ;;  (define-mouse fi:definition-mode-mouse-map mouse-left mouse-down
  ;;		'lep::dmode-mouse-select))
  (when fi:definition-mode-mouse-map
    (use-local-mouse-map fi:definition-mode-mouse-map))

  (run-hooks 'fi:definition-mode-hook))

(defun fi:list-buffer-definitions ()
  "List the definition for all the objects in the current buffer.  That is,
use the current buffer and display all the definitions contained in it."
  (interactive)
  (let ((buffer (current-buffer)))
    (make-request (scm::file-definitions-session
		   :pathname (buffer-file-name buffer))
		  ;; Normal continuation
		  ((buffer fi:package) (the-definitions)
		   (lep:display-some-definitions 
		    fi:package
		    the-definitions
		    (list 'lep::find-buffer-definition buffer)))
		  ;; Error continuation
		  ((buffer) (error)
		   (message "Cannot find the definitions of buffer %s: %s"
			    buffer error)))))

;;(defun lep::dmode-mouse-select (info)
;;  (goto-char (car info))
;;  (beginning-of-line)
;;  (fi:definition-mode-goto-definition))

(defun fi:definition-mode-quit ()
  "Quit definition mode and restore the window configuration as it was
before definition mode was entered."
  (interactive)
  (bury-buffer)
  (set-window-configuration lep::definition-mode-saved-window-configuration))

(defun fi:definition-mode-goto-definition ()
  "Find the definition associated with the entry on the current line."
  (interactive)
  (let* ((n (count-lines (point-min)
			 (save-excursion (beginning-of-line) (point))))
	 (buffer (current-buffer))
	 (def (nth n lep::definitions))
	 (other (nth n lep::definition-other-args))
	 (type (nth n lep::definition-types)))
    (when (and (not (equal type '(nil))) lep::definition-finding-function)
      (apply (car lep::definition-finding-function)
	     def type buffer
	     (append other (cdr lep::definition-finding-function))))))

(defun fi:definition-mode-goto-next ()
  "Find the definition on the next line.  Equivalent to ``\\<global-map>\\[next-line]''
followed by \
``\\<fi:definition-mode-map>\\[fi:definition-mode-goto-definition]'' \
in definition mode."
  (interactive)
  (next-line 1)
  (fi:definition-mode-goto-definition))

(defun fi:definition-mode-goto-previous ()
  "Find the definition on the previous line.  Equivalent to ``\\<global-map>\\[previous-line]''
followed by \
``\\<fi:definition-mode-map>\\[fi:definition-mode-goto-definition]'' \
in definition mode."
  (interactive)
  (previous-line 1)
  (fi:definition-mode-goto-definition))

(defun lep::find-buffer-definition (string type list-buffer buffer)
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
  (setq lep::definition-mode-saved-window-configuration
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
  (fi:definition-mode)
  (not-modified)
  (setq buffer-read-only t)
  (setq lep::definitions (mapcar 'car buffer-definitions))
  (setq lep::definition-types (mapcar 'second buffer-definitions))
  (setq lep::definition-other-args (mapcar 'third buffer-definitions))
  (setq lep::definition-finding-function fn-and-arguments)
  (setq fi:package package)
  (let ((height (window-height)))
    (when (> height 5) (shrink-window (- height 5))))
  (beginning-of-buffer))
