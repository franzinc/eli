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
;; $Header: /repo/cvs.copy/eli/fi-db.el,v 1.4 1991/02/15 23:17:56 layer Exp $
;;

(defconst lep:current-frame-regexp "^ ->")
(defconst lep:ok-frame-regexp "^   (")
(defconst lep:scan-stack-mode-map nil)
(defconst lep:debugger-help
    "Debugger commands:

  C-c    :continue
  C-p    :pop
  C-r    :reset
  .      make frame under the point the current frame
  R      restart the current frame (give prefix to specify different form)
  a      toggle all frames visible (by default a subset are visible)
  d      next line
  e      edit source corresponding to function in the current frame
  g      revert stack from Lisp
  h      Causes this help text to become visible
  l      display the lexical variables for the current frame
  p      pretty print the current frame
  q      switch back to \"%s\" buffer
  r      return a value from the current frame
  u      previous line             

Type SPACE to hide this help summary.

")
(defconst lep::help-displayed t)
(defconst lep:show-all-frames nil)

(defvar lep::process-name nil)
(make-variable-buffer-local 'lep::process-name)

(defvar lep::debugger-from-buffer nil)
(make-variable-buffer-local 'lep::debugger-from-buffer)

(defun fi:scan-stack (&optional all)
  (interactive "P")
  (let ((process-name (lep::buffer-process))
	(from-buffer (when fi:subprocess-mode
		       (current-buffer))))
    (make-request (lep::zoom-session :process-name process-name
				     :all (or lep:show-all-frames all))
		  ;; Normal continuation
		  ((from-buffer process-name) (stack)
		   (let* ((name "*acl debugger*")
			  (big-name (format "%s<%s>" name process-name))
			  (buffer-name
			   (cond ((get-buffer name) name)
				 ((get-buffer big-name) big-name)
				 (t name))))
		     (pop-to-buffer buffer-name)
		     (if buffer-read-only (toggle-read-only))
		     (erase-buffer)
		     (insert (format lep:debugger-help
				     (buffer-name
				      (or from-buffer
					  lep::debugger-from-buffer))))
		     (unless lep::help-displayed
		       (lep:ss-hide-help-text))
		     (insert stack)
		     (beginning-of-buffer)
		     (re-search-forward lep:current-frame-regexp)
		     (beginning-of-line)
		     (lep::scan-stack-mode from-buffer process-name)))
		  ;; Error continuation
		  (() (error)
		   (message "Cannot zoom on stack: %s" error)))))

(defun lep::scan-stack-mode (from-buffer process-name)
  (interactive)
  (let ((saved-from-buffer
	 ;; KILL-ALL-LOCAL-VARIABLES will kill lep::debugger-from-buffer
	 lep::debugger-from-buffer))
    (kill-all-local-variables)
    (setq lep::debugger-from-buffer (or from-buffer
					saved-from-buffer)))
  (setq lep::process-name process-name)
  (setq major-mode 'let:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null lep:scan-stack-mode-map)
      (let ((map (make-keymap)))
	(define-key map "\C-c"	'lep:ss-continue)
	(define-key map "\C-p"	'lep:ss-pop)
	(define-key map "\C-r"	'lep:ss-reset)
	(define-key map " "	'lep:ss-hide-help-text)
	(define-key map "."	'lep:ss-set-current)
	(define-key map "R"	'lep:ss-restart)
	(define-key map "d"	'next-line)
	(define-key map "a"	'lep:ss-toggle-all)
	(define-key map "e"	'lep:ss-edit)
	(define-key map "g"	'lep:ss-get-new-stack)
	(define-key map "h"	'lep:ss-unhide-help-text)
	(define-key map "l"	'lep:ss-display-locals-for-frame)
	(define-key map "p"	'lep:ss-pprint-frame)
	(define-key map "q"	'lep:ss-quit)
	(define-key map "r"	'lep:ss-return)
	(define-key map "u"	'previous-line)
	(setq lep:scan-stack-mode-map map)))
  (use-local-map lep:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'lep:scan-stack-mode-hook))

(defun lep:ss-reset ()
  (interactive)
  (lep::do-tpl-command-on-process t "reset"))

(defun lep:ss-continue ()
  (interactive)
  (lep::do-tpl-command-on-process t "continue"))

(defun lep:ss-pop ()
  (interactive)
  (lep::do-tpl-command-on-process t "pop"))

(defun lep:ss-return ()
  (interactive)
  (lep::do-tpl-command-on-process
   t
   "return"
   (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): " "nil"))))

(defun lep:ss-restart (new-form)
  (interactive "P")
  (lep::do-tpl-command-on-process
   t
   "restart"
   (when new-form
     (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): ")))))

(defun lep:ss-edit ()
  (interactive)
  (lep::do-tpl-command-on-process 'make-current "edit"))

(defun lep:ss-get-new-stack ()
  (interactive)
  (fi:scan-stack))

(defun lep:ss-toggle-all ()
  (interactive)
  (setq lep:show-all-frames (not lep:show-all-frames))
  (fi:scan-stack))

(defun lep:ss-set-current ()
  (interactive)
  (let ((offset (lep::offset-from-current-frame)))
    (if offset
	(if (> offset 0)
	    (lep::do-tpl-command-on-process nil "dn" offset ':zoom nil)
	  (lep::do-tpl-command-on-process nil "up" (- offset) ':zoom nil)))))

(defun lep:ss-quit (&optional buffer)
  (interactive)
  (let* ((buf (or lep::debugger-from-buffer buffer))
	 (win (get-buffer-window buf)))
    (bury-buffer)
    (if win
	(select-window win)
      (switch-to-buffer buf))))

(defun lep:ss-display-locals-for-frame ()
  (interactive)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request
     (lep::local-session :process-name process-name
			 :offset offset)
     ((offset) (text)
      (when offset (lep::make-current offset))
      (show-some-text text nil))
     (() (error)
      (message "Cannot find locals: %s" error)))))

(defun lep:ss-pprint-frame ()
  (interactive)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request
     (lep::pprint-frame-session :process-name process-name
				:offset offset)
     ((offset) (text)
      (when offset (lep::make-current offset))
;;;; figure out how to find the package
      (show-some-text text nil))
     (() (error)
      (message "Cannot pprint: %s" error)))))

(defun lep:ss-hide-help-text ()
  (interactive)
  (save-excursion
    (widen)
    (beginning-of-buffer)
    (or (re-search-forward "^Evaluation stack:$" nil t)
	(end-of-buffer))
    (beginning-of-line)
    (narrow-to-region (point) (point-max))
    (setq lep::help-displayed nil)))

(defun lep:ss-unhide-help-text ()
  (interactive)
  (save-excursion
    (widen))
  (recenter)
  (setq lep::help-displayed t))

;;;
;;; internals
;;;

(defun lep::do-tpl-command-on-process (done command &rest args)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame))
	(buffer lep::debugger-from-buffer))
    (make-request (lep::tpl-command-session
		   :process-name process-name
		   :command command
		   :args args
		   :done (eq t done)
		   :offset (when done offset))
		  ;; Normal continuation
		  ((buffer offset) (done)
		   (if (eq t done)
		       (lep:ss-quit buffer)
		     (when offset (lep::make-current offset))))
		  ;; Error continuation
		  ((process-name) (error)
		   (message "Lisp error: %s" error)
		   (beep)
		   (sit-for 2)
		   (if (y-or-n-p
			(format "Revert stack from process \"%s\"? "
				process-name))
		       (fi:scan-stack))))))

(defun lep::offset-from-current-frame ()
  (beginning-of-line)
  (if (looking-at lep:current-frame-regexp)
      nil
    (if (not (looking-at lep:ok-frame-regexp))
	(error "Not on a frame."))
    (let* ((down t)
	   (start (point))
	   (end
	    (save-excursion
	      (or (and (re-search-forward lep:current-frame-regexp nil t)
		       (progn (setq down nil) t))
		  (re-search-backward lep:current-frame-regexp nil t)
		  (error "Can't find current frame indicator."))
	      (beginning-of-line)
	      (point)))
	   (lines (count-lines start end)))
      (if down
	  lines
	(- lines)))))

(defun lep::make-current (offset)
  (toggle-read-only)
  (delete-char 3)
  (insert " ->")
  (beginning-of-line)
  (save-excursion
    (forward-line (- offset))
    (unwind-protect
	(progn
	  (if (not (looking-at lep:current-frame-regexp))
	      (error "Not on current frame."))
	  (replace-match "   "))
      (toggle-read-only))))

(defun lep::buffer-process ()
  (cond
   (lep::process-name)
   (t
    (let* ((processes
	    (cdr (car (lep::eval-in-lisp 'lep::list-all-processes-session))))
	   (completions
	    (mapcar (function (lambda (x) (list x))) processes)))
      (completing-read "Process to debug: "
		       completions
		       nil
		       t
		       "Initial Lisp Listener")))))
