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
;; $Header: /repo/cvs.copy/eli/fi-db.el,v 1.1 1991/02/13 23:43:03 layer Exp $
;;

;;; need to figure out a nice heuristic for determining the process-name of
;;; the Lisp process in the current Lisp subprocess buffer.  One solution:
;;; put the process name in the listener in the startup frammis.

;;; all the commands which operate on the current frame should set it if it
;;; is not current.

;;; need to make sure no commands have disturbed the stack.  To do this,
;;; need to hack the toplevel to set a flag each time it does one of the
;;; unsafe tpl commands (dn, up, pop, etc).  when fi:scan-stack starts, it
;;; should clear the flag, so the lep:ss-* commands can check it before
;;; doing their thang.

(defvar lep:current-frame-regexp "^ ->")

(defun fi:scan-stack ()
  (interactive)
  (let ((process-name (lep::buffer-process)))
    (make-request (lep::zoom-session :process-name process-name)
		  ;; Normal continuation
		  ((process-name) (stack)
		   (lep::display-stack process-name stack))
		  ;; Error continuation
		  (() (error)
		   (message "Cannot zoom on stack: %s" error)))))

(defun lep::display-stack (process-name stack)
  (pop-to-buffer (format "*%s's stack*" process-name))
  (if buffer-read-only (toggle-read-only))
  (erase-buffer)
  (insert stack)
  (beginning-of-buffer)
  (re-search-forward lep:current-frame-regexp)
  (beginning-of-line)
  (lep::scan-stack-mode))

(defvar lep:scan-stack-mode-map nil)

(defun lep::scan-stack-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'let:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null lep:scan-stack-mode-map)
      (let ((map (make-keymap)))
	(define-key map "d"	'next-line)
	(define-key map "n"	'next-line)
	(define-key map "p"	'previous-line)
	(define-key map "u"	'previous-line)
	(define-key map "."	'lep:ss-set-current)
	(define-key map "l"	'lep:ss-display-locals-for-frame)
	(define-key map "L"	'lep:ss-set-local)
	(define-key map "r"	'lep:ss-restart)
	(define-key map "R"	'lep:ss-return)
	(define-key map "\C-c"	'lep:ss-continue)
	(define-key map "\C-p"	'lep:ss-pop)
	(define-key map "\C-r"	'lep:ss-reset)
	(define-key map "g"	'lep:ss-get-new-stack)

	(setq lep:scan-stack-mode-map map)))
  (use-local-map lep:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'lep:scan-stack-mode-hook))

(defun lep:ss-get-new-stack ()
  (interactive)
  ;; should save the process-name away somewhere and use it here
  )

(defun lep:ss-reset ()
  (interactive)
  (lep::check-for-state-change)
  (lep::do-tpl-command-on-process "reset"))

(defun lep:ss-set-local ()
  (interactive)
  (lep::check-for-state-change)
  (error "not done"))

(defun lep:ss-display-locals-for-frame ()
  (interactive)
  (lep::check-for-state-change)
  (error "not done"))

(defun lep:ss-set-current ()
  (interactive)
  (lep::check-for-state-change)
  (beginning-of-line)
  (if (not (looking-at lep:current-frame-regexp))
      (let* ((down t)
	     (start (point))
	     (end
	      (save-excursion
		(or (and (re-search-forward lep:current-frame-regexp nil t)
			 (setq down nil))
		    (re-search-backward lep:current-frame-regexp nil t)
		    (error "can't find current frame indicator"))
		(beginning-of-line)
		(toggle-read-only)
		(replace-match "   ")
		(point)))
	     (lines (count-lines start end)))
	(if down
	    (lep::do-tpl-command-on-process "dn" lines)
	  (lep::do-tpl-command-on-process "up" lines))
	(delete-char 3)
	(insert " ->")
	(toggle-read-only)
	(beginning-of-line))))

(defun lep:ss-continue ()
  (interactive)
  (lep::check-for-state-change)
  (lep::do-tpl-command-on-process "continue"))

(defun lep:ss-pop ()
  (interactive)
  (lep::check-for-state-change)
  (lep::do-tpl-command-on-process "pop"))

(defun lep:ss-return ()
  (interactive)
  (lep::check-for-state-change)
  (lep::do-tpl-command-on-process "return"))

(defun lep:ss-restart ()
  (interactive)
  (lep::check-for-state-change)
  (lep::do-tpl-command-on-process "restart"))

(defun lep::check-for-state-change ()
  (let ((process-name (lep::buffer-process)))
    (make-request (lep::state-query-session :process-name process-name)
		  ;; Normal continuation
		  ((process-name) (changed)
		   (if changed (error "stack scan out of date")))
		  ;; Error continuation
		  (() (error)
		   (message "Cannot query state: %s" error)))))

(defun lep::do-tpl-command-on-process (command &rest args)
  (let ((process-name (lep::buffer-process)))
    (make-request (lep::tpl-command-session :process-name process-name
					    :command command
					    :args args)
		  ;; Normal continuation
		  (() ()
		   (bury-buffer)
		   (switch-to-buffer "*common-lisp*")
;;;; pop to the *common-lisp* buffer
		   )
		  ;; Error continuation
		  (() (error)
		   (message "command failed: %s" error)))))

(defun lep::buffer-process ()
  "Initial Lisp Listener")
