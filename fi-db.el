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
;; $Header: /repo/cvs.copy/eli/fi-db.el,v 1.2 1991/02/15 01:42:30 layer Exp $
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

(defvar lep:scan-stack-mode-map nil)

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

(defun lep::scan-stack-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'let:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null lep:scan-stack-mode-map)
      (let ((map (make-keymap)))
	(define-key map "\C-c"	'lep:ss-continue)
	(define-key map "\C-p"	'lep:ss-pop)
	(define-key map "\C-r"	'lep:ss-reset)
	(define-key map "."	'lep:ss-set-current)
	(define-key map "L"	'lep:ss-set-local)
	(define-key map "R"	'lep:ss-return)
	(define-key map "d"	'next-line)
	(define-key map "e"	'lep:ss-edit)
	(define-key map "g"	'lep:ss-get-new-stack)
	(define-key map "l"	'lep:ss-display-locals-for-frame)
	(define-key map "p"	'lep:ss-pprint-frame)
	(define-key map "q"	'lep:ss-quit)
	(define-key map "r"	'lep:ss-restart)
	(define-key map "u"	'previous-line)

	(setq lep:scan-stack-mode-map map)))
  (use-local-map lep:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'lep:scan-stack-mode-hook))

(defun lep::do-tpl-command-on-process (done command &rest args)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request (lep::tpl-command-session
		   :process-name process-name
		   :command command
		   :args args
		   :done (eq t done)
		   :offset (when done offset))
		  ;; Normal continuation
		  ((offset) (done)
		   (if (eq t done)
		       (progn
			 (bury-buffer)
;;;; shouldn't hardwire this:
			 (switch-to-buffer "*common-lisp*"))
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
    (let* ((down t)
	   (start (point))
	   (end
	    (save-excursion
	      (or (and (re-search-forward lep:current-frame-regexp nil t)
		       (progn (setq down nil) t))
		  (re-search-backward lep:current-frame-regexp nil t)
		  (error "can't find current frame indicator"))
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
	      (error "not on current frame"))
	  (replace-match "   "))
      (toggle-read-only))))


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
  (lep::do-tpl-command-on-process t "return"))

(defun lep:ss-restart ()
  (interactive)
  (lep::do-tpl-command-on-process t "restart"))

(defun lep:ss-edit ()
  (interactive)
  (lep::do-tpl-command-on-process 'make-current "edit"))

(defun lep:ss-get-new-stack ()
  (interactive)
  (fi:scan-stack))

(defun lep:ss-set-current ()
  (interactive)
  (let ((offset (lep::offset-from-current-frame)))
    (if offset
	(if (> offset 0)
	    (lep::do-tpl-command-on-process nil "dn" offset ':zoom nil)
	  (lep::do-tpl-command-on-process nil "up" (- offset) ':zoom nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; stuff below here is not done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lep:ss-quit ()
  (interactive)
  (bury-buffer)
  (switch-to-buffer "*common-lisp*"))


(defun lep::buffer-process ()
  "Initial Lisp Listener")

(defun lep:ss-pprint-frame ()
  (interactive)
  (error "not done"))

(defun lep:ss-display-locals-for-frame ()
  (interactive)
  (error "not done"))

(defun lep:ss-set-local ()
  (interactive)
  ;; since the emacs reader can't read real lisp objects, it may be better
  ;; to just pop to the *common-lisp* buffer and insert ":set-local foo "
  (error "not done"))
