;; local-fi-developer-hooks.el
;; $Id: local-fi-developer-hooks.el,v 2.6 1996/11/22 01:14:42 layer Exp $

;; This file is not for public distribution.
;; It contains extra hooks for fi developers only, things like special
;; file write hooks that automatically time stamp runtime system files.
;; If this file is present, it is loaded by fi-site-init.

(defun fi::update-acl-id ()
  (interactive "")
  (save-excursion
    (beginning-of-buffer)
    (let ((type
	   (cond ((re-search-forward "^void ACLID_" 1500 t) 'c)
		 ((re-search-forward "^(def-runtime-q ACLID_" 1500 t) 'rs))))
      (when type
	(condition-case ()
	    (progn
	      (delete-region (progn (goto-char (match-end 0)) (point))
			     (progn (end-of-line) (point)))
	      (let ((str
		     (concat (substring buffer-file-name
					(+ 1
					   (or (position ?/
							 buffer-file-name
							 :from-end t)
					       -1)))
			     "_"
			     (let ((s (current-time-string)))
			       (concat (substring s 22 24)
				       (substring s 4 7)
				       (substring s 8 16)))
			     "_"
			     (user-login-name))))
		(setq str (substitute ?_ ?. str))
		(setq str (substitute ?_ ?/ str))
		(setq str (substitute ?_ ?: str))
		(setq str (substitute ?_ ?- str))
		(setq str (substitute ?_ ?  str))
		(insert-string str)
		(insert-string (case type
				 (c "(){}")
				 (rs " ())")))))
	  (foo (message "error updating modify line...")
	       (sit-for 1))))))
  nil)

(add-hook 'fi:common-lisp-mode-hook
	  (function
	   (lambda ()
	     (pushnew 'fi::update-acl-id local-write-file-hooks))))

(add-hook 'c-mode-hook
	  (function
	   (lambda ()
	     (pushnew 'fi::update-acl-id local-write-file-hooks))))

(add-hook 'c++-mode-hook
	  (function
	   (lambda ()
	     (pushnew 'fi::update-acl-id local-write-file-hooks))))

(defun update-modify-line ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (search-forward "-[" 500 t)
	(let ((debug-on-error nil))
	  (condition-case ()
	      (let ((from (point)))
		(search-forward "]-" (+ (point) 100))
		(backward-char 2)
		(delete-region from (point))
		(insert-string (concat (current-time-string)
				       " by " (user-login-name))))
	    (error (message "error updating modify line...")
		   (sit-for 1))))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup C code editing

(setq c-auto-newline nil)
(setq c-tab-always-indent t)

(add-hook 'c-mode-hook   'local-fi::c-mode-hook-func)
(add-hook 'c++-mode-hook 'local-fi::c-mode-hook-func)

(if (and (on-ms-windows)
	 (not (eq system-type 'windows-nt)))
    (load "cxx-mode")
  (require 'cc-mode))

(push '("\\.h$" . c++-mode) auto-mode-alist)
(push '("\\.c$" . c++-mode) auto-mode-alist)
(push '("\\.cc$" . c++-mode) auto-mode-alist)
(push '("\\.cpp$" . c++-mode) auto-mode-alist)

(defun local-fi::c-mode-hook-func ()
  (auto-fill-mode 1)
  (cond ((eq fi::emacs-type 'emacs19)
	 (c-set-style "cc-mode"))
	)
;;;; This is way too slow:
  ;;(when (fboundp 'font-lock-mode) (font-lock-mode 1))
  )
