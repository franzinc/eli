;; local-fi-developer-hooks.el

;; This file is not for public distribution.
;; It contains extra hooks for fi developers only, things like special
;; file write hooks that automatically time stamp runtime system files.
;; If this file is present, it is loaded by fi-site-init.

(defun fi::update-acl-id ()
  (interactive "")
  (save-excursion
    (beginning-of-buffer)
    (let ((type (cond ((re-search-forward "^void ACLID_" 1500 t) 'c)
		      ((re-search-forward "^(def-runtime-q ACLID_" 1500 t) 'rs))))
      (when type
	(condition-case ()
	    (progn
	      (goto-char (match-end 0))
	      (push-mark nil t)
	      (end-of-line)
	      (delete-region (point) (mark))
	      (let ((str (concat (substring buffer-file-name
					    (+ 1
					       (or (position ?/ buffer-file-name :from-end t)
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
