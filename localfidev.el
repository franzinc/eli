;; local-fi-developer-hooks.el

;; This file is not for public distribution.
;; It contains extra hooks for fi developers only, things like special
;; file write hooks that automatically time stamp runtime system files.
;; If this file is present, it is loaded by fi-site-init.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup C code edit mode

(condition-case ()
    (progn
      (require 'cc-mode)
      (require 'cc-styles)
      (add-hook 'c-mode-common-hook
		(function (lambda () (c-set-style "bsd")))))
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-modify-line ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "-[" 500 t)
	(let ((debug-on-error nil))
	  (condition-case ()
	      (let ((from (point)))
		(search-forward "]-" (+ (point) 100))
		(backward-char 2)
		(delete-region from (point))
		(insert (current-time-string)
			" by "
			(user-login-name)))
	    (error (message "error updating modify line...")
		   (sit-for 1))))))
  nil)

(put 'excl::.primcall 'fi:lisp-indent-hook 1)
