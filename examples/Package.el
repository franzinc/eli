
;;;; Subprocess mode-line

(make-variable-buffer-local 'package-mode-info)
(setq package-mode-info nil)

(setq fi:subprocess-mode-hook
  (function
   (lambda ()
     (interactive)
     (setq mode-line-buffer-identification
       (list (buffer-name)
	     (let* ((proc (get-buffer-process (current-buffer)))
		    (name (and proc (calculate-process-name proc))))
	       (if name
		   (format " (%s)" (file-name-nondirectory name))))
	     'package-mode-info)))))

(setq fi:inferior-common-lisp-mode-hook
  '(lambda ()
    (interactive)
    (setq package-mode-info (list " {" 'fi:package "}"))))

(defun calculate-process-name (process)
  (let ((arglist (process-command process)))
    (if (string= "rsh" (car arglist))
	(format "remote on %s" (car (cdr arglist)))
      (car arglist))))
