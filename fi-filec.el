;; $Header: /repo/cvs.copy/eli/fi-filec.el,v 1.8 1988/07/23 12:08:50 layer Exp $

;; Command and file name completion

(defvar fi:shell-token-pattern "[ \t\n()<>&|;=]"
  "*The regular expression used by file name completion to mark path name
boundaries.")

(defvar fi::shell-completions-window nil
  "If non-nil, completion window requires cleaning up.")

(defun fi:shell-do-completion ()
  "Do either command or file name completion in a subprocess buffer
containing a shell (or other subprocess for which it would be useful, such
as Common Lisp or rlogin buffers)."
  ;; First, find out whether or not we are completing a command or file
  ;; name.  Then, if a command, determine if it can be reduced to file name
  ;; completion because it contains a slash (either absolute or relative,
  ;; it doesn't matter).
  (interactive)
  (let* ((completion-ignore-case nil)
	 (completion-ignored-extensions nil)
	 (opoint (point))
	 (input-start
	  (save-excursion
	    (goto-char fi::last-input-end)
	    (if (re-search-forward subprocess-prompt-pattern opoint t)
		(point)))))
    (if input-start
	(if (save-excursion
	      (or (re-search-backward "[ \t]" input-start t)
		  (search-backward "/" input-start t)))
	    (call-interactively 'fi:shell-file-name-completion)
	  (call-interactively 'fi:shell-command-completion)))))

(defun fi:shell-file-name-completion ()
  "Perform file name completion in subprocess modes."
  (interactive)
  (let ((shell-expand-string (fi::shell-completion-default-prefix))
	(shell-expand-dir nil)
	(shell-expand-file nil)
	(shell-expand-completion nil))
    ;; directory part of name
    (setq shell-expand-dir
      (or (file-name-directory shell-expand-string) default-directory))
    ;; file part of name
    (setq shell-expand-file
      (file-name-nondirectory shell-expand-string))
    
    ;; do the expansion
    (setq shell-expand-completion
      (file-name-completion shell-expand-file shell-expand-dir))
    ;; display the results
    (if (eq shell-expand-completion t) (message "Sole completion")
      (if (eq shell-expand-completion nil) (message "No match")
	(if (equal shell-expand-completion shell-expand-file)
	    (progn
	      (if fi::shell-completions-window nil
		(setq fi::shell-completions-window
		  (current-window-configuration)))
	      (message "Making completion list...")
	      (with-output-to-temp-buffer " *Completions*"
		(display-completion-list
		  (sort (file-name-all-completions
			  shell-expand-completion shell-expand-dir)
			'string-lessp)))
	      (message ""))
 	  ;; put in the expansion
	  (search-backward shell-expand-file)
	  (replace-match shell-expand-completion t t))))))

(defun fi:shell-command-completion (shell-expand-string)
  "Perform file name completion in subprocess modes."
  (interactive (list (fi::shell-completion-default-prefix)))
  (let ((completions nil)
	(dirs exec-path))
    ;;
    ;; Find all possible completions of `shell-expand-string' in the
    ;; exec-path (ie, PATH environment variable), comprised of only
    ;; executable file names.
    ;;
    (while dirs
      (let* ((dir (expand-file-name (car dirs)))
	     (res
	      (fi::executable-files
	       (file-name-all-completions shell-expand-string dir)
	       dir)))
	(if res
	    (setq completions (append res completions))))
      (setq dirs (cdr dirs)))
    
    ;; `completions' is a list of all possible completions

    (cond
      ((null completions)
       (message "No match"))
      ((= 1 (length completions))
       (search-backward shell-expand-string)
       (replace-match (car completions) t t))
      (t ;; display the completions
       (if (not fi::shell-completions-window)
	   (setq fi::shell-completions-window
	     (current-window-configuration)))
       (with-output-to-temp-buffer " *Completions*"
	 (display-completion-list
	  (sort completions 'string-lessp)))))))

(defun fi::executable-files (files dir)
  (cond
    (files
     (let (res file)
       (while files
	 (setq file (concat dir "/" (car files)))
	 (if (not (zerop (logand 73 (file-modes file))))
	     (setq res (cons (car files) res)))
	 (setq files (cdr files)))
       res))))

(defun fi::shell-completion-default-prefix ()
  (re-search-backward fi:shell-token-pattern nil t)
  (forward-char)
  (buffer-substring
   (point)
   (progn
     (if (re-search-forward fi:shell-token-pattern nil 0) (backward-char))
     (point))))

(defun fi::shell-completion-cleanup ()
  "Clean up windows after shell file name completion."
  (interactive)
  (if fi::shell-completions-window
      (save-excursion
 	(set-window-configuration fi::shell-completions-window)
 	(setq fi::shell-completions-window nil))))
