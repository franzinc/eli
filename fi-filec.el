;; $Header: /repo/cvs.copy/eli/fi-filec.el,v 1.5 1988/05/19 16:23:58 layer Exp $

;; File name completions

(defvar fi:shell-token-pattern "[ \t\n()<>&|;=]"
  "*The regular expression used by file name completion to mark path name
boundaries.")

(defvar fi::shell-completions-window nil
  "If non-nil, completion window requires cleaning up.")

(defun fi:shell-file-name-completion ()
  "Perform file name completion in subprocess modes."
  (interactive)
  (let ((shell-expand-string nil)
	(shell-expand-begin nil)
	(shell-expand-end nil)
	(shell-expand-dir nil)
	(shell-expand-file nil)
	(shell-expand-completion nil))

    ;; look back
    (re-search-backward fi:shell-token-pattern nil t)
    (forward-char)
    (setq shell-expand-begin (point))
    ;; look ahead
    (if (re-search-forward fi:shell-token-pattern nil 0) (backward-char))
    (setq shell-expand-end (point))

    ;; the name requiring expansion
    (setq shell-expand-string
      (buffer-substring shell-expand-begin shell-expand-end))
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

(defun fi::shell-completion-cleanup ()
  "Clean up windows after shell file name completion."
  (interactive)
  (if fi::shell-completions-window
      (save-excursion
	(set-window-configuration fi::shell-completions-window)
	(setq fi::shell-completions-window nil))))
