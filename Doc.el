(load "fi/site-init.el")

(fi:common-lisp-mode)
(fi:franz-lisp-mode)
(fi:lisp-mode)
(fi:emacs-lisp-mode)
(fi:inferior-lisp-mode)
(fi:shell-mode)
(fi:tcp-lisp-mode)

(switch-to-buffer "*foo*")
(erase-buffer)
(insert-file "list.n")

(set-syntax-table fi:lisp-mode-syntax-table)

(beginning-of-buffer)

(defvar index 0)

(while (re-search-forward "^[a-z]" nil t)
  (beginning-of-line)
  (let* ((debug-on-error t)
	 (var-string (buffer-substring (point) (progn (end-of-line) (point))))
	 (var (intern var-string))
	 val doc)
    (save-excursion
      (beginning-of-line)
      (insert (format "[%d] " index))
      (setq index (+ index 1)))
    (cond
      ((fboundp var)
       (indent-to-column (- 75 (length "[function]")))
       (insert "[function]\n   invoke with: ")
       (insert (substitute-command-keys
		(format "\\[%s]" var)))
       (insert "\n   ")
       (insert (or (documentation var) "NO DOC")))
      (t 				; assume variable
       (indent-to-column (- 75 (length "[variable]")))
       (if (boundp var)
	   (progn
	     (insert "[variable]\n   value: ")
	     (setq val (symbol-value var))
	     (cond ((keymapp val) (insert "#<keymap>"))
		   ((syntax-table-p val) (insert "#<syntax-table>"))
		   (t (insert (prin1-to-string val))))))
       (insert "\n   ")
       (insert (or (documentation-property var 'variable-documentation)
		   "NO DOC"))))
    (insert "\n\n")))

(write-region (point-min) (point-max) "doc.n")
