(load "fi/site-init.el")

(switch-to-buffer "*foo*")
(erase-buffer)
(insert-file "list.n")

(set-syntax-table fi:lisp-mode-syntax-table)

(beginning-of-buffer)

(while (re-search-forward "^[a-z]" nil t)
  (beginning-of-line)
  (let* ((debug-on-error t)
	 (var-string (buffer-substring (point) (progn (end-of-line) (point))))
	 (var (intern var-string))
	 doc)
    (cond
      ((fboundp var)
       (indent-to-column (- 79 (length "[function]")))
       (insert "[function]\n   ")
       (insert (or (documentation var) "NO DOC")))
      (t 				; assume variable
       (indent-to-column (- 79 (length "[variable]")))
       (insert "[variable]\n   value: ")
       (insert (prin1-to-string (and (boundp var) (symbol-value var))))
       (insert "\n   ")
       (insert (or (documentation-property var 'variable-documentation)
		   "NO DOC"))))
    (insert "\n---------\n")))

(write-region (point-min) (point-max) "doc.n")
