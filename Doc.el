(load "fi/site-init.el")

(switch-to-buffer "*foo*")
(erase-buffer)

(fi:common-lisp-mode)
(fi:franz-lisp-mode)
(fi:lisp-mode)
(fi:emacs-lisp-mode)
(fi:inferior-lisp-mode)
(fi:shell-mode)
(fi:tcp-lisp-mode)
(fundamental-mode)

(insert-file "list.n")

(set-syntax-table fi:lisp-mode-syntax-table)

(beginning-of-buffer)

(defvar index 1)

(while (= 0 (forward-line 1))
  (beginning-of-line)
  (cond
    ((looking-at "^[a-z]")
     (let* ((debug-on-error t)
	    (var-string (buffer-substring
			 (point) (progn (end-of-line) (point))))
	    (var (intern var-string))
	    val doc)
       (save-excursion
	 (beginning-of-line)
	 (insert (format "[%d] " index))
	 (setq index (+ index 1)))
       (cond
	 ((fboundp var)
	  (indent-to-column (- 75 (length "[function]")))
	  (insert
	   (format "[function]\n   Invoke with: %s in %s mode.\n   %s"
		   (substitute-command-keys (format "\\[%s]" var))
		   mode-name
		   (or (documentation var) "NO DOC"))))
	 (t ;; assume a bound variable
	  (let* ((val (symbol-value var))
		 (type (cond ((syntax-table-p val) "[syntax-table]")
			     ((keymapp val) "[keymap]")
			     (t "[variable]")))
		 (doc (or (documentation-property var 'variable-documentation)
			  "NO DOC")))
	    (indent-to-column (- 75 (length type)))
	    (cond ((syntax-table-p val)
		   (insert (format "%s\n   %s" type doc)))
		  ((keymapp val)
		   (insert (format "%s\n   %s\n%s" type doc
				   (substitute-command-keys
				    (format "\\{%s}" var)))))
		  (t
		   (insert (format "%s\n   Value: %s\n   %s" type
				   (prin1-to-string val)
				   doc)))))))
       (insert "\n\n")))
    ((looking-at "^%eval: ")
     (let ((start (point))
	   (string
	    (buffer-substring
	     (progn (search-forward "eval: ") (point))
	     (progn (end-of-line) (point)))))
       (delete-region start (progn (forward-char 1) (point)))
       (eval (car (read-from-string string)))))
    ((looking-at "^%mode: ")
     (let ((start (point))
	   (mode
	    (intern
	     (buffer-substring
	      (progn (search-forward "mode: ") (point))
	      (progn (end-of-line) (point))))))
       (funcall mode)
       (delete-region start (progn (forward-char 1) (point)))))))

(write-region (point-min) (point-max) "doc.n")
