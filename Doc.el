(setq load-path
  (cons (file-name-directory (directory-file-name default-directory))
	load-path))

(load "fi/site-init.el")

(defun xxx-doc-xxx ()
  (switch-to-buffer "*foo*")
  (erase-buffer)

  (fi:common-lisp-mode)
  (fi:franz-lisp-mode)
  (fi:tcp-common-lisp-mode)
  (fundamental-mode)

  (insert-file "spec.n")

  (beginning-of-buffer)

  (while (re-search-forward "^%" nil t)
    (beginning-of-line)
    (cond
      ((looking-at "^%% ")
       (delete-char 3)
       (let* ((debug-on-error nil)
	      (var-string (buffer-substring
			   (point) (progn (end-of-line) (point))))
	      (var (intern var-string))
	      val doc)
	 (cond
	   ((fboundp var)
	    (insert-char ?. (- 78 (length "[function]")
			       (length (symbol-name var))))
	    (setq current-local-map-var (current-local-map))
	    (let ((key (substitute-command-keys
			(format "\\<current-local-map-var>\\[%s]" var))))
	      (insert "[function]\n")
	      (if (/= 0 (or (string-match "M-x" key) 1))
		  (insert (format "   Invoke with: %s in %s mode.\n"
				  key mode-name)))
	      (insert (format "   %s")
		      (or (documentation var)
			  "NO DOCUMENTATION AVAILABLE"))))
	   (t ;; assume a bound variable
	    (let* ((val (symbol-value var))
		   (type (cond ((syntax-table-p val) "[syntax-table]")
			       ((keymapp val) "[keymap]")
			       (t "[variable]")))
		   (doc (or (documentation-property
			     var 'variable-documentation)
			    "NO DOCUMENTATION AVAILABLE")))
	      (insert-char ?. (- 78 (length type)
				 (length (symbol-name var))))
	      (cond ((syntax-table-p val)
		     (insert (format "%s\n   %s" type doc)))
		    ((keymapp val)
		     (insert
		      (format "%s\n   %s\n%s" type doc
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

  (write-region (point-min) (point-max) "spec.out")
  )

(xxx-doc-xxx)
