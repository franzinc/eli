;; $Header: /repo/cvs.copy/eli/Doc.el,v 1.20 1991/02/28 23:09:38 layer Exp $

(setq load-path
  (cons (file-name-directory (directory-file-name default-directory))
	load-path))

(setq fi:use-lep t)
(load "fi/site-init.el")

(defun xxx-doc-xxx ()
  (switch-to-buffer "*foo*")
  (erase-buffer)

  (fi:common-lisp-mode)
  (fi:franz-lisp-mode)
  (fi:tcp-common-lisp-mode)
  (fi:tcp-common-lisp-mode)
  (fi:clman-mode)
  (fi:emacs-lisp-mode)
  (fi:shell-mode)
  (fi:su-mode)
  (fi:telnet-mode)
  (fi:rlogin-mode)
  (fundamental-mode)

  (insert-file "spec.n")

  (beginning-of-buffer)

  (while (re-search-forward "^%" nil t)
    (beginning-of-line)
    (cond
      ((looking-at "^%% ")
       (let* ( ;;(debug-on-error nil)
	      (bol (point))
	      (xx (re-search-forward
		   "^%% \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
		   (save-excursion (end-of-line) (point))))
	      (var-string
	       (buffer-substring (match-beginning 1) (match-end 1)))
	      (mode-string
	       (when (match-beginning 2)
		 (buffer-substring (match-beginning 2) (match-end 2))))
	      (var (intern var-string))
	      (mode (when (and mode-string (not (string= "" mode-string)))
		      (intern mode-string)))
	      (xmode-name
	       (when mode
		 (if (string-match "\\(.*\\)-map" mode-string)
		     (substring mode-string (match-beginning 1)
				(match-end 1)))))
	      (xx (progn (beginning-of-line)
			 (re-search-forward
			  "^%% \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
			  (save-excursion (end-of-line) (point)))
			 (replace-match "\\1")))
	      (func (if mode
			"[interactive function]"
		      "[function]"))
	      val doc)
	 ;;(message "%s" var)
	 (cond
	   ((fboundp var)
	    (insert-char ?. (- 78 (length func)
			       (length var-string)))
	    (when (symbol-value mode)
	      (setq current-local-map-var (symbol-value mode)))
	    (let ((key (substitute-command-keys
			(format "\\<current-local-map-var>\\[%s]" var))))
	      (insert func)
	      (insert "\n")
	      (if (/= 0 (or (string-match "M-x" key) 1))
		  (insert (format "   Invoke with \"%s\" in %s.\n"
				  key xmode-name)))
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
