;; $Header: /repo/cvs.copy/eli/Doc.el,v 1.35 1992/08/19 07:20:02 layer Exp $

(require 'cl)

(setq load-path
  (cons (file-name-directory (directory-file-name default-directory))
	load-path))

(setq fi:lisp-do-indentation t)
(load "fi/site-init.el")

(setq args (cdr (fi:member-equal "--" command-line-args)))
(setq input-file (car args))
(setq output-file (car (cdr args)))
;;(setq input-file "spec.n")
;;(setq output-file "spec.out")

(message "input-file %s, output-file %s" input-file output-file)

(defun xxx-doc-xxx ()
  (switch-to-buffer "*foo*")
  (erase-buffer)

  (fi:scan-stack-mode)
  (toggle-read-only)
  (fi:definition-mode)
  (fi:common-lisp-mode)
  (fi:inferior-common-lisp-mode)
  (fi:franz-lisp-mode)
  (fi:lisp-listener-mode)
  (fi:clman-mode)
  (fi:emacs-lisp-mode)
  (fi:shell-mode)
  (fi:su-mode)
  (fi:telnet-mode)
  (fi:rlogin-mode)
  (fundamental-mode)

  (insert-file input-file)

  (beginning-of-buffer)

  (while (or (re-search-forward "^%" nil t)
	     (re-search-forward "^@" nil t))
    (beginning-of-line)
    (cond
     ((or (looking-at "^%% ")
	  (looking-at "^@@ "))
      (let* ((verbose (looking-at "^%% "))
	     (bol (point))
	     (xx (re-search-forward
		  "^\\(%%\\|@@\\) \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
		  (save-excursion (end-of-line) (point))))
	     (var-string
	      (buffer-substring (match-beginning 2) (match-end 2)))
	     (mode-string
	      (when (match-beginning 3)
		(buffer-substring (match-beginning 3) (match-end 3))))
	     (var (intern var-string))
	     (mode (when (and mode-string (not (string= "" mode-string)))
		     (intern mode-string)))
	     (xmode-name
	      (when mode-string
		(if (string-match "\\(.*\\)-map" mode-string)
		    (substring mode-string (match-beginning 1)
			       (match-end 1)))))
	     (xx (progn (beginning-of-line)
			(re-search-forward
			 "^\\(%%\\|@@\\) \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
			 (save-excursion (end-of-line) (point)))
			(replace-match "\\2")))
	     (xfunc (and (fboundp var) (symbol-function var)))
	     (func (if (and xfunc
			    (consp xfunc)
			    (or (and (stringp (third xfunc))
				     (not
				      (eq 'interactive
					  (car (fourth xfunc)))))
				(and (not (stringp (third xfunc)))
				     (not (eq 'interactive
					      (car (third xfunc)))))))
		       " [function]"
		     " [command]"))
	     (line-pad 88)
	     val doc)
	(cond
	 ((boundp var)
	  (let* ((val (symbol-value var))
		 (type (cond ((syntax-table-p val) " [syntax-table]")
			     ((keymapp val) " [keymap]")
			     (t " [variable]")))
		 (doc (or (documentation-property var 'variable-documentation)
			  (error "no documentation available for %s" var))))
	    (insert-char ?. (- line-pad (length type)
			       (length (symbol-name var))))
	    (cond ((syntax-table-p val)
		   (insert (format "%s" type))
		   (if verbose
		       (insert "\n")
		     (forward-line 1))
		   (when verbose
		     (insert-doc-string doc)))
		  ((keymapp val)
		   (insert (format "%s" type))
		   (if verbose
		       (insert "\n")
		     (forward-line 1))
		   (when verbose
		     (insert-doc-string doc)
		     (insert
		      (format "\n%s" (substitute-command-keys
				      (format "\\{%s}" var))))))
		  (t
		   (insert type)
		   (if verbose
		       (insert "\n")
		     (forward-line 1))
		   (insert (format "   Initial value: %s\n"
				   (frob-newlines (prin1-to-string val))))
		   (when verbose
		     (insert-doc-string doc))))))
	 ((fboundp var)
	  (let* ((xx (symbol-function var))
		 (arglist (and (consp xx) (car (cdr xx))))
		 (n line-pad))
	    (when arglist
	      (let ((string
		     (concat " " (mapconcat 'symbol-name arglist " "))))
		(setq n (- n (length string)))
		(insert string)))
	    (insert-char ?. (- n (length func) (length var-string))))
	  (setq current-local-map-var
	    (cond ((symbol-value mode))
		  (t nil)))
	  (let ((key (when current-local-map-var
		       (substitute-command-keys
			(format "\\<current-local-map-var>\\[%s]" var)))))
	    (insert func)
	    (if verbose
		(insert "\n")
	      (forward-line 1))
	    (if key
		(progn
		  (insert (format "   Invoke with \"%s\"" key))
		  (if (and key (null (string-match "M-x" key)))
		      (insert (format " in %s" xmode-name)))
		  (insert ".\n"))
	      (when verbose
		(insert (format "   Invoke with \"M-x %s\"" var))
		(insert ".\n")))
	    (when verbose
	      (insert-doc-string
	       (or (documentation var)
		   (error "no documentation available for %s" var))))))
	 (t (error "Variable %s is not bound or fbound" var)))
	(when verbose
	  (insert "\n\n"))))))

  (write-region (point-min) (point-max) output-file))

(defun insert-doc-string (string)
  (insert "\n")
  (let* ((start (point))
	 (end (progn
		(insert string)
		(if (not (bolp)) (forward-line 1))
		(point)))
	 (lines (count-lines start end)))
    (goto-char start)
    (while (> lines 0)
      (insert "   ")
      (forward-line 1)
      (setq lines (- lines 1)))))

(defun frob-newlines (string)
  (let ((i 0)
	(max (length string))
	(res nil)
	c)
    (while (< i max)
      (if (= 10 (setq c (aref string i)))
	  (progn (setq res (cons ?\\ res))
		 (setq res (cons ?n res)))
	(setq res (cons c res)))
      (setq i (+ i 1)))
    (concat (nreverse res))))

(xxx-doc-xxx)
