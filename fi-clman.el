;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/Attic/fi-clman.el,v 2.4 1993/04/29 18:53:59 layer Exp $

(defun fi::figure-out-mandir ()
  (do* ((path load-path (cdr path))
	(pa (car path) (car path))
	(result nil))
      ((or result (null path))
       (or result (error "Couldn't find fi/manual/")))
    (if (file-exists-p 
	 (concat pa "/fi/manual/"))
	(setq result (concat pa "/fi/manual/")))))

(defvar fi::manual-dir (fi::figure-out-mandir))

(if (not (boundp 'fi::clman-big-oblist))
    (load (concat fi::manual-dir "OBLIST")))

(defvar fi:clman-mode-map nil
  "*Major mode key definitions for viewing a clman page.")

(defvar fi:clman-displaying-buffer "*clman*"
  "Name of the buffer in which to display CLMAN pages.")

(defvar fi::clman-window-configuration nil)

(defun fi:clman (symbol)
  "Look up SYMBOL in the online CL manual, with completion.  The optional
argument SYMBOL is prompted for in the minibuffer, if not supplied.   To get
completion for a symbol in a package other than the :lisp 
package, use the nickname of that package, followed by a colon (e.g. cw: or
math:).  The buffer that is displayed will be in CLMAN mode."
  (interactive
   (let* ((symbol-at-point (fi::get-symbol-at-point t))
	  (res (completing-read (format "CLMAN for Symbol (default %s): "
					symbol-at-point)
				fi::clman-big-oblist
				nil
				nil
				nil)))
     (list (if (string= "" res)
	       symbol-at-point
	     res))))
  (setq fi::clman-window-configuration (current-window-configuration))
  (let ((files (cdr (assoc symbol fi::clman-big-oblist))))
    (if files
	(progn
	  (fi::clman-display-file fi:clman-displaying-buffer files)
	  (length files))
      (error "couldn't find entry for %s" symbol))))
    
(defun fi:clman-apropos ()
  "Prompts for a string on which an apropos search is done.  Displays a
buffer which lists all documented symbols which match the string.  The
buffer will be in CLMAN mode."
  (interactive)
  (setq fi::clman-window-configuration (current-window-configuration))
  (let* ((string (read-string "clman apropos: "))
	 (buf (get-buffer-create "*clman apropos*")))
    (if (not (eq buf (current-buffer)))
      (switch-to-buffer buf))
    (erase-buffer)
    (dolist (obj fi::clman-big-oblist)
      (if  (and (string-match ".+:" (car obj))
		(string-match string (car obj)))
	  (insert (car obj) "\n")))
    (fi:clman-mode)
    (goto-char (point-min))))

(defun fi:clman-mode ()
  "Major mode for viewing Allegro manual pages.  text-mode-syntax-table and
text-mode-abbrev-table are `used' in this mode.
\\{fi:clman-mode-map}"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map fi:clman-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'fi:clman-mode)
  (setq mode-name "CLMAN")
  (run-hooks 'fi:clman-mode-hook 'text-mode-hook))

(defun fi:clman-search-forward-see-alsos ()
  "Move text cursor directly to the beginnig of the SEE ALSO section of a
clman buffer, from anywhere in the buffer."
  (interactive)
  (if (search-forward "SEE ALSO" nil t)
      (beginning-of-line)
    (if (search-backward "SEE ALSO" nil t)
	(beginning-of-line))))

(defun fi:clman-next-entry ()
  "Find the DESCRIPTION section."
  (interactive)
  (if (search-forward "DESCRIPTION" nil t)
      (progn (beginning-of-line)(forward-line 1))
    (progn 
      (goto-char (point-min))
      (search-forward "DESCRIPTION" nil t)
      (beginning-of-line)(forward-line 1) )))

(defun fi:clman-flush-doc ()
  "Flush the current clman buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration fi::clman-window-configuration))

(defun fi::clman-display-file (buf names)
  (with-output-to-temp-buffer buf
    (let ((first t))
      (dolist (name names)
	(when (not first)
	  (princ
	   "===============================================================================\n"))
	(princ (fi::file-contents (concat fi::manual-dir name)))
	(setq first nil))))
  (save-excursion
    (set-buffer buf)
    (fi:clman-mode))
  (message "%d additional clman pages at end of buffer"
	   (- (length names) 1)))

(defun fi::file-contents (file)
  (let ((buffer (find-file-noselect file)))
    (save-excursion
      (set-buffer buffer)
      (prog1 (buffer-string)
	(kill-buffer buffer)))))

(if fi:clman-mode-map
    nil
  (setq fi:clman-mode-map (make-keymap))
  (define-key fi:clman-mode-map "\C-C\C-C" 'fi:clman-flush-doc)
  (define-key fi:clman-mode-map "a" 'fi:clman-apropos)
  (define-key fi:clman-mode-map "m" 'fi:clman)
  (define-key fi:clman-mode-map "s" 'fi:clman-search-forward-see-alsos)
  (define-key fi:clman-mode-map " " 'scroll-up)
  (define-key fi:clman-mode-map "n" 'fi:clman-next-entry))
