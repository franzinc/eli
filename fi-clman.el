;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are the property 
;; of Franz, Inc.  
;;
;; This file (or any derivation of it) may be distributed without 
;; further permission from Franz Inc. as long as:
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, other than a tape
;;	  fee, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this interface, please feel
;; free to contact Franz Inc. at
;;	Franz Inc.
;;	Attn: Kevin Layer
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;;	(415) 548-3600
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/Attic/fi-clman.el,v 1.1 1989/02/14 17:20:06 layer Exp $

(defconst clman:doc-directory (fi::find-path "fi/manual/"))

(defconst clman:package-info
  (list 
   (list "xcw-pilot"
	 (concat clman-doc-directory "winman/pages/x-specific/new-pilot/"))
   (list "xcw" (concat clman-doc-directory "winman/pages/x-specific/"))
   (list "cw" (concat clman-doc-directory "winman/pages/"))
   (list "math" (concat clman-doc-directory "mathpack/pages/"))
   (list "lisp" (concat clman-doc-directory "refman/pages/"))))

(load "fi/clman-oblist.el")

;;; clman-mode

(defvar clman-mode-map nil)

(defun clman-mode ()
  "Major mode for getting around
Like Text Mode but with these additional comands:\n\\{clman-mode-map}\n"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map clman-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'clman-mode)
  (setq mode-name "CLMAN")
  (run-hooks 'text-mode-hook))

(defun search-forward-see-alsos ()
  (interactive)
  (search-forward "SEE ALSO"))

(defun back-a-sexp ()
  (interactive)
  (backward-sexp)(backward-sexp)
  (forward-sexp))

(if clman-mode-map
    nil
  (setq clman-mode-map (make-sparse-keymap))
  (define-key clman-mode-map "p" 'previous-line)
  (define-key clman-mode-map "n" 'next-line)
  (define-key clman-mode-map "a" 'beginning-of-line) 
  (define-key clman-mode-map "f" 'forward-sexp) 
  (define-key clman-mode-map "b" 'back-a-sexp)
  (define-key clman-mode-map "m" 'clman)
  (define-key clman-mode-map "s" 'search-forward-see-alsos) 
  (define-key clman-mode-map "\C-C\C-C" 'clman-flush-doc))

(defun clman-flush-doc ()
  (interactive)
   (kill-buffer (current-buffer))
   (other-window 1)(delete-other-windows))

(defvar clman-displaying-fn 'clman-find-file
  "This function will be funcalled with two arguments, the
.doc file to be displayed, and the buffer which is the value of
clman-displaying-buffer.  If you wish, you can set this variable
to your own displaying function.")

(defvar clman-displaying-buffer "*excldoc*"
  "Either nil or a string naming the buffer
that the system will use for displaying documentation pages.  If nil, then
the system will not try to reuse the same buffer.")

(defun edit-package-info ()
  (interactive)
  (let ((buf (generate-new-buffer "Package Info Editor")))
    (switch-to-buffer buf)
    (emacs-lisp-mode)
    (insert-string "(setq clman-package-info ")
    (newline)
    (insert-string "'(")
    (mapcar 'insert-and-newline clman-package-info)
    (insert "))")        
    (backward-sexp)
    (indent-sexp)))
                 
(defun insert-and-newline (obj)
  (insert-string (prin1-to-string obj))
  (newline))

(defun clman (&optional symbol)
  (interactive)
  (let* ((temp-info clman-package-info)(package nil)
         (doc-page nil)(syn nil)
         (done nil))
    (setq sym (or symbol (clman-get-sym-to-lookup)))
    (while (not done) 
      (setq package (car temp-info))
      (if (not package)
          (progn (setq done t)
                 (message "Couldn't find the doc page for %s " sym))
	(progn
	  (setq doc-page
	    (concat (car (cdr package))
		    (file-nameify sym))))
	(if (file-exists-p doc-page)
	    (progn
	      (setq done t)
	      (clman-display-file doc-page clman-displaying-buffer))
	  (setq temp-info (cdr temp-info))))))) 
                
   

(defun clman-get-sym-to-lookup ()
  (interactive)		   
  (let* ((str nil)(sym nil)(ans nil))
    ;; make sure we have a symbol table
    ;; get a symbol to look up, if the user did not provide one
       (setq str (clman-backward-copy-sexp-as-kill))
       (setq sym (if (or (string= str "")
			 (string= (substring str 0 1) "(")
			 (string= (substring str 0 1) "."))
		     nil (read-from-string str)))
       (if (listp (car sym))(setq str nil))
          
       (setq ans (completing-read  
		  (concat "Symbol (" str "): ") clman-oblist))
       (if (string=  ans "")(setq ans str))
       (setq ans (clman-strip-leading-package-name ans))
       ans))

(defun clman-strip-leading-package-name (str)
  (interactive)
  (let ((pos (string-match ":" str)))
   (if (and pos (not (= pos 0)))
       (substring str (+ 1 pos) (length str))
       str)))

(defun clman-retrieve-doc-page (str table doc-dir)
  "Retrieve the documentation page for the string argument, which is 
the name of a symbol that we want to look up. If the symbol is 
not found, you will be prompted for an alternate package. If you just
hit return, this function returns nil."
  (let ((name
	  (man-page-lookup str table doc-dir)))
    ;; name is the full pathname of the doc page we want
    (bury-buffer)
    (if name name nil)))
		       
(defun clman-display-file (name buf)
  "Display name, which is an clman .doc file according to a displaying style.
The displaying style is the value of the global var clman-displaying-fn.
The two built in displaying functions are 'clman-view-file, which uses 'view,
and clman-find-file, which inserts the .doc file into the buffer named
by the value of the variable clman-displaying-buffer"
;; If buf is non-nil then we want to reuse the displaying buffer,
;; so have to erase it first
  (if buf
      (if (get-buffer buf)
	  (save-excursion 
	    (switch-to-buffer buf)

	    (erase-buffer))))
  (funcall clman-displaying-fn name buf)
  (clman-mode))

(defun clman-view-file (name buf)
  "A built-in function that you may use for the value of clman-displaying-fn.
This function uses the function 'view-file."
  (view-file name))

(defun clman-find-file (name buf)
  "A built-in function that you may use for the value of clman-displaying-fn.
This function uses the function 'insert-file to insert the file that is
named by the first argument into the buffer named by the second argument."
  (if (not (string=  buf (buffer-name(current-buffer))))
      (switch-to-buffer-other-window buf))
  (insert-file name))

(defun man-page-lookup (str table doc-dir)
  "Lookup  a string in the filename/symbol table.  The system used the
buffer which is named by the third element in clman-current-package-info.
Return the full pathname of the file the symbol is in. "
    (interactive)
    (switch-to-buffer table)
    (let ((buf (current-buffer))
	  (new-str (concat " " str " "))
	  (success nil))
      (beginning-of-buffer)
      (setq success (search-forward new-str (point-max) t))
      (if (not success) 
	  nil
	  (beginning-of-line)
	  (setq begin (point))
	  (search-forward " ")
	  (backward-char)
	  (concat doc-dir "/" (buffer-substring begin (point))))))


(defun clman-backward-copy-sexp-as-kill ()
  "Low level function."
  (backward-sexp)
  (let* ((begin (point)) end sym)
    (forward-sexp)
    (clman-remove-chars-from-string '(?\ ?\n)
    (buffer-substring begin (point)))))

(defun escape-funny-chars (sym)
  ;; the shell requires that certain chars be preceded by \
  ;; and that entire command be surrounded by '  '
  (let ((temp sym)
	(star "*")
        (circumflex "^")
        (dollar "$")
	(result "")
        (leftbrack "\[")
        (rightbrack "\]")
        (quote "'")
        (backquote "`")
        (counter 1))
    (while (not (string= temp ""))
      (setq ch (substring temp 0 1))
      (if (or (string= ch star)
              (string= ch circumflex)
              (string= ch dollar)
              (string= ch leftbrack)
              (string= ch rightbrack)
              (string= ch quote)
              (string= ch backquote))
	  (setq result (concat result "\\" ch))
          (setq result (concat result ch)))
      (setq temp (substring temp 1)))
    (setq result (concat "\"" result "\""))))

(defun clman-sub-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (char-to-string (cdr pair))
                     (char-to-string char)))
	       string
	       nil)))

(defun clman-remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
	       (if (memq char char-list)
		   nil
		 (char-to-string char)))
	     string
	     nil))

(defun file-nameify (str)
  (let ((result
         (clman-sub-chars-in-string '((?* . ?S)(?~ . ?T)
                                        (?< . ?L)(?> . ?G)
                                        (?/ . ?D)(?& . ?A) (?: . ?C)
                                        (?= . ?E)(?\\ . ?B)
                                        (?$ . ?d)(?% . ?p)
                                        (?\? . ?Q) (?\( . ?o)
                                        (?\) . ?c)(?| . ?V)
                                        (?^ . ?K)(?\[ . ?b)
                                        (?\' . ?q)(?\" . ?Z)
                                        (?\# . ?h)(?\` . ?b)
                                        (?\; . ?s)(?- . ?H)
                                        (?, . ?k)(?+ . ?a)(?\. . ?e)(?\  . ?B)
                                        )
                                      str)))
    ;;   (setq result (clman-remove-chars-from-string 
    ;;                  '(?\  ) result))
   (concat result ".doc")))


;;;;

(defun clman-apropos ()
  (interactive)
  (let ((oblist-buffer
         (generate-new-buffer "*oblist*"))
        (string (read-string "String: ")))
    (switch-to-buffer oblist-buffer)
    (insert-file 
     (concat clman-lisp-directory
             "clman-oblist.el"))
    (with-output-to-temp-buffer "*CLMAN-APROPOS*"
      (save-excursion
        (switch-to-buffer "*oblist*")
        (goto-char 1)
        (while (re-search-forward string nil t)
          (beginning-of-line)
          (princ (buffer-substring (point)
                                   (progn (end-of-line 1)
                                          (point))))
          (terpri)
;;          (tedbug "thang")
          (forward-line 1))))
    (kill-buffer "*oblist*")
    (switch-to-buffer "*CLMAN-APROPOS*")
    (clman-mode)
    (beginning-of-buffer)
    (replace-string "\"" "")
    (beginning-of-buffer)
    (replace-string "(" "")
    (beginning-of-buffer)
    (replace-string ")" "")
    (beginning-of-buffer)
    (while (search-forward "if assoc" nil t)
      (beginning-of-line)
      (kill-line 1))
    (beginning-of-buffer)))
