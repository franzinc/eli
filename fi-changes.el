;;
;; copyright (C) 1991 Franz Inc, Berkeley, Ca.
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
;;	emacs-info@franz.com
;;	uunet!franz!emacs-info
;;
;; $Header: /repo/cvs.copy/eli/fi-changes.el,v 1.2 1991/03/13 21:42:09 layer Exp $

;;; Support for changed definitions


;; prefix arg of 1 (which is the default) is changes since first read.
;; 2 changes since saved
;; 3 changes since last evaluated.
;; Is this compatible with slymbolics

(defun fi:list-buffer-changed-definitions (since)
  "List the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':list since))

(defun fi:list-changed-definitions (since)
  "List the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':list since t))

(defun fi:eval-buffer-changed-definitions (since)
  "eval the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':eval since))


(defun fi:eval-changed-definitions (since)
  "eval the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':eval since t))

(defun fi:compile-buffer-changed-definitions (since)
  "eval the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':compile since))

(defun fi:compile-changed-definitions (since)
  "eval the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':compile since t))

(defun fi:copy-buffer-changed-definitions (since)
  "copy into the killring the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':copy since))

(defun fi:copy-changed-definitions (since)
  "copy into the killring the changed definitions SINCE read, saved, last evalled"
  (interactive "p")
  (do-buffer-changed-definitions ':copy since t))

(defun fi:compare-source-files (new-file old-file)
  (interactive "fnew file:
fold file")
  (find-file new-file)
  (let ((buffer (current-buffer))
	(package fi:package))
    (make-request (scm::list-changed-definitions
		   :operation ':list
		   :old-file old-file
		   :new-file new-file)
		   ((buffer package) (changes)
		    (if changes
			(show-changes buffer package changes)
		      (message "There are no changes.")))
		   (() (error)
		    (error "Cannnot list changed definitions: %s" error)))))

;;; The guts of the problem

(defun do-buffer-changed-definitions (operation since &optional all-buffers)
  (message "Computing changes...")
  (setq since (convert-since-prefix since))
  (let ((buffer (current-buffer))
	(copy-file-name (and (eq operation ':copy)
			     (format "%s/%s.cl"
				     fi:emacs-to-lisp-transaction-directory
				     (make-temp-name "EtoL"))))
	(package fi:package))
    (if all-buffers
	(let ((args nil))
	  (save-excursion
	    (dolist (buffer (buffer-list))
	      (set-buffer buffer)
	      (if (check-buffer-for-changes-p since)
		  (push (compute-file-changed-values-for-current-buffer) args))))
	  (if args
	      (apply
	       (function do-buffer-changed-definitions-1)
	       copy-file-name
	       (transpose-list args))
	    (message "There are no changes.")))
      (if (check-buffer-for-changes-p since)
	(apply
	 (function do-buffer-changed-definitions-1)
	 copy-file-name
	 (compute-file-changed-values-for-current-buffer))
	(message "There are no changes.")))))

(defun check-buffer-for-changes-p (since)
  "Decide whether this buffer is worth checking for changes"
  (and (eq major-mode 'fi:common-lisp-mode)
       (buffer-file-name)
       (ecase since
	 (:read
	  (or (buffer-modified-p)
	      buffer-backed-up))
	 (:saved
	  (buffer-modified-p))
	 (:eval
	  ;; Its like this buffer needs an every-modified-flag
	  t))))

(defun transpose-list (list)
  (let ((l (make-list (length (car list)) nil)))
    (dolist (k list)
      (let ((n 0))
	(dolist (a k)
	  (push a (nth n l))
	  (incf n))))
    l))

(defun compute-file-changed-values-for-current-buffer ()
  (let ((actual-file (buffer-file-name))
	  (old-file 
	   (if (and (not (eq since ':saved))
		    buffer-backed-up)
	       (make-backup-file-name (buffer-file-name))
	     (buffer-file-name)))
	  (new-file (buffer-file-name)))
    (list actual-file old-file new-file)))

(defun do-buffer-changed-definitions-1 (copy-file-name actual-file old-file new-file)
    (make-request (scm::list-changed-definitions
		   :operation operation
		   :copy-file-name copy-file-name
		   :actual-file actual-file
		   :old-file old-file
		 :new-file new-file
		 :since since)
		  ((operation copy-file-name) (changes)
		   (if changes
		       (progn
			 (if (eq operation ':copy)
			     (insert-file-contents-into-kill-ring
				copy-file-name))
			 (show-changes changes))
		     (message "There are no changes.")))
		((operation) (error)
		 (error 		 
		  (ecase operation
		    (:copy "copy changed definitions: %s")
		    (:list "Cannnot list changed definitions: %s")
		    (:eval "Cannnot evaluate changed definitions: %s")
		    (:compile "Cannnot compile changed definitions: %s"))
		  error))))

(defun insert-file-contents-into-kill-ring (copy-file-name)
  (let ((buffer (generate-new-buffer "*temp*")))
    (save-excursion
      (set-buffer buffer)
      (insert-file copy-file-name)
      (copy-region-as-kill (point-min) (point-max)))
    (kill-buffer buffer)))

(defun show-changes (changes)
  (lep:display-some-definitions fi:package
				changes
				(list 'find-buffer-definition)
				"*changes*"))

(defun convert-since-prefix (since)
  (ecase since
    (1 ':read)
    (2 ':saved)
    (3 ':eval)))
