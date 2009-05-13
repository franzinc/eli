;; Stuff grabbed and modified from the GNU Emacs sources.
;; $Id: fi-gnu.el,v 3.0.176.3 2009/02/27 00:53:57 layer Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994, 1995, 1996,
;;   1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Maintainer: FSF

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trimmed down version of find-backup-file-name from lisp/files.el.
(defun fi::find-most-recent-backup-file-name (fn)
  (if (eq version-control 'never)
      (make-backup-file-name fn)
    (let* ((basic-name (make-backup-file-name-1 fn))
	   (base-versions (concat (file-name-nondirectory basic-name) ".~"))
	   (backup-extract-version-start (length base-versions))
	   (high-water-mark 0)
	   (number-to-delete 0)
	   possibilities deserve-versions-p versions)
      (condition-case ()
	  (setq possibilities (file-name-all-completions
			       base-versions
			       (file-name-directory basic-name))
		versions (sort (mapcar #'backup-extract-version
				       possibilities)
			       #'<)
		high-water-mark (apply 'max 0 versions)
		deserve-versions-p (or version-control
				       (> high-water-mark 0))
		number-to-delete (- (length versions)
				    kept-old-versions
				    kept-new-versions
				    -1))
	(file-error (setq possibilities nil)))
      (list (if (not deserve-versions-p)
		(make-backup-file-name fn)
	      (format "%s.~%d~" basic-name high-water-mark))))))

;; from Robert P. Goldman, rpgoldman@sift.info
(defun fi::make-backup-file-name-1 (file)
  "Replacement for FSF emacs function in xemacs."
  (let* ((backup-directory (temp-directory))
	;; If backup-directory is relative, it should be relative to the
	;; file's directory.  By expanding explicitly here, we avoid
	;; depending on default-directory.
	 (abs-backup-directory
	  (expand-file-name backup-directory
				(file-name-directory file))))
    (when (and abs-backup-directory (not (file-exists-p
abs-backup-directory)))
	(condition-case nil
	    (make-directory abs-backup-directory 'parents)
	  (file-error (setq backup-directory nil
			    abs-backup-directory nil))))
    (if (null backup-directory)
	file
      (if (file-name-absolute-p backup-directory)
	  (progn
	    (when (memq system-type '(windows-nt ms-dos cygwin))
	      ;; Normalize DOSish file names: downcase the drive
	      ;; letter, if any, and replace the leading "x:" with
	      ;; "/drive_x".
	      (or (file-name-absolute-p file)
		  (setq file (expand-file-name file))) ; make defaults explicit
	      ;; Replace any invalid file-name characters (for the
	      ;; case of backing up remote files).
	      (setq file (expand-file-name (convert-standard-filename file)))
	      (if (eq (aref file 1) ?:)
		  (setq file (concat "/"
				     "drive_"
				     (char-to-string (downcase (aref file 0)))
				     (if (eq (aref file 2) ?/)
					 ""
				       "/")
				     (substring file 2)))))
	    ;; Make the name unique by substituting directory
	    ;; separators.  It may not really be worth bothering about
	    ;; doubling `!'s in the original name...
	    (expand-file-name
	     (replace-regexp-in-string "/" "!"
	      (replace-regexp-in-string "!" "!!" file))
	     backup-directory))
	(expand-file-name (file-name-nondirectory file)
			  (file-name-as-directory abs-backup-directory))))))

(when (and (eq fi::emacs-type 'xemacs20)
	   (not (fboundp 'make-backup-file-name-1)))
  (fset 'make-backup-file-name-1
	(symbol-function 'fi::make-backup-file-name-1)))
