;; Stuff grabbed and modified from the GNU Emacs sources.

;; Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.
;; Copyright (C) 1993 Franz Inc.

;; This file is (was) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; $Id: fi-gnu.el,v 2.3 1996/08/01 22:35:55 layer Exp $

(defun fi::find-most-recent-backup-file-name (fn)
  (if (eq version-control 'never)
      (make-backup-file-name fn)
    (let* ((base-versions (concat (file-name-nondirectory fn) ".~"))
	   (bv-length (length base-versions))
	   (possibilities (file-name-all-completions
			   base-versions
			   (file-name-directory fn)))
	   (versions (sort (mapcar 'backup-extract-version possibilities)
			   '<))
	   (high-water-mark (apply 'max (cons 0 versions)))
	   (deserve-versions-p (or version-control
				   (> high-water-mark 0))))
      (if (not deserve-versions-p)
	  (make-backup-file-name fn)
	(concat fn ".~" (int-to-string high-water-mark) "~")))))

;;(defun fi::backup-extract-version (fn)
;;  (if (and (string-match "[0-9]+~$" fn bv-length)
;;	   (= (match-beginning 0) bv-length))
;;      (string-to-int (substring fn bv-length -1))
;;      0))
