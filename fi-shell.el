;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and stored only in accordance with the terms of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure by the Government are subject to
;; restrictions of Restricted Rights for Commercial Software developed
;; at private expense as specified in DOD FAR 52.227-7013 (c) (1) (ii).
;;
;; This file may be distributed without further permission from
;; Franz Inc. as long as
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this package, please feel
;; free to contact Franz Inc. at
;;
;;	Franz Inc.
;;	Attn: Emacs Group Manager
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-shell.el,v 1.7 1988/05/19 16:24:13 layer Exp $

(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")

(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:shell-image-name "csh"
  "*Default Shell image to invoke from (fi:shell).")

(defvar fi:shell-image-arguments '("-i")
  "*Default Shell image arguments when invoked from (fi:shell).")

(defvar fi:shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defun fi:shell-mode ()
  "Major mode for interacting with an inferior shell."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:shell-mode)
  (setq mode-name "Shell")

  (if (null fi:shell-mode-super-key-map)
      (progn
	(setq fi:shell-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys fi:shell-mode-super-key-map 'shell)))

  (if (null fi:shell-mode-map)
      (setq fi:shell-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:shell-mode-super-key-map
				      'shell)))
  (use-local-map fi:shell-mode-map)
  (setq fi:subprocess-super-key-map fi:shell-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:shell (&optional buffer-number)
  "Start a sub-shell in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*shell'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*shell-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen.

The image file and image arguments are taken from the variables
`fi:shell-image-name' and `fi:shell-image-arguments'.

See fi:explicit-shell."
  (interactive "p")
  (fi::make-subprocess
   buffer-number "shell" 'fi:shell-mode
   fi:shell-prompt-pattern fi:shell-image-name fi:shell-image-arguments))

(defun fi:explicit-shell (&optional buffer-number
				    image-name image-arguments)
  "The same as fi:shell, except that the image and image arguments
are read from the minibuffer."
  (interactive "p\nsImage name: \nxImage arguments (a list): ")
  (fi::make-subprocess
   buffer-number "shell" 'fi:shell-mode fi:shell-prompt-pattern
   image-name image-arguments))
