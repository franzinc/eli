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

;; $Header: /repo/cvs.copy/eli/fi-shell.el,v 1.13 1991/03/12 18:30:30 layer Exp $

(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")

(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:shell-image-name "csh"
  "*Default Shell image to invoke from (fi:shell).  If the value
is a string then it names the image file or image path that
`fi:shell' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:shell-image-arguments '("-i")
  "*Default Shell image arguments when invoked from (fi:shell).")

(defvar fi:shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defun fi:shell-mode (&optional mode-hook)
  "Major mode for interacting with an inferior shell.
The keymap for this mode is bound to fi:shell-mode-map:
\\{fi:shell-mode-map}
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:shell-mode-hook

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
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
  "Start a shell in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*shell*'
and end with an optional \"<N>\".  If BUFFER-NUMBER is not given it defaults
to 1.  If BUFFER-NUMBER is 1, then the trailing \"<1>\" is omited.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen (a
buffer with no process attached to it.

The shell image file and image arguments are taken from the variables
`fi:shell-image-name' and `fi:shell-image-arguments'."
  (interactive "p")
  (fi::make-subprocess "shell"
		       buffer-number
		       default-directory
		       'fi:shell-mode
		       fi:shell-prompt-pattern
		       fi:shell-image-name
		       fi:shell-image-arguments))
