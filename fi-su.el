;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Id: fi-su.el,v 1.16 1996/10/30 17:59:55 layer Exp $

(defvar fi:su-mode-map nil
  "The su major-mode keymap.")

(defvar fi:su-mode-super-key-map nil
  "Used for super-key processing in su mode.")

(defvar fi:remote-su-mode-map nil
  "The su major-mode keymap.")

(defvar fi:remote-su-mode-super-key-map nil
  "Used for super-key processing in su mode.")

(defvar fi:su-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in su mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:su-initial-input "stty -echo nl tabs\n"
  "*The initial input sent to the su subprocess, after the first prompt
is seen.")

(defun fi:su-mode (&optional mode-hook)
  "Major mode for interacting with an inferior su.
The keymap for this mode is bound to fi:su-mode-map:
\\{fi:su-mode-map}
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:su-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:su-mode)
  (setq mode-name "Su")

  (if (null fi:su-mode-super-key-map)
      (progn
	(setq fi:su-mode-super-key-map (make-keymap))
	(fi::subprocess-mode-super-keys fi:su-mode-super-key-map 'shell)))

  (if (null fi:su-mode-map)
      (setq fi:su-mode-map
	(fi::subprocess-mode-commands (make-keymap)
				      fi:su-mode-super-key-map
				      'shell)))
  (use-local-map fi:su-mode-map)
  (setq fi:subprocess-super-key-map fi:su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:su-mode-hook))

(defun fi:remote-su-mode (&optional mode-hook)
  "Major mode for interacting with an remote inferior su.
The keymap for this mode is bound to fi:remote-su-mode-map:
\\{fi:remote-su-mode-map}
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:rlogin-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:remote-su-mode)
  (setq mode-name "Remote Su")

  (if (null fi:remote-su-mode-super-key-map)
      (progn
	(setq fi:remote-su-mode-super-key-map (make-keymap))
	(fi::subprocess-mode-super-keys fi:remote-su-mode-super-key-map
					'rlogin)))

  (if (null fi:remote-su-mode-map)
      (setq fi:remote-su-mode-map
	(fi::subprocess-mode-commands (make-keymap)
				      fi:remote-su-mode-super-key-map
				      'rlogin)))
  (use-local-map fi:remote-su-mode-map)
  (setq fi:subprocess-super-key-map fi:remote-su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:rlogin-mode-hook))

(defun fi:su (&optional buffer-number)
  "Start an su in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Su buffer names start with `*su*'
and end with an optional \"<N>\".  If BUFFER-NUMBER is not given it defaults
to 1.  If BUFFER-NUMBER is 1, then the trailing \"<1>\" is omited.  If
BUFFER-NUMBER is < 0, then the first available buffer name is chosen (a
buffer with no process attached to it."
  (interactive "p")
  (fi::make-subprocess nil
		       "root"
		       buffer-number
		       default-directory
		       'fi:su-mode
		       fi:su-prompt-pattern
		       "su"
		       nil
		       'fi::su-filter))

(defun fi:remote-root-login (&optional buffer-number host)
  "Start a remote root rlogin in a buffer whose name is determined from the
optional prefix argument BUFFER-NUMBER and the HOST.  Remote root Rlogin
buffer names start with `*root-HOST*' and end with an optional \"<N>\".  If
BUFFER-NUMBER is not given it defaults to 1.  If BUFFER-NUMBER is 1, then
the trailing \"<1>\" is omited.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen (a buffer with no process attached to it.

The host name is read from the minibuffer."
  (interactive "p\nsRemote host: ")
  (let ((fi:subprocess-env-vars
	 '(("EMACS" . "t")
	   ("TERM" . "dumb")
	   ("DISPLAY" . (getenv "DISPLAY")))))
    (fi::make-subprocess nil
			 (format "root-%s" host)
			 buffer-number
			 default-directory
			 'fi:remote-su-mode
			 fi:su-prompt-pattern
			 "rlogin"
			 (list host "-l" "root")
			 'fi::su-filter)))

(defvar fi::su-password nil)

(defun fi::su-filter (process output)
  "Filter for `fi:su' subprocess buffers.
Watch for the first shell prompt from the su, then send the
string bound to fi:su-initial-input, and turn ourself off."
  (fi::subprocess-filter process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (cond ((string-match "assword" output)
	   (setq fi::su-password (fi::read-password))
	   (send-string process (concat fi::su-password "\n")))
	  (t (if (save-excursion (beginning-of-line)
				 (looking-at fi::prompt-pattern))
		 (progn
		   (set-process-filter process 'fi::subprocess-filter)
		   (setq fi::su-password nil)
		   (if fi:su-initial-input
		       (send-string process fi:su-initial-input))))))))
