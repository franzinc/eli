;;
;; copyright (C) 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-su.el,v 1.7 1991/03/12 18:30:15 layer Exp $

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

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:su-mode)
  (setq mode-name "Su")

  (if (null fi:su-mode-super-key-map)
      (progn
	(setq fi:su-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys fi:su-mode-super-key-map 'shell)))

  (if (null fi:su-mode-map)
      (setq fi:su-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:su-mode-super-key-map
				      'shell)))
  (use-local-map fi:su-mode-map)
  (setq fi:subprocess-super-key-map fi:su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:su-mode-hook))

(defun fi:remote-su-mode (&optional mode-hook)
  "Major mode for interacting with an inferior su."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:remote-su-mode)
  (setq mode-name "Remote Su")

  (if (null fi:remote-su-mode-super-key-map)
      (progn
	(setq fi:remote-su-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys fi:remote-su-mode-super-key-map
					'rlogin)))

  (if (null fi:remote-su-mode-map)
      (setq fi:remote-su-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:remote-su-mode-super-key-map
				      'rlogin)))
  (use-local-map fi:remote-su-mode-map)
  (setq fi:subprocess-super-key-map fi:remote-su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:rlogin-mode-hook))

(defun fi:su (&optional buffer-number)
  "Start an su in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*root'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*root-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen."
  (interactive "p")
  (fi::make-subprocess "root"
		       buffer-number
		       default-directory
		       'fi:su-mode
		       fi:su-prompt-pattern
		       "su"
		       nil
		       'fi::su-filter))

(defun fi:remote-root-login (&optional buffer-number host)
  "Start an su in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*root'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*root-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen."
  (interactive "p\nsRemote host: ")
  (let ((fi:subprocess-env-vars
	 '(("EMACS" . "t")
	   ("TERM" . "dumb")
	   ("DISPLAY" . (getenv "DISPLAY")))))
    (fi::make-subprocess (format "root-%s" host)
			 buffer-number
			 default-directory
			 'fi:remote-su-mode
			 fi:su-prompt-pattern
			 "rlogin"
			 (list host "-l" "root")
			 'fi::su-filter)))

(defun fi::su-filter (process output)
  "Filter for `fi:su' subprocess buffers.
Watch for the first shell prompt from the su, then send the
string bound to fi:su-initial-input, and turn ourself off."
  (fi::subprocess-filter process output)
  (switch-to-buffer (process-buffer process))
  (cond ((string-match "assword" output)
	 (setq password (fi::read-password))
	 (send-string process (concat password "\n")))
	(t (if (save-excursion (beginning-of-line)
			       (looking-at subprocess-prompt-pattern))
	       (progn
		 (set-process-filter process 'fi::subprocess-filter)
		 (if fi:su-initial-input
		     (send-string process fi:su-initial-input)))))))
