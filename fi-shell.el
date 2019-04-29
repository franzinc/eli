;; See the file LICENSE for the full license governing this code.


(defun fi:shell-mode (&optional mode-hook)
  "Major mode for interacting with an inferior shell.
The keymap for this mode is bound to fi:shell-mode-map:

<font face=\"Courier New\">\\{fi:shell-mode-map}</font>
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:shell-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (fi::kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:shell-mode)
  (setq mode-name "Shell")
  (fi::initialize-mode-map 'fi:shell-mode-map 'fi:shell-super-key-map 'shell)
  (use-local-map fi:shell-mode-map)
  (run-mode-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:shell-mode-bang (&optional arg)
  "Expand !$ in shell mode."
  (interactive "*p")
  (message "!-")
  (let ((c (read-char)))
    (cond
     ;;((= c ?!) (fi:pop-input arg))
     ((= c ?$) (fi:pop-input-last-word arg))
     (t (insert "!")
	;;(setq unread-command-char c)
	(insert-char c 1)))))

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
  (fi::make-subprocess nil
		       "shell"
		       buffer-number
		       default-directory
		       'fi:shell-mode
		       fi:shell-prompt-pattern
		       fi:shell-image-name
		       fi:shell-image-arguments
		       (when (on-ms-windows)
			 'fi::subprocess-dos-filter)))

(defun fi::subprocess-dos-filter (process output &optional stay cruft)
  (fi::subprocess-filter process output stay t))
