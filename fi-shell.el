(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")

(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:explicit-shell-file-name nil
  "Explicit Shell image to invoke from (fi:shell).")

(defvar fi:explicit-shell-image-arguments nil
  "Explicit Shell image arguments when invoked from (fi:shell).")

(defvar fi:default-shell-file-name "sh"
  "Default Shell image to invoke from (fi:shell).")

(defvar fi:default-shell-image-arguments '("-i")
  "Default Shell image arguments when invoked from (fi:shell).")

(defvar fi:shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defun fi:shell-mode ()
  "Major mode for interacting with an inferior shell.  The shell
process-name is same as the buffer name, without asterisks.
\\[fi:subprocess-send-input] at end of buffer sends line as input.
\\[fi:subprocess-send-input] not at end copies rest of line to end and
sends it, starting with the prompt if is one or the beginning of the line
if there isn't. 

An input ring saves input sent to the shell subprocess. \\[fi:pop-input]
recalls previous input, travelling backward in the ring. \\[fi:push-input]
recalls previous input, travelling forward in the ring.
\\[fi:re-search-backward-input] searches backward in the input ring for a
previous input that contains a regular expression.
\\[fi:re-search-forward-input] searches forward in the input ring for a
previous input that contains a regular expression. \\[fi:list-input-ring]
lists the contents of the input ring.

The value of the variable `fi:shell-mode-map' is the local keymap used in
shell mode.

Keys that are bound to `fi:subprocess-superkey' invoke their bindings in
the map bound (a buffer-local) called `fi:subprocess-super-key-map' when at
the end of the buffer, otherwise they invoke their globally-bound
functions. The super-keymap is taken from the variable
`fi:shell-mode-super-key-map'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:shell-mode-hook', in this order and each with no args, if their values
are the names of functions.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
the buffer's default directory the same as the shell's working directory.
Variables `fi:shell-cd-regexp', `fi:shell-pushd-regexp' and
`fi:shell-popd-regexp' are used to match these command names, and are
buffer-local variables."
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

(defun fi:shell ()
  "Run an inferior shell, with input/output through buffer *shell*."
  (interactive)
  (fi::make-process "shell" "shell" 'fi:shell-mode))

(defun fi:another-shell ()
  "Run a new inferior shell, with input/output through buffer *shell-N*.
This function always creates a new subprocess and buffer."
  (interactive)
  (fi::make-process "shell" "shell" 'fi:shell-mode nil t))
