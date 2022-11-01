;; The Franz Inc. Lisp/Emacs interface.

(defvar fi:emacs-lisp-interface-version "5.00")

(when (not load-file-name)
  (error "load-file-name is nil... how are you loading fi-site-init??"))

(when (< emacs-major-version 23)
  (error "ELI requires GNU Emacs 23 or later."))

(setq fi::*debug* nil)

(require 'cl-lib)

(defun on-ms-windows ()
  (or (cygwinp)
      (memq system-type '(windows-nt ms-windows ms-dos win386))))

(defun cygwinp ()
  (memq system-type '(cygwin cygwin32)))

(defvar fi::load-subprocess-files t)
(defvar fi::install-acl-menubar t)
(defvar fi::build-time nil)

(defvar fi::*current-frame-width* nil)
(defvar fi::*new-frame-width* nil)
(when (fboundp 'frame-width)
  (ignore-errors (setq fi::*current-frame-width* (1- (window-width)))))

(defvar fi::library-directory nil)
(setq fi::library-directory (file-name-directory load-file-name))

(defvar fi::initialization-forms nil)
(defun fi::initialize-emacs-lisp-interface ()
  (when fi::initialization-forms
    (dolist (form (nreverse fi::initialization-forms))
      (eval form))
    (setq fi::initialization-forms nil)))

(defvar fi:compile-at-load-time t
  "If non-nil, .el files are compiled when ELI is loaded.")

(defvar fi--force-compile nil)

(defvar fi:native-comp-available-p (and (>= emacs-major-version 28)
                                        (fboundp 'native-comp-available-p)
                                        (native-comp-available-p)))

(defvar fi--eli-cache-dir
  (expand-file-name (format "~/.emacs.d/franzinc/%d.%d/eli/"
	                    emacs-major-version
	                    emacs-minor-version)))

(make-directory fi--eli-cache-dir t)

(defun fi::load (file)
  (let* ((el-path  (format "%s%s.el"  fi::library-directory file))
         (compiled-el-path (if fi:native-comp-available-p
                               (format "%s%s.eln" fi--eli-cache-dir     file)
                             (format   "%s%s.elc" fi--eli-cache-dir     file)))
         (byte-compile-dest-file-function
          #'(lambda (file)
              file ;; ignored
              compiled-el-path)))
    (or (file-exists-p el-path) (error "File %s does not exist." el-path))
    (cond (fi:compile-at-load-time
           (cond ((or fi--force-compile
                      (file-newer-than-file-p el-path compiled-el-path))
                  (if fi:native-comp-available-p
                      (load (native-compile el-path compiled-el-path))
                    (byte-compile-file el-path t)))
                 (t (load compiled-el-path))))
          (t (load el-path)))))

(fi::load "fi-vars")
(fi::load "fi-utils")
(fi::load "fi-keys")
(fi::load "fi-modes")
(and fi:lisp-do-indentation (fi::load "fi-indent"))
(fi::load "fi-gnu")

(when (not fi::load-subprocess-files)
  (defvar fi:package nil)
  (make-variable-buffer-local 'fi:package)
  ;; used in Common Lisp edit mode:
  (setq fi::common-lisp-backdoor-main-process-name nil))

(when fi::load-subprocess-files
  (fi::load "fi-subproc")
  (fi::load "fi-sublisp")
  (fi::load "fi-basic-lep")
  (fi::load "fi-lep")
  (fi::load "fi-dmode")
  (fi::load "fi-composer")
  (fi::load "fi-lze")
  (fi::load "fi-changes")
  (fi::load "fi-db")
  (fi::load "fi-stream")
  (fi::load "fi-ring")
  (fi::load "fi-filec")
  (fi::load "fi-shell")
  (fi::load "fi-rlogin")
  (fi::load "fi-telnet")
  (fi::load "fi-su")
  (fi::load "fi-emacs21"))

(fi::load "fi-manual")

(condition-case nil
    (fi::load "localfidev")
  (error nil))

(setq fi:package-loaded t)

(defun fi::top-level ()
  (fi::initialize-emacs-lisp-interface)
  (eval fi::build-time))

(if fi::build-time
    (progn
      (setq fi::build-time top-level)
      (setq top-level '(fi::top-level)))
  (fi::initialize-emacs-lisp-interface))

(provide 'fi-site-init)
