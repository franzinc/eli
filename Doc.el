;; $Header: /repo/cvs.copy/eli/Doc.el,v 1.45 1998/08/06 23:18:30 layer Exp $

;; This file is used to assemble documentation files
;; It is provided (in distributions) purely for informational purposes,
;; to allow possible usage for others assembling documentation.

(require 'cl)

(setq load-path (cons default-directory load-path))

(setq fi:lisp-do-indentation t)
(load "fi-site-init.el")

(defvar args)
(setq args (cdr (fi:member-equal "--" command-line-args)))
(setq command-line-args-left (butlast command-line-args-left 3))

(defvar input-file)
(setq input-file (car args))
(defvar output-file)
(setq output-file (car (cdr args)))

(message "input-file %s, output-file %s" input-file output-file)

(load "Doc0.elc")

(generate-eli-documentation input-file output-file)
