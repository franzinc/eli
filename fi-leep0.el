;; See the file LICENSE for the full license governing this code.

;; The epoch side of presentations in a lisp-listener window.

;; This defstruct has to be in a separate file compiled and loaded
;; before the mail file because the cruftly compiler doesn't understand
;; a defstruct at compile time in the same file.

(defstruct presentation
  start
  end
  data
  subpresentation-vector)
