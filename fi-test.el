;; $Header: /repo/cvs.copy/eli/fi-test.el,v 1.1 1993/04/12 16:37:08 layer Exp $

(setq load-path (cons "/usr/emacs/lisp-interface" load-path))

(load "fi/site-init")

(defmacro test-equal (compare test)
  (list 'let (list (list 'result test))
	(list 'unless
	      (list 'equal compare 'result)
	      (list 'error
		    "test-equal test failed: wanted %s, got %s"
		    compare
		    'result))))

(defun fi:test ()
  (interactive)
  (test-equal '("12345612345" "6")
	      (fi::explode "123456123456" 1))
  (test-equal '("123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "" "" "")
	      (fi::explode "" 1))
  (test-equal '("a" "b" "c" "d" "e")
	      (fi::explode "abcde" 1))
  (test-equal '("aaaa")
	      (fi::explode "aaaa" 1))
  (test-equal '("aa" "bb" "cc" "dd" "ee")
	      (fi::explode "aabbccddee" 1))
  )

(fi:test)
(kill-emacs)
