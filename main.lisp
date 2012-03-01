(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"grammars.lisp"
		"story.lisp"
		"2a.lisp"
		"2b.lisp"
		"2c.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () 
  (tests))


(defun hello (&optional (who "world"))
	(format nil "hello ~a~%" who))
