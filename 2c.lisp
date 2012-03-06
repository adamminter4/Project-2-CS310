(defparameter *grammar* nil)

(deftest !generates ()
  (reset-seed)
  (test nil (generates 10 'Start '*grammar4*)))

(deftest !storyCache ()
  (reset-seed)
  (storyCache 'BOY *grammar2*)
  (test nil (generates 10 'sentence '*grammar*)))
  
(defun storyCache (nonterm g)
  (setf *grammar* g)
  (let ((a (random-elt (cdr (cdr (assoc nonterm *grammar*))))))
  (rplacd (assoc nonterm *grammar*) (list '-> a))))
	 
  