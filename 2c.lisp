(defparameter *grammar* nil)

(defparameter *Story_Hash* (make-hash-table))

(defun storyCache (nonterm g)
  (setf *grammar* g)
  (setf (gethash 'cache *Story_Hash*) (random-elt (cdr (cdr nonterm))))
  (dolist (i *grammar*)
    (cond ((eql (car i) nonterm) 
	   (dolist (j (cdr (cdr i)))
	     (setf j (gethash 'cache *Story_Hash*)))
	   (remove-duplicates i)))))
    
  