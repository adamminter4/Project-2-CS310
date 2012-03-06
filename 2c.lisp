(defparameter *grammar* nil)

(defparameter *Story_Hash* (make-hash-table))

(defun initializeStoryCache ()
  (setf (gethash 'cache *Story_Hash*) nil))

(deftest !generates ()
  (reset-seed)
  (test nil (generates 10 'Start '*grammar4*)))

(deftest !storyCache ()
  (reset-seed)
  (initializeStoryCache)
  (storyCache 'BOY *grammar2*)
  (test nil (generates 10 'sentence '*grammar*)))
  
(defun storyCache (nonterm g)
  (setf *grammar* g)
  (setf (gethash 'cache *Story_Hash*) (random-elt (cdr (cdr (assoc nonterm *grammar*)))))
  (rplacd (assoc nonterm *grammar*) (list '-> (gethash 'cache *Story_Hash*))))
	 
  