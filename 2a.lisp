(deftest !generate2 ()
  (test ;some list;
   (!generate2-prim 'Schedule *grammar3*)))

(defun !generate2-prim (type gram 10)
  (setf *grammar* gram)
  (reset-seed)
  (mapcar #'generate2
	  '(Schedule Schedule Schedule Schedule Schedule
	    Schedule Schedule Schedule Schedule Schedule)))

(defun generates2 (n type &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    (print (gernerate type))))

(defun genSchedule (type)
  (cond ((listp type)
	 (mappend #'generate type))
	((rewrites type)
	 (generate (random-elt (rewrites type))))
	(t (list phrase))))