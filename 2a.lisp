(deftest !generate2 ()
  (test ;some list;
   (!generate2-prim 'Schedule *grammar3*)))

(defparameter *Sch_Hash* (make-hash-table))

(defun initializeHash ()
  (setf (gethash 'GECs *Sch_Hash*) 'GEC_List)
  (setf (gethash 'Year1 *Sch_Hash*) 'Year1_List)
  (setf (gethash 'Year2 *Sch_Hash*) 'Year2_List)
  (setf (gethash 'Year3 *Sch_Hash*) 'Year3_List))

(defun storeGEC (phrase)
  (let ((lst (gethash 'GECs *Sch_Hash*)))
    (push (random-elt (rewrites phrase)) lst)
    (setf (gethash 'GECs *Sch_Hash*) 'lst)))
  
(defun genSchedule (phrase)
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((rewrite phrase)
	 (if (eq ("GEC_"[0-2]) phrase)
	     (storeGEC (phrase)))
	 (generate (random-elt (rewrites phrase))))
	(t (list phrase))))


(defun !generate2-prim (type gram 10)
  (setf *grammar* gram)
  (reset-seed)
  (mapcar #'generateSchedule
	  '(Schedule Schedule Schedule Schedule Schedule
	    Schedule Schedule Schedule Schedule Schedule)))

(defun generates2 (n phrase &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    ;Format the schedule here, using my hash tables;
    (print (gernerateSchedule phrase))))