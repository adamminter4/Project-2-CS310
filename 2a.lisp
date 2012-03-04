(deftest !generate2 ()
  (test ;some list;
   (!generate2-prim 'Schedule *grammar3*)))

;Creates a Hash table to store the lists I get from generatesSchedule ;
;The GECs use the hash table to *hopefully* elimiate redundancy. The  ;
;other keys in the hash table are for storing the different years that;
;the schedule creates. It's really only used for formatting at the end;
;in the generates2 function. If this takes more than 2 hours to fix,  ;
;if my code doesn't work, I'm scrapping the year keys. I'm really not ;
;sure if those will work.                                             ;
(defparameter *GEC_Hash* (make-hash-table))

;Don't think I actually need this function. I will test it with and   ;
;and without when I get to that point.                                ;
(defun initializeHash ()
  (setf (gethash 'GECs *GEC_Hash*) 'GEC_List)

;I wrote this as a seperate function for organization, but it also    ;
;kinda made sense to do. If my logic is correct, this won't break the ;
;chain of commands on the grammar. Once genSchedule gets to a GEC_#,  ;
;there isn't another exit. Only terminals.                            ;
(defun cacheGEC (phrase)
  (let ((lst (gethash 'GECs *GEC_Hash*)))
    (let ((gec (random-elt (rule-rhs phrase))))
      (if (listp (member gec lst))
	  (cacheGEC phrase)
	  ((push gec lst) 
	   (setf (gethash 'GECs *GEC_Hash*) 'lst))))))

(defun genSchedule (phrase)
  (cond ((listp phrase)
	  (mappend #'genSchedule phrase)))
	((rewrite phrase)
	 ;Here's where I attempt to check if the phrase is a GEC_# non-terminal, which;
	 ;I know, leads to a non-terminal. So I pass it up to (storeGEC).             ;
	 (if (search "GEC" phrase)
	     (storeGEC (phrase))
	 (genSchedule (random-elt (rewrites phrase)))))
	(t (list phrase (gethash 'GECs *GEC_Hash))))

(defun !generate2-prim (type gram 10)
  (setf *grammar* gram)
  (reset-seed)
  (mapcar #'genSchedule
	  '(Schedule Schedule Schedule Schedule Schedule
	    Schedule Schedule Schedule Schedule Schedule)))

(defun generates2 (n phrase &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    (print (genSchedule phrase))))