;Creates a Hash table to store the lists I get from generatesSchedule ;
;The GECs use the hash table to *hopefully* elimiate redundancy.      ;                                           
(defparameter *GEC_Hash* (make-hash-table))

;We need to initialize the hash before we use it, to effectively make ;
;it global.                                                           ;
(defun initializeHash ()
  (setf (gethash 'GECs *GEC_Hash*) 'GEC_List))

;I wrote this as a seperate function for organization, but it also    ;
;kinda made sense to do. If my logic is correct, this won't break the ;
;chain of commands on the grammar. Once genSchedule gets to a GEC_#,  ;
;there isn't another exit. Only terminals.                            ;
(defun cacheGEC (phrase)
  (let ((lst (gethash 'GECs *GEC_Hash*)))
    (let ((gec (random-elt (rule-rhs phrase))))
      (if (listp (member gec lst))
	  (cacheGEC phrase)
	  (setf (gethash 'GECs *GEC_Hash*) '(push gec lst))))))

(defun genSch (phrase)
  (cond ((listp phrase)
	 ;Here's where I attempt to check if the phrase is a GEC_# non-terminal, which;
	 ;I know, leads to a non-terminal. So I pass it up to (storeGEC).             ;
	 (if (numberp (search "GEC" (rule-lhs phrase)))
	     (cacheGEC (phrase))
	     (mappend #'genSch phrase)))
	((rewrite phrase)
	 (genSch (random-elt (rewrites phrase))))
	(t (list phrase (gethash 'GECs *GEC_Hash*)))))

(defun generatesSch (n phrase &optional (g *grammar*))
  (initializeHash)
  (dotimes (i n)
    (setf *grammar* (eval g))
    (print (genSch phrase))))

;***********Tests****************;

(defun !generate2-prim (type g &optional (n 2))
  (setf *grammar* g)
  (reset-seed)
  (mapcar #'genSchedule
	  '(Schedule Schedule Schedule Schedule Schedule
	    Schedule Schedule Schedule Schedule Schedule)))


(deftest !genSchedule1 ()
   (!generate2-prim 'Schedule *grammar3*))

(deftest !raindi ()
  (reset-seed)
  (test (9 0.2163) (randi 10)))

(deftest !reset-seed ()
  (test 10013 (reset-seed)))

(deftest !shuffle ()
  (test '(brah bro dude adam) (shuffle '(bro dude adam brah))))

(deftest !random-elt ()
  (reset-seed)
  (test 'brah (random-elt '(bro dude adam brah))))

(deftest !one-of ()
  (reset-seed)
  (test (listp (one-of '(bro dude adam brah)))))

(deftest !mappend ()
  (test '(2 3 5 6) (mappend #'cdr '((1 2 3) '(4 5 6)))))