;Creates a Hash table to store the lists I get from generatesSchedule ;
;The GECs use the hash table to *hopefully* elimiate redundancy.      ;                                           
(defparameter *GEC_Hash* (make-hash-table))
(defparameter *grammar* nil)

(defun rewrites2 (category)
  (rule-rhs (assoc category *grammar*)))

;We need to initialize the hash before we use it, to effectively make ;
;it global.                                                           ;
(defun initializeHash ()
  (setf (gethash 'GECs *GEC_Hash*) 'GEC_Cache))

;I wrote this as a seperate function for organization, but it also    ;
;kinda made sense to do. If my logic is correct, this won't break the ;
;chain of commands on the grammar. Once genSchedule gets to a GEC_#,  ;
;there isn't another exit. Only terminals.                            ;
(defun cacheGEC (phrase)
  (let ((lst (gethash 'GECs *GEC_Hash*)))
    (let ((gec (random-elt (rule-rhs phrase))))
      (if (listp (member gec lst))
	  (cacheGEC phrase)
	  (setf (gethash 'GECs *GEC_Hash*) (push gec lst))))))

(defun generate2 (phrase)
  (cond ((listp phrase)
	 ;Here's where I attempt to check if the phrase is a GEC_# non-terminal, which;
	 ;I know, leads to a non-terminal. So I pass it up to (storeGEC).             ;
	 (if (numberp (search "GEC" (rule-lhs phrase)))
	     (cacheGEC phrase)
	     (mappend #'generate2 phrase)))
	((rewrites2 phrase)
	 (generate2 (random-elt (rewrites2 phrase))))
	(t (list phrase))))

(defun generates2 (n phrase &optional (g *grammar*))
  (dotimes (i n)
    (initializeHash)
    (setf *grammar* g)
    (print (list (generate2 phrase) (gethash 'GECs *GEC_Hash*)))))


;***********Tests****************;

(deftest !generate3 ()
  (reset-seed)
  (initializeHash)
  (test nil (generates2 10 'Schedule *grammar3*)))

(deftest !raindi ()
  (reset-seed)
  (test 9 (randi 10)))

(deftest !reset-seed ()
  (test 10013 (reset-seed)))

(deftest !shuffle ()
  (reset-seed)
  (test '(adam bro dude brah) (shuffle '(bro dude adam brah))))

(deftest !random-elt ()
  (reset-seed)
  (test 'brah (random-elt '(bro dude adam brah))))

(deftest !one-of ()
  (reset-seed)
  (test '(brah) (one-of '(bro dude adam brah))))

(deftest !mappend ()
  (test '(2 3 5 6) (mappend #'cdr '((1 2 3) (4 5 6)))))