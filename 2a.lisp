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
(defparameter *Sch_Hash* (make-hash-table))

;Don't think I actually need this function. I will test it with and   ;
;and without when I get to that point.                                ;
(defun initializeHash ()
  (setf (gethash 'GECs *Sch_Hash*) 'GEC_List)
  (setf (gethash 'Year1 *Sch_Hash*) 'Year1_List)
  (setf (gethash 'Year2 *Sch_Hash*) 'Year2_List)
  (setf (gethash 'Year3 *Sch_Hash*) 'Year3_List))

;I wrote this as a seperate function for organization, but it also    ;
;kinda made sense to do. If my logic is correct, this won't break the ;
;chain of commands on the grammar. Once genSchedule gets to a GEC_#,  ;
;there isn't another exit. Only terminals.                            ;
(defun storeGEC (phrase)
  (let ((lst (gethash 'GECs *Sch_Hash*)))
    (push (random-elt (rewrites phrase)) lst)
    (setf (gethash 'GECs *Sch_Hash*) 'lst)))

;This is probably not going to work. Need to make sure those RegExs   ;
;work too...
(defun genSchedule (phrase)
  (cond ((listp phrase)
	 ;First checking to see if this catches a non-terminal that contains Year#.;
	 (if (eq (/^."Year"[1-3]) (car phrase))
	     ;If it does, then I parse out the Year# from the car of the phrase and set;
	     ;it to a dummy variable s;
	     ((setf (subseq (car phrase) (- (length (car phrase) 4) (length (car phrase)) s)))
	      ;From here, I grab my hash table, and map my dummy variable as the key.;
	      ;Not sure if this will actually work...                                ;
	      ;Then I just follow the same (mappend) function, as it will eventually ;
	      ;result in a list, which I set to the corresponding Year# key.         ;
	      ;I'm not worried about my GECs with this, as they will get caught in   ;
	      ;the recursive function call. I think.                                 ;
	      (setf (gethash '(s) *Sch_Hash*) (mappend #'genSchedule phrase)))
	     ;else, it's business as usual.;
	     (mappend #'genSchedule phrase)))
	
	((rewrite phrase)
	 ;Here's where I attempt to check if the phrase is a GEC_# non-terminal, which;
	 ;I know, leads to a non-terminal. So I pass it up to (storeGEC).             ;
	 (if (eq ("GEC_"[0-2]) phrase)
	     (storeGEC (phrase))
	 (genSchedule (random-elt (rewrites phrase)))))
	(t (list phrase))))

(defun !generate2-prim (type gram 10)
  (setf *grammar* gram)
  (reset-seed)
  (mapcar #'genSchedule
	  '(Schedule Schedule Schedule Schedule Schedule
	    Schedule Schedule Schedule Schedule Schedule)))

(defun generates2 (n phrase &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    ;Format the schedule here, using my hash tables;
    (print (genSchedule phrase))))