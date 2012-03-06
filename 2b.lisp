(defparameter *terms_Hash* (make-hash-table))

(defparameter *grammar* nil)

(defparameter *testgrammarFAIL*
  '((!Test -> (!Awesome !Dolphin !Basketball))
    (!Awesome -> ((adamMinterIsAwesome) !PaintingStuff))
    (!StupidTerm ->   )
    (!Lard ->  (iLiekTurtles) (iHeardULiekMudkips))
    (!Batman -> (betterHaveHisBackBrokenByBaneInDarkKnightRises))
    (!Dolphin -> (dolphinsRule))
    (!RickSantorum -> )))

(defun get-terms (gram)
  (setf *grammar* gram)
  (let ((terms nil) (nonterms nil) (rhs nil))
      (dolist (i *grammar*)
	(if (terminalp (car i))
	    (push (car i) nonterms)
	    nil)
	(let ((temp (flatten (rest (rest i)))))
	  (dolist (j temp)
	    (if (terminalp j)
		(push j rhs)
		(push j terms)))))
      (setf (gethash 'NonTerms *terms_Hash*) (remove nil  
						     (reverse 
						      (flatten 
						       (remove-duplicates nonterms)))))
      (setf (gethash 'RHS_NonTerms *terms_Hash*) (remove-duplicates rhs))
      (setf (gethash 'Terminals *terms_Hash*) (remove nil (remove-duplicates terms)))))



;1.) DefTests for Terminalp function;
(deftest !terminalp ()
  (test t (terminalp '!TEST)))

;I need to clarify something: When I initially wrote this function, and the scifi grammar, I;
;wrote all of my NON-TERMINALS to have ! marks as the first character. I later learned that ;
;this isn't what I was supposed to do, but instead put an ! in front of all terminals. The  ;
;code and my tests all still work the same, but I just test for non-terminals, instead of   ;
;terminals. Sorry for any confusion...                                                      ;
(defun terminalp (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\!)))



;2.) Deftests for Undefined-nonterminal function;
(deftest !undefined-nonterminal () 
  (test '(!HappyEnding !AndTakeAFewAndLeave !IsStressed) (undefined-nonterminal *grammar4*)))

(defun undefined-nonterminal (gram)
  (setf *grammar* gram)
  (get-terms *grammar*)
  (let ((lst nil) (lhs (gethash 'NonTerms *terms_Hash*)) (rhs (gethash 'RHS_NonTerms *terms_Hash*)))
    (pop lhs)
    (dolist (i rhs)
      (if (null (member i lhs))
	  (setf lst (list i lst))
	  t))
    (remove nil (reverse (flatten lst)))))



;3.) Deftests for Unused-Rewrite Function;
(deftest !unused-rewrite () 
  (test '(!WantSomething !Dine !Denoument?) (unused-rewrites *grammar4*)))

(defun unused-rewrites (gram)
  (setf *grammar* gram)
  (get-terms *grammar*)
  (let ((lst nil) (lhs (gethash 'NonTerms *terms_Hash*)) (rhs (gethash 'RHS_NonTerms *terms_Hash*)))
    (pop lhs)
    (dolist (i lhs)
      (if (null (member i rhs))
	  (setf lst (list i lst))
	  t))
    (remove nil (reverse (flatten lst)))))