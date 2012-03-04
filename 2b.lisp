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

(defun terminalp (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\!)))



;2.) Deftests for Undefined-nonterminal function;
(deftest !undefined-nonterminal () 
  (test '(!StupidTerm !RickSantorum) (undefined-nonterminal *testgrammarFAIL*)))

(defun undefined-nonterminal (gram)
  (setf *grammar* gram)
  (let (lst)
    (dolist (i gram)
      (if (and (terminalp (car i))
	       (null (cdr (cdr i))))
	  (push (car i) lst)
	  t)
      )
    (remove nil (reverse (flatten lst)))))



;3.) Deftests for Unused-Rewrite Function;
(deftest !unused-rewrite () 
  (test '(!StupidTerm !Lard !Batman !RickSantorum) (unused-rewrites *testgrammarFAIL*)))

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