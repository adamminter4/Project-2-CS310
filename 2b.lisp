(defparameter *terms_Hash* (make-hash-table))

(defparameter *grammar* nil)

(defparameter *testgrammarFAIL*
  '((!Test -> (!Awesome !Dolphin !Basketball))
    (!Awesome -> ((adam minter is awesome) !PaintingStuff))
    (!StupidTerm ->   )
    (!Lard ->  (i liek turtles) (i heard u liek mudkips))
    (!Batman -> (better have his back broken by Bane in Dark Knight Rises))
    (!Dolphin -> (dolphins rule))
    (!RickSantorum -> )))

(defun get-terms (gram)
  (setf *grammar* gram)
  (let ((terms nil) (nonterms nil) (rhs nil)) 
      (dolist (i *grammar*)
	(if (terminalp (car i))
	    (setf nonterms (list (car i) nonterms))
	    (let ((temp (flatten (rest (rest i)))))
	      (dolist (j (flatten temp))
		(if (terminalp j)
		    (setf rhs (list j rhs))
		    (setf terms (list j terms)))))))
      (setf (gethash 'NonTerms *terms_Hash*) '(remove-duplicates nonterms))
      (setf (gethash 'RHS_NonTerms *terms_Hash*) '(remove-duplicates rhs))
      (setf (gethash 'Terminals *terms_Hash*) '(remove-duplicates terms))))



;1.) DefTests for Terminalp function;
(deftest !terminalp ()
  (test t (terminalp "!TEST")))

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
	  (setf lst (list (car i) lst))
	  t)
      )
    (print (remove nil (flatten (reverse lst))))))



;3.) Deftests for Unused-Rewrite Function;
(deftest !unused-rewrite () 
  (test '(!StupidTerm !RickSantorum) (unused-rewrites *testgrammarFAIL*)))

(defun unused-rewrites (gram)
  (setf *grammar* gram)
  (get-terms *grammar*)
  (let ((lst nil) (lhs (gethash 'NonTerms *terms_Hash*)) (rhs (gethash 'RHS_NonTerms *terms_Hash*)))
    (dolist (i rhs)
      (if (listp (member i lhs))
	  t
	  (setf lst (list i lst))))
    lst))