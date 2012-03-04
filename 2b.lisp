(defparameter *grammar* nil)
(defparameter *testgrammarFAIL*
  '((!Test -> (!Awesome !Dolphin !Basketball)
    (!Awesome -> ((adam minter is awesome) !PaintingStuff))
    (!StupidTerm ->   )
    (!Lard ->  (i liek turtles) (i heard u liek mudkips))
    (!Batman -> (better have his back broken by Bane in Dark Knight Rises))
    (!Dolphin -> (dolphins rule))
    (!RickSantorum -> ))))

(defparameter *testgrammarWIN*
  '((!Test -> (!Awesome !Dolphin !Batman)
     (!Awesome -> (adam minter is awesome) !Bro)
     (!Bro -> (andrew butcher told me that tim and will are bros))
     (!Awesome -> sweet dude (!Awesome !Awesome))
     (!Batman -> (seriously better have his back broken in Dark Knight Rises)))))

(defun rewrites2b (phrase)
  (rule-rhs (assoc phrase *grammar*)))

;1.) DefTests for Terminalp function;
(deftest !terminalp0 ()
  (test t (terminalp "!fTw")))

(deftest !terminalp1 ()
  (test nil (terminalp "cat")))

(defun terminalp (nonterm)
  (let ((i (char nonterm 0)))
    (and (symbolp i)
	 (eql (char (symbol-name i) 0) #\!))))


;2.) Deftests for Undefined-nonterminal function;
(deftest !undefined-nonterminal0 (&aux (root (!Test))) 
  (test '(!PaintingStuff !Basketball) (!undefined-nonterm-list root *testgrammarFAIL*)))

(deftest !undefined-nonterminal1 (&aux (root (!Test)))
  (test nil (!undefined-nonterm-list root *testgrammarWIN*)))

(defun generatesUndefTerm (10 phrase &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    (undefined-nonterminal (phrase))))

(defun undefined-nonterminal (phrase)
  (let ((lst nil))
    (cond ((listp phrase)
	   (mappend #'undefined-nonterminal phrase))
	  (((terminalp (car phrase))
	    (if (eql (rule-rhs phrase) nil)  
		(list (car phrase) lst)
		(undefined-nonterminal (rewrites2b phrase)))))
	  )
    lst))

(defun !undefined-nonterm-list (root g &optional (n 5))
  (setf *grammar* g)
  (reset-seed)
  (remove-duplicates (list (mapcar #'undefined-nonterminal '(!Test !Test !Test)))))


;3.) Deftests for Unused-Rewrite Function;
(deftest !unused-rewrite () 
  (test '(Lard Batman) (unused-rewrite *testgrammarFAIL)))

(deftest !unused-rewrite ()
  (test nil (unused-rewrite *testgrammarWIN*)))
  
(defun unused-rewrite (g)
  (setf *grammar* g)
  

   