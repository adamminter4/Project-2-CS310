(defparameter *grammar* nil)
(defparameter *testgrammarFAIL*
  '((!Test -> (!Awesome !Dolphin)
    (!Awesome -> (adam minter is awesome) !PaintingStuff)
    (!PaintingStuff -> )
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



;1.) DefTests for Terminalp function;
(deftest !terminalp0 ()
  (test t (terminalp "!fTw")))

(deftest !terminalp1 ()
  (test nil (terminalp "cat")))

(defun terminalp (nonterm)
  (let ((i (char nonterm 0)))
    (and (symbolp i)
	 (eql (char (symbol-name i) 0) !\!))))



;2.) Deftests for Undefined-nonterminal function;
(deftest !undefined-nonterminal0 () 
  (test '(PaintingStuff RickSantourm) undefined-nonterminal *testgrammarFAIL*))

(deftest !undefined-nonterminal1 ()
  (test nil undefined-nonterminal *testgrammarWIN*))

(defun undefined-nonterminal (g)
  (setf *grammar* g))



;3.) Deftests for Unused-Rewrite Function;
(deftest !unused-rewrite () 
  (test '(Lard Batman) (unused-rewrite *testgrammarFAIL)))

(deftest !unused-rewrite ()
  (test nil (unused-rewrite *testgrammarWIN*)))
  
(defun unused-rewrite (g)
  (setf *grammar* g)
  

   