(defparameter *grammar* nil)

(deftest !terminalp ()
  (test t (terminalp "!fTw")))

(deftest !terminalp ()
  (test nil (terminalp "cat")))

(defun terminalp (nonterm)
  (let ((i (char nonterm 0)))
    (and (symbolp i)
	 (eql (char (symbol-name i) 0) !\!))))

(deftest !undefined-nonterminal (&aux (gram '((Grammar -> Awesome Lard Dolphin)
					      (Awesome -> (adam minter is awesome) PaintingStuff)
					      (Dolphin -> (dolphins rule) (RickSantorum)))))
  (test Lard undefined-nonterminal(gram)))

(defun undefined-nonterminal (g)
  (setf *grammar* g))
  


   