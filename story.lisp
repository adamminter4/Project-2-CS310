; from http://changingminds.org/disciplines/storytelling/plots/propp/propp.htm

; lint (used, set, loops)
; memoization
; compilation
; meta-interpreter for maths
; compile meta interpreter into lambda bodies


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; ==============================

(defparameter *grammar* nil)

(defun random-elt (choices)
  (elt choices (randi (length choices))))

(defun one-of (set)
  (list (random-elt set)))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generates (n phrase &optional (g *grammar*))
  (dotimes (i n)
    (setf *grammar* (eval g))
    (print (generate phrase))))

(defun generate (phrase)
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "return generate, and the rule name"
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun generate-all (phrase)
  "exhaustive enumeratoion of all sentences. 
   Warning: only use for short grammars"
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

;;;;; tests
(deftest !random-elt ()
  (reset-seed)
  (test 'cabbage (random-elt '(apples bananas cabbage))))

(defun !generate1-prim  (root g &optional (n 5))
  (setf *grammar* g)
  (reset-seed)
  (mapcar #'generate 
	  '(sentence sentence sentence 
	    sentence sentence sentence sentence)))
    
(deftest !generate1 ()
  (test
   '((GEORGE SAW THESE)
    (GEORGE SAW A ADIABATIC GREEN WOMAN ON THE
        ADIABATIC BIG WOMAN IN A LITTLE BIG
	    LITTLE BALL)
    (THE GREEN GREEN WOMAN IN LAURA HIT GEORGE WITH LAURA TO OBAMA)
    (THE WOMAN TOOK OBAMA IN LAURA) (GEORGE SAW THAT)
    (THE LITTLE GREEN TABLE WITH THESE WITH LAURA ON 
	 GEORGE IN GEORGE LIKED IT BY
	 A MAN ON GEORGE BY LAURA TO A TABLE)
    (THE MAN SAW A MAN WITH THESE ON THE GREEN MAN IN THE ADIABATIC MAN))
   (!generate1-prim 'sentence *grammar1*)))

(deftest !generate2 ()
  (!generate1-prim 'sentence *grammar2*))