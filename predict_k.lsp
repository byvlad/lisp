(DEFUN isCharEqualsEpsilon (char)
	(
		(COND
			((EQL char e) T)
			(T NIL)
		)
	)
)

(DEFUN isCharATerminal (char rules)
	(
		(COND
			((NULL rules) T)
			((EQL char (CAAR rules)) NIL)
			(T (isCharATerminal char (CDR rules)))
		)
	)
)

(DEFUN getRulesWithAInLeftSide (a rules)
	(
		(COND
			((NULL rules) NIL)
			((EQL a (CAAR rules)) (CONS (CAR rules) (getRulesWithAInLeftSide a (CDR rules))))
			(T (getRulesWithAInLeftSide a (CDR rules)))
		)
	)
)

(DEFUN isEpsilonInARightSideOfRule (rightside)
	(
		(COND
			((NULL rightside) NIL)
			((EQL e (CAR rightside)) T)
			(T (isEpsilonInARightSideOfRule (CDR rightside)))
		)
	)
)

(DEFUN getRightSidesOfRulesFromList (rules)
	(
		(COND
			((NULL rules) NIL)
			(T (CONS (CADAR rules) (getRightSidesOfRulesFromList (CDR rules))))
		)
	)
)

(DEFUN deleteEpsilonFromRightSide (rightside)
	(
		(COND
			((NULL rightside) NIL)
			((EQL e (CAR rightside)) (deleteEpsilonFromRightSide (CDR rightside)))
			(T (CONS (CAR rightside) (deleteEpsilonFromRightSide (CDR rightside))))
		)
	)
)

(DEFUN predictForOneNonTerminal (a rightside rules)
	(
		(COND
			((NULL rightside) e)
			((isEpsilonInARightSideOfRule (predict (CAR rightside) rules))
				(APPEND (deleteEpsilonFromRightSide (first (CAR rightside) rules)) (predictForOneNonTerminal a (CDR rightside) rules))
			)
			(T (predict (CAR rightside) rules))
		)
	)
)

(DEFUN predictForAllNonTerminals (a rightsides rules)
	(
		(COND
			((NULL rightsides) NIL)
			(T (CONS (predictForOneNonTerminal a (CAR rightsides) rules) (predictForAllNonTerminals a (CDR rightsides) rules)))
		)
	)
)

(DEFUN predict (a rules)
	(
		(COND
			((isCharEqualsEpsilon a) e)
			((isCharATerminal a rules) a)
			(T (predictForAllNonTerminals a (getRightSidesOfRulesFromList (getRulesWithAInLeftSide a rules)) rules))
		)
	)
)

(DEFUN predict-k (seq rules)
	(
		(predict s (CONS (CONS s (LIST seq)) rules))
	)
)

(predict-k '(a w ) '((a (b c)) (b (ab)) (a (e))))
(predict-k '(b q w) '((a (b)) (b (c)) (c (x t))))
(predict-k '(a w w q) '((a (q w e)) (q (f)) (w (b n)) (a (e))))
(predict-k '(q w z) '((a (q w e)) (q (e)) (w (b n)) (a (e)) (q (f))))
