
(clear-all)

(defclass plateau()
   (case11
   case12
   case13
   case21
   case22
   case23
   case31
   case32
   case33)
)

(defvar *plateau* (make-instance 'plateau))

(defun initplateau()	
	(setf (slot-value *plateau* 'case11) "E")
	(setf (slot-value *plateau* 'case12) "E")
	(setf (slot-value *plateau* 'case13) "E")
	(setf (slot-value *plateau* 'case21) "E")
	(setf (slot-value *plateau* 'case22) "E")
	(setf (slot-value *plateau* 'case23) "E")
	(setf (slot-value *plateau* 'case31) "E")
	(setf (slot-value *plateau* 'case32) "E")
	(setf (slot-value *plateau* 'case33) "E")
)

(defun createBlockBoard(x y)
	(if (oddp (+ x y))
		(setf ligne (+ 1 (act-r-random 2))) ;si le coup est sur une case sans diagonale
		(if (eql x 2)
			(setf ligne (+ 1 (act-r-random 4))) ;si le coup est au milieu
			(setf ligne (+ 1 (act-r-random 3))) ;sinon
		)
	)
	
	(initplateau)
	
	(case ligne
		(1 (case x
			(1 (case y
				(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case13) "O")))
				(2 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case13) "O")))
				(3 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case13) "E")))
				)
			)
			(2 (case y
				(1 (progn (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case23) "O")))
				(2 (progn (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case23) "O")))
				(3 (progn (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case23) "E")))
				)
			)
			(3 (case y
				(1 (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case32) "O") (setf (slot-value *plateau* 'case33) "O")))
				(2 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case32) "E") (setf (slot-value *plateau* 'case33) "O")))
				(3 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case32) "O") (setf (slot-value *plateau* 'case33) "E")))
				)
			)
			)
		)
		(2 (case y
			(1 (case x
				(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case31) "O")))
				(2 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case31) "O")))
				(3 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case31) "E")))
				)
			)
			(2 (case x
				(1 (progn (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case32) "O")))
				(2 (progn (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case32) "O")))
				(3 (progn (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case32) "E")))
				)
			)
			(3 (case x
				(1 (progn (setf (slot-value *plateau* 'case13) "E") (setf (slot-value *plateau* 'case23) "O") (setf (slot-value *plateau* 'case33) "O")))
				(2 (progn (setf (slot-value *plateau* 'case13) "O") (setf (slot-value *plateau* 'case23) "E") (setf (slot-value *plateau* 'case33) "O")))
				(3 (progn (setf (slot-value *plateau* 'case13) "O") (setf (slot-value *plateau* 'case23) "O") (setf (slot-value *plateau* 'case33) "E")))
				)
			)
			)
		)
		(3 (progn
			(if (and (eql x 1) (eql y 1)) (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case33) "O")))
			(if (and (eql x 2) (eql y 2)) (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case33) "O")))
			(if (and (eql x 3) (eql y 3)) (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case33) "E")))
			(if (and (eql x 3) (eql y 1)) (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case13) "O")))
			(if (and (eql x 1) (eql y 3)) (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case13) "E")))
			)
		)
		(4 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case13) "O")))
	)
	
	(drawplateau)
)

(defun createWinBoard(x y)
	(if (oddp (+ x y))
		(setf ligne (+ 1 (act-r-random 2))) ;si le coup est sur une case sans diagonale
		(if (eql x 2)
			(setf ligne (+ 1 (act-r-random 4))) ;si le coup est au milieu
			(setf ligne (+ 1 (act-r-random 3))) ;sinon
		)
	)
	
	(initplateau)
	
	(case ligne
		(1 (case x
			(1 (case y
				(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case13) "X")))
				(2 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case13) "X")))
				(3 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case13) "E")))
				)
			)
			(2 (case y
				(1 (progn (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case23) "X")))
				(2 (progn (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case23) "X")))
				(3 (progn (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case23) "E")))
				)
			)
			(3 (case y
				(1 (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case32) "X") (setf (slot-value *plateau* 'case33) "X")))
				(2 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case32) "E") (setf (slot-value *plateau* 'case33) "X")))
				(3 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case32) "X") (setf (slot-value *plateau* 'case33) "E")))
				)
			)
			)
		)
		(2 (case y
			(1 (case x
				(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case31) "X")))
				(2 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case31) "X")))
				(3 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case31) "E")))
				)
			)
			(2 (case x
				(1 (progn (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case32) "X")))
				(2 (progn (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case32) "X")))
				(3 (progn (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case32) "E")))
				)
			)
			(3 (case x
				(1 (progn (setf (slot-value *plateau* 'case13) "E") (setf (slot-value *plateau* 'case23) "X") (setf (slot-value *plateau* 'case33) "X")))
				(2 (progn (setf (slot-value *plateau* 'case13) "X") (setf (slot-value *plateau* 'case23) "E") (setf (slot-value *plateau* 'case33) "X")))
				(3 (progn (setf (slot-value *plateau* 'case13) "X") (setf (slot-value *plateau* 'case23) "X") (setf (slot-value *plateau* 'case33) "E")))
				)
			)
			)
		)
		(3 (progn
			(if (and (eql x 1) (eql y 1)) (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case33) "X")))
			(if (and (eql x 2) (eql y 2)) (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case33) "X")))
			(if (and (eql x 3) (eql y 3)) (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case33) "E")))
			(if (and (eql x 3) (eql y 1)) (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case13) "X")))
			(if (and (eql x 1) (eql y 3)) (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case13) "E")))
			)
		)
		(4 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case13) "X")))
	)
	
	(drawplateau)
)

(defun drawplateau()
   (format t "~A" (slot-value *plateau* 'case11))
   (format t " ~A" (slot-value *plateau* 'case12))
   (format t " ~A~%" (slot-value *plateau* 'case13))
   (format t "~A" (slot-value *plateau* 'case21))
   (format t " ~A" (slot-value *plateau* 'case22))
   (format t " ~A~%" (slot-value *plateau* 'case23))
   (format t "~A" (slot-value *plateau* 'case31))
   (format t " ~A" (slot-value *plateau* 'case32))
   (format t " ~A~%" (slot-value *plateau* 'case33))
)

(defun setGoal()
	(goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
								 `((isa matrice ligne1, (car  (define-chunks-fct `(( isa triple pos, "1"  case1 , "E" case2 , "O" case3 , "X"))))
									state, nil
								  ))
						 )
				    )
	)
(run-full-time 10) 
)
 

(define-model tictactoe

(define-chunks 

    (test isa chunk) 
)

(chunk-type pattern id case1 case2 case3) 
(chunk-type goal ligne1 ligne2 ligne3 col1 col2 col3 diag1 diag2 state currentligne currentCol) 
(chunk-type learned-move ligne col diago1 diago2 x y) 
(declare-buffer-usage goal ligne :all)

(add-dm
	(EEE ISA pattern id 111 case1 "E" case2 "E" case3 "E") ;E = 1

	(XEE ISA pattern id 211 case1 "X" case2 "E" case3 "E") ; X = 2
	(EXE ISA pattern id 121 case1 "E" case2 "X" case3 "E")
	(EEX ISA pattern id 112 case1 "E" case2 "E" case3 "X")

	(XXE ISA pattern id 221 case1 "X" case2 "X" case3 "E")
	(XEX ISA pattern id 212 case1 "X" case2 "E" case3 "X")
	(EXX ISA pattern id 122 case1 "E" case2 "X" case3 "X")

	(OEE ISA pattern id 311 case1 "O" case2 "E" case3 "E") ; O = 3
	(EOE ISA pattern id 131 case1 "E" case2 "O" case3 "E")
	(EEO ISA pattern id 113 case1 "E" case2 "E" case3 "O")

	(OOE ISA pattern id 331 case1 "O" case2 "O" case3 "E")
	(OEO ISA pattern id 313 case1 "O" case2 "E" case3 "O")
	(EOO ISA pattern id 133 case1 "E" case2 "O" case3 "O")

	(XOE ISA pattern id 231 case1 "X" case2 "O" case3 "E")
	(OXE ISA pattern id 321 case1 "O" case2 "X" case3 "E")
	(XEO ISA pattern id 213 case1 "X" case2 "E" case3 "O")
	(OEX ISA pattern id 312 case1 "O" case2 "E" case3 "X")
	(EXO ISA pattern id 123 case1 "E" case2 "X" case3 "O")
	(EOX ISA pattern id 132 case1 "E" case2 "O" case3 "X")
)

)
