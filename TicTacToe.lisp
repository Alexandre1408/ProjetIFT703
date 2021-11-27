
(clear-all)

(defun createRandomNumbers()
	(setf coupBlockant (1+ (act-r-random 2))) ;1 si le coup doit être bloquant, 2 sinon
	(setf coupGagnantLigne (1+ (act-r-random 3))) ; genere la position du coup gagnant en ligne
	(setf coupGagnantCol (1+ (act-r-random 3))) ; genere la position du coup gagnant en colonne


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

(add-dm
(111 ISA pattern case1 "E" case2 "E" case3 "E")

(211 ISA pattern case1 "X" case2 "E" case3 "E")
(121 ISA pattern case1 "E" case2 "X" case3 "E")
(112 ISA pattern case1 "E" case2 "E" case3 "X")

(221 ISA pattern case1 "X" case2 "X" case3 "E")
(212 ISA pattern case1 "X" case2 "E" case3 "X")
(122 ISA pattern case1 "E" case2 "X" case3 "X")

(311 ISA pattern case1 "O" case2 "E" case3 "E")
(131 ISA pattern case1 "E" case2 "O" case3 "E")
(113 ISA pattern case1 "E" case2 "E" case3 "O")

(331 ISA pattern case1 "O" case2 "O" case3 "E")
(313 ISA pattern case1 "O" case2 "E" case3 "O")
(133 ISA pattern case1 "E" case2 "O" case3 "O")

(231 ISA pattern case1 "X" case2 "O" case3 "E")
(321 ISA pattern case1 "O" case2 "X" case3 "E")
(213 ISA pattern case1 "X" case2 "E" case3 "O")
(312 ISA pattern case1 "O" case2 "E" case3 "X")
(123 ISA pattern case1 "E" case2 "X" case3 "O")
(132 ISA pattern case1 "E" case2 "O" case3 "X")
)

(chunk-type pattern id case1 case2 case3) 
(chunk-type goal ligne1 ligne2 ligne3 col1 col2 col3 diag1 diag2 state currentLine currentCol) 
(chunk-type learned-move ligne col diago1 diago2 x y) 
(declare-buffer-usage goal line :all)



)
