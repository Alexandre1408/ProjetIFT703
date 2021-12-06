
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

(defvar *response* nil)
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

(defun tictactoe-once(loop)
	(let( 	(compteur 0)
			(window (open-exp-window "tic tac toe" :visible nil))
		)
		(install-device window)
		(dotimes (i loop)
			(setf compteur 0)
			(let (	(type-move (+ 1 (act-r-random 2)))
						(coupGagnantLigne (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en ligne
						(coupGagnantCol (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en colonne
					 )
					(case type-move
						(1 (createWinBoard coupGagnantLigne coupGagnantCol))
						(2 (createBlockBoard coupGagnantLigne coupGagnantCol))
					)
					
					(setf *response* nil)
					
					(add-act-r-command "response" 'respond-to-key-press)
					(monitor-act-r-command "output-key" "response")
					
					(set-goal *plateau* nil)
					
					(if (STRING-EQUAL (car *response*) "a")
						(progn
							(setf *response* nil)							
							(case type-move
								(1 (response-to-model coupGagnantLigne coupGagnantCol "gagnant" "finish"))
								(2 (response-to-model coupGagnantLigne coupGagnantCol "bloquant" "finish"))
							) 
						)
					)
					
					(if (STRING-EQUAL (car *response*) "w") 
						(setf compteur (+ compteur 1))
					)

					(remove-act-r-command-monitor "output-key" "response")
					(remove-act-r-command "response")
			)
		)
	compteur
	)
)

(defun tictactoe-win(loop)
	(let( (moyenne 0)
			(compteur 0)
			(not-win t)
			(window (open-exp-window "tic tac toe" :visible nil))
		)
		(install-device window)
		(dotimes (i loop)
			(setf compteur 0)
			(setf not-win t)
			(let (	(type-move (+ 1 (act-r-random 2)))
						(coupGagnantLigne (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en ligne
						(coupGagnantCol (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en colonne
					 )
					(case type-move
						(1 (createWinBoard coupGagnantLigne coupGagnantCol))
						(2 (createBlockBoard coupGagnantLigne coupGagnantCol))
					)
					
					(while not-win
						(setf compteur (+ compteur 1))
						(setf *response* nil)
						
						(add-act-r-command "response" 'respond-to-key-press)
						(monitor-act-r-command "output-key" "response")
						
						(set-goal *plateau* nil)
						
						(if (STRING-EQUAL (car *response*) "a")
							(progn
								(setf *response* nil)							
								(case type-move
									(1 (response-to-model coupGagnantLigne coupGagnantCol "gagnant" "finish"))
									(2 (response-to-model coupGagnantLigne coupGagnantCol "bloquant" "finish"))
								) 
							)
						)
					    
						(if (STRING-EQUAL (car *response*) "w") 
							(setf not-win nil)
						)

						(remove-act-r-command-monitor "output-key" "response")
						(remove-act-r-command "response")
					)
			)
			(setf moyenne (+ moyenne compteur))
		)
	(/ moyenne loop)
	)
)

(defun run-blocks (blocks block-size)
	(let ( (retour 0)
			(points))
		(dotimes (i blocks)
			(setf retour (tictactoe-win block-size))
			(push retour points)
			(format t "Anvancee : ~A%~%" (/ (* i 100) blocks))
		)
		(format t "Retour : ~A%~%" points)
		(draw-graph (rev points))
	)
)

(defun rev (l)
	(cond
		((null l) '())
		(T (append (rev (cdr l)) (list (car l))))
	)
) 

(defun draw-graph (points)
	(let ((w (open-exp-window "Data" :visible t :width 550 :height 460))
		(size (length points))
		)
		(add-line-to-exp-window w '(1 0) '(1 6) 'white)
		(dotimes (i 6)
			(add-text-to-exp-window w (format nil "~3,1f" (- 6 i)) 
								  :x 5 :y (+ 5 (* i 80)) :width 35)
			(add-line-to-exp-window w (list 45 (+ 10 (* i 80))) 
								  (list 550 (+ 10 (* i 80))) 'white)
		)
		
		(loop for i from 1 to size
			do (add-line-to-exp-window w (list (+ 45 (* (- i 1) (/ 500 size))) (- 490 (* 80 (nth (- i 1) points)))) (list (+ 45 (* i (/ 500 size))) (- 490 (* 80 (nth i points)))) 'blue)
		)
	)
)

(defun respond-to-key-press (model key)
	(declare (ignore model))
	(push key *response*)
)

(defun createBlockBoard(x y)
	(let ( ( ligne 0))
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
					(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case13) "O") (fillCroixLigne 2 3)))
					(2 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case13) "O") (fillCroixLigne 2 3)))
					(3 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case13) "E") (fillCroixLigne 2 3)))
					)
				)
				(2 (case y
					(1 (progn (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case23) "O") (fillCroixLigne 1 3)))
					(2 (progn (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case23) "O") (fillCroixLigne 1 3)))
					(3 (progn (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case23) "E") (fillCroixLigne 1 3)))
					)
				)
				(3 (case y
					(1 (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case32) "O") (setf (slot-value *plateau* 'case33) "O") (fillCroixLigne 1 2)))
					(2 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case32) "E") (setf (slot-value *plateau* 'case33) "O") (fillCroixLigne 1 2)))
					(3 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case32) "O") (setf (slot-value *plateau* 'case33) "E") (fillCroixLigne 1 2)))
					)
				)
				)
			)
			(2 (case y
				(1 (case x
					(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case31) "O") (fillCroixCol 2 3)))
					(2 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case31) "O") (fillCroixCol 2 3)))
					(3 (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case21) "O") (setf (slot-value *plateau* 'case31) "E") (fillCroixCol 2 3)))
					)
				)
				(2 (case x
					(1 (progn (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case32) "O") (fillCroixCol 1 3)))
					(2 (progn (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case32) "O") (fillCroixCol 1 3)))
					(3 (progn (setf (slot-value *plateau* 'case12) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case32) "E") (fillCroixCol 1 3)))
					)
				)
				(3 (case x
					(1 (progn (setf (slot-value *plateau* 'case13) "E") (setf (slot-value *plateau* 'case23) "O") (setf (slot-value *plateau* 'case33) "O") (fillCroixCol 1 2)))
					(2 (progn (setf (slot-value *plateau* 'case13) "O") (setf (slot-value *plateau* 'case23) "E") (setf (slot-value *plateau* 'case33) "O") (fillCroixCol 1 2)))
					(3 (progn (setf (slot-value *plateau* 'case13) "O") (setf (slot-value *plateau* 'case23) "O") (setf (slot-value *plateau* 'case33) "E") (fillCroixCol 1 2)))
					)
				)
				)
			)
			(3 (progn
				(if (and (eql x 1) (eql y 1)) (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case33) "O") (fillCroixDiag 1)))
				(if (and (eql x 2) (eql y 2)) (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case33) "O") (fillCroixDiag 1)))
				(if (and (eql x 3) (eql y 3)) (progn (setf (slot-value *plateau* 'case11) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case33) "E") (fillCroixDiag 1)))
				(if (and (eql x 3) (eql y 1)) (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case13) "O") (fillCroixDiag 2)))
				(if (and (eql x 1) (eql y 3)) (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case22) "O") (setf (slot-value *plateau* 'case13) "E") (fillCroixDiag 2)))
				)
			)
			(4 (progn (setf (slot-value *plateau* 'case31) "O") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case13) "O")  (fillCroixDiag 2)))
		)
	)
)

(defun createWinBoard(x y)
	(let ( ( ligne 0))
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
					(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case13) "X") (fillRondligne 2 3)))
					(2 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case13) "X") (fillRondligne 2 3)))
					(3 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case13) "E") (fillRondligne 2 3)))
					)
				)
				(2 (case y
					(1 (progn (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case23) "X") (fillRondligne 1 3)))
					(2 (progn (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case23) "X") (fillRondligne 1 3)))
					(3 (progn (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case23) "E") (fillRondligne 1 3)))
					)
				)
				(3 (case y
					(1 (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case32) "X") (setf (slot-value *plateau* 'case33) "X") (fillRondligne 1 2)))
					(2 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case32) "E") (setf (slot-value *plateau* 'case33) "X") (fillRondligne 1 2)))
					(3 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case32) "X") (setf (slot-value *plateau* 'case33) "E") (fillRondligne 1 2)))
					)
				)
				)
			)
			(2 (case y
				(1 (case x
					(1 (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case31) "X") (fillRondCol 2 3)))
					(2 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case21) "E") (setf (slot-value *plateau* 'case31) "X") (fillRondCol 2 3)))
					(3 (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case21) "X") (setf (slot-value *plateau* 'case31) "E") (fillRondCol 2 3)))
					)
				)
				(2 (case x
					(1 (progn (setf (slot-value *plateau* 'case12) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case32) "X") (fillRondCol 1 3)))
					(2 (progn (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case32) "X") (fillRondCol 1 3)))
					(3 (progn (setf (slot-value *plateau* 'case12) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case32) "E") (fillRondCol 1 3)))
					)
				)
				(3 (case x
					(1 (progn (setf (slot-value *plateau* 'case13) "E") (setf (slot-value *plateau* 'case23) "X") (setf (slot-value *plateau* 'case33) "X") (fillRondCol 1 2)))
					(2 (progn (setf (slot-value *plateau* 'case13) "X") (setf (slot-value *plateau* 'case23) "E") (setf (slot-value *plateau* 'case33) "X") (fillRondCol 1 2)))
					(3 (progn (setf (slot-value *plateau* 'case13) "X") (setf (slot-value *plateau* 'case23) "X") (setf (slot-value *plateau* 'case33) "E") (fillRondCol 1 2)))
					)
				)
				)
			)
			(3 (progn
				(if (and (eql x 1) (eql y 1)) (progn (setf (slot-value *plateau* 'case11) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case33) "X") (fillRondDiag 1)))
				(if (and (eql x 2) (eql y 2)) (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case33) "X") (fillRondDiag 1)))
				(if (and (eql x 3) (eql y 3)) (progn (setf (slot-value *plateau* 'case11) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case33) "E") (fillRondDiag 1)))
				(if (and (eql x 3) (eql y 1)) (progn (setf (slot-value *plateau* 'case31) "E") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case13) "X") (fillRondDiag 2)))
				(if (and (eql x 1) (eql y 3)) (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case22) "X") (setf (slot-value *plateau* 'case13) "E") (fillRondDiag 2)))
				)
			)
			(4 (progn (setf (slot-value *plateau* 'case31) "X") (setf (slot-value *plateau* 'case22) "E") (setf (slot-value *plateau* 'case13) "X") (fillRondDiag 2)))
		)
	)
)

;;; place un rond dans posCoupLig1 à la ligne ligneNb1 et posCoupLig2 à la ligne ligneNb2
(defun fillRondligne(ligneNb1 ligneNb2)
   (let ( (posCoupLig1 (1+ (act-r-random 3)))  
		  (posCoupLig2 (1+ (act-r-random 3))) 
		)
	   
	   (case ligneNb1
			(1 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case12) "O"))
				(3 (setf (slot-value *plateau* 'case13) "O"))
				)
			)
			(2 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case21) "O"))
				(2 (setf (slot-value *plateau* 'case22) "O"))
				(3 (setf (slot-value *plateau* 'case23) "O"))
				)
			)
			(3 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case31) "O"))
				(2 (setf (slot-value *plateau* 'case32) "O"))
				(3 (setf (slot-value *plateau* 'case33) "O"))
				)
			)
		)
		
	   (case ligneNb2
			(1 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case12) "O"))
				(3 (setf (slot-value *plateau* 'case13) "O"))
				)
			)
			(2 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case21) "O"))
				(2 (setf (slot-value *plateau* 'case22) "O"))
				(3 (setf (slot-value *plateau* 'case23) "O"))
				)
			)
			(3 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case31) "O"))
				(2 (setf (slot-value *plateau* 'case32) "O"))
				(3 (setf (slot-value *plateau* 'case33) "O"))
				)
			)
		)
	)
)

;;; place un rond dans posCoupLig1 à la COLONNE colNb1 et posCoupLig2 à la COLONNE colNb2
(defun fillRondCol(colNb1 colNb2)
   (let ( (posCoupCol1 (1+ (act-r-random 3)))  
		  (posCoupCol2 (1+ (act-r-random 3))) 
		)

	   
	   (case colNb1
			(1 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case21) "O"))
				(3 (setf (slot-value *plateau* 'case31) "O"))
				)
			)
			(2 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case12) "O"))
				(2 (setf (slot-value *plateau* 'case22) "O"))
				(3 (setf (slot-value *plateau* 'case32) "O"))
				)
			)
			(3 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case13) "O"))
				(2 (setf (slot-value *plateau* 'case23) "O"))
				(3 (setf (slot-value *plateau* 'case33) "O"))
				)
			)
		)
		
	   (case colNb2
			(1 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case21) "O"))
				(3 (setf (slot-value *plateau* 'case31) "O"))
				)
			)
			(2 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case12) "O"))
				(2 (setf (slot-value *plateau* 'case22) "O"))
				(3 (setf (slot-value *plateau* 'case32) "O"))
				)
			)
			(3 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case13) "O"))
				(2 (setf (slot-value *plateau* 'case23) "O"))
				(3 (setf (slot-value *plateau* 'case33) "O"))
				)
			)
		)
	)
)

;;; place 2 rond partout sauf dans la diagonale diagNb
(defun fillRondDiag(diagNb)

	(let ( (posCoupDiag1 (1+ (act-r-random 6)))  
		   (posCoupDiag2 (1+ (act-r-random 6)))
		 )
		 
		; Si les coups sont aux même endroits
		(while (eql posCoupDiag1 posCoupDiag2)
			(setf posCoupDiag1 (1+ (act-r-random 6))) 
		)
	   
		(case diagNb
			(1 (case posCoupDiag1
				(1 (setf (slot-value *plateau* 'case12) "O"))
				(2 (setf (slot-value *plateau* 'case13) "O"))
				(3 (setf (slot-value *plateau* 'case23) "O"))
				(4 (setf (slot-value *plateau* 'case32) "O"))
				(5 (setf (slot-value *plateau* 'case31) "O"))
				(6 (setf (slot-value *plateau* 'case21) "O"))
				)
				
			   (case posCoupDiag2
				(1 (setf (slot-value *plateau* 'case12) "O"))
				(2 (setf (slot-value *plateau* 'case13) "O"))
				(3 (setf (slot-value *plateau* 'case23) "O"))
				(4 (setf (slot-value *plateau* 'case32) "O"))
				(5 (setf (slot-value *plateau* 'case31) "O"))
				(6 (setf (slot-value *plateau* 'case21) "O"))
				)
			)
			(2 (case posCoupDiag1
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case12) "O"))
				(3 (setf (slot-value *plateau* 'case21) "O"))
				(4 (setf (slot-value *plateau* 'case23) "O"))
				(5 (setf (slot-value *plateau* 'case32) "O"))
				(6 (setf (slot-value *plateau* 'case33) "O"))
				)
				
			   (case posCoupDiag2
				(1 (setf (slot-value *plateau* 'case11) "O"))
				(2 (setf (slot-value *plateau* 'case12) "O"))
				(3 (setf (slot-value *plateau* 'case21) "O"))
				(4 (setf (slot-value *plateau* 'case23) "O"))
				(5 (setf (slot-value *plateau* 'case32) "O"))
				(6 (setf (slot-value *plateau* 'case33) "O"))
				)
			)
		)
	)
)

;;; Place deux croix, une dans ligneNb1 à la  position posCoupLig1 et l'autre dans ligneNb2 à la pos posCoupLig2
(defun fillCroixLigne(ligneNb1 ligneNb2)
   (let ( (posCoupLig1 (1+ (act-r-random 3)))  
		  (posCoupLig2 (1+ (act-r-random 3))) 
		)
	   
	   (case ligneNb1
			(1 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case12) "X"))
				(3 (setf (slot-value *plateau* 'case13) "X"))
				)
			)
			(2 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case21) "X"))
				(2 (setf (slot-value *plateau* 'case22) "X"))
				(3 (setf (slot-value *plateau* 'case23) "X"))
				)
			)
			(3 (case posCoupLig1
				(1 (setf (slot-value *plateau* 'case31) "X"))
				(2 (setf (slot-value *plateau* 'case32) "X"))
				(3 (setf (slot-value *plateau* 'case33) "X"))
				)
			)
		)
		
	   (case ligneNb2
			(1 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case12) "X"))
				(3 (setf (slot-value *plateau* 'case13) "X"))
				)
			)
			(2 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case21) "X"))
				(2 (setf (slot-value *plateau* 'case22) "X"))
				(3 (setf (slot-value *plateau* 'case23) "X"))
				)
			)
			(3 (case posCoupLig2
				(1 (setf (slot-value *plateau* 'case31) "X"))
				(2 (setf (slot-value *plateau* 'case32) "X"))
				(3 (setf (slot-value *plateau* 'case33) "X"))
				)
			)
		)
	)
)

;;; place une croix dans posCoupLig1 à la COLONNE colNb1 et posCoupLig2 à la COLONNE colNb2
(defun fillCroixCol(colNb1 colNb2)
   (let ( (posCoupCol1 (1+ (act-r-random 3)))  
		  (posCoupCol2 (1+ (act-r-random 3))) 
		)
	   
	   (case colNb1
			(1 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case21) "X"))
				(3 (setf (slot-value *plateau* 'case31) "X"))
				)
			)
			(2 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case12) "X"))
				(2 (setf (slot-value *plateau* 'case22) "X"))
				(3 (setf (slot-value *plateau* 'case32) "X"))
				)
			)
			(3 (case posCoupCol1
				(1 (setf (slot-value *plateau* 'case13) "X"))
				(2 (setf (slot-value *plateau* 'case23) "X"))
				(3 (setf (slot-value *plateau* 'case33) "X"))
				)
			)
		)
		
	   (case colNb2
			(1 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case21) "X"))
				(3 (setf (slot-value *plateau* 'case31) "X"))
				)
			)
			(2 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case12) "X"))
				(2 (setf (slot-value *plateau* 'case22) "X"))
				(3 (setf (slot-value *plateau* 'case32) "X"))
				)
			)
			(3 (case posCoupCol2
				(1 (setf (slot-value *plateau* 'case13) "X"))
				(2 (setf (slot-value *plateau* 'case23) "X"))
				(3 (setf (slot-value *plateau* 'case33) "X"))
				)
			)
		)
	)
)

;;; place 2 croix partout sauf dans la diagonale diagNb
(defun fillCroixDiag(diagNb)

	(let ( (posCoupDiag1 (1+ (act-r-random 6)))  
		   (posCoupDiag2 (1+ (act-r-random 6)))
		 )
		 
		; Si les coups sont aux même endroits
		(while (eql posCoupDiag1 posCoupDiag2)
			(setf posCoupDiag1 (1+ (act-r-random 6))) 
		)
	   
		(case diagNb
			(1 (case posCoupDiag1
				(1 (setf (slot-value *plateau* 'case12) "X"))
				(2 (setf (slot-value *plateau* 'case13) "X"))
				(3 (setf (slot-value *plateau* 'case23) "X"))
				(4 (setf (slot-value *plateau* 'case32) "X"))
				(5 (setf (slot-value *plateau* 'case31) "X"))
				(6 (setf (slot-value *plateau* 'case21) "X"))
				)
				
			   (case posCoupDiag2
				(1 (setf (slot-value *plateau* 'case12) "X"))
				(2 (setf (slot-value *plateau* 'case13) "X"))
				(3 (setf (slot-value *plateau* 'case23) "X"))
				(4 (setf (slot-value *plateau* 'case32) "X"))
				(5 (setf (slot-value *plateau* 'case31) "X"))
				(6 (setf (slot-value *plateau* 'case21) "X"))
				)
			)
			(2 (case posCoupDiag1
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case12) "X"))
				(3 (setf (slot-value *plateau* 'case21) "X"))
				(4 (setf (slot-value *plateau* 'case23) "X"))
				(5 (setf (slot-value *plateau* 'case32) "X"))
				(6 (setf (slot-value *plateau* 'case33) "X"))
				)
				
			   (case posCoupDiag2
				(1 (setf (slot-value *plateau* 'case11) "X"))
				(2 (setf (slot-value *plateau* 'case12) "X"))
				(3 (setf (slot-value *plateau* 'case21) "X"))
				(4 (setf (slot-value *plateau* 'case23) "X"))
				(5 (setf (slot-value *plateau* 'case32) "X"))
				(6 (setf (slot-value *plateau* 'case33) "X"))
				)
			)
		)
	)
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

   
(defun set-goal(plateau &optional state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
        (mod-focus-fct `(
                            case1_1 ,(slot-value plateau 'case11)  case1_2 ,(slot-value plateau 'case12) case1_3 ,(slot-value plateau 'case13)
                            case2_1 ,(slot-value plateau 'case21)  case2_2 ,(slot-value plateau 'case22) case2_3 ,(slot-value plateau 'case23)
                            case3_1 ,(slot-value plateau 'case31)  case3_2 ,(slot-value plateau 'case32) case3_3 ,(slot-value plateau 'case33)
                            nextLigne, 1
                            nextCol, 1
							firstEmptyLig, nil
							firstEmptyCol, nil
							currentLigne, nil
							currentCol, nil
							goodAnswerCol, nil
							goodAnswerLig, nil
							type-move, nil
                            state , state
                       )
        )
        (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                            `((isa board-state 
                                case1_1 ,(slot-value plateau 'case11)  case1_2 ,(slot-value plateau 'case12) case1_3 ,(slot-value plateau 'case13)
                                case2_1 ,(slot-value plateau 'case21)  case2_2 ,(slot-value plateau 'case22) case2_3 ,(slot-value plateau 'case23)
                                case3_1 ,(slot-value plateau 'case31)  case3_2 ,(slot-value plateau 'case32) case3_3 ,(slot-value plateau 'case33)
                                nextLigne, 1
                                nextCol, 1
								firstEmptyLig, nil
								firstEmptyCol, nil
								currentLigne, nil
								currentCol, nil
								goodAnswerCol, nil
								goodAnswerLig, nil
								type-move, nil
                                state , state
                            ))
                            )
                        )
        )
    ) 
   (run 100)
)

(defun response-to-model(x y type-move state)

	(if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
        (mod-focus-fct `(goodAnswerLig , x
			goodAnswerCol , y
			type-move , type-move
			state, state)
        )
        (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                            `((isa result goodAnswerLig , x
								goodAnswerCol , y
								state, state
								type-move , type-move)))
                        )
        )
    )
   (run 100)
)
 

(define-model tictactoe

(sgp :esc nil :ans 0.1 :bll 0.5 :rt -2)

(define-chunks 
    (search-empty isa chunk)
    (create-move isa chunk)
    (select-line isa chunk)
    (create-ligne isa chunk)
    (select-col isa chunk)
    (create-col isa chunk)
	(select-diag1 isa chunk)
	(create-diag1 isa chunk)
	(select-diag2 isa chunk)
	(create-diag2 isa chunk)
	(try-remember-move isa chunk)
	(remembering isa chunk)
	(finish isa chunk)
	(playing isa chunk)
)

(chunk-type pattern id case1 case2 case3) 
(chunk-type board-state case1_1 case1_2 case1_3 case2_1 case2_2 case2_3 case3_1 case3_2 case3_3 nextLigne nextCol currentLigne currentCol firstEmptyLig firstEmptyCol goodAnswerLig goodAnswerCol type-move state) 
(chunk-type learned-move ligne col diag1 diag2 type) 
(declare-buffer-usage goal board-state :all)

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

(p start
   =goal>
        isa board-state
        state nil
   ==>
   =goal>
        state search-empty
)

(p test-non-empty1_1
    =goal>
        state search-empty
        nextLigne 1
        nextCol 1
        - case1_1 "E"
==>
    =goal>
        currentLigne 1
        currentCol 1
        nextLigne 1
        nextCol 2
)

(p test-empty1_1
    =goal>
        state search-empty
        nextLigne 1
        nextCol 1
        case1_1 "E"
==>
    =goal>
        state create-move
        currentLigne 1
        currentCol 1
        nextLigne 1
        nextCol 2
)

(p test-empty1_2
    =goal>
        state search-empty
        nextLigne 1
        nextCol 2
        case1_2 "E"
==>
    =goal>
        state create-move
        currentLigne 1
        currentCol 2
        nextLigne 1
        nextCol 3
)

(p test-non-empty1_2
    =goal>
        state search-empty
        nextLigne 1
        nextCol 2
        - case1_2 "E"
==>
    =goal>
        currentLigne 1
        currentCol 2
        nextLigne 1
        nextCol 3
)

(p test-empty1_3
    =goal>
        state search-empty
        nextLigne 1
        nextCol 3
        case1_3 "E"
==>
    =goal>
        state create-move
        currentLigne 1
        currentCol 3
        nextLigne 2
        nextCol 1
)

(p test-non-empty1_3
    =goal>
        state search-empty
        nextLigne 1
        nextCol 3
        - case1_3 "E"
==>
    =goal>
        currentLigne 1
        currentCol 3
        nextLigne 2
        nextCol 1
)

(p test-empty2_1
    =goal>
        state search-empty
        nextLigne 2
        nextCol 1
        case2_1 "E"
==>
    =goal>
        state create-move
        currentLigne 2
        currentCol 1
        nextLigne 2
        nextCol 2
)

(p test-non-empty2_1
    =goal>
        state search-empty
        nextLigne 2
        nextCol 1
        - case2_1 "E"
==>
    =goal>
        currentLigne 2
        currentCol 1
        nextLigne 2
        nextCol 2
)

(p test-empty2_2
    =goal>
        state search-empty
        nextLigne 2
        nextCol 2
        case2_2 "E"
==>
    =goal>
        state create-move
        currentLigne 2
        currentCol 2
        nextLigne 2
        nextCol 3
)

(p test-non-empty2_2
    =goal>
        state search-empty
        nextLigne 2
        nextCol 2
        - case2_2 "E"
==>
    =goal>
        currentLigne 2
        currentCol 2
        nextLigne 2
        nextCol 3
)

(p test-empty2_3
    =goal>
        state search-empty
        nextLigne 2
        nextCol 3
        case2_3 "E"
==>
    =goal>
        state create-move
        currentLigne 2
        currentCol 3
        nextLigne 3
        nextCol 1
)

(p test-non-empty2_3
    =goal>
        state search-empty
        nextLigne 2
        nextCol 3
        - case2_3 "E"
==>
    =goal>
        currentLigne 2
        currentCol 3
        nextLigne 3
        nextCol 1
)

(p test-empty3_1
    =goal>
        state search-empty
        nextLigne 3
        nextCol 1
        case3_1 "E"
==>
    =goal>
        state create-move
        currentLigne 3
        currentCol 1
        nextLigne 3
        nextCol 2
)


(p test-non-empty3_1
    =goal>
        state search-empty
        nextLigne 3
        nextCol 1
        - case3_1 "E"
==>
    =goal>
        currentLigne 3
        currentCol 1
        nextLigne 3
        nextCol 2
)

(p test-empty3_2
    =goal>
        state search-empty
        nextLigne 3
        nextCol 2
        case3_2 "E"
==>
    =goal>
        state create-move
        currentLigne 3
        currentCol 2
        nextLigne 3
        nextCol 3
)


(p test-non-empty3_2
    =goal>
        state search-empty
        nextLigne 3
        nextCol 2
        - case3_2 "E"
==>
    =goal>
        currentLigne 3
        currentCol 2
        nextLigne 3
        nextCol 3
)

(p test-empty3_3
    =goal>
        state search-empty
        nextLigne 3
        nextCol 3
        case3_3 "E"
==>
    =goal>
        state create-move
        currentLigne 3
        currentCol 3
)


(p test-non-empty3_3
    =goal>
        state search-empty
        nextLigne 3
        nextCol 3
        - case3_3 "E"
==>
    =goal>
        currentLigne 3
        currentCol 3
)

(p create-move
   =goal>
		state create-move
==>
   =goal>
		state select-line
)

(p select-line1
   =goal>
		state select-line
		currentLigne 1
		case1_1 =c1
		case1_2 =c2
		case1_3 =c3
==>
   +retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-ligne
)

(p select-line2
   =goal>
		state select-line
		currentLigne 2
		case2_1 =c1
		case2_2 =c2
		case2_3 =c3
==>
   +retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-ligne
)

(p select-line3
   =goal>
		state select-line
		currentLigne 3
		case3_1 =c1
		case3_2 =c2
		case3_3 =c3
==>
   +retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-ligne
)

(p create-line
	=goal>
		state create-ligne
	=retrieval> 
		isa pattern
		id =idpattern
	?imaginal>
		state free
==>
    +imaginal>
		isa learned-move
		ligne =idpattern
	=goal>
		state select-col
)

(p select-col1
	=goal>
		state select-col
		currentCol 1
		case1_1 =c1
		case2_1 =c2
		case3_1 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-col
)

(p select-col2
	=goal>
		state select-col
		currentCol 2
		case1_2 =c1
		case2_2 =c2
		case3_2 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-col
)

(p select-col3
	=goal>
		state select-col
		currentCol 3
		case1_3 =c1
		case2_3 =c2
		case3_3 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-col
)

(p create-col
	=goal>
		state create-col
	=retrieval> 
		isa pattern
		id =idpattern
	=imaginal>
		isa learned-move
		col nil
==>
    =imaginal>
		col =idpattern
	=goal>
		state select-diag1
)

(p select-diag1
	=goal>
		state select-diag1
		currentLigne =nb
		currentCol =nb
		case1_1 =c1
		case2_2 =c2
		case3_3 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-diag1
)

;la case actuelle n'a pas de diagonal 1
(p no-diag1
    =goal>
        state select-diag1
        currentLigne =nb
        - currentCol =nb
        case1_1 =c1
        case2_2 =c2
        case3_3 =c3
==>
    =goal>
        state select-diag2
)

(p create-diag1
	=goal>
		state create-diag1
	=retrieval> 
		isa pattern
		id =idpattern
	=imaginal>
		isa learned-move
		diag1 nil
==>
    =imaginal>
		diag1 =idpattern
	=goal>
		state select-diag2
)

;selectionne la diagonal numéro 2 de la case ligne 2 col 1
(p select-diag2_1
	=goal>
		state select-diag2
		currentLigne 1
		currentCol 3
		case1_3 =c1
		case2_2 =c2
		case3_1 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-diag2
)

;selectionne la diagonal numéro 2 de la case ligne 2 col 2
(p select-diag2_2
	=goal>
		state select-diag2
		currentLigne 2
		currentCol 2
		case1_3 =c1
		case2_2 =c2
		case3_1 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-diag2
)

;selectionne la diagonal numéro 2 de la case ligne 3 col 1
(p select-diag2_3
	=goal>
		state select-diag2
		currentLigne 3
		currentCol 1
		case1_3 =c1
		case2_2 =c2
		case3_1 =c3
==>
	+retrieval> 
		isa pattern
		case1 =c1
		case2 =c2
		case3 =c3
	=goal>
		state create-diag2
)

; la case n'a pas de diagonal2 si elle n'est pas à la ligne 1 colonne 3 
(p no-diag2_1_3
	=goal>
		state select-diag2
		currentLigne		1
		- currentCol		3
		
==>
	=goal>
		state try-remember-move
)

; la case n'a pas de diagonal2 si elle n'est pas à la ligne 2 colonne 2
(p no-diag2_2_3
	=goal>
		state select-diag2
		currentLigne		2
		- currentCol		2
		
==>
	=goal>
		state try-remember-move
)

; la case n'a pas de diagonal2 si elle n'est pas à la ligne 3 colonne 1
(p no-diag2_3_3
	=goal>
		state select-diag2
		currentLigne		3
		- currentCol		1
		
==>
	=goal>
		state try-remember-move
)

;ajoute le pattern correspondant à la diagonal numéro 2 dans le buffer imaginal
(p create-diag2
	=goal>
		state create-diag2
	=retrieval> 
		isa pattern
		id =idpattern
	=imaginal>
		isa learned-move
		diag2 nil
==>
    =imaginal>
		diag2 =idpattern
	=goal>
		state try-remember-move
)

;essaye de se rappeler d'un move avec 4 pattern
(p try-remember-move-both-diag
	=goal>
		state try-remember-move
		- nextLigne nil
		- nextCol nil
	=imaginal>
		isa learned-move
		ligne 	=arg1
		col 	=arg2
		diag1	=arg3
		diag2	=arg4
==>
	=imaginal>
	+retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   =arg4
		- type nil
	=goal>
		state remembering
)

;essaye de se rappeler d'un move avec 3 pattern
(p try-remember-move-only-diag1
	=goal>
		state try-remember-move
		- nextLigne nil
		- nextCol nil
	=imaginal>
		isa learned-move
		ligne 	=arg1
		col 	=arg2
		diag1	=arg3
		diag2	nil
==>
	=imaginal>
	+retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2		nil
		- type nil
	=goal>
		state remembering
)

;essaye de se rappeler d'un move avec 3 pattern
(p try-remember-move-only-diag2
	=goal>
		state try-remember-move
		- nextLigne nil
		- nextCol nil
	=imaginal>
		isa learned-move
		ligne 	=arg1
		col 	=arg2
		diag1	nil
		diag2	=arg3
==>
	=imaginal>
	+retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2		=arg3
		- type nil
	=goal>
		state remembering
)

;essaye de se rappeler d'un move avec 2 pattern
(p try-remember-move-no-diag
	=goal>
		state try-remember-move
		- nextLigne nil
		- nextCol nil
	=imaginal>
		isa learned-move
		ligne 	=arg1
		col 	=arg2
		diag1	nil
		diag2	nil
==>
	=imaginal>
	+retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2	   nil
		- type nil
	=goal>
		state remembering
)

;si on se rappelle pas d'un move on continue à chercher
(p cannot-remember-move
	=goal>
		state remembering
	?retrieval>
		buffer failure
==>
	=goal>
		state search-empty
	-imaginal>
)

;si on ne se rappelle pas d'un move, on enregistre la position de la premiere case vide
; et on continue à chercher
(p cannot-remember-move-first-empty
   =goal>
   		state remembering
		firstEmptyLig nil
		firstEmptyCol nil
		currentLigne =lig
		currentCol =col
	?retrieval>
		buffer failure
==>
   =goal>
		state search-empty
		firstEmptyLig =lig
		firstEmptyCol =col
	-imaginal>
)

;si on se rappelle d'un mauvais move, on ne fait rien on continue à chercher
(p remember-bad-move
	=goal>
		state remembering
	=retrieval>
		ISA		learned-move
		type    "lose"
==>
	=goal>
		state search-empty
)

; joue le move si on s'en rappelle
;on demande une réponse au model grace au buffer manual
(p remember-move-both-diag
	=goal>
		state remembering
	=retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   =arg4
		type 	   =arg5
		- type "lose"
	=imaginal>
	?manual>
		state free
==>
	=imaginal>
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   =arg4
		type	   =arg5
	+manual>
		cmd press-key
		key "a"
)

;joue le move si on s'en rappelle (uniquement la diagonal 1 dans le move)
; on demande une réponse au model grace au buffer manual
(p remember-move-only-diag1
	=goal>
		state remembering
	=retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2 	   =arg4
		type 	   =arg5
		- type "lose"
	?manual>
		state free
	=imaginal>
==>
	=imaginal>
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2 	   =arg4
		type	   =arg5
	+manual>
		cmd press-key
		key "a"
)


;joue le move si on s'en rappelle (uniquement la diagonal 2 dans le move)
;on demande une réponse au model grace au buffer manual
(p remember-move-only-diag2
	=goal>
		state remembering
	=retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   nil
		type 	   =arg5
		- type "lose"
	?manual>
		state free
	=imaginal>
==>
	=imaginal>
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   nil
		type	   =arg5
	+manual>
		cmd press-key
		key "a"
)

;joue le move si on s'en rappelle (aucunes diagonal dans le move)
; on demande une réponse au model grace au buffer manual
(p remember-move-no-diag
	=goal>
		state remembering
	=retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2 	   nil
		type 	   =arg5
		- type 	"lose"
	?manual>
		state free
	=imaginal>
==>
	=imaginal>
		ligne      =arg1
		col        =arg2
		diag1 	   nil
		diag2 	   nil
		type	   =arg5
	+manual>
		cmd press-key
		key "a"
)

;si on a trouvé aucun coup : on créé le move associé à la premiere case vide du plateau
(p create-default-move
	=goal>
		state search-empty
		currentLigne 3
		currentCol 3
	    nextCol  3
		nextLigne  3
		firstEmptyLig =val1
		firstEmptyCol =val2
	==>
	=goal>
		state create-move
		currentLigne =val1
		currentCol =val2
		nextCol nil
		nextLigne nil
)

; joue le move par défaut
(p play-default-move
	=goal>
		state try-remember-move
		nextCol nil
		nextLigne nil
		firstEmptyLig 	=val1 ;remplacer par first empty lig et col ? 
		firstEmptyCol   =val2
	?manual>
		state free
==>
	=goal>
		state playing
	+manual>
		cmd press-key
		key "a"
)

;s'active si le modele gagne en jouant dans le move par défaut
(p memorize-win-default
	=goal>
		state "finish"
		type-move =val
		goodAnswerLig	=a
		goodAnswerCol 	=b
		firstEmptyLig   =a
		firstEmptyCol	=b
		nextCol nil
		nextLigne nil
	=imaginal>
		type nil
   ?manual>
      state free
==>
	=imaginal>
		type =val
	-imaginal>
	=goal>
		state finish
	+manual>
		cmd press-key
		key "w"
)

;s'active si le modele se souvient d'un move de type bloquant 
(p memorize-win-remember1
	=goal>
		state "finish"
	=imaginal>
		type "bloquant"
   ?manual>
      state free
==>
	-imaginal>
	=goal>
		state finish
	+manual>
		cmd press-key
		key "w"
)

;s'active si le modele se souvient d'un move de type gagnant
(p memorize-win-remember2
	=goal>
		state "finish"
	=imaginal>
		type "gagnant"
   ?manual>
      state free
==>
	-imaginal>
	=goal>
		state finish
	+manual>
		cmd press-key
		key "w"
)


(p memorize-lose-ligne
	=goal>
		state "finish"
		type-move =val
		goodAnswerLig	=a
	-	firstEmptyLig	=a
	=imaginal>
		type nil
==>
	=imaginal>
		type "lose"
	-imaginal>
	=goal>
		state finish
)

(p memorize-lose-col
	=goal>
		state "finish"
		type-move =val
		goodAnswerCol	=arg1
	-	firstEmptyCol	=arg1
	=imaginal>
		type nil
==>
	=imaginal>
		type "lose"
	-imaginal>
	=goal>
		state finish
)

)