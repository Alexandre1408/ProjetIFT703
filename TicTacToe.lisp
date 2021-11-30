
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

(defun main()
	(let (	(typeCoup (+ 1 (act-r-random 2)))
			(coupGagnantLigne (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en ligne
			(coupGagnantCol (+ 1 (act-r-random 3)))  ; genere la position du coup gagnant en colonne
		 )
		(case typeCoup
			(1 (createWinBoard coupGagnantLigne coupGagnantCol))
			(2 (createBlockBoard coupGagnantLigne coupGagnantCol))
		)
		
		(drawplateau)
		
		(setf *coords-response* nil)
		
		(add-act-r-command "response" 'respond-to-key-press)
		(monitor-act-r-command "output-key" "response")
		
		(let((modelKey (set-goal *plateau* nil)))
		
		(remove-act-r-command-monitor "output-key" "response")
		(remove-act-r-command "response")
		
		(if (eql (nth 1 *coords-response*) coupGagnantLigne)
			(if (eql (nth 2 *coords-response*) coupGagnantCol)
				(case typeCoup
					(1 (response-to-model("win")))
					(2 (response-to-model("block")))
				)
				(response-to-model("loose"))
			)
			(response-to-model("loose"))
		)
	)
)

(defun respond-to-key-press (model key)
	(declare (ignore model))
	(push key *coords-responses*))
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
		  (randomNb (1+ (act-r-random 2))) 
		)
	   ;(format t "RANDOM ~d ~%" randomNb)

		; random si les 2 O sont sur la même ligne
	   (if (eql randomNb 1)
			(setf ligneNb1 ligneNb2)
	   )
	   
	   ; Si les coups sont sur la même ligne et au même endroit, refaire un random
	   (while (and (eql posCoupLig1 posCoupLig2) (eql ligneNb1 ligneNb2))
		  (setf posCoupLig1 (1+ (act-r-random 3))) 
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
		  (randomNbCol (1+ (act-r-random 2)))
		)

		; random si les 2 O sont sur la même 	colonne
	   (if (eql randomNbCol 1)
			(setf colNb1 colNb2)
	   )
	   
	   ; Si les coups sont sur la même ligne et au même endroit, refaire un random
	   (while (and (eql posCoupCol1 posCoupCol2) (eql colNb1 colNb2))	
		  (setf posCoupCol1 (1+ (act-r-random 3))) 
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
                            currentLigne, 1
                            currentCol, 1
							bestMoveLig, nil
							bestMoveCol, nil
							prevLigne, nil
							prevCol, nil
                            state , state
                       )
        )
        (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                            `((isa board-state 
                                case1_1 ,(slot-value plateau 'case11)  case1_2 ,(slot-value plateau 'case12) case1_3 ,(slot-value plateau 'case13)
                                case2_1 ,(slot-value plateau 'case21)  case2_2 ,(slot-value plateau 'case22) case2_3 ,(slot-value plateau 'case23)
                                case3_1 ,(slot-value plateau 'case31)  case3_2 ,(slot-value plateau 'case32) case3_3 ,(slot-value plateau 'case33)
                                currentLigne, 1
                                currentCol, 1
								bestMoveLig, nil
								bestMoveCol, nil
								prevLigne, nil
								prevCol, nil
                                state , state
                            ))
                            )
                        )
        )
    ) 
   (run-full-time 10)
   *model-action*
)

(defun response-to-model(state)
	(if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
        (mod-focus-fct `(state , state)
        )
        (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                            `((isa board-state state , state))
                            )
                        )
        )
    ) 
   (run-full-time 10)
)
 

(define-model tictactoe

(define-chunks 
    (search-empty isa chunk)
    (create-move isa chunk)
    (select-line isa chunk)
    (select-col isa chunk)
    (select-diag isa chunk)
    (create-col isa chunk)
    (create-ligne isa chunk)
	(select-diag1 isa chunk)
	(create-diag1 isa chunk)
	(select-diag2 isa chunk)
	(create-diag2 isa chunk)
	(try-remember-move isa chunk)
	(answer isa chunk)
	(replace-empty isa chunk)
)

(chunk-type pattern id case1 case2 case3) 
(chunk-type board-state case1_1 case1_2 case1_3 case2_1 case2_2 case2_3 case3_1 case3_2 case3_3  currentLigne currentCol prevLigne prevCol bestMoveLig bestMoveCol state) 
(chunk-type learned-move ligne col diag1 diag2 x y) 
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
        currentLigne 1
        currentCol 1
        - case1_1 "E"
==>
    =goal>
        prevLigne 1
        prevCol 1
        currentLigne 1
        currentCol 2
)

(p test-empty1_1
    =goal>
        state search-empty
        currentLigne 1
        currentCol 1
        case1_1 "E"
==>
    =goal>
        state create-move
        prevLigne 1
        prevCol 1
        currentLigne 1
        currentCol 2
)

(p test-empty1_2
    =goal>
        state search-empty
        currentLigne 1
        currentCol 2
        case1_2 "E"
==>
    =goal>
        state create-move
        prevLigne 1
        prevCol 2
        currentLigne 1
        currentCol 3
)

(p test-non-empty1_2
    =goal>
        state search-empty
        currentLigne 1
        currentCol 2
        - case1_2 "E"
==>
    =goal>
        prevLigne 1
        prevCol 2
        currentLigne 1
        currentCol 3
)

(p test-empty1_3
    =goal>
        state search-empty
        currentLigne 1
        currentCol 3
        case1_3 "E"
==>
    =goal>
        state create-move
        prevLigne 1
        prevCol 3
        currentLigne 2
        currentCol 1
)

(p test-non-empty1_3
    =goal>
        state search-empty
        currentLigne 1
        currentCol 3
        - case1_3 "E"
==>
    =goal>
        prevLigne 1
        prevCol 3
        currentLigne 2
        currentCol 1
)

(p test-empty2_1
    =goal>
        state search-empty
        currentLigne 2
        currentCol 1
        case2_1 "E"
==>
    =goal>
        state create-move
        prevLigne 2
        prevCol 1
        currentLigne 2
        currentCol 2
)

(p test-non-empty2_1
    =goal>
        state search-empty
        currentLigne 2
        currentCol 1
        - case2_1 "E"
==>
    =goal>
        prevLigne 2
        prevCol 1
        currentLigne 2
        currentCol 2
)

(p test-empty2_2
    =goal>
        state search-empty
        currentLigne 2
        currentCol 2
        case2_2 "E"
==>
    =goal>
        state create-move
        prevLigne 2
        prevCol 2
        currentLigne 2
        currentCol 3
)

(p test-non-empty2_2
    =goal>
        state search-empty
        currentLigne 2
        currentCol 2
        - case2_2 "E"
==>
    =goal>
        prevLigne 2
        prevCol 2
        currentLigne 2
        currentCol 3
)

(p test-empty2_3
    =goal>
        state search-empty
        currentLigne 2
        currentCol 3
        case2_3 "E"
==>
    =goal>
        state create-move
        prevLigne 2
        prevCol 3
        currentLigne 3
        currentCol 1
)

(p test-non-empty2_3
    =goal>
        state search-empty
        currentLigne 2
        currentCol 3
        - case2_3 "E"
==>
    =goal>
        prevLigne 2
        prevCol 3
        currentLigne 3
        currentCol 1
)

(p test-empty3_1
    =goal>
        state search-empty
        currentLigne 3
        currentCol 1
        case3_1 "E"
==>
    =goal>
        state create-move
        prevLigne 3
        prevCol 1
        currentLigne 3
        currentCol 2
)


(p test-non-empty3_1
    =goal>
        state search-empty
        currentLigne 3
        currentCol 1
        - case3_1 "E"
==>
    =goal>
        prevLigne 3
        prevCol 1
        currentLigne 3
        currentCol 2
)

(p test-empty3_2
    =goal>
        state search-empty
        currentLigne 3
        currentCol 2
        case3_2 "E"
==>
    =goal>
        state create-move
        prevLigne 3
        prevCol 2
        currentLigne 3
        currentCol 3
)


(p test-non-empty3_2
    =goal>
        state search-empty
        currentLigne 3
        currentCol 2
        - case3_2 "E"
==>
    =goal>
        prevLigne 3
        prevCol 2
        currentLigne 3
        currentCol 3
)

(p test-empty3_3
    =goal>
        state search-empty
        currentLigne 3
        currentCol 3
        case3_3 "E"
==>
    =goal>
        state create-move
        prevLigne 3
        prevCol 3
        ;currentLigne 3
        ;currentCol 3
)


(p test-non-empty3_3
    =goal>
        state search-empty
        currentLigne 3
        currentCol 3
        - case3_3 "E"
==>
    =goal>
        prevLigne 3
        prevCol 3
        ;currentLigne 3
        ;currentCol 3
)

(p first-empty
   =goal>
   		state create-move
		bestMoveLig nil
		bestMoveCol nil
		prevLigne =lig
		prevCol =col
==>
   =goal>
		bestMoveLig =lig
		bestMoveCol =col
)

(p create-move
   =goal>
		state create-move
		- bestMoveLig nil
		- bestMoveCol nil
==>
   =goal>
		state select-line
)

(p select-line1
   =goal>
		state select-line
		prevLigne 1
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
		prevLigne 2
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
		prevLigne 3
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
		prevCol 1
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
		prevCol 2
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
		prevCol 3
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
		prevLigne =nb
		prevCol =nb
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

(p no-diag1
    =goal>
        state select-diag1
        prevLigne =nb
        - prevCol =nb
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

(p select-diag21
	=goal>
		state select-diag2
		prevLigne 1
		prevCol 3
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

(p select-diag22
	=goal>
		state select-diag2
		prevLigne 2
		prevCol 2
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

(p select-diag23
	=goal>
		state select-diag2
		prevLigne 3
		prevCol 1
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

(p no-diag2
	=goal>
		state select-diag2
		;rajouter condition pr pas prendre cette regle si on a uen diag
	?retrieval>
		buffer failure
==>
	=goal>
		state try-remember-move
)

(p no-diag2-2
	=goal>
		state select-diag2
==>
	=goal>
		state try-remember-move
)

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

(p try-remember-move-both-diag
	=goal>
		state try-remember-move
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
)

(p try-remember-move-only-diag1
	=goal>
		state try-remember-move
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
)

(p try-remember-move-only-diag2
	=goal>
		state try-remember-move
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
)

(p try-remember-move-no-diag
	=goal>
	;changer l'état en sortie ?
		state try-remember-move
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
)

(p cannot-remember-move
	=goal>
		state try-remember-move
	?retrieval>
		buffer failure
	
==>
	=goal>
		state search-empty
	-imaginal>
)


(p remember-move
	=goal>
		state try-remember-move
	=retrieval>
		ISA		learned-move
		ligne      =arg1
		col        =arg2
		diag1 	   =arg3
		diag2 	   =arg4
		x 		   =arg5
		y 		   =arg6
==>
	=goal>
		state answer
)

(p not-continue-search-empty 
	=goal>
		state search-empty
		- case1_1 "E"
		- case1_2 "E"
		- case1_3 "E"
		- case2_1 "E"
		- case2_2 "E"
		- case2_3 "E"
		- case3_1 "E"
		- case3_2 "E"
		- case3_3 "E"
	==>
	=goal>
		state answer
)

)