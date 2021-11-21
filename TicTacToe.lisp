
(clear-all)

(define-model tictactoe

(chunk-type line pos case1 case2 case3)
(chunk-type column pos case1 case2 case3)
(chunk-type diagonal number case1 case2 case3)
(chunk-type case numberLine numberCol inDiag1 inDiag2 value)

(chunk-type searchEmpty lineNumber colNumber)
(chunk-type playMove bestLineNumber bestColNumber)

(P test-empty
   =goal>
     ISA         searchEmpty
	 lineNumber  =valueLine
	 colNumber   =valueCol
   =imaginal>
     ISA         case
     numberLine  =valueLine 
     numberCol   =valueCol
	 value 		 "E"
==>
   -goal>
   +goal>
	ISA		createMove
	lineNumber =valueLine
	colNumber =valueCol
)

(P try-recall-move
)

(P answer-good-move
) 

)