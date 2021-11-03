(clear-all)

(define-model TicTacToe

(chunk-type cases line col val)

(chunk-type line line_nb case1 case2 case3) // les cases sont des pointeurs vers les chunk type cases_x_y)
(chunk-type column col_nb case1 case2 case3)
(chunk-type diagonal case1 case2 case3)
(chunk-type play state)

(chunk-type play_one matrice state) // state : { playToWin, playToNotLose } 
// matrice decrit l'état du plateau

)