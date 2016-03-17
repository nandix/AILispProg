;Print board takes a list of 8, 15, or 24 puzzles and prints them out in
; an easy to read manner. It takes as parameters:
;   puzSize - the size puzzle on one dimension (3, 4, 5, etc)
;   puzPerLine - the number of puzzles to print in a single row
;   puzList - the list of puzzles ( ((p11 p12 ... p1n ) (p21 p22 ... p2n ) ... (pn1 pn2... pnn) ) ... (remaining puzzles) )
;                                   _________________________ one puzzle ________________________
(defun printPuzzles ( puzSize puzPerLine puzList ) 
    
    (setf puzList '( ((1 2 3) (4 5 6) (7 8 0)) ((2 3 4) (5 6 7) (8 0 1)) ((4 5 6) (7 8 0) (1 2 3)) ))

    ; Get 
    ( let  ( (numPuz (length puzList)) ; Get the number of puzzles
            )
        (format t "There are ~d puzzles" numPuz)

    )
)