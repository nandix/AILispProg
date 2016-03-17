;Print board takes a list of 8, 15, or 24 puzzles and prints them out in
; an easy to read manner. It takes as parameters:
;   puzSize - the size puzzle on one dimension (3, 4, 5, etc)
;   puzPerLine - the number of puzzles to print in a single row
;   puzList - the list of puzzles ( ((p11 p12 ... p1n ) (p21 p22 ... p2n ) ... (pn1 pn2... pnn) ) ... (remaining puzzles) )
;                                   _________________________ one puzzle ________________________
(defun printPuzzles ( puzPerLine puzList ) 
    
    (setf puzList '( ((1 2 3) (4 5 6) (7 8 0)) ((2 3 4) (5 6 7) (8 0 1)) ((4 5 6) (7 8 0) (1 2 3)) ))

    ; Get 
    ( let  ( 
                (numPuz (length puzList)) ; Get the number of puzzles
                (puzSize (length (car puzList))) ; Get the dimension of a puzzle
            )
        (format t "There are ~d puzzles~%" numPuz)
        (format t "Puzzle Size: ~d~%" puzSize)

        (splitPuzzleList puzList 2)

    )
)

#|
    - Figure out the number of rows we need
    - Split the list of puzzles into sublists with the number of puzzles per row in each list
    - Call a function on each sublist to print a row of puzzles
        - for i = 0 to puzSize-1
            - print row i
|#
(defun splitPuzzleList (puzzleList puzPerLine)
    (setf nLines (ceiling (/ (length puzzleList) puzPerLine)))
    (setf splitList (make-list nLines))


    (let ((rowIndex -1))
        ; For i=0 to length-1
        (dotimes (i (length puzzleList))
            ; Increment the rowIndex
            (if (= '0 (mod i nLines)) (setf rowIndex (1+ rowIndex)) NIL)
            ;Append it to the appropriate index in the splitList
            (setf (nth rowIndex splitList) (append (nth rowIndex splitList) (copy-tree (nth i puzzleList))))
            
        )

        (return-from splitPuzzleList splitList)
    )
)

(defun printSolutionRow ( puzzleList rowIndex lastRow ) 
    (dolist (puzzle puzzleList)
        (printPuzRow (nth rowIndex puzzle))
        (format "    ")
    )
    (format "~%")
)

(defun printPuzRow (rowList)
    (dolist (rowElem rowList)
        (format "~2d  " rowElem)
    )
)