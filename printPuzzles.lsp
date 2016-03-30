;Print board takes a list of 8, 15, or 24 puzzles and prints them out in
; an easy to read manner. It takes as parameters:
;   puzSize - the size puzzle on one dimension (3, 4, 5, etc)
;   puzPerLine - the number of puzzles to print in a single row
;   puzList - the list of puzzles ( ((p11 p12 ... p1n ) (p21 p22 ... p2n ) ... (pn1 pn2... pnn) ) ... (remaining puzzles) )
;                                   |________________________ one puzzle _______________________|

;(setf *solutionPath* '())
;(setf *closedList* '())
;(setf *nodeCount* '8)
;(setf *uniqueCount* '6)



(defun solvePuzzles (start_state)

    (let ((numPerRow 4) (solutionPath '()))

        ; Run breadth first search
        (setf *nodeCount* '0)
        (setf solutionPath (search_bfs_dfs start_state 'bfs))

        ; Print BFS Statistics
        (printStats solutionPath "BFS graph search")
        (printPuzzles numPerRow solutionPath)

        ; Run DFID
        (setf *nodeCount* '0)
	(setf solutionPath (dfs_id start_state))

        ; Print DFID Statistics
        (printStats solutionPath "DFID graph search")
        (printPuzzles numPerRow solutionPath)

        ; Run A*
        (setf *nodeCount* '0)
	(setf solutionPath (astar start_state #'numWrong))

        ; Print A* Statistics
        (printStats solutionPath "A* graph search (heuristic: Number in wrong position)")
        (printPuzzles numPerRow solutionPath)

        ; Run A*
        (setf *nodeCount* '0)
	(setf solutionPath (astar start_state #'minDist))

        ; Print A* Statistics
        (printStats solutionPath "A* graph search (heuristic: Sum of Minimum distances)")
        (printPuzzles numPerRow solutionPath)
		
        ; Run A*
        (setf *nodeCount* '0)
        (setf solutionPath (astar start_state #'inadmissible))

        ; Print A* Statistics
        (printStats solutionPath "A* graph search(heuristic: Sum of Minimum distances + 3*Number in wrong position)")
        (printPuzzles numPerRow solutionPath)

    )
)

(defun printStats (puzList titleString)

    (format t titleString)
    (format t "~%")

    ( dotimes (i (length titleString))
        (format t "-")
    )
    (format t "~%")

    (format t "Solution found in ~d moves~%" (1- (length puzList)))
    (format t "~d nodes generated " *nodeCount* )
    (format t "(~d distinct nodes), " *uniqueCount* )
    (format t "~d nodes expanded~%~%" *expandedCount* )
)


#|
    - Figure out the number of rows we need
    - Split the list of puzzles into sublists with the number of puzzles per row in each list
    - Call a function on each sublist to print a row of puzzles
        - for i = 0 to puzSize-1
            - print row i
|#
(defun splitPuzzleList (puzzleList puzPerLine)
    ; Compute the number of lines based on the puzzles per line
    (setf nLines (ceiling (/ (length puzzleList) puzPerLine)))
    ; Construct a list of NILs of length of number of lines
    (setf splitList (make-list nLines))


    (let ((rowIndex 0))
        ; For i=0 to length-1
        (dotimes (i (length puzzleList))
            ; Increment the rowIndex
            (cond 
                (
                    (and (> i '0) (= '0 (mod i puzPerLine))) 
                    (setf rowIndex (1+ rowIndex)) 
                )
                
                (t NIL)
            )
            ;Append it to the appropriate index in the splitList
            ;(setf (nth rowIndex splitList) (append (nth rowIndex splitList) (copy-tree (nth i puzzleList))))
            (setf (nth rowIndex splitList) 
                (append (nth rowIndex splitList) (list  (nth i puzzleList)) )
            )
            
        )
        (setf (cdr (last (car(last splitList)))) (cons 'NIL 'NIL) )

        (return-from splitPuzzleList splitList)
    )
)


(defun printPuzzles ( puzPerLine puzList ) 
    
    ;(setf puzList '( ((1 2 3) (4 5 6) (7 8 0)) ((2 3 4) (5 6 7) (8 0 1)) ((4 5 6) (7 8 0) (1 2 3)) ))

    ; Get 
    ( let  ( 
                (numPuz (length puzList)) ; Get the number of puzzles
                (puzSize (length (car puzList))) ; Get the dimension of a puzzle
                (lastRow)
            )

        (setf splitPuzzles (splitPuzzleList puzList puzPerLine))

        ; For each row of puzzles to be printed
        (dolist (solutionRow splitPuzzles)
            ; For each row in the puzzles
            (setf isLastRow (car (last solutionRow)))

            (dotimes (i puzSize)
                ( if (= i (floor (/ puzSize 2)))
                    (printSolutionRow solutionRow i isLastRow t)
                    (printSolutionRow solutionRow i isLastRow 'NIL)
                )
            )
            (format t "~%")
        )

    )
)

; Print one line of a solution all the way across the terminal (one
;   row from each puzzle in that line)
(defun printSolutionRow ( puzzleList rowIndex lastRow middleRow ) 
    
    ;(dolist (puzzle puzzleList)
    ;(print puzzleList)
    (let (puzzle)
    (dotimes (i (length puzzleList))
        (setf puzzle (nth i puzzleList))
        ;(format t "Cadr is ~s~%" (cadr puzzle))



        (cond
            

            ; If it's the middle row and there's more puzzles to print,
            ;   print an arrow
            (
                (and (not (null middleRow)) (not (equal puzzle 'NIL)))
                
                (printPuzRow (nth rowIndex puzzle))

                (if 
                    (and (= i (- (length puzzleList) 2)) (equal lastRow 'NIL) ) 
                    (format t "")
                    (format t "   ->   ")
                )
                
                
            )
            ; Otherwise, print filler spaces
            (  (not (equal puzzle 'NIL))
                (printPuzRow (nth rowIndex puzzle))
                (if 
                    (and (= i (- (length puzzleList) 2)) (equal lastRow 'NIL) ) 
                    (format t "")
                    (format t "        ") 
                )
            )
            (t (format t ""))
        )

    )
    )
    

    (format t "~%")
)

(defun printPuzRow (rowList)
    (dolist (rowElem rowList)
        (if (/= '0 rowElem)
            (format t "~2d " rowElem)
            (format t "   ")
        )
    )
)
