;------------------------------------------------------------------------------
; Function:     solvePuzzles
;
; Author:       Daniel Nix
;
; Description:  This function accepts a start state for an n puzzle problem and
;               solves it using breadth first search, depth first iterated 
;               deepening, and A*. A* uses two admissible heuristics (the 
;               hamming and manhattan distances) and one inadmissible heuristic
;               (two times the manhattan distance). This function resets the
;               global *nodeCount* variable and prints the puzzle solutions 
;               and corresponding statistics 
;
; Parameters:   start_state - the start state for the n-puzzle problem
;
; Return:       none
;------------------------------------------------------------------------------
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
        (printStats solutionPath "A* graph search (heuristic: Number in correct position)")
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
		(printStats solutionPath "A* graph search(heuristic: 2 * Sum of Minimum distances)")
		(printPuzzles numPerRow solutionPath)

    )
)

;------------------------------------------------------------------------------
; Function:     printStats
;
; Author:       Daniel Nix
;
; Description:  printStats prints the foratted statistics for a puzzle solution
;               It reports the number of nodes, number of unique nodes, and
;               number of nodes expanded to solve the puzzle
;
; Parameters:   puzList - the list of puzzles which form the solution
;               titleString - the header for the formatted output
;
; Return:       none
;------------------------------------------------------------------------------
(defun printStats (puzList titleString)

    ; Print the title with a newline
    (format t titleString)
    (format t "~%")

    ; Print the underline under the title string
    ( dotimes (i (length titleString))
        (format t "-")
    )
    (format t "~%")

    ; Print the number of moves to complete the puzzle
    (format t "Solution found in ~d moves~%" (1- (length puzList)))
    ; Print the total number of nodes required
    (format t "~d nodes generated " *nodeCount* )
    ; Print the number of distinct nodes
    (format t "(~d distinct nodes), " *uniqueCount* )
    ; Print the number of nodes expanded
    (format t "~d nodes expanded~%~%" *expandedCount* )
)


;------------------------------------------------------------------------------
; Function:     splitPuzzles
;
; Author:       Daniel Nix
;
; Description:  splitPuzzleList accepts a solution path (in the form of a 
;               list of puzzles) and splits it into sublists of puzPerLine
;               puzzles each. This is done so no indexing, modulus, or other
;               calculations are required when printing the puzzles.
;               It appends NIL to the end of the last puzzle list so the 
;               print functions know not to print the last arrow.
;
; Parameters:   puzzleList - the list of puzzles which form the solution
;               puzPerLine - number of puzzles which should be printed
;                               on a single row
;
; Return:       splitList - the puzzle list with its entries split into
;                           puzPerLine puzzles in each sublist
;------------------------------------------------------------------------------
(defun splitPuzzleList (puzzleList puzPerLine)
    ; Compute the number of lines based on the puzzles per line
    (setf nLines (ceiling (/ (length puzzleList) puzPerLine)))
    ; Construct a list of NILs of length of number of lines
    (setf splitList (make-list nLines))


    ; For each row of n puzzles to print out
    (let ((rowIndex 0))
        ; For each puzzle to be printed...
        (dotimes (i (length puzzleList))
            ; Increment the rowIndex if the current line already 
            ;   had puzPerLine puzzles
            (cond 
                (
                    (and (> i '0) (= '0 (mod i puzPerLine))) 
                    (setf rowIndex (1+ rowIndex)) 
                )
                
                (t NIL)
            )
            ;Append the current puzzle to the appropriate index in the splitList
            (setf (nth rowIndex splitList) 
                (append (nth rowIndex splitList) (list  (nth i puzzleList)) )
            )
            
        )
        ; Append a nil to the end of the last list
        (setf (cdr (last (car(last splitList)))) (cons 'NIL 'NIL) )

        ; Return the list of puzzles split into puz per line
        (return-from splitPuzzleList splitList)
    )
)

;------------------------------------------------------------------------------
; Function:     printPuzzles
;
; Author:       Daniel Nix
;
; Description:  printPuzzles accepts a list of n-puzzles and prints them
;               to the terminal across the screen
;
; Parameters:   puzList - the list of puzzles which form the solution
;               puzPerLine - number of puzzles to be printed in each line
;
; Return:       none
;------------------------------------------------------------------------------
(defun printPuzzles ( puzPerLine puzList ) 
    
    ; Local variables are declared in the let
    ( let  ( 
                (numPuz (length puzList)) ; Get the number of puzzles
                (puzSize (length (car puzList))) ; Get the dimension of a puzzle
                (lastRow)
            )
        ; Split the puzzle list into the correct number of puzzles per line
        (setf splitPuzzles (splitPuzzleList puzList puzPerLine))

        ; For each row of puzzles to be printed
        (dolist (solutionRow splitPuzzles)
            ; Figure out if this is the last row of puzzles to print so we
            ;   don't print an arrow after the last puzzle
            (setf isLastRow (car (last solutionRow)))

            ; For each line in a puzzle
            (dotimes (i puzSize)
                ; Print the row with or without arrows, depending if it is
                ;   the middle row
                ( if (= i (floor (/ puzSize 2)))
                    (printSolutionRow solutionRow i isLastRow t)
                    (printSolutionRow solutionRow i isLastRow 'NIL)
                )
            )
            (format t "~%")
        )

    )
)

;------------------------------------------------------------------------------
; Function:     printSoutionRow
;
; Author:       Daniel Nix
;
; Description:  printSolutionRow prints one row from a puzzle list across the
;               terminal. For example, if 3 puzzles are provided it will print
;               the 3 n puzzles across the terminal horizontally
;
; Parameters:   puzzleList - the list of puzzles to be printed in line
;               rowIndex - Which row of the puzzle to print
;               lastRow - is this the last row of the puzzle? t/nil
;               middleRow - is this the middle row of the puzzle? t/nil
;
; Return:       none
;------------------------------------------------------------------------------
(defun printSolutionRow ( puzzleList rowIndex lastRow middleRow ) 
    
    ;(dolist (puzzle puzzleList)
    ;(print puzzleList)
    (let (puzzle)
        ; For each puzzle in the solution row
        (dotimes (i (length puzzleList))
            ; Set the current puzzle to the next in the list
            (setf puzzle (nth i puzzleList))
            
            (cond
                ; If it's the middle row and there's more puzzles to print,
                ;   print the row and an arrow between all puzzles
                (
                    (and (not (null middleRow)) (not (equal puzzle 'NIL)))
                    
                    (printPuzRow (nth rowIndex puzzle))

                    (if 
                        ; If this is the last row of puzzles to print print nothing
                        ;   after the last puzzle. Otherwise, print an arrow
                        (and (= i (- (length puzzleList) 2)) (equal lastRow 'NIL) ) 
                        (format t "")
                        (format t "   ->   ")
                    )
                    
                    
                )
                ; Otherwise, print filler spaces
                (  (not (equal puzzle 'NIL))
                    (printPuzRow (nth rowIndex puzzle))
                    (if 
                        ; If this is the last row of puzzles to print print nothing
                        ;   after the last puzzle. Otherwise, print spaces
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

;------------------------------------------------------------------------------
; Function:     printPuzRow
;
; Author:       Daniel Nix
;
; Description:  prints one formatted row of one puzzle to the terminal. Zeroes
;               are replaced with blanks
;
; Parameters:   rowList - the list of numbers to print
;
; Return:       none
;------------------------------------------------------------------------------
(defun printPuzRow (rowList)
    ; For each element in a list of numbers
    (dolist (rowElem rowList)
        ; If it's nonzero, print the number. Otherwise, print spaces.
        (if (/= '0 rowElem)
            (format t "~2d " rowElem)
            (format t "   ")
        )
    )
)
