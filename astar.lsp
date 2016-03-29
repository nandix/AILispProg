(load 'generateSuccs)
(load 'goalState)
(load 'search)
(load 'printPuzzles)

; hard coded goal state, will replace with read in goal state
(defun goal-state ()
	(setf x '((1 2 3) (8 0 4) (7 6 5)))
	(return-from goal-state x)
)

; This function calculates the number of tiles in the correct position
(defun numCorrect (L)
	(let ((correct 0) (col 0) (row 0))

		; first loop, enters the state list
		(dolist (i  (list L) correct)
			
			; loops through each row
			(dolist (j i nil)
				; loops through each column
				(dolist (k j nil)
					; checks if the current element L[i][k] is equal to goal, goal[i][k]
					(if (not(= k (nth col (nth row (goal-state)))))
						(setf butts 0)
						(setf correct (+ 1 correct)) ; increments correct counter
					)
					(setf col (+ 1 col)) ; increments column index
				)
				; resets column index for the next loop, increments row index
				(setf col 0)
				(setf row (+ 1 row)) 
			)
			
		)
		(return-from numCorrect correct)
	)
)

; This function finds the minimum distance for each tile to be in the goal state and sums
; them up.  This sum is used as the heuristic value to be sorted.
(defun minDist (L)
	(let ((distSum 0) left  right temp)
		(dolist (i (list L) nil) ;enters state list
			(dolist (j i nil)
				(dolist (k j nil)
					(setf left (find-atom k L))
					(setf right (find-atom k (goal-state)))
				
					(setf temp (+ (abs(- (car right)(car left))) (abs(- (second right)(second left)))))
					(setf distSum (+ distSum temp))
				)
			)
		)
		(return-from minDist distSum)
	)
)

; This is the function that can be used in the built in sort function.
(defun numCorrectSort (L R)
	(let ()
		(cond
			;  If the left node is less correct tiles than right
			((< (numCorrect (node-state L)) (numCorrect (node-state R))) 
				nil
			)
			; If the left node has more correct tiles than right
			((> (numCorrect (node-state L)) (numCorrect (node-state R)))
				t
			)
		)
	)
)

; The minimum distance function that can be passed to the lisp sort function
(defun minDistSort (L R)
	(let ()
		(cond
			;  If the left node is less than right
			((< (minDist (node-state L)) (minDist (node-state R))) 
				t
			)
			; If the left node is more than right
			((> (minDist (node-state L)) (minDist (node-state R)))
				nil
			)
		)
	)
)




;------------------------------------------------------------------------------
; Function: 	astar
;
; Author:	John M. Weiss, Ph.D., Modified by Mack Smith
;
; Description:	This is a modification of the depth first search code provided
;		by Dr. Weiss. This function is basically breadth first search except that
;		it sorts the OPEN list based on the passed in heuristic.  The car of OPEN
;		should then be the best, which the function then uses as the next node to
;		expand.  
;
; Parameters:	start - start state representation
;		heuristic - the functional argument that represents the desired
;			   heuristic.  Default is number of correct tiles.
;
; Return:	solution path from start state to goal state
;------------------------------------------------------------------------------
(defun astar (start &optional (heuristic #'numCorrectSort))
	(do*                                                    ; note use of sequential DO*
		(                                                   ; initialize local loop vars
		    (curNode (make-node :state start :parent nil))  ; current node: (start nil)
		    (OPEN (list curNode))                           ; OPEN list:    ((start nil))
		    (CLOSED nil)                                    ; CLOSED list:  ( )
		)

		; termination condition - return solution path when goal is found
		((goal-state? (node-state curNode)) (build-solution curNode CLOSED))

		; loop body
		(when (null OPEN) (return nil))             ; no solution

		; sort OPEN list based on heuristic
		(setf OPEN (sort OPEN heuristic))
		;(print OPEN)
		
		; get current node from OPEN, update OPEN and CLOSED
		(setf curNode (car OPEN))
		(setf OPEN (cdr OPEN))
		(setf CLOSED (cons curNode CLOSED))

		; add successors of current node to OPEN
		(dolist (child (generate-successors (node-state curNode)))

		    ; for each child node
		    (setf child (make-node :state child :parent (node-state curNode)))

		    ; if the node is not on OPEN or CLOSED
		    (if (and (not (member child OPEN   :test #'equal-states))
		             (not (member child CLOSED :test #'equal-states)))

		        ; add it to the OPEN list
		        (setf OPEN (append OPEN (list child)))
		    )
		)
		
	)
)

