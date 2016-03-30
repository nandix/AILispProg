
; -------------------------------------------------------------------------------------------------------
; Function: numWrong 
;
; Author:	Mack Smith, Dylan Geyer
;
; Description:  This function finds the number of tiles in the wrong postition.
;
; Parameters:  L - the state to be processed
;
; Returns: wrong - the number of tiles in the wrong position
; -------------------------------------------------------------------------------------------------------
(defun numWrong (L)
	(let ((wrong 0) (col 0) (row 0))
		; first loop, enters the state list
		(dolist (i  (list L) wrong)
			; loops through each row
			(dolist (j i nil)
				; loops through each column
				(dolist (k j nil)
					; checks if the current element L[i][k] is equal to goal, goal[i][k]
					(if (not(= k (nth col (nth row *goalState*))))
                        (setf wrong (+ 1 wrong)) ; increments wrong counter
						(values)
					)
					(setf col (+ 1 col)) ; increments column index
				)
				; resets column index for the next loop, increments row index
				(setf col 0)
				(setf row (+ 1 row)) 
			)
			
		)
		(return-from numWrong wrong)
	)
)
; -------------------------------------------------------------------------------------------------------
; Function: minDist 
;
; Author:	Mack Smith
;
; Description:  This function finds the minimum distance for each tile to be in the goal state and sums
; 				them up.  This sum is used as the heuristic value to be sorted.
;
; Parameters:  L - the state to be processed
;
; Returns: distSum - the sum of the distances each tile has to the goal state.
; -------------------------------------------------------------------------------------------------------
(defun minDist (L)
	(let ((distSum 0) left  right temp)
		(dolist (i (list L) nil) ;enters state list
			(dolist (j i nil)
				(dolist (k j nil)
					(setf left (find-atom k L))
					(setf right (find-atom k *goalState*))
				
					(setf temp (+ (abs(- (car right)(car left))) (abs(- (second right)(second left)))))
					(setf distSum (+ distSum temp))
				)
			)
		)
		(return-from minDist distSum)
	)
)

; -------------------------------------------------------------------------------------------------------
; Function: inadmissible 
;
; Author:	Mack Smith
;
; Description:  This is our inadmissible heuristic function.  It calculates the minimum distance each
;				tile is from the goal state by calling the minDist function.  Then it looks at the number
;				of tiles out of place and multiplies it by 3. These numbers are added together to provide
;				a cost to goal.
;
; Parameters:  L - the state to be processed
;
; Returns: heurVal - the heuristic value.
; -------------------------------------------------------------------------------------------------------
(defun inadmissible (L)
	(let (heurVal)
		(setf heurVal (+ (* 3 (numWrong L)) (minDist L)))
		(return-from inadmissible heurVal)
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
;				heuristic - the functional argument that represents the desired
;			   	heuristic.  Default is number of wrong tiles.
;
; Return:	solution path from start state to goal state
;------------------------------------------------------------------------------
(defun astar (start &optional (heuristic #'numWrong))
	(do*                                                    ; note use of sequential DO*
		(                                                   ; initialize local loop vars
		    (curNode (make-node :state start :parent nil :depth 0 :score (funcall heuristic start)))  ; current node: (start nil)
		    (OPEN (list curNode))                           ; OPEN list:    ((start nil))
		    (CLOSED nil)                                    ; CLOSED list:  ( )
		)

		; termination condition - return solution path when goal is found
		((goal-state? (node-state curNode)) (build-solution curNode CLOSED))

		; loop body
		(when (null OPEN) (return nil))             ; no solution

		; sort OPEN list based on node score
		(setf OPEN (sort OPEN #'< :key #'node-score))
		;(print OPEN)
		
		; get current node from OPEN, update OPEN and CLOSED
		(setf curNode (car OPEN))
		(setf OPEN (cdr OPEN))
		(setf CLOSED (cons curNode CLOSED))

		; add successors of current node to OPEN
		(dolist (child (generate-successors (node-state curNode)))

		    ; for each child node
		    (setf child (make-node :state child :parent (node-state curNode) :depth (1+ (node-depth curNode)) :score (+ (funcall heuristic child) (1+ (node-depth curNode)))))

		    ; if the node is not on OPEN or CLOSED
		    (if (and (not (member child OPEN   :test #'equal-states))
		             (not (member child CLOSED :test #'equal-states)))

		        ; add it to the OPEN list
		        (setf OPEN (append OPEN (list child)))
		    )
		)
		(setf *expandedCount* (length CLOSED))
		(setf *uniqueCount* (+ (length OPEN) (length CLOSED)))
	)
	
)

