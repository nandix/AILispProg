; Node structure: stores state and parent.
(defstruct node state parent)


(load 'generateSuccs)
(load 'goalState)
(load 'search)


(defun goal-state ()
	(setf x '((1 2 3) (8 0 4) (7 6 5)))
	(return-from goal-state x)
)



(defun test ()
	(setf testvar '((7 8 4)(1 0 3)(2 5 6)))
	(print testvar)

	(setf temp (nth 0 (nth 0 testvar)))
	(print temp)
)

(defun numCorrect (L)
	(let ((correct 0) (col 0) (row 0))

		;(print L)
		;(print (goal-state))
		(dolist (i  (list L) correct)

			(dolist (j i nil)
				(dolist (k j nil)
					;(print k)
					(if (not(= k (nth col (nth row (goal-state)))))
						(setf butts 0)
						(setf correct (+ 1 correct))
					)
					(setf col (+ 1 col))
				)
				(setf col 0)
				(setf row (+ 1 row))
			)
			
		)
		(return-from numCorrect correct)
	)
)

(defun numCorrectSort (L R)
	(let ()
		(cond
			((< (numCorrect (node-state L)) (numCorrect (node-state R))) 
				nil
			)
			((> (numCorrect (node-state L)) (numCorrect (node-state R)))
				t
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
;		by Dr. Weiss. This function takes a depth bound as an optional
;		argument and without it it will run as 'unbounded' with an
;		essentially infinite depth bound. The modifications that make
;		this function depth bounded is the encapsulation of the generate
;		succesors code in an if statement that checks the current depth
;		agains the depth bound. If we have reach the depth bound then
;		no more successors are generated.
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

