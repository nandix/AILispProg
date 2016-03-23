(defun goal-state? (testState)
	
	#|
	(if (null *goalState*)
	
		(
			(setf *goalState* '())
			(let ( (fin (open 'goal.puz) (puzLine)) ) 
				; If the file opened...
				(when fin
					; Read each line of the file
					(loop for puzLine = (read-line fin nil)
						; Append to goal state
						(setf *goalState* (append *goalState* (list puzLine)))
					)
				)
				(close fin)

				(return-from goal-state *goalState*)
			)
		)

		(
			(return-from goal-state *goalState*)
		)
	)
	|#
	(setf goal? (equalp testState '((1 2 3) (8 0 4) (7 6 5)) ))
	(return-from goal-state? goal? )

	
)