(defun goal-state? (testState)
	(if (null *goalState*)
		(setf *goalState* (read-state-file 'goal.puz))
	)

	(return-from goal-state? (equalp testState *goalState*))

	
)
