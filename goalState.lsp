;------------------------------------------------------------------------------
; Function: 	goal-state?
;
; Author:		Dylan Geyer
;
; Description:	This function returns t/nil if the state passed in is equal to
;				the goal-state. The goal state exists inside of goal.puz. We 
;				want to make sure this file is only read when needed so if the
;				global variable *goalState* is not yet set do the file read of
;				goal.puz and set the global. Once we are sure there is something
;				in *goalState* we do a compare on the state that was passed in.
;
; Parameters:	testState - state to be compared to *goalState*
;
; Return:		t - if *goalState* is the same as testState
;				nil - if *goalState* is not the same as testState
;------------------------------------------------------------------------------
(defun goal-state? (testState)
	(if (null *goalState*)
		(setf *goalState* (read-state-file 'goal.puz))
	)

	(return-from goal-state? (equalp testState *goalState*))

	
)
