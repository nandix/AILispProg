;------------------------------------------------------------------------------
; File: 8puzzle.lsp
;
; Author:		Dylan Geyer
;
; Description:	This file is the glue that holds this program together. It loads in
;				all of the required helper functions to solve the 8-tile problem.
;				It contains the function 8puzzle which is what the user will actually
;				call in order to solve an 8-tile problem.
;
; Usage:		Once this file has been loaded an 8puzzle can be solved by running...
;				(8puzzle)	- Allows you to enter puzzle at cmdline
;				(8puzzle 'input.puz)	- Allows reading puzzle from file
;				$> clisp 8puzzle.lsp input.puz 	-Allows usage outside REPL
;
; 
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;		GLOBAL VARIABLE DECLARATIONS
;------------------------------------------------------------------------------
(defvar *nodeCount* 0)		;Keeps track of number of states generated as successors
(defvar *uniqueCount* 0)	;Keeps track of unique nodes generated as successors
(defvar *expandedCount* 0)	;Keeps track of nodes exlored in the search
(defvar *goalState* nil)	;Final state we are searching for

;------------------------------------------------------------------------------
;		FILE INCLUDES
;------------------------------------------------------------------------------
(load "generateSuccs.lsp")
(load "goalState.lsp")
(load "search.lsp")
(load "astar.lsp")
(load "printPuzzles.lsp")

;------------------------------------------------------------------------------
; Function: 	8puzzle
;
; Author:		Dylan Geyer
;
; Description:	This function handles getting the start state from a file or from
;				cmdline. If a user passes in a 0 do nothing otherwise handle some
;				form of input to get the start_state.
;
; Parameters:	start_state - optional starting state, can contain filename or list
;
; Return:		none
;------------------------------------------------------------------------------
(defun 8puzzle (&optional (start_state nil))
	(cond
		((equal start_state 0)	;0 passed in means skip everything
			(return-from 8puzzle)
		)		
		((null start_state)	;Nothing passed in, get it from user at cmdline
			(setf start_state (read-state))
		)
		((atom start_state)	;Filename passed in
			(setf start_state (read-state-file start_state))
		)
		(t					;State list passed in
			(setf start_state (convert-state start_state))
		)
	)
	(solvePuzzles start_state)
	(values)	;Suppress output of nil
)

;------------------------------------------------------------------------------
; Function: 	read-state
;
; Author:	Dylan Geyer
;
; Description:	This function asks the user what size of puzzle they would like
;		to enter (3,4,5...) and then reads in that many rows and columns
;		of numbers to fill out the n x n matrix.
;
; Parameters:	none
;
; Return:	n x n matrix containing the state read from cmdline
;------------------------------------------------------------------------------
(defun read-state ()
	(let ((n 0) (data nil))
		;Get rows/cols of the square matrix that makes up node-state
		(format t "Enter n x n:")
		(setf n (read))
		
		;Get n * n numbers from user to fill out the node-state
		(format t "Enter tile positions:")
		(loop for i from 0 to (1- n) do
			(loop for j from 0 to (1- n) do
				(setf data (append data (list (read))))	;Append number to data
			)
		)
		(convert-state data)	;Return our data in our usable square matrix format
	)
)

;------------------------------------------------------------------------------
; Function: 	read-state-file
;
; Author:	Dylan Geyer
;
; Description:	This function opens the filename passed in and reads the state
;		contained in the file. The file is read into one long list and
;		this list is then converted to an n x n matrix using the
;		convert-state function. 
;
; Parameters:	filename - name of the file containing the state
;
; Return:	n x n matrix containing the state read from file
;------------------------------------------------------------------------------
(defun read-state-file (filename)
	(let (	(data nil)
			(fin (open filename))
	     )
		(loop for expr = (read fin nil)	;Return nil on EOF
		while expr			;While line read is non-nil (not EOF)
		do
			(setf data (append data (list expr)))	;Append number to data
		)
		(convert-state data)	;Return state in our square matrix format
	)
)

;------------------------------------------------------------------------------
; Function: 	convert-state
;
; Author:	Dylan Geyer
;
; Description:	This function converts a node-state which comes in as a list of
;		9/16/25 numbers in a single list to a list of rows which splits
;		the list in to an n x n matrix of numbers. A 9 element list becomes
;		a 3x3 matrix and so on.
;
; Parameters:	state_list - long list of 9/16/25 numbers to be converted
;
; Return:	state matrix containing rows
;------------------------------------------------------------------------------
(defun convert-state (state_list)
	(let (  (rows (sqrt (length state_list)))	;rows = cols for n x n matrix
			(end_list nil)			;final list being built
			(cur_row nil)			;current row being built
		 )

		(loop for i from 0 to (1- rows) do
			(setf cur_row nil)
			;Build cur_row by appending (rows) elements from state_list
			(loop for j from 0 to (1- rows) do
				(setf cur_row (append cur_row (list (nth (+ j (* rows i)) state_list))))
			)
			;Build end_list by appending cur_row 
			(setf end_list (append end_list (list cur_row)))
		)
		end_list	;Return end_list
	)
)

;If the user supplied a cmdline argument, call 8puzzle with that argument
(cond
	( (> (length *args*) 0)
		(8puzzle (car *args*))
	)
)
