(defvar *nodeCount* 0)
(defvar *uniqueCount* 0)
(defvar *expandedCount* 0)
(defvar *goalState* nil)

(load "generateSuccs.lsp")
(load "goalState.lsp")
(load "search.lsp")
(load "astar.lsp")
(load "printPuzzles.lsp")

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
)

(defun read-state ()
	(let ((n 0) (data nil))
		(format t "Enter n x n:")
		(setf n (read))
		(format t "Enter tile positions:")
		(loop for i from 0 to (1- n) do
			(loop for j from 0 to (1- n) do
				(setf data (append data (list (read))))
			)
		)
		(convert-state data)
	)
)

(defun read-state-file (filename)
	(let (	(data nil)
			(fin (open filename))
		 )
		(loop for expr = (read fin nil)
		while expr
		do
			(setf data (append data (list expr)))
		)
		(convert-state data)
	)
)

(defun convert-state (state_list)
	(let (  (rows (sqrt (length state_list)))
			(end_list nil)
			(cur_row nil)
		 )

		(loop for i from 0 to (1- rows) do
			(setf cur_row nil)
			(loop for j from 0 to (1- rows) do
				(setf cur_row (append cur_row (list (nth (+ j (* rows i)) state_list))))
			)
			(setf end_list (append end_list (list cur_row)))
		)
		end_list
	)
)

(8puzzle 0)
