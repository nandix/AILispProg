#|
Generate successors in a general manner. The state representation is ((123)(456)(780))
This allows us to move the 0 left 1 index, right 1 index, up one row, down one row.

Need a function to get the (row,col) position of the 0 in a state.

we can then swap (row,col) with (row +/- 1, col +/- 1)

copying puzzle (setf puzCopy (copy-tree puzzle))

moving the zero 
(rotatef (nth col (nth row puzCopy)) (nth col (nth (1- row) puzCopy)))	// up
(rotatef (nth col (nth row puzCopy)) (nth col (nth (1+ row) puzCopy))) 	// down
(rotatef (nth col (nth row puzCopy)) (nth (1- col) (nth row puzCopy)))	// left
(rotatef (nth col (nth row puzCopy)) (nth (1+ col) (nth row puzCopy)))	// right

Check for stepping off bounds of list where 1- and 1+ are going < 0 or > col/row
|#

(defun find-atom (&optional (atom 0) (lst '()))
	(let ((r 0) (c 0))
		(loop for row in lst do
			(setf c (position atom row :test #'equal))
			(cond
				( (null c)
					(setf r (1+ r))
				)
				(t
					(return-from find-atom (list r c))
				)
			)
		)
	)
)


(defun generate-successors (lst)
	(let 
		(
			(pos nil) 
			(row nil) 
			(col nil) 
			(puzCopy nil) 
			(len nil)
			(children '()) 
		)

		(setf pos (find-atom 0 lst))

		(setf row (car pos))
		(setf col (car (cdr pos)))
		(setf len (length lst))


		(cond
			( (>= row 1)	;If row index is >= 1 then we can move our zero UP
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth col (nth (1- row) puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
				(incf *nodeCount*)
			)
		)
		(cond
			( (< row (1- len))	;If row index is < len then we can move our zero DOWN
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth col (nth (1+ row) puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
				(incf *nodeCount*)
			)
		)
		(cond
			( (>= col 1)	;If col index is >= 1 then we can move our zero LEFT
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth (1- col) (nth row puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
				(incf *nodeCount*)
			)
		)
		(cond
			( ( < col (1- len))	;If col index is < len then we can move our zero RIGHT
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth (1+ col) (nth row puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
				(incf *nodeCount*)
			)
		)
		(return-from generate-successors children)
	)
)


;(setf puz '((1 2 3 4)(5 6 0 8)(9 10 11 12)(13 14 15 7)))
