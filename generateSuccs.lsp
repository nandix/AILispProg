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

;;;Let statement provides a static scope for the function
(let ((loc nil))
	(defun findzero (lst start &optional (row 0) (col 0))
	
		(if (not (null start)) (setf loc NIL) NIL)

		(cond
			((not (null loc)))	; 0 has already been found, stop looking and return

			( (equal (car lst) 0)	; 0 has just been found! set the static variable
				(setf loc (list row col))	;Return the list (row col)
			)
			( (and (atom (car lst)) (not (null (car lst))) ) 	;we are inside a row, so iterate over columns
				(findzero (cdr lst ) NIL row (1+ col))
			)
			( (and (listp (car lst)) (not (null lst)) )	;lst is a list of rows so find 0 in the current row (car)
														;and find 0 in the rest of the rows (cdr)
				(findzero (car lst) NIL row col)
				(findzero (cdr lst) NIL (1+ row) col)
			)
		)
		(return-from findzero loc)
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

		(setf pos (findzero lst t))

		(setf row (car pos))
		(setf col (car (cdr pos)))
		(setf len (length lst))


		(cond
			( (>= row 1)	;If row index is >= 1 then we can move our zero UP
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth col (nth (1- row) puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
			)
		)
		(cond
			( (< row (1- len))	;If row index is < len then we can move our zero DOWN
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth col (nth (1+ row) puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
			)
		)
		(cond
			( (>= col 1)	;If col index is >= 1 then we can move our zero LEFT
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth (1- col) (nth row puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
			)
		)
		(cond
			( ( < col (1- len))	;If col index is < len then we can move our zero RIGHT
				(setf puzCopy (copy-tree lst))
				(rotatef (nth col (nth row puzCopy)) (nth (1+ col) (nth row puzCopy)))
				(setf children (append children (list (copy-tree puzCopy))))
			)
		)
		(return-from generate-successors children)
	)
)


;(setf puz '((1 2 3 4)(5 6 0 8)(9 10 11 12)(13 14 15 7)))
