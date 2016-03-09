; moves element from upper left corner to the right one space
(defun move_uleft_right (parent)

	(let ((child (copy-list parent)))
		(let (temp)

			(setf temp (second (first  child)))
			(setf (second (first child)) 0)
			(setf (first (first child)) temp)

		)
		(return-from move_uleft_right child)
	)
)
; Moves element from upper left corner down one space
(defun move_uleft_down (parent)

	(let ((child (copy-list parent)))	
		(let (temp)

			(setf temp (first (second child)))
			(setf (first (second child )) 0)
			(setf (first (first child )) temp)
		)
		(return-from move_uleft_down child)
	)

)

; Will generate list of children based on if the blank is in the first row
(defun first_row (parent)

	(let ((child (copy-list parent)))
		(cond
			; the blank is in upper left corner
			((= (first (car parent)) 0)
				(print (move_uleft_down child))
				(print (move_uleft_right child))
			)
			((= (second(car  parent)) 0) (write "Upper middle"))
			((= (third (car parent)) 0) (write "Upper right"))
		)
	)
)

(defun main ()

	; test list
	(setf x '((0 1 4)(8 3 2)(7 6 5)))
	(print x)
	(cond
		((find 0 (first x)) (first_row x))
		((find 0 (second x))(write "Zero in second row"))
		((find 0 (third x)) (write "Zero in third row")) 
	)
)

(main)
