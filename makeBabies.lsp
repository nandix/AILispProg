; moves element from upper left corner to the right one space
(defun move_uleft_right (parent)

	(let ((child (copy-list parent))(temp))

		(setf temp (second (first  child)))
		(setf (second (first child)) 0)			
		(setf (first (first child)) temp)

		(return-from move_uleft_right  child)
	)
)
; Moves element from upper left corner down one space
(defun move_uleft_down (parent)

	(let ((child (copy-list parent))(temp))	

		(setf temp (first (second child)))
		(setf (first (second child )) 0)
		(setf (first (first child )) temp)

		(return-from move_uleft_down  child)
	)

)

(defun upper_left (parent)
	(let ((child()))
		(append (list (move_uleft_down parent))
			(list (move_uleft_right parent))
		)
	)
)

(defun move_blank (parent)

	(let ((tempchildren()))
		(cond
			; if blank is in first row
			((find 0 (first x)) 
				(cond
					; if blank is in the upper left corner
					((= (first (car parent)) 0)
						(setf tempchildren (upper_left parent))
						(print tempchildren)
					)

					; if blank is in the upper middle box
					((= (second (car parent)) 0)
					)

					; if blank is in upper right corner
					((= (third (car parent)) 0)
					)
				)
			)
			
			((find 0 (second x)) ); if blank is in second row
			((find 0 (third x)) ); if blank is in third row
		)
	)

)

; Will generate list of children based on if the blank is in the first row
(defun first_row (parent)

	(let ((child (copy-list parent)))
		(cond
			; the blank is in upper left corner
			((= (first (car parent)) 0)
				(print (move_uleft_down parent))
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
	(move_blank x)
)

(main)
