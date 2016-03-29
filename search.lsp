#|
                    ***** SEARCH.LSP *****

General-purpose exhaustive search routine includes both breadth-first
search and depth-first search. Uses graph search with OPEN and CLOSED
lists rather than tree search, to avoid cycles. Does not use heuristics
to limit or guide search.

To solve a specific problem, the functions "generate-successors" and
"goal-state" must be defined. "Generate-successors" takes a state as its
argument and returns a list of child states. "Goal-state?" returns T if
its argument is a goal state, NIL otherwise.

In order to retrace a solution path, nodes are stored as (state parent)
pairs, where "state" is the current state and "parent" is the parent
state. Given a goal node, a solution path is generated by simply tracing
backwards through the parent states.

Author: John M. Weiss, Ph.D.
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

;--------------------------------------------------------------------------

; Node structure: stores state and parent.
(defstruct node state parent depth)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
(defun bfs (start) (search_bfs_dfs start 'bfs))

; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun dfs (start) (search_bfs_dfs start 'dfs))

; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.

; aStar called like (aStar startPuzzle #'numberCorrectHeur)
;(defun aStar (start #'heuristicFunc) ; Not sure if that's right

;    (heursticFunc curState goalState)

;)

(defun search_bfs_dfs (start type)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ( (goal-state? (node-state curNode)) (build-solution curNode CLOSED) )

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

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
                (cond

                    ; BFS - add to end of OPEN list (queue)
                    ((eq type 'bfs) (setf OPEN (append OPEN (list child))))

                    ; DFS - add to start of OPEN list (stack)
                    ((eq type 'dfs) (setf OPEN (cons child OPEN)))

                    ; error handling for incorrect usage
                    (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
                )
            )
        )

        (setf *expandedCount* (length CLOSED))
        (setf *uniqueCount* (+ (length OPEN) (length CLOSED)))
    )
)

;--------------------------------------------------------------------------

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (node node-list)
    (do
        ((path (list (node-state node))))        ; local loop var
        ((null (node-parent node)) path)         ; termination condition

        ; find the parent of the current node
        (setf node (member-state (node-parent node) node-list))

        ; add it to the path
        (setf path (cons (node-state node) path))
    )
)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)

;------------------------------------------------------------------------------
; Function: 	dfs_id
;
; Author:	Dylan Geyer
;
; Description:	This is a helper function to the depth first search. It will
;		call the depth first search routine with an increasing depth
;		bound each time. This will continue until a solution is found.
;
; Parameters:	start - start state representation
;
; Return:	solution path from start state to goal state
;------------------------------------------------------------------------------
(defun dfs_id (start)
	(let ((res nil) (depth 1) (expandedSum 0) (uniqueSum 0))
		;Increase depth bound by 1 until a solution is found
		(loop
			(setf res (bounded_dfs start depth))
			
			;Keep a running total of *expandedCount* and *uniqueCount*
			(setf expandedSum (+ expandedSum *expandedCount*))
			(setf uniqueSum (+ uniqueSum *uniqueCount*))


			;Set the globals to the sums in case we return this loop
			(setf *expandedCount* expandedSum)
			(setf *uniqueSum* uniqueSum)

			(when (not (null res)) (return-from dfs_id res))
			(setf depth (1+ depth))
		)
	)
)

;------------------------------------------------------------------------------
; Function: 	bounded_dfs
;
; Author:	John M. Weiss, Ph.D., Modified by Dylan Geyer
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
;		depthbound - Maximum depth to search, default to 9999
;
; Return:	solution path from start state to goal state
;------------------------------------------------------------------------------
(defun bounded_dfs (start &optional (depthbound 9999))
    (do*                                                    		; note use of sequential DO*
        (                                                   		; initialize local loop vars
            (curNode (make-node :state start :parent nil :depth 0)) ; current node: (start nil)
            (OPEN (list curNode))                           		; OPEN list:    ((start nil))
            (CLOSED nil)                                    		; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ((goal-state? (node-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

		; if curNodes depth is less than depthbound generate successors
		(if (< (node-depth curNode) depthbound)

			; add successors of current node to OPEN
			(dolist (child (generate-successors (node-state curNode)))

				; for each child node
				(setf child (make-node :state child :parent (node-state curNode) :depth (1+ (node-depth curNode))))

				; if the node is not on OPEN or CLOSED
				(if (and (not (member child OPEN   :test #'equal-states))
				         (not (member child CLOSED :test #'equal-states)))
					
				    ; add it to the OPEN list
					; DFS - add to start of OPEN list (stack)
					(setf OPEN (cons child OPEN))
				)
			)
		)

        (setf *expandedCount* (length CLOSED))
        (setf *uniqueCount* (+ (length OPEN) (length CLOSED)))
    )
)
