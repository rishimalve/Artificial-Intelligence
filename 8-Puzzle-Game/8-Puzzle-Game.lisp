(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 9 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setq puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defparameter *goal* #(1 2 3 4 5 6 7 8 9 8))

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setq ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations 20000))
    "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited states.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."

    (let* ((queue (make-empty-queue)) (history nil) (iterations 0) (state initial-state) (child nil))
        (funcall enqueueing-function state queue)
        (setf history (nconc history (list (puzzle-from-state state))))
        (loop
            (incf iterations)
            (when (or (> iterations maximum-iterations) (empty-queue? queue)) (return 'failed))
            (setf state (remove-front queue))
            (if (funcall goal-test state)
                (progn 
                    (format t "~%Number of Iteratiorns: ~a" iterations)
                    (return state))
                (progn 
                    (foreach-valid-move (move (puzzle-from-state state))
                        (setf child (make-move move (puzzle-from-state state)))
                        (if (null (dolist (h history) (if (equalp h child) (return T))))
                            (progn 
                                (funcall enqueueing-function (build-state child state) queue)
                                (setf history (nconc history (list child)))))))))))


(defun IDA-Star (s max-depth enqueueing-function)
    (let ((depth 0) (result nil))
    (loop
        (incf depth)
        (format t "~%Depth: ~a" depth)
        (when (> depth max-depth) (return 'failed))
        (setf result (general-search s #'goal-p enqueueing-function))
        (if (not (equalp result 'failed)) (return result)))
    (if (> depth max-depth) (print 'failed) (print result))))

(defun goal-p (state)
    "Returns T if state is a goal state, else NIL.  Our goal test."

    (if (equalp (puzzle-from-state state) *goal*) T NIL))

(defun dfs-enqueuer (state queue)
    "Enqueues in depth-first order"

    (enqueue-at-front queue state))

(defun bfs-enqueuer (state queue)
    "Enqueues in breadth-first order"

    (enqueue-at-end queue state))                                         

(defun manhattan-enqueuer (state queue)
    "Enqueues by manhattan distance"

    (enqueue-by-priority queue #'f-n-manhattan state))

(defun f-n-manhattan (state)
    (let* ((puzzle (puzzle-from-state state)) (cnt 0) (pos 0) (psize (- (array-total-size puzzle) 1)) (side (isqrt psize)))
        (loop for i from 0 to (- psize 1)
            do
                (if (and (not (equalp (aref puzzle i) (aref *goal* i))) (not (equalp (aref puzzle i) 9)))
                    (progn
                        (setf pos (position  (aref puzzle i) *goal*))
                        (setf cnt (+ cnt (abs ( - (floor i side) (floor pos side)))))
                        (setf cnt (+ cnt (abs ( - (mod i side) (mod pos side))))))))
                        (+ cnt (depth state))))


(defun num-out-enqueuer (state queue)
    "Enqueues by number of tiles out of place"

    (enqueue-by-priority queue #'f-n-num-out state))

(defun f-n-num-out (state)
    (let* ((puzzle (puzzle-from-state state)) (psize (- 1 (array-total-size puzzle))) (cnt 0))
    (loop for i from 0 to psize
        do
            (if (and (equalp (aref puzzle i) (aref *goal* i)) (not (equalp (aref puzzle i) 9))) (incf cnt)))
            (+ cnt (depth state))))


;;; Solves in 4 moves:
;(setq s (make-initial-state '(9 2 3 1 4 6 7 5 8)))

;;; Solves in 8 moves:
(setq s (make-initial-state '(2 4 3 1 5 6 9 7 8)))

;;; Solves in 16 moves:
;(setq s (make-initial-state '(2 3 9 5 4 8 1 6 7)))

;;; Solves in 24 moves:
;(setq s (make-initial-state '(1 8 9 3 2 4 6 5 7)))

;;; easy or hard to solve?  Why?
;(setq s (make-initial-state '(9 2 3 4 5 6 7 8 1)))


(print-solution (IDA-Star s 10 #'manhattan-enqueuer))
