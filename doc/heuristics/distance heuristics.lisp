;;; Distance Heuristics

;;; These compute spatial distance between positions.

;;; Low-level (coordinate-based)
(defun manhattan-distance (x1 y1 x2 y2)
  "Returns Manhattan distance between two points."
  (+ (abs (- x2 x1)) (abs (- y2 y1))))


(defun euclidean-distance (x1 y1 x2 y2)
  "Returns Euclidean distance between two points."
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))


(defun chebyshev-distance (x1 y1 x2 y2)
  "Returns Chebyshev (chessboard king) distance between two points."
  (max (abs (- x2 x1)) (abs (- y2 y1))))


;;; Example for 15-puzzle
(define-query h-tile-manhattan (?tile)
  (do (bind (loc ?tile $current-row $current-col))
      (bind (goal-loc ?tile $goal-row $goal-col))
      (manhattan-distance $current-row $current-col $goal-row $goal-col)))


;;; Example for Talos-style problems
(define-query h-agent-to-area (?agent ?target-area)
  (do (mvsetq ($ax $ay $az) (get-coordinates ?agent))
      (mvsetq ($tx $ty $tz) (get-fixed-coordinates ?target-area))
      (manhattan-distance $ax $ay $tx $ty)))


;;; Complete Example: 15-Puzzle Heuristic
(define-query heuristic? ()
  (combine-heuristics
    '((1 . h-total-manhattan))
    :combiner :weighted-sum
    :admissible t))


(define-query h-total-manhattan ()
  (do (setq $sum 0)
      (doall (?t tile)
        (do (bind (loc ?t $row $col))
            (bind (goal-loc ?t $goal-row $goal-col))
            (incf $sum (manhattan-distance $row $col $goal-row $goal-col))))
      $sum))