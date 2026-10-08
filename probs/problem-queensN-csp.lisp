;;; Filename: problem-queensN-csp.lisp
;;; Design notes
;;; Assign rows in a fixed order so each board has one construction path.
;;; Track occupied columns and both diagonal families with three integer bit masks.
;;; The mask tuple is directed: its three fields have distinct roles.
;;; Keep row assignments for the symmetry check and the printed example.
;;; Count accepted boards, retaining only one example path and goal state.
;;; Optionally accept only the smallest board in each rotation/reflection class.
;;; In class mode, prune right-half first placements before expanding their branches.
;;; In both modes, prune if any future row has no legal column.
;;; Check completed boards against all rotations/reflections to select representatives.
;;; Keep symmetry checking optional until its cost at larger sizes is measured.

(in-package :ww)

(defparameter *N* 13)
(defparameter *queens-count-classes* t
  "T counts rotation/reflection classes; NIL counts all boards.
   Set after staging and changing threads. Never change during a search.")

(ww-set *problem-name* queensN-csp)
(ww-set *problem-type* csp)
(ww-set *solution-type* count)
(ww-set *tree-or-graph* tree)
(ww-set *threads* 16)

(define-types
  queen-row (compute (loop for i from 1 to *N* collect i))
  column    (compute (loop for j from 1 to *N* collect j)))

(define-dynamic-relations
  (occupied> $integer $integer $integer)
  (assigned queen-row $column)
  (next-row $fixnum))

(define-action assign-queen-to-col
  1
  (?col column)
  (and (bind (next-row $current-row))
       (<= $current-row *N*)
       (bind (occupied> $columns $sum-diagonals $difference-diagonals))
       (not (logbitp (1- ?col) $columns))
       (not (logbitp (- (+ $current-row ?col) 2) $sum-diagonals))
       (not (logbitp (+ (- $current-row ?col) (1- *N*)) $difference-diagonals)))
  (?col)
  (assert (assigned $current-row ?col)
          (occupied> (logior $columns (ash 1 (1- ?col)))
                     (logior $sum-diagonals (ash 1 (- (+ $current-row ?col) 2)))
                     (logior $difference-diagonals
                             (ash 1 (+ (- $current-row ?col) (1- *N*)))))
          (next-row (1+ $current-row))))

(define-init (next-row 1) (occupied> 0 0 0))

(defun queens-empty-future-row-p (next-row columns sum-diagonals difference-diagonals size)
  "True when a future row has no legal column under the current assignments."
  (loop with unused = (logandc2 (1- (ash 1 size)) columns)
        for row from next-row to size
        thereis
        (not (loop with candidates = (logandc2 unused (ash sum-diagonals (- 1 row)))
                   while (plusp candidates)
                   for bit = (logand candidates (- candidates))
                   for col = (integer-length bit)
                   thereis (not (logbitp (+ (- row col) (1- size)) difference-diagonals))
                   do (setf candidates (logand candidates (1- candidates)))))))

(define-query prune-state? ()
  (or (and *queens-count-classes*
           (next-row 2)
           (bind (assigned 1 $first-column))
           (> $first-column (ceiling *N* 2)))
      (and (bind (next-row $row))
           (bind (occupied> $columns $sum-diagonals $difference-diagonals))
           (queens-empty-future-row-p $row $columns $sum-diagonals
                                     $difference-diagonals *N*))))

(defun queens-board-no-greater-p (board image reverse-rows complement-columns)
  "Compare BOARD lexicographically with a reflected IMAGE, using zero-based columns."
  (loop with last = (1- (length board))
        for row from 0 to last
        for left = (aref board row)
        for raw = (aref image (if reverse-rows (- last row) row))
        for right = (if complement-columns (- last raw) raw)
        when (/= left right) return (< left right)
        finally (return t)))

(defun queens-transpose-board (board)
  "Transpose a complete board: its row-to-column permutation becomes its inverse."
  (let ((inverse (make-array (length board))))
    (dotimes (row (length board) inverse)
      (setf (aref inverse (aref board row)) row))))

(defun queens-canonical-board-p (board)
  "Accept exactly the lexicographic minimum of the board's eight square symmetries.
   Equal images are accepted, so smaller symmetry classes are counted correctly."
  (and (queens-board-no-greater-p board board nil t)
       (queens-board-no-greater-p board board t nil)
       (queens-board-no-greater-p board board t t)
       (let ((inverse (queens-transpose-board board)))
         (and (queens-board-no-greater-p board inverse nil nil)
              (queens-board-no-greater-p board inverse nil t)
              (queens-board-no-greater-p board inverse t nil)
              (queens-board-no-greater-p board inverse t t)))))

(define-query canonical-queens-board? ()
  (do (setf $board (make-array *N*))
      (doall (?row queen-row)
        (do (bind (assigned ?row $col))
            (setf (aref $board (1- ?row)) (1- $col))))
      (queens-canonical-board-p $board)))

(define-goal
  (and (bind (next-row $row))
       (> $row *N*)
       (or (not *queens-count-classes*) (canonical-queens-board?))))
