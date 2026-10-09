;;; Filename: problem-queensN.lisp
;;; Design notes
;;; Counts N-queens solutions (or their rotation/reflection classes). The
;;; optimizations applied, each with its reason:
;;;
;;; 1. Constraint-satisfaction formulation.
;;;    Rows are the variables and columns their values, assigned in a fixed row
;;;    order. Queens have no identities, so each board has exactly one
;;;    construction path: no duplicate boards, no cycles, no repeated states.
;;;
;;; 2. One action with a next-row index.
;;;    A single action places a queen in the next row, and the index says which
;;;    row that is. This keeps the spec independent of the board size, where the
;;;    csp problem type alone would need one action per row.
;;;
;;; 3. Bit-mask conflict tests.
;;;    Occupied columns and both diagonal families are each kept as one integer
;;;    mask, so testing a candidate column is a single bit test instead of a scan
;;;    over the queens already placed.
;;;
;;; 4. Diagonal masks kept relative to the next row.
;;;    Each placement shifts the two diagonal masks one column, so they always
;;;    line up with the next row's columns. Masks stay within the board width
;;;    (fixnums up to a very large board) and attacks on later rows are a few
;;;    shifts away.
;;;
;;; 5. Cheap dead-end pruning.
;;;    Before expanding a partial board, check whether any remaining row already
;;;    has every column attacked; if so, abandon the branch. Thanks to item 4
;;;    this costs one combined mask test per remaining row.
;;;
;;; 6. Whole state in one fixed-size fact.
;;;    The next row, the three masks and the board share one fact; the board is
;;;    packed into one integer, a few bits per row. The state stays the same size
;;;    whatever the board size, each candidate column needs one lookup, and each
;;;    placement makes one write (one undo record under backtracking).
;;;
;;; 7. Reflection pruning at the first row (class mode only).
;;;    A board's mirror image has its first queen on the opposite side (or in the
;;;    same middle column), so only left-half first columns are tried. The
;;;    restriction sits in the action's precondition, so right-half branches are
;;;    never generated at all.
;;;
;;; 8. One representative per symmetry class (class mode only).
;;;    A complete board is accepted only if it is the smallest of its eight
;;;    rotations and reflections. The check runs on complete boards only, which
;;;    are a small fraction of the search.
;;;
;;; 9. Parallel backtracking.
;;;    Each placement changes only one small fact, so updating one working state
;;;    in place and undoing on return is cheaper than copying a state per step.
;;;    The search is split into independent subtrees run on parallel threads.
;;;
;;; Limit: the packed board fits a fixnum only up to N=15; beyond that it becomes
;;; a bignum, one small allocation per step.

(in-package :ww)

(defparameter *N* 15)
(defparameter *queens-count-classes* t
  "T counts rotation/reflection classes; NIL counts all boards.
   Set after staging and changing threads. Never change during a search.")
(defparameter *queens-width* (integer-length (1- *N*))
  "Bits per row in the packed board; each field holds a zero-based column.")
(defparameter *queens-full* (1- (ash 1 *N*))
  "Mask with one bit set for every column.")

(ww-set *problem-name* queensN)
(ww-set *problem-type* csp)
(ww-set *solution-type* count)
(ww-set *tree-or-graph* tree)
(ww-set *algorithm* backtracking)
(ww-set *threads* 16)

(define-types
  column (compute (loop for j from 1 to *N* collect j)))

(define-dynamic-relations
  (queens> $fixnum $fixnum $fixnum $fixnum $integer))  ;next row, columns, left diagonals,
                                                        ;right diagonals, packed board

(define-action assign-queen-to-col
  1
  (?col column)
  (and (bind (queens> $row $columns $left $right $board))
       (<= $row *N*)
       (or (> $row 1)
           (not *queens-count-classes*)
           (<= ?col (ceiling *N* 2)))
       (not (logbitp (1- ?col) (logior $columns $left $right))))
  (?col)
  (assert (queens> (1+ $row)
                   (logior $columns (ash 1 (1- ?col)))
                   (logand *queens-full* (ash (logior $left (ash 1 (1- ?col))) 1))
                   (ash (logior $right (ash 1 (1- ?col))) -1)
                   (logior $board (ash (1- ?col) (* (1- $row) *queens-width*))))))

(define-init (queens> 1 0 0 0 0))

(defun queens-empty-future-row-p (rows-left columns left right full)
  "True when one of the ROWS-LEFT remaining rows, starting with the next row, has
   every column attacked. LEFT and RIGHT are diagonal masks for the next row."
  (loop for k from 0 below rows-left
        for l = left then (logand full (ash l 1))
        for r = right then (ash r -1)
        thereis (= full (logior columns l r))))

(define-query prune-state? ()
  (and (bind (queens> $row $columns $left $right $board))
       (queens-empty-future-row-p (- (1+ *N*) $row) $columns $left $right *queens-full*)))

(defun queens-unpack-board (packed size width)
  "Return a vector of zero-based columns, one per row, from the packed board."
  (let ((board (make-array size)))
    (dotimes (row size board)
      (setf (aref board row) (ldb (byte width (* row width)) packed)))))

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
  (and (bind (queens> $row $columns $left $right $packed))
       (queens-canonical-board-p (queens-unpack-board $packed *N* *queens-width*))))

(define-goal
  (and (bind (queens> $row $columns $left $right $board))
       (> $row *N*)
       (or (not *queens-count-classes*) (canonical-queens-board?))))
