;;; Goal-counting Heuristics

;;; These count unsatisfied conditions in the goal.


(defun count-unsatisfied (predicate-results)
  "Counts how many items in list are nil/false."
  (count nil predicate-results))


(defun count-satisfied (predicate-results)
  "Counts how many items in list are non-nil/true."
  (count-if #'identity predicate-results))

;;;;;;;;;;;;;;;;;;;;


;;; Count inactive receivers (Talos)
(define-query h-inactive-receivers ()
  (do (setq $count 0)
      (doall (?r receiver)
        (if (not (active ?r))
          (incf $count)))
      $count))


;;; Count misplaced tiles (15-puzzle)
(define-query h-misplaced-tiles ()
  (do (setq $count 0)
      (doall (?t tile)
        (if (not (tile-in-goal-position ?t))
          (incf $count)))
      $count))