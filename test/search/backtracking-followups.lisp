;;; Load after Wouldwork, then call (ww::test-backtracking-followups).
;;; Uses the BACKTRACKING-FOLLOWUPS fixture; leaves it staged.
;;; Checks that FINALLY followups run and are undone under backtracking, that a
;;; followup's (inconsistent-state) drops the successor, and that immediate-inverse
;;; cycle pruning does not discard SWITCH-OFF after SWITCH-ON's followup write.
;;; Also checks, under both algorithms, that asserts generated in a loop each carry
;;; only their own followup.
(in-package :ww)


(defun test-backtracking-followups ()
  (stage backtracking-followups)
  (ww-set *algorithm* backtracking)
  (ww-set *threads* 0)
  (ww-set *bt-cycle-check* nil)
  (check-followup-run "immediate-inverse")
  (ww-set *bt-cycle-check* t)
  (check-followup-run "full-path")
  (ww-set *bt-cycle-check* nil)
  (check-followups-per-assert-bt)
  (ww-set *algorithm* depth-first)
  (check-followups-per-assert-df)
  (format t "~&BACKTRACKING-FOLLOWUPS-CHECKS-PASSED~%"))


(defun check-followup-run (mode)
  (let ((*standard-output* (make-broadcast-stream)))
    (solve))
  ;; Without followups, (seen-on) is never written and no solution exists.
  (assert (= 1 (length *solution-paths*)))
  (assert (equal '((switch-on) (switch-off))
                 (mapcar #'second (solution.path (first *solution-paths*)))))
  ;; JAM at the root and JAM after SWITCH-ON are both dropped.
  (assert (= 2 *inconsistent-states-dropped*))
  ;; Every followup write is undone on backtracking.
  (assert (null *choice-stack*))
  (assert (equalp (problem-state.idb *start-state*)
                  (problem-state.idb *backtrack-state*)))
  (format t "~&FOLLOWUPS PASS: ~A cycle check~%" mode))


(defun check-followups-per-assert-bt ()
  (let* ((*backtrack-state* (copy-problem-state *start-state*))
         (*choice-stack* nil)
         (action (find 'mark-levels *actions* :key #'action.name))
         (choices (generate-choices-for-single-combination-bt action nil t 0)))
    (assert (= 3 (length choices)))
    (assert (every (lambda (choice) (equal '((mark-seen!)) (choice.followups choice)))
                   choices))
    (assert (equalp (problem-state.idb *start-state*)
                    (problem-state.idb *backtrack-state*))))
  (format t "~&FOLLOWUPS PASS: one followup per assert, backtracking~%"))


(defun check-followups-per-assert-df ()
  (let* ((action (find 'mark-levels *actions* :key #'action.name))
         (updates (funcall (action.eff-defun-name action)
                           (copy-problem-state *start-state*))))
    (assert (= 3 (length updates)))
    (assert (every (lambda (update) (equal '((mark-seen!)) (update.followups update)))
                   updates)))
  (format t "~&FOLLOWUPS PASS: one followup per assert, depth-first~%"))
