;;; T10 final-goal search (option 1, approved by D 2026-09-25).  Fresh stage, threads 16,
;;; import the location15 archive by replay, then ONE search for the original goal
;;; (has-location agent1 location19) at cutoff 10.  The first run at cutoff 12 ran out of
;;; memory; D approved this rerun at cutoff 10, where the known 10-action leg lies.
;;; No deepening, no retry.
;;; On success: export the final archive first, then VALIDATE-SEARCH-CHECKPOINT on the whole
;;; accumulated path from the original state.  Acceptance: SUCCESS-P, GOAL-CHECKED-P and
;;; GOAL-SATISFIED-P all T.  The hand-derived final leg is 10 actions.
;;; Run after build-c3-location15-checkpoint-2026-09-25.lisp has exported its archive:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/run-t10-final-search-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *t10-location15-checkpoint* nil)


(defparameter *t10-final-checkpoint* nil)


(defparameter *t10-final-validation* nil)


(defun run-t10-final-search ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (setf *t10-location15-checkpoint*
        (import-search-checkpoint
          (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-c3-location15-checkpoint.txt"
                           (asdf:system-source-directory :wouldwork))))
  (ww-set *depth-cutoff* 10)
  (ww-set *tree-or-graph* graph)
  (assert (null *symmetry-pruning*))
  (setf *min-steps-pruning-enabled* t)
  (multiple-value-bind (checkpoint found-p)
      (solve-search-checkpoint *t10-location15-checkpoint*
                               '(has-location agent1 location19))
    (setf *t10-final-checkpoint* checkpoint)
    ;; Persist first so later reporting cannot lose a successful result.
    (when found-p
      (export-search-checkpoint checkpoint
        (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-final-checkpoint.txt"
                         (asdf:system-source-directory :wouldwork))))
    (format t "~&FINAL-FOUND-P ~S~%FINAL-CHECKPOINT ~S~%" found-p checkpoint)
    (format t "FINAL-OUTCOME ~S~%FINAL-TRUNCATION ~S~%FINAL-CUTOFF-HITS ~S~%"
            *last-search-outcome* *depth-cutoff-truncated* *depth-cutoff-hits*)
    (format t "FINAL-SETTINGS ~S~%"
            (list :threads *threads* :cutoff *depth-cutoff*
                  :graph *tree-or-graph* :symmetry *symmetry-pruning*
                  :minimum-steps *min-steps-pruning-enabled*))
    (when found-p
      (format t "FINAL-SEGMENT ~S~%"
              (solution.path (goal-chain-phase-solution
                               (car (last (goal-chain-session-phases
                                            (search-checkpoint-session checkpoint)))))))
      (setf *t10-final-validation* (validate-search-checkpoint checkpoint))
      (format t "VALIDATION SUCCESS-P ~S  ACTION-COUNT ~S  GOAL-CHECKED-P ~S  GOAL-SATISFIED-P ~S~%"
              (action-sequence-validation-success-p *t10-final-validation*)
              (action-sequence-validation-action-count *t10-final-validation*)
              (action-sequence-validation-goal-checked-p *t10-final-validation*)
              (not (null (action-sequence-validation-goal-satisfied-p *t10-final-validation*)))))
    checkpoint))


(run-t10-final-search)
