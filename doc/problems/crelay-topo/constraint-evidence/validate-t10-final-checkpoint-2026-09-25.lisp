;;; T10 final acceptance (2026-09-25), after D approved the NORMALIZE-VALIDATION-ACTIONS fix
;;; (per-entry normalization of mixed plain/timestamped paths).  Fresh stage, threads 16,
;;; import t10-final-checkpoint.txt by replay (80 replay-built + 7 search-found actions),
;;; then VALIDATE-SEARCH-CHECKPOINT on the whole path from the original state.  NO search.
;;; Acceptance: SUCCESS-P, GOAL-CHECKED-P and GOAL-SATISFIED-P all T.
;;; Run after Wouldwork is reloaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-t10-final-checkpoint-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *t10-final-checkpoint* nil)


(defparameter *t10-final-validation* nil)


(defun validate-t10-final-checkpoint ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (setf *t10-final-checkpoint*
        (import-search-checkpoint
          (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-final-checkpoint.txt"
                           (asdf:system-source-directory :wouldwork))))
  (setf *t10-final-validation* (validate-search-checkpoint *t10-final-checkpoint*))
  (format t "~&CHECKPOINT ~S~%" *t10-final-checkpoint*)
  (format t "VALIDATION SUCCESS-P ~S  ACTION-COUNT ~S  GOAL-CHECKED-P ~S  GOAL-SATISFIED-P ~S~%"
          (action-sequence-validation-success-p *t10-final-validation*)
          (action-sequence-validation-action-count *t10-final-validation*)
          (action-sequence-validation-goal-checked-p *t10-final-validation*)
          (not (null (action-sequence-validation-goal-satisfied-p *t10-final-validation*))))
  (unless (action-sequence-validation-success-p *t10-final-validation*)
    (format t "FAILED at action ~S: ~S~%REASON: ~A~%"
            (action-sequence-validation-failure-index *t10-final-validation*)
            (action-sequence-validation-failure-action *t10-final-validation*)
            (action-sequence-validation-failure-reason *t10-final-validation*)))
  *t10-final-validation*)


(validate-t10-final-checkpoint)
