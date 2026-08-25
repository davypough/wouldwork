;;; Filename: -recorder-cycle-chaining.lisp

;;; Explicit recorder orchestration behind the generic goal-chaining interface.  Each
;;; dispatched call requires and searches exactly one new closed, validator-approved
;;; recorder cycle.  An intermediate cycle commits its integrated playback boundary
;;; immediately, retains an
;;; independent history record, prepares a fresh recording shadow, and discards the
;;; just-searched program.
;;; The final operation records the last cycle but retains its ordinary solution result.
;;;
;;; REQUIRES:
;;;   nested  : -recorder-cycle-boundary
;;;   planner : goal-chaining checkpoints, WW-SOLVE, solution selection
;;; PROVIDES:
;;;   functions : solve-recorder-subgoal-form and solve-recorder-final (policy handlers),
;;;               print-recorder-chain-report
;;;   structure : recorder-cycle-record

(include-tech -recorder-cycle-boundary)

(in-package :ww)


(defstruct (recorder-cycle-record (:conc-name recorder-cycle-record.))
  "One committed closed recorder cycle."
  subgoal
  closed-goal
  solution
  report
  boundary-state
  solution-type
  depth
  elapsed-time
  value-change
  cumulative-depth
  cumulative-time
  cumulative-value)


(defvar *recorder-cycle-history* nil
  "Committed recorder cycles in chronological order.")

;; Recorder problems are staged repeatedly in one Lisp image.  A newly staged problem
;; starts a new chain rather than inheriting records from the preceding problem.
(setf *recorder-cycle-history* nil)


(defun copy-recorder-cycle-record-deeply (record)
  "Return an independent copy of recorder cycle RECORD."
  (make-recorder-cycle-record
    :subgoal (copy-tree (recorder-cycle-record.subgoal record))
    :closed-goal (copy-tree (recorder-cycle-record.closed-goal record))
    :solution (copy-solution-deeply (recorder-cycle-record.solution record))
    :report (copy-tree (recorder-cycle-record.report record))
    :boundary-state
      (copy-problem-state (recorder-cycle-record.boundary-state record))
    :solution-type (recorder-cycle-record.solution-type record)
    :depth (recorder-cycle-record.depth record)
    :elapsed-time (recorder-cycle-record.elapsed-time record)
    :value-change (recorder-cycle-record.value-change record)
    :cumulative-depth (recorder-cycle-record.cumulative-depth record)
    :cumulative-time (recorder-cycle-record.cumulative-time record)
    :cumulative-value (recorder-cycle-record.cumulative-value record)))


(defun copy-recorder-cycle-history ()
  "Return independent copies of the committed recorder cycle history."
  (mapcar #'copy-recorder-cycle-record-deeply *recorder-cycle-history*))


(defun restore-recorder-cycle-history (history)
  "Replace recorder cycle history with an independent copy of HISTORY."
  (setf *recorder-cycle-history*
        (mapcar #'copy-recorder-cycle-record-deeply history)))


(register-goal-chaining-checkpoint-extension
  'recorder-cycle-history
  'copy-recorder-cycle-history
  'restore-recorder-cycle-history)


(defun validate-recorder-cycle-orchestration ()
  "Validate a guided baseline and return the one permitted next cycle number."
  (validate-continuation-preconditions)
  (unless (member 'validate-recorder-solution *solution-validators*)
    (error "Recorder cycle solving requires the services installed by (include-tech recorder)."))
  (when *solutions-valid*
    (error "A completed solution is still active. Undo or stage a fresh recorder chain ~
            before starting another recorder cycle."))
  (when (member '(recording-in-progress) (database *start-state*) :test #'equal)
    (error "Guided recorder chaining requires a closed starting state."))
  (unless (funcall (symbol-function 'recorder-closed-ghost-free) *start-state*)
    (error "Guided recorder chaining cannot start from stale ghost state."))
  (let ((next-cycle
          (1+ (funcall (symbol-function 'recorder-cycle-count) *start-state*))))
    (when (> next-cycle *max-recorder-cycles*)
      (error "Guided recorder cycle ~D exceeds *MAX-RECORDER-CYCLES* = ~D."
             next-cycle *max-recorder-cycles*))
    next-cycle))


(defun recorder-cycle-final-goal ()
  "Return the original problem goal, or the current goal for a single-cycle solve."
  (copy-tree (or *final-goal* *goal*)))


(defun recorder-guided-cycle-goal (subgoal cycle-number)
  "Require SUBGOAL at the closed end of exactly the next guided recorder cycle."
  `(and ,(copy-tree subgoal)
        (recorder-cycles-used ,cycle-number)
        (recorder-cycle-ended)))


(defun make-committed-recorder-cycle (subgoal closed-goal)
  "Build a stable history record from the canonical completed solution."
  (let* ((solution
           (copy-solution-deeply (select-continuation-solution)))
         (boundary
           (copy-problem-state (solution.goal solution)))
         (report (build-recorder-report solution))
         (previous (car (last *recorder-cycle-history*)))
         (depth (solution.depth solution))
         (elapsed-time
           (- (solution.time solution) (problem-state.time *start-state*)))
         (value-change
           (- (solution.value solution) (problem-state.value *start-state*))))
    (unless (recorder-cycle-boundary-closed-p boundary)
      (error "Validated recorder cycle ended at an open boundary."))
    (unless (= (getf report :cycle-count) 1)
      (error "A guided recorder search must commit exactly one cycle, not ~D."
             (getf report :cycle-count)))
    (make-recorder-cycle-record
      :subgoal (copy-tree subgoal)
      :closed-goal (copy-tree closed-goal)
      :solution solution
      :report report
      :boundary-state boundary
      :solution-type *solution-type*
      :depth depth
      :elapsed-time elapsed-time
      :value-change value-change
      :cumulative-depth
        (+ depth (if previous (recorder-cycle-record.cumulative-depth previous) 0))
      :cumulative-time
        (+ elapsed-time (if previous (recorder-cycle-record.cumulative-time previous) 0))
      :cumulative-value
        (+ value-change (if previous (recorder-cycle-record.cumulative-value previous) 0)))))


(defun print-recorder-cycle-record (record cycle-number stream)
  "Print one committed recorder cycle RECORD."
  (format stream "~&~%Cycle ~D~%" cycle-number)
  (format stream "Closed goal: ~S~%" (recorder-cycle-record.closed-goal record))
  (format stream "Search policy: ~A~%" (recorder-cycle-record.solution-type record))
  (format stream "Cycle metrics: depth ~D; elapsed time ~A; value change ~A~%"
          (recorder-cycle-record.depth record)
          (recorder-cycle-record.elapsed-time record)
          (recorder-cycle-record.value-change record))
  (let* ((report (recorder-cycle-record.report record))
         (cycles (getf report :cycles)))
    (unless (= (length cycles) 1)
      (error "A guided recorder record must contain exactly one cycle."))
    (print-recorder-report-sequence
      "Integrated sequence" (getf report :integrated) stream)
    (print-recorder-cycle-report (first cycles) stream))
  record)


(defun print-recorder-chain-totals (record stream)
  "Print the cumulative metrics and optimization scope through RECORD."
  (format stream "~&~%Chain totals: depth ~D; elapsed time ~A; value change ~A~%"
          (recorder-cycle-record.cumulative-depth record)
          (recorder-cycle-record.cumulative-time record)
          (recorder-cycle-record.cumulative-value record))
  (format stream
          "Any optimization is local to its cycle; this chain is not globally optimized.~%")
  record)


(defun print-recorder-chain-report
    (&optional (history *recorder-cycle-history*) (stream *standard-output*))
  "Print committed recorder HISTORY in cycle order with local and cumulative metrics."
  (unless history
    (error "No committed recorder cycles are available for a chain report."))
  (format stream "~&~%===== Recorder cycle chain =====~%")
  (loop for record in history
        for cycle-number from 1
        do (print-recorder-cycle-record record cycle-number stream))
  (print-recorder-chain-totals (car (last history)) stream)
  history)


(defun install-next-recorder-cycle-baseline (prepared-state)
  "Commit PREPARED-STATE as the next cycle's independent search baseline."
  (setf (problem-state.name prepared-state) 'recorder-cycle-start
        (problem-state.instantiations prepared-state) nil
        *start-state* prepared-state
        *solution-paths* nil
        *solutions-valid* nil)
  prepared-state)


(defun commit-intermediate-recorder-cycle (subgoal closed-goal)
  "Record one cycle, prepare its next baseline, and discard its searched program."
  (let* ((record (make-committed-recorder-cycle subgoal closed-goal))
         (prepared
           (prepare-recorder-cycle-state
             (recorder-cycle-record.boundary-state record))))
    (setf *recorder-cycle-history*
          (append *recorder-cycle-history* (list record)))
    (install-next-recorder-cycle-baseline prepared)
    (format t "~&Recorder cycle committed; the next cycle has a fresh recording shadow.~%")
    record))


(defun commit-final-recorder-cycle (subgoal closed-goal)
  "Record the final cycle while retaining its completed solution."
  (let ((record (make-committed-recorder-cycle subgoal closed-goal)))
    ;; Restore the user's unstrengthened final goal after the closed solution was accepted.
    (install-compiled-goal subgoal)
    (setf *recorder-cycle-history*
          (append *recorder-cycle-history* (list record))
          *final-goal* nil)
    (format t "~&Final recorder cycle committed.~%")
    record))


(defun run-recorder-cycle-planner ()
  "Run WW-SOLVE without duplicating its single-cycle recorder supplement."
  (let ((saved-printers *solution-report-printers*))
    (unwind-protect
        (progn
          (setf *solution-report-printers*
                (remove 'print-recorder-report *solution-report-printers*))
          (ww-solve))
      (setf *solution-report-printers* saved-printers))))


(defun run-recorder-cycle-search (subgoal final-p cycle-number)
  "Search and commit one closed recorder SUBGOAL, final when FINAL-P is true."
  (let* ((closed-goal (recorder-guided-cycle-goal subgoal cycle-number))
         (completed nil))
    ;; The stateful configured maximum still limits the complete guided history.  This
    ;; narrower dynamic limit prevents one guided call from consuming later cycle slots.
    (let ((*max-recorder-cycles* cycle-number))
      (unwind-protect
          (progn
            (install-compiled-goal closed-goal)
            (run-recorder-cycle-planner)
            (let ((record
                    (when *solutions-valid*
                      (if final-p
                        (commit-final-recorder-cycle subgoal closed-goal)
                        (commit-intermediate-recorder-cycle subgoal closed-goal)))))
              (setf completed t)
              (unless record
                (format t "~&Recorder cycle produced no solution. Retry this cycle, or use ~
                           (ww-undo) to restore the preceding session.~%"))
              (when record
                (print-recorder-chain-report))
              t))
        (unless completed
          (setf *solution-paths* nil
                *solutions-valid* nil)
          (format t "~&Recorder cycle interrupted. Use (ww-undo) to restore the preceding ~
                     session.~%"))))))


(defun solve-recorder-subgoal-form (goal-form)
  "Solve and commit one closed intermediate recorder cycle for GOAL-FORM."
  (let ((cycle-number (validate-recorder-cycle-orchestration)))
    (save-undo-checkpoint)
    (unless *final-goal*
      (setf *final-goal* (copy-tree *goal*)))
    (run-recorder-cycle-search goal-form nil cycle-number)))


(defun solve-recorder-final ()
  "Solve and commit the original goal as the final closed recorder cycle."
  (let ((cycle-number (validate-recorder-cycle-orchestration))
        (goal-form (recorder-cycle-final-goal)))
    (save-undo-checkpoint)
    (run-recorder-cycle-search goal-form t cycle-number)))
