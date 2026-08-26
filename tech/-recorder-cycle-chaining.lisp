;;; Filename: -recorder-cycle-chaining.lisp

;;; Recorder-aware sequential subgoals behind the generic goal-chaining interface.  A user
;;; subgoal is a certified checkpoint in the integrated path, not an implied recorder-cycle
;;; boundary.  Consecutive searches therefore continue through an open cycle, close one, or
;;; start another according to the ordinary recorder actions selected by the planner.
;;;
;;; Every accepted segment is appended to the chain and the complete accumulated path is
;;; replayed from the original staged state.  This makes an open-cycle checkpoint safe to use
;;; as the next search baseline while retaining the original pre-recording history needed for
;;; recorder validation and final reporting.  Recorder cycles are derived from the accumulated
;;; path rather than assigned one per user subgoal.
;;;
;;; REQUIRES:
;;;   nested  : -recorder-cycle-boundary
;;;   planner : goal-chaining checkpoints, WW-SOLVE, solution selection
;;; PROVIDES:
;;;   functions : solve-recorder-subgoal-form and solve-recorder-final (policy handlers),
;;;               print-recorder-subgoal-chain-report
;;;   structures: recorder-subgoal-segment, recorder-subgoal-chain

(include-tech -recorder-cycle-boundary)

(in-package :ww)


(defstruct (recorder-subgoal-segment (:conc-name recorder-subgoal-segment.))
  "One accepted search segment ending at a user checkpoint or the original final goal."
  goal
  solution
  solution-type
  final-p
  cumulative-depth
  cumulative-time
  cumulative-value
  cycle-count
  recording-open-p)


(defstruct (recorder-subgoal-chain (:conc-name recorder-subgoal-chain.))
  "The original baseline and chronological accepted segments of one guided solve."
  origin-state
  original-goal
  segments)


(defvar *recorder-subgoal-chain* nil
  "Active recorder checkpoint chain, or NIL before the first guided subgoal.")

;; Recorder problems are staged repeatedly in one Lisp image.  A newly staged problem starts
;; a new checkpoint chain rather than inheriting the preceding problem's accepted segments.
(setf *recorder-subgoal-chain* nil)


(defun copy-recorder-subgoal-segment-deeply (segment)
  "Return an independent copy of recorder checkpoint SEGMENT."
  (make-recorder-subgoal-segment
    :goal (copy-tree (recorder-subgoal-segment.goal segment))
    :solution (copy-solution-deeply (recorder-subgoal-segment.solution segment))
    :solution-type (recorder-subgoal-segment.solution-type segment)
    :final-p (recorder-subgoal-segment.final-p segment)
    :cumulative-depth (recorder-subgoal-segment.cumulative-depth segment)
    :cumulative-time (recorder-subgoal-segment.cumulative-time segment)
    :cumulative-value (recorder-subgoal-segment.cumulative-value segment)
    :cycle-count (recorder-subgoal-segment.cycle-count segment)
    :recording-open-p (recorder-subgoal-segment.recording-open-p segment)))


(defun copy-active-recorder-subgoal-chain ()
  "Return an independent copy of the active recorder checkpoint chain."
  (when *recorder-subgoal-chain*
    (make-recorder-subgoal-chain
      :origin-state
        (copy-problem-state
          (recorder-subgoal-chain.origin-state *recorder-subgoal-chain*))
      :original-goal
        (copy-tree
          (recorder-subgoal-chain.original-goal *recorder-subgoal-chain*))
      :segments
        (mapcar #'copy-recorder-subgoal-segment-deeply
                (recorder-subgoal-chain.segments *recorder-subgoal-chain*)))))


(defun restore-recorder-subgoal-chain (chain)
  "Restore recorder checkpoint CHAIN from an undo snapshot."
  (setf *recorder-subgoal-chain* chain))


(register-goal-chaining-checkpoint-extension
  'recorder-subgoal-chain
  'copy-active-recorder-subgoal-chain
  'restore-recorder-subgoal-chain)


(defun recorder-state-recording-open-p (state)
  "Whether STATE is inside an active recorder cycle."
  (member '(recording-in-progress) (database state) :test #'equal))


(defun recorder-state-cycle-count (state)
  "Return STATE's number of recorder cycles started."
  (funcall (symbol-function 'recorder-cycle-count) state))


(defun recorder-subgoal-chain-origin ()
  "Return the active chain's original staged state."
  (recorder-subgoal-chain.origin-state *recorder-subgoal-chain*))


(defun recorder-subgoal-chain-segments ()
  "Return the active chain's accepted segments in chronological order."
  (recorder-subgoal-chain.segments *recorder-subgoal-chain*))


(defun recorder-subgoal-chain-path (&optional (segments (recorder-subgoal-chain-segments)))
  "Return a fresh concatenation of SEGMENTS' integrated action paths."
  (loop for segment in segments
        append (copy-tree
                 (solution.path (recorder-subgoal-segment.solution segment)))))


(defun recorder-subgoal-replay-state-equal-p (state1 state2)
  "Whether replayed STATE1 is the same planning checkpoint as recorded STATE2."
  (and (equalp (problem-state.idb state1) (problem-state.idb state2))
       (= (problem-state.time state1) (problem-state.time state2))
       (= (problem-state.value state1) (problem-state.value state2))
       (equalp (problem-state.happenings state1)
               (problem-state.happenings state2))))


(defun make-recorder-subgoal-cumulative-solution
    (&optional (segments (recorder-subgoal-chain-segments)))
  "Build one complete solution record from accepted recorder checkpoint SEGMENTS."
  (unless segments
    (error "No recorder subgoal segments are available."))
  (let* ((last-segment (car (last segments)))
         (last-solution (recorder-subgoal-segment.solution last-segment))
         (path (recorder-subgoal-chain-path segments)))
    (make-solution
      :depth (length path)
      :time (solution.time last-solution)
      :value (solution.value last-solution)
      :path path
      :goal (copy-problem-state (solution.goal last-solution)))))


(defun validate-recorder-subgoal-segment-replays (segments)
  "Replay SEGMENTS in order and require every stored checkpoint state to recur exactly."
  (let ((current-state (copy-problem-state (recorder-subgoal-chain-origin))))
    (dolist (segment segments current-state)
      (let* ((solution (recorder-subgoal-segment.solution segment))
             (validation
               (validate-action-sequence current-state (solution.path solution))))
        (unless (action-sequence-validation-success-p validation)
          (error "Accumulated recorder subgoal replay failed at checkpoint ~S: ~S"
                 (recorder-subgoal-segment.goal segment)
                 (action-sequence-validation-failure-reason validation)))
        (let ((replayed-state
                (action-sequence-validation-final-state validation)))
          (unless (recorder-subgoal-replay-state-equal-p
                    replayed-state (solution.goal solution))
            (error "Accumulated recorder replay did not reproduce checkpoint ~S."
                   (recorder-subgoal-segment.goal segment)))
          (setf current-state replayed-state))))))


(defun validate-recorder-subgoal-cumulative-path (segments)
  "Certify SEGMENTS both checkpoint by checkpoint and as one complete recorder path."
  (let* ((replayed-state (validate-recorder-subgoal-segment-replays segments))
         (solution (make-recorder-subgoal-cumulative-solution segments)))
    (unless (recorder-subgoal-replay-state-equal-p
              replayed-state (solution.goal solution))
      (error "Accumulated recorder replay ended in the wrong final state."))
    (multiple-value-bind (valid-p diagnostic)
        (validate-recorder-solution
          (recorder-subgoal-chain-origin)
          (solution.path solution)
          (solution.goal solution))
      (unless valid-p
        (error "Accumulated recorder path was rejected: ~S" diagnostic)))
    solution))


(defun start-recorder-subgoal-chain (original-goal)
  "Create a checkpoint chain rooted at the current staged state."
  (setf *recorder-subgoal-chain*
        (make-recorder-subgoal-chain
          :origin-state (copy-problem-state *start-state*)
          :original-goal (copy-tree original-goal)
          :segments nil)))


(defun recorder-subgoal-cumulative-metrics (solution)
  "Return cumulative depth, elapsed time, and value change for SOLUTION."
  (let ((origin (recorder-subgoal-chain-origin)))
    (values
      (solution.depth solution)
      (- (solution.time solution) (problem-state.time origin))
      (- (solution.value solution) (problem-state.value origin)))))


(defun make-recorder-subgoal-segment-record (goal-form final-p solution)
  "Build a segment record for GOAL-FORM and selected segment SOLUTION."
  (let* ((prior-segments (recorder-subgoal-chain-segments))
         (temporary-segment
           (make-recorder-subgoal-segment
             :goal (copy-tree goal-form)
             :solution (copy-solution-deeply solution)
             :solution-type *solution-type*
             :final-p final-p))
         (segments (append prior-segments (list temporary-segment)))
         (cumulative-solution
           (validate-recorder-subgoal-cumulative-path segments)))
    (multiple-value-bind (depth elapsed-time value-change)
        (recorder-subgoal-cumulative-metrics cumulative-solution)
      (setf (recorder-subgoal-segment.cumulative-depth temporary-segment) depth
            (recorder-subgoal-segment.cumulative-time temporary-segment) elapsed-time
            (recorder-subgoal-segment.cumulative-value temporary-segment) value-change
            (recorder-subgoal-segment.cycle-count temporary-segment)
              (recorder-state-cycle-count (solution.goal solution))
            (recorder-subgoal-segment.recording-open-p temporary-segment)
              (not (null (recorder-state-recording-open-p
                           (solution.goal solution)))))
      (values temporary-segment cumulative-solution))))


(defun append-recorder-subgoal-segment (segment)
  "Append accepted SEGMENT to the active chain."
  (setf (recorder-subgoal-chain.segments *recorder-subgoal-chain*)
        (append (recorder-subgoal-chain-segments) (list segment))))


(defun print-recorder-subgoal-status (segment stream)
  "Print the user checkpoint and current recorder-cycle status for SEGMENT."
  (let ((solution (recorder-subgoal-segment.solution segment)))
    (format stream "~&~%~:[Subgoal checkpoint~;Final goal~] reached: ~S~%"
            (recorder-subgoal-segment.final-p segment)
            (recorder-subgoal-segment.goal segment))
    (format stream "  Segment depth: ~D~%" (solution.depth solution))
    (format stream "  Cumulative depth: ~D~%"
            (recorder-subgoal-segment.cumulative-depth segment))
    (cond
      ((recorder-subgoal-segment.recording-open-p segment)
       (format stream "  Recorder cycle ~D remains open.~%"
               (recorder-subgoal-segment.cycle-count segment)))
      ((zerop (recorder-subgoal-segment.cycle-count segment))
       (format stream "  No recorder cycle is active.~%"))
      (t
       (format stream "  Recorder state is closed after cycle ~D.~%"
               (recorder-subgoal-segment.cycle-count segment))))
    (format stream "  Accumulated prefix replay: accepted.~%")))


(defun print-recorder-subgoal-chain-report
    (&optional (chain *recorder-subgoal-chain*) (stream *standard-output*))
  "Print every accepted checkpoint in CHAIN and its cumulative metrics."
  (unless chain
    (error "No recorder subgoal chain is active."))
  (format stream "~&~%===== Recorder subgoal chain =====~%")
  (loop for segment in (recorder-subgoal-chain.segments chain)
        for number from 1
        do (format stream "~&~%Checkpoint ~D~%" number)
           (format stream "Goal: ~S~%" (recorder-subgoal-segment.goal segment))
           (format stream "Search policy: ~A~%"
                   (recorder-subgoal-segment.solution-type segment))
           (format stream "Segment depth: ~D; cumulative depth: ~D~%"
                   (solution.depth (recorder-subgoal-segment.solution segment))
                   (recorder-subgoal-segment.cumulative-depth segment))
           (format stream "Cumulative elapsed time: ~A; value change: ~A~%"
                   (recorder-subgoal-segment.cumulative-time segment)
                   (recorder-subgoal-segment.cumulative-value segment))
           (format stream "Recorder: cycle ~D, ~:[closed~;open~]~%"
                   (recorder-subgoal-segment.cycle-count segment)
                   (recorder-subgoal-segment.recording-open-p segment)))
  chain)


(defun install-recorder-subgoal-baseline (state)
  "Commit an independent copy of checkpoint STATE as the next search baseline."
  (let ((baseline (copy-problem-state state)))
    (setf (problem-state.name baseline) 'recorder-subgoal-checkpoint
          (problem-state.instantiations baseline) nil
          *start-state* baseline
          *solution-paths* nil
          *solutions-valid* nil)
    baseline))


(defun validate-recorder-subgoal-orchestration ()
  "Validate the planning session before a recorder checkpoint operation."
  (validate-continuation-preconditions)
  (unless (member 'validate-recorder-solution *solution-validators*)
    (error "Recorder subgoaling requires the services installed by (include-tech recorder)."))
  (when *solutions-valid*
    (error "A completed solution is still active. Undo or stage a fresh recorder chain ~
            before starting another subgoal."))
  (when (and (not (recorder-state-recording-open-p *start-state*))
             (not (funcall (symbol-function 'recorder-closed-ghost-free) *start-state*)))
    (error "Recorder subgoaling cannot continue from stale ghost state."))
  t)


(defun run-recorder-subgoal-planner ()
  "Run WW-SOLVE without printing a misleading segment-local recorder report."
  (let ((saved-printers *solution-report-printers*))
    (unwind-protect
        (progn
          (setf *solution-report-printers*
                (remove 'print-recorder-report *solution-report-printers*))
          (ww-solve))
      (setf *solution-report-printers* saved-printers))))


(defun commit-recorder-subgoal (goal-form final-p)
  "Commit the selected segment for GOAL-FORM and return its cumulative solution."
  (let ((selected (copy-solution-deeply (select-continuation-solution))))
    (multiple-value-bind (segment cumulative-solution)
        (make-recorder-subgoal-segment-record goal-form final-p selected)
      (append-recorder-subgoal-segment segment)
      (print-recorder-subgoal-status segment *standard-output*)
      (if final-p
        (progn
          (install-compiled-goal goal-form)
          ;; The retained solution is the complete path, so expose the original staged
          ;; state as its baseline just as an ordinary, unchained solve would.
          (setf *start-state*
                  (copy-problem-state (recorder-subgoal-chain-origin))
                *solution-paths* (list cumulative-solution)
                *solutions-valid* t
                *final-goal* nil)
          (print-recorder-subgoal-chain-report)
          (print-recorder-report cumulative-solution))
        (install-recorder-subgoal-baseline (solution.goal selected)))
      cumulative-solution)))


(defun run-recorder-subgoal-search (goal-form final-p)
  "Search one user GOAL-FORM, append it to the current chain, and retain its checkpoint."
  (let ((completed nil))
    (unwind-protect
        (progn
          (install-compiled-goal goal-form)
          (run-recorder-subgoal-planner)
          (let ((solution
                  (when *solutions-valid*
                    (commit-recorder-subgoal goal-form final-p))))
            (setf completed t)
            (unless solution
              (format t "~&Recorder subgoal produced no solution. Retry from the current ~
                         checkpoint, or use (ww-undo) to restore the preceding session.~%"))
            solution))
      (unless completed
        (setf *solution-paths* nil
              *solutions-valid* nil)
        (format t "~&Recorder subgoal interrupted. Use (ww-undo) to restore the preceding ~
                   session.~%")))))


(defun solve-recorder-subgoal-form (goal-form)
  "Solve GOAL-FORM as the next certified checkpoint, without forcing a cycle boundary."
  (validate-recorder-subgoal-orchestration)
  (save-undo-checkpoint)
  (unless *recorder-subgoal-chain*
    (start-recorder-subgoal-chain *goal*))
  (unless *final-goal*
    (setf *final-goal*
          (copy-tree
            (recorder-subgoal-chain.original-goal *recorder-subgoal-chain*))))
  (run-recorder-subgoal-search goal-form nil))


(defun solve-recorder-final ()
  "Finish the active checkpoint chain with the original unstrengthened problem goal."
  (validate-recorder-subgoal-orchestration)
  (unless *recorder-subgoal-chain*
    (error "No recorder subgoal chain is active."))
  (save-undo-checkpoint)
  (run-recorder-subgoal-search
    (copy-tree (recorder-subgoal-chain.original-goal *recorder-subgoal-chain*))
    t))
