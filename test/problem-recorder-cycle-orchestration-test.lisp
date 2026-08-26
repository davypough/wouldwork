;;; Filename: problem-recorder-cycle-orchestration-test.lisp

;;; Focused characterization of sequential recorder subgoals embedded in one cycle.  The
;;; first two user checkpoints are reached while recording remains open; the final SOLVE
;;; appends the required STOP and returns one cumulative, fully replayed solution.

(in-package :ww)


(ww-set *problem-name* recorder-cycle-orchestration-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  toggle-plate (cycle-plate)
  location (recorder-site)
  cycle-stage (cycle-start cycle-middle cycle-end cycle-unreachable))


(include-tech recorder)
(include-tech plate)


(define-dynamic-relations
  (cycle-at cycle-stage))


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-position recorder1 recorder-site)
  (has-position cycle-plate recorder-site)
  (cycle-at cycle-start))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action advance-cycle-to-middle
  2
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (cycle-at cycle-start)
       (recording-agent-at-recorder ?agent))
  (">" ?agent "reaches the first embedded checkpoint")
  (assert (not (cycle-at cycle-start))
          (cycle-at cycle-middle)
          (assign $objective-value 7.0)))


(define-action advance-cycle-to-end
  3
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (cycle-at cycle-middle)
       (recording-agent-at-recorder ?agent))
  (">" ?agent "reaches the second embedded checkpoint")
  (assert (not (cycle-at cycle-middle))
          (cycle-at cycle-end)
          (assign $objective-value 11.0)))


(define-test-helper recorder-orchestration-state-at-p (state stage)
  (member `(cycle-at ,stage)
          (list-database (problem-state.idb state))
          :test #'equal))


(define-test-helper recorder-orchestration-chain-length ()
  (if *recorder-subgoal-chain*
    (length (recorder-subgoal-chain.segments *recorder-subgoal-chain*))
    0))


(define-test-helper recorder-orchestration-segment (index)
  (nth index (recorder-subgoal-chain.segments *recorder-subgoal-chain*)))


(define-test-helper recorder-orchestration-path ()
  '((1.0 (start-recorder live-agent))
    (3.0 (advance-cycle-to-middle ghost-agent))
    (6.0 (advance-cycle-to-end ghost-agent))
    (7.0 (stop-recorder ghost-agent))))


(define-test-helper recorder-orchestration-cumulative-solution-p ()
  (let* ((solution (first *solution-paths*))
         (origin
           (and *recorder-subgoal-chain*
                (recorder-subgoal-chain.origin-state *recorder-subgoal-chain*)))
         (report
           (and solution origin
                (let ((*start-state* origin))
                  (build-recorder-report solution)))))
    (and solution
         (= (solution.depth solution) 4)
         (= (solution.time solution) 7.0)
         (= (solution.value solution) 11.0)
         (equal (solution.path solution) (recorder-orchestration-path))
         (recorder-orchestration-state-at-p (solution.goal solution) 'cycle-end)
         (funcall (symbol-function 'ghost-stops-recorder) (solution.goal solution))
         (= (getf report :cycle-count) 1)
         (equal (getf (first (getf report :cycles)) :closure) :explicit)
         (equal (getf report :totals)
                '(:depth 4 :elapsed-time 7.0 :value-change 11.0)))))


(define-test-helper recorder-orchestration-chain-report-p ()
  (let ((printed
          (with-output-to-string (stream)
            (print-recorder-subgoal-chain-report
              *recorder-subgoal-chain* stream))))
    (and (search "Checkpoint 1" printed)
         (search "Checkpoint 2" printed)
         (search "Checkpoint 3" printed)
         (search "CYCLE-MIDDLE" printed)
         (search "CYCLE-END" printed)
         (search "cumulative depth: 4" printed :test #'char-equal))))


(define-test-claim recorder-initial-solve-remains-ordinary
  (let ((ordinary-solve-p nil))
    (unwind-protect
        (progn
          (solve)
          (setf ordinary-solve-p
                (and *solutions-valid*
                     (= (solution.depth (select-continuation-solution)) 4)
                     (null *final-goal*)
                     (null *recorder-subgoal-chain*)
                     (null *undo-stack*))))
      (setf *solution-paths* nil
            *solutions-valid* nil))
    ordinary-solve-p))


(define-test-claim recorder-subgoals-remain-inside-open-cycle
  (let ((initial-database (list-database (problem-state.idb *start-state*))))
    (solve-subgoal (cycle-at cycle-middle))
    (let ((first-segment (recorder-orchestration-segment 0)))
      (and
        (= (recorder-orchestration-chain-length) 1)
        (equal (recorder-subgoal-segment.goal first-segment)
               '(cycle-at cycle-middle))
        (= (solution.depth (recorder-subgoal-segment.solution first-segment)) 2)
        (= (recorder-subgoal-segment.cumulative-depth first-segment) 2)
        (recorder-subgoal-segment.recording-open-p first-segment)
        (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
        (recorder-state-recording-open-p *start-state*)
        (null *solution-paths*)
        (not *solutions-valid*)
        (equal *final-goal*
               '(and (cycle-at cycle-end) (ghost-stops-recorder)))

        (progn (solve-subgoal (cycle-at cycle-end)) t)
        (= (recorder-orchestration-chain-length) 2)
        (let ((second-segment (recorder-orchestration-segment 1)))
          (and (equal (recorder-subgoal-segment.goal second-segment)
                      '(cycle-at cycle-end))
               (= (solution.depth
                    (recorder-subgoal-segment.solution second-segment)) 1)
               (= (recorder-subgoal-segment.cumulative-depth second-segment) 3)
               (recorder-subgoal-segment.recording-open-p second-segment)))
        (recorder-orchestration-state-at-p *start-state* 'cycle-end)
        (recorder-state-recording-open-p *start-state*)

        (progn (solve) t)
        (= (recorder-orchestration-chain-length) 3)
        *solutions-valid*
        (null *final-goal*)
        (equal *goal* '(and (cycle-at cycle-end) (ghost-stops-recorder)))
        (recorder-orchestration-cumulative-solution-p)
        (recorder-orchestration-chain-report-p)

        ;; Undo removes one pasted segment at a time, restoring both the exact open state
        ;; and the accumulated checkpoint history.
        (ww-undo)
        (= (recorder-orchestration-chain-length) 2)
        (recorder-orchestration-state-at-p *start-state* 'cycle-end)
        (recorder-state-recording-open-p *start-state*)
        (not *solutions-valid*)
        (equal *final-goal*
               '(and (cycle-at cycle-end) (ghost-stops-recorder)))
        (ww-undo)
        (= (recorder-orchestration-chain-length) 1)
        (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
        (recorder-state-recording-open-p *start-state*)
        (ww-undo)
        (null *recorder-subgoal-chain*)
        (null *final-goal*)
        (null *undo-stack*)
        (equal initial-database
               (list-database (problem-state.idb *start-state*)))))))


(define-test-claim recorder-subgoal-failure-retains-checkpoint
  (let ((initial-database (list-database (problem-state.idb *start-state*))))
    (solve-subgoal (cycle-at cycle-middle))
    (let ((middle-database (list-database (problem-state.idb *start-state*))))
      (solve-subgoal (cycle-at cycle-unreachable))
      (and
        (= (recorder-orchestration-chain-length) 1)
        (equal middle-database (list-database (problem-state.idb *start-state*)))
        (not *solutions-valid*)
        (ww-undo)
        (= (recorder-orchestration-chain-length) 1)
        (equal middle-database (list-database (problem-state.idb *start-state*)))
        (ww-undo)
        (null *recorder-subgoal-chain*)
        (null *final-goal*)
        (null *undo-stack*)
        (equal initial-database
               (list-database (problem-state.idb *start-state*)))))))


(define-test-claim recorder-public-goal-chaining-dispatch
  (and *goal-chaining-policy*
       (eql (goal-chaining-policy-subgoal-solver *goal-chaining-policy*)
            'solve-recorder-subgoal-form)
       (eql (goal-chaining-policy-final-solver *goal-chaining-policy*)
            'solve-recorder-final)))


(define-goal
  (and (cycle-at cycle-end)
       (ghost-stops-recorder)))
