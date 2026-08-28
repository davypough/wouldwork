;;; Filename: problem-recorder-goal-chain-backtracking-test.lisp

;;; Recorder-specific projection test for generic backtracking across a real cycle boundary.
;;; The first closed-cycle checkpoint can retain a bad or good hidden route.  Only the good
;;; exact endpoint permits the second requested phase, so the generic controller must replace
;;; the first recorder segment and replay the resulting two-cycle cumulative path exactly.

(in-package :ww)


(ww-set *problem-name* recorder-goal-chain-backtracking-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)
(ww-set *max-recorder-cycles* 2)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  location (recorder-site)
  boundary-stage (boundary-start boundary-a boundary-b)
  boundary-route (boundary-bad boundary-good))


(include-tech recorder)


(define-dynamic-relations
  (boundary-at boundary-stage)
  (boundary-route-selected boundary-route))


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-position recorder1 recorder-site)
  (boundary-at boundary-start))


;; This recorder search visits the first declared route first.
(define-action choose-bad-boundary-route
  1 (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (boundary-at boundary-start)
       (recording-agent-at-recorder ?agent))
  (">" ?agent "selects the bad boundary route")
  (assert (not (boundary-at boundary-start))
          (boundary-at boundary-a)
          (boundary-route-selected boundary-bad)))


(define-action choose-good-boundary-route
  1 (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (boundary-at boundary-start)
       (recording-agent-at-recorder ?agent))
  (">" ?agent "selects the good boundary route")
  (assert (not (boundary-at boundary-start))
          (boundary-at boundary-a)
          (boundary-route-selected boundary-good)))


(define-action advance-good-boundary-route
  1 (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (boundary-at boundary-a)
       (boundary-route-selected boundary-good)
       (recording-agent-at-recorder ?agent))
  (">" ?agent "advances the good boundary route")
  (assert (not (boundary-at boundary-a))
          (boundary-at boundary-b)))


(define-test-helper recorder-boundary-state-has-p (state proposition)
  (member proposition (list-database (problem-state.idb state)) :test #'equal))


(define-test-helper call-with-isolated-recorder-backtracking-chain (thunk)
  (let* ((saved-goal-function (symbol-function 'goal-fn))
         (saved-goal-value (symbol-value 'goal-fn))
         (saved-goal-form (get 'goal-fn :form))
         (saved-solutions *solution-paths*)
         (saved-valid *solutions-valid*)
         (saved-outcome *last-search-outcome*))
    (unwind-protect
        (let ((*start-state* (copy-problem-state *start-state*))
              (*goal* (copy-tree *goal*))
              (*final-goal* nil)
              (*goal-chain-session* nil)
              (*recorder-subgoal-chain* nil)
              (*undo-stack* nil)
              (*threads* 0)
              (*solution-type* 'first)
              (*tree-or-graph* 'graph)
              (*depth-cutoff* 3)
              (*max-recorder-cycles* 2))
          (setf *solution-paths* nil
                *solutions-valid* nil
                *last-search-outcome* nil)
          (install-compiled-goal
            '(and (boundary-at boundary-b)
                  (ghost-stops-recorder)))
          (funcall thunk))
      (setf (symbol-function 'goal-fn) saved-goal-function
            (symbol-value 'goal-fn) saved-goal-value
            (get 'goal-fn :form) saved-goal-form
            *solution-paths* saved-solutions
            *solutions-valid* saved-valid
            *last-search-outcome* saved-outcome))))


(define-test-claim recorder-backtracks-across-cycle-boundary
  (call-with-isolated-recorder-backtracking-chain
    (lambda ()
      (let ((origin (copy-problem-state *start-state*)))
        (solve-subgoal
          (and (boundary-at boundary-a) (recorder-cycle-ended)))
        (let ((first-was-bad
                (recorder-boundary-state-has-p
                  *start-state* '(boundary-route-selected boundary-bad))))
          (solve-subgoal (boundary-at boundary-b))
          (let ((recovered-good
                  (and (= (length (goal-chain-session-phases *goal-chain-session*)) 2)
                       (= (length (recorder-subgoal-chain.segments
                                    *recorder-subgoal-chain*)) 2)
                       (recorder-boundary-state-has-p
                         (solution.goal
                           (goal-chain-phase-solution
                             (first (goal-chain-session-phases
                                      *goal-chain-session*))))
                         '(boundary-route-selected boundary-good))
                       (recorder-state-recording-open-p *start-state*))))
            (solve)
            (let* ((solution (first *solution-paths*))
                   (action-names
                     (mapcar (lambda (move) (first (second move)))
                             (solution.path solution))))
              (and first-was-bad recovered-good
                   *solutions-valid*
                   (= (solution.depth solution) 6)
                   (= (count 'choose-good-boundary-route action-names) 1)
                   (not (member 'choose-bad-boundary-route action-names))
                   (= (length (recorder-subgoal-chain.segments
                                *recorder-subgoal-chain*)) 3)
                   (= (recorder-state-cycle-count (solution.goal solution)) 2)
                   (funcall (symbol-function 'ghost-stops-recorder)
                            (solution.goal solution))
                   (multiple-value-bind (valid-p diagnostic)
                       (validate-recorder-solution
                         origin (solution.path solution) (solution.goal solution))
                     (declare (ignore diagnostic))
                     valid-p)))))))))


(define-goal
  (boundary-at boundary-start))
