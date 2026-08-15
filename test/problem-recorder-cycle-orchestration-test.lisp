;;; Filename: problem-recorder-cycle-orchestration-test.lisp

;;; Focused two-call characterization of recorder-cycle orchestration.  Each cycle has one
;;; ghost action and ends at the recorder.  The state carried from START through MIDDLE to
;;; END is intentionally simple; Stage 5 supplies the nontrivial persistent-apparatus
;;; example.  Here the contract under test is solve, validate, commit, discard, fail,
;;; rollback, retry final, and undo with history kept in step throughout.
;;; Expected minimum path length for the staged problem itself: START plus two ghost
;;; advances in one final open cycle, three actions total.

(in-package :ww)


(ww-set *problem-name* recorder-cycle-orchestration-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)
(ww-set *max-recorder-cycles* 2)

(setf *expected-min-length* 3)


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
  (">" ?agent "commits the first recorder cycle")
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
  (">" ?agent "commits the final recorder cycle")
  (assert (not (cycle-at cycle-middle))
          (cycle-at cycle-end)
          (assign $objective-value 11.0)))


(define-test-helper recorder-orchestration-state-at-p (state stage)
  (member `(cycle-at ,stage)
          (list-database (problem-state.idb state))
          :test #'equal))


(define-test-helper recorder-history-boundary-at-p (index stage)
  (let ((record (nth index *recorder-cycle-history*)))
    (and record
         (recorder-orchestration-state-at-p
           (recorder-cycle-record.boundary-state record)
           stage))))


(define-test-helper recorder-cycle-metrics-p
    (record depth elapsed-time value-change cumulative-depth cumulative-time cumulative-value)
  (and (= (recorder-cycle-record.depth record) depth)
       (= (recorder-cycle-record.elapsed-time record) elapsed-time)
       (= (recorder-cycle-record.value-change record) value-change)
       (= (recorder-cycle-record.cumulative-depth record) cumulative-depth)
       (= (recorder-cycle-record.cumulative-time record) cumulative-time)
       (= (recorder-cycle-record.cumulative-value record) cumulative-value)))


(define-test-helper recorder-chain-report-contract-p ()
  (let* ((printed
           (with-output-to-string (stream)
             (print-recorder-chain-report *recorder-cycle-history* stream)))
         (cycle-1 (search "Cycle 1" printed))
         (cycle-2 (search "Cycle 2" printed))
         (integrated-1 (search "Integrated sequence:" printed :start2 cycle-1))
         (setup-1 (search "Setup phase:" printed :start2 cycle-1))
         (recording-1 (search "Recording phase:" printed :start2 cycle-1))
         (playback-1 (search "Playback phase:" printed :start2 cycle-1))
         (integrated-2 (search "Integrated sequence:" printed :start2 cycle-2))
         (setup-2 (search "Setup phase:" printed :start2 cycle-2))
         (recording-2 (search "Recording phase:" printed :start2 cycle-2))
         (playback-2 (search "Playback phase:" printed :start2 cycle-2)))
    (and cycle-1 cycle-2
         integrated-1 setup-1 recording-1 playback-1
         integrated-2 setup-2 recording-2 playback-2
         (< cycle-1 integrated-1 setup-1 recording-1 playback-1 cycle-2
            integrated-2 setup-2 recording-2 playback-2)
         (search "Chain totals: depth 6; elapsed time 9.0; value change 11.0" printed)
         (search "Any optimization is local to its cycle; this chain is not globally optimized."
                 printed))))


(define-test-claim recorder-initial-solve-remains-ordinary
  (let ((ordinary-solve-p nil))
    (unwind-protect
        (progn
          (solve)
          (setf ordinary-solve-p
                (and *solutions-valid*
                     (= (solution.depth (select-continuation-solution)) 3)
                     (null *final-goal*)
                     (null *recorder-cycle-history*)
                     (null *undo-stack*))))
      ;; An initial SOLVE does not create an undo checkpoint, so discard only its
      ;; ordinary search result before the remaining claims run.
      (setf *solution-paths* nil
            *solutions-valid* nil))
    ordinary-solve-p))


(define-test-claim recorder-cycle-failure-restores-original-session
  (let ((initial-database (list-database (problem-state.idb *start-state*))))
    (solve-subgoal (cycle-at cycle-unreachable))
    (and (null *recorder-cycle-history*)
         (not *solutions-valid*)
         (null *solution-paths*)
         (equal *final-goal* '(cycle-at cycle-end))
         (equal *goal*
                '(and (cycle-at cycle-unreachable)
                      (recorder-cycles-used 1)
                      (ghost-stops-recorder)))
         (= (length *undo-stack*) 1)
         (ww-undo)
         (null *recorder-cycle-history*)
         (null *final-goal*)
         (equal *goal* '(cycle-at cycle-end))
         (equal initial-database
                (list-database (problem-state.idb *start-state*)))
         (null *undo-stack*))))


(define-test-claim recorder-guided-maximum-fails-before-checkpoint
  (let ((exhausted-state (copy-problem-state *start-state*))
        (original-start-state *start-state*)
        (original-maximum *max-recorder-cycles*)
        (original-solutions *solution-paths*)
        (original-solutions-valid *solutions-valid*)
        (original-undo-stack *undo-stack*)
        (result nil))
    (add-proposition '(recorder-cycles-used 1)
                     (problem-state.idb exhausted-state))
    (add-proposition '(recorder-cycle-closed)
                     (problem-state.idb exhausted-state))
    (invalidate-problem-state-hash exhausted-state)
    (unwind-protect
        (progn
          (setf *start-state* exhausted-state
                *max-recorder-cycles* 1
                *solution-paths* nil
                *solutions-valid* nil
                *undo-stack* nil)
          (setf result
                (and
                  (expect-condition
                    (lambda ()
                      (solve-recorder-subgoal-form
                        '(cycle-at cycle-unreachable)))
                    'error
                    :containing "exceeds *MAX-RECORDER-CYCLES*")
                  (null *undo-stack*))))
      (setf *start-state* original-start-state
            *max-recorder-cycles* original-maximum
            *solution-paths* original-solutions
            *solutions-valid* original-solutions-valid
            *undo-stack* original-undo-stack))
    result))


(define-test-claim recorder-cycle-commit-final-and-undo
  (let ((initial-database (list-database (problem-state.idb *start-state*))))
    (solve-subgoal (cycle-at cycle-middle))
    (and
      (= (length *recorder-cycle-history*) 1)
      (recorder-history-boundary-at-p 0 'cycle-middle)
      (= (solution.depth
           (recorder-cycle-record.solution (first *recorder-cycle-history*)))
         3)
      (recorder-cycle-metrics-p
        (first *recorder-cycle-history*) 3 4 7 3 4 7)
      (equal (recorder-cycle-record.subgoal (first *recorder-cycle-history*))
             '(cycle-at cycle-middle))
      (equal (recorder-cycle-record.closed-goal
               (first *recorder-cycle-history*))
             '(and (cycle-at cycle-middle)
                   (recorder-cycles-used 1)
                   (ghost-stops-recorder)))
      (= (funcall (symbol-function 'recorder-cycle-count) *start-state*) 1)
      (equal (getf (recorder-cycle-record.report (first *recorder-cycle-history*))
                   :recording)
             '((1.0 (start-recorder live-agent))
               (3.0 (advance-cycle-to-middle ghost-agent))
               (4.0 (stop-recorder ghost-agent))))
      (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
      (null *solution-paths*)
      (not *solutions-valid*)
      (equal *final-goal* '(cycle-at cycle-end))

      ;; A failed later cycle leaves the committed first cycle available.  Its checkpoint
      ;; restores the history as well as the generic planning session.
      (progn
        (solve-subgoal (cycle-at cycle-unreachable))
        t)
      (= (length *recorder-cycle-history*) 1)
      (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
      (ww-undo)
      (= (length *recorder-cycle-history*) 1)
      (recorder-history-boundary-at-p 0 'cycle-middle)
      (recorder-cycle-metrics-p
        (first *recorder-cycle-history*) 3 4 7 3 4 7)
      (recorder-orchestration-state-at-p *start-state* 'cycle-middle)

      (progn (solve) t)
      (= (length *recorder-cycle-history*) 2)
      (recorder-history-boundary-at-p 1 'cycle-end)
      (recorder-cycle-metrics-p
        (second *recorder-cycle-history*) 3 5 4 6 9 11)
      (equal (recorder-cycle-record.closed-goal
               (second *recorder-cycle-history*))
             '(and (cycle-at cycle-end)
                   (recorder-cycles-used 2)
                   (ghost-stops-recorder)))
      (= (funcall (symbol-function 'recorder-cycle-count)
                  (recorder-cycle-record.boundary-state
                    (second *recorder-cycle-history*)))
         2)
      (equal (getf (recorder-cycle-record.report (first *recorder-cycle-history*))
                   :integrated)
             '((1.0 (start-recorder live-agent))
               (3.0 (advance-cycle-to-middle ghost-agent))
               (4.0 (stop-recorder ghost-agent))))
      (equal (getf (recorder-cycle-record.report (first *recorder-cycle-history*))
                   :playback)
             '((3.0 (advance-cycle-to-middle ghost-agent))))
      (equal (getf (recorder-cycle-record.report (second *recorder-cycle-history*))
                   :integrated)
             '((5.0 (start-recorder live-agent))
               (8.0 (advance-cycle-to-end ghost-agent))
               (9.0 (stop-recorder ghost-agent))))
      (equal (getf (recorder-cycle-record.report (second *recorder-cycle-history*))
                   :recording)
             '((5.0 (start-recorder live-agent))
               (8.0 (advance-cycle-to-end ghost-agent))
               (9.0 (stop-recorder ghost-agent))))
      (equal (getf (recorder-cycle-record.report (second *recorder-cycle-history*))
                   :playback)
             '((8.0 (advance-cycle-to-end ghost-agent))))
      (recorder-chain-report-contract-p)
      *solutions-valid*
      (= (solution.depth (select-continuation-solution)) 3)
      (null *final-goal*)
      (equal *goal* '(cycle-at cycle-end))

      ;; Undo the final cycle, then the first committed cycle.
      (ww-undo)
      (= (length *recorder-cycle-history*) 1)
      (recorder-history-boundary-at-p 0 'cycle-middle)
      (recorder-cycle-metrics-p
        (first *recorder-cycle-history*) 3 4 7 3 4 7)
      (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
      (not *solutions-valid*)
      (equal *final-goal* '(cycle-at cycle-end))
      (ww-undo)
      (null *recorder-cycle-history*)
      (null *final-goal*)
      (equal *goal* '(cycle-at cycle-end))
      (equal initial-database
             (list-database (problem-state.idb *start-state*)))
      (null *undo-stack*))))


(define-test-claim recorder-public-goal-chaining-dispatch
  (let ((initial-database (list-database (problem-state.idb *start-state*)))
        (subgoal-dispatched-p nil)
        (final-dispatched-p nil))
    (unwind-protect
        (progn
          ;; These are the public goal-chaining commands.  Recorder-enabled problems
          ;; should give them recorder-cycle semantics without changing the interface.
          (solve-subgoal (cycle-at cycle-middle))
          (setf subgoal-dispatched-p
                (and (= (length *recorder-cycle-history*) 1)
                     (recorder-history-boundary-at-p 0 'cycle-middle)
                     (recorder-orchestration-state-at-p *start-state* 'cycle-middle)
                     (null *solution-paths*)
                     (not *solutions-valid*)
                     (equal *final-goal* '(cycle-at cycle-end))))

          (solve)
          (setf final-dispatched-p
                (and (= (length *recorder-cycle-history*) 2)
                     (recorder-history-boundary-at-p 1 'cycle-end)
                     *solutions-valid*
                     (= (solution.depth (select-continuation-solution)) 3)
                     (null *final-goal*)
                     (equal *goal* '(cycle-at cycle-end)))))
      ;; Each public chaining call creates one checkpoint.  Restore the staged problem
      ;; after characterizing automatic recorder dispatch.
      (loop while *undo-stack* do (ww-undo)))
    (and subgoal-dispatched-p
         final-dispatched-p
         (null *recorder-cycle-history*)
         (null *final-goal*)
         (equal *goal* '(cycle-at cycle-end))
         (equal initial-database
                (list-database (problem-state.idb *start-state*)))
         (null *undo-stack*))))


(define-goal
  (cycle-at cycle-end))
