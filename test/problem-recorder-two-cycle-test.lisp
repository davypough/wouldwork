;;; Filename: problem-recorder-two-cycle-test.lisp

;;; Executable planner-native and guided two-cycle recorder characterization.  Cycle 1
;;; requires genuine live/ghost cooperation: the ghost primes a transient opportunity and
;;; the live agent consumes it while pressing a toggle plate.  The priming fact is gone at
;;; the boundary, so the persistent advance is the latch produced by both sides rather than
;;; an ordinary setup action.  STOP preserves that latch, removes ghosts, seeds
;;; RECORDING-LATCHED from the committed live baseline, and derives RECORDING-OPEN.  Cycle 2
;;; then starts from the normalized boundary and its freshly forked ghost uses the recording
;;; gate.  A single integrated window cannot validate the same actions: isolated recording
;;; replay omits the live press and therefore sees the original closed recording gate.
;;; With a maximum of one cycle there is no solution through depth seven; with two, one
;;; ordinary SOLVE finds the unique seven-action path.  Recorder-aware subgoaling instead
;;; checkpoints the open first cycle after three actions, then transparently closes it and
;;; enters the second cycle while pursuing the final goal.  The
;;; ordinary Talos harness goal is deliberately zero-action: the characterization claim
;;; installs the non-flattenable final goal while it exercises the chained solver, then
;;; restores the harness goal before TEST-TALOS performs its required ordinary search.

(in-package :ww)


(ww-set *problem-name* recorder-two-cycle-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)
(ww-set *max-recorder-cycles* 2)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  toggle-plate (cycle-plate)
  gate (cycle-gate)
  location (recorder-site)
  cycle-status (cycle-pending cycle-complete cycle-unreachable))


(include-tech recorder)
(include-tech plate)
(include-tech gate)


(define-dynamic-relations
  (current-cycle-status cycle-status)
  (playback-latch-primed))


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-position recorder1 recorder-site)
  (has-position cycle-plate recorder-site)
  (controls ((cycle-plate)) cycle-gate normal)
  (current-cycle-status cycle-pending))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action prime-playback-latch
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (current-cycle-status cycle-pending)
       (not (playback-latch-primed)))
  (">" ?agent "primes the live playback latch")
  (assert (playback-latch-primed)))


(define-action latch-playback-gate
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress)
       (playback-latch-primed)
       (has-location ?agent recorder-site)
       (not (depressed cycle-plate))
       (not (latched cycle-plate)))
  (">" ?agent "presses the persistent playback latch")
  (assert (not (playback-latch-primed))
          (on ?agent cycle-plate)
          (finally (propagate-changes!))))


(define-action use-recording-gate
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (has-location ?agent recorder-site)
       (current-cycle-status cycle-pending)
       (latched cycle-plate)
       (recording-latched cycle-plate)
       (open cycle-gate)
       (recording-open cycle-gate))
  (">" ?agent "uses the gate opened by the preceding recorder cycle")
  (assert (not (current-cycle-status cycle-pending))
          (current-cycle-status cycle-complete)))


(define-test-helper recorder-two-cycle-fact-p (state proposition)
  (member proposition
          (list-database (problem-state.idb state))
          :test #'equal))


(define-test-helper recorder-two-cycle-open-checkpoint-p ()
  ;; ON is bijective, so the database stores its ON1/ON2 index pair rather than a plain
  ;; (ON ...) tuple; check ON1 (keyed by the occupant) directly.
  (and (recorder-two-cycle-fact-p *start-state* '(on1 live-agent cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(depressed cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(latched cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(open cycle-gate))
       (recorder-two-cycle-fact-p *start-state* '(recording-in-progress))
       (not (recorder-two-cycle-fact-p
              *start-state* '(recording-latched cycle-plate)))
       (not (recorder-two-cycle-fact-p
              *start-state* '(recording-open cycle-gate)))
       (recorder-two-cycle-fact-p
         *start-state* '(current-cycle-status cycle-pending))))


(define-test-helper recorder-two-cycle-checkpoint-chain-length ()
  (if *recorder-subgoal-chain*
    (length (recorder-subgoal-chain.segments *recorder-subgoal-chain*))
    0))


(define-test-helper recorder-two-cycle-native-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (prime-playback-latch ghost-agent))
    (3.0 (latch-playback-gate live-agent))
    (4.0 (stop-recorder ghost-agent))
    (5.0 (start-recorder live-agent))
    (6.0 (use-recording-gate ghost-agent))
    (7.0 (stop-recorder ghost-agent))))


(define-test-helper recorder-two-cycle-native-path-p (path)
  (and
    (= (length path) 7)
    (equal (subseq path 0 3)
           '((1.0 (start-recorder live-agent))
             (2.0 (prime-playback-latch ghost-agent))
             (3.0 (latch-playback-gate live-agent))))
    (member (fourth path)
            '((4.0 (stop-recorder ghost-agent))
              (4.0 (cancel-playback live-agent)))
            :test #'equal)
    (equal (subseq path 4)
           '((5.0 (start-recorder live-agent))
             (6.0 (use-recording-gate ghost-agent))
             (7.0 (stop-recorder ghost-agent))))))


(define-test-helper recorder-two-cycle-native-solution-p (solution)
  (when solution
    (let* ((path (solution.path solution))
           (report (build-recorder-report solution))
           (cycles (getf report :cycles)))
      (and
        (= (solution.depth solution) 7)
        (recorder-two-cycle-native-path-p path)
        (validate-recorder-solution *start-state* path (solution.goal solution))
        (= (getf report :cycle-count) 2)
        (equal (mapcar (lambda (cycle) (getf cycle :closure)) cycles)
               (if (recorder-cycle-cancellation-p (fourth path))
                 '(:cancelled :explicit)
                 '(:explicit :explicit)))
        (equal (mapcar (lambda (cycle) (getf cycle :depth)) cycles)
               '(4 3))
        (equal (mapcar (lambda (cycle) (getf cycle :elapsed-time)) cycles)
               '(4.0 3.0))
        (every (lambda (cycle) (null (getf cycle :setup))) cycles)
        (equal (getf report :totals)
               '(:depth 7 :elapsed-time 7.0 :value-change 0.0))))))


(define-test-claim recorder-two-cycle-single-window-replay-fails
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state*
        '((1.0 (start-recorder live-agent))
          (2.0 (prime-playback-latch ghost-agent))
          (3.0 (latch-playback-gate live-agent))
          (4.0 (use-recording-gate ghost-agent)))
        *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :recording)
         (eql (getf diagnostic :reason) :action-failed)
         (equal (getf diagnostic :action)
                '(use-recording-gate ghost-agent)))))


(define-test-claim recorder-two-cycle-planner-native-search
  (let ((harness-goal (copy-tree *goal*))
        (original-maximum *max-recorder-cycles*)
        (original-cutoff *depth-cutoff*)
        (maximum-one-rejected-p nil)
        (result nil))
    (unwind-protect
      (progn
        (install-compiled-goal
          '(and (current-cycle-status cycle-complete)
                (recorder-cycle-closed)))
        (setf *max-recorder-cycles* 1
              *depth-cutoff* 7)
        (solve)
        (setf maximum-one-rejected-p
              (and (null *solution-paths*)
                   (not *solutions-valid*)))
        (setf *max-recorder-cycles* 2)
        (solve)
        (setf result
              (and maximum-one-rejected-p
                   (null *recorder-subgoal-chain*)
                   *solutions-valid*
                   (recorder-two-cycle-native-solution-p
                     (first *solution-paths*)))))
      (setf *max-recorder-cycles* original-maximum
            *depth-cutoff* original-cutoff
            *solution-paths* nil
            *unique-solution-states* nil
            *solutions-valid* nil)
      (install-compiled-goal harness-goal))
    result))


(define-test-helper recorder-two-cycle-checkpoint-solution-p ()
  (let* ((solution (first *solution-paths*))
         (origin
           (and *recorder-subgoal-chain*
                (recorder-subgoal-chain.origin-state *recorder-subgoal-chain*)))
         (path (and solution (solution.path solution)))
         (report
           (and solution origin
                (let ((*start-state* origin))
                  (build-recorder-report solution))))
         (cycles (and report (getf report :cycles))))
    (and solution
         (= (solution.depth solution) 6)
         (equal (subseq path 0 3)
                '((1.0 (start-recorder live-agent))
                  (2.0 (prime-playback-latch ghost-agent))
                  (3.0 (latch-playback-gate live-agent))))
         (member (fourth path)
                 '((4.0 (stop-recorder ghost-agent))
                   (4.0 (cancel-playback live-agent)))
                 :test #'equal)
         (equal (subseq path 4)
                '((5.0 (start-recorder live-agent))
                  (6.0 (use-recording-gate ghost-agent))))
         (= (getf report :cycle-count) 2)
         (equal (mapcar (lambda (cycle) (getf cycle :closure)) cycles)
                (if (recorder-cycle-cancellation-p (fourth path))
                  '(:cancelled :synthesized)
                  '(:explicit :synthesized)))
         (equal (mapcar (lambda (cycle) (getf cycle :depth)) cycles)
                '(4 2))
         (recorder-two-cycle-fact-p
           (solution.goal solution) '(current-cycle-status cycle-complete))
         (recorder-two-cycle-fact-p
           (solution.goal solution) '(recording-in-progress)))))


(define-test-claim recorder-two-cycle-checkpoint-recover-and-undo
  (let ((initial-database (list-database (problem-state.idb *start-state*)))
        (harness-goal (copy-tree *goal*))
        (result nil))
    (unwind-protect
        (progn
          (install-compiled-goal '(current-cycle-status cycle-complete))
          (solve-subgoal (open cycle-gate))
          (setf result
            (and
              (= (recorder-two-cycle-checkpoint-chain-length) 1)
              (let ((segment
                      (first
                        (recorder-subgoal-chain.segments
                          *recorder-subgoal-chain*))))
                (and (equal (recorder-subgoal-segment.goal segment)
                            '(open cycle-gate))
                     (= (solution.depth
                          (recorder-subgoal-segment.solution segment)) 3)
                     (= (recorder-subgoal-segment.cumulative-depth segment) 3)
                     (recorder-subgoal-segment.recording-open-p segment)))
              (recorder-two-cycle-open-checkpoint-p)
              (null *solution-paths*)
              (not *solutions-valid*)

              ;; A failed continuation leaves the accepted open-cycle checkpoint available.
              (progn
                (solve-subgoal (current-cycle-status cycle-unreachable))
                t)
              (= (recorder-two-cycle-checkpoint-chain-length) 1)
              (recorder-two-cycle-open-checkpoint-p)
              (ww-undo)
              (= (recorder-two-cycle-checkpoint-chain-length) 1)
              (recorder-two-cycle-open-checkpoint-p)

              ;; The final search discovers the remaining first-cycle ending and enters
              ;; cycle 2 without a user-authored cycle boundary subgoal.
              (progn (solve) t)
              (= (recorder-two-cycle-checkpoint-chain-length) 2)
              *solutions-valid*
              (= (solution.depth (select-continuation-solution)) 6)
              (equal *goal* '(current-cycle-status cycle-complete))
              (recorder-two-cycle-checkpoint-solution-p)

              ;; Undo restores the open first-cycle checkpoint, then the original state.
              (ww-undo)
              (= (recorder-two-cycle-checkpoint-chain-length) 1)
              (recorder-two-cycle-open-checkpoint-p)
              (not *solutions-valid*)
              (ww-undo)
              (null *recorder-subgoal-chain*)
              (null *final-goal*)
              (null *undo-stack*)
              (equal initial-database
                     (list-database (problem-state.idb *start-state*)))
              (not (recorder-two-cycle-fact-p
                     *start-state* '(latched cycle-plate)))
              (not (recorder-two-cycle-fact-p
                     *start-state* '(recording-latched cycle-plate)))
              (not (recorder-two-cycle-fact-p
                     *start-state* '(open cycle-gate)))
              (not (recorder-two-cycle-fact-p
                     *start-state* '(recording-open cycle-gate))))))
      (install-compiled-goal harness-goal))
    result))


(define-goal
  (always-true))
