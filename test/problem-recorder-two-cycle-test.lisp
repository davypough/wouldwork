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
;;; ordinary SOLVE finds the unique seven-action path.  Guided cycle lengths are four and
;;; three actions.  The
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


(define-test-helper recorder-two-cycle-first-record-p ()
  (let* ((record (first *recorder-cycle-history*))
         (boundary (and record (recorder-cycle-record.boundary-state record)))
         (report (and record (recorder-cycle-record.report record))))
    (and record
         (equal (recorder-cycle-record.subgoal record) '(open cycle-gate))
         (= (recorder-cycle-record.depth record) 4)
         (= (recorder-cycle-record.elapsed-time record) 4)
         (= (recorder-cycle-record.cumulative-depth record) 4)
         (= (recorder-cycle-record.cumulative-time record) 4)
         (recorder-two-cycle-fact-p boundary '(latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(open cycle-gate))
         (recorder-two-cycle-fact-p boundary '(recording-latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(recording-open cycle-gate))
         (not (recorder-two-cycle-fact-p boundary '(playback-latch-primed)))
         (not (recorder-state-contains-ghost-reference-p boundary))
         (equal (getf report :integrated)
                '((1.0 (start-recorder live-agent))
                  (2.0 (prime-playback-latch ghost-agent))
                  (3.0 (latch-playback-gate live-agent))
                  (4.0 (stop-recorder ghost-agent))))
         (equal (getf report :recording)
                '((1.0 (start-recorder live-agent))
                  (2.0 (prime-playback-latch ghost-agent))
                  (pause)
                  (4.0 (stop-recorder ghost-agent))))
         (equal (getf report :playback)
                '((2.0 (prime-playback-latch ghost-agent))
                  (pause)
                  (3.0 (latch-playback-gate live-agent)))))))


(define-test-helper recorder-two-cycle-prepared-baseline-p ()
  ;; ON is bijective, so the database stores its ON1/ON2 index pair rather than a plain
  ;; (ON ...) tuple; check ON1 (keyed by the occupant) directly.
  (and (recorder-two-cycle-fact-p *start-state* '(on1 live-agent cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(depressed cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(latched cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(open cycle-gate))
       (recorder-two-cycle-fact-p
         *start-state* '(recording-depressed cycle-plate))
       (recorder-two-cycle-fact-p
         *start-state* '(recording-latched cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(recording-open cycle-gate))
       (recorder-two-cycle-fact-p
         *start-state* '(current-cycle-status cycle-pending))))


(define-test-helper recorder-two-cycle-final-record-p ()
  (let* ((record (second *recorder-cycle-history*))
         (boundary (and record (recorder-cycle-record.boundary-state record)))
         (report (and record (recorder-cycle-record.report record))))
    (and record
         (equal (recorder-cycle-record.subgoal record)
                '(current-cycle-status cycle-complete))
         (= (recorder-cycle-record.depth record) 3)
         (= (recorder-cycle-record.elapsed-time record) 3)
         (= (recorder-cycle-record.cumulative-depth record) 7)
         (= (recorder-cycle-record.cumulative-time record) 7)
         (recorder-two-cycle-fact-p boundary '(latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(recording-latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(open cycle-gate))
         (recorder-two-cycle-fact-p boundary '(recording-open cycle-gate))
         (recorder-two-cycle-fact-p
           boundary '(current-cycle-status cycle-complete))
         (not (recorder-state-contains-ghost-reference-p boundary))
         (equal (getf report :integrated)
                '((5.0 (start-recorder live-agent))
                  (6.0 (use-recording-gate ghost-agent))
                  (7.0 (stop-recorder ghost-agent))))
         (equal (getf report :recording)
                '((5.0 (start-recorder live-agent))
                  (6.0 (use-recording-gate ghost-agent))
                  (7.0 (stop-recorder ghost-agent))))
         (equal (getf report :playback)
                '((6.0 (use-recording-gate ghost-agent)))))))


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
                   (null *recorder-cycle-history*)
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


(define-test-claim recorder-two-cycle-commit-recover-and-undo
  (let ((initial-database (list-database (problem-state.idb *start-state*)))
        (harness-goal (copy-tree *goal*))
        (result nil))
    (unwind-protect
        (progn
          (install-compiled-goal '(current-cycle-status cycle-complete))
          (solve-subgoal (open cycle-gate))
          (setf result
            (and
              (= (length *recorder-cycle-history*) 1)
              (recorder-two-cycle-first-record-p)
              (recorder-two-cycle-prepared-baseline-p)
              (null *solution-paths*)
              (not *solutions-valid*)

              ;; A failed second cycle leaves both the committed history boundary and its
              ;; freshly seeded next-cycle baseline available.  Undo removes only the
              ;; failed attempt.
              (progn
                (solve-subgoal (current-cycle-status cycle-unreachable))
                t)
              (= (length *recorder-cycle-history*) 1)
              (recorder-two-cycle-first-record-p)
              (recorder-two-cycle-prepared-baseline-p)
              (ww-undo)
              (= (length *recorder-cycle-history*) 1)
              (recorder-two-cycle-first-record-p)
              (recorder-two-cycle-prepared-baseline-p)

              (progn (solve) t)
              (= (length *recorder-cycle-history*) 2)
              (recorder-two-cycle-first-record-p)
              (recorder-two-cycle-final-record-p)
              *solutions-valid*
              (= (solution.depth (select-continuation-solution)) 3)
              (equal *goal* '(current-cycle-status cycle-complete))

              ;; Undo the final cycle back to the reseeded boundary, then undo cycle 1
              ;; back to the original closed gate and unlatched recording shadow.
              (ww-undo)
              (= (length *recorder-cycle-history*) 1)
              (recorder-two-cycle-first-record-p)
              (recorder-two-cycle-prepared-baseline-p)
              (not *solutions-valid*)
              (ww-undo)
              (null *recorder-cycle-history*)
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
