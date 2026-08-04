;;; Filename: problem-recorder-two-cycle-test.lisp

;;; Executable two-cycle recorder specification.  Cycle 1's live agent presses a toggle
;;; plate, leaving ordinary playback with its controlled gate open while the recording
;;; shadow remains closed.  Committing that cycle must preserve the ordinary latch, discard
;;; its program, reset the stale shadow, seed RECORDING-LATCHED from the new playback
;;; baseline, and derive RECORDING-OPEN.  Only then can cycle 2's ghost use the recording
;;; gate.  The two actions cannot be flattened into one valid recording: ghost-only replay
;;; omits the live press and therefore sees the original closed recording gate.
;;; Expected chained path length: one locally selected action in each of two cycles.  The
;;; ordinary Talos harness goal is deliberately zero-action: the characterization claim
;;; installs the non-flattenable final goal while it exercises the chained solver, then
;;; restores the harness goal before TEST-TALOS performs its required ordinary search.

(in-package :ww)


(ww-set *problem-name* recorder-two-cycle-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)

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


(enable-recorder-solution)


(define-dynamic-relations
  (current-cycle-status cycle-status))


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
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


(define-action latch-playback-gate
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (has-location ?agent recorder-site)
       (not (depressed cycle-plate))
       (not (latched cycle-plate)))
  (">" ?agent "presses the persistent playback latch")
  (assert (on ?agent cycle-plate)
          (finally (propagate-changes!))))


(define-action use-recording-gate
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
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
         (= (recorder-cycle-record.depth record) 1)
         (= (recorder-cycle-record.elapsed-time record) 1)
         (= (recorder-cycle-record.cumulative-depth record) 1)
         (= (recorder-cycle-record.cumulative-time record) 1)
         (recorder-two-cycle-fact-p boundary '(latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(open cycle-gate))
         (not (recorder-two-cycle-fact-p boundary '(recording-latched cycle-plate)))
         (not (recorder-two-cycle-fact-p boundary '(recording-open cycle-gate)))
         (equal (getf report :integrated)
                '((1.0 (latch-playback-gate live-agent))))
         (equal (getf report :recording)
                '((start-recorder) (pause) (stop-recorder)))
         (equal (getf report :playback)
                '((pause) (1.0 (latch-playback-gate live-agent)))))))


(define-test-helper recorder-two-cycle-prepared-baseline-p ()
  (and (recorder-two-cycle-fact-p *start-state* '(on live-agent cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(depressed cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(latched cycle-plate))
       (recorder-two-cycle-fact-p *start-state* '(open cycle-gate))
       (not (recorder-two-cycle-fact-p
              *start-state* '(recording-depressed cycle-plate)))
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
         (= (recorder-cycle-record.depth record) 1)
         (= (recorder-cycle-record.elapsed-time record) 1)
         (= (recorder-cycle-record.cumulative-depth record) 2)
         (= (recorder-cycle-record.cumulative-time record) 2)
         (recorder-two-cycle-fact-p boundary '(latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(recording-latched cycle-plate))
         (recorder-two-cycle-fact-p boundary '(open cycle-gate))
         (recorder-two-cycle-fact-p boundary '(recording-open cycle-gate))
         (recorder-two-cycle-fact-p
           boundary '(current-cycle-status cycle-complete))
         (equal (getf report :integrated)
                '((2.0 (use-recording-gate ghost-agent))))
         (equal (getf report :recording)
                '((start-recorder)
                  (2.0 (use-recording-gate ghost-agent))
                  (stop-recorder)))
         (equal (getf report :playback)
                '((2.0 (use-recording-gate ghost-agent)))))))


(define-test-claim recorder-two-cycle-cannot-be-flattened
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state*
        '((1.0 (latch-playback-gate live-agent))
          (2.0 (use-recording-gate ghost-agent)))
        *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :recording)
         (eql (getf diagnostic :reason) :action-failed)
         (equal (getf diagnostic :action)
                '(use-recording-gate ghost-agent)))))


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
              (= (solution.depth (select-continuation-solution)) 1)
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
