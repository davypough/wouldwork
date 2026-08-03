;;; Filename: problem-recorder-cycle-boundary-test.lisp

;;; Zero-action characterization of the chained recorder boundary contract.  The playback
;;; baseline has a latched, ghost-occupied toggle plate, one powered receiver/gate/gears
;;; lane, and one unpowered lane.  A synthetic preceding-cycle state inverts every
;;; recording shadow fact.  Preparation must leave that boundary untouched, copy it, seed
;;; the plate's stateful edge memory, and derive the remaining shadow from the new baseline.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-cycle-boundary-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  toggle-plate (cycle-plate)
  pressure-plate (quiet-plate)
  transmitter (transmitter1)
  receiver (powered-receiver quiet-receiver)
  gate (powered-gate quiet-gate)
  wall-gears (powered-gears quiet-gears)
  hue (blue)
  location (recorder-site away-site))


(include-tech recorder)
(include-tech plate)
(include-tech gate)
(include-tech beam-direct)
(include-tech -gears-fan)


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
  (has-position recorder1 recorder-site)
  (has-position cycle-plate recorder-site)
  (has-position quiet-plate recorder-site)
  (on ghost-agent cycle-plate)
  (latched cycle-plate)

  (has-chroma transmitter1 blue)
  (has-chroma powered-receiver blue)
  (has-chroma quiet-receiver blue)
  (coupled transmitter1 powered-receiver)
  (beam-via transmitter1 () powered-receiver)

  (controls ((powered-receiver)) powered-gate normal)
  (controls ((quiet-receiver)) quiet-gate normal)
  (controls ((cycle-plate)) powered-gears normal)
  (controls ((quiet-plate)) quiet-gears normal))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper recorder-boundary-fact-p (state proposition)
  (member proposition
          (list-database (problem-state.idb state))
          :test #'equal))


(define-test-helper delete-recorder-boundary-fact! (state proposition)
  (delete-proposition proposition (problem-state.idb state))
  (setf (problem-state.idb-hash state) nil)
  state)


(define-test-helper add-recorder-boundary-fact! (state proposition)
  (add-proposition proposition (problem-state.idb state))
  (setf (problem-state.idb-hash state) nil)
  state)


(define-test-helper corrupted-recorder-cycle-boundary ()
  (let ((state (copy-problem-state *start-state*)))
    (dolist (proposition
              '((recording-depressed cycle-plate)
                (recording-latched cycle-plate)
                (recording-active powered-receiver)
                (recording-open powered-gate)
                (recording-turning powered-gears)))
      (delete-recorder-boundary-fact! state proposition))
    (dolist (proposition
              '((recording-depressed quiet-plate)
                (recording-active quiet-receiver)
                (recording-open quiet-gate)
                (recording-turning quiet-gears)))
      (add-recorder-boundary-fact! state proposition))
    (setf (problem-state.name state) 'preceding-cycle
          (problem-state.instantiations state) '(ghost-agent)
          (problem-state.happenings state) '((clock (4 12.5 forward)))
          (problem-state.time state) 12.5
          (problem-state.value state) 7.0)
    state))


(define-test-helper open-recorder-cycle-boundary ()
  (let ((state (copy-problem-state *start-state*)))
    (delete-recorder-boundary-fact!
      state '(has-location ghost-agent recorder-site))
    (add-recorder-boundary-fact!
      state '(has-location ghost-agent away-site))
    state))


(define-test-helper recorder-cycle-preparation-signals-error-p (state)
  (handler-case
      (progn (prepare-recorder-cycle-state state) nil)
    (error () t)))


(define-test-claim recorder-cycle-boundary-policy
  (equal (recorder-cycle-goal '(always-true))
         '(and (always-true) (ghost-stops-recorder)))
  (equal *recorder-shadow-lifecycles*
         '((plate reset-recording-plate-shadow! seed-recording-plate-shadow!)
           (receiver reset-recording-receiver-shadow! nil)
           (gate reset-recording-gate-shadow! nil)
           (wall-gears reset-recording-wall-gears-shadow! nil)))
  (let* ((open-state (open-recorder-cycle-boundary))
         (before (list-database (problem-state.idb open-state))))
    (and (not (recorder-cycle-boundary-closed-p open-state))
         (recorder-cycle-preparation-signals-error-p open-state)
         (equal before (list-database (problem-state.idb open-state))))))


(define-test-claim recorder-cycle-shadow-preparation
  (let* ((boundary (corrupted-recorder-cycle-boundary))
         (before (list-database (problem-state.idb boundary)))
         (prepared (prepare-recorder-cycle-state boundary))
         (report (build-recorder-report (make-solution :goal prepared))))
    (and
      (not (eq boundary prepared))
      (not (eq (problem-state.idb boundary) (problem-state.idb prepared)))
      (equal before (list-database (problem-state.idb boundary)))

      (recorder-boundary-fact-p prepared '(recording-depressed cycle-plate))
      (recorder-boundary-fact-p prepared '(recording-latched cycle-plate))
      (recorder-boundary-fact-p prepared '(recording-active powered-receiver))
      (recorder-boundary-fact-p prepared '(recording-open powered-gate))
      (recorder-boundary-fact-p prepared '(recording-turning powered-gears))

      (not (recorder-boundary-fact-p prepared '(recording-depressed quiet-plate)))
      (not (recorder-boundary-fact-p prepared '(recording-active quiet-receiver)))
      (not (recorder-boundary-fact-p prepared '(recording-open quiet-gate)))
      (not (recorder-boundary-fact-p prepared '(recording-turning quiet-gears)))

      (eql (problem-state.name prepared) 'preceding-cycle)
      (equal (problem-state.instantiations prepared) '(ghost-agent))
      (equal (problem-state.happenings prepared) '((clock (4 12.5 forward))))
      (= (problem-state.time prepared) 12.5)
      (= (problem-state.value prepared) 7.0)
      (recorder-cycle-boundary-closed-p prepared)
      (not (state-is-inconsistent prepared))
      (equal (getf report :recording)
             '((start-recorder) (stop-recorder))))))


(define-goal
  (always-true))
