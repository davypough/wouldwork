;;; Filename: problem-recorder-cycle-state-test.lisp

;;; Focused characterization of planner-native recorder cycle state.  The test applies the
;;; real START-RECORDER and STOP-RECORDER actions to small states, without asking search to
;;; parse multiple windows (that is Stage 2).  It verifies count materialization, maximum
;;; enforcement, physical cross-layer boundary rejection, nonphysical cross-layer link
;;; removal, ghost disappearance, persistent ordinary latch state, recording-shadow
;;; reseeding, and a second clean fork from a changed live baseline.  Expected ordinary
;;; harness path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-cycle-state-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(ww-set *max-recorder-cycles* 2)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  toggle-plate (cycle-plate)
  location (recorder-site first-site second-site))


(include-tech recorder)
(include-tech plate)


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)
  (has-location live-agent recorder-site)
  (has-location live-box first-site)
  (has-position recorder1 recorder-site)
  (has-position cycle-plate recorder-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action persist-cycle-latch
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress)
       (not (latched cycle-plate)))
  (">" ?agent "creates a persistent latch change")
  (assert (latched cycle-plate)))


(define-action move-live-box-between-cycles
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recorder-cycle-closed)
       (not (recording-in-progress))
       (has-location live-box first-site))
  (">" ?agent "moves the live box between recorder cycles")
  (assert (has-location live-box second-site)))


(define-test-helper recorder-cycle-state-fact-p (state proposition)
  (member proposition (database state) :test #'equal))


(define-test-helper recorder-cycle-apply (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (unless valid-p
      (error "Recorder cycle test action failed: ~S (~S)" action diagnostic))
    next-state))


(define-test-helper recorder-cycle-action-rejected-p (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (declare (ignore next-state diagnostic))
    (not valid-p)))


(define-test-claim recorder-cycle-normalization-and-refork
  (let* ((opened-1
           (recorder-cycle-apply *start-state* '(start-recorder live-agent)))
         (latched
           (recorder-cycle-apply opened-1 '(persist-cycle-latch live-agent)))
         (closed-1
           (recorder-cycle-apply latched '(stop-recorder ghost-agent)))
         (setup-2
           (recorder-cycle-apply
             closed-1 '(move-live-box-between-cycles live-agent)))
         (opened-2
           (recorder-cycle-apply setup-2 '(start-recorder live-agent)))
         (closed-2
           (recorder-cycle-apply opened-2 '(stop-recorder ghost-agent))))
    (and
      (= (funcall (symbol-function 'recorder-cycle-count) opened-1) 1)
      (recorder-cycle-state-fact-p opened-1
                                   '(has-location ghost-box first-site))
      (not (recorder-cycle-state-fact-p opened-1
                                        '(has-location ghost-box second-site)))

      (recorder-cycle-boundary-closed-p closed-1)
      (not (recorder-state-contains-ghost-reference-p closed-1))
      (recorder-cycle-state-fact-p closed-1 '(latched cycle-plate))
      (recorder-cycle-state-fact-p closed-1
                                   '(recording-latched cycle-plate))
      (= (funcall (symbol-function 'recorder-cycle-count) closed-1) 1)

      (= (funcall (symbol-function 'recorder-cycle-count) opened-2) 2)
      (recorder-cycle-state-fact-p opened-2
                                   '(has-location ghost-box second-site))
      (not (recorder-cycle-state-fact-p opened-2
                                        '(has-location ghost-box first-site)))
      (recorder-cycle-state-fact-p opened-2 '(latched cycle-plate))

      (recorder-cycle-boundary-closed-p closed-2)
      (not (recorder-state-contains-ghost-reference-p closed-2))
      (= (funcall (symbol-function 'recorder-cycle-count) closed-2) 2)
      (recorder-cycle-action-rejected-p
        closed-2 '(start-recorder live-agent)))))


(define-test-claim recorder-cycle-rejects-cross-layer-boundary
  (let ((opened
          (recorder-cycle-apply *start-state* '(start-recorder live-agent))))
    (add-proposition '(on live-agent ghost-box) (problem-state.idb opened))
    (invalidate-problem-state-hash opened)
    (and (recorder-cross-layer-boundary-reference-p opened)
         (recorder-cycle-action-rejected-p
           opened '(stop-recorder ghost-agent)))))


(define-test-claim recorder-cycle-drops-cross-layer-pairing
  (let ((opened
          (recorder-cycle-apply *start-state* '(start-recorder live-agent))))
    ;; PAIRED is intentionally synthetic here: the boundary policy operates on stored
    ;; relation names and recorder sides, independently of the beam-relay capability.
    (add-proposition '(paired live-box ghost-box) (problem-state.idb opened))
    (invalidate-problem-state-hash opened)
    (let ((closed
            (recorder-cycle-apply opened '(stop-recorder ghost-agent))))
      (and (not (recorder-cross-layer-boundary-reference-p opened))
           (recorder-cycle-boundary-closed-p closed)
           (not (recorder-cycle-state-fact-p
                  closed '(paired live-box ghost-box)))
           (not (recorder-state-contains-ghost-reference-p closed))))))


(define-test-claim recorder-cycle-requires-physical-closure
  (let ((opened
          (recorder-cycle-apply *start-state* '(start-recorder live-agent))))
    (add-proposition '(has-location ghost-agent second-site)
                     (problem-state.idb opened))
    (invalidate-problem-state-hash opened)
    (and (not (recorder-cycle-agents-ready-p opened))
         (recorder-cycle-action-rejected-p
           opened '(stop-recorder ghost-agent)))))


(define-test-claim recorder-cycle-legacy-count
  (let ((legacy-open (copy-problem-state *start-state*)))
    (add-proposition '(recording-in-progress) (problem-state.idb legacy-open))
    (invalidate-problem-state-hash legacy-open)
    (and (= (funcall (symbol-function 'recorder-cycle-count) *start-state*) 0)
         (= (funcall (symbol-function 'recorder-cycle-count) legacy-open) 1))))


(define-test-claim recorder-cycle-parameter-validation
  (progn
    (check-problem-parameter '*max-recorder-cycles* 1)
    t)
  (expect-condition
    (lambda () (check-problem-parameter '*max-recorder-cycles* 0))
    'error
    :containing "positive integer")
  (expect-condition
    (lambda () (check-problem-parameter '*max-recorder-cycles* nil))
    'error
    :containing "positive integer"))


(define-goal
  (always-true))
