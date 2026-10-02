;;; Filename: problem-recorder-snapshot-validation-test.lisp

;;; Focused regression for recorder snapshot validation.  PREPARE-SNAPSHOT moves the live
;;; box before START-RECORDER.  The valid ghost action therefore depends on START-RECORDER
;;; forking that current location, rather than the location authored in DEFINE-INIT.  The
;;; alternate ghost action demonstrates the converse: replaying START-RECORDER from the
;;; original problem state would accept it, but replaying from the true snapshot rejects it.
;;; HOLD-BEFORE-RECORDER additionally characterizes the session boundary itself: even at the
;;; recorder, a live agent carrying a mapped object cannot start recording.
;;;
;;; Expected minimum path length: three.

(in-package :ww)


(ww-set *problem-name* recorder-snapshot-validation-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  snapshot-result (snapshot-unused snapshot-current snapshot-original)
  location (recorder-site original-site snapshot-site))


(include-tech recorder)


(define-dynamic-relations
  (current-snapshot-result snapshot-result))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)
  (has-location live-agent recorder-site)
  (has-location live-box original-site)
  (has-position recorder1 recorder-site)
  (current-snapshot-result snapshot-unused))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action prepare-snapshot
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (recording-in-progress))
       (has-location live-box original-site))
  (">" ?agent "moves the live box before starting the recorder")
  (assert (has-location live-box snapshot-site)))


(define-action hold-before-recorder
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (recording-in-progress))
       (has-location ?agent recorder-site)
       (has-location live-box original-site))
  (">" ?agent "holds the box before starting the recorder")
  (assert (holding ?agent live-box)
          (has-location live-box recorder-site)))


(define-action use-current-snapshot
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (has-location ghost-box snapshot-site)
       (current-snapshot-result snapshot-unused))
  (">" ?agent "uses the box inherited from the current snapshot")
  (assert (not (current-snapshot-result snapshot-unused))
          (current-snapshot-result snapshot-current)))


(define-action use-original-snapshot
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (has-location ghost-box original-site)
       (current-snapshot-result snapshot-unused))
  (">" ?agent "uses the box from the authored initial state")
  (assert (not (current-snapshot-result snapshot-unused))
          (current-snapshot-result snapshot-original)))


(define-test-helper recorder-snapshot-valid-path ()
  '((1.0 (prepare-snapshot live-agent))
    (2.0 (start-recorder live-agent))
    (3.0 (use-current-snapshot ghost-agent))))


(define-test-helper recorder-snapshot-stale-path ()
  '((1.0 (prepare-snapshot live-agent))
    (2.0 (start-recorder live-agent))
    (3.0 (use-original-snapshot ghost-agent))))


(define-test-helper recorder-held-start-path ()
  '((1.0 (hold-before-recorder live-agent))
    (2.0 (start-recorder live-agent))))


(define-test-claim recorder-snapshot-validation-contract
  (equal (recorder-pre-recording-path (recorder-snapshot-valid-path))
         '((1.0 (prepare-snapshot live-agent))))
  (multiple-value-bind (snapshot diagnostic)
      (recorder-recording-snapshot *start-state* (recorder-snapshot-valid-path))
    (and snapshot
         (null diagnostic)
         (member '(has-location live-box snapshot-site)
                 (database snapshot)
                 :test #'equal)
         (not (member '(has-location ghost-box snapshot-site)
                      (database snapshot)
                      :test #'equal))))
  (validate-recorder-solution
    *start-state* (recorder-snapshot-valid-path) *start-state*)
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state* (recorder-snapshot-stale-path) *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :recording)
         (eql (getf diagnostic :reason) :action-failed)
         (equal (getf diagnostic :action)
                '(use-original-snapshot ghost-agent))))
  (let ((validation
          (validate-action-sequence *start-state* (recorder-held-start-path))))
    (and (not (action-sequence-validation-success-p validation))
         (= (action-sequence-validation-failure-index validation) 2)
         (equal (action-sequence-validation-failure-action validation)
                '(start-recorder live-agent))))
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state*
        '((1.0 (start-recorder live-agent))
          (2.0 (start-recorder live-agent)))
        *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :reason) :invalid-boundary)
         (eql (getf diagnostic :detail) :multiple-starts))))


(define-goal
  (current-snapshot-result snapshot-current))
