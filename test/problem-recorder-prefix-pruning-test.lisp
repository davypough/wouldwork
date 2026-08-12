;;; Filename: problem-recorder-prefix-pruning-test.lisp

;;; Focused characterization of recorder recording-prefix pruning.  The valid branch
;;; prepares a fact before START-RECORDER, so the fact belongs to the captured snapshot and
;;; remains available to the isolated ghost recording.  The invalid branch creates its fact
;;; with a live action after START-RECORDER.  Integrated search can therefore perform the
;;; following ghost action, but the isolated recording cannot: the live action is absent.
;;;
;;; Expected minimum path length: three.  Search-prefix validation should prune at least
;;; one state while retaining the valid branch.

(in-package :ww)


(ww-set *problem-name* recorder-prefix-pruning-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)
(ww-set *recorder-prefix-pruning* t)

(setf *expected-min-length* 3)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  prefix-result (prefix-unused prefix-valid prefix-invalid)
  location (recorder-site))


(include-tech recorder)


(define-dynamic-relations
  (snapshot-ready)
  (live-only-ready)
  (prefix-result prefix-result))


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-position recorder1 recorder-site)
  (prefix-result prefix-unused))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action prepare-snapshot-prefix
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (recording-in-progress))
       (not (snapshot-ready)))
  (">" ?agent "prepares a fact captured by the recorder snapshot")
  (assert (snapshot-ready)))


(define-action enable-live-only-prefix
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress)
       (not (live-only-ready)))
  (">" ?agent "creates a fact unavailable in the isolated recording")
  (assert (live-only-ready)))


(define-action use-snapshot-prefix
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (snapshot-ready)
       (prefix-result prefix-unused))
  (">" ?agent "uses the fact captured in the snapshot")
  (assert (not (prefix-result prefix-unused))
          (prefix-result prefix-valid)))


(define-action use-live-only-prefix
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (live-only-ready)
       (prefix-result prefix-unused))
  (">" ?agent "uses a live-only fact missing from the recording")
  (assert (not (prefix-result prefix-unused))
          (prefix-result prefix-invalid)))


(define-test-helper recorder-valid-prefix-path ()
  '((1.0 (prepare-snapshot-prefix live-agent))
    (2.0 (start-recorder live-agent))
    (3.0 (use-snapshot-prefix ghost-agent))))


(define-test-helper recorder-invalid-prefix-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (enable-live-only-prefix live-agent))
    (3.0 (use-live-only-prefix ghost-agent))))


(define-test-claim recorder-prefix-pruning-contract
  *recorder-prefix-pruning*
  (find 'validate-recorder-recording-prefix
        *search-prefix-validators*
        :key #'search-prefix-validator.validator)
  (validate-recorder-recording-prefix
    *start-state* (recorder-valid-prefix-path) *start-state*)
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-recording-prefix
        *start-state* (recorder-invalid-prefix-path) *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :recording)
         (eql (getf diagnostic :reason) :action-failed)
         (equal (getf diagnostic :action)
                '(use-live-only-prefix ghost-agent))))
  (candidate-search-prefix-valid-p
    (recorder-valid-prefix-path) *start-state*)
  (not (candidate-search-prefix-valid-p
         (recorder-invalid-prefix-path) *start-state*)))


(define-goal
  (prefix-result prefix-valid))
