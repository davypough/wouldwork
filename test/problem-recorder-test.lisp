;;; Filename: problem-recorder-test.lisp

;;; Dedicated zero-action regression for recorder identity and placement.  The authored
;;; mapping pairs agents with agents and connectors with connectors, while an unmapped fan
;;; proves that MOBILE-OBJECT membership alone does not assign a recording side.  Direct
;;; validation probes characterize the one-to-one, disjoint, category-compatible contract.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (live-agent ghost-agent other-live-agent other-ghost-agent third-agent)
  connector (live-connector ghost-connector)
  fan (unmapped-fan)
  recorder (recorder1 unpositioned-recorder)
  location (recorder-site alternate-site))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech recorder)


;;;; INITIALIZATION ;;;;


(define-init
  (has-position recorder1 recorder-site)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-connector ghost-connector))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(define-test-claim recorder-schema
  (expect-relation-schema
    'recording-copy> :static '(mobile-object mobile-object)
    :fluent-indices '(2))
  ;; Membership, not the full union roster: another technology may add a fixed kind.
  (expect-type-component 'fixed-position-object 'recorder)
  (expect-type-instance 'fixed-position-object 'recorder1)
  (expect-registrations
    :solution-validator '(validate-recorder-solution))
  (expect-registrations
    :solution-printer '(print-recorder-report))
  (goal-chaining-policy-p *goal-chaining-policy*)
  (eq (goal-chaining-policy-subgoal-solver *goal-chaining-policy*)
      'solve-recorder-subgoal-form)
  (eq (goal-chaining-policy-final-solver *goal-chaining-policy*)
      'solve-recorder-final))


(define-test-claim recorder-validation
  (expect-condition
    (lambda ()
      (check-proposition '(recording-copy> recorder1 ghost-agent)))
    'error
    :containing "not of specified type MOBILE-OBJECT")
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-agent ghost-agent)
          (recording-copy> live-agent other-ghost-agent))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key")
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-agent ghost-agent)
          (recording-copy> other-live-agent ghost-agent))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "repeats a ghost object"
    :check 'recorder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-agent live-agent))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "maps an object to itself"
    :check 'recorder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-agent ghost-agent)
          (recording-copy> ghost-agent third-agent))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "both live and ghost sides"
    :check 'recorder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-agent ghost-connector))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "incompatible object categories"
    :check 'recorder-init-check))


;;;; CHARACTERIZATION QUERIES AND GOAL ;;;;
;;;; One named query per theme rather than a single conjunction, so a regression narrows to
;;;; a handful of clauses and each theme can be exercised on its own at the repl, eg
;;;; (funcall 'recorder-side-classification-valid *start-state*).


(define-query recorder-mapping-valid ()
  ;; RECORDING-COPY> holds as authored and is directional: the reverse pair is not a fact.
  (and (recording-copy> live-agent ghost-agent)
       (recording-copy> live-connector ghost-connector)
       (not (recording-copy> ghost-agent live-agent))))


(define-query recorder-side-classification-valid ()
  ;; Each mapped object classifies onto exactly one recording side.
  (and (live-recording-object live-agent)
       (live-recording-object live-connector)
       (not (ghost-recording-object live-agent))
       (ghost-recording-object ghost-agent)
       (ghost-recording-object ghost-connector)
       (not (live-recording-object ghost-agent))))


(define-query recorder-unmapped-object-valid ()
  ;; MOBILE-OBJECT membership alone assigns no side: the unmapped fan is neither live nor
  ;; ghost, which is what makes the mapping authoritative rather than exhaustive.
  (and (not (live-recording-object unmapped-fan))
       (not (ghost-recording-object unmapped-fan))))


(define-query recorder-position-valid ()
  ;; HAS-POSITION is functional over recorders, and a recorder may have none at all.
  (and (has-position recorder1 recorder-site)
       (not (has-position recorder1 alternate-site))
       (not (bind
              (has-position
                unpositioned-recorder $unpositioned-recorder-location)))))


(define-goal
  (and (recorder-mapping-valid)
       (recorder-side-classification-valid)
       (recorder-unmapped-object-valid)
       (recorder-position-valid)))
