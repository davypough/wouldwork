;;;; Filename: problem-vertical-reach-relevance-flat-test.lisp

;;; Technology-override characterization for a flat cargo model.  Pickup and placement
;;; splice -support-elevation and genuinely call its reach queries, but every location,
;;; agent base, cargo base, and usable support top is zero.  Explicit reach probes distinguish
;;; the overridden limit without changing the model's zero-action goal.

(in-package :ww)


(ww-set *problem-name* vertical-reach-relevance-flat-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


(define-types
  agent (flat-agent)
  location (flat-origin flat-target)
  connector (flat-connector)
  toggle-plate (flat-plate))


(include-tech -pickup)
(include-tech -placement)

(defparameter *vertical-reach-limit* 3/2)


(define-init
  (has-location flat-agent flat-origin)
  (has-location flat-connector flat-origin)
  (has-position flat-plate flat-target)
  (location-coords> flat-origin 0 0)
  (location-coords> flat-target 1 0))


(define-test-claim vertical-reach-parameter-hidden-for-flat-manipulation
  (= *vertical-reach-limit* 3/2)
  (not (vertical-reach-limit-relevant-p *start-state*))
  (not (search "*VERTICAL-REACH-LIMIT*"
               (with-output-to-string (*standard-output*)
                 (display-current-parameters)))))


(define-goal
  (and (within-agent-vertical-reach flat-agent 3/2)
       (not (within-agent-vertical-reach flat-agent 2))))
