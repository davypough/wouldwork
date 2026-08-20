;;; Filename: problem-box-test.lisp

;;;; Dedicated regression coverage for public box behavior.
;;;;
;;;; The three-action planning goal combines two independent lifecycles:
;;;;   1. pick up MOVING-BOX from SOURCE-BOX at the inclusive reach boundary,
;;;;      then put it on TARGET-BOX;
;;;;   2. put the already-held PLATE-BOX on PUT-PLATE, depressing the plate.
;;;;
;;;; The goal also characterizes action applicability without adding plan steps:
;;;; an occupied box cannot be picked up, a clear box just above vertical reach
;;;; cannot be picked up, an agent with a full hand cannot pick up another box,
;;;; and placement offers ground plus a clear box while excluding an occupied
;;;; box and an over-height location.
;;;;
;;;; Expected minimum path length: 3.

(in-package :ww)

(ww-set *problem-name* box-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)
(setf *expected-min-length* 3)

(define-types
  agent (lifecycle-agent plate-agent occupied-agent high-agent holding-agent
         probe-agent blocked-rider probe-rider)
  location (source-site target-site plate-site occupied-site high-site
            holding-site probe-origin probe-site too-high-site)
  pressure-plate (put-plate)
  box (moving-box source-box target-box plate-box blocked-box high-support
       high-box already-held-box loose-box probe-box clear-probe-support
       occupied-probe-support))

(include-tech plate)
(include-tech box)
(include-tech reachability)

(define-init
  ;; Lifecycle scenario: MOVING-BOX begins on a height-one support, exactly at
  ;; the fixed vertical-reach boundary.  TARGET-BOX is equally tall.  The agent's
  ;; explicit height two is deliberately independent of that unit reach.
  (has-location lifecycle-agent source-site)
  (has-height lifecycle-agent 2)
  (has-location source-box source-site)
  (has-height source-box 1)
  (has-location moving-box source-site)
  (on moving-box source-box)
  (has-location target-box target-site)
  (has-height target-box 1)
  (reach-via source-site () target-site)

  ;; Independent placement lifecycle: the held box must be put on the plate.
  (has-location plate-agent plate-site)
  (holding plate-agent plate-box)
  (has-position put-plate plate-site)

  ;; Occupied-support negative pickup scenario.
  (has-location occupied-agent occupied-site)
  (has-height occupied-agent 2)
  (has-location blocked-box occupied-site)
  (has-location blocked-rider occupied-site)
  (on blocked-rider blocked-box)

  ;; Clear but just-over-reach negative pickup scenario.
  (has-location high-agent high-site)
  (has-height high-agent 2)
  (has-location high-support high-site)
  (has-height high-support 2)
  (has-location high-box high-site)
  (on high-box high-support)

  ;; Empty-hand negative pickup scenario.
  (has-location holding-agent holding-site)
  (holding holding-agent already-held-box)
  (has-location loose-box holding-site)

  ;; Placement characterization: ground and one clear box are valid supports;
  ;; an occupied box and an elevation-two location are not.
  (has-location probe-agent probe-origin)
  (has-height probe-agent 2)
  (holding probe-agent probe-box)
  (has-location clear-probe-support probe-site)
  (has-height clear-probe-support 1)
  (has-location occupied-probe-support probe-site)
  (has-location probe-rider probe-site)
  (on probe-rider occupied-probe-support)
  (reach-via probe-origin () probe-site)
  (reach-via probe-origin () too-high-site)
  (has-elevation too-high-site 2))

(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))

(define-test-helper box-action-applicable-p (state action-name args)
  "Whether the installed box action produces a successor for ARGS in STATE."
  (let ((action (find action-name *actions* :key #'action.name)))
    (when (member args (get-precondition-args action state) :test #'equal)
      (let ((pre-result (apply (action.pre-defun-name action) state args)))
        (and pre-result
             (if (eql pre-result t)
               (funcall (action.eff-defun-name action) state)
               (apply (action.eff-defun-name action) state pre-result)))))))

(define-query box-scenarios-valid ()
  (and
      ;; The planned box lifecycle ends on the new support and releases the
      ;; source support.
      (has-location lifecycle-agent source-site)
      (not (holding lifecycle-agent moving-box))
      (has-location moving-box target-site)
      (on moving-box target-box)
      (not (on moving-box source-box))
      (cleartop source-box)
      (not (cleartop target-box))
      (= (top source-box) 1)
      (= (top target-box) 1)
      (= (base moving-box) 1)

      ;; Putting the independent held box on the plate depresses it.
      (not (holding plate-agent plate-box))
      (has-location plate-box plate-site)
      (on plate-box put-plate)
      (depressed put-plate)
      (not (cleartop put-plate))

      ;; BLOCKED-BOX is geometrically reachable, but its rider makes the box
      ;; itself unavailable to PICKUP-BOX.
      (not (cleartop blocked-box))
      (pickup-clear occupied-agent occupied-site blocked-box occupied-site)
      (not (box-action-applicable-p
             state 'pickup-box '(occupied-agent blocked-box)))

      ;; HIGH-BOX is clear, but elevation two is just beyond the fixed unit
      ;; vertical reach, regardless of the agent's explicit height two.
      (cleartop high-box)
      (= (top high-support) 2)
      (= (base high-box) 2)
      (not (within-agent-vertical-reach high-agent 2))
      (not (pickup-clear high-agent high-site high-box high-site))
      (not (box-action-applicable-p state 'pickup-box '(high-agent high-box)))

      ;; An otherwise available loose box remains unavailable while the agent
      ;; already holds a different box.
      (holding holding-agent already-held-box)
      (cleartop loose-box)
      (not (pickup-clear holding-agent holding-site loose-box holding-site))
      (not (box-action-applicable-p
             state 'pickup-box '(holding-agent loose-box)))

      ;; Placement exposes exactly the intended reach/occupancy boundaries.
      (reachable probe-site probe-origin)
      (member 'ground
              (placement-options probe-agent probe-site probe-box))
      (member 'clear-probe-support
              (placement-options probe-agent probe-site probe-box))
      (not (member 'occupied-probe-support
                   (placement-options probe-agent probe-site probe-box)))
      (box-action-applicable-p
        state 'put-box '(probe-agent probe-box probe-site))
      (reachable too-high-site probe-origin)
      (= (location-elevation too-high-site) 2)
      (not (within-agent-vertical-reach probe-agent 2))
      (not (placement-options probe-agent too-high-site probe-box))
      (not (box-action-applicable-p
             state 'put-box '(probe-agent probe-box too-high-site)))))

(define-goal
  (box-scenarios-valid))
