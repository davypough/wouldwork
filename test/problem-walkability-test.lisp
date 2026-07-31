;;; Filename: problem-walkability-test.lisp
;;;
;;; Dedicated regression coverage for relation-based walkability.
;;;
;;; The main scenario requires one WALK action from MAIN-START to MAIN-GOAL.
;;; Its derived closure crosses a directional first edge and a symmetric edge
;;; whose disjunctive obstacles are cleared only by the open-gate/screen clause.
;;;
;;; Independent characterization scenarios verify:
;;; - directional asymmetry and disjunctive obstacle handling;
;;; - empty-hand passage through screens and ladders, and rejection while holding;
;;; - exact elevation equality and rejection of a one-level mismatch;
;;; - derived reachability for a supported agent while WALK remains unavailable;
;;; - reflexive reachability without a no-op WALK successor; and
;;; - removal of the main agent's old location after walking.
;;;
;;; Expected minimum path length: 1.

(in-package :ww)

(ww-set *problem-name* walkability-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(define-types
  agent (main-agent holding-agent supported-agent isolated-agent)
  location (main-start main-mid main-goal
            screen-start screen-goal ladder-start ladder-goal
            closed-start closed-goal
            level-start level-peer level-high
            supported-start supported-goal isolated-site)
  gate (open-gate closed-gate)
  screen (screen1)
  ladder (ladder1)
  connector (carried-connector)
  box (support-box))

(include-tech walkability)

(define-init
  (has-location main-agent main-start)

  (has-location holding-agent screen-start)
  (holding holding-agent carried-connector)

  (has-location supported-agent supported-start)
  (has-location support-box supported-start)
  (on supported-agent support-box)

  (has-location isolated-agent isolated-site)

  (open open-gate)

  (walk-via> main-start () main-mid)
  (walk-via main-mid ((closed-gate) (open-gate screen1)) main-goal)

  (walk-via screen-start ((open-gate screen1)) screen-goal)
  (walk-via ladder-start ((open-gate ladder1)) ladder-goal)
  (walk-via closed-start ((closed-gate)) closed-goal)

  (has-elevation level-start 2)
  (has-elevation level-peer 2)
  (has-elevation level-high 3)
  (walk-via level-start () level-peer)
  (walk-via level-start () level-high)

  (walk-via supported-start () supported-goal))

(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))

(defun walk-action-produces-successor-p (state agent)
  "Whether the installed WALK action produces a successor for AGENT in STATE."
  (let* ((action (find 'walk *actions* :key #'action.name))
         (args (list agent)))
    (when (member args (get-precondition-args action state) :test #'equal)
      (let ((pre-result
              (apply (action.pre-defun-name action) state args)))
        (and pre-result
             (if (eql pre-result t)
                 (funcall (action.eff-defun-name action) state)
                 (apply (action.eff-defun-name action)
                        state
                        pre-result)))))))

(define-query walkability-scenarios-valid ()
  (and
    ;; The successful walk must replace, rather than retain, the old location.
    (has-location main-agent main-goal)
    (not (has-location main-agent main-start))
    (not (has-location main-agent main-mid))

    ;; MAIN-START has a three-location transitive closure.  Its first edge is
    ;; directional; the second is symmetric and uses the passing DNF clause.
    (= (length (walkable-locations main-agent main-start)) 3)
    (member 'main-start
            (walkable-locations main-agent main-start))
    (member 'main-mid
            (walkable-locations main-agent main-start))
    (member 'main-goal
            (walkable-locations main-agent main-start))
    (one-step-walkable main-agent main-start main-mid)
    (not (one-step-walkable main-agent main-mid main-start))
    (one-step-walkable main-agent main-mid main-goal)
    (walkable main-agent main-start main-goal)
    (not (walkable main-agent main-mid main-start))

    ;; The closed-gate-only route is blocked.  The open gate remains open.
    (open open-gate)
    (not (open closed-gate))
    (not (one-step-walkable main-agent closed-start closed-goal))
    (not (walkable main-agent closed-start closed-goal))

    ;; Empty hands pass screen and ladder barriers; holding blocks both.
    (one-step-walkable main-agent screen-start screen-goal)
    (one-step-walkable main-agent ladder-start ladder-goal)
    (holding holding-agent carried-connector)
    (not (one-step-walkable
           holding-agent screen-start screen-goal))
    (not (one-step-walkable
           holding-agent ladder-start ladder-goal))

    ;; Elevations must be exactly equal.
    (= (location-elevation level-start) 2)
    (= (location-elevation level-peer) 2)
    (= (location-elevation level-high) 3)
    (one-step-walkable main-agent level-start level-peer)
    (not (one-step-walkable main-agent level-start level-high))
    (walkable main-agent level-start level-peer)
    (not (walkable main-agent level-start level-high))

    ;; Support does not change the closure, but it disables the WALK action.
    (has-location supported-agent supported-start)
    (on supported-agent support-box)
    (walkable supported-agent supported-start supported-goal)
    (not (walk-action-produces-successor-p state 'supported-agent))

    ;; Every start is reachable from itself, but WALK cannot make a no-op child.
    (has-location isolated-agent isolated-site)
    (= (length
         (walkable-locations isolated-agent isolated-site))
       1)
    (member 'isolated-site
            (walkable-locations isolated-agent isolated-site))
    (walkable isolated-agent isolated-site isolated-site)
    (not (walk-action-produces-successor-p state 'isolated-agent))))

(define-goal
  (walkability-scenarios-valid))
