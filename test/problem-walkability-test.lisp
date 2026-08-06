;;; Filename: problem-walkability-test.lisp
;;;
;;; Dedicated regression coverage for walking through the mobility core.
;;;
;;; The main scenario requires one MOVE action from MAIN-START to MAIN-GOAL.
;;; Its derived closure crosses a directional first edge and a symmetric edge
;;; whose disjunctive obstacles are cleared only by the open-gate/screen clause.
;;;
;;; Independent characterization scenarios verify:
;;; - directional asymmetry and disjunctive obstacle handling;
;;; - empty-hand passage through screens and ladders, and rejection while holding;
;;; - exact elevation equality and rejection of a one-level mismatch;
;;; - derived traversability for a supported agent while MOVE remains unavailable;
;;; - reflexive traversability without a no-op MOVE successor; and
;;; - removal of the main agent's old location after walking.
;;;
;;; Expected minimum path length: 1.

(in-package :ww)

(ww-set *problem-name* walkability-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)

(define-types
  agent (main-agent holding-agent supported-agent isolated-agent)
  location (main-start main-mid main-goal
            canonical-start canonical-a canonical-b canonical-goal
            shortest-start shortest-mid shortest-goal
            screen-start screen-goal ladder-start ladder-goal
            closed-start closed-goal
            level-start level-peer level-high
            supported-start supported-goal isolated-site)
  gate (open-gate open-gate-b closed-gate)
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
  (open open-gate-b)

  (walk-via> main-start () main-mid)
  (walk-via main-mid
            ((closed-gate) (open-gate screen1) (open-gate-b))
            main-goal)

  ;; Two equal-length routes exercise the lexical tie-break; the separate
  ;; direct edge proves that segment count takes precedence over lexical order.
  (walk-via> canonical-start () canonical-a)
  (walk-via> canonical-a () canonical-goal)
  (walk-via> canonical-start () canonical-b)
  (walk-via> canonical-b () canonical-goal)
  (walk-via> shortest-start () shortest-mid)
  (walk-via> shortest-mid () shortest-goal)
  (walk-via> shortest-start () shortest-goal)

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

(define-test-helper move-action-updates (state agent)
  "Return every update produced by the installed MOVE action for AGENT in STATE."
  (let* ((action (find 'move *actions* :key #'action.name))
         (args (list agent)))
    (when (member args (get-precondition-args action state) :test #'equal)
      (let ((pre-result
              (apply (action.pre-defun-name action) state args)))
        (when pre-result
          (if (eql pre-result t)
              (funcall (action.eff-defun-name action) state)
              (apply (action.eff-defun-name action)
                     state
                     pre-result)))))))


(define-test-helper move-action-produces-successor-p (state agent)
  "Whether the installed MOVE action produces a successor for AGENT in STATE."
  (not (null (move-action-updates state agent))))


(define-test-claim move-produces-one-successor-per-endpoint
  (let* ((updates (move-action-updates *start-state* 'main-agent))
         (destinations
           (mapcar (lambda (update)
                     (third (update.instantiations update)))
                   updates)))
    (and (= (length updates) 2)
         (= (count 'main-mid destinations) 1)
         (= (count 'main-goal destinations) 1)
         (equal
           (fourth
             (update.instantiations
               (find 'main-goal updates
                     :key (lambda (update)
                            (third (update.instantiations update))))))
           '((walk main-start nil main-mid)
             (walk main-mid (open-gate-b) main-goal)))
         (not (move-action-updates *start-state* 'supported-agent))
         (not (move-action-updates *start-state* 'isolated-agent)))))


(define-test-claim mobility-route-selection-is-canonical
  (and
    (equal
      (second
        (assoc 'canonical-goal
               (funcall (symbol-function 'mobility-results)
                        *start-state* 'main-agent 'canonical-start)))
      '((walk canonical-start nil canonical-a)
        (walk canonical-a nil canonical-goal)))
    (equal
      (second
        (assoc 'shortest-goal
               (funcall (symbol-function 'mobility-results)
                        *start-state* 'main-agent 'shortest-start)))
      '((walk shortest-start nil shortest-goal)))
    (equal
      (funcall (symbol-function 'mobility-results)
               *start-state* 'main-agent 'canonical-start)
      (funcall (symbol-function 'mobility-results)
               *start-state* 'main-agent 'canonical-start))))

(define-query walkability-scenarios-valid ()
  (and
    ;; The successful move must replace, rather than retain, the old location.
    (has-location main-agent main-goal)
    (not (has-location main-agent main-start))
    (not (has-location main-agent main-mid))

    ;; MAIN-START has a three-location transitive closure.  Its first edge is
    ;; directional; the second is symmetric and uses the passing DNF clause.
    (= (length (mobility-locations main-agent main-start)) 3)
    (member 'main-start
            (mobility-locations main-agent main-start))
    (member 'main-mid
            (mobility-locations main-agent main-start))
    (member 'main-goal
            (mobility-locations main-agent main-start))
    (one-step-walkable main-agent main-start main-mid)
    (not (one-step-walkable main-agent main-mid main-start))
    (one-step-walkable main-agent main-mid main-goal)
    (traversable main-agent main-start main-goal)
    (not (traversable main-agent main-mid main-start))

    ;; The closed-gate-only route is blocked.  The open gate remains open.
    (open open-gate)
    (not (open closed-gate))
    (not (one-step-walkable main-agent closed-start closed-goal))
    (not (traversable main-agent closed-start closed-goal))

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
    (traversable main-agent level-start level-peer)
    (not (traversable main-agent level-start level-high))

    ;; Support does not change the closure, but it disables the MOVE action.
    (has-location supported-agent supported-start)
    (on supported-agent support-box)
    (traversable supported-agent supported-start supported-goal)
    (not (move-action-produces-successor-p state 'supported-agent))

    ;; Every start is traversable from itself, but MOVE cannot make a no-op child.
    (has-location isolated-agent isolated-site)
    (= (length
         (mobility-locations isolated-agent isolated-site))
       1)
    (member 'isolated-site
            (mobility-locations isolated-agent isolated-site))
    (traversable isolated-agent isolated-site isolated-site)
    (not (move-action-produces-successor-p state 'isolated-agent))))

(define-goal
  (walkability-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-action-precondition-mutation move-allows-supported-agent move
  (and (bind (has-location ?agent $source))
       (assign $mobility-results
               (mobility-results ?agent $source)))
  "Drops MOVE's ground-only guard.  The supported-agent probe must then make
   this characterization fail.")
