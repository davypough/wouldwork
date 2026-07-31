;;; Filename: problem-gun-test.lisp

;;; Gun regression covering threat safety, controls, multiple threats, and jammer lifecycle.
;;; gun1 is an uncontrolled point fixture, positioned via LOS rather than HAS-POSITION, and
;;; threatens watched on the only walking route from start to goal.  The agent must pick up
;;; jammer1, jam gun1 from start, cross watched while it is safe, then remotely pick the
;;; jammer back up from goal.  That last pickup clears the jam and rearms gun1; the final
;;; characterization checks that watched is again unsafe and absent from walkable-locations,
;;; so a broken threat-safety filter cannot pass merely by producing a shorter walk.
;;; Uncontrolled guns default on, matching the default turning behavior of gears-fan.
;;;
;;; Independent zero-action scenarios verify normal and inverted plate control in both
;;; states, plus a shared location threatened by two guns: jamming one does not make the
;;; location safe while the other remains lethal.  The goal also invokes JAM-TARGET's
;;; installed precondition directly.  It requires an allowed clear sightline to be
;;; applicable, while rejecting both a visible JAM-DISALLOWED> placement and a structural
;;; sightline blocked by a closed gate.
;;;
;;; Expected minimum solution (4 steps): pickup-jammer jammer1; jam-target gun1 at start;
;;; walk start->goal through watched; pickup-jammer jammer1 remotely from start.


(in-package :ww)


(ww-set *problem-name* gun-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (start watched goal disallowed-site blocked-site
            controls-site normal-clear-zone normal-down-zone
            inverted-clear-zone inverted-down-zone shared-zone jammed-site)
  jammer (jammer1 jammer2)
  gun (gun1 gun2 gun3 gun4 gun5 gun6 gun7)
  gate (gate1)
  plate (plate1 plate2 plate3 plate4)
  box (weight1 weight2)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech gun)
(include-tech jammer)
(include-tech walkability)
(include-tech reachability)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Lifecycle scenario: jammer1 starts with the agent; jammer2 keeps gun2 disabled
  ;; throughout the independent multiple-threat scenario.
  (has-location agent1 start)
  (has-location jammer1 start)
  (has-location jammer2 jammed-site)
  (jamming jammer2 gun2)

  ;; Jam-target sightlines.  gun1 and gun2 have clear authored LOS.  gun3 has structural
  ;; LOS from blocked-site, but its sole occluder gate is closed.
  (los-to-apparatus start () gun1)
  (los-to-apparatus disallowed-site () gun2)
  (los-to-apparatus blocked-site (gate1) gun3)
  (jam-disallowed> goal disallowed-site gun2)

  ;; Uncontrolled and controlled kill zones.  gun2 and gun3 deliberately overlap.
  (threatens gun1 (watched))
  (threatens gun2 (shared-zone))
  (threatens gun3 (shared-zone))
  (threatens gun4 (normal-clear-zone))
  (threatens gun5 (normal-down-zone))
  (threatens gun6 (inverted-clear-zone))
  (threatens gun7 (inverted-down-zone))

  ;; The lifecycle's walking route and the final-state reach edges used to retrieve
  ;; jammer1 and evaluate the three jam-target placements.
  (walk-via start () watched)
  (walk-via watched () goal)
  (reach-via goal () start)
  (reach-via goal () disallowed-site)
  (reach-via goal () blocked-site)

  ;; Four independent control cases: clear/depressed for both normal and inverted modes.
  (has-position plate1 controls-site)
  (has-position plate2 controls-site)
  (has-position plate3 controls-site)
  (has-position plate4 controls-site)
  (has-location weight1 controls-site)
  (has-location weight2 controls-site)
  (on weight1 plate2)
  (on weight2 plate4)
  (controls ((plate1)) gun4 normal)
  (controls ((plate2)) gun5 normal)
  (controls ((plate3)) gun6 inverted)
  (controls ((plate4)) gun7 inverted)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(defun jam-target-applicable-p (state agent target location)
  "Whether the installed JAM-TARGET action accepts this exact parameter tuple in STATE."
  (let* ((action (find 'jam-target *actions* :key #'action.name))
         (args (list agent target location)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


(define-query gun-scenarios-valid ()
  (and
    ;; jammer1 has completed the jam/cross/retrieve lifecycle, leaving gun1 rearmed.
    (has-location agent1 goal)
    (holding agent1 jammer1)
    (lethal gun1)
    (not (jamming jammer1 gun1))
    (not (safe watched))
    (not (member 'watched (walkable-locations agent1 goal)))

    ;; The real action precondition accepts the allowed placement and rejects the two
    ;; distinct negative cases: policy-disallowed and closed-gate-blocked.
    (jam-target-applicable-p state 'agent1 'gun1 'start)
    (visible disallowed-site gun2)
    (jam-disallowed> goal disallowed-site gun2)
    (not (jam-target-applicable-p state 'agent1 'gun2 'disallowed-site))
    (potentially-visible blocked-site gun3)
    (not (visible blocked-site gun3))
    (not (jam-target-applicable-p state 'agent1 'gun3 'blocked-site))

    ;; Normal control follows plate state; inverted control negates it.
    (not (depressed plate1))
    (not (lethal gun4))
    (safe normal-clear-zone)
    (depressed plate2)
    (lethal gun5)
    (not (safe normal-down-zone))
    (not (depressed plate3))
    (lethal gun6)
    (not (safe inverted-clear-zone))
    (depressed plate4)
    (not (lethal gun7))
    (safe inverted-down-zone)

    ;; One jammed threat does not make a shared zone safe while another gun is live.
    (jamming jammer2 gun2)
    (not (lethal gun2))
    (lethal gun3)
    (not (safe shared-zone))))


(define-goal
  (gun-scenarios-valid))
