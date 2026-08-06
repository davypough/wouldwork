;;; Filename: problem-step-test.lisp

;;;; Dedicated regression coverage for public step behavior.
;;;;
;;;; Three independent actions are required:
;;;;   1. BOARDING-AGENT steps from ground onto BOARDING-PLATE.
;;;;   2. LEAVING-AGENT steps off LEAVING-PLATE onto ground.
;;;;   3. FAN-AGENT steps onto a clear gears-mounted floor fan.
;;;;
;;;; A characterization query verifies both lifecycles and directly checks that step
;;;; transition generation rejects an occupied plate, an already-supported agent, a remote
;;;; plate, a loose fan, a wall-mounted fan without a location, a box as a mount target,
;;;; and an agent attempting to dismount a box.  The mounted fan's normal control remains
;;;; clear, keeping its gears stopped so the fan occupancy can be observed directly.
;;;;
;;;; Expected minimum path length: 3.

(in-package :ww)

(ww-set *problem-name* step-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


;;;; TYPES ;;;;


(define-types
  agent (boarding-agent leaving-agent fan-agent occupied-agent supported-agent
         loose-agent wall-agent box-agent)
  location (boarding-site leaving-site fan-site control-site occupied-site
            supported-site loose-site remote-site wall-site box-site)
  pressure-plate (boarding-plate leaving-plate fan-control-plate occupied-plate
                  current-plate alternate-plate remote-plate)
  box (plate-blocker nonsteppable-box box-support)
  floor-gears (floor-gears1)
  wall-gears (wall-gears1)
  fan (floor-fan loose-fan wall-fan))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech -gears-fan)
(include-tech step)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Required plate-on lifecycle: a ground-level agent and a clear colocated plate.
  (has-location boarding-agent boarding-site)
  (has-position boarding-plate boarding-site)

  ;; Required plate-off lifecycle: initialization depresses the occupied plate.
  (has-location leaving-agent leaving-site)
  (has-position leaving-plate leaving-site)
  (on leaving-agent leaving-plate)

  ;; Required fan-on lifecycle.  The clear control plate keeps both mounted fans
  ;; stopped, and welding prevents unrelated pickup-fan branches.
  (has-location fan-agent fan-site)
  (has-position fan-control-plate control-site)
  (has-location floor-fan fan-site)
  (has-position floor-gears1 fan-site)
  (mounted-on floor-fan floor-gears1)
  (welded floor-fan floor-gears1)
  (controls ((fan-control-plate)) floor-gears1 normal)

  ;; Occupancy alone blocks an otherwise colocated plate step.
  (has-location occupied-agent occupied-site)
  (has-position occupied-plate occupied-site)
  (has-location plate-blocker occupied-site)
  (on plate-blocker occupied-plate)

  ;; Existing support blocks stepping onto another clear plate at the same location.
  (has-location supported-agent supported-site)
  (has-position current-plate supported-site)
  (has-position alternate-plate supported-site)
  (on supported-agent current-plate)

  ;; A loose fan and a box are both colocated and clear, but neither is a legal
  ;; step target.  REMOTE-PLATE separately checks exact location equality.
  (has-location loose-agent loose-site)
  (has-location loose-fan loose-site)
  (has-location nonsteppable-box loose-site)
  (has-position remote-plate remote-site)

  ;; A wall-mounted fan is attached but deliberately has no location.
  (has-location wall-agent wall-site)
  (has-position wall-gears1 wall-site)
  (mounted-on wall-fan wall-gears1)
  (welded wall-fan wall-gears1)
  (controls ((fan-control-plate)) wall-gears1 normal)

  ;; Stepping off applies only to steppables, not to a box top.
  (has-location box-agent box-site)
  (has-location box-support box-site)
  (on box-agent box-support))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; TRANSITION CHARACTERIZATION ;;;;


(define-test-helper step-transition-available-p (state agent transition)
  "Whether the central configuration action offers TRANSITION for AGENT in STATE."
  (member transition
          (configuration-transition-results state agent)
          :test #'equal))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query step-scenarios-valid ()
  (and
    ;; A step transition establishes occupancy without moving the agent and propagates plate
    ;; depression.
    (has-location boarding-agent boarding-site)
    (on boarding-agent boarding-plate)
    (not (cleartop boarding-plate))
    (depressed boarding-plate)

    ;; Dismounting removes the only support fact, leaves location unchanged, and
    ;; propagates the plate back to clear and undepressed.
    (has-location leaving-agent leaving-site)
    (not (exists (?support support)
           (on leaving-agent ?support)))
    (cleartop leaving-plate)
    (not (depressed leaving-plate))
    (step-transition-available-p
      state 'leaving-agent
      '(step (leaving-site ground) nil
             (leaving-site leaving-plate)))

    ;; A mounted floor fan is steppable while a clear control keeps it stopped.
    (has-location fan-agent fan-site)
    (has-location floor-fan fan-site)
    (mounted-on floor-fan floor-gears1)
    (welded floor-fan floor-gears1)
    (on fan-agent floor-fan)
    (not (cleartop floor-fan))
    (not (depressed fan-control-plate))
    (not (turning floor-gears1))
    (not (blowing floor-fan))
    (step-transition-available-p
      state 'fan-agent
      '(step (fan-site floor-fan) nil (fan-site ground)))

    ;; An occupied plate is geometrically eligible but fails CLEARTOP.
    (has-location occupied-agent occupied-site)
    (on plate-blocker occupied-plate)
    (not (cleartop occupied-plate))
    (depressed occupied-plate)
    (not (step-transition-available-p
           state 'occupied-agent
           '(step (occupied-site ground) nil
                  (occupied-site occupied-plate))))

    ;; An agent already on a plate cannot transfer directly to another clear plate.
    (has-location supported-agent supported-site)
    (on supported-agent current-plate)
    (not (cleartop current-plate))
    (depressed current-plate)
    (cleartop alternate-plate)
    (not (depressed alternate-plate))
    (not (step-transition-available-p
           state 'supported-agent
           '(step (supported-site current-plate) nil
                  (supported-site alternate-plate))))

    ;; Mounting requires exact colocation.
    (has-location loose-agent loose-site)
    (cleartop remote-plate)
    (not (step-transition-available-p
           state 'loose-agent
           '(step (loose-site ground) nil
                  (loose-site remote-plate))))

    ;; A loose fan has a location and a clear top but lacks a gears attachment.
    (has-location loose-fan loose-site)
    (not (exists (?gears gears)
           (mounted-on loose-fan ?gears)))
    (cleartop loose-fan)
    (not (step-transition-available-p
           state 'loose-agent
           '(step (loose-site ground) nil
                  (loose-site loose-fan))))

    ;; A box is a support but not a steppable fixture.
    (has-location nonsteppable-box loose-site)
    (cleartop nonsteppable-box)
    (not (step-transition-available-p
           state 'loose-agent
           '(step (loose-site ground) nil
                  (loose-site nonsteppable-box))))

    ;; A wall-mounted fan has an attachment but no floor location to match.
    (has-location wall-agent wall-site)
    (mounted-on wall-fan wall-gears1)
    (welded wall-fan wall-gears1)
    (not (exists (?location location)
           (has-location wall-fan ?location)))
    (cleartop wall-fan)
    (not (turning wall-gears1))
    (not (blowing wall-fan))
    (not (step-transition-available-p
           state 'wall-agent
           '(step (wall-site ground) nil
                  (wall-site wall-fan))))

    ;; Step dismount deliberately excludes box tops; jump owns that transition.
    (has-location box-agent box-site)
    (on box-agent box-support)
    (not (step-transition-available-p
           state 'box-agent
           '(step (box-site box-support) nil (box-site ground))))

    ;; The old action pair is gone; one central action owns support mutation.
    (find 'change-configuration *actions* :key #'action.name)
    (not (find 'step-on *actions* :key #'action.name))
    (not (find 'step-off *actions* :key #'action.name))))


(define-goal
  (step-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation step-mount-allows-supported-agent step-source-can-mount
  (?source-place)
  (not nil)
  "Drops the step provider's ground-only mount guard.  The supported-agent probe must then make
   this characterization fail.")


(define-query-mutation step-mount-ignores-location steppable-fixture-at
  (?fixture steppable-object ?location location)
  (do ?location
      (or (plate ?fixture)
          (and (fan ?fixture)
               (bind (mounted-on ?fixture $gears))
               (bind (has-location ?fixture $fixture-location)))))
  "Drops the step provider's exact-colocation check.  The remote-plate probe must then
   make this characterization fail.")


(define-query-mutation step-dismount-allows-any-support step-source-can-dismount
  (?source-place)
  (not (eql ?source-place 'ground))
  "Drops the step provider's steppable-support guard.  The box-support probe must then make
   this characterization fail.")
