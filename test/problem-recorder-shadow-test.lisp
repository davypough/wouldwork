;;; Filename: problem-recorder-shadow-test.lisp

;;; Recorder Stage 3 characterization.  Eight phase-gated physical occupancy transitions
;;; exercise one toggle plate and its wall fan in both environmental views.  A parallel
;;; pressure plate with mapped box weights verifies ghost-only pressure output:
;;;
;;;   - live occupancy first turns on playback only, so a live box is swept while its
;;;     ghost copy remains and only the live actor sees the stream as impassable;
;;;   - additional live weight and both live releases do not retrigger the toggle;
;;;   - the first ghost occupancy then turns playback off but recording on, so a newly
;;;     introduced live box remains while the ghost box is swept and only the ghost sees
;;;     the stream as impassable;
;;;   - additional ghost weight and both ghost releases do not retrigger either view.
;;;
;;; Expected minimum path length: 8.

(in-package :ww)


(ww-set *problem-name* recorder-shadow-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 8)

(setf *expected-min-length* 8)


;;;; TYPES ;;;;


(define-types
  agent (live-first ghost-first live-second ghost-second)
  box (live-swept ghost-swept live-later ghost-later
       live-pressure ghost-pressure)
  recorder (recorder1)
  pressure-plate (pressure-control)
  toggle-plate (control-plate)
  wall-gears (wgears1 pressure-gears)
  fan (fan1)
  location (plate-site swept destination storage)
  test-phase (phase0 phase1 phase2 phase3 phase4 phase5 phase6 phase7 phase8))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech recorder)
(include-tech plate)
(include-tech wall-blower)


;;;; TEST STATE ;;;;


(define-dynamic-relations
  (current-phase test-phase))


(define-init
  (recording-copy> live-first ghost-first)
  (recording-copy> live-second ghost-second)
  (recording-copy> live-swept ghost-swept)
  (recording-copy> live-later ghost-later)
  (recording-copy> live-pressure ghost-pressure)

  (has-location live-first plate-site)
  (has-location ghost-first plate-site)
  (has-location live-second plate-site)
  (has-location ghost-second plate-site)
  (has-location live-swept swept)
  (has-location ghost-swept swept)
  (has-location live-later storage)
  (has-location ghost-later storage)
  (has-location live-pressure plate-site)
  (has-location ghost-pressure plate-site)

  (has-position recorder1 plate-site)
  (has-position control-plate plate-site)
  (has-position pressure-control plate-site)
  (has-position wgears1 swept)
  (has-position pressure-gears swept)
  (mounted-on fan1 wgears1)
  (welded fan1 wgears1)
  (controls ((control-plate)) wgears1 normal)
  (controls ((pressure-control)) pressure-gears normal)
  (aimed-at wgears1 destination)
  (aimed-at pressure-gears destination)

  (current-phase phase0))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; PHASE-GATED OCCUPANCY TRANSITIONS ;;;;


(define-action press-live-first
  1
  ()
  (and (current-phase phase0)
       (not (depressed control-plate))
       (not (recording-depressed control-plate)))
  ("> live-first adds the first playback-only plate weight")
  (assert (on live-first control-plate)
          (on live-pressure pressure-control)
          (not (current-phase phase0))
          (current-phase phase1)
          (finally (propagate-changes!))))


(define-action add-live-second
  1
  ()
  (and (current-phase phase1)
       (latched control-plate)
       (not (recording-latched control-plate))
       (turning wgears1)
       (not (recording-turning wgears1))
       (depressed pressure-control)
       (not (recording-depressed pressure-control))
       (turning pressure-gears)
       (not (recording-turning pressure-gears))
       (has-location live-swept destination)
       (has-location ghost-swept swept)
       (not (obstacle-clear live-first wgears1))
       (obstacle-clear ghost-first wgears1))
  ("> live-second adds weight without a second playback toggle")
  (assert (on live-second control-plate)
          (not (current-phase phase1))
          (current-phase phase2)
          (finally (propagate-changes!))))


(define-action remove-live-first
  1
  ()
  (and (current-phase phase2)
       (depressed control-plate)
       (latched control-plate)
       (on live-second control-plate))
  ("> live-first leaves while the playback plate remains depressed")
  (assert (not (on live-first control-plate))
          (not (current-phase phase2))
          (current-phase phase3)
          (finally (propagate-changes!))))


(define-action remove-live-second
  1
  ()
  (and (current-phase phase3)
       (depressed control-plate)
       (latched control-plate)
       (on live-second control-plate))
  ("> live-second clears the plate without changing its latch")
  (assert (not (on live-second control-plate))
          (not (current-phase phase3))
          (current-phase phase4)
          (finally (propagate-changes!))))


(define-action press-ghost-first
  1
  ()
  (and (current-phase phase4)
       (not (depressed control-plate))
       (latched control-plate)
       (not (recording-depressed control-plate))
       (not (recording-latched control-plate)))
  ("> ghost-first adds the first recording-side plate weight")
  (assert (has-location live-later swept)
          (not (on live-pressure pressure-control))
          (on ghost-pressure pressure-control)
          (on ghost-first control-plate)
          (not (current-phase phase4))
          (current-phase phase5)
          (finally (propagate-changes!))))


(define-action add-ghost-second
  1
  ()
  (and (current-phase phase5)
       (not (latched control-plate))
       (recording-latched control-plate)
       (not (turning wgears1))
       (recording-turning wgears1)
       (depressed pressure-control)
       (recording-depressed pressure-control)
       (turning pressure-gears)
       (recording-turning pressure-gears)
       (has-location live-later swept)
       (has-location ghost-swept destination)
       (obstacle-clear live-first wgears1)
       (not (obstacle-clear ghost-first wgears1)))
  ("> ghost-second adds weight without retriggering either view")
  (assert (on ghost-second control-plate)
          (not (current-phase phase5))
          (current-phase phase6)
          (finally (propagate-changes!))))


(define-action remove-ghost-first
  1
  ()
  (and (current-phase phase6)
       (depressed control-plate)
       (recording-depressed control-plate)
       (not (latched control-plate))
       (recording-latched control-plate))
  ("> ghost-first leaves while both plate views remain depressed")
  (assert (not (on ghost-first control-plate))
          (not (current-phase phase6))
          (current-phase phase7)
          (finally (propagate-changes!))))


(define-action remove-ghost-second
  1
  ()
  (and (current-phase phase7)
       (depressed control-plate)
       (recording-depressed control-plate)
       (not (latched control-plate))
       (recording-latched control-plate))
  ("> ghost-second clears both views without changing either latch")
  (assert (not (on ghost-second control-plate))
          (not (on ghost-pressure pressure-control))
          (not (current-phase phase7))
          (current-phase phase8)
          (finally (propagate-changes!))))


;;;; GOAL ;;;;


(define-goal
  (and (current-phase phase8)
       (not (depressed control-plate))
       (not (recording-depressed control-plate))
       (not (latched control-plate))
       (recording-latched control-plate)
       (not (turning wgears1))
       (recording-turning wgears1)
       (not (depressed pressure-control))
       (not (recording-depressed pressure-control))
       (not (turning pressure-gears))
       (not (recording-turning pressure-gears))
       (has-location live-swept destination)
       (has-location ghost-swept destination)
       (has-location live-later swept)
       (obstacle-clear live-first wgears1)
       (not (obstacle-clear ghost-first wgears1))))
