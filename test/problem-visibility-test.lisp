;;; Filename: problem-visibility-test.lisp

;;; Visibility regression characterization.
;;;
;;; The independent scenarios below verify direct, gated, target-gate,
;;; location-to-location, beam-occlusion, and disconnected visibility.
;;; The goal also asserts important absent visibility facts and the exact
;;; inclusive beam-blocker boundary.
;;;
;;; This is a zero-action characterization problem.  The initial and final
;;; states are identical, and the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* visibility-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (idle-agent)
  location (idle
            apparatus-direct-site
            apparatus-open-site
            apparatus-mixed-site
            target-direct-site
            target-blocked-site
            clear-left
            clear-right
            open-left
            open-right
            blocked-left
            blocked-right
            beam-source
            beam-mid
            disconnected-left
            disconnected-right)
  receiver (direct-receiver
            open-receiver
            mixed-receiver
            beam-receiver)
  gate (open-gate1
        open-gate2
        closed-gate
        direct-target-gate
        blocked-target-gate)
  box (beam-blocker))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location idle-agent idle)
  (has-location beam-blocker beam-mid)
  (has-height beam-blocker 1)

  (open open-gate1)
  (open open-gate2)

  (los-to-apparatus apparatus-direct-site () direct-receiver)
  (los-to-apparatus
    apparatus-open-site
    (open-gate1 open-gate2)
    open-receiver)
  (los-to-apparatus
    apparatus-mixed-site
    (open-gate1 closed-gate)
    mixed-receiver)

  (los-to-target target-direct-site () direct-target-gate)
  (los-to-target
    target-blocked-site
    (closed-gate)
    blocked-target-gate)

  (los-to-location clear-left () clear-right)
  (los-to-location open-left (open-gate1) open-right)
  (los-to-location blocked-left (closed-gate) blocked-right)

  (los-to-apparatus beam-source (beam-mid) beam-receiver)
  (location-coords> beam-source 0 0)
  (location-coords> beam-mid 5 0)
  (apparatus-coords> beam-receiver 10 0))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query visibility-scenarios-valid ()
  (and
    (open open-gate1)
    (open open-gate2)
    (not (open closed-gate))
    (not (open direct-target-gate))
    (not (open blocked-target-gate))
    (visible-clear open-gate1)
    (visible-clear open-gate2)
    (not (visible-clear closed-gate))

    (potentially-visible apparatus-direct-site direct-receiver)
    (visible apparatus-direct-site direct-receiver)

    (potentially-visible apparatus-open-site open-receiver)
    (visible apparatus-open-site open-receiver)
    (beam-visible apparatus-open-site 1 open-receiver 1)

    (potentially-visible apparatus-mixed-site mixed-receiver)
    (not (visible apparatus-mixed-site mixed-receiver))
    (not (beam-visible apparatus-mixed-site 1 mixed-receiver 1))

    (potentially-visible target-direct-site direct-target-gate)
    (visible target-direct-site direct-target-gate)

    (potentially-visible target-blocked-site blocked-target-gate)
    (not (visible target-blocked-site blocked-target-gate))

    (potentially-visible clear-left clear-right)
    (potentially-visible clear-right clear-left)
    (visible clear-left clear-right)
    (visible clear-right clear-left)

    (potentially-visible open-left open-right)
    (potentially-visible open-right open-left)
    (visible open-left open-right)
    (visible open-right open-left)

    (potentially-visible blocked-left blocked-right)
    (potentially-visible blocked-right blocked-left)
    (not (visible blocked-left blocked-right))
    (not (visible blocked-right blocked-left))

    (potentially-visible beam-source beam-receiver)
    (visible beam-source beam-receiver)
    (has-location beam-blocker beam-mid)
    (has-height beam-blocker 1)
    (beam-blocker-spans-elevation beam-blocker 1)
    (not (beam-blocker-spans-elevation beam-blocker 2))
    (beam-blocker-occludes-location beam-mid 1)
    (not (beam-blocker-occludes-location beam-mid 2))
    (= (beam-elevation-at-location
         beam-mid beam-source 1 beam-receiver 1)
       1)
    (= (beam-elevation-at-location
         beam-mid beam-source 2 beam-receiver 2)
       2)
    (not (beam-visible beam-source 1 beam-receiver 1))
    (beam-visible beam-source 2 beam-receiver 2)

    (not (potentially-visible disconnected-left disconnected-right))
    (not (potentially-visible disconnected-right disconnected-left))
    (not (visible disconnected-left disconnected-right))
    (not (visible disconnected-right disconnected-left))))


(define-goal
  (visibility-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation visible-clear-ignores-gate-state visible-clear
  (?occluder gate)
  (gate ?occluder)
  "Drops VISIBLE-CLEAR's open-state check.  The closed-gate visibility probes
   must then make this characterization fail.")
