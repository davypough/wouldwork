;;; Filename: problem-recorder-occupancy-test.lisp

;;; Dedicated regression for superimposed live/ghost support occupancy.  A support top
;;; holds at most one occupant per recorder layer, not at most one occupant, and the case
;;; that proves it is the one START-RECORDER creates by itself: a live box resting on a
;;; plate both layers share forks a ghost box onto that same plate.  Before this was
;;; modeled, ON's bijective reverse index silently evicted the live box at the moment
;;; recording began, dropping it off the plate with no error anywhere.
;;;
;;; One action -- START-RECORDER -- is enough, since the fork is the whole mechanism.  The
;;; goal then pins all four consequences:
;;;
;;;   - both ON facts survive together, so the plate carries one occupant per layer;
;;;   - SUPPORT-OCCUPIED and the plate's own DEPRESSED stay layer-blind, since ghost
;;;     objects have weight (rule 15);
;;;   - CLEARTOP admits the live box despite the ghost superimposed on it, and admits
;;;     neither a second live occupant nor a second ghost one;
;;;   - the ghost, which had no physical state at all beforehand, has exactly the live
;;;     box's placement afterwards.
;;;
;;; The private components are included rather than the public RECORDER assembly, so this
;;; characterizes the fork without also installing the solution-path policy or the
;;; supported-scope init checks, both of which other recorder characterizations cover.
;;; Expected minimum path length: one.

(in-package :ww)


(ww-set *problem-name* recorder-occupancy-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  pressure-plate (shared-plate bare-plate)
  recorder (recorder1)
  location (site))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech -recorder-session)
(include-tech plate)


;;;; INITIALIZATION ;;;;


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)

  (has-position recorder1 site)
  (has-position shared-plate site)
  (has-position bare-plate site)

  (has-location live-agent site)
  (has-location live-box site)

  ;; The live box rests on a plate the two layers share.  Its ghost has no physical state
  ;; of any kind until START-RECORDER forks it.
  (on live-box shared-plate))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; INIT VALIDATION CHARACTERIZATION ;;;;


(define-test-claim recorder-occupancy-init-validation
  ;; The authored-placement check follows the same rule the runtime hook does: a live
  ;; object and its own recorder ghost may be placed on one support, two occupants of the
  ;; same layer may not.
  (null
    (validate-init-literals
      '((recording-copy> live-box ghost-box)
        (has-position shared-plate site)
        (has-location live-box site)
        (has-location ghost-box site)
        (on live-box shared-plate)
        (on ghost-box shared-plate))
      :checks '(physical-state-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-box ghost-box)
          (has-position shared-plate site)
          (has-location live-box site)
          (has-location live-agent site)
          (on live-box shared-plate)
          (on live-agent shared-plate))
        :checks '(physical-state-init-check)))
    'init-check-failure
    :containing "contending objects on the same support"
    :check 'physical-state-init-check))


;;;; CHARACTERIZATION QUERIES AND GOAL ;;;;
;;;; One named query per theme, so a regression narrows to a few clauses and each theme can
;;;; be run on its own at the repl, eg (funcall 'forked-occupancy-valid *start-state*).


(define-query forked-occupancy-valid ()
  ;; Both layers rest on the one shared plate, and the ghost inherited exactly the live
  ;; box's placement rather than replacing it.
  (and (recording-in-progress)
       (on live-box shared-plate)
       (on ghost-box shared-plate)
       (has-location ghost-box site)
       (not (on live-box bare-plate))
       (not (on ghost-box bare-plate))))


(define-query occupancy-physics-layer-blind ()
  ;; Weight does not care which layer it belongs to.  The shared plate reads occupied and
  ;; stays depressed; the untouched plate reads neither.
  (and (support-occupied shared-plate)
       (depressed shared-plate)
       (not (support-occupied bare-plate))
       (not (depressed bare-plate))))


(define-query occupancy-clearance-layer-aware ()
  ;; CLEARTOP is the manipulation gate, and it is relative to the occupant asking.  The
  ;; live box is not blocked by the ghost superimposed on it, nor by itself; a second
  ;; occupant of either layer is blocked by its own layer's box.  An empty plate is clear
  ;; for everyone.
  (and (cleartop shared-plate live-box)
       (cleartop shared-plate ghost-box)
       (not (cleartop shared-plate live-agent))
       (not (cleartop shared-plate ghost-agent))
       (cleartop bare-plate live-agent)
       (cleartop bare-plate ghost-agent)))


(define-goal
  (and (forked-occupancy-valid)
       (occupancy-physics-layer-blind)
       (occupancy-clearance-layer-aware)))
