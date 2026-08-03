;;; Filename: problem-jump-test.lisp

;;; Combined stageable regression for jump.lisp.  Four independent planning lanes exercise:
;;;
;;;   1. A height-2 agent must first mount a height-2 box, then clear a wall whose top is
;;;      exactly two units above the box top.  The directed jump lands on ground and removes
;;;      the launch-box support.
;;;   2. An agent already on a box drops to local ground without changing location.
;;;   3. An agent crosses a symmetric edge in the reverse authored direction and lands
;;;      directly from one height-4 box onto another.  A ground landing cannot later reach
;;;      the destination box, so the direct box-landing branch is required.
;;;   4. A height-2 agent carries a box across a height-2 screen.  Carrying makes the screen
;;;      non-passable, so this is an exact-boundary vault, and the cargo must remain held.
;;;
;;; Independent stationary probes characterize the public clearance queries and inspect
;;; JUMP-TO's real generated children.  They verify inclusive and just-over elevation
;;; boundaries, downward freedom, barrier defaults and explicit overrides, highest-feature
;;; selection, empty-handed screen passability, directed-edge asymmetry, rejection of an
;;; unsafe landing, rejection of an occupied box top while preserving its ground landing,
;;; and rejection of an over-height local box mount.
;;;
;;; Expected minimum solution (5 steps, in any interleaving): mount vault-box; cross
;;; vault-start -> vault-goal; drop from drop-box; cross transfer-start -> transfer-goal
;;; directly onto transfer-target-box; cross carry-start -> carry-goal while holding
;;; carried-box.


(in-package :ww)


(ww-set *problem-name* jump-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 5)

(setf *expected-min-length* 5)


;;;; TYPES ;;;;


(define-types
  agent (vault-agent drop-agent transfer-agent carrying-agent
         boundary-agent screen-probe-agent unsafe-probe-agent
         occupied-probe-agent tall-box-probe-agent)
  location (vault-start vault-goal drop-site
            transfer-start transfer-goal carry-start carry-goal
            boundary-site screen-probe-start screen-probe-goal
            unsafe-start unsafe-goal occupied-start occupied-goal
            tall-box-site)
  box (vault-box drop-box transfer-source-box transfer-target-box carried-box
       boundary-box occupied-target-box tall-local-box)
  connector (blocking-connector)
  gate (default-gate)
  screen (cargo-screen passable-screen)
  fence (default-fence)
  wall (vault-wall default-wall)
  gun (unsafe-gun))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech jump)
(include-tech gun)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Planned lane 1: ground cannot clear vault-wall's top elevation 4.  Vault-box raises
  ;; vault-agent from elevation 0 to 2, making the remaining clearance exactly its height 2.
  (has-location vault-agent vault-start)
  (has-height vault-agent 2)
  (has-location vault-box vault-start)
  (has-height vault-box 2)
  (has-elevation vault-wall 2)
  (has-height vault-wall 2)
  (jump-via> vault-start (vault-wall) vault-goal)

  ;; Planned lane 2: the only useful transition is the local box-to-ground drop.
  (has-location drop-agent drop-site)
  (has-location drop-box drop-site)
  (on drop-agent drop-box)

  ;; Planned lane 3: both box tops are elevation 4.  The edge is authored target-first so
  ;; the required source-to-target traversal depends on JUMP-VIA symmetry.
  (has-location transfer-agent transfer-start)
  (has-height transfer-agent 1)
  (has-location transfer-source-box transfer-start)
  (has-height transfer-source-box 4)
  (on transfer-agent transfer-source-box)
  (has-location transfer-target-box transfer-goal)
  (has-height transfer-target-box 4)
  (jump-via transfer-goal () transfer-start)

  ;; Planned lane 4: holding carried-box makes cargo-screen non-passable.  Its explicit
  ;; height 2 is nevertheless exactly within carrying-agent's vaulting clearance.
  (has-location carrying-agent carry-start)
  (has-height carrying-agent 2)
  (holding carrying-agent carried-box)
  (has-height cargo-screen 2)
  (jump-via> carry-start (cargo-screen) carry-goal)

  ;; Exact-boundary query probe: standing elevation 2 plus agent height 2 reaches 4, but
  ;; not 5.  default-fence also has top elevation 4 (base 2 + default height 2).
  (has-location boundary-agent boundary-site)
  (has-height boundary-agent 2)
  (has-location boundary-box boundary-site)
  (has-height boundary-box 2)
  (on boundary-agent boundary-box)
  (has-elevation default-fence 2)

  ;; An empty-handed agent ignores a default-height screen even though height 3 exceeds its
  ;; own height 1.  This probe remains stationary; the goal inspects the generated child.
  (has-location screen-probe-agent screen-probe-start)
  (has-height screen-probe-agent 1)
  (jump-via> screen-probe-start (passable-screen) screen-probe-goal)

  ;; An uncontrolled gun is lethal after initialization, so JUMP-TO must produce no child
  ;; at its threatened destination.
  (has-location unsafe-probe-agent unsafe-start)
  (jump-via> unsafe-start () unsafe-goal)
  (threatens unsafe-gun (unsafe-goal))

  ;; The destination box is occupied by a non-box support occupant.  The edge must still
  ;; produce a ground landing, but never a landing on occupied-target-box itself.
  (has-location occupied-probe-agent occupied-start)
  (has-location occupied-target-box occupied-goal)
  (has-location blocking-connector occupied-goal)
  (on blocking-connector occupied-target-box)
  (jump-via> occupied-start () occupied-goal)

  ;; A clear height-3 local box is one unit beyond this height-2 agent's reach.
  (has-location tall-box-probe-agent tall-box-site)
  (has-height tall-box-probe-agent 2)
  (has-location tall-local-box tall-box-site)
  (has-height tall-local-box 3))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; GENERATED-CHILD CHARACTERIZATION ;;;;


(define-test-helper jump-child-matches-p (child required-facts absent-facts)
  (let ((facts (database child)))
    (and (every (lambda (fact)
                  (member fact facts :test #'equal))
                required-facts)
         (notany (lambda (fact)
                   (member fact facts :test #'equal))
                 absent-facts))))


(define-test-helper jump-transition-scenarios-valid-p (state)
  "Characterize positive and negative JUMP-TO successors from STATE."
  (let ((action (find 'jump-to *actions* :key #'action.name))
        (saved-dropped-count *inconsistent-states-dropped*))
    (unwind-protect
      (let* ((*actions* (list action))
             (children
               (generate-children
                 (make-node :state state :depth 0))))
        (and
          ;; Empty-handed passability ignores passable-screen's default height 3.
          (some (lambda (child)
                  (jump-child-matches-p
                    child
                    '((has-location screen-probe-agent screen-probe-goal))
                    nil))
                children)

          ;; A lethal destination cannot be produced by the real jump action.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location unsafe-probe-agent unsafe-goal))
                      nil))
                  children)

          ;; The occupied-box lane retains its legal ground child but has no box-top child.
          (some (lambda (child)
                  (jump-child-matches-p
                    child
                    '((has-location occupied-probe-agent occupied-goal))
                    '((on occupied-probe-agent occupied-target-box))))
                children)
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location occupied-probe-agent occupied-goal)
                        (on occupied-probe-agent occupied-target-box))
                      nil))
                  children)

          ;; The clear but over-height local box cannot be mounted.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((on tall-box-probe-agent tall-local-box))
                      nil))
                  children)

          ;; vault-start -> vault-goal is directed and cannot be traversed backward.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location vault-agent vault-start))
                      nil))
                  children)))
      (setf *inconsistent-states-dropped* saved-dropped-count))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query jump-scenarios-valid ()
  (and
    ;; Planned lane 1 completed the mount/cross lifecycle and cleared prior support.
    (has-location vault-agent vault-goal)
    (not (on vault-agent vault-box))
    (has-location vault-box vault-start)
    (cleartop vault-box)
    (not (jump-via> vault-goal (vault-wall) vault-start))

    ;; Planned lane 2 dropped locally without relocating either participant.
    (has-location drop-agent drop-site)
    (has-location drop-box drop-site)
    (not (on drop-agent drop-box))
    (cleartop drop-box)

    ;; Planned lane 3 used the symmetric edge's reverse authored direction and transferred
    ;; support directly; the source top is now clear and the target top occupied.
    (has-location transfer-agent transfer-goal)
    (on transfer-agent transfer-target-box)
    (not (on transfer-agent transfer-source-box))
    (cleartop transfer-source-box)
    (not (cleartop transfer-target-box))
    (jump-via transfer-start () transfer-goal)
    (jump-via transfer-goal () transfer-start)

    ;; Planned lane 4 retained cargo, which continues to make its screen non-passable.
    (has-location carrying-agent carry-goal)
    (holding carrying-agent carried-box)
    (not (exists (?location location)
           (has-location carried-box ?location)))
    (not (jump-feature-passable carrying-agent cargo-screen))
    (= (jump-required-clearance-height carrying-agent '(cargo-screen)) 2)
    (jump-path-clear carrying-agent '(cargo-screen))

    ;; Inclusive upward boundary, just-over rejection, and unrestricted downward movement.
    (= (occupant-elevation boundary-agent) 2)
    (jump-elevation-reachable boundary-agent 4)
    (not (jump-elevation-reachable boundary-agent 5))
    (jump-elevation-reachable boundary-agent -100)
    (jump-path-clear boundary-agent '(vault-wall))
    (not (jump-path-clear tall-box-probe-agent '(vault-wall)))

    ;; Barrier defaults, explicit override, top elevation, feature typing, and maximum
    ;; non-passable height.  The passable screen contributes nothing to the mixed list.
    (= (jump-barrier-height default-fence) 2)
    (= (jump-barrier-height default-gate) 3)
    (= (jump-barrier-height passable-screen) 3)
    (= (jump-barrier-height default-wall) 3)
    (= (jump-barrier-height vault-wall) 2)
    (= (jump-barrier-top-elevation default-fence) 4)
    (= (jump-barrier-top-elevation vault-wall) 4)
    (jump-feature-list '(passable-screen default-wall default-fence))
    (jump-feature-passable screen-probe-agent passable-screen)
    (not (jump-feature-passable screen-probe-agent default-gate))
    (not (jump-feature-passable screen-probe-agent default-fence))
    (not (jump-feature-passable screen-probe-agent default-wall))
    (not (jump-required-clearance-height
           screen-probe-agent '(passable-screen)))
    (= (jump-required-clearance-height
         screen-probe-agent '(passable-screen default-wall default-fence))
       4)

    ;; Threat state and occupied-top setup remain present for the generated-child probes.
    (lethal unsafe-gun)
    (not (safe unsafe-goal))
    (has-location unsafe-probe-agent unsafe-start)
    (has-location occupied-probe-agent occupied-start)
    (on blocking-connector occupied-target-box)
    (not (cleartop occupied-target-box))
    (has-location tall-box-probe-agent tall-box-site)
    (cleartop tall-local-box)

    ;; Inspect the installed action rather than merely restating its branch conditions.
    (jump-transition-scenarios-valid-p state)))


(define-goal
  (jump-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation jump-safe-always-true safe
  (?location location)
  (not nil)
  "Disables destination safety.  The unsafe landing probe must then make this
   characterization fail.")


(define-query-mutation jump-unbounded-elevation jump-elevation-reachable
  (?agent agent ?target-elevation)
  (not nil)
  "Drops the upward-height restriction.  The just-over-boundary probe must then
   make this characterization fail.")
