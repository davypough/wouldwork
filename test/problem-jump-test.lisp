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
;;;   4. A height-2 agent carries a box through a stairs-then-jump mobility route in one
;;;      MOVE.  The stairs raise the hypothetical jump source from elevation 0 to 2.  The
;;;      jump then reaches elevation 4 and clears two barriers whose tops are also 4.
;;;   5. A grounded agent jumps directly onto a remote clear box, exercising the remaining
;;;      ground-to-support configuration boundary.
;;;
;;; Independent stationary probes characterize the public clearance queries and inspect
;;; MOVE's and CHANGE-CONFIGURATION's real generated children.  They verify inclusive and
;;; just-over elevation
;;; boundaries, downward freedom, barrier defaults and explicit overrides, highest-feature
;;; selection, empty-handed screen passability, directed-edge asymmetry, rejection of an
;;; unsafe landing, rejection of an occupied box top while preserving its ground landing,
;;; rejection of an over-height local box mount, exclusive ownership of grounded jumps
;;; by mobility rather than the configuration-transition substrate, and rejection of an
;;; edge instance from a JUMP-VIA feature list -- VAULTABLE-OBJECT deliberately excludes
;;; edge, since an edge has no independent "top" to vault onto.
;;;
;;; Expected minimum solution (6 steps, in any interleaving): mount vault-box; cross
;;; vault-start -> vault-goal; drop from drop-box; cross transfer-start -> transfer-goal
;;; directly onto transfer-target-box; move carry-approach -> carry-goal through stairs and
;;; jump segments while holding carried-box; jump from remote-mount-start directly onto
;;; remote-target-box.


(in-package :ww)


(ww-set *problem-name* jump-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 6)

(setf *expected-min-length* 6)


;;;; TYPES ;;;;


(define-types
  agent (vault-agent drop-agent transfer-agent carrying-agent
         boundary-agent screen-probe-agent unsafe-probe-agent
         occupied-probe-agent tall-box-probe-agent remote-mount-agent)
  location (vault-start vault-goal drop-site
            transfer-start transfer-goal carry-approach carry-start carry-goal
            boundary-site screen-probe-start screen-probe-goal
            unsafe-start unsafe-goal occupied-start occupied-goal
            tall-box-site remote-mount-start remote-mount-goal)
  box (vault-box drop-box transfer-source-box transfer-target-box carried-box
       transfer-base-box boundary-box occupied-target-box tall-local-box
       remote-target-box)
  connector (blocking-connector)
  gate (default-gate)
  screen (cargo-screen passable-screen)
  wall (vault-wall default-wall)
  edge (probe-edge)
  gun (unsafe-gun))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech jump)
(include-tech stairs)
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
  (has-height transfer-target-box 3)
  (has-location transfer-base-box transfer-goal)
  (on transfer-target-box transfer-base-box)
  (jump-via transfer-goal () transfer-start)

  ;; Planned lane 4: one mobility route first climbs stairs from elevation 0 to 2, then
  ;; jumps to elevation 4.  Both non-passable barriers have top elevation 4, exactly within
  ;; carrying-agent's height from the hypothetical intermediate source.  Computing the jump
  ;; from carrying-agent's actual pre-move elevation 0 would incorrectly reject this route.
  (has-location carrying-agent carry-approach)
  (has-height carrying-agent 2)
  (holding carrying-agent carried-box)
  (has-elevation carry-start 2)
  (has-elevation carry-goal 4)
  (stairs-via> carry-approach () carry-start)
  (has-elevation cargo-screen 2)
  (has-height cargo-screen 2)
  (jump-via> carry-start (vault-wall cargo-screen) carry-goal)

  ;; Exact-boundary query probe: standing elevation 2 plus agent height 2 reaches 4, but
  ;; not 5.
  (has-location boundary-agent boundary-site)
  (has-height boundary-agent 2)
  (has-location boundary-box boundary-site)
  (has-height boundary-box 2)
  (on boundary-agent boundary-box)

  ;; An empty-handed agent ignores a default-height screen even though height 3 exceeds its
  ;; own height 1.  This probe remains stationary; the goal inspects the generated child.
  (has-location screen-probe-agent screen-probe-start)
  (has-height screen-probe-agent 1)
  (jump-via> screen-probe-start (passable-screen) screen-probe-goal)

  ;; An uncontrolled gun is lethal after initialization, so the transition action must
  ;; produce no child
  ;; at its threatened destination.
  (has-location unsafe-probe-agent unsafe-start)
  (jump-via> unsafe-start () unsafe-goal)
  (threatens unsafe-gun unsafe-goal)

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
  (has-height tall-local-box 3)

  ;; A grounded remote support landing remains explicit and occupies the target box.  Two
  ;; authored edges offer the same destination through different witnesses; central
  ;; canonicalization must retain only the lexical first.
  (has-location remote-mount-agent remote-mount-start)
  (has-location remote-target-box remote-mount-goal)
  (jump-via remote-mount-start () remote-mount-goal)
  (jump-via> remote-mount-start (passable-screen) remote-mount-goal))


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
  "Characterize positive and negative grounded and support-changing MOVE successors."
  (let ((move-action (find 'move *actions* :key #'action.name))
        (saved-dropped-count *inconsistent-states-dropped*))
    (unwind-protect
      (let ((move-children
              (let ((*actions* (list move-action)))
                (generate-children
                  (make-node :state state :depth 0)))))
        (and
          ;; Empty-handed passability ignores passable-screen's default height 3.
          (some (lambda (child)
                  (jump-child-matches-p
                    child
                    '((has-location screen-probe-agent screen-probe-goal))
                    nil))
                move-children)

          ;; A lethal destination cannot be produced by the real mobility action.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location unsafe-probe-agent unsafe-goal))
                      nil))
                  move-children)

          ;; The occupied-box lane retains its legal grounded child, but cannot land on the
          ;; occupied box top.
          (some (lambda (child)
                  (jump-child-matches-p
                    child
                    '((has-location occupied-probe-agent occupied-goal))
                    '((on occupied-probe-agent occupied-target-box))))
                move-children)
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location occupied-probe-agent occupied-goal)
                        (on occupied-probe-agent occupied-target-box))
                      nil))
                  move-children)

          ;; The clear but over-height local box cannot be mounted.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((on tall-box-probe-agent tall-local-box))
                      nil))
                  move-children)

          ;; vault-start -> vault-goal is directed and cannot be traversed backward by MOVE.
          (notany (lambda (child)
                    (jump-child-matches-p
                      child
                      '((has-location vault-agent vault-start))
                      nil))
                  move-children)))
      (setf *inconsistent-states-dropped* saved-dropped-count))))


(define-test-claim jump-configuration-canonicalization-contract
  (equal
    (configuration-transition-results
      *start-state* 'remote-mount-agent)
    '((jump (remote-mount-start ground) (passable-screen)
            (remote-mount-goal remote-target-box)))))


(define-test-claim jump-vaultable-object-excludes-edge
  ;; A genuine wall passes JUMP-INIT-CHECK's feature-list type check unchanged.
  (null
    (validate-init-literals
      '((jump-via vault-start (default-wall) vault-goal))
      :checks '(jump-init-check)))
  ;; An edge instance in that same list position is rejected: VAULTABLE-OBJECT is
  ;; (either gate screen wall), and edge is deliberately not a member -- it has no
  ;; independent "top" a jump could vault onto.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((jump-via vault-start (probe-edge) vault-goal))
        :checks '(jump-init-check)))
    'init-check-failure
    :containing "expected an instance of one of"
    :check 'jump-init-check))


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
    (on transfer-target-box transfer-base-box)
    (= (support-top-elevation transfer-target-box) 4)
    (jump-via transfer-start () transfer-goal)
    (jump-via transfer-goal () transfer-start)

    ;; Planned lane 4 retained cargo after one composed stairs/jump MOVE.  The canonical
    ;; route proves that the jump provider used the intermediate floor elevation 2 rather
    ;; than carrying-agent's actual elevation before the action.
    (has-location carrying-agent carry-goal)
    (holding carrying-agent carried-box)
    (not (exists (?location location)
           (has-location carried-box ?location)))
    (not (vaultable-object-passable carrying-agent cargo-screen))
    (= (jump-required-clearance-height carrying-agent '(cargo-screen)) 4)
    (jump-path-clear carrying-agent 2 '(cargo-screen))
    (equal
      (assoc 'carry-goal
             (mobility-results carrying-agent carry-approach))
      '(carry-goal
         ((stairs carry-approach nil carry-start)
          (jump carry-start (cargo-screen vault-wall) carry-goal))))

    ;; Inclusive upward boundary, just-over rejection, unrestricted downward movement, and
    ;; explicit hypothetical-source behavior independent of the agent's actual support.
    (= (occupant-elevation boundary-agent) 2)
    (jump-elevation-reachable boundary-agent 2 4)
    (not (jump-elevation-reachable boundary-agent 2 5))
    (jump-elevation-reachable boundary-agent 2 -100)
    (jump-path-clear boundary-agent 2 '(vault-wall))
    (not (jump-path-clear boundary-agent 0 '(vault-wall)))
    (jump-path-clear tall-box-probe-agent 2 '(vault-wall))
    (not (jump-path-clear tall-box-probe-agent 0 '(vault-wall)))

    ;; Provider directionality and symmetric reverse traversal are preserved independently
    ;; of whether the agent is currently grounded and eligible to execute MOVE.
    (traversable transfer-agent transfer-start transfer-goal)
    (not (traversable vault-agent vault-goal vault-start))

    ;; Ground-to-remote-support is one explicit jump configuration transition.  Its
    ;; duplicate authored destination was checked against the initial state above.
    (has-location remote-mount-agent remote-mount-goal)
    (on remote-mount-agent remote-target-box)
    (not (cleartop remote-target-box))

    ;; Barrier default, explicit override, top elevation, feature typing, and maximum
    ;; non-passable height.  The passable screen contributes nothing to the mixed list.
    (= (jump-barrier-height default-gate) 3)
    (= (jump-barrier-height passable-screen) 3)
    (= (jump-barrier-height default-wall) 3)
    (= (jump-barrier-height vault-wall) 2)
    (= (jump-barrier-top-elevation vault-wall) 4)
    (vaultable-object-list '(passable-screen default-wall))
    (vaultable-object-passable screen-probe-agent passable-screen)
    (not (vaultable-object-passable screen-probe-agent default-gate))
    (not (vaultable-object-passable screen-probe-agent default-wall))
    (not (jump-required-clearance-height
           screen-probe-agent '(passable-screen)))
    (= (jump-required-clearance-height
         screen-probe-agent '(passable-screen default-wall))
       3)

    ;; Threat state and occupied-top setup remain present for the generated-child probes.
    (lethal unsafe-gun)
    (not (safe unsafe-goal))
    (has-location unsafe-probe-agent unsafe-start)
    (has-location occupied-probe-agent occupied-start)
    (on blocking-connector occupied-target-box)
    (not (cleartop occupied-target-box))
    (has-location tall-box-probe-agent tall-box-site)
    (cleartop tall-local-box)

    ;; One MOVE action owns both grounded routes and explicit support changes.
    (find 'move *actions* :key #'action.name)
    (not (find 'change-configuration *actions* :key #'action.name))
    (not (find 'jump-to *actions* :key #'action.name))

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
  (?agent agent ?source-elevation ?target-elevation)
  (not nil)
  "Drops the upward-height restriction.  The just-over-boundary probe must then
   make this characterization fail.")


(define-query-mutation jump-elevation-uses-actual-source jump-elevation-reachable
  (?agent agent ?source-elevation ?target-elevation)
  (<= (- ?target-elevation (occupant-elevation ?agent))
      (declared-height ?agent))
  "Ignores the explicit hypothetical source elevation.  The stairs-then-jump
   route must then fail from the agent's actual pre-move elevation.")


(define-query-mutation jump-path-uses-actual-source jump-path-clear
  (?agent agent ?source-elevation ?features)
  (and (vaultable-object-list ?features)
       (assign $required
               (jump-required-clearance-height ?agent ?features))
       (or (not $required)
           (<= (- $required (occupant-elevation ?agent))
               (declared-height ?agent))))
  "Ignores the explicit hypothetical source elevation for path clearance.  The
   elevated intermediate jump and the explicit-source probes must detect it.")
