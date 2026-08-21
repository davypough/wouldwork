;;; Filename: problem-jump-test.lisp

;;; Combined stageable regression for jump.lisp.  Four independent planning lanes exercise:
;;;
;;;   1. An agent mounts a box exactly at the fixed jump-elevation limit, then clears a wall
;;;      from the box top, itself exactly at the same fixed limit above that raised launch
;;;      point.  The directed jump lands on ground and removes the launch-box support.
;;;   2. An agent already on a box drops to local ground without changing location.
;;;   3. An agent crosses a symmetric edge in the reverse authored direction and lands
;;;      directly from one height-4 box onto another.  A ground landing cannot later reach
;;;      the destination box, so the direct box-landing branch is required.
;;;   4. An agent carries a box through a stairs-then-jump mobility route in one MOVE.  The
;;;      stairs raise the hypothetical jump source from elevation 0 to 2.  The jump then
;;;      rises exactly the fixed jump-elevation limit to elevation 3 and clears two barriers
;;;      from that same hypothetical source -- one comfortably under the limit, one exactly
;;;      at it.
;;;   5. A grounded agent jumps directly onto a remote clear box, exercising the remaining
;;;      ground-to-support configuration boundary.
;;;
;;; Independent stationary probes characterize the public clearance queries and inspect
;;; MOVE's and CHANGE-CONFIGURATION's real generated children.  They verify inclusive and
;;; just-over elevation boundaries -- JUMP-ELEVATION-REACHABLE's raised-landing bound and
;;; JUMP-PATH-CLEAR's vaulting bound share the same fixed *vertical-reach-limit*, and
;;; neither consults any agent's declared height -- downward freedom, barrier defaults and
;;; explicit overrides, highest-feature selection, empty-handed screen passability,
;;; directed-edge asymmetry, rejection of an unsafe landing, rejection of an occupied box
;;; top while preserving its ground landing, rejection of an over-limit local box mount,
;;; exclusive ownership of grounded jumps by mobility rather than the configuration-
;;; transition substrate, and rejection of an edge instance from a jumping TRAVERSE-VIA
;;; feature list --
;;; VAULTABLE-OBJECT deliberately excludes edge, since an edge has no independent "top" to
;;; vault onto.  The move-type tag itself is also characterized: a genuinely vaulting
;;; transition (lane 4) is tagged VAULT, while a non-empty but fully passable feature list
;;; (the remote-mount canonicalization contract below) still tags JUMP.
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
  ;; Planned lane 1: ground cannot clear vault-wall's top from source elevation 0 (required
  ;; clearance 2 exceeds the fixed jump-elevation limit 1).  Vault-box raises vault-agent
  ;; from elevation 0 to 1 -- exactly the fixed limit -- making the remaining vault
  ;; clearance exactly that same limit.  No agent height is asserted anywhere in this
  ;; file: neither JUMP-ELEVATION-REACHABLE nor JUMP-PATH-CLEAR consults it.
  (has-location vault-agent vault-start)
  (has-location vault-box vault-start)
  (has-height vault-box 1)
  (has-height vault-wall 2)
  (traverse-via> jumping vault-start ((vault-wall)) vault-goal)

  ;; Planned lane 2: the only useful transition is the local box-to-ground drop.
  (has-location drop-agent drop-site)
  (has-location drop-box drop-site)
  (on drop-agent drop-box)

  ;; Planned lane 3: both box tops are elevation 4, so the jump between them has zero
  ;; elevation gain and is unaffected by any upward-reach limit.  The edge is authored
  ;; target-first so the required source-to-target traversal depends on TRAVERSE-VIA
  ;; symmetry in jumping mode.
  (has-location transfer-agent transfer-start)
  (has-location transfer-source-box transfer-start)
  (has-height transfer-source-box 4)
  (on transfer-agent transfer-source-box)
  (has-location transfer-target-box transfer-goal)
  (has-height transfer-target-box 3)
  (has-location transfer-base-box transfer-goal)
  (on transfer-target-box transfer-base-box)
  (traverse-via jumping transfer-goal () transfer-start)

  ;; Planned lane 4: one mobility route first climbs stairs from elevation 0 to 2, then
  ;; jumps to elevation 3 -- exactly the fixed limit above that hypothetical source.  Both
  ;; non-passable barriers must clear within that same limit from source 2: VAULT-WALL's
  ;; top 2 is already under it, and CARGO-SCREEN's top 3 is the binding constraint, exactly
  ;; source plus the limit.  Computing either check from carrying-agent's actual pre-move
  ;; elevation 0 would incorrectly reject this route: the elevation gain would be 3, and
  ;; CARGO-SCREEN's required clearance would be 3, both exceeding the fixed limit 1.
  (has-location carrying-agent carry-approach)
  (holding carrying-agent carried-box)
  (has-elevation carry-start 2)
  (has-elevation carry-goal 3)
  (traverse-via> stairway carry-approach () carry-start)
  (has-elevation cargo-screen 1)
  (has-height cargo-screen 2)
  (traverse-via> jumping carry-start ((vault-wall cargo-screen)) carry-goal)

  ;; Exact-boundary query probes.  The fixed *vertical-reach-limit* governs both
  ;; JUMP-ELEVATION-REACHABLE and JUMP-PATH-CLEAR identically: source 2 reaches 3 but not
  ;; 4, and clears a barrier at top 2 but not one it would need to clear from ground level.
  (has-location boundary-agent boundary-site)
  (has-location boundary-box boundary-site)
  (has-height boundary-box 2)
  (on boundary-agent boundary-box)

  ;; An empty-handed agent ignores a default-height screen entirely: passability
  ;; short-circuits vaulting clearance before the fixed elevation limit is ever consulted.
  ;; This probe remains stationary; the goal inspects the generated child.
  (has-location screen-probe-agent screen-probe-start)
  (traverse-via> jumping screen-probe-start ((passable-screen)) screen-probe-goal)

  ;; An uncontrolled gun is lethal after initialization, so the transition action must
  ;; produce no child
  ;; at its threatened destination.
  (has-location unsafe-probe-agent unsafe-start)
  (traverse-via> jumping unsafe-start () unsafe-goal)
  (threatens unsafe-gun unsafe-goal)

  ;; The destination box is occupied by a non-box support occupant.  The edge must still
  ;; produce a ground landing, but never a landing on occupied-target-box itself.
  (has-location occupied-probe-agent occupied-start)
  (has-location occupied-target-box occupied-goal)
  (has-location blocking-connector occupied-goal)
  (on blocking-connector occupied-target-box)
  (traverse-via> jumping occupied-start () occupied-goal)

  ;; A clear height-2 local box is one unit beyond the fixed *vertical-reach-limit*; the
  ;; mount must fail regardless of any agent's height, since none is consulted.
  (has-location tall-box-probe-agent tall-box-site)
  (has-location tall-local-box tall-box-site)
  (has-height tall-local-box 2)

  ;; A grounded remote support landing remains explicit and occupies the target box.  Two
  ;; authored edges offer the same destination through different witnesses; central
  ;; canonicalization must retain only the lexical first.
  (has-location remote-mount-agent remote-mount-start)
  (has-location remote-target-box remote-mount-goal)
  (traverse-via jumping remote-mount-start () remote-mount-goal)
  (traverse-via> jumping remote-mount-start ((passable-screen)) remote-mount-goal))


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
          ;; Empty-handed passability ignores passable-screen's default height 4.
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

          ;; The clear but over-limit local box cannot be mounted.
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
  ;; PASSABLE-SCREEN never required clearance for this empty-handed agent, so the winning
  ;; candidate's move-type tag is JUMP despite its non-empty feature list -- the tag
  ;; reflects genuine clearance need, not mere feature-list presence.
  (equal
    (configuration-transition-results
      *start-state* 'remote-mount-agent)
    '((jump (remote-mount-start ground) (passable-screen)
            (remote-mount-goal remote-target-box)))))


(define-test-claim jump-vaultable-object-excludes-edge
  ;; A genuine wall passes TRAVERSAL-INIT-CHECK's per-mode clause type check unchanged.
  (null
    (validate-init-literals
      '((traverse-via jumping vault-start ((default-wall)) vault-goal))
      :checks '(traversal-init-check)))
  ;; An edge instance in that same list position is rejected: VAULTABLE-OBJECT is
  ;; (either gate screen wall), and edge is deliberately not a member -- it has no
  ;; independent "top" a jump could vault onto.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traverse-via jumping vault-start ((probe-edge)) vault-goal))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "expected an instance of one of"
    :check 'traversal-init-check))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query jump-scenarios-valid ()
  (and
    ;; Planned lane 1 completed the mount/cross lifecycle and cleared prior support.
    (has-location vault-agent vault-goal)
    (not (on vault-agent vault-box))
    (has-location vault-box vault-start)
    (cleartop vault-box)
    (not (traverse-via> jumping vault-goal ((vault-wall)) vault-start))

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
    (= (top transfer-target-box) 4)
    (traverse-via jumping transfer-start () transfer-goal)
    (traverse-via jumping transfer-goal () transfer-start)

    ;; Planned lane 4 retained cargo after one composed stairs/jump MOVE.  The canonical
    ;; route proves that the jump provider used the intermediate floor elevation 2 rather
    ;; than carrying-agent's actual elevation before the action.
    (has-location carrying-agent carry-goal)
    (holding carrying-agent carried-box)
    (not (exists (?location location)
           (has-location carried-box ?location)))
    (not (vaultable-object-passable carrying-agent cargo-screen))
    (= (jump-required-clearance-height carrying-agent '(cargo-screen)) 3)
    (jump-path-clear carrying-agent 2 '(cargo-screen))
    (equal
      (assoc 'carry-goal
             (mobility-results carrying-agent carry-approach))
      '(carry-goal
         ((stairs carry-approach nil carry-start)
          (vault carry-start (cargo-screen vault-wall) carry-goal))))

    ;; Inclusive upward boundary and just-over rejection for the fixed, agent-independent
    ;; elevation limit; unrestricted downward movement.
    (= (base boundary-agent) 2)
    (jump-elevation-reachable boundary-agent 2 3)
    (not (jump-elevation-reachable boundary-agent 2 4))
    (jump-elevation-reachable boundary-agent 2 -100)

    ;; JUMP-PATH-CLEAR's vaulting bound is the identical fixed limit, from an explicit
    ;; hypothetical source rather than any agent's own reach.
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
    (= (object-height default-gate) 4)
    (= (object-height passable-screen) 4)
    (= (object-height default-wall) 4)
    (= (object-height vault-wall) 2)
    (= (jump-barrier-top-elevation vault-wall) 2)
    (vaultable-object-list '(passable-screen default-wall))
    (vaultable-object-passable screen-probe-agent passable-screen)
    (not (vaultable-object-passable screen-probe-agent default-gate))
    (not (vaultable-object-passable screen-probe-agent default-wall))
    (not (jump-required-clearance-height
           screen-probe-agent '(passable-screen)))
    (= (jump-required-clearance-height
         screen-probe-agent '(passable-screen default-wall))
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

    ;; One MOVE action owns both grounded routes and explicit support changes.
    (find 'move *actions* :key #'action.name)
    (not (find 'change-configuration *actions* :key #'action.name))
    (not (find 'jump-to *actions* :key #'action.name))

    ;; Inspect the installed action rather than merely restating its branch conditions.
    (jump-transition-scenarios-valid-p state)))


(define-goal
  (jump-scenarios-valid))


(define-test-claim vertical-reach-parameter-relevant-to-jumping
  (vertical-reach-limit-relevant-p *start-state*)
  (some (lambda (fact)
          (and (member (first fact) '(traverse-via traverse-via>))
               (eq (second fact) 'jumping)
               (vertical-reach-jump-fact-relevant-p *start-state* fact)))
        (list-static-db))
  (search "*VERTICAL-REACH-LIMIT*"
          (with-output-to-string (*standard-output*)
            (display-current-parameters))))


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
  (<= (- ?target-elevation (base ?agent))
      *vertical-reach-limit*)
  "Ignores the explicit hypothetical source elevation.  The stairs-then-jump
   route must then fail from the agent's actual pre-move elevation.")


(define-query-mutation jump-path-uses-actual-source jump-path-clear
  (?agent agent ?source-elevation ?features)
  (and (vaultable-object-list ?features)
       (assign $required
               (jump-required-clearance-height ?agent ?features))
       (or (not $required)
           (<= (- $required (base ?agent))
               *vertical-reach-limit*)))
  "Ignores the explicit hypothetical source elevation for path clearance.  The
   elevated intermediate jump and the explicit-source probes must detect it.")
