;;; Filename: jump.lisp

;;; Jumping traversal kind, plus the explicit support-changing transitions only jumping
;;; provides.  A clause naming an EDGE, a WALL, or a floor drive (FLOOR-BLOWER or
;;; FLOOR-GEARS) is a jump, and this file registers the one
;;; predicate that decides it: the landing may rise no more than *vertical-reach-limit*
;;; above the launch, and every feature in the clause that is not currently passable must
;;; be low enough to clear within that same bound.  Level and downward landings are
;;; unrestricted.
;;;
;;; An edge is a static separator -- the vertical face between two levels -- so it names
;;; the jump without being vaulted: it stays in the segment's witness, but -traversal's
;;; TRAVERSAL-CLAUSE-PROFILE removes it from the features the clearance rules see.  A wall
;;; is a marker too, and stays a feature, since clearing it is the point.  A floor drive is
;;; a static marker like an edge: a clause naming it is a drop off the side of its stream,
;;; from the hovering AIMED-AT destination to ground beside it, where nothing else is drawn
;;; between the two.  An occupant is at that destination only while the stream holds it,
;;; so the drive needs no clearance test.  A wall drive's destination is ordinary ground,
;;; so wall drives are not markers.  In a bare-level
;;; problem, -traversal also hands this builder a walk-kind clause across a level
;;; difference, which is what an unnamed jumping edge used to mean.
;;;
;;; Jumping handles exclusively elevation-related moves: local support changes involve box
;;; tops and trays currently held by another agent (mounting and dismounting flush supports
;;; like plates and gears-mounted fans belongs to the step technology; a fan resting on a
;;; box top is not a jump landing).  A grounded tray remains inert.
;;; Open gates and passable screens impose no clearance requirement.  Closed gates,
;;; non-passable screens, and walls contribute their top elevations; a multi-feature jump
;;; must clear the highest feature that is not currently passable.
;;;
;;; Each produced segment or transition is tagged JUMP when nothing required clearance, or
;;; VAULT when some feature genuinely did (accounting for passability) -- a move-type label
;;; a printed route displays alongside WALK, STAIRS, and LADDER.  It is a label on the
;;; segment, not a kind: both come from the one jump clause, and which one a crossing earns
;;; depends on the state it is evaluated in.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  box, tray, wall, edge, floor-blower, and floor-gears
;;;               are declared optional here
;;;   nested    : -vertical (top, location-elevation);
;;;               -support-elevation (support occupancy and *vertical-reach-limit*, which
;;;               this file reuses rather than defining its own jump-specific parameter);
;;;               -passability (holding and
;;;               obstacle-clear); -threat (safe); -traversal; -mobility-action
;;; PROVIDES:
;;;   types     : box, tray, wall, edge, floor-blower, floor-gears  --  declared optional;
;;;               jumping remains usable without them; vaultable-object
;;;               (either gate screen wall)
;;;   kind      : jump, registered with -traversal
;;;   cache     : *vertical-reach-limit*, registered with -traversal's segment cache
;;;   queries   : jump-elevation-reachable, vaultable-object-passable,
;;;               jump-barrier-top-elevation, vaultable-object-list,
;;;               jump-required-clearance-height, jump-path-clear,
;;;               jump-landing-support-at, jump-configuration-transitions
;;;   provider  : jump-configuration-transitions registered with
;;;               -configuration-transition
;;;   action    : move (grounded routes and support changes)

(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -passability)
(include-tech -threat)
(include-tech -traversal)
(include-tech -mobility-action)

(in-package :ww)


(define-optional-types box tray wall edge floor-blower floor-gears)


(define-types
  jump-landing-support (either box tray)
  vaultable-object (either gate screen wall))


(define-query jump-elevation-reachable
    (?agent agent ?source-elevation ?target-elevation)
  ;; Downward and level jumps are unrestricted.  An upward landing may rise no more than
  ;; *vertical-reach-limit* above the explicit source elevation.  Keeping the source
  ;; explicit lets mobility test an intermediate location without moving the agent there.
  (do ?agent
      (<= (- ?target-elevation ?source-elevation)
          *vertical-reach-limit*)))


(define-query vaultable-object-passable (?agent agent ?feature vaultable-object)
  ;; Gates and screens may be crossed without vaulting when their ordinary passability rule
  ;; permits it.  Walls always require clearance.
  (or (and (gate ?feature)
           (obstacle-clear ?agent ?feature))
      (and (screen ?feature)
           (obstacle-clear ?agent ?feature))))


(define-query jump-barrier-top-elevation (?feature vaultable-object)
  (top ?feature))


(define-query vaultable-object-list (?features)
  (ww-loop for $feature in ?features
           always (vaultable-object $feature)))


(define-query jump-required-clearance-height (?agent agent ?features)
  ;; Passable features need no clearance.  Every remaining feature is physically vaulted, so
  ;; the required clearance is the highest of their top elevations.  NIL means all features
  ;; are currently passable or the clause is empty.
  (do (assign $required nil)
      (ww-loop for $feature in ?features
               do (if (not (vaultable-object-passable ?agent $feature))
                    (do (assign $top (jump-barrier-top-elevation $feature))
                        (assign $required
                                (if $required
                                  (max $required $top)
                                  $top)))))
      $required))


(define-query jump-path-clear (?agent agent ?source-elevation ?features)
  ;; A vaultable barrier's top may rise no more than *vertical-reach-limit* above the
  ;; explicit launch elevation -- the same fixed bound JUMP-ELEVATION-REACHABLE applies to
  ;; a raised landing.  Clearing a barrier and rising in elevation are the same physical
  ;; constraint, independent of the jumping agent's own declared height; ?agent still
  ;; matters here only through VAULTABLE-OBJECT-PASSABLE's holding-state check.
  (and (vaultable-object-list ?features)
       (assign $required
               (jump-required-clearance-height ?agent ?features))
       (or (not $required)
           (<= (- $required ?source-elevation)
               *vertical-reach-limit*))))


(define-problem-helper jump-segment-for-clause
    (state agent source destination clause)
  "Return a normalized JUMP or VAULT segment when CLAUSE's features can be cleared from
   SOURCE's level and the landing is within reach.  The label is VAULT when some feature
   genuinely required clearance for this agent, else JUMP.  The features are the clause
   without its edges; the witness is the whole canonical clause."
  (let ((features (canonical-enabling-means (second (traversal-clause-profile clause))))
        (source-elevation
          (funcall (symbol-function 'location-elevation) state source))
        (target-elevation
          (funcall (symbol-function 'location-elevation) state destination)))
    (when (and
            (funcall (symbol-function 'jump-path-clear)
                     state agent source-elevation features)
            (funcall (symbol-function 'jump-elevation-reachable)
                     state agent source-elevation target-elevation)
            (funcall (symbol-function 'safe) state destination))
      (list (if (funcall (symbol-function 'jump-required-clearance-height)
                         state agent features)
              'vault
              'jump)
            source (canonical-enabling-means clause) destination))))


(register-traversal-kind 'jump 'jump-segment-for-clause
                         '(edge wall floor-blower floor-gears)
                         '(edge floor-blower floor-gears)
                         '(gate screen))


;; JUMP-ELEVATION-REACHABLE and JUMP-PATH-CLEAR both read *VERTICAL-REACH-LIMIT*, so it
;; belongs in -traversal's segment cache key: changing it at the REPL mid-session must not
;; be answered from segments computed under the old bound.
(register-traversal-cache-parameter '*vertical-reach-limit*)


;;;; SUPPORT-CHANGING TRANSITIONS ;;;;
;;;; Landing on or stepping off a support is a configuration change rather than a move
;;;; between locations, so it cannot go through -traversal's segment provider: a
;;;; transition's endpoints are (location place) configurations.  The authored facts are
;;;; the same ones, but only their jump-kind clauses are read here: a pair whose family
;;;; offers only a walk, a stairway or a climb offers no support landing (decision D7).


(define-query jump-landing-support-at
    (?agent agent ?support jump-landing-support ?location location)
  ;; A located box always offers its top.  A tray offers its top only while another agent
  ;; holds it: grounded trays are inert, and letting a holder mount its own tray would
  ;; create a recursive support cycle.  SUPPORT-USE-ALLOWED below applies recorder-side
  ;; policy separately, including live playback's use of a ghost-held tray.
  (and (has-location ?support ?location)
       (or (box ?support)
           (and (tray ?support)
                (bind (holding $holder ?support))
                (different $holder ?agent)))))


(define-problem-helper jump-configuration-transition-for-clause
    (state agent source-configuration source-elevation
           destination-configuration target-elevation clause)
  "Return one feasible support-changing JUMP or VAULT transition across CLAUSE, or NIL.
   As in JUMP-SEGMENT-FOR-CLAUSE, the features exclude edges and the witness does not."
  (let ((features (canonical-enabling-means (second (traversal-clause-profile clause))))
        (destination (first destination-configuration)))
    (when (and
            (funcall (symbol-function 'jump-path-clear)
                     state agent source-elevation features)
            (funcall (symbol-function 'jump-elevation-reachable)
                     state agent source-elevation target-elevation)
            (funcall (symbol-function 'safe) state destination))
      (list (if (funcall (symbol-function 'jump-required-clearance-height)
                         state agent features)
              'vault
              'jump)
            source-configuration (canonical-enabling-means clause)
            destination-configuration))))


(define-problem-helper jump-configuration-transition-for-family
    (state agent source-configuration source-elevation
           destination-configuration target-elevation family)
  "The first transition FAMILY's jump-kind clauses permit, in canonical order, or NIL.
   The configuration twin of -traversal's TRAVERSAL-SEGMENT-FOR-FAMILY, which cannot serve
   here because these endpoints are configurations rather than locations.  Kind is judged
   between the two locations, as for a grounded segment, whatever supports the endpoints."
  (loop for clause in (traversal-family-kind-clauses
                        state (first source-configuration)
                        (first destination-configuration) family 'jump)
        for transition = (jump-configuration-transition-for-clause
                           state agent source-configuration source-elevation
                           destination-configuration target-elevation clause)
        when transition
          return transition))


(define-query jump-configuration-transitions
    (?agent agent ?source-configuration)
  (do (assign $source-location (first ?source-configuration))
      (assign $source-place (second ?source-configuration))
      (assign $source-elevation
              (if (eql $source-place 'ground)
                (location-elevation $source-location)
                (top $source-place)))
      (assign $transitions nil)

      ;; Local mounts and transfers.  A box may itself be part of a stack; its top elevation
      ;; already follows that support chain through TOP.  A held tray's top similarly
      ;; follows its holder, while JUMP-LANDING-SUPPORT-AT excludes grounded and self-held
      ;; trays.
      (doall (?support jump-landing-support)
        (if (and (jump-landing-support-at
                   ?agent ?support $source-location)
                 (different ?support $source-place)
                 (cleartop ?support ?agent)
                 (support-use-allowed ?agent ?support)
                 (jump-elevation-reachable
                   ?agent $source-elevation (top ?support)))
          (assign $transitions
                  (cons
                    (list 'jump ?source-configuration nil
                          (list $source-location ?support))
                    $transitions))))

      ;; A local drop belongs to jump only from a raised jump support.  Flush plate/fan
      ;; dismounts are supplied by the step provider.
      (if (and (not (eql $source-place 'ground))
               (jump-landing-support $source-place))
        (assign $transitions
                (cons
                  (list 'jump ?source-configuration nil
                        (list $source-location 'ground))
                  $transitions)))

      ;; Remote clear-support landings are configuration transitions whether the launch is
      ;; grounded or supported.  For a tray, the destination remains legal only while a
      ;; different agent holds it.
      (doall (?landing-support jump-landing-support)
        (if (and (bind (has-location ?landing-support $destination))
                 (different $source-location $destination)
                 (jump-landing-support-at
                   ?agent ?landing-support $destination)
                 (cleartop ?landing-support ?agent)
                 (support-use-allowed ?agent ?landing-support))
          (do (assign $destination-configuration
                      (list $destination ?landing-support))
              (assign $target-elevation
                      (top ?landing-support))
              (assign $symmetric-transition nil)
              (assign $directed-transition nil)
              (if (bind (traverse-via
                          $source-location $symmetric-family $destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-family)))
              (if (bind (traverse-via>
                          $source-location $directed-family $destination))
                (assign $directed-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $directed-family)))
              (if $symmetric-transition
                (assign $transitions
                        (cons $symmetric-transition $transitions)))
              (if $directed-transition
                (assign $transitions
                        (cons $directed-transition $transitions))))))

      ;; Only a supported source uses an authored jump clause to land on remote ground;
      ;; grounded versions of the same clauses belong to -traversal's segment provider.
      (if (not (eql $source-place 'ground))
        (doall (?destination location)
          (do (assign $destination-configuration
                      (list ?destination 'ground))
              (assign $target-elevation
                      (location-elevation ?destination))
              (assign $symmetric-transition nil)
              (assign $directed-transition nil)
              (if (bind (traverse-via
                          $source-location $symmetric-family ?destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-family)))
              (if (bind (traverse-via>
                          $source-location $directed-family ?destination))
                (assign $directed-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $directed-family)))
              (if $symmetric-transition
                (assign $transitions
                        (cons $symmetric-transition $transitions)))
              (if $directed-transition
                (assign $transitions
                        (cons $directed-transition $transitions))))))
      $transitions))


(register-configuration-transition-provider
  'jump-configuration-transitions)
