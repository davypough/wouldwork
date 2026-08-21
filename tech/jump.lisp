;;; Filename: jump.lisp

;;; Jumping mobility mode, plus the explicit support-changing transitions only jumping
;;; provides.  Registers the one predicate that makes a traversal edge a jump: the landing
;;; may rise no more than *vertical-reach-limit* above the launch, and every feature in the
;;; chosen clause that is not currently passable must be low enough to clear within that
;;; same bound.  Level and downward landings are unrestricted.
;;;
;;; Jumping handles exclusively elevation-related moves: local support changes involve box
;;; tops only (mounting and dismounting flush supports like plates and gears-mounted fans
;;; belongs to the step technology; a fan resting on a box top is not a jump landing).
;;; Open gates and passable screens impose no clearance requirement.  Closed gates,
;;; non-passable screens, and walls contribute their top elevations; a multi-feature jump
;;; must clear the highest feature that is not currently passable.
;;;
;;; Each produced segment or transition is tagged JUMP when nothing required clearance, or
;;; VAULT when some feature genuinely did (accounting for passability) -- a move-type label
;;; a printed route displays alongside WALK, STAIRS, and LADDER.  It is a label on the
;;; segment, not a mode: both come from the one authored JUMPING edge, and which one a
;;; crossing earns depends on the state it is evaluated in.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  box and wall are declared optional here
;;;   nested    : -vertical (top, location-elevation);
;;;               -support-elevation (support occupancy and *vertical-reach-limit*, which
;;;               this file reuses rather than defining its own jump-specific parameter);
;;;               -passability (holding and
;;;               obstacle-clear); -threat (safe); -traversal; -mobility-action
;;; PROVIDES:
;;;   types     : box, wall  --  declared optional; jumping remains usable without them
;;;               vaultable-object (either gate screen wall)
;;;   mode      : jumping, registered with -traversal
;;;   queries   : jump-elevation-reachable, vaultable-object-passable,
;;;               jump-barrier-top-elevation, vaultable-object-list,
;;;               jump-required-clearance-height, jump-path-clear,
;;;               jump-configuration-transitions
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


(define-optional-types box wall)


(define-types
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
   genuinely required clearance for this agent, else JUMP."
  (let ((features (canonical-enabling-means clause))
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
            source features destination))))


(register-traversal-mode 'jumping 'jump-segment-for-clause
                         '(gate screen wall))


;;;; SUPPORT-CHANGING TRANSITIONS ;;;;
;;;; Landing on or stepping off a support is a configuration change rather than a move
;;;; between locations, so it cannot go through -traversal's segment provider: a
;;;; transition's endpoints are (location place) configurations.  The authored edges are
;;;; the same JUMPING ones, read here with the same clause selection.


(define-problem-helper jump-configuration-transition-for-clause
    (state agent source-configuration source-elevation
           destination-configuration target-elevation clause)
  "Return one feasible support-changing JUMP or VAULT transition across CLAUSE, or NIL."
  (let ((features (canonical-enabling-means clause))
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
            source-configuration features
            destination-configuration))))


(define-problem-helper jump-configuration-transition-for-family
    (state agent source-configuration source-elevation
           destination-configuration target-elevation family)
  "The first transition FAMILY's clauses permit, in canonical order, or NIL.  The
   configuration twin of -traversal's TRAVERSAL-SEGMENT-FOR-FAMILY, which cannot serve
   here because these endpoints are configurations rather than locations."
  (loop for clause in (if family
                        (traversal-canonical-family family)
                        (list nil))
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

      ;; Local box mounts and transfers.  A box may itself be part of a stack; its top
      ;; elevation already follows that support chain through TOP.
      (doall (?box box)
        (if (and (has-location ?box $source-location)
                 (different ?box $source-place)
                 (cleartop ?box)
                 (support-use-allowed ?agent ?box)
                 (jump-elevation-reachable
                   ?agent $source-elevation (top ?box)))
          (assign $transitions
                  (cons
                    (list 'jump ?source-configuration nil
                          (list $source-location ?box))
                    $transitions))))

      ;; A local drop belongs to jump only from a box top.  Flush plate/fan dismounts are
      ;; supplied by the step provider.
      (if (and (not (eql $source-place 'ground))
               (box $source-place))
        (assign $transitions
                (cons
                  (list 'jump ?source-configuration nil
                        (list $source-location 'ground))
                  $transitions)))

      ;; Remote clear-box landings are configuration transitions whether the launch is
      ;; grounded or supported.
      (doall (?landing-box box)
        (if (and (bind (has-location ?landing-box $destination))
                 (different $source-location $destination)
                 (cleartop ?landing-box)
                 (support-use-allowed ?agent ?landing-box))
          (do (assign $destination-configuration
                      (list $destination ?landing-box))
              (assign $target-elevation
                      (top ?landing-box))
              (assign $symmetric-transition nil)
              (assign $directed-transition nil)
              (if (bind (traverse-via
                          jumping $source-location $symmetric-family $destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-family)))
              (if (bind (traverse-via>
                          jumping $source-location $directed-family $destination))
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

      ;; Only a supported source uses an authored jumping edge to land on remote ground;
      ;; grounded versions of the same edges belong to -traversal's segment provider.
      (if (not (eql $source-place 'ground))
        (doall (?destination location)
          (do (assign $destination-configuration
                      (list ?destination 'ground))
              (assign $target-elevation
                      (location-elevation ?destination))
              (assign $symmetric-transition nil)
              (assign $directed-transition nil)
              (if (bind (traverse-via
                          jumping $source-location $symmetric-family ?destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-family
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-family)))
              (if (bind (traverse-via>
                          jumping $source-location $directed-family ?destination))
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
