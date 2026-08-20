;;; Filename: jump.lisp

;;; Jumping technology: contribute grounded jumps across authored edges to the mobility
;;; closure, or provide one explicit agent-configuration transition.  Support-changing
;;; landings may be local ground, remote ground, or a clear box top.  Level and downward
;;; landings are unrestricted; upward landings -- whether reaching a higher support or
;;; clearing a vaultable barrier's top -- are limited to *vertical-reach-limit* (default
;;; 1), independent of the jumping agent's own declared height.
;;; Jumping handles exclusively elevation-related moves: local support changes involve box
;;; tops only (mounting and dismounting flush supports like plates and gears-mounted fans
;;; belongs to the step technology; a fan resting on a box top is not a jump landing).
;;; Open gates and passable screens impose no clearance requirement.  Closed gates,
;;; non-passable screens, and walls contribute their top elevations; a multi-feature jump
;;; must clear the highest feature that is not currently passable, within that same fixed
;;; elevation limit.
;;; Each produced segment or transition is tagged JUMP when nothing required clearance, or
;;; VAULT when some feature genuinely did (accounting for passability) -- a move-type label
;;; a printed route displays alongside WALK, STAIRS, and CLIMB; it does not affect
;;; feasibility.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  box and wall are declared optional here
;;;   nested    : -support-elevation (support occupancy, location, height, elevation,
;;;               top, base, and *vertical-reach-limit*,
;;;               which this file reuses rather than defining its own jump-specific
;;;               parameter); -passability (holding and obstacle-clear); -threat (safe --
;;;               true unless an armed gun or other threat endangers the landing location);
;;;               -mobility-action
;;; PROVIDES:
;;;   types     : box, wall  --  declared optional; jumping remains usable without them
;;;               vaultable-object (either gate screen wall)
;;;   relations : (jump-via location $list location)
;;;               (jump-via> location $list location)
;;;   queries   : jump-elevation-reachable, vaultable-object-passable,
;;;               jump-barrier-top-elevation, vaultable-object-list,
;;;               jump-required-clearance-height, jump-path-clear,
;;;               jump-traversal-segments, jump-configuration-transitions
;;;   provider  : jump-traversal-segments registered with -mobility
;;;               jump-configuration-transitions registered with
;;;               -configuration-transition
;;;   action    : move (grounded mobility and explicit support changes)

(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -passability)
(include-tech -threat)
(include-tech -mobility-action)

(in-package :ww)


(define-optional-types box wall)


(define-types
  vaultable-object (either gate screen wall))


(define-static-relations
  (jump-via location $list location)  ;symmetric jump edge; $list = path features
  (jump-via> location $list location))  ;directed jump edge; $list = path features


(define-init-check jump-init-check (literals)
  (:consumes gate screen wall)
  (check-init-list-relation-items-have-types
    literals 'jump-via '(gate screen wall))
  (check-init-list-relation-items-have-types
    literals 'jump-via> '(gate screen wall)))


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
  (+ (base ?feature)
     (object-height ?feature)))


(define-query vaultable-object-list (?features)
  (ww-loop for $feature in ?features
           always (vaultable-object $feature)))


(define-query jump-required-clearance-height (?agent agent ?features)
  ;; Passable features need no clearance.  Every remaining feature is physically vaulted, so
  ;; the required clearance is the highest of their top elevations.  NIL means all features
  ;; are currently passable or the edge has no features.
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


(define-problem-helper jump-segment-for-features
    (state agent source destination features)
  "Return a normalized JUMP or VAULT segment for a feasible grounded traversal.  The
   move-type tag is VAULT when some feature genuinely required clearance for this agent,
   else JUMP."
  (let ((canonical-features (canonical-enabling-means features))
        (source-elevation
          (funcall (symbol-function 'location-elevation) state source))
        (target-elevation
          (funcall (symbol-function 'location-elevation) state destination)))
    (when (and
            (funcall (symbol-function 'jump-path-clear)
                     state agent source-elevation canonical-features)
            (funcall (symbol-function 'jump-elevation-reachable)
                     state agent source-elevation target-elevation)
            (funcall (symbol-function 'safe) state destination))
      (list (if (funcall (symbol-function 'jump-required-clearance-height)
                         state agent canonical-features)
              'vault
              'jump)
            source canonical-features destination))))


(define-query jump-traversal-segments (?agent agent ?from location)
  (do (assign $segments nil)
      (doall (?to location)
        (do (assign $symmetric-segment nil)
            (assign $directional-segment nil)
            (if (bind (jump-via ?from $symmetric-features ?to))
              (assign $symmetric-segment
                      (jump-segment-for-features
                        state ?agent ?from ?to $symmetric-features)))
            (if (bind (jump-via> ?from $directional-features ?to))
              (assign $directional-segment
                      (jump-segment-for-features
                        state ?agent ?from ?to $directional-features)))
            (if $symmetric-segment
              (assign $segments (cons $symmetric-segment $segments)))
            (if $directional-segment
              (assign $segments (cons $directional-segment $segments)))))
      $segments))


(register-mobility-provider 'jump-traversal-segments)


(define-problem-helper jump-configuration-transition-for-features
    (state agent source-configuration source-elevation
           destination-configuration target-elevation features)
  "Return one feasible support-changing JUMP or VAULT transition across an authored edge.
   The move-type tag is VAULT when some feature genuinely required clearance for this
   agent, else JUMP."
  (let ((canonical-features (canonical-enabling-means features))
        (destination (first destination-configuration)))
    (when (and
            (funcall (symbol-function 'jump-path-clear)
                     state agent source-elevation canonical-features)
            (funcall (symbol-function 'jump-elevation-reachable)
                     state agent source-elevation target-elevation)
            (funcall (symbol-function 'safe) state destination))
      (list (if (funcall (symbol-function 'jump-required-clearance-height)
                         state agent canonical-features)
              'vault
              'jump)
            source-configuration canonical-features
            destination-configuration))))


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
      ;; elevation already follows that support chain through SUPPORT-TOP-ELEVATION.
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
              (assign $directional-transition nil)
              (if (bind (jump-via
                          $source-location $symmetric-features $destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-features
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-features)))
              (if (bind (jump-via>
                          $source-location $directional-features $destination))
                (assign $directional-transition
                        (jump-configuration-transition-for-features
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $directional-features)))
              (if $symmetric-transition
                (assign $transitions
                        (cons $symmetric-transition $transitions)))
              (if $directional-transition
                (assign $transitions
                        (cons $directional-transition $transitions))))))

      ;; Only a supported source uses an authored jump edge to land on remote ground;
      ;; grounded versions of the same edges belong to the mobility provider.
      (if (not (eql $source-place 'ground))
        (doall (?destination location)
          (do (assign $destination-configuration
                      (list ?destination 'ground))
              (assign $target-elevation
                      (location-elevation ?destination))
              (assign $symmetric-transition nil)
              (assign $directional-transition nil)
              (if (bind (jump-via
                          $source-location $symmetric-features ?destination))
                (assign $symmetric-transition
                        (jump-configuration-transition-for-features
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $symmetric-features)))
              (if (bind (jump-via>
                          $source-location $directional-features ?destination))
                (assign $directional-transition
                        (jump-configuration-transition-for-features
                          state ?agent ?source-configuration $source-elevation
                          $destination-configuration $target-elevation
                          $directional-features)))
              (if $symmetric-transition
                (assign $transitions
                        (cons $symmetric-transition $transitions)))
              (if $directional-transition
                (assign $transitions
                        (cons $directional-transition $transitions))))))
      $transitions))


(register-configuration-transition-provider
  'jump-configuration-transitions)
