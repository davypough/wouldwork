;;; Filename: box.lisp

;;; Box technology: a carryable box.  An agent can pick up a box within its own height of reach,
;;; put a held box down on a destination within its own height of reach, and jump between
;;; supports at its location or across a jump-via edge to an adjacent, elevation-differing
;;; location -- climbing at most its own height up, stepping or dropping down freely.  A box is
;;; movable cargo that occupies its support via the (on ...) relation; it has no derived state
;;; of its own.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  plate and box are declared optional here
;;;               (define-optional-types), so a problem lacking either need not declare it
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -location (mobile-object, (has-location ...)); -holding (cargo, (holding ...));
;;;               -position (fixed-position-object, (has-position ...)); -height
;;;               (heighted-object, (has-height ...)); -elevation ((has-elevation ...),
;;;               location-elevation)  --  all shared via nested include-tech rather than
;;;               local declaration, same pattern as beam-direct/beam-relay/beam-crossing
;;;               nesting -beam-substrate
;;;   queries   : reachable (reachability, for pickup/put);
;;;               all-clear (accessibility, for jump-to's location crossings);
;;;               agent-height (agent, for reach/climb bounds)
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : plate, box  --  declared optional here; other techs (plate, gate, jammer,
;;;               barrier, beam-relay, etc.) still declare their own plate-alias/box-alias
;;;               forms for their own pre-params; the bare and aliased forms resolve compatibly
;;;   relations : (jump-via location $list location)  --  vertical/elevation-crossing edge;
;;;               jump-only, never usable by move
;;;   queries   : support-top-elevation, occupant-elevation, box-height     ; declared-or-default unit height; plate/ground reads location-elevation
;;;   actions   : pickup-box, put-box, jump-to              ; jump-to also crosses to an elevation-differing adjacent location

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -holding)
(include-tech -position)
(include-tech -height)
(include-tech -elevation)

(in-package :ww)


(define-optional-types plate box)


(define-static-relations
  (jump-via location $list location))  ;symmetric elevation-crossing edge; $list = guarding obstacles (gates/screens)


(define-action pickup-box
  1
  (?agent agent ?box box)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (has-location ?agent $a-location))
       (bind (has-location ?box $box-location))
       (cleartop ?box)
       (reachable $box-location $a-location)
       (<= (abs (- (occupant-elevation ?box) (occupant-elevation ?agent))) (agent-height ?agent)))  ;vertical reach: box rests within the agent's height of the agent's level
  (":" ?agent "picks up" ?box "at" $box-location "from" $a-location)
  (assert (holding ?agent ?box)
          (not (has-location ?box $box-location))
          (if (bind (on ?box $support))
            (not (on ?box $support)))
          (finally (propagate-changes!))))


(define-action put-box
  ;; Place a held box on the ground or on a clear support at a reachable location (including the
  ;; agent's own): one successor per plate, per clear box top, and the ground fallback.  Each
  ;; destination is gated by manual reach -- its resting level must be within the agent's
  ;; height (agent-height) of the agent's own level.
  1
  (?agent agent ?box box ?location location)
  (and (holding ?agent ?box)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location))
  (":" ?agent "puts" ?box "on" $place "at" ?location)
  (do (doall (?plate plate)
        (if (and (has-position ?plate ?location)
                 (cleartop ?plate)
                 (<= (abs (- (support-top-elevation ?plate) (occupant-elevation ?agent))) (agent-height ?agent)))
          (assert (not (holding ?agent ?box))
                  (has-location ?box ?location)
                  (on ?box ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      (doall (?support-box box)
        (if (and (different ?support-box ?box)
                 (has-location ?support-box ?location)
                 (cleartop ?support-box)
                 (<= (abs (- (support-top-elevation ?support-box) (occupant-elevation ?agent))) (agent-height ?agent)))
          (assert (not (holding ?agent ?box))
                  (has-location ?box ?location)
                  (on ?box ?support-box)
                  (assign $place ?support-box)
                  (finally (propagate-changes!)))))
      (if (<= (occupant-elevation ?agent) (agent-height ?agent))
        (assert (not (holding ?agent ?box))
                (has-location ?box ?location)
                (assign $place 'ground)
                (finally (propagate-changes!))))))


(define-action jump-to
  ;; Change the agent's support or location via a vertical transition: climb onto a clear box at
  ;; the current location (at most the agent's own height up, agent-height), step down onto a
  ;; lower clear box, drop to the ground, or cross a jump-via edge to an adjacent location whose
  ;; elevation differs from the agent's current level (bounded the same way).  Same-elevation
  ;; location changes are move's job, not jump-to's -- the crossing branch below excludes a
  ;; zero elevation delta so the two actions never reach the identical result.  One successor
  ;; per legal destination: box, ground, or elevation-differing adjacent location.  The agent
  ;; may carry cargo throughout.
  1
  (?agent agent)
  (bind (has-location ?agent $a-location))
  (":" ?agent "at" $a-location "jumps onto" $place)
  (do (doall (?box box)
        (if (and (has-location ?box $a-location)
                 (cleartop ?box)
                 (not (on ?agent ?box))
                 (<= (- (support-top-elevation ?box) (occupant-elevation ?agent)) (agent-height ?agent)))
          (assert (if (bind (on ?agent $current))
                    (not (on ?agent $current)))
                  (on ?agent ?box)
                  (assign $place ?box)
                  (finally (propagate-changes!)))))
      (if (bind (on ?agent $current-support))
        (assert (not (on ?agent $current-support))
                (assign $place 'ground)
                (finally (propagate-changes!))))
      (doall (?to-location location)
        (if (and (bind (jump-via $a-location $obstacles ?to-location))
                 (/= (location-elevation ?to-location) (occupant-elevation ?agent))
                 (<= (- (location-elevation ?to-location) (occupant-elevation ?agent)) (agent-height ?agent))
                 (all-clear ?agent $obstacles))
          (assert (if (bind (on ?agent $prior-support))
                    (not (on ?agent $prior-support)))
                  (has-location ?agent ?to-location)
                  (assign $place ?to-location)
                  (finally (propagate-changes!)))))))


(define-query support-top-elevation (?support support)
  ;; Elevation of a support's top surface, where an occupant standing on it would rest: for a
  ;; plate, the elevation of the location it's positioned at; otherwise the box's own resting
  ;; level plus its declared-or-default height (box-height).
  (if (box ?support)
    (+ (occupant-elevation ?support) (box-height ?support))
    (do (bind (has-position ?support $location))
        (location-elevation $location))))


(define-query occupant-elevation (?occupant support-occupant)
  ;; Standing level of an occupant: on the ground or on a plate, the elevation of its own
  ;; location; otherwise the support box's own level plus its declared-or-default height
  ;; (box-height).
  (if (and (bind (on ?occupant $support))
           (box $support))
    (+ (occupant-elevation $support) (box-height $support))
    (do (bind (has-location ?occupant $location))
        (location-elevation $location))))


(define-query box-height (?box box)
  ;; Declared physical height of a box, or 1 (the historical unit height) when undeclared, so
  ;; problems with no (has-height ...) facts keep their existing stacking arithmetic unchanged.
  (if (bind (has-height ?box $h))
    $h
    1))
