;;; Filename: -gears-fan.lisp

;;; Gears/fan substrate: the shared machinery every blower technology programs against,
;;; modeled on -beam-substrate's peer-substrate pattern.  A blower is a fan mounted on a
;;; set of fixed gears; the mountings differ (floor-blower's flush floor fixture,
;;; wall-blower's wall fixture, angled-blower's flush pad fixture), but the constituents
;;; are the same: floor-gears, wall-gears, and angled-gears are leaf types unified as
;;; gears, the fan is a carryable disc, mounting is an attachment via (mounted-on ...)
;;; rather than a support placement, control follows gate.lisp's DNF convention via
;;; -controls, and the derived state is uniform -- gears turn when uncontrolled or
;;; control-on, and a fan blows iff it is mounted on turning gears.  What blowing *does*
;;; is mounting-specific, so this file's update-gears-status! derives turning/blowing
;;; only; each mounting tech owns its own consequences update (floor-blower's
;;; update-floor-blower-status! launches and drops, wall-blower's
;;; update-wall-blower-status! sweeps, angled-blower's update-angled-blower-status!
;;; launches along the arc), called after it.
;;;
;;; A floor- or angled-mounted fan is a floor object: mount-fan gives it a has-location,
;;; so it is steppable (step.lisp) and a placement target (-placement) at its gears'
;;; location.  A wall-mounted fan hangs on the wall with NO has-location: every
;;; has-location bind in step, placement, and the resting-fan pickup branch fails for it
;;; automatically, so nothing can stand or rest on it, and pickup-fan reaches it through
;;; its own wall-mounted branch instead (vertical reach to gears-elevation).
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy ((on ...), cleartop); -location ((has-location ...));
;;;               -position ((has-position ...)); -elevation ((has-elevation ...),
;;;               location-elevation); -controls ((controls ...), energized);
;;;               -placement (placement-options, place-held-object!; nests
;;;               -support-elevation and -holding); -reachability (reachable);
;;;               -pickup (pickup-clear)
;;;   conditional relations:
;;;               jamming (jammer), guarded by an exists over jammer (gate.lisp's pattern):
;;;               a jamming jammer forces gears stopped in update-gears-status!; jammer is
;;;               declared optional here, so a problem with no jammers need not declare it
;;;   substrate edits (companions to this file; these files splice before this file's
;;;   gears union is installed, so they reference the leaf types directly):
;;;               -controls          : (controls $list (either gate floor-gears wall-gears
;;;                                    angled-gears) $mode)
;;;               -position          : fixed-position-object (either plate ladder
;;;                                    floor-gears wall-gears angled-gears)
;;;               -elevation         : elevated-object includes wall-gears only --
;;;                                    floor-gears and angled-gears are both flush, so
;;;                                    neither needs an elevation override
;;;               -support-occupancy : support (either plate box fan); gears are NOT a
;;;                                    support -- only a fan can occupy them, by attachment
;;;               -location          : mobile-object includes fan
;;;               -holding           : cargo includes fan
;;;               -support-elevation : a fan is a movable, zero-thickness support
;;;               step.lisp          : steppable (either plate fan); reads mounted-on,
;;;                                    guarded by fan
;;;   driver    : the master propagate-consequences! must call update-gears-status!
;;;               after update-receiver-status! and update-plate-status!, and each
;;;               included mounting tech's consequences update after that
;;; PROVIDES:
;;;   types     : floor-gears, wall-gears, angled-gears, fan  --  declared optional here
;;;               gears (either floor-gears wall-gears angled-gears)
;;;   relations : (aimed-at> gears $location)  --  fixed destination of the air stream
;;;               (mounted-on fan $gears)  --  the fan's attachment to its gears
;;;               (welded fan $gears)  --  static; the fan and gears form an inseparable
;;;               unit: pickup-fan refuses to separate them.  A welding problem declares
;;;               both (welded ...) and the init's (mounted-on ...)
;;;               (turning gears)  --  derived; asserted only by update-gears-status!
;;;               (blowing fan)  --  derived; a fan blows iff it is mounted on turning
;;;               gears; asserted only by update-gears-status!
;;;   query     : gears-elevation  --  the working height of a mounted fan: wall gears'
;;;               declared has-elevation or 1 (matching transmitter/receiver anchors);
;;;               floor gears' floor elevation
;;;               landing-support  --  the first clear plate/floor-mounted-fan/box at a
;;;               location whose top matches a required elevation (nil accepts any), no
;;;               agent or reach gate
;;;   updates   : update-gears-status! (state only), relocate-stack!, land-on-support!
;;;               (rests a relocated object on its destination's landing-support match;
;;;               read by wall-blower's sweep and angled-blower's arc)
;;;   actions   : pickup-fan, put-fan, mount-fan

(include-tech -propagation)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -elevation)
(include-tech -controls)
(include-tech -placement)
(include-tech -reachability)
(include-tech -pickup)

(in-package :ww)


(define-optional-types floor-gears wall-gears angled-gears fan jammer)


(define-types
  gears (either floor-gears wall-gears angled-gears))


(define-dynamic-relations
  (mounted-on fan $gears)  ;the fan's attachment to its gears; not an (on ...) support fact
  (turning gears)  ;derived each pass; asserted only by update-gears-status!
  (blowing fan))  ;derived each pass; a fan blows iff it is mounted on turning gears


(define-static-relations
  (aimed-at> gears $location)  ;fixed destination the air stream carries an occupant to
  (welded fan $gears))  ;the fan is permanently attached to these gears and cannot be separated; declare alongside the init's (mounted-on ...)


(define-query gears-elevation (?gears gears)
  ;; The working height of a fan mounted on ?gears, used for mounting/dismounting reach
  ;; and for wall-blower's stream-strike test (who the horizontal air stream hits).
  ;; Wall gears hang at their declared has-elevation, defaulting to 1 (the same anchor
  ;; default as transmitters and receivers); floor and angled gears are both flush, so
  ;; their fan works at the floor elevation of their position.
  (if (wall-gears ?gears)
    (if (bind (has-elevation ?gears $level))
      $level
      1)
    (do (bind (has-position ?gears $location))
        (location-elevation $location))))


(define-update update-gears-status! ()
  ;; Pass 1: turning <=> (uncontrolled OR control-on) AND not jammed, with control-on
  ;; computed exactly as gate.lisp's update-gate-status! computes it: some DNF clause has
  ;; every member energized (normal); inverted negates that aggregate.  A jamming jammer
  ;; forces gears stopped -- the polarity mirror of gate's jam-forces-open: a jam always
  ;; disables the barrier.  Pass 2: a fan blows iff it is mounted on turning gears.  Pure
  ;; state derivation only: what blowing does (launching, sweeping, dropping) is
  ;; mounting-specific and owned by each mounting tech's own consequences update, which
  ;; the driver calls after this one.  Change detection is automatic, so an unchanged
  ;; re-assert is silent.
  (do (doall (?g gears)
        (do (assign $control-on t)  ;uncontrolled gears are always on
            (if (bind (controls $clauses ?g $mode))
              (do (assign $any-clause-on
                    (ww-loop for $clause in $clauses
                             thereis (ww-loop for $c in $clause
                                              always (energized $c))))
                  (if (eql $mode 'normal)
                    (assign $control-on $any-clause-on)
                    (if (eql $mode 'inverted)
                      (assign $control-on (not $any-clause-on))))))
            (if (and $control-on
                     (not (exists (?j jammer)
                            (jamming ?j ?g))))
              (turning ?g)
              (not (turning ?g)))))
      (doall (?f fan)
        (if (and (bind (mounted-on ?f $gears))
                 (turning $gears))
          (blowing ?f)
          (not (blowing ?f))))))


(define-update relocate-stack! (?base ?destination)
  ;; Move ?base and, transitively, every occupant stacked above it to ?destination.
  ;; Breadth-first over the (on ...) links, so arbitrary stack depth needs no recursion.
  (do (assign $moving (list ?base))
      (ww-loop while $moving
               do (assign $next nil)
                  (ww-loop for $object in $moving
                           do (has-location $object ?destination)
                              (doall (?y support-occupant)
                                (if (on ?y $object)
                                  (push ?y $next))))
                  (assign $moving $next))))


(define-query landing-support (?location location ?self support-occupant ?required-elevation)
  ;; The first clear plate, floor-mounted fan, or box at ?location whose top matches
  ;; ?required-elevation, excluding ?self as a candidate, scanned in that plate/fan/box
  ;; order, or nil if none does -- nil for ?required-elevation accepts any candidate's
  ;; elevation.  The ?self exclusion matters only when ?self is itself a box: by the time
  ;; land-on-support! calls this, relocate-stack! has already moved ?self to ?location, so
  ;; without the exclusion a relocated box could be offered as its own landing support
  ;; (harmless for plate/fan candidates, since ?self can never be eql to a
  ;; differently-typed one -- the same reasoning -placement's placement-options already
  ;; documents for its own ?self parameter).  Shared by wall-blower's flush-only landing
  ;; and angled-blower's any-elevation landing, via land-on-support!.  Unlike
  ;; -placement's placement-options, this carries no agent or vertical-reach gate: it is
  ;; read by a physical consequence, not an agent's manipulation choice.
  (do (assign $landing nil)
      (doall (?plate plate)
        (if (and (not $landing)
                 (different ?plate ?self)
                 (has-position ?plate ?location)
                 (cleartop ?plate)
                 (or (not ?required-elevation)
                     (eql (support-top-elevation ?plate) ?required-elevation)))
          (assign $landing ?plate)))
      (doall (?fan fan)
        (if (and (not $landing)
                 (different ?fan ?self)
                 (bind (mounted-on ?fan $gears))
                 (has-location ?fan ?location)
                 (cleartop ?fan)
                 (or (not ?required-elevation)
                     (eql (support-top-elevation ?fan) ?required-elevation)))
          (assign $landing ?fan)))
      (doall (?box box)
        (if (and (not $landing)
                 (different ?box ?self)
                 (has-location ?box ?location)
                 (cleartop ?box)
                 (or (not ?required-elevation)
                     (eql (support-top-elevation ?box) ?required-elevation)))
          (assign $landing ?box)))
      $landing))


(define-update land-on-support! (?base ?destination ?required-elevation)
  ;; Rest ?base, already moved to ?destination by relocate-stack!, on the first
  ;; landing-support match there (excluding ?base itself), or leave it resting on bare
  ;; ground (relocate-stack!'s default) if none matches.
  (do (assign $support (landing-support ?destination ?base ?required-elevation))
      (if $support
        (on ?base $support))))


(define-action pickup-fan
  ;; Pick up a clear fan within reach.  Two cases: a fan with a has-location (resting on
  ;; a support or ground, or mounted on floor or angled gears) goes through the ordinary
  ;; pickup-clear path; a wall-mounted fan has no has-location, so it is reached through
  ;; its gears' position and stream elevation instead, and nothing can rest on it.
  ;; Occupied fans (like occupied boxes) cannot be lifted, so an agent standing on a fan
  ;; can never carry it away.  A fan welded to its gears is an inseparable unit and can
  ;; never be picked up.  Lifting a fan off turning gears is allowed -- the fan itself is
  ;; never blown -- and the ensuing propagation clears its blowing status and applies the
  ;; mounting tech's consequences (e.g. dropping hovering occupants).
  1
  (?agent agent ?fan fan)
  (and (not (bind (welded ?fan $weld-gears)))
       (bind (has-location ?agent $a-location))
       (or (and (bind (has-location ?fan $fan-location))
                (cleartop ?fan)
                (pickup-clear ?agent $a-location ?fan $fan-location))
           (and (bind (mounted-on ?fan $w-gears))
                (wall-gears $w-gears)
                (not (bind (holding ?agent $any-held)))
                (bind (has-position $w-gears $fan-location))
                (reachable $fan-location $a-location)
                (within-agent-vertical-reach ?agent (gears-elevation $w-gears)))))
  (">" ?agent "picks up" ?fan "at" $fan-location "from" $a-location)
  (assert (holding ?agent ?fan)
          (if (bind (has-location ?fan $f-location))
            (not (has-location ?fan $f-location)))
          (if (bind (on ?fan $support))
            (not (on ?fan $support)))
          (if (bind (mounted-on ?fan $gears))
            (not (mounted-on ?fan $gears)))
          (finally (propagate-changes!))))


(define-action put-fan
  ;; Place a held fan on the ground or on a clear support at a reachable location
  ;; (including the agent's own): one successor per legal placement-options result.
  ;; Mounting on gears is the separate mount-fan action, since gears are not a support.
  1
  (?agent agent ?fan fan ?location location)
  (and (holding ?agent ?fan)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?fan)))
  (">" ?agent "puts" ?fan "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?fan ?location $placement-option)
                      (finally (propagate-changes!)))))


(define-action mount-fan
  ;; Attach a held fan to vacant gears at a reachable location, within vertical reach of
  ;; the gears' working height.  A floor or angled mount makes the fan a floor object at
  ;; the gears' location; a wall mount hangs it with no has-location.  Mounting on
  ;; already-turning gears is legal; the empty fan just starts blowing.
  1
  (?agent agent ?fan fan ?gears gears)
  (and (holding ?agent ?fan)
       (bind (has-location ?agent $a-location))
       (bind (has-position ?gears $g-location))
       (reachable $g-location $a-location)
       (not (exists (?f fan)
              (and (bind (mounted-on ?f $g))
                   (eql $g ?gears))))
       (within-agent-vertical-reach ?agent (gears-elevation ?gears)))
  (">" ?agent "mounts" ?fan "on" ?gears "at" $g-location)
  (assert (not (holding ?agent ?fan))
          (if (or (floor-gears ?gears)
                  (angled-gears ?gears))
            (has-location ?fan $g-location))
          (mounted-on ?fan ?gears)
          (finally (propagate-changes!))))
