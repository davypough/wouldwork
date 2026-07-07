;;; Filename: jammer.lisp

;;; Jammer technology: a carried jammer that, when placed at a location with line of sight
;;; to a target, jams it (forcing the target open in gate's update-gate-status!).  A
;;; placed jammer is movable cargo: it may rest on a plate (depressing it) or a clear box
;;; top, and picking it up clears both its jamming and its support.
;;;
;;; REQUIRES (supplied by other techs):
;;;   types     : agent, location  --  plate, jammer, and box are declared optional here
;;;               (define-optional-types), so a problem lacking any of them need not
;;;               declare it
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -location (mobile-object, (has-location ...)); -holding (cargo, (holding ...));
;;;               -position (fixed-position-object, (has-position ...))  --  all shared via
;;;               nested include-tech rather than local declaration
;;;   queries   : reachable (reachability), visible (visibility),
;;;               occupant-elevation (box, for pickup-jammer's vertical reach)
;;;   driver    : propagate-changes! (master); (jamming ...) is consumed by gate's
;;;               update-gate-status!
;;; PROVIDES:
;;;   types     : plate, jammer, box  --  declared optional here; other techs (plate, gate,
;;;               box, barrier, beam-relay, accessibility, ladder, etc.) still declare their
;;;               own plate-alias/box-alias forms for their own pre-params; the bare and
;;;               aliased forms resolve compatibly
;;;               target (either gate)  --  what a jammer can jam; connector pairings use
;;;               beam-relay's terminus instead
;;;   relations : (jamming jammer $target)
;;;               (jam-disallowed> location location target)
;;;   actions   : pickup-jammer, jam-target

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -holding)
(include-tech -position)

(in-package :ww)


(define-types
  target (either gate))  ;what a jammer can jam; connector pairings use terminus


(define-optional-types plate jammer box)


(define-dynamic-relations
  (jamming jammer $target))


(define-static-relations
  (jam-disallowed> location location target))  ;agent location, jammer placement, target; directional


(define-action pickup-jammer
  1
  (?agent agent ?jammer jammer)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (has-location ?agent $a-location))
       (bind (has-location ?jammer $jammer-location))
       (reachable $jammer-location $a-location)
       (<= (abs (- (occupant-elevation ?jammer) (occupant-elevation ?agent))) 1))  ;vertical reach: jammer rests within +/-1 of agent level
  (":" ?agent "picks up" ?jammer "at" $a-location)
  (assert (holding ?agent ?jammer)
          (not (has-location ?jammer $jammer-location))
          (if (bind (jamming ?jammer $any-target))
            (not (jamming ?jammer $any-target)))
          (if (bind (on ?jammer $support))
            (not (on ?jammer $support)))
          (finally (propagate-changes!))))


(define-action jam-target
  1
  (?agent agent ?target target ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (visible ?location ?target)
       (not (jam-disallowed> $a-location ?location ?target)))
  (":" ?agent "jams" ?target "with" $any-jammer "at" ?location "on" $place)
  (do (doall (?plate plate)
        (if (and (has-position ?plate ?location)
                 (cleartop ?plate))
          (assert (not (holding ?agent $any-jammer))
                  (jamming $any-jammer ?target)
                  (has-location $any-jammer ?location)
                  (on $any-jammer ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      (doall (?box box)
        (if (and (has-location ?box ?location)
                 (cleartop ?box))
          (assert (not (holding ?agent $any-jammer))
                  (jamming $any-jammer ?target)
                  (has-location $any-jammer ?location)
                  (on $any-jammer ?box)
                  (assign $place ?box)
                  (finally (propagate-changes!)))))
      (assert (not (holding ?agent $any-jammer))
              (jamming $any-jammer ?target)
              (has-location $any-jammer ?location)
              (assign $place 'ground)
              (finally (propagate-changes!)))))
