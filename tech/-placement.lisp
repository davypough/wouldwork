;;; Filename: -placement.lisp

;;; Placement substrate: where a carried object may be set down -- a plate, a floor-mounted
;;; fan, a clear box top, or bare ground -- gated by cleartop and the agent's vertical
;;; reach.  A fan qualifies as a support only while mounted on gears: a loose fan is mere
;;; cargo, like a connector, and a wall-mounted fan has no has-location.  Shared by
;;; every carried-object technology that must choose where a held object comes to rest:
;;; box, jammer, beam-relay, and floor-blower's fan.  Declared identically by each until
;;; now; this file owns it once.  Mounting a fan on gears is an attachment, not a support
;;; placement, so it is -gears-fan's own mount-fan action rather than a case here.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-elevation (support occupancy, location, position, height,
;;;               elevation, support-top-elevation, occupant-elevation, and
;;;               within-agent-vertical-reach); -holding (cargo, holding)
;;; PROVIDES:
;;;   queries   : placement-choice-allowed -- shared policy gate used by both option
;;;               generation and the placement update
;;;               placement-options  --  legal plate/fan/box/ground placements at a
;;;               location, excluding a given object (?self) as a candidate support;
;;;               only a floor-mounted fan is ever offered
;;;   update    : place-held-object!  --  releases ?agent's hold, sets ?object's location,
;;;               and rests it on ?place unless ?place is 'ground

(include-tech -support-elevation)
(include-tech -holding)

(in-package :ww)


(define-optional-types fan)


(define-query placement-choice-allowed (?agent agent ?object cargo ?place)
  ;; ?place is either a support object or the Lisp marker GROUND.
  (and (object-manipulation-allowed ?agent ?object)
       (or (eql ?place 'ground)
           (and (support ?place)
                (support-use-allowed ?object ?place)))))


(define-query placement-options (?agent agent ?location location ?self cargo)
  ;; Every plate/fan/box/ground placement at ?location currently legal for ?agent,
  ;; excluding ?self as a candidate support (relevant only when ?self can itself be a box
  ;; or fan; harmless otherwise, since ?self can never be eql to a differently-typed
  ;; candidate).
  (do (assign $places nil)
      (doall (?plate plate)
        (if (and (has-position ?plate ?location)
                 (cleartop ?plate)
                 (placement-choice-allowed ?agent ?self ?plate)
                 (within-agent-vertical-reach ?agent (support-top-elevation ?plate)))
          (assign $places (cons ?plate $places))))
      (doall (?fan fan)
        (if (and (different ?fan ?self)
                 (bind (mounted-on ?fan $gears))  ;a fan supports only while gears-mounted; a loose fan is mere cargo
                 (has-location ?fan ?location)  ;and a wall-mounted fan has no has-location, so floor-mounted only
                 (cleartop ?fan)
                 (placement-choice-allowed ?agent ?self ?fan)
                 (within-agent-vertical-reach ?agent (support-top-elevation ?fan)))
          (assign $places (cons ?fan $places))))
      (doall (?support-box box)
        (if (and (different ?support-box ?self)
                 (has-location ?support-box ?location)
                 (cleartop ?support-box)
                 (placement-choice-allowed ?agent ?self ?support-box)
                 (within-agent-vertical-reach ?agent (support-top-elevation ?support-box)))
          (assign $places (cons ?support-box $places))))
      (if (and (placement-choice-allowed ?agent ?self 'ground)
               (within-agent-vertical-reach ?agent (location-elevation ?location)))
        (assign $places (cons 'ground $places)))
      $places))


(define-update place-held-object!
    (?agent agent ?object cargo ?location location ?place)
  ;; ?place is either a support object or the Lisp marker GROUND, so it remains untyped.
  (if (placement-choice-allowed ?agent ?object ?place)
    (do (not (holding ?agent ?object))
        (has-location ?object ?location)
        (if (not (eql ?place 'ground))
          (on ?object ?place)))
    (inconsistent-state)))
