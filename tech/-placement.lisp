;;; Filename: -placement.lisp

;;; Placement substrate: where a carried object may be set down -- a plate, a clear box
;;; top, or bare ground -- gated by cleartop and the agent's vertical reach.  Shared by
;;; every carried-object technology that must choose where a held object comes to rest:
;;; box, jammer, and beam-relay.  Declared identically by each until now; this file owns
;;; it once.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-elevation (support occupancy, location, position, height,
;;;               elevation, support-top-elevation, occupant-elevation, and
;;;               within-agent-vertical-reach); -holding (cargo, holding)
;;; PROVIDES:
;;;   query     : placement-options  --  legal plate/box/ground placements at a location,
;;;               excluding a given object (?self) as a candidate support
;;;   update    : place-held-object!  --  releases ?agent's hold, sets ?object's location,
;;;               and rests it on ?place unless ?place is 'ground

(include-tech -support-elevation)
(include-tech -holding)

(in-package :ww)


(define-query placement-options (?agent agent ?location location ?self)
  ;; Every plate/box/ground placement at ?location currently legal for ?agent, excluding
  ;; ?self as a candidate support (relevant only when ?self can itself be a box; harmless
  ;; otherwise, since ?self can never be eql to a differently-typed candidate).
  (do (assign $places nil)
      (doall (?plate plate)
        (if (and (has-position ?plate ?location)
                 (cleartop ?plate)
                 (within-agent-vertical-reach ?agent (support-top-elevation ?plate)))
          (assign $places (cons ?plate $places))))
      (doall (?support-box box)
        (if (and (different ?support-box ?self)
                 (has-location ?support-box ?location)
                 (cleartop ?support-box)
                 (within-agent-vertical-reach ?agent (support-top-elevation ?support-box)))
          (assign $places (cons ?support-box $places))))
      (if (within-agent-vertical-reach ?agent (location-elevation ?location))
        (assign $places (cons 'ground $places)))
      $places))


(define-update place-held-object! (?agent ?object ?location ?place)
  (do (not (holding ?agent ?object))
      (has-location ?object ?location)
      (if (not (eql ?place 'ground))
        (on ?object ?place))))
