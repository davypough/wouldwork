;;; Filename: -placement.lisp

;;; Placement substrate: where a carried object may be set down -- a plate, a floor-mounted
;;; fan, a fixed floor/angled blower, a clear box top, another agent's currently-held tray,
;;; or bare ground -- gated by cleartop and the agent's placement reach, which bounds how far
;;; above the agent a resting place may be but never how far below it.  A fan qualifies as
;;; a support only while mounted on gears: a loose fan is mere cargo, like a connector, and
;;; a wall-mounted fan has no has-location.  A fixed floor/angled blower exposes the same
;;; flush support surface through its fixed position.  A tray qualifies as a support only
;;; while held: grounded, it is inert, so only currently-held trays are ever offered.  Shared by every carried-object
;;; technology that must choose where a held object comes to rest: box, jammer, beam-relay,
;;; and a fan mounted on floor-gears.  Declared identically by each until now; this file owns it
;;; once.  Mounting a fan on gears is an attachment, not a support placement, so it is
;;; -gears-fan's own mount-fan action rather than a case here.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -vertical (top, location-elevation); -support-elevation
;;;               (within-agent-placement-reach); -support-occupancy
;;;               (support, on, cleartop); -location (has-location);
;;;               -position (has-position); -holding (cargo, holding);
;;;               -interaction-policy (object-manipulation-allowed,
;;;               support-use-allowed)
;;;   conditional relation:
;;;               mounted-on (-gears-fan), guarded by a DOALL over optional FAN;
;;;               a problem with fans must include a gears/fan technology
;;; PROVIDES:
;;;   queries   : placement-choice-allowed -- shared policy gate used by both option
;;;               generation and the placement update
;;;               placement-options  --  legal plate/fan/fixed-blower/box/tray/ground
;;;               placements at a location, excluding a given object (?self) as a
;;;               candidate support; only a floor-mounted fan and only a currently-held
;;;               tray are ever offered
;;;               placement-elevation -- the resting base elevation produced by an option
;;;   update    : place-held-object!  --  releases ?agent's hold, sets ?object's location,
;;;               unloads it first when it is a tray, and rests it on ?place unless
;;;               ?place is 'ground

(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -holding)
(include-tech -interaction-policy)

(in-package :ww)


(define-optional-types fan floor-blower angled-blower)


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
  ;; candidate).  Each candidate resting place is gated by WITHIN-AGENT-PLACEMENT-REACH, so
  ;; a place below the agent is always offered and one above it only within reach.
  (do (assign $places nil)
      (doall (?plate plate)
        (if (and (has-position ?plate ?location)
                 (cleartop ?plate)
                 (placement-choice-allowed ?agent ?self ?plate)
                 (within-agent-placement-reach ?agent (top ?plate)))
          (assign $places (cons ?plate $places))))
      (doall (?fan fan)
        (if (and (different ?fan ?self)
                 (bind (mounted-on ?fan $gears))  ;a fan supports only while gears-mounted; a loose fan is mere cargo
                 (has-location ?fan ?location)  ;and a wall-mounted fan has no has-location, so floor-mounted only
                 (cleartop ?fan)
                 (placement-choice-allowed ?agent ?self ?fan)
                 (within-agent-placement-reach ?agent (top ?fan)))
          (assign $places (cons ?fan $places))))
      (doall (?fixed (either floor-blower angled-blower))
        (if (and (different ?fixed ?self)
                 (has-position ?fixed ?location)
                 (cleartop ?fixed)
                 (placement-choice-allowed ?agent ?self ?fixed)
                 (within-agent-placement-reach ?agent
                                                 (top ?fixed)))
          (assign $places (cons ?fixed $places))))
      (doall (?support-box box)
        (if (and (different ?support-box ?self)
                 (has-location ?support-box ?location)
                 (cleartop ?support-box)
                 (placement-choice-allowed ?agent ?self ?support-box)
                 (within-agent-placement-reach ?agent (top ?support-box)))
          (assign $places (cons ?support-box $places))))
      (doall (?tray tray)
        (if (and (different ?tray ?self)
                 (bind (holding $holder ?tray))  ;a tray supports only while held; grounded, it is inert
                 (has-location ?tray ?location)  ;co-located with ?agent (synced to the holder's location)
                 (cleartop ?tray)
                 (placement-choice-allowed ?agent ?self ?tray)
                 (within-agent-placement-reach ?agent (top ?tray)))
          (assign $places (cons ?tray $places))))
      (if (and (placement-choice-allowed ?agent ?self 'ground)
               (within-agent-placement-reach ?agent (location-elevation ?location)))
        (assign $places (cons 'ground $places)))
      $places))


(define-query placement-elevation (?location location ?place)
  ;; ?place is either a support object or the Lisp marker GROUND.  This is the base level
  ;; an object will have after PLACE-HELD-OBJECT!, before its own height is added.
  (if (eql ?place 'ground)
    (location-elevation ?location)
    (top ?place)))


(define-update place-held-object!
    (?agent agent ?object cargo ?location location ?place)
  ;; ?place is either a support object or the Lisp marker GROUND, so it remains untyped.
  ;; A tray stops being a support as soon as its holder releases it.  Its direct rider's
  ;; HAS-LOCATION was kept synchronized while the tray moved, so retracting ON leaves that
  ;; rider on the ground at the release location.  Any stack above the rider stays intact.
  (if (placement-choice-allowed ?agent ?object ?place)
    (do (not (holding ?agent ?object))
        (has-location ?object ?location)
        (if (tray ?object)
          (if (bind (on $rider ?object))
            (not (on $rider ?object))))
        (if (not (eql ?place 'ground))
          (on ?object ?place)))
    (inconsistent-state)))
