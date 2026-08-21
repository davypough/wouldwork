;;; Filename: -pickup.lisp

;;; Pickup substrate: whether an agent may pick up a carried object -- empty-handed,
;;; the object's location reachable from the agent's own, and the object's resting
;;; elevation within the agent's vertical reach.  Shared by every carried-object
;;; technology's pickup action: box, jammer, and beam-relay.  Each caller still binds
;;; its own agent/object has-location facts locally (needed downstream for its own
;;; message and effect), and box alone adds its own cleartop check on top of this,
;;; since only box is itself a valid support for another object.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -vertical (base); -support-elevation
;;;               (within-agent-vertical-reach); -holding (cargo, holding);
;;;               -interaction-policy (object-manipulation-allowed);
;;;               -reachability (reachable)
;;; PROVIDES:
;;;   query     : pickup-clear  --  true when ?agent, currently at ?a-location, may
;;;               pick up ?object, currently at ?object-location

(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -holding)
(include-tech -interaction-policy)
(include-tech -reachability)

(in-package :ww)


(define-query pickup-clear (?agent agent ?a-location location ?object cargo ?object-location location)
  (and (object-manipulation-allowed ?agent ?object)
       (not (bind (holding ?agent $any-held-object)))
       (reachable ?object-location ?a-location)
       (within-agent-vertical-reach ?agent (base ?object))))
