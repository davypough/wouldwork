;;; Filename: tray.lisp

;;; Tray technology: a carryable, stackable support -- but only while held.  An agent can
;;; pick up a tray within its own height of reach and put a held tray on the ground, a
;;; plate, a clear box top, or another agent's currently-held tray, exactly like a box.
;;; Unlike a box, a tray also supports occupants of its own while held: another agent may
;;; place an object on any currently-held tray (see -placement's held-tray clause), and
;;; that object's has-location tracks the holder's as it moves (see
;;; -configuration-transition's relocation cascade).  A tray resting on the ground is
;;; inert -- nothing can be placed on it there -- and it keeps its has-location fact even
;;; while held, the one deviation from held cargo having no location, so that its
;;; occupant's has-location consumers (beam-relay, visibility, etc.) keep working
;;; unchanged.
;;;
;;; REQUIRES:
;;;   types     : agent, location; tray is declared optional here
;;;   nested    : -placement (placement-options, place-held-object!; also brings in
;;;               support occupancy, location, position, height, elevation, and holding);
;;;               -reachability (identity-default reachable, overridden by reachability);
;;;               -pickup (pickup-clear, shared with box, jammer, and beam-relay)
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : tray -- declared optional here
;;;   actions   : pickup-tray, put-tray

(include-tech -placement)
(include-tech -reachability)
(include-tech -pickup)

(in-package :ww)


(define-optional-types tray)


(define-action pickup-tray
  ;; Pick up a tray resting on the ground or on a support, within reach.  Unlike other
  ;; cargo, a tray keeps its has-location fact even while held (synced to its holder's
  ;; location by apply-agent-configuration!'s relocation cascade), so a tray already held
  ;; by someone else must be excluded explicitly instead of relying on a missing
  ;; has-location to rule it out.  A grounded tray is always cleartop by construction --
  ;; nothing can rest on a tray unless it is held -- so no cleartop check is needed here.
  1
  (?agent agent ?tray tray)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?tray $tray-location))
       (not (bind (holding $holder ?tray)))
       (pickup-clear ?agent $a-location ?tray $tray-location))
  (">" ?agent "picks up" ?tray "at" $tray-location "from" $a-location)
  (assert (holding ?agent ?tray)
          (has-location ?tray $a-location)
          (if (bind (on ?tray $support))
            (not (on ?tray $support)))
          (finally (propagate-changes!))))


(define-action put-tray
  ;; Place a held tray on the ground or on a clear support at a reachable location
  ;; (including the agent's own): one successor per legal placement-options result.  A
  ;; tray may be loaded when put down -- its occupant's has-location is already correct,
  ;; kept synced throughout by the relocation cascade, so it simply lands wherever the
  ;; tray does; support-top-elevation's grounded-tray branch takes over from that instant.
  1
  (?agent agent ?tray tray ?location location)
  (and (holding ?agent ?tray)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?tray)))
  (">" ?agent "puts" ?tray "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?tray ?location $placement-option)
                      (finally (propagate-changes!)))))
