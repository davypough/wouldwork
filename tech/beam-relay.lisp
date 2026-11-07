;;; Filename: beam-relay.lisp

;;; Relay beam technology: movable connectors and fixed repeaters propagate beam color
;;; through one graph.  Connector links are dynamic PAIRED facts and require authored live
;;; visibility; fixed apparatus links are directional COUPLED facts and use BEAM-VIA
;;; corridors.  A relay lights only when every source reaching it in the same propagation
;;; layer carries one hue.  Conflicting hues leave it unlit.
;;;
;;; Connectors retain their pickup/placement/pairing actions.  Pairing is split as in the
;;; game: SELECT-TERMINI records SELECTED facts while the connector is held, from wherever
;;; the agent stands, and PUT-CONNECTOR converts every selection into a PAIRED fact at
;;; put-down.  Repeaters have no HAS-LOCATION and no actions; repeater.lisp assembles
;;; this peer with beam-direct and visibility so fixed corridors and apparatus sightlines
;;; are live.
;;;
;;; State invariant: no location may hold two active connectors on the same recording
;;; side.  A model location is an idealized point standing for a small region; in the game
;;; two connectors there sit slightly apart, so whether their beams cross depends on an
;;; offset the model lacks.  Lighting records a CONNECTOR-LOCATION-CLASH wherever a
;;; connector would light at a location already holding a lit same-side connector, and
;;; PROPAGATION-INVARIANT-VIOLATED makes any settled state with a clash inconsistent,
;;; whatever action or update produced it.  An inactive connector carries no beams and
;;; never clashes, so it may be put down beside an active one.
;;;
;;; REQUIRES:
;;;   types      : agent, location -- connector/transmitter/receiver/repeater and related
;;;                leaf types are optional through the nested roles
;;;   nested     : -propagation (derived-state driver); -beam-substrate (beam relations
;;;                and hooks); -placement; -visibility (null defaults); -vertical (top);
;;;                -mobility; -reachability; -pickup; -beam-relay-init-checks;
;;;                -recorder-fork-registry (paired- and selected-fact forks)
;;;   parameter  : *max-connector-pairings* -- defaults to 3; at most that many PAIRED links per
;;;                connector at any time, counting links on both sides, links retained
;;;                through a pickup, and pending SELECTED facts; fixed couplings are
;;;                unlimited; a problem may set a smaller value first
;;;   driver     : propagate-consequences! must call
;;;                  update-relay-status! -> update-receiver-status!
;;; PROVIDES:
;;;   types      : relay (either connector floor-repeater wall-repeater);
;;;                terminus (either transmitter receiver connector floor-repeater
;;;                wall-repeater)
;;;   relations  : paired, selected, color, connector-location-clash
;;;   queries    : relay-beam-reaches-receiver,
;;;                recording-shadow-relay-beam-reaches-receiver,
;;;                compute-relay-lighting, compute-relay-lighting-for-object,
;;;                relay-anchor, relay-linked,
;;;                relay-link-clear, relay-link-clear-for-object,
;;;                paired-relay-visible, paired-relay-visible-for-object,
;;;                relay-beam-live-for-cutting, beam-relay-source-distance,
;;;                compute-relay-lighting-and-clashes, lit-connector-clash,
;;;                connectable-terminus, connector-link-count;
;;;                overrides propagation-invariant-violated (-propagation)
;;;   updates    : update-relay-status!
;;;   actions    : pickup-connector, pickup-connector-retaining-pairings,
;;;                put-connector, select-termini

(include-tech -propagation)
(include-tech -beam-substrate)
(include-tech -placement)
(include-tech -visibility)
(include-tech -vertical)
(include-tech -mobility)
(include-tech -reachability)
(include-tech -pickup)
(include-tech -beam-relay-init-checks)
(include-tech -recorder-fork-registry)

(in-package :ww)


(setf *max-connector-pairings* (or *max-connector-pairings* 3))


(define-types
  relay (either connector floor-repeater wall-repeater)
  terminus
    (either transmitter receiver connector floor-repeater wall-repeater))


(define-optional-types box hue connector transmitter receiver)


(define-dynamic-relations
  (paired connector terminus)
  (selected connector terminus)
  (color relay $hue)
  (connector-location-clash location))


(define-derived-relations
  color connector-location-clash)


;; PAIRED's contribution to the recorder's ghost fork, registered here because this file
;; owns the relation.  PAIRED declares no fluent argument -- either side may be a plain
;; connector or fixed apparatus -- so BIND cannot extract a terminus the way JAMMING and
;; MOUNTED-ON allow.  The clause instead walks every stored (connector terminus) pair
;; directly.  A connector-to-connector pairing may have been stored with either connector
;; first, depending on which one was placed second, so both sides are substituted with
;; their own ghost independently; a side with no ghost keeps its live value, which covers
;; shared fixed apparatus and any unmapped connector.  $CONNECTOR-GHOST and
;; $TERMINUS-GHOST are cleared per iteration because effect variables outlive a DOALL pass.
(register-recorder-fork-clause 'paired
  '(doall (?connector connector)
     (doall (?terminus terminus)
       (if (paired ?connector ?terminus)
         (do (assign $connector-ghost nil)
             (assign $terminus-ghost nil)
             (if (bind (recording-copy> ?connector $connector-ghost))
               (if (bind (recording-copy> ?terminus $terminus-ghost))
                 (paired $connector-ghost $terminus-ghost)
                 (paired $connector-ghost ?terminus))
               (if (bind (recording-copy> ?terminus $terminus-ghost))
                 (paired ?connector $terminus-ghost))))))))


;; SELECTED's contribution to the recorder's ghost fork, built like PAIRED's above: a
;; live holder's pending selections carry over to its ghost holder, with each side
;; substituted by its ghost where one exists.
(register-recorder-fork-clause 'selected
  '(doall (?connector connector)
     (doall (?terminus terminus)
       (if (selected ?connector ?terminus)
         (do (assign $connector-ghost nil)
             (assign $terminus-ghost nil)
             (if (bind (recording-copy> ?connector $connector-ghost))
               (if (bind (recording-copy> ?terminus $terminus-ghost))
                 (selected $connector-ghost $terminus-ghost)
                 (selected $connector-ghost ?terminus))
               (if (bind (recording-copy> ?terminus $terminus-ghost))
                 (selected ?connector $terminus-ghost))))))))


;;;; ACTIONS ;;;;


(define-action pickup-connector
  1
  (?agent agent ?connector connector)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?connector $connector-location))
       (pickup-clear ?agent $a-location ?connector $connector-location))
  (">" ?agent "picks up" ?connector "at" $a-location)
  (assert (holding ?agent ?connector)
          (not (has-location ?connector $connector-location))
          (do (doall (?t terminus)
                (if (paired ?connector ?t)
                  (not (paired ?connector ?t))))
              (doall (?c connector)
                (if (paired ?c ?connector)
                  (not (paired ?c ?connector))))
              (doall (?c connector)
                (if (selected ?c ?connector)
                  (not (selected ?c ?connector)))))
          (if (bind (on ?connector $support))
            (not (on ?connector $support)))
          (finally (propagate-changes!))))


(define-action pickup-connector-retaining-pairings
  ;; The same lift as PICKUP-CONNECTOR, except that every PAIRED fact survives.  A held
  ;; connector has no HAS-LOCATION, so RELAY-ANCHOR returns nothing for it and
  ;; COMPUTE-RELAY-LIGHTING skips it: the pairings outlast the carry while the beam does
  ;; not, and setting the connector down revives whatever those retained pairings can still
  ;; light.  A connector riding a held tray, or a box on one, keeps its beam instead, since
  ;; relocation keeps that connector's HAS-LOCATION.  Offered only when the connector has a
  ;; pairing to retain; without one it would duplicate PICKUP-CONNECTOR's successor exactly.
  1
  (?agent agent ?connector connector)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?connector $connector-location))
       (or (exists (?t terminus)
             (paired ?connector ?t))
           (exists (?c connector)
             (paired ?c ?connector)))
       (pickup-clear ?agent $a-location ?connector $connector-location))
  (">" ?agent "picks up" ?connector "retaining pairings at" $a-location)
  (assert (holding ?agent ?connector)
          (not (has-location ?connector $connector-location))
          (if (bind (on ?connector $support))
            (not (on ?connector $support)))
          (finally (propagate-changes!))))


(define-action put-connector
  ;; The single put-down.  Every pending selection becomes a PAIRED fact and is cleared.
  ;; A put-down that would leave two active connectors at one location is discarded by
  ;; the state invariant after propagation, whether the pairings were kept or selected.
  1
  (?agent agent ?connector connector ?location location)
  (and (holding ?agent ?connector)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?connector)))
  (">" ?agent "puts" ?connector "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object!
                        ?agent ?connector ?location $placement-option)
                      (doall (?t terminus)
                        (if (selected ?connector ?t)
                          (do (not (selected ?connector ?t))
                              (paired ?connector ?t))))
                      (finally (propagate-changes!)))))


(define-action select-termini
  ;; Selection happens while the connector is held, from the agent's current location,
  ;; and survives later moves, including one-way drops.  One action per viewpoint: each
  ;; child selects a nonempty subset of the termini structurally visible from here,
  ;; within the room left under *max-connector-pairings*.  Lighting is unaffected.
  1
  (?agent agent)
  (and (bind (holding ?agent $connector))
       (connector $connector)
       (assign $selection-room
               (- *max-connector-pairings* (connector-link-count $connector)))
       (> $selection-room 0)
       (bind (has-location ?agent $a-location))
       (exists (?t terminus)
         (connectable-terminus ?agent $a-location $connector ?t)))
  (">" ?agent "selects" $termini "for" $connector "at" $a-location)
  (do (assign $selectable nil)
      (doall (?terminus terminus)
        (if (connectable-terminus ?agent $a-location $connector ?terminus)
          (assign $selectable (cons ?terminus $selectable))))
      (ww-loop for $subset in (rest (subsets-up-to $selectable $selection-room))
               do (assert (assign $termini $subset)
                          (ww-loop for $terminus in $subset
                                   do (selected $connector $terminus))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update update-relay-status! ()
  ;; Clash facts are derived like COLOR, so they settle with the fixpoint;
  ;; PROPAGATION-INVARIANT-VIOLATED reads them only after convergence.
  (do (assign $result
              (compute-relay-lighting-and-clashes nil (current-crossing-set)))
      (assign $lighting (first $result))
      (assign $clashes (second $result))
      (doall (?relay relay)
        (do (assign $record (assoc ?relay $lighting))
            (if $record
              (color ?relay (second $record))
              (if (bind (color ?relay $old-hue))
                (not (color ?relay $old-hue))))))
      (doall (?location location)
        (if (member ?location $clashes)
          (connector-location-clash ?location)
          (not (connector-location-clash ?location))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query relay-beam-reaches-receiver (?receiver receiver)
  (do (assign $reaches nil)
      (doall (?relay relay)
        (if (and (bind (color ?relay $relay-hue))
                 (bind (has-chroma ?receiver $required-hue))
                 (eql $relay-hue $required-hue)
                 (or (and (connector ?relay)
                          (paired ?relay ?receiver)
                          (bind (has-location ?relay $location))
                          (beam-visible
                            $location (top ?relay)
                            ?receiver (top ?receiver))
                          (not (beam-cut $location ?receiver)))
                     (and (repeater ?relay)
                          (coupled ?relay ?receiver)
                          (fixed-beam-corridor-clear ?relay ?receiver)
                          (not (beam-cut ?relay ?receiver)))))
          (assign $reaches t)))
      $reaches))


(define-query recording-shadow-relay-beam-reaches-receiver
    (?view ?lighting ?receiver receiver)
  (do (assign $reaches nil)
      ;; Lighting contains only relays available in this view; do not rescan every relay
      ;; or repeat the recording-shadow presence test here.
      (ww-loop for $record in ?lighting
               do (assign $relay (first $record))
                  (if (and
                       (bind (has-chroma ?receiver $required-hue))
                       (eql (second $record) $required-hue)
                       (or (and (connector $relay)
                                (paired $relay ?receiver)
                                (bind (has-location $relay $location))
                                (beam-visible-for-object
                                  ?view $location (top $relay)
                                  ?receiver (top ?receiver)))
                           (and (repeater $relay)
                                (coupled $relay ?receiver)
                                (fixed-beam-corridor-clear-for-object
                                  ?view $relay ?receiver))))
                    (assign $reaches t)))
      $reaches))


(define-query compute-relay-lighting (?active)
  (compute-relay-lighting-for-object nil ?active))


(define-query propagation-invariant-violated ()
  ;; Read once after PROPAGATE-CHANGES! converges: a settled clash discards the state.
  (exists (?location location)
    (connector-location-clash ?location)))


(define-query compute-relay-lighting-for-object (?view ?active)
  (first (compute-relay-lighting-and-clashes ?view ?active)))


(define-query compute-relay-lighting-and-clashes (?view ?active)
  ;; Breadth-first propagation from every transmitter.  Each lighting record is
  ;; (relay hue distance); frontier records additionally carry the relay's beam endpoint.
  ;; Returns (lighting clashes).  A single-hue connector reaching a location already lit
  ;; by a connector on its own recording side stays unlit, and the location is recorded as
  ;; a clash.  Live and ghost connectors at one location light independently.
  (do (assign $lit nil)
      (assign $clashes nil)
      ;; The view class is invariant throughout propagation.
      (assign $recording-view-p (recording-shadow-object ?view))
      (assign $visited nil)
      (assign $frontier nil)
      (doall (?transmitter transmitter)
        (if (bind (has-chroma ?transmitter $hue))
          (assign $frontier
                  (cons (list ?transmitter ?transmitter $hue 0) $frontier))))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (doall (?target relay)
                    (if (and (or (not $recording-view-p)
                                 (recording-shadow-object-present ?target))
                             (not (member ?target $visited)))
                      (do (assign $target-anchor (relay-anchor ?target))
                          (if $target-anchor
                            (do (assign $hues nil)
                                (assign $reach-hue nil)
                                (assign $reach-distance nil)
                                (ww-loop for $source-record in $frontier
                                         do (assign $source (first $source-record))
                                            (assign $source-anchor
                                                    (second $source-record))
                                            (assign $source-hue (third $source-record))
                                            (assign $source-distance
                                                    (fourth $source-record))
                                            (if (relay-link-clear-for-object
                                                  ?view $source $source-anchor
                                                  ?target $target-anchor ?active)
                                              (do (if (not (member $source-hue $hues))
                                                    (assign $hues
                                                            (cons $source-hue $hues)))
                                                  (assign $reach-hue $source-hue)
                                                  (assign $reach-distance
                                                          (1+ $source-distance)))))
                                (if $hues
                                  (do (assign $visited (cons ?target $visited))
                                      (if (not (cdr $hues))
                                        (if (and (connector ?target)
                                                 (lit-connector-clash
                                                   ?target $target-anchor $lit))
                                          (if (not (member $target-anchor $clashes))
                                            (assign $clashes
                                                    (cons $target-anchor $clashes)))
                                          (do (assign $lit
                                                      (cons
                                                        (list ?target
                                                              $reach-hue
                                                              $reach-distance)
                                                        $lit))
                                              (assign $next-frontier
                                                      (cons
                                                        (list ?target
                                                              $target-anchor
                                                              $reach-hue
                                                              $reach-distance)
                                                        $next-frontier))))))))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      (list $lit $clashes)))


(define-query lit-connector-clash (?connector ?anchor ?lighting)
  ;; True when a connector already lit in ?LIGHTING stands at ?ANCHOR on ?CONNECTOR's
  ;; recording side.  Live and ghost copies never clash (CONNECTOR-LOCATION-CONFLICT-P).
  (do (assign $clash nil)
      ;; One DO form: in a problem with no connector type the IF translates to NIL, which
      ;; LOOP would reject as a bare keyword.
      (ww-loop for $record in ?lighting
               do (do (assign $other (first $record))
                      (if (and (connector $other)
                               (eql (relay-anchor $other) ?anchor)
                               (connector-location-conflict-p ?connector $other))
                        (assign $clash t))))
      $clash))


(define-query relay-anchor (?relay relay)
  ;; Movable connectors beam from their current location; fixed repeaters are apparatus
  ;; endpoints in their own right.
  (if (connector ?relay)
    (do (bind (has-location ?relay $location))
        $location)
    ?relay))


(define-query relay-linked
    (?source (either transmitter connector floor-repeater wall-repeater) ?target relay)
  ;; PAIRED is structurally undirected but is always stored with a connector first.
  ;; COUPLED is directional and can target only a repeater or receiver.
  (if (connector ?target)
    (or (paired ?target ?source)
        (and (connector ?source)
             (paired ?source ?target)))
    (or (coupled ?source ?target)
        (and (connector ?source)
             (paired ?source ?target)))))


(define-query relay-link-clear
    (?source (either transmitter connector floor-repeater wall-repeater)
     ?source-anchor beam-node
     ?target relay
     ?target-anchor beam-node
     ?active)
  (relay-link-clear-for-object
    nil ?source ?source-anchor ?target ?target-anchor ?active))


(define-query relay-link-clear-for-object
    (?view
     ?source (either transmitter connector floor-repeater wall-repeater)
     ?source-anchor beam-node
     ?target relay
     ?target-anchor beam-node
     ?active)
  (and (relay-linked ?source ?target)
       (if (coupled ?source ?target)
         (fixed-beam-corridor-clear-for-object ?view ?source ?target)
         (paired-relay-visible-for-object
           ?view ?source ?source-anchor ?target ?target-anchor))
       (not (beam-cut-in ?source-anchor ?target-anchor ?active))))


(define-query paired-relay-visible
    (?source (either transmitter connector floor-repeater wall-repeater)
     ?source-anchor beam-node
     ?target relay
     ?target-anchor beam-node)
  (paired-relay-visible-for-object
    nil ?source ?source-anchor ?target ?target-anchor))


(define-query paired-relay-visible-for-object
    (?view
     ?source (either transmitter connector floor-repeater wall-repeater)
     ?source-anchor beam-node
     ?target relay
     ?target-anchor beam-node)
  ;; A paired link always has at least one connector.  BEAM-VISIBLE is location-first, so
  ;; orient the test from whichever endpoint is the connector.  Two connectors at one
  ;; location sit side by side in the game, so their link is always clear; a beam along it
  ;; makes both active there, which the state invariant then discards.
  (if (eql ?source-anchor ?target-anchor)
    t
    (if (connector ?target)
      (beam-visible-for-object
        ?view ?target-anchor (top ?target)
        ?source-anchor (top ?source))
      (do (connector ?source)
          (beam-visible-for-object
            ?view ?source-anchor (top ?source)
            ?target-anchor (top ?target))))))


(define-query relay-beam-live-for-cutting
    (?from beam-node ?to beam-node ?lighting)
  ;; Arrival/visibility gates whether a relay becomes lit.  Once lit, its outbound segment
  ;; is live for crossing analysis; fixed coupled segments additionally require their
  ;; authored corridor to be clear.
  (or (and (transmitter ?from)
           (location ?to)
           (exists (?connector connector)
             (and (has-location ?connector ?to)
                  (paired ?connector ?from))))
      (and (repeater ?from)
           (assoc ?from ?lighting)
           (or (and (or (repeater ?to) (receiver ?to))
                    (fixed-beam-corridor-clear ?from ?to))
               (and (location ?to)
                    (exists (?connector connector)
                      (and (has-location ?connector ?to)
                           (paired ?connector ?from))))))
      (and (location ?from)
           (exists (?connector connector)
             (and (has-location ?connector ?from)
                  (assoc ?connector ?lighting)
                  (or (and (receiver ?to)
                           (paired ?connector ?to))
                      (and (repeater ?to)
                           (paired ?connector ?to))
                      (and (location ?to)
                           (exists (?other connector)
                             (and (different ?other ?connector)
                                  (has-location ?other ?to)
                                  (or (paired ?other ?connector)
                                      (paired ?connector ?other)))))))))))


(define-query beam-relay-source-distance (?from beam-node ?lighting)
  (if (repeater ?from)
    (do (assign $record (assoc ?from ?lighting))
        (if $record (third $record) most-positive-fixnum))
    (if (location ?from)
      (do (assign $distance most-positive-fixnum)
          (doall (?connector connector)
            (if (has-location ?connector ?from)
              (do (assign $record (assoc ?connector ?lighting))
                  (if $record
                    (assign $distance (third $record))))))
          $distance)
      most-positive-fixnum)))


(define-query connectable-terminus
    (?agent agent
     ?vantage location
     ?connector connector
     ?terminus terminus)
  ;; Selection uses structural LOS (gates ignored) from the agent's current location.
  ;; Exact placement and live visibility later decide whether the beam carries color.
  ;; A terminus already linked to or selected by the connector is not offered again.
  ;; A connector terminus must stand somewhere (not held) and have room under
  ;; *max-connector-pairings*.  A same-location connector pair can carry a beam only
  ;; with both connectors active there, which the state invariant discards.
  (and (connector-pairing-allowed ?agent ?connector ?terminus)
       (not (paired ?connector ?terminus))
       (not (selected ?connector ?terminus))
       (or (and (or (transmitter ?terminus)
                    (receiver ?terminus)
                    (repeater ?terminus))
                (potentially-visible ?vantage ?terminus))
           (and (connector ?terminus)
                (different ?terminus ?connector)
                (not (paired ?terminus ?connector))
                (< (connector-link-count ?terminus) *max-connector-pairings*)
                (bind (has-location ?terminus $terminus-location))
                (potentially-visible ?vantage $terminus-location)))))


(define-query connector-link-count (?connector connector)
  ;; Every PAIRED or SELECTED fact naming the connector, on either side.  Retained links
  ;; count, and so do pending selections, which become links at put-down.
  (do (assign $count 0)
      (doall (?terminus terminus)
        (do (if (paired ?connector ?terminus)
              (assign $count (1+ $count)))
            (if (selected ?connector ?terminus)
              (assign $count (1+ $count)))))
      (doall (?other connector)
        (do (if (paired ?other ?connector)
              (assign $count (1+ $count)))
            (if (selected ?other ?connector)
              (assign $count (1+ $count)))))
      $count))
