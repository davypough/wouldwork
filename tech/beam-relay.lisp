;;; Filename: beam-relay.lisp

;;; Relay beam technology: movable connectors and fixed repeaters propagate beam color
;;; through one graph.  Connector links are dynamic PAIRED facts and require authored live
;;; visibility; fixed apparatus links are directional COUPLED facts and use BEAM-VIA
;;; corridors.  A relay lights only when every source reaching it in the same propagation
;;; layer carries one hue.  Conflicting hues leave it unlit.
;;;
;;; Connectors retain their pickup/placement/pairing actions.  Repeaters have no
;;; HAS-LOCATION and no actions; repeater.lisp assembles this peer with beam-direct and
;;; visibility so fixed corridors and apparatus sightlines are live.
;;;
;;; REQUIRES:
;;;   types      : agent, location -- connector/transmitter/receiver/repeater and related
;;;                leaf types are optional through the nested roles
;;;   nested     : -beam-substrate (beam relations and hooks); -placement;
;;;                -visibility (null defaults); -support-elevation; -elevation;
;;;                -mobility; -reachability; -pickup
;;;   parameter  : *max-pairings* -- defaults to 3; connector pairings only; fixed
;;;                couplings are unlimited; a problem may set a smaller value first
;;;   driver     : propagate-consequences! must call
;;;                  update-relay-status! -> update-receiver-status!
;;; PROVIDES:
;;;   types      : relay (either connector floor-repeater wall-repeater);
;;;                terminus (either transmitter receiver connector floor-repeater
;;;                wall-repeater)
;;;   relations  : paired, color
;;;   queries    : relay-beam-reaches-receiver,
;;;                recording-shadow-relay-beam-reaches-receiver,
;;;                compute-relay-lighting, compute-relay-lighting-for-object,
;;;                relay-anchor, relay-linked,
;;;                relay-link-clear, relay-link-clear-for-object,
;;;                paired-relay-visible, paired-relay-visible-for-object,
;;;                relay-beam-live-for-cutting, beam-relay-source-distance,
;;;                connectable-location, connectable-terminus
;;;   updates    : update-relay-status!
;;;   actions    : pickup-connector, put-connector, connect-connector

(include-tech -propagation)
(include-tech -beam-substrate)
(include-tech -placement)
(include-tech -visibility)
(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -elevation)
(include-tech -mobility)
(include-tech -reachability)
(include-tech -pickup)
(include-tech -beam-relay-init-checks)
(include-tech -recorder-fork-registry)

(in-package :ww)


(setf *max-pairings* (or *max-pairings* 3))


(define-types
  relay (either connector floor-repeater wall-repeater)
  terminus
    (either transmitter receiver connector floor-repeater wall-repeater))


(define-optional-types box hue connector transmitter receiver)


(define-dynamic-relations
  (paired connector terminus)
  (color relay $hue))


(define-derived-relations
  color)


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
                  (not (paired ?c ?connector)))))
          (if (bind (on ?connector $support))
            (not (on ?connector $support)))
          (finally (propagate-changes!))))


(define-action put-connector
  1
  (?agent agent ?connector connector ?location location)
  (and (holding ?agent ?connector)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?connector)))
  (">" ?agent "puts" ?connector "at" ?location "on" $place "without pairings")
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object!
                        ?agent ?connector ?location $placement-option)
                      (finally (propagate-changes!)))))


(define-action connect-connector
  1
  (?agent agent ?location location)
  (and (bind (holding ?agent $connector))
       (connector $connector)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (connectable-location $connector ?location)
       (assign $places (placement-options ?agent ?location $connector))
       (assign $pairing-vantages (mobility-locations ?agent $a-location))
        (exists (?t terminus)
          (connectable-terminus
            ?agent $pairing-vantages ?location $connector ?t)))
  (">" ?agent "connects" $connector "at" ?location "on" $place "to" $termini)
  (do (assign $connectable nil)
      (doall (?terminus terminus)
        (if (connectable-terminus
              ?agent $pairing-vantages ?location $connector ?terminus)
          (assign $connectable (cons ?terminus $connectable))))
      (ww-loop for $selected-termini in
                 (rest (subsets-up-to $connectable *max-pairings*))
               do (ww-loop for $placement-option in $places
                           do (assert
                                (assign $termini $selected-termini)
                                (assign $place $placement-option)
                                (place-held-object!
                                  ?agent $connector ?location $placement-option)
                                (ww-loop for $terminus in $selected-termini
                                         do (paired $connector $terminus))
                                (finally (propagate-changes!)))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update update-relay-status! ()
  (do (assign $lighting (compute-relay-lighting (current-crossing-set)))
      (doall (?relay relay)
        (do (assign $record (assoc ?relay $lighting))
            (if $record
              (color ?relay (second $record))
              (if (bind (color ?relay $old-hue))
                (not (color ?relay $old-hue))))))))


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
      (doall (?relay relay)
        (if (and (relay-available-for-object ?view ?relay)
                 (assign $record (assoc ?relay ?lighting))
                 $record
                 (bind (has-chroma ?receiver $required-hue))
                 (eql (second $record) $required-hue)
                 (or (and (connector ?relay)
                          (paired ?relay ?receiver)
                          (bind (has-location ?relay $location))
                          (beam-visible-for-object
                            ?view $location (top ?relay)
                            ?receiver (top ?receiver)))
                     (and (repeater ?relay)
                          (coupled ?relay ?receiver)
                          (fixed-beam-corridor-clear-for-object
                            ?view ?relay ?receiver))))
          (assign $reaches t)))
      $reaches))


(define-query compute-relay-lighting (?active)
  (compute-relay-lighting-for-object nil ?active))


(define-query compute-relay-lighting-for-object (?view ?active)
  ;; Breadth-first propagation from every transmitter.  Each lighting record is
  ;; (relay hue distance); frontier records additionally carry the relay's beam endpoint.
  (do (assign $lit nil)
      (assign $lit-locations nil)
      (assign $visited nil)
      (assign $frontier nil)
      (doall (?transmitter transmitter)
        (if (bind (has-chroma ?transmitter $hue))
          (assign $frontier
                  (cons (list ?transmitter ?transmitter $hue 0) $frontier))))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (doall (?target relay)
                    (if (and (relay-available-for-object ?view ?target)
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
                                      (if (and (not (cdr $hues))
                                               (or (repeater ?target)
                                                   (not (member
                                                          $target-anchor
                                                          $lit-locations))))
                                        (do (assign $lit
                                                    (cons
                                                      (list ?target
                                                            $reach-hue
                                                            $reach-distance)
                                                      $lit))
                                            (if (connector ?target)
                                              (assign $lit-locations
                                                      (cons
                                                        $target-anchor
                                                        $lit-locations)))
                                            (assign $next-frontier
                                                    (cons
                                                      (list ?target
                                                            $target-anchor
                                                            $reach-hue
                                                            $reach-distance)
                                                      $next-frontier)))))))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $lit))


(define-query relay-available-for-object (?view ?relay relay)
  (or (not (recording-shadow-object ?view))
      (recording-shadow-object-present ?relay)))


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
  ;; orient the test from whichever endpoint is the connector.
  (if (connector ?target)
    (beam-visible-for-object
      ?view ?target-anchor (top ?target)
      ?source-anchor (top ?source))
    (do (connector ?source)
        (beam-visible-for-object
          ?view ?source-anchor (top ?source)
          ?target-anchor (top ?target)))))


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


(define-query connectable-location (?connector connector ?location location)
  (not (exists (?other connector)
         (and (different ?other ?connector)
              (has-location ?other ?location)
              (bind (color ?other $hue))))))


(define-query connectable-terminus
    (?agent agent
     ?pairing-vantages
     ?placement-location location
     ?connector connector
     ?terminus terminus)
  ;; Pairing selection uses structural LOS from any currently traversable vantage.  Exact
  ;; placement and live visibility subsequently determine whether the beam carries color.
  (and (connector-pairing-allowed ?agent ?connector ?terminus)
       (ww-loop for $vantage in ?pairing-vantages
                thereis
                  (or (and (or (transmitter ?terminus)
                               (receiver ?terminus)
                               (repeater ?terminus))
                           (potentially-visible $vantage ?terminus))
                      (and (connector ?terminus)
                           (different ?terminus ?connector)
                           (bind (has-location ?terminus $terminus-location))
                           (different ?placement-location $terminus-location)
                           (potentially-visible $vantage $terminus-location))))))
