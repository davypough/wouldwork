;;; Filename: beam-relay.lisp

;;; Relay beam technology: movable connectors that can relay beam color through authored
;;; visibility.  A peer over -beam-substrate, adding only connector-specific behavior;
;;; it does NOT pull in the direct beam line, so a problem wanting both a direct
;;; transmitter -> receiver line and connectors must include beam-direct as well.  If
;;; beam-crossing is also included, connector lighting and receiver activation respect
;;; crossing cuts through the beam-cut / beam-cut-in hooks.
;;;
;;; Self-contained; spliced by (include-tech beam-relay).
;;;
;;; REQUIRES:
;;;   types      : agent, location  --  plate, box, hue, connector, transmitter, and
;;;                receiver are declared optional here (define-optional-types)
;;;   nested     : -beam-substrate (beam relations, receiver update, and peer hooks);
;;;                -holding (cargo, holding); -location (mobile-object, has-location);
;;;                -support-occupancy (support-occupant, support, on, cleartop);
;;;                -position (fixed-position-object, has-position);
;;;                -visibility (null-default visible interface);
;;;                -reachability (identity-default reachable query; overridden by
;;;                reachability when that technology is included)
;;;   parameter  : *max-pairings*
;;;   extension  : visibility overrides -visibility's null default with authored LOS
;;;   driver     : propagate-consequences! must call
;;;                  update-connector-status! -> update-receiver-status!
;;; PROVIDES:
;;;   types      : terminus (either transmitter receiver connector)  --  what a connector
;;;                can pair/connect to; owned here so problems using beam-relay need only
;;;                declare the leaf object types they instantiate
;;;                plate, box, hue, connector, transmitter, receiver  --  declared optional
;;;                here; other techs (plate, gate, jammer, box, barrier, -beam-substrate,
;;;                beam-direct, beam-crossing, visibility, etc.) independently declare their
;;;                own -alias forms for their own pre-params; the bare and aliased forms
;;;                resolve compatibly
;;;   relations  : paired, color
;;;   queries    : relay-beam-reaches-receiver, compute-connector-lighting,
;;;                relay-beam-live-for-cutting, beam-relay-source-distance,
;;;                connectable-location, connectable-terminus
;;;   update     : update-connector-status!
;;;   actions    : pickup-connector, connect-connector

(include-tech -beam-substrate)
(include-tech -holding)
(include-tech -location)
(include-tech -support-occupancy)
(include-tech -position)
(include-tech -visibility)
(include-tech -reachability)

(in-package :ww)


(define-types
  terminus (either transmitter receiver connector))  ;what a connector can pair/connect to


(define-optional-types plate box hue connector transmitter receiver)


(define-dynamic-relations
  (paired connector terminus)
  (color connector $hue))


;;;; ACTIONS ;;;;


(define-action pickup-connector
  1
  (?agent agent ?connector connector)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (has-location ?agent $a-location))
       (bind (has-location ?connector $connector-location))
       (reachable $connector-location $a-location))
  (":" ?agent "picks up" ?connector "at" $a-location)
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


(define-action connect-connector
  1
  (?agent agent ?location location)
  (and (bind (holding ?agent $connector))
       (connector $connector)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (connectable-location $connector ?location)
       (exists (?t terminus)
         (connectable-terminus ?location $connector ?t)))
  (":" ?agent "connects" $connector "at" ?location "on" $place "to" $termini)
  (do (assign $connectable nil)
      (doall (?terminus terminus)
        (if (connectable-terminus ?location $connector ?terminus)
          (assign $connectable (cons ?terminus $connectable))))
      (ww-loop for $termini in (rest (subsets-up-to $connectable *max-pairings*))
               do (do (doall (?plate plate)
                        (if (and (has-position ?plate ?location)
                                 (cleartop ?plate))
                          (assert (not (holding ?agent $connector))
                                  (has-location $connector ?location)
                                  (on $connector ?plate)
                                  (assign $place ?plate)
                                  (ww-loop for $terminus in $termini
                                           do (paired $connector $terminus))
                                  (finally (propagate-changes!)))))
                      (doall (?support-box box)
                        (if (and (has-location ?support-box ?location)
                                 (cleartop ?support-box))
                          (assert (not (holding ?agent $connector))
                                  (has-location $connector ?location)
                                  (on $connector ?support-box)
                                  (assign $place ?support-box)
                                  (ww-loop for $terminus in $termini
                                           do (paired $connector $terminus))
                                  (finally (propagate-changes!)))))
                      (assert (not (holding ?agent $connector))
                              (has-location $connector ?location)
                              (assign $place 'ground)
                              (ww-loop for $terminus in $termini
                                       do (paired $connector $terminus))
                              (finally (propagate-changes!)))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update update-connector-status! ()
  (do (assign $lighting (compute-connector-lighting (current-crossing-set)))
      (doall (?c connector)
        (do (assign $record (assoc ?c $lighting))
            (if $record
              (color ?c (second $record))
              (if (bind (color ?c $old-hue))
                (not (color ?c $old-hue))))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query relay-beam-reaches-receiver (?receiver receiver)
  ;; A receiver lights by relay when a lit connector is paired to it with clear visibility.
  ;; beam-cut is a no-op unless beam-crossing is included.
  (do (assign $reaches nil)
      (doall (?c connector)
        (if (and (paired ?c ?receiver)
                 (bind (color ?c $c-hue))
                 (bind (has-chroma ?receiver $required-hue))
                 (eql $c-hue $required-hue)
                 (bind (has-location ?c $c-loc))
                 (visible $c-loc ?receiver)
                 (not (beam-cut $c-loc ?receiver)))
          (assign $reaches t)))
      $reaches))


(define-query compute-connector-lighting (?active)
  ;; BFS from every transmitter over the pairing graph, returning lit connectors as
  ;; ($connector $hue $distance) records.  If beam-crossing is included, ?active is the
  ;; candidate crossing set used by beam-cut-in; otherwise the cut hook is a no-op.
  (do (assign $lit nil)
      (assign $lit-locations nil)
      (assign $visited nil)
      (assign $frontier nil)
      (doall (?tr transmitter)
        (if (bind (has-chroma ?tr $tr-hue))
          (assign $frontier (cons (list ?tr ?tr $tr-hue 0) $frontier))))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (doall (?c connector)
                    (if (and (not (member ?c $visited))
                             (bind (has-location ?c $c-loc)))
                      (do (assign $hues nil)
                          (assign $reach-hue nil)
                          (assign $reach-dist nil)
                          (ww-loop for $source-rec in $frontier
                                   do (assign $src-obj (first $source-rec))
                                      (assign $src-anchor (second $source-rec))
                                      (assign $src-hue (third $source-rec))
                                      (assign $src-dist (fourth $source-rec))
                                      (if (and (or (paired ?c $src-obj)
                                                   (paired $src-obj ?c))
                                               (visible $c-loc $src-anchor)
                                               (not (beam-cut-in $src-anchor $c-loc ?active)))
                                        (do (if (not (member $src-hue $hues))
                                              (assign $hues (cons $src-hue $hues)))
                                            (assign $reach-hue $src-hue)
                                            (assign $reach-dist (1+ $src-dist)))))
                          (if $hues
                            (do (assign $visited (cons ?c $visited))
                                (if (and (not (cdr $hues))
                                         (not (member $c-loc $lit-locations)))
                                  (do (assign $lit
                                              (cons (list ?c $reach-hue $reach-dist) $lit))
                                      (assign $lit-locations
                                              (cons $c-loc $lit-locations))
                                      (assign $next-frontier
                                              (cons (list ?c
                                                          $c-loc
                                                          $reach-hue
                                                          $reach-dist)
                                                    $next-frontier)))))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $lit))


(define-query relay-beam-live-for-cutting (?from ?to ?lighting)
  ;; Relay beams cut crossings once emitted.  The outbound sightline is not tested here;
  ;; arrival gates lighting in compute-connector-lighting, not cutting.
  (or (and (transmitter ?from)
           (exists (?c connector)
             (and (has-location ?c ?to)
                  (paired ?c ?from))))
      (exists (?c connector)
        (and (has-location ?c ?from)
             (assoc ?c ?lighting)
             (or (and (receiver ?to)
                      (paired ?c ?to))
                 (exists (?c2 connector)
                   (and (has-location ?c2 ?to)
                        (different ?c2 ?c)
                        (or (paired ?c2 ?c)
                            (paired ?c ?c2)))))))))


(define-query beam-relay-source-distance (?from ?lighting)
  (do (assign $distance most-positive-fixnum)
      (doall (?c connector)
        (if (has-location ?c ?from)
          (do (assign $record (assoc ?c ?lighting))
              (if $record
                (assign $distance (third $record))))))
      $distance))


(define-query connectable-location (?connector connector ?location location)
  (not (exists (?other connector)
         (and (different ?other ?connector)
              (has-location ?other ?location)
              (bind (color ?other $hue))))))


(define-query connectable-terminus (?location location ?connector connector ?terminus terminus)
  (or (and (or (transmitter ?terminus)
               (receiver ?terminus))
           (visible ?location ?terminus))
      (and (connector ?terminus)
           (different ?terminus ?connector)
           (bind (has-location ?terminus $t-loc))
           (different ?location $t-loc)
           (visible ?location $t-loc))))
