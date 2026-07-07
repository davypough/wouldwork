;;; Filename: beam-crossing.lisp

;;; Beam crossing technology: crossing points that cut beams when both authored beam
;;; segments reach the crossing.  A peer over -beam-substrate, adding only crossing-
;;; specific behavior; it does NOT pull in the direct beam line, so a problem wanting
;;; a direct line must include beam-direct as well.  If beam-relay is also included,
;;; relay beams participate through relay hook queries supplied by beam-relay.
;;;
;;; Self-contained; spliced by (include-tech beam-crossing).
;;;
;;; REQUIRES:
;;;   types     : crossing, beam-endpoint  --  gate and transmitter are declared optional
;;;               here (define-optional-types), gate coordinated with gate, accessibility,
;;;               visibility, reachability, and beam-direct, which all convert gate
;;;               together since they share the (open gate) relation verbatim
;;;   driver    : propagate-consequences! must call update-crossing-status! before
;;;               update-connector-status! and/or update-receiver-status!
;;; PROVIDES:
;;;   types     : gate, transmitter  --  declared optional here; other techs (gate,
;;;               accessibility, visibility, reachability, beam-direct, -beam-substrate,
;;;               beam-relay, etc.) independently declare their own gate-alias/
;;;               transmitter-alias for their own pre-params; the bare and aliased forms
;;;               resolve compatibly
;;;   relations : (open gate)  --  also declared identically by gate, accessibility,
;;;               visibility, reachability, and beam-direct; only gate's
;;;               update-gate-status! ever asserts it
;;;               crossing-active, beam-crossing>, crossings-along-beam>,
;;;               crossings-before-gate>
;;;   queries   : current-crossing-set, beam-reaches-crossing,
;;;               compute-active-crossings, arbitrate-crossings, crossing-reaches,
;;;               crossing-priority, beam-source-distance, same-crossing-set,
;;;               beam-cut, beam-cut-in
;;;   update    : update-crossing-status!

(include-tech -beam-substrate)

(in-package :ww)


(define-optional-types gate transmitter)


(define-dynamic-relations
  (open gate)  ;also declared by gate/accessibility/visibility/reachability/beam-direct; only gate writes it
  (crossing-active crossing))


(define-static-relations
  (beam-crossing> crossing $beam-endpoint $beam-endpoint $beam-endpoint $beam-endpoint)
  (crossings-along-beam> beam-endpoint $list beam-endpoint)
  (crossings-before-gate> beam-endpoint $list gate beam-endpoint))


;;;; UPDATE FUNCTIONS ;;;;


(define-update update-crossing-status! ()
  ;; Recompute this pass's active crossing set from a frozen candidate set.  Stored
  ;; crossing-active facts are changed only after a fixed point has been established.
  (do (assign $active nil)
      (assign $previous nil)
      (assign $have-previous nil)
      (assign $resolved nil)
      (ww-loop for $iteration from 1 to 10
               do (assign $next (compute-active-crossings $active))
                  (if (same-crossing-set $next $active)
                    (do (assign $active $next)
                        (assign $resolved t)
                        (return t))
                    (if (and $have-previous
                             (same-crossing-set $next $previous))
                      (do (assign $candidate (union $active $next))
                          (assign $validated (compute-active-crossings $candidate))
                          (if (same-crossing-set $validated $candidate)
                            (do (assign $active $candidate)
                                (assign $resolved t))
                            (do (assign $arbitrated (arbitrate-crossings $candidate))
                                (assign $arb-validated (compute-active-crossings $arbitrated))
                                (if (same-crossing-set $arb-validated $arbitrated)
                                  (do (assign $active $arbitrated)
                                      (assign $resolved t))
                                  (inconsistent-state))))
                          (return nil))
                      (do (assign $previous $active)
                          (assign $have-previous t)
                          (assign $active $next))))
               finally (inconsistent-state))
      (if $resolved
        (doall (?x crossing)
          (if (member ?x $active)
            (crossing-active ?x)
            (not (crossing-active ?x)))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query current-crossing-set ()
  (do (assign $active nil)
      (doall (?x crossing)
        (if (crossing-active ?x)
          (assign $active (cons ?x $active))))
      $active))


(define-query beam-reaches-crossing (?from ?to ?xing ?active ?lighting)
  ;; Resolve the live orientation first.  Fixed direct beams are live only in their
  ;; natural transmitter -> receiver orientation; relay beams are supplied by beam-relay.
  (do (assign $reaches nil)
      (assign $src nil)
      (assign $dst nil)
      (if (beam-live-for-cutting ?from ?to ?lighting)
        (do (assign $src ?from)
            (assign $dst ?to))
        (if (beam-live-for-cutting ?to ?from ?lighting)
          (do (assign $src ?to)
              (assign $dst ?from))))
      (if $src
        (do (assign $blocked nil)
            (assign $reached nil)
            (doall (?gate gate)
              (if (and (bind (crossings-before-gate> $src $before ?gate $dst))
                       (not (open ?gate))
                       (not (member ?xing $before)))
                (assign $blocked t)))
            (if (bind (crossings-along-beam> $src $ids $dst))
              (ww-loop for $e in $ids
                       do (if (eql $e ?xing)
                            (assign $reached t)
                            (if (and (not $reached)
                                     (member $e ?active))
                              (assign $blocked t)))))
            (if (not $blocked)
              (assign $reaches t))))
      $reaches))


(define-query compute-active-crossings (?active)
  (do (assign $lighting (compute-connector-lighting ?active))
      (assign $next nil)
      (doall (?x crossing)
        (if (crossing-reaches ?x ?active $lighting)
          (assign $next (cons ?x $next))))
      $next))


(define-query arbitrate-crossings (?candidate)
  ;; Resolve cascade-coupled candidate sets by distance priority.
  (do (assign $kept nil)
      (assign $remaining ?candidate)
      (ww-loop for $round from 1 to (length ?candidate)
               do (assign $lighting (compute-connector-lighting $kept))
                  (assign $best nil)
                  (assign $best-priority most-positive-fixnum)
                  (doall (?x crossing)
                    (if (and (member ?x $remaining)
                             (crossing-reaches ?x $kept $lighting))
                      (do (assign $priority (crossing-priority ?x $lighting))
                          (if (or (< $priority $best-priority)
                                  (and (= $priority $best-priority)
                                       (or (not $best)
                                           (string< (symbol-name ?x) (symbol-name $best)))))
                            (do (assign $best ?x)
                                (assign $best-priority $priority))))))
                  (if (not $best)
                    (return t)
                    (do (assign $kept (cons $best $kept))
                        (assign $remaining (remove $best $remaining)))))
      $kept))


(define-query crossing-reaches (?xing ?active ?lighting)
  (and (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
       (beam-reaches-crossing $f1 $t1 ?xing ?active ?lighting)
       (beam-reaches-crossing $f2 $t2 ?xing ?active ?lighting)))


(define-query crossing-priority (?xing ?lighting)
  (do (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
      (assign $d1 (beam-source-distance $f1 ?lighting))
      (assign $d2 (beam-source-distance $f2 ?lighting))
      (if (< $d1 $d2)
        $d2
        $d1)))


(define-query beam-source-distance (?from ?lighting)
  (if (transmitter ?from)
    0
    (beam-relay-source-distance ?from ?lighting)))


(define-query same-crossing-set (?left ?right)
  (and (= (length ?left) (length ?right))
       (ww-loop for $crossing in ?left
                always (member $crossing ?right))))


(define-query beam-cut (?from ?to)
  ;; True iff some committed crossing on this directed beam currently cuts it.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (crossing-active $e)
                      (assign $cut t))))
      $cut))


(define-query beam-cut-in (?from ?to ?active)
  ;; Candidate-set analog of beam-cut.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (member $e ?active)
                      (assign $cut t))))
      $cut))
