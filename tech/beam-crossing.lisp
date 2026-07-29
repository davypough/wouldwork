;;; Filename: beam-crossing.lisp

;;; Beam crossing technology: crossing points that cut beams when both authored beam
;;; segments reach the crossing.  A peer over -beam-substrate, adding only crossing-
;;; specific behavior; it does NOT pull in the direct beam line, so a problem wanting
;;; a direct line must include beam-direct as well.  If beam-relay is also included,
;;; relay beams participate through relay hook queries supplied by beam-relay.
;;; BEAM-CROSSING> is derived internally, lazily and once, from whichever facts populate
;;; CROSSINGS-ALONG-BEAM> -- hand-authored directly, or computed from coordinates via the
;;; nested -beam-crossing-coordinates substrate (see that file; problem-corner-topo uses the
;;; coordinate path).  A problem may still assert
;;; BEAM-CROSSING> itself; it is simply never read.
;;;
;;; Self-contained; spliced by (include-tech beam-crossing).
;;;
;;; REQUIRES:
;;;   types     : crossing is declared optional by the nested -beam-crossing-coordinates
;;;               substrate and stays empty; the pool is minted at init time and reached
;;;               through CURRENT-CROSSINGS>/GET-CURRENT-CROSSINGS rather than through the
;;;               type extension, so no problem declares it; beam-endpoint is declared by the nested
;;;               -beam-los-coordinates substrate (via -beam-crossing-coordinates);
;;;               transmitter is declared optional here (define-optional-types); gate comes
;;;               from nested -gate
;;;   driver    : propagate-consequences! must call update-crossing-status! before
;;;               update-connector-status! and/or update-receiver-status!
;;; PROVIDES:
;;;   nested    : -beam-crossing-coordinates (optional coordinate-based CROSSINGS-ALONG-BEAM>/
;;;               CROSSINGS-BEFORE-GATE> input; itself nests -beam-los-coordinates for
;;;               BEAM-ENDPOINT, APPARATUS-COORDS>, WALL-SEGMENTS, GATE-SEGMENTS,
;;;               BOUNDARY-WALL, and LOS derivation);
;;;               -gate (gate optional type, (open gate) relation) -- shared with gate,
;;;               accessibility (via -passability), reachability, visibility, and
;;;               beam-direct, which all nest -gate instead of hand-declaring it
;;;   types     : transmitter  --  declared optional here; other techs (gate,
;;;               accessibility, visibility, reachability, beam-direct, -beam-substrate,
;;;               beam-relay, etc.) independently declare their own transmitter-alias
;;;               for their own pre-params; the bare and aliased forms resolve compatibly
;;;   relations : crossing-active, beam-crossing>, crossings-along-beam>,
;;;               crossings-before-gate>, current-crossings>
;;;   queries   : get-current-crossings, current-crossing-set, beam-reaches-crossing,
;;;               compute-active-crossings, arbitrate-crossings, crossing-reaches,
;;;               crossing-priority, beam-source-distance, same-crossing-set,
;;;               beam-cut, beam-cut-in, beam-crossing-endpoints
;;;   update    : update-crossing-status!

(include-tech -propagation)
(include-tech -beam-substrate)
(include-tech -beam-crossing-coordinates)
(include-tech -gate)

(in-package :ww)


(define-optional-types transmitter)


(define-dynamic-relations
  (crossing-active crossing))


(define-static-relations
  (beam-crossing> crossing $beam-endpoint $beam-endpoint $beam-endpoint $beam-endpoint)
  (crossings-along-beam> beam-endpoint $list beam-endpoint)
  (crossings-before-gate> beam-endpoint $list gate beam-endpoint)
  (current-crossings> $list))  ;the crossing pool itself, in crossing1, crossing2, ... order


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
        (doall (?x (get-current-crossings))
          (if (member ?x $active)
            (crossing-active ?x)
            (not (crossing-active ?x)))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query get-current-crossings ()
  ;; The crossing pool, as a runtime lookup rather than a compile-time type extension.
  ;; Every DOALL over crossings in this file iterates (get-current-crossings) instead of
  ;; the bare CROSSING type, because TRANSLATE-DOALL resolves a bare type name into a
  ;; literal domain at load time -- when INSTALL-QUERY calls TRANSLATE -- and the pool
  ;; isn't known until -beam-crossing-coordinates' ESTABLISH-BEAM-COORDINATES has computed
  ;; the geometry during init.  Naming a query in the type slot selects TRANSLATE-DOALL's
  ;; other branch, which evaluates the domain against state on each call.  This must
  ;; therefore stay a DEFINE-QUERY: the branch is selected by membership in *QUERY-NAMES*,
  ;; so demoting it to a plain DEFUN would silently revert every caller to an empty
  ;; compile-time domain.  Returns nil when nothing has asserted CURRENT-CROSSINGS>, which
  ;; makes each such DOALL skip its body -- the same inert behavior a problem with no
  ;; crossings gets today.
  (do (bind (current-crossings> $crossings))
      $crossings))


(define-query current-crossing-set ()
  (do (assign $active nil)
      (doall (?x (get-current-crossings))
        (if (crossing-active ?x)
          (assign $active (cons ?x $active))))
      $active))


(define-query beam-reaches-crossing
    (?from beam-endpoint
     ?to beam-endpoint
     ?xing crossing
     ?active
     ?lighting)
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
      (doall (?x (get-current-crossings))
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
                  (doall (?x (get-current-crossings))
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


(defparameter *beam-crossing-cache* nil
  "Hash table mapping a crossing to its (from1 to1 from2 to2) endpoint list, lazily
   derived from CROSSINGS-ALONG-BEAM> on first use and memoized for the rest of the run --
   CROSSINGS-ALONG-BEAM> is static and never changes after initialization, so the mapping
   never needs to be recomputed.")


(define-query beam-crossing-endpoints (?xing crossing)
  ;; Returns (values from1 to1 from2 to2) for ?xing -- the two beams that meet there --
  ;; equivalent to (bind (beam-crossing> ?xing from1 to1 from2 to2)), but derived from
  ;; CROSSINGS-ALONG-BEAM> regardless of whether BEAM-CROSSING> itself was ever authored.
  (do (ensure-beam-crossing-cache)
      (assign $endpoints (gethash ?xing *beam-crossing-cache*))
      (values (first $endpoints) (second $endpoints) (third $endpoints) (fourth $endpoints))))


(define-query ensure-beam-crossing-cache ()
  ;; Populates *beam-crossing-cache* once, from the already-authored CROSSINGS-ALONG-BEAM>
  ;; facts, purely symbolically -- no coordinates.  For each declared crossing, finds the
  ;; (canonical) beams whose crossings-along-beam> list mentions it; errors if that count
  ;; isn't exactly 2, which would indicate crossings-along-beam> itself is inconsistent.
  (if (not *beam-crossing-cache*)
    (do (assign $beams (beam-crossing-canonical-beams))
        (assign $cache (make-hash-table :test 'eq))
        (doall (?crossing (get-current-crossings))
          (do (assign $containing (beam-crossing-beams-for-crossing ?crossing $beams))
              (if (/= (length $containing) 2)
                (error "Crossing ~A appears on ~A canonical beam(s); expected exactly 2."
                       ?crossing (length $containing)))
              (assign $beam1 (first $containing))
              (assign $beam2 (second $containing))
              (setf (gethash ?crossing $cache)
                    (list (first $beam1) (second $beam1) (first $beam2) (second $beam2)))))
        (setf *beam-crossing-cache* $cache))))


(define-query beam-crossing-canonical-beams ()
  ;; Every (from . to) pair with an authored CROSSINGS-ALONG-BEAM> entry, keeping only one
  ;; direction for beams authored bidirectionally.  Bidirectional authoring (both (from to)
  ;; and (to from) present) is detected structurally -- by the presence of the reverse
  ;; entry -- rather than by endpoint type, so this works for any beam-endpoint composition
  ;; a problem declares.  Errors if a bidirectional pair's two authored lists disagree.
  (do (assign $beams nil)
      (doall (?from beam-endpoint)
        (doall (?to beam-endpoint)
          (if (bind (crossings-along-beam> ?from $ids ?to))
            (do (assign $mirrored (bind (crossings-along-beam> ?to $reverse-ids ?from)))
                (if (or (not $mirrored)
                        (string< (symbol-name ?from) (symbol-name ?to)))
                  (do (if (and $mirrored (not (equal $ids (reverse $reverse-ids))))
                        (error "CROSSINGS-ALONG-BEAM> for ~A -> ~A and its reverse ~A -> ~A ~
                                disagree: ~A vs (reverse ~A)."
                               ?from ?to ?to ?from $ids $reverse-ids))
                      (push (list ?from ?to) $beams)))))))
      $beams))


(define-query beam-crossing-beams-for-crossing (?crossing crossing ?beams)
  ;; The canonical beams (drawn from ?beams) whose crossings-along-beam> list contains
  ;; ?crossing.  Correctly-authored data yields exactly two.
  (do (assign $containing nil)
      (ww-loop for $beam in ?beams
               do (assign $from (first $beam))
                  (assign $to (second $beam))
                  (bind (crossings-along-beam> $from $ids $to))
                  (if (member ?crossing $ids)
                    (push $beam $containing)))
      $containing))


(define-query crossing-reaches (?xing crossing ?active ?lighting)
  (do (mv-assign ($f1 $t1 $f2 $t2) (beam-crossing-endpoints ?xing))
      (and (beam-reaches-crossing $f1 $t1 ?xing ?active ?lighting)
           (beam-reaches-crossing $f2 $t2 ?xing ?active ?lighting))))


(define-query crossing-priority (?xing crossing ?lighting)
  (do (mv-assign ($f1 $t1 $f2 $t2) (beam-crossing-endpoints ?xing))
      (assign $d1 (beam-source-distance $f1 ?lighting))
      (assign $d2 (beam-source-distance $f2 ?lighting))
      (if (< $d1 $d2)
        $d2
        $d1)))


(define-query beam-source-distance (?from beam-endpoint ?lighting)
  (if (transmitter ?from)
    0
    (beam-relay-source-distance ?from ?lighting)))


(define-query same-crossing-set (?left ?right)
  (and (= (length ?left) (length ?right))
       (ww-loop for $crossing in ?left
                always (member $crossing ?right))))


(define-query beam-cut
    (?from (either transmitter location)
     ?to (either receiver location))
  ;; True iff some committed crossing on this directed beam currently cuts it.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (crossing-active $e)
                      (assign $cut t))))
      $cut))


(define-query beam-cut-in
    (?from (either transmitter location)
     ?to (either receiver location)
     ?active)
  ;; Candidate-set analog of beam-cut.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (member $e ?active)
                      (assign $cut t))))
      $cut))
