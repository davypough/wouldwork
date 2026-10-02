;;; Filename: problem-beam-crossing-cascade-test.lisp

;;; Dedicated zero-action regression for beam-crossing's own cascading arbitration --
;;; COMPUTE-ACTIVE-BEAM-CROSSINGS' fixpoint and ARBITRATE-BEAM-CROSSINGS' priority tie-break --
;;; which no existing test reaches, since every other beam-crossing fixture has at most
;;; one crossing whose two beams have nothing else in their way.
;;;
;;; Four beams form the four sides of a square, each extended one unit past its two
;;; nominal corners so every side properly crosses both of its neighbors (a strict
;;; interior intersection for both beams) while the two pairs of opposite sides stay
;;; exactly parallel and never cross.  This yields exactly four crossings, one per
;;; corner, and every beam's own two crossings are ordered -- by distance from that
;;; beam's transmitter -- so each corner blocks the next going around the square:
;;; SOUTHWEST blocks SOUTHEAST, SOUTHEAST blocks NORTHEAST, NORTHEAST blocks NORTHWEST,
;;; NORTHWEST blocks SOUTHWEST.
;;;
;;; Tracing UPDATE-CROSSING-STATUS! by hand: computing from no assumptions at all finds
;;; every crossing reachable, and computing again from that full set blocks all four --
;;; an exact repeat of the empty set two passes back, so the fixpoint loop detects a
;;; genuine oscillation and calls ARBITRATE-BEAM-CROSSINGS on the full four.
;;;
;;; An earlier version of this file made all four beams plain direct links, so every
;;; crossing tied at priority zero and ARBITRATE-BEAM-CROSSINGS fell back to its alphabetical
;;; tie-break alone -- and that fallback cannot resolve this shape.  The two
;;; lowest-numbered crossings are always the two corners of whichever beam sorts first
;;; alphabetically, and those two corners always share that beam, so they always have
;;; exactly one direct block edge between them.  ARBITRATE-BEAM-CROSSINGS only checks blocking
;;; going forward: it keeps the lower-numbered corner in round one and never revisits it
;;; once the higher-numbered one is kept too, so both ride into the kept set even though
;;; one truly excludes the other.  Hand-tracing every distinct edge orientation confirms
;;; this is a structural limit of the alphabetical fallback for a four-way loop, not a
;;; quirk of one naming choice -- and staging that all-direct version confirms it in
;;; practice: it reaches VALIDATE-START-STATE-CONSISTENCY's "Initial state is
;;; inconsistent" error, because the three-crossing set ARBITRATE-BEAM-CROSSINGS greedily
;;; keeps does not survive its own re-validation.
;;;
;;; The SOUTHEAST beam is therefore relayed through REPEATER-SE, planted at (10,5),
;;; between its own two crossings on that line -- the SOUTHEAST corner at (10,0) and the
;;; NORTHEAST corner at (10,10).  BEAM-RELAY-SOURCE-DISTANCE now reads 1 for the segment
;;; leaving the repeater, so the NORTHEAST corner (the far end of the relayed leg)
;;; carries priority 1 while the other three crossings stay at priority 0.  Retracing
;;; ARBITRATE-BEAM-CROSSINGS: round one still keeps NORTHWEST (tied at 0, alphabetically first
;;; among the three still at priority 0).  Round two compares the two crossings still
;;; reachable -- NORTHEAST at priority 1 and SOUTHEAST at priority 0 -- and keeps
;;; SOUTHEAST on the numeric branch, not the alphabetical one.  Round three finds nothing
;;; left reachable.  The kept pair, {NORTHWEST, SOUTHEAST}, re-validates as its own fixed
;;; point, so the loop resolves cleanly.  This is the one part of the cascade a pure
;;; direct-beam version could never exercise: the priority branch of the tie-break
;;; actually deciding an outcome, rather than its alphabetical fallback.
;;;
;;; Once SOUTHEAST's own corner is kept active, it cuts the beam feeding REPEATER-SE
;;; (its only incoming link), so the repeater goes dark -- it never acquires a color --
;;; and RECEIVER-SE stays inactive for that reason rather than because its own outgoing
;;; segment is directly cut.  The other three beams are each cut by one of the two kept
;;; corners in the ordinary way, so all four receivers stay inactive.  Initial and final
;;; dynamic states are identical (only the derived CROSSING-ACTIVE, COLOR, and ACTIVE
;;; facts change, and those are established by the ordinary init-action rather than by a
;;; plan step).  Expected minimum path length: zero.
;;;
;;; A CHARACTERIZATION HELPER below re-derives the crossing-active facts on a copy of
;;; state via UPDATE-CROSSING-STATUS!, mirroring
;;; problem-beam-crossing-deadlock-test.lisp's own idiom, rather than trusting only the
;;; facts the init-action already baked in.

(in-package :ww)

(ww-set *problem-name* beam-crossing-cascade-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent         (agent1)
  location      (loc1)
  transmitter   (transmitter-sw transmitter-se transmitter-ne transmitter-nw)
  receiver      (receiver-sw receiver-se receiver-ne receiver-nw)
  wall-repeater (repeater-se)
  gate          (unused-gate)
  hue           (violet)
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; repeater brings in beam-direct, beam-relay, and visibility together.  visibility is
;;;; required even though this problem has no sightlines: beam-crossing nests
;;;; -beam-crossing-coordinates, which nests -beam-los-coordinates, whose
;;;; DERIVE-LOS-FROM-SEGMENTS references LOS-VIA -- a relation only visibility declares.
;;;; It stays inert here since no segment-barrier geometry is authored.


(include-tech repeater)
(include-tech beam-crossing)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 loc1)

  ;; One shared hue: only the crossing cascade is under test, not chroma matching.
  (has-chroma transmitter-sw violet)  (has-chroma receiver-sw violet)
  (has-chroma transmitter-se violet)  (has-chroma receiver-se violet)
  (has-chroma transmitter-ne violet)  (has-chroma receiver-ne violet)
  (has-chroma transmitter-nw violet)  (has-chroma receiver-nw violet)

  ;; Empty corridors: every beam is live for cutting purely on crossing outcome.  The
  ;; southeast beam is relayed through REPEATER-SE instead of coupled straight through,
  ;; so its far crossing carries a nonzero BEAM-RELAY-SOURCE-DISTANCE (see header).
  (coupled transmitter-sw receiver-sw)  (beam-via transmitter-sw () receiver-sw)
  (coupled transmitter-se repeater-se)  (beam-via transmitter-se () repeater-se)
  (coupled repeater-se receiver-se)     (beam-via repeater-se () receiver-se)
  (coupled transmitter-ne receiver-ne)  (beam-via transmitter-ne () receiver-ne)
  (coupled transmitter-nw receiver-nw)  (beam-via transmitter-nw () receiver-nw)

  ;; The square's four sides, each extended one unit past its own two corners so it
  ;; properly crosses both neighbors.  Bottom (y=0): west corner (0,0) is 1 unit from
  ;; the transmitter, east corner (10,0) is 11 units away, so west blocks east.  Right,
  ;; top, and left repeat the same pattern rotated one corner further each time.
  (apparatus-coords> transmitter-sw -1 0)   (apparatus-coords> receiver-sw 11 0)
  (apparatus-coords> transmitter-se 10 -1)  (apparatus-coords> receiver-se 10 11)
  (apparatus-coords> transmitter-ne 11 10)  (apparatus-coords> receiver-ne -1 10)
  (apparatus-coords> transmitter-nw 0 11)   (apparatus-coords> receiver-nw 0 -1)

  ;; Planted between the southeast beam's own two crossings -- (10,0) and (10,10) -- so
  ;; each stays on its original segment: the near one before REPEATER-SE, the far one
  ;; after it.
  (apparatus-coords> repeater-se 10 5)

  ;; Off every beam; required regardless of whether any beam reaches it.
  (location-coords> loc1 5 5)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION HELPER ;;;;


(define-test-helper beam-crossing-cascade-recomputed-valid-p (state)
  "Confirm that re-running UPDATE-CROSSING-STATUS! on a copy of STATE reproduces the
   same crossing-active facts the init-action already established, and leaves the real
   STATE untouched.  A broken ARBITRATE-BEAM-CROSSINGS changes the kept set on
   recomputation -- either landing on a different but self-consistent set, or failing
   its own re-validation and marking the copy inconsistent -- so either symptom is
  caught here without needing to predict which one occurs."
  (let* ((before (database state))
         (trial (copy-problem-state state)))
    (funcall 'update-crossing-status! trial)
    (and (equal (database trial) before)
         (not (state-is-inconsistent trial))
         (equal (database state) before))))


(define-test-claim beam-crossing-cascade-recomputation
  (beam-crossing-cascade-recomputed-valid-p *start-state*))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-crossing-cascade-scenario-valid ()
  (and
    (= (length (get-current-beam-crossings)) 4)
    (= (length (current-crossing-set)) 2)
    (beam-cut transmitter-sw receiver-sw)
    (beam-cut transmitter-se repeater-se)
    (beam-cut transmitter-ne receiver-ne)
    (beam-cut transmitter-nw receiver-nw)
    (not (exists (?h hue) (color repeater-se ?h)))
    (not (active receiver-sw))
    (not (active receiver-se))
    (not (active receiver-ne))
    (not (active receiver-nw))))


(define-goal
  (beam-crossing-cascade-scenario-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation beam-crossing-alphabetical-arbitration arbitrate-beam-crossings
  (?candidate)
  (do (assign $kept nil)
      (assign $remaining ?candidate)
      (ww-loop for $round from 1 to (length ?candidate)
               do (assign $lighting (compute-relay-lighting $kept))
                  (assign $best nil)
                  (doall (?x (get-current-beam-crossings))
                    (if (and (member ?x $remaining)
                             (crossing-reaches ?x $kept $lighting))
                      (if (or (not $best)
                              (string< (symbol-name ?x) (symbol-name $best)))
                        (assign $best ?x))))
                  (if (not $best)
                    (return t)
                    (do (assign $kept (cons $best $kept))
                        (assign $remaining (remove $best $remaining)))))
      $kept)
  "Drops numeric crossing priority and retains only alphabetical arbitration.
   The four-way cascade must then make this characterization fail.")


(define-query-mutation beam-crossing-subset-equality same-crossing-set
  (?left ?right)
  (ww-loop for $crossing in ?left always (member $crossing ?right))
  "Drops the crossing-set length check.  The oscillating empty/full sequence must
   then make this characterization fail.")
