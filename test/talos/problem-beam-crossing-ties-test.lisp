;;; Filename: problem-beam-crossing-ties-test.lisp

;;; Zero-action regression for crossings that share a point on a beam (ties).  ESTABLISH-
;;; BEAM-COORDINATES records each tied pair as CROSSINGS-COINCIDE>, and BEAM-REACHES-CROSSING
;;; never lets one tied crossing block another, so every beam that reaches the shared point
;;; is cut there.  All beams are direct couplings with empty corridors, so each is live for
;;; cutting and only the crossing rule decides the outcome.  Two scenarios, far enough
;;; apart that none of their beams cross:
;;;
;;; CONCURRENT (around the origin): three beams -- horizontal, vertical, diagonal -- all pass
;;; through (0,0).  Their three crossings coincide pairwise, all three are active, and all
;;; three beams are cut.  Under the old earlier-blocks-later rule the first-listed crossing
;;; would shadow the other two, leaving one beam uncut.
;;;
;;; OVERLAPPING (around x=40): the long beam TRANSMITTER-LONG -> RECEIVER-LONG on y=5 contains
;;; the short collinear beam TRANSMITTER-SHORT -> RECEIVER-SHORT (collinear segments do not
;;; cross).  An apparatus is not a beam occluder, so both stay live.  The vertical beam
;;; TRANSMITTER-CROSS -> RECEIVER-CROSS on x=40 crosses both at (40,5); its two crossings
;;; coincide, both are active, and all three beams are cut.  This is the shape of the
;;; collinear grid points in problem-intersect, in the variant where both overlapping beams
;;; can be live at once.
;;;
;;; Initial and final states are identical; expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* beam-crossing-ties-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent       (agent1)
  location    (loc1)
  transmitter (transmitter-horizontal transmitter-vertical transmitter-diagonal
               transmitter-long transmitter-short transmitter-cross)
  receiver    (receiver-horizontal receiver-vertical receiver-diagonal
               receiver-long receiver-short receiver-cross)
  gate        (unused-gate)
  hue         (blue green red))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech beam-direct)
(include-tech beam-crossing)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 loc1)

  ;; Distinct hues within each scenario keep the receiver outcomes independent.
  (has-chroma transmitter-horizontal red)
  (has-chroma receiver-horizontal red)
  (has-chroma transmitter-vertical blue)
  (has-chroma receiver-vertical blue)
  (has-chroma transmitter-diagonal green)
  (has-chroma receiver-diagonal green)
  (has-chroma transmitter-long red)
  (has-chroma receiver-long red)
  (has-chroma transmitter-short blue)
  (has-chroma receiver-short blue)
  (has-chroma transmitter-cross green)
  (has-chroma receiver-cross green)

  ;; Empty corridors make every direct beam live for cutting.
  (coupled transmitter-horizontal receiver-horizontal)
  (coupled transmitter-vertical receiver-vertical)
  (coupled transmitter-diagonal receiver-diagonal)
  (coupled transmitter-long receiver-long)
  (coupled transmitter-short receiver-short)
  (coupled transmitter-cross receiver-cross)
  (beam-via transmitter-horizontal () receiver-horizontal)
  (beam-via transmitter-vertical () receiver-vertical)
  (beam-via transmitter-diagonal () receiver-diagonal)
  (beam-via transmitter-long () receiver-long)
  (beam-via transmitter-short () receiver-short)
  (beam-via transmitter-cross () receiver-cross)

  ;; CONCURRENT: three beams through (0,0).
  (apparatus-coords> transmitter-horizontal -10 0)
  (apparatus-coords> receiver-horizontal 10 0)
  (apparatus-coords> transmitter-vertical 0 -10)
  (apparatus-coords> receiver-vertical 0 10)
  (apparatus-coords> transmitter-diagonal -10 -10)
  (apparatus-coords> receiver-diagonal 10 10)

  ;; OVERLAPPING: the short beam lies inside the long one on y=5; the cross beam meets
  ;; both at (40,5).
  (apparatus-coords> transmitter-long 30 5)
  (apparatus-coords> receiver-long 50 5)
  (apparatus-coords> transmitter-short 35 5)
  (apparatus-coords> receiver-short 45 5)
  (apparatus-coords> transmitter-cross 40 0)
  (apparatus-coords> receiver-cross 40 10)

  ;; Off every beam; BEAM-COORDINATES-ENDPOINT-POSITIONS requires a position for it.
  (location-coords> loc1 20 20))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query coincident-crossing-fact-count ()
  ;; Counts CROSSINGS-COINCIDE> facts over the pool; each tied pair is stored both ways.
  (do (assign $count 0)
      (doall (?x (get-current-beam-crossings))
        (doall (?y (get-current-beam-crossings))
          (if (crossings-coincide> ?x ?y)
            (assign $count (1+ $count)))))
      $count))


(define-query beam-crossing-ties-scenarios-valid ()
  (do (assign $crossings (get-current-beam-crossings))
      (and
        ;; Three concurrent crossings plus two overlapping-beam crossings.
        (= (length $crossings) 5)
        ;; Three tied pairs at the origin, one at (40,5).
        (= (coincident-crossing-fact-count) 8)
        ;; No tied crossing blocks another, so every crossing is active.
        (same-crossing-set $crossings (current-crossing-set))
        (beam-cut transmitter-horizontal receiver-horizontal)
        (beam-cut transmitter-vertical receiver-vertical)
        (beam-cut transmitter-diagonal receiver-diagonal)
        (beam-cut transmitter-long receiver-long)
        (beam-cut transmitter-short receiver-short)
        (beam-cut transmitter-cross receiver-cross)
        (not (active receiver-horizontal))
        (not (active receiver-vertical))
        (not (active receiver-diagonal))
        (not (active receiver-long))
        (not (active receiver-short))
        (not (active receiver-cross)))))


(define-goal
  (beam-crossing-ties-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation beam-crossing-ties-block beam-reaches-crossing
  (?from los-endpoint
   ?to los-endpoint
   ?xing crossing
   ?active
   ?lighting)
  (do (assign $reaches nil)
      (assign $src nil)
      (assign $dst nil)
      (assign $elevations (beam-live-for-cutting ?from ?to ?lighting))
      (if $elevations
        (do (assign $src ?from)
            (assign $dst ?to))
        (do (assign $elevations (beam-live-for-cutting ?to ?from ?lighting))
            (if $elevations
              (do (assign $src ?to)
                  (assign $dst ?from)))))
      (if $src
        (do (assign $blocked
                    (beam-barrier-before-crossing
                      $src $dst ?xing (first $elevations) (second $elevations)))
            (assign $reached nil)
            (doall (?gate gate)
              (if (and (bind (beam-crossings-before-gate> $src $before ?gate $dst))
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
      $reaches)
  "Restores the earlier-blocks-later rule for tied crossings.  The first-listed tied
   crossing then shadows the others, leaving a beam uncut in each scenario, so this
   characterization must fail.")
