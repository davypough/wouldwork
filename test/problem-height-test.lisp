;;; Filename: problem-height-test.lisp

;;; Dedicated zero-action regression for the shared -height role.  A complete
;;; matrix gives every HEIGHTED-OBJECT leaf one explicit-height fixture and one
;;; fixture with no authored HAS-HEIGHT fact:
;;;
;;;   agent, box, jammer, connector, fence, gate, screen, wall,
;;;   floor-repeater, and wall-repeater.
;;;
;;; Distinct explicit values verify exact binding and DECLARED-HEIGHT lookup.  The
;;; explicit agent has height one, distinguishing an authored value equal to the
;;; fallback from the ten absent facts that must all default to one.  Barrier
;;; clearance policy is intentionally outside this test: fixed obstacles belong to
;;; HEIGHTED-OBJECT, but jump.lisp applies its own barrier-specific defaults.
;;; Initial and final states are identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* height-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  agent (explicit-agent default-agent)
  box (explicit-box default-box)
  jammer (explicit-jammer default-jammer)
  connector (explicit-connector default-connector)
  fence (explicit-fence default-fence)
  gate (explicit-gate default-gate)
  screen (explicit-screen default-screen)
  wall (explicit-wall default-wall)
  floor-repeater (explicit-floor-repeater default-floor-repeater)
  wall-repeater (explicit-wall-repeater default-wall-repeater))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -height)


;;;; INITIALIZATION ;;;;


(define-init
  ;; The explicit value one is deliberately indistinguishable numerically from
  ;; the fallback; the characterization query also requires its authored fact.
  (has-height explicit-agent 1)
  (has-height explicit-box 2)
  (has-height explicit-jammer 3)
  (has-height explicit-connector 4)
  (has-height explicit-fence 5)
  (has-height explicit-gate 6)
  (has-height explicit-screen 7)
  (has-height explicit-wall 8)
  (has-height explicit-floor-repeater 9)
  (has-height explicit-wall-repeater 10)

  ;; Every DEFAULT-* fixture deliberately omits HAS-HEIGHT.
  )


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query explicit-height-valid
    (?object heighted-object ?expected-height)
  (and
    (has-height ?object ?expected-height)
    (do (bind (has-height ?object $bound-height))
        (= $bound-height ?expected-height))
    (= (declared-height ?object) ?expected-height)))


(define-query default-height-valid (?object heighted-object)
  (and
    (not (bind (has-height ?object $authored-height)))
    (= (declared-height ?object) 1)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query height-scenarios-valid ()
  (and
    ;; Complete explicit-height matrix, including both repeater orientations.
    (explicit-height-valid explicit-agent 1)
    (explicit-height-valid explicit-box 2)
    (explicit-height-valid explicit-jammer 3)
    (explicit-height-valid explicit-connector 4)
    (explicit-height-valid explicit-fence 5)
    (explicit-height-valid explicit-gate 6)
    (explicit-height-valid explicit-screen 7)
    (explicit-height-valid explicit-wall 8)
    (explicit-height-valid explicit-floor-repeater 9)
    (explicit-height-valid explicit-wall-repeater 10)

    ;; The exact absent-fact boundary always returns the shared default of one.
    (default-height-valid default-agent)
    (default-height-valid default-box)
    (default-height-valid default-jammer)
    (default-height-valid default-connector)
    (default-height-valid default-fence)
    (default-height-valid default-gate)
    (default-height-valid default-screen)
    (default-height-valid default-wall)
    (default-height-valid default-floor-repeater)
    (default-height-valid default-wall-repeater)))


(define-goal
  (height-scenarios-valid))
