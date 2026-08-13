;;; Filename: problem-height-test.lisp

;;; Dedicated zero-action regression for the shared -height role.  A complete
;;; matrix gives every HEIGHTED-OBJECT leaf one explicit-height fixture and one
;;; fixture with no authored HAS-HEIGHT fact:
;;;
;;;   agent, box, jammer, connector, gate, screen, wall, edge,
;;;   floor-repeater, and wall-repeater.
;;;
;;; Distinct explicit values verify exact binding and DECLARED-HEIGHT lookup.  The
;;; explicit agent has height two, distinct from the default agent height.  Undeclared
;;; agent and edge fixtures default to 3/2; gate, screen, and wall share the default 4.
;;; Initial and final states are
;;; identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* height-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (explicit-agent default-agent)
  box (explicit-box default-box)
  jammer (explicit-jammer default-jammer)
  connector (explicit-connector default-connector)
  gate (explicit-gate default-gate)
  screen (explicit-screen default-screen)
  wall (explicit-wall default-wall)
  edge (explicit-edge default-edge)
  floor-repeater (explicit-floor-repeater default-floor-repeater)
  wall-repeater (explicit-wall-repeater default-wall-repeater))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -height)


;;;; INITIALIZATION ;;;;


(define-init
  ;; The explicit agent value two is deliberately distinct from the 3/2 fallback;
  ;; the characterization query also requires its authored fact.
  (has-height explicit-agent 2)
  (has-height explicit-box 2)
  (has-height explicit-jammer 3)
  (has-height explicit-connector 4)
  (has-height explicit-gate 6)
  (has-height explicit-screen 7)
  (has-height explicit-wall 8)
  (has-height explicit-edge 11)
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


(define-query default-height-valid (?object heighted-object ?expected-height)
  (and
    (not (bind (has-height ?object $authored-height)))
    (= (declared-height ?object) ?expected-height)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query height-scenarios-valid ()
  (and
    ;; Complete explicit-height matrix, including both repeater orientations.
    (explicit-height-valid explicit-agent 2)
    (explicit-height-valid explicit-box 2)
    (explicit-height-valid explicit-jammer 3)
    (explicit-height-valid explicit-connector 4)
    (explicit-height-valid explicit-gate 6)
    (explicit-height-valid explicit-screen 7)
    (explicit-height-valid explicit-wall 8)
    (explicit-height-valid explicit-edge 11)
    (explicit-height-valid explicit-floor-repeater 9)
    (explicit-height-valid explicit-wall-repeater 10)

    ;; Box, jammer, connector, and repeaters default to one; gate, screen, and wall
    ;; default to four; edge and agent both default to 3/2.
    (default-height-valid default-agent 3/2)
    (default-height-valid default-box 1)
    (default-height-valid default-jammer 1)
    (default-height-valid default-connector 1)
    (default-height-valid default-gate 4)
    (default-height-valid default-screen 4)
    (default-height-valid default-wall 4)
    (default-height-valid default-edge 3/2)
    (default-height-valid default-floor-repeater 1)
    (default-height-valid default-wall-repeater 1)))


(define-goal
  (height-scenarios-valid))
