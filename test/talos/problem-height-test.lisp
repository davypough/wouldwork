;;; Filename: problem-height-test.lisp

;;; Dedicated zero-action regression for the -height relation.  A complete matrix gives
;;; every HEIGHTED-OBJECT leaf one explicit-height fixture and one fixture with no
;;; authored HAS-HEIGHT fact:
;;;
;;;   agent, box, jammer, connector, gate, screen, wall, edge,
;;;   floor-repeater, and wall-repeater.
;;;
;;; What this file pins is the relation itself: its schema, its type domain, exact
;;; binding of authored values including fractional ones, and the absence of any fact
;;; for an undeclared fixture.  The per-type height defaults are no longer this
;;; substrate's business -- they live in -vertical's *VERTICAL-TYPE-CONSTANTS* and are
;;; pinned by problem-vertical-test, which is also the only place an unauthored height
;;; resolves to a number at all.  Initial and final states are identical.  Expected
;;; minimum path length: zero.

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


(define-query explicit-height-valid (?object heighted-object ?expected-height)
  (and
    (has-height ?object ?expected-height)
    (do (bind (has-height ?object $bound-height))
        (= $bound-height ?expected-height))))


(define-query absent-height-valid (?object heighted-object)
  (not (bind (has-height ?object $authored-height))))


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

    ;; No fixture without an authored fact has one.  What such a fixture is *worth*
    ;; is -vertical's business, not this relation's, and is pinned by
    ;; problem-vertical-test's height table.
    (absent-height-valid default-agent)
    (absent-height-valid default-box)
    (absent-height-valid default-jammer)
    (absent-height-valid default-connector)
    (absent-height-valid default-gate)
    (absent-height-valid default-screen)
    (absent-height-valid default-wall)
    (absent-height-valid default-edge)
    (absent-height-valid default-floor-repeater)
    (absent-height-valid default-wall-repeater)))


(define-test-claim height-relation-contract
  (expect-relation-schema
    'has-height :static '(heighted-object rational)
    :fluent-indices '(2))
  (expect-condition
    (lambda ()
      (check-proposition '(has-height explicit-agent 1.5)))
    'error
    :containing "not of specified type RATIONAL")
  (expect-condition
    (lambda ()
      (check-init-duplicate-fluent-keys
        '((has-height explicit-agent 2)
          (has-height explicit-agent 3))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key"))


(define-goal
  (height-scenarios-valid))
