;;; Filename: problem-traversal-substrate-test.lisp

;;; Dedicated zero-action regression for the -traversal substrate, which replaced
;;; -walkability's WALK-VIA pair and the four per-mode relations beside it.  The substrate
;;; owns the relation, the mode registry, the clause selection, and the single mobility
;;; provider; every mode's own rule lives in that mode's technology, and this problem
;;; registers a probe mode of its own rather than including one, so the mechanics are
;;; characterized without any elevation, ladder or vault rule mixed in.
;;;
;;;   1. TRAVERSAL-VIA is symmetric: the engine mirrors it because LOCATION is its
;;;      repeated argument type and its name has no ">" suffix, so prepending the mode
;;;      leaves the two location positions as the mirrored pair.  TRAVERSAL-VIA> is not
;;;      mirrored.  Both preserve their DNF payloads opaquely.
;;;   2. A payload is a family of clauses: OR over clauses, AND within one.  With DOOR-A
;;;      open and DOOR-B/DOOR-C shut, the symmetric edge is crossed by its first clause,
;;;      and the witness names exactly that clause.  With every door shut it is not
;;;      crossed at all, though the edge still exists.
;;;   3. () is the direct, unguarded family and offers the single empty clause, so a mode
;;;      still gets exactly one attempt at an edge that names no obstacle.
;;;   4. A fact whose mode no technology registered fails initialization, which is what
;;;      catches a JUMPING edge in a problem that never included jump.
;;;   5. A self-loop fails initialization because mobility is already reflexive and the
;;;      edge would otherwise vanish silently inside its visited-location closure.
;;;
;;; The initial and final dynamic states are unchanged by the goal.  Expected minimum
;;; path length: zero.

(in-package :ww)


(ww-set *problem-name* traversal-substrate-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (first-agent second-agent)
  location (origin symmetric-neighbor directional-neighbor shut-neighbor isolated-site)
  gate (door-a door-b door-c door-shut))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -traversal)
(include-tech -passability)  ;the probe builder below calls ALL-CLEAR, so this problem nests it itself


;;;; PROBE MODE ;;;;


(define-problem-helper probe-segment-for-clause
    (state agent source destination clause)
  "The substrate's contract with a mode, and nothing more: accept the clause when every
   obstacle in it is passable, and name that clause as the segment's witness.  A real mode
   adds its own rule on top -- walking an elevation-equality test, jumping a clearance
   bound, climbing a positioned ladder -- and none of those is under test here."
  (when (funcall (symbol-function 'all-clear) state agent clause)
    (list 'probe source clause destination)))


(register-traversal-mode 'walking 'probe-segment-for-clause '(gate))


;;;; STATIC TOPOLOGY ;;;;


(define-init
  (open door-a)

  (traversal-via walking
    origin
    ((door-a) (door-b door-c))
    symmetric-neighbor)

  ;; Every clause of this one is shut, so the edge exists and is never crossed.
  (traversal-via walking
    origin
    ((door-shut))
    shut-neighbor)

  (traversal-via> walking
    origin
    ()
    directional-neighbor))


;;;; CHARACTERIZATION FIXTURES ;;;;


(define-query substrate-family-is (?from location ?to location ?expected)
  (do (bind (traversal-via walking ?from $actual ?to))
      (equal $actual ?expected)))


(define-query substrate-directed-family-is (?from location ?to location ?expected)
  (do (bind (traversal-via> walking ?from $actual ?to))
      (equal $actual ?expected)))


(define-query substrate-segment-to (?agent agent ?from location ?to location)
  ;; The one segment the shared provider produces toward ?TO, or NIL.  Membership rather
  ;; than list equality, so the claims do not depend on the provider's accumulation order.
  (do (assign $found nil)
      (ww-loop for $segment in (traversal-segments ?agent ?from)
               do (if (eql (fourth $segment) ?to)
                    (assign $found $segment)))
      $found))


;;;; VALIDATION CHARACTERIZATION ;;;;


(define-test-claim traversal-substrate-contract
  ;; The relation installs with the mode leading and the two locations mirrored.
  (expect-relation-schema
    'traversal-via :static '(traversal-mode location list location)
    :fluent-indices '(3))
  (expect-relation-schema
    'traversal-via> :static '(traversal-mode location list location)
    :fluent-indices '(3))
  (equal (gethash 'traversal-via *symmetrics*) '((1 3)))
  (null (gethash 'traversal-via> *symmetrics*))

  ;; The substrate registers exactly one mobility provider, however many modes exist.
  (equal *mobility-providers* '(traversal-segments))
  (equal (mapcar #'first *traversal-modes*) '(walking))

  ;; A mode outside the type, and a mode registered twice, are authoring errors.
  (expect-condition
    (lambda () (register-traversal-mode 'swimming 'probe-segment-for-clause '(gate)))
    'error
    :containing "must be an instance of TRAVERSAL-MODE")
  (expect-condition
    (lambda () (register-traversal-mode 'walking 'probe-segment-for-clause '(gate)))
    'error
    :containing "registered more than once")

  ;; A fact naming an unregistered mode fails initialization, and the message says which
  ;; technology is missing rather than reporting an unknown object.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traversal-via jumping origin () symmetric-neighbor))
        :checks '(traversal-init-check)))
    'error
    :containing "No technology registers the traversal mode")

  ;; A clause item outside the mode's registered types is refused.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traversal-via walking origin ((first-agent)) symmetric-neighbor))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "expected an instance of one of"
    :check 'traversal-init-check)

  ;; Mobility already returns (ORIGIN NIL), so a self-loop cannot represent movement.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traversal-via> walking origin () origin))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "source and destination are the same location"
    :check 'traversal-init-check))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query traversal-substrate-scenarios-valid ()
  (and
    ;; TRAVERSAL-VIA is mirrored and retains its opaque DNF value in both directions.
    (substrate-family-is origin symmetric-neighbor '((door-a) (door-b door-c)))
    (substrate-family-is symmetric-neighbor origin '((door-a) (door-b door-c)))

    ;; TRAVERSAL-VIA> retains the direct empty value but never reverses.
    (substrate-directed-family-is origin directional-neighbor nil)
    (not (bind (traversal-via> walking
                 directional-neighbor $unexpected-directed-family origin)))

    ;; The crossing takes the first clause that passes, and says so in its witness.
    (equal (substrate-segment-to first-agent origin symmetric-neighbor)
           '(probe origin (door-a) symmetric-neighbor))

    ;; An empty family offers the one empty clause, so a direct edge still crosses.
    (equal (substrate-segment-to first-agent origin directional-neighbor)
           '(probe origin nil directional-neighbor))

    ;; An edge whose every clause is shut exists but produces no segment.
    (substrate-family-is origin shut-neighbor '((door-shut)))
    (null (substrate-segment-to first-agent origin shut-neighbor))

    ;; The directed edge is not crossed the other way, and an isolated location has no
    ;; edges at all.
    (null (substrate-segment-to first-agent directional-neighbor origin))
    (equal (mobility-locations second-agent isolated-site) '(isolated-site))
    (traversable second-agent isolated-site isolated-site)
    (not (traversable second-agent isolated-site origin))))


(define-goal
  (traversal-substrate-scenarios-valid))
