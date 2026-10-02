;;; Filename: problem-traversal-substrate-test.lisp

;;; Dedicated zero-action regression for the -traversal substrate.  The substrate owns the
;;; separator relation, the kind registry, clause-kind inference, the per-fact segment
;;; choice, replay acceptance, and the single mobility provider; every kind's own rule lives
;;; in that kind's technology.  This problem registers three probe kinds of its own, served
;;; by one builder, rather than including any real kind, so the mechanics are characterized
;;; without any elevation, ladder-position or vault rule mixed in.
;;;
;;;   1. TRAVERSE-VIA is symmetric: the engine mirrors it because LOCATION is its repeated
;;;      argument type and its name has no ">" suffix.  TRAVERSE-VIA> is not mirrored.
;;;      Both preserve their DNF payloads opaquely.
;;;   2. A payload is a family of clauses: OR over clauses, AND within one.  With DOOR-A
;;;      open and DOOR-B/DOOR-C shut, the symmetric fact is crossed by its first clause,
;;;      and the witness names exactly that clause.  With every clause shut the fact is not
;;;      crossed at all, though it still exists.
;;;   3. () is the direct family and offers the single empty clause, a walk.
;;;   4. A clause's kind is inferred from its marker types: RAMP makes a stairs clause,
;;;      LADDER a climb, no marker a walk.  RAMP is registered static, so it stays in the
;;;      witness but is removed from the means the builder's clearance test sees.
;;;   5. A fact yields one segment, chosen by *TRAVERSAL-KIND-PREFERENCE* (walk, stairs,
;;;      climb, jump) rather than by canonical clause order: when a climb clause and a
;;;      stairs clause both pass, the stairs clause is taken even though the climb clause
;;;      sorts first.
;;;   6. Replay accepts any passing clause of the pair's fact (decision D6), including the
;;;      non-preferred climb, and nothing that is not a clause of the fact.
;;;   7. Initialization rejects an unregistered or repeated kind, an item no kind permits,
;;;      a clause mixing two kinds' markers, a pair authored twice, and a self-loop.
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
  location (origin symmetric-neighbor directional-neighbor shut-neighbor
            stair-neighbor preferred-neighbor isolated-site)
  gate (door-a door-b door-c door-shut)
  ramp (ramp-a)
  ladder (ladder-a))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -traversal)
(include-tech -passability)  ;the probe builder below calls ALL-CLEAR, so this problem nests it itself


;;;; PROBE KINDS ;;;;


(define-problem-helper probe-segment-for-clause
    (state agent source destination clause)
  "The substrate's contract with a kind, and nothing more: accept the clause when every
   one of its means is passable, label the segment with the clause's inferred kind, and
   name the whole clause as the witness.  A real kind adds its own rule on top -- walking
   an elevation-equality test, jumping a clearance bound, climbing a positioned ladder --
   and none of those is under test here."
  (let ((profile (traversal-clause-profile clause)))
    (when (funcall (symbol-function 'all-clear) state agent (second profile))
      (list (first profile) source clause destination))))


(register-traversal-kind 'walk 'probe-segment-for-clause nil nil '(gate))


(register-traversal-kind 'stairs 'probe-segment-for-clause '(ramp) '(ramp) '(gate))


(register-traversal-kind 'climb 'probe-segment-for-clause '(ladder) nil '(gate))


;;;; STATIC TOPOLOGY ;;;;


(define-init
  (open door-a)

  (traverse-via
    origin
    ((door-a) (door-b door-c))
    symmetric-neighbor)

  ;; Every clause of this one is shut, so the fact exists and is never crossed.
  (traverse-via
    origin
    ((door-shut))
    shut-neighbor)

  ;; The walk clause is shut; the stairs clause passes because the static ramp is not a means.
  (traverse-via
    origin
    ((door-shut) (ramp-a))
    stair-neighbor)

  ;; Both clauses pass.  (LADDER-A) sorts first canonically; the stairs clause is preferred.
  (traverse-via
    origin
    ((ladder-a) (door-a ramp-a))
    preferred-neighbor)

  (traverse-via>
    origin
    ()
    directional-neighbor))


;;;; CHARACTERIZATION FIXTURES ;;;;


(define-query substrate-family-is (?from location ?to location ?expected)
  (do (bind (traverse-via ?from $actual ?to))
      (equal $actual ?expected)))


(define-query substrate-directed-family-is (?from location ?to location ?expected)
  (do (bind (traverse-via> ?from $actual ?to))
      (equal $actual ?expected)))


(define-query substrate-segment-to (?agent agent ?from location ?to location)
  ;; The one segment the shared provider produces toward ?TO, or NIL.  Membership rather
  ;; than list equality, so the claims do not depend on the provider's accumulation order.
  (do (assign $found nil)
      (ww-loop for $segment in (traversal-segments ?agent ?from)
               do (if (eql (fourth $segment) ?to)
                    (assign $found $segment)))
      $found))


(define-query substrate-replayable (?agent agent ?from location ?segment)
  (traversal-segment-replayable-p state ?agent ?from ?segment))


;;;; VALIDATION CHARACTERIZATION ;;;;


(define-test-claim traversal-substrate-contract
  ;; The relation installs with no mode argument and the two locations mirrored.
  (expect-relation-schema
    'traverse-via :static '(location list location)
    :fluent-indices '(2))
  (expect-relation-schema
    'traverse-via> :static '(location list location)
    :fluent-indices '(2))
  (equal (gethash 'traverse-via *symmetrics*) '((0 2)))
  (null (gethash 'traverse-via> *symmetrics*))

  ;; The substrate registers exactly one mobility provider, however many kinds exist.
  (equal *mobility-providers* '(traversal-segments))
  (equal (mapcar #'first *traversal-kinds*) '(walk stairs climb))

  ;; A kind outside the preference list, and a kind registered twice, are authoring errors.
  (expect-condition
    (lambda () (register-traversal-kind 'swimming 'probe-segment-for-clause nil nil '(gate)))
    'error
    :containing "must be one of")
  (expect-condition
    (lambda () (register-traversal-kind 'walk 'probe-segment-for-clause nil nil '(gate)))
    'error
    :containing "registered more than once")

  ;; A clause item no registered kind permits is refused.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traverse-via origin ((first-agent)) symmetric-neighbor))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "expected an instance of one of"
    :check 'traversal-init-check)

  ;; One clause is one way across, so it may not name two kinds' markers.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traverse-via origin ((ramp-a ladder-a)) symmetric-neighbor))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "mixes the separators"
    :check 'traversal-init-check)

  ;; A symmetric pair authored twice, even in the reverse order, is refused.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traverse-via origin ((door-a)) symmetric-neighbor)
          (traverse-via symmetric-neighbor ((door-b)) origin))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "is authored twice under"
    :check 'traversal-init-check)

  ;; Mobility already returns (ORIGIN NIL), so a self-loop cannot represent movement.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((traverse-via> origin () origin))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "source and destination are the same location"
    :check 'traversal-init-check))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query traversal-substrate-scenarios-valid ()
  (and
    ;; TRAVERSE-VIA is mirrored and retains its opaque DNF value in both directions.
    (substrate-family-is origin symmetric-neighbor '((door-a) (door-b door-c)))
    (substrate-family-is symmetric-neighbor origin '((door-a) (door-b door-c)))

    ;; TRAVERSE-VIA> retains the direct empty value but never reverses.
    (substrate-directed-family-is origin directional-neighbor nil)
    (not (bind (traverse-via> directional-neighbor $unexpected-directed-family origin)))

    ;; The crossing takes the first clause that passes, and says so in its witness.
    (equal (substrate-segment-to first-agent origin symmetric-neighbor)
           '(walk origin (door-a) symmetric-neighbor))

    ;; An empty family offers the one empty clause, so a direct fact still crosses.
    (equal (substrate-segment-to first-agent origin directional-neighbor)
           '(walk origin nil directional-neighbor))

    ;; A fact whose every clause is shut exists but produces no segment.
    (substrate-family-is origin shut-neighbor '((door-shut)))
    (null (substrate-segment-to first-agent origin shut-neighbor))

    ;; A shut walk falls through to the stairs clause, whose static ramp is no obstacle.
    (equal (substrate-segment-to first-agent origin stair-neighbor)
           '(stairs origin (ramp-a) stair-neighbor))

    ;; Kind preference, not canonical clause order, picks the one grounded segment.
    (equal (substrate-segment-to first-agent origin preferred-neighbor)
           '(stairs origin (door-a ramp-a) preferred-neighbor))

    ;; Replay accepts the non-preferred climb, and refuses a witness that is no clause.
    (substrate-replayable first-agent origin '(climb origin (ladder-a) preferred-neighbor))
    (substrate-replayable first-agent origin '(stairs origin (door-a ramp-a) preferred-neighbor))
    (not (substrate-replayable first-agent origin '(climb origin (door-a) preferred-neighbor)))

    ;; The directed fact is not crossed the other way, and an isolated location has no
    ;; facts at all.
    (null (substrate-segment-to first-agent directional-neighbor origin))
    (equal (mobility-locations second-agent isolated-site) '(isolated-site))
    (traversable second-agent isolated-site isolated-site)
    (not (traversable second-agent isolated-site origin))))


(define-goal
  (traversal-substrate-scenarios-valid))
