;;; Filename: problem-init-literal-defaults-test.lisp

;;; Dedicated zero-action regression for the engine's init-literal default machinery.
;;; A relation may declare defaults for a suffix of its arguments, after which a
;;; DEFINE-INIT literal omitting them is padded before anything else sees it.  The
;;; fixtures characterize the complete contract:
;;;
;;;   1. An omitted trailing argument is supplied from the registered default, and a
;;;      written one is left alone.  Padding applies to negative literals too.
;;;   2. A literal may stop anywhere inside the optional suffix but nowhere before it,
;;;      and may not run past the signature.  Both are authoring errors.
;;;   3. A relation with no registered defaults is returned untouched, so the fluentless
;;;      lookup keys CHECK-PROPOSITION tolerates elsewhere keep working.
;;;   4. Padding runs ahead of CHECK-PROPOSITION, so an omitted argument is type-checked
;;;      exactly as a written one.  This is the point of the mechanism: CHECK-PROPOSITION
;;;      reads an argument list shorter than the signature as a fluentless lookup key and
;;;      strips the fluent type-defs before checking, so a relation left merely tolerant
;;;      of short literals would type-check none of their arguments at all.
;;;   5. Defaults may be registered only for a declared relation, only once, and never
;;;      more of them than the relation has arguments.
;;;   6. Storage is uniform: a padded fact and a fully written one produce value vectors
;;;      of the same length, so no consumer ever binds NIL where a value belongs.
;;;
;;; The goal binds every fixture at full arity.  Initial and final dynamic states are
;;; empty.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* init-literal-defaults-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  spot (padded written both-defaulted partly-defaulted))


;;;; RELATIONS ;;;;


(define-static-relations
  (probe-coords> spot $rational $rational $rational)
  (probe-range> spot $rational $rational)
  (probe-lonely> spot $rational))


;;;; DEFAULT REGISTRATION ;;;;
;;;; PROBE-COORDS> makes its final argument optional; PROBE-RANGE> makes its final two
;;;; optional, so a literal may supply neither, or only the first.  PROBE-LONELY>
;;;; registers nothing and must therefore pass through padding unchanged.


(register-init-literal-defaults 'probe-coords> 0)

(register-init-literal-defaults 'probe-range> 5 7)


;;;; INITIALIZATION ;;;;


(define-init
  (probe-coords> padded 1 2)
  (probe-coords> written 3 4 3/2)
  (probe-range> both-defaulted)
  (probe-range> partly-defaulted 9))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(define-test-claim init-literal-defaults-contract
  (expect-relation-schema
    'probe-coords> :static '(spot rational rational rational)
    :fluent-indices '(2 3 4))
  (expect-relation-schema
    'probe-range> :static '(spot rational rational)
    :fluent-indices '(2 3))

  ;; Registered defaults, in argument order, covering the signature's suffix.
  (equal (gethash 'probe-coords> *init-literal-defaults*) '(0))
  (equal (gethash 'probe-range> *init-literal-defaults*) '(5 7))
  (null (gethash 'probe-lonely> *init-literal-defaults*))

  ;; An omitted trailing argument is supplied; a written one is left alone.
  (equal (pad-init-literal '(probe-coords> padded 1 2))
         '(probe-coords> padded 1 2 0))
  (equal (pad-init-literal '(probe-coords> written 3 4 3/2))
         '(probe-coords> written 3 4 3/2))

  ;; A literal may stop anywhere inside the optional suffix.
  (equal (pad-init-literal '(probe-range> both-defaulted))
         '(probe-range> both-defaulted 5 7))
  (equal (pad-init-literal '(probe-range> partly-defaulted 9))
         '(probe-range> partly-defaulted 9 7))

  ;; Negative literals are padded inside the negation.
  (equal (pad-init-literal '(not (probe-coords> padded 1 2)))
         '(not (probe-coords> padded 1 2 0)))

  ;; A relation with no registered defaults is returned untouched, at any arity.
  (equal (pad-init-literal '(probe-lonely> padded 1)) '(probe-lonely> padded 1))
  (equal (pad-init-literal '(probe-lonely> padded)) '(probe-lonely> padded))

  ;; Stopping before the optional suffix, or running past the signature, is an error.
  (expect-condition
    (lambda ()
      (pad-init-literal '(probe-coords> padded 1)))
    'error
    :containing "Only its last 1 argument may be omitted")
  (expect-condition
    (lambda ()
      (pad-init-literal '(probe-coords> padded 1 2 3 4)))
    'error
    :containing "supplies 5 of PROBE-COORDS>'s 4 arguments")

  ;; Padding restores the type checking a short literal would otherwise escape.
  (expect-condition
    (lambda ()
      (check-proposition (pad-init-literal '(probe-coords> padded 1.0 2))))
    'error
    :containing "not of specified type RATIONAL")
  (expect-condition
    (lambda ()
      (check-proposition (pad-init-literal '(probe-coords> 0 1 2))))
    'error
    :containing "not of specified type SPOT")

  ;; Defaults belong to a declared relation, registered once, never over-supplied.
  (expect-condition
    (lambda ()
      (register-init-literal-defaults 'probe-undeclared> 0))
    'error
    :containing "must name a declared relation")
  (expect-condition
    (lambda ()
      (register-init-literal-defaults 'probe-coords> 0))
    'error
    :containing "registered more than once")
  (expect-condition
    (lambda ()
      (register-init-literal-defaults 'probe-lonely> 1 2 3))
    'error
    :containing "takes 2 arguments, but 3 defaults were registered")

  ;; Storage is uniform: a padded fact and a written one hold value vectors of one length.
  (equal (gethash '(probe-coords> padded) *static-db*) '(1 2 0))
  (equal (gethash '(probe-coords> written) *static-db*) '(3 4 3/2))
  (equal (gethash '(probe-range> both-defaulted) *static-db*) '(5 7))
  (equal (gethash '(probe-range> partly-defaulted) *static-db*) '(9 7)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query init-literal-defaults-scenarios-valid ()
  (do
    (bind (probe-coords> padded $padded-x $padded-y $padded-z))
    (bind (probe-coords> written $written-x $written-y $written-z))
    (bind (probe-range> both-defaulted $both-low $both-high))
    (bind (probe-range> partly-defaulted $partly-low $partly-high))
    (and
      ;; A defaulted argument binds its default, never NIL.
      (= $padded-x 1)
      (= $padded-y 2)
      (= $padded-z 0)

      ;; A written argument is unaffected by the default beside it.
      (= $written-x 3)
      (= $written-y 4)
      (= $written-z 3/2)

      ;; Partial defaulting fills only the arguments actually omitted.
      (= $both-low 5)
      (= $both-high 7)
      (= $partly-low 9)
      (= $partly-high 7)

      ;; The mechanism invents nothing for a relation the problem never asserted.
      (not (bind (probe-lonely> padded $lonely))))))


(define-goal
  (init-literal-defaults-scenarios-valid))
