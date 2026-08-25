;;; Filename: problem-relaxed-hmax-tray-test.lisp
;;;
;;; Focused Topo relaxation contract for relocating a tray support chain.  One ordinary
;;; tray placement must move every actual rider at the same shared action cost, while an
;;; occupant outside that ON chain must not inherit the tray's destination.

(in-package :ww)


(ww-set *problem-name* relaxed-hmax-tray-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)


(define-types
  agent (relaxed-tray-agent)
  location (relaxed-tray-origin relaxed-tray-target)
  tray (relaxed-tray)
  box (relaxed-tray-rider1 relaxed-tray-rider2 relaxed-tray-nonrider))


(include-tech tray)
(include-tech box)
(include-tech reachability)
(include-tech topo-lower-bound)


;;;; AUTHORED PROPAGATION DRIVER ;;;;

;;; No technology spliced here declares a propagating update, so INIT derives no order and
;;; -propagation's sentinel would stand -- yet TRAY's and BOX's effects call the master
;;; PROPAGATE-CHANGES! unconditionally, and the sentinel signals when they do.  A problem
;;; may author its own driver, and this one does: the empty body is the whole truth about a
;;; problem with no derived state, and stating it here leaves the substrate's empty-order
;;; policy, and PROBLEM-PROPAGATION-SUBSTRATE-TEST's characterization of it, untouched.

(define-update propagate-consequences! ()
  nil)


(define-init
  (has-location relaxed-tray-agent relaxed-tray-origin)
  (has-location relaxed-tray relaxed-tray-origin)
  (has-location relaxed-tray-rider1 relaxed-tray-origin)
  (has-location relaxed-tray-rider2 relaxed-tray-origin)
  (has-location relaxed-tray-nonrider relaxed-tray-origin)
  (holding relaxed-tray-agent relaxed-tray)
  (on relaxed-tray-rider1 relaxed-tray)
  (on relaxed-tray-rider2 relaxed-tray-rider1)
  (reach-via relaxed-tray-origin () relaxed-tray-target))


(define-test-helper relaxed-tray-test-operator-family-p (operator family)
  (let ((name (relaxed-hmax-operator.name operator)))
    (and (consp name) (eq (first name) family))))


(define-test-helper relaxed-tray-test-chain-operators ()
  (remove-if-not
    (lambda (operator)
      (member
        (relaxed-hmax-operator.name operator)
        '((put-ground relaxed-tray-agent relaxed-tray
            relaxed-tray-origin relaxed-tray-target nil)
          (supported-location
            relaxed-tray-rider1 relaxed-tray relaxed-tray-target)
          (supported-location
            relaxed-tray-rider2 relaxed-tray-rider1 relaxed-tray-target)
          (supported-location
            relaxed-tray-nonrider relaxed-tray relaxed-tray-target))
        :test #'equal))
    *topo-relaxed-all-operators*))


(define-test-helper relaxed-tray-test-facts ()
  '((holding relaxed-tray-agent relaxed-tray)
    (has-location relaxed-tray-agent relaxed-tray-origin)
    (on relaxed-tray-rider1 relaxed-tray)
    (on relaxed-tray-rider2 relaxed-tray-rider1)))


(define-test-helper relaxed-tray-test-cost (goals evaluator)
  (let* ((operators (relaxed-tray-test-chain-operators))
         (model (compile-relaxed-indexed-model operators goals)))
    (funcall
      (symbol-function evaluator)
      (relaxed-tray-test-facts)
      model)))


(define-test-claim relaxed-tray-relocation-contract
  (= (length (relaxed-tray-test-chain-operators)) 4)
  (notany
    (lambda (operator)
      (relaxed-tray-test-operator-family-p
        operator 'release-tray-and-riders))
    *topo-relaxed-all-operators*)
  (= (relaxed-tray-test-cost
       '((has-location relaxed-tray-rider1 relaxed-tray-target)
         (has-location relaxed-tray-rider2 relaxed-tray-target))
       'relaxed-indexed-hmax-cost)
     1)
  (= (relaxed-tray-test-cost
       '((has-location relaxed-tray-rider1 relaxed-tray-target)
         (has-location relaxed-tray-rider2 relaxed-tray-target))
       'relaxed-indexed-lm-cut-cost)
     1)
  (null
    (relaxed-tray-test-cost
      '((has-location relaxed-tray-nonrider relaxed-tray-target))
      'relaxed-indexed-hmax-cost))
  (null
    (relaxed-tray-test-cost
      '((has-location relaxed-tray-nonrider relaxed-tray-target))
      'relaxed-indexed-lm-cut-cost))
  (= (topo-finite-resource-bound-for
       *start-state*
       '(has-location relaxed-tray relaxed-tray-target))
     1)
  (zerop (topo-finite-resource-bound-for *start-state* *goal*)))


(define-goal
  (has-location relaxed-tray-rider1 relaxed-tray-target))
