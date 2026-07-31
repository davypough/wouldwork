;;; Dedicated zero-action regression for the shared -gate substrate.
;;;
;;; AUTHORED-OPEN-GATE verifies that the substrate installs OPEN as an ordinary
;;; dynamic relation whose authored state is preserved.  DEFAULT-CLOSED-GATE
;;; verifies the complementary absence default.  The goal also confirms that
;;; the public gate control/update layer, initialization, and actions remain
;;; absent when only the substrate is included.
;;;
;;; The initial and final dynamic states contain exactly
;;; (OPEN AUTHORED-OPEN-GATE).  The expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* gate-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  gate (authored-open-gate default-closed-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -gate)


;;;; INITIALIZATION ;;;;


(define-init
  (open authored-open-gate))


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'gate-substrate-metadata-valid-p)
  (lambda (state)
    (and
      (equal
        (gethash 'gate *types*)
        '(authored-open-gate default-closed-gate))
      (nth-value 1 (gethash 'open *relations*))
      (not (nth-value 1 (gethash 'open *static-relations*)))
      (= (hash-table-count *relations*) 2)
      (= (hash-table-count *static-relations*) 2)
      (nth-value 1 (gethash 'gate *static-relations*))
      (nth-value 1 (gethash 'always-true *static-relations*))
      (not (member 'update-gate-status! *update-names*))
      (not (nth-value 1 (gethash 'controls *static-relations*)))
      (null (gethash 'jammer *types*))
      (null *init-actions*)
      (null *actions*)
      (equal (database state) '((open authored-open-gate)))
      (not (state-is-inconsistent state)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query gate-substrate-type-valid
    (?object gate)
  (do
    ?object
    t))


(define-query gate-substrate-scenarios-valid ()
  (and
    ;; Both constants inhabit the optional type supplied by the substrate.
    (gate-substrate-type-valid authored-open-gate)
    (gate-substrate-type-valid default-closed-gate)

    ;; Authored dynamic membership is preserved; absence remains closed.
    (open authored-open-gate)
    (not (open default-closed-gate))

    ;; No public gate machinery or additional dynamic state may leak in.
    (gate-substrate-metadata-valid-p state)))


(define-goal
  (gate-substrate-scenarios-valid))
