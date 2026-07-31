;;; Dedicated zero-action regression for the shared -reachability substrate.
;;;
;;; Three independent locations characterize the complete identity default:
;;; every location is reachable from itself, while all six ordered pairs of
;;; distinct locations remain unreachable.  The goal also verifies that the
;;; public REACH-VIA/gate extension, initialization, actions, and technology-
;;; owned state are absent.
;;;
;;; The initial and final dynamic states are empty.  The characterization query
;;; is true immediately after staging, so the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* reachability-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (first-site second-site isolated-site))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -reachability)


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'reachability-substrate-metadata-valid-p)
  (lambda (state)
    (and
      (member 'reachable *query-names*)
      (not (member 'reachable-clear *query-names*))
      (zerop (hash-table-count *static-relations*))
      (= (hash-table-count *relations*) 1)
      (nth-value 1 (gethash 'inconsistent-state *relations*))
      (null *init-actions*)
      (null *actions*)
      (null (database state))
      (not (state-is-inconsistent state)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query reachability-substrate-scenarios-valid ()
  (and
    ;; Identity is the complete positive behavior.
    (reachable first-site first-site)
    (reachable second-site second-site)
    (reachable isolated-site isolated-site)

    ;; Every ordered pair of distinct locations remains outside manual reach.
    (not (reachable first-site second-site))
    (not (reachable second-site first-site))
    (not (reachable first-site isolated-site))
    (not (reachable isolated-site first-site))
    (not (reachable second-site isolated-site))
    (not (reachable isolated-site second-site))

    ;; No public extension or stateful behavior may leak into the role.
    (reachability-substrate-metadata-valid-p state)))


(define-goal
  (reachability-substrate-scenarios-valid))
