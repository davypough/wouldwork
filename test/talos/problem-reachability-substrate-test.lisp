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


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim reachability-substrate-schema
  (expect-registered :query 'reachable)
  (expect-not-registered :query 'reachable-clear)
  (expect-relations :static '())
  (expect-relations :dynamic '(inconsistent-state))
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


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
    (not (reachable isolated-site second-site))))


(define-goal
  (reachability-substrate-scenarios-valid))
