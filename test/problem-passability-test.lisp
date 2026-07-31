;;; Filename: problem-passability-test.lisp

;;; Dedicated zero-action regression for -passability's neutral GEARS branch.
;;; Existing ladder, jump, and walkability tests cover gates, screens, ladders,
;;; and ordinary ALL-CLEAR conjunctions; this file isolates the otherwise unused
;;; STREAM-OBSTACLE-CLEAR default before -stream-passability overrides it.
;;;
;;; One bare gears instance must pass for both an empty-handed agent and an agent
;;; holding cargo.  Singleton and repeated-gears lists must therefore be all clear,
;;; while adding a closed gate must still block the conjunction.  The characterization
;;; also verifies that including -passability alone does not install fan mounting,
;;; turning, or blowing machinery.
;;;
;;; Initial and final dynamic state: CARRYING-AGENT holds CARRIED-CONNECTOR;
;;; CLOSED-GATE remains closed.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* passability-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  agent (empty-agent carrying-agent)
  connector (carried-connector)
  gate (closed-gate)
  gears (neutral-gears))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -passability)


;;;; INITIALIZATION ;;;;


(define-init
  (holding carrying-agent carried-connector))


;;;; LAYERING CHARACTERIZATION ;;;;


(setf
  (symbol-function 'passability-neutral-layering-valid-p)
  (lambda ()
    (and
      (equal (gethash 'gears *types*) '(neutral-gears))
      (not (nth-value 1 (gethash 'mounted-on *relations*)))
      (not (nth-value 1 (gethash 'turning *relations*)))
      (not (nth-value 1 (gethash 'blowing *relations*))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query passability-scenarios-valid ()
  (and
    ;; The default hook is neutral and independent of an agent's cargo state.
    (stream-obstacle-clear neutral-gears)
    (obstacle-clear empty-agent neutral-gears)
    (obstacle-clear carrying-agent neutral-gears)

    ;; ALL-CLEAR routes every bare gears item through the same neutral hook.
    (all-clear empty-agent '(neutral-gears))
    (all-clear carrying-agent '(neutral-gears))
    (all-clear
      carrying-agent '(neutral-gears neutral-gears))

    ;; A clear gears must not make the surrounding conjunction globally true.
    (not (open closed-gate))
    (not (obstacle-clear empty-agent closed-gate))
    (not
      (all-clear
        empty-agent '(neutral-gears closed-gate)))

    ;; Cargo remains live but affects neither the default gears hook nor its branch.
    (holding carrying-agent carried-connector)
    (not
      (bind
        (holding empty-agent $unexpected-cargo)))

    ;; The passability substrate retains its intended dependency boundary.
    (passability-neutral-layering-valid-p)))


(define-goal
  (passability-scenarios-valid))
