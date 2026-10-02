;;; Filename: problem-beam-crossing-deadlock-test.lisp

;;; Dedicated zero-action regression for the one cascade shape ARBITRATE-BEAM-CROSSINGS can
;;; never resolve: three beams that block each other in a closed loop rather than a
;;; chain.  Compare problem-beam-crossing-cascade-test.lisp's four-beam loop, which
;;; resolves cleanly -- the difference is not priority or tie-breaking but the loop's
;;; length.  Checking every one of the eight possible active/inactive combinations by
;;; hand for this three-beam loop confirms none of them is self-consistent: whichever
;;; crossing a combination keeps active always turns out to block, or be blocked by,
;;; another member of the same combination.  That is a structural fact about an odd
;;; cascade, independent of which beam has priority, so no tie-break rule could fix it.
;;;
;;; Because a genuine three-way deadlock leaves the state permanently INCONSISTENT-STATE
;;; and no ordinary action or later propagation pass can undo that, this problem never
;;; runs UPDATE-CROSSING-STATUS! on the real planner state at all -- doing so would
;;; poison the one state the planner needs to stay valid, exactly as
;;; problem-propagation-convergence-limit-test.lisp avoids corrupting its own real start
;;; state.  Instead the characterization helper copies the state, runs
;;; UPDATE-CROSSING-STATUS! only on that copy, and confirms the copy alone becomes
;;; inconsistent while the real planner state is untouched.
;;;
;;; Geometry: transmitter-a/receiver-a run from (10,10) to (0,0); transmitter-b/
;;; receiver-b from (10,0) to (0,10); transmitter-c/receiver-c from (-5,3) to (15,3).
;;; Every pair crosses properly once -- (5,5), (3,3), and (7,3) -- and each beam's own
;;; transmitter is closer to one crossing than the other, giving crossing-a-b blocks
;;; crossing-a-c, crossing-a-c blocks crossing-b-c, and crossing-b-c blocks crossing-a-b:
;;; a closed three-way loop.
;;;
;;; No action or initialization action runs propagation, so the expected minimum path
;;; length is 0.

(in-package :ww)

(ww-set *problem-name* beam-crossing-deadlock-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent       (agent1)
  transmitter (transmitter-a transmitter-b transmitter-c)
  receiver    (receiver-a receiver-b receiver-c)
  location    (loc1)
  gate        (unused-gate)
  hue         (indigo)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech beam-direct)
(include-tech beam-crossing)
(include-tech visibility)


;;;; INITIALIZATION ;;;;
;;;; No init-action runs PROPAGATE-CHANGES! here -- see the file header.  These facts
;;;; are static, so they sit inertly until the characterization helper below copies the
;;;; state and derives the crossing topology only on that isolated copy.


(define-init
  (has-location agent1 loc1)

  (has-chroma transmitter-a indigo)  (has-chroma receiver-a indigo)
  (has-chroma transmitter-b indigo)  (has-chroma receiver-b indigo)
  (has-chroma transmitter-c indigo)  (has-chroma receiver-c indigo)

  (coupled transmitter-a receiver-a)  (beam-via transmitter-a () receiver-a)
  (coupled transmitter-b receiver-b)  (beam-via transmitter-b () receiver-b)
  (coupled transmitter-c receiver-c)  (beam-via transmitter-c () receiver-c)

  (apparatus-coords> transmitter-a 10 10)  (apparatus-coords> receiver-a 0 0)
  (apparatus-coords> transmitter-b 10 0)   (apparatus-coords> receiver-b 0 10)
  (apparatus-coords> transmitter-c -5 3)   (apparatus-coords> receiver-c 15 3)

  (location-coords> loc1 20 20)
)


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim beam-crossing-deadlock-contract
  (let* ((before (database *start-state*))
         (trial (copy-problem-state *start-state*))
         (result
           (funcall 'update-crossing-status! trial)))
    (and
      (null result)
      (state-is-inconsistent trial)
      (equal (database *start-state*) before)
      (not (state-is-inconsistent *start-state*))))
  (expect-registrations :action '()))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-crossing-deadlock-scenario-valid ()
  (= (length (get-current-beam-crossings)) 3))


(define-goal
  (beam-crossing-deadlock-scenario-valid))
