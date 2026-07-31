;;; Filename: problem-ladder-test.lisp

;;; Combined stageable regression for ladder.lisp.  The planning lane requires two explicit
;;; one-way climbs: lower -> middle uses ladder1 through the flat conjunction
;;; (ladder1 screen1), then middle -> upper uses ladder2.  The agent is empty-handed and on
;;; ground throughout.  This verifies screen/ladder passability, exact fixed positioning,
;;; directional edges, and the fact that climb edges remain individual actions rather than
;;; entering a movement closure.
;;;
;;; Independent stationary probes invoke USE-LADDER's installed precondition directly.
;;; They reject carrying cargo, starting on a support, using a ladder positioned elsewhere,
;;; using a clear means list that omits the selected ladder, crossing a closed gate, and
;;; landing at a lethal destination.  The goal also characterizes ONE-WAY-CLEAR's flat
;;; all-means conjunction, including its vacuously clear empty-list boundary.
;;;
;;; Expected minimum solution (2 steps): use ladder1 from lower to middle; use ladder2 from
;;; middle to upper.  Every negative probe remains in its initial state.


(in-package :ww)


(ww-set *problem-name* ladder-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)

(setf *expected-min-length* 2)


;;;; TYPES ;;;;


(define-types
  agent (climber carrying-agent supported-agent misplaced-agent
         unlisted-agent gate-agent unsafe-agent)
  location (lower middle upper
            carry-start carry-goal
            supported-start supported-goal
            misplaced-start misplaced-ladder-site misplaced-goal
            unlisted-start unlisted-goal
            gate-start gate-goal
            unsafe-start unsafe-goal)
  ladder (ladder1 ladder2 ladder3 ladder4 ladder5 ladder6 ladder7 ladder8)
  screen (screen1 unlisted-screen)
  gate (closed-gate)
  box (carried-box support-box)
  gun (unsafe-gun))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech ladder)
(include-tech gun)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Planned two-action chain.  The first edge's flat means list requires both its ladder
  ;; and screen to pass; the second edge requires its own ladder at middle.
  (has-location climber lower)
  (has-position ladder1 lower)
  (has-position ladder2 middle)
  (climb-via> lower (ladder1 screen1) middle)
  (climb-via> middle (ladder2) upper)

  ;; Carrying blocks the ladder itself, so an otherwise direct climb is inapplicable.
  (has-location carrying-agent carry-start)
  (holding carrying-agent carried-box)
  (has-position ladder3 carry-start)
  (climb-via> carry-start (ladder3) carry-goal)

  ;; A supported agent cannot use a ladder even when the ladder and edge are otherwise clear.
  (has-location supported-agent supported-start)
  (has-location support-box supported-start)
  (on supported-agent support-box)
  (has-position ladder4 supported-start)
  (climb-via> supported-start (ladder4) supported-goal)

  ;; The edge starts where the agent stands, but its named ladder is fixed elsewhere.
  (has-location misplaced-agent misplaced-start)
  (has-position ladder5 misplaced-ladder-site)
  (climb-via> misplaced-start (ladder5) misplaced-goal)

  ;; The ladder is correctly positioned and the screen is passable, but the selected
  ;; ladder is absent from the edge's means list.
  (has-location unlisted-agent unlisted-start)
  (has-position ladder6 unlisted-start)
  (climb-via> unlisted-start (unlisted-screen) unlisted-goal)

  ;; The ladder passes for this empty-handed agent, but every means must pass and the
  ;; unincluded public gate technology leaves closed-gate closed.
  (has-location gate-agent gate-start)
  (has-position ladder7 gate-start)
  (climb-via> gate-start (ladder7 closed-gate) gate-goal)

  ;; The edge and ladder are clear, but an uncontrolled gun makes the destination lethal.
  (has-location unsafe-agent unsafe-start)
  (has-position ladder8 unsafe-start)
  (climb-via> unsafe-start (ladder8) unsafe-goal)
  (threatens unsafe-gun (unsafe-goal)))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; ACTION-PRECONDITION CHARACTERIZATION ;;;;


(defun use-ladder-applicable-p (state agent ladder destination)
  "Whether the installed USE-LADDER action accepts this exact parameter tuple in STATE."
  (let* ((action (find 'use-ladder *actions* :key #'action.name))
         (args (list agent ladder destination)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query ladder-scenarios-valid ()
  (and
    ;; The planned climber completed exactly two explicit directed actions and remains on
    ;; ground.  Neither authored edge exists in reverse.
    (has-location climber upper)
    (not (has-location climber lower))
    (not (has-location climber middle))
    (not (exists (?support support)
           (on climber ?support)))
    (climb-via> lower (ladder1 screen1) middle)
    (climb-via> middle (ladder2) upper)
    (not (climb-via> middle (ladder1 screen1) lower))
    (not (climb-via> upper (ladder2) middle))

    ;; ONE-WAY-CLEAR is a flat conjunction.  Empty means are vacuously clear, while both
    ;; the positive multi-item list and its individual empty-handed obstacles pass.
    (one-way-clear climber '())
    (one-way-clear climber '(ladder1 screen1))
    (one-way-clear climber '(ladder1))
    (one-way-clear climber '(screen1))

    ;; Carrying blocks ladder and screen passability, and the real action stays unavailable.
    (has-location carrying-agent carry-start)
    (holding carrying-agent carried-box)
    (not (exists (?location location)
           (has-location carried-box ?location)))
    (not (one-way-clear carrying-agent '(ladder3)))
    (not (one-way-clear carrying-agent '(unlisted-screen)))
    (not (use-ladder-applicable-p
           state 'carrying-agent 'ladder3 'carry-goal))

    ;; Ground-only: the edge itself is clear, but existing support rejects the action.
    (has-location supported-agent supported-start)
    (on supported-agent support-box)
    (one-way-clear supported-agent '(ladder4))
    (not (use-ladder-applicable-p
           state 'supported-agent 'ladder4 'supported-goal))

    ;; Exact fixed positioning: a clear, listed ladder at another location is unusable.
    (has-location misplaced-agent misplaced-start)
    (has-position ladder5 misplaced-ladder-site)
    (one-way-clear misplaced-agent '(ladder5))
    (not (use-ladder-applicable-p
           state 'misplaced-agent 'ladder5 'misplaced-goal))

    ;; Membership is separate from clearance: this screen-only list is clear, but ladder6
    ;; is not one of its means and therefore cannot instantiate the action.
    (has-location unlisted-agent unlisted-start)
    (one-way-clear unlisted-agent '(unlisted-screen))
    (not (use-ladder-applicable-p
           state 'unlisted-agent 'ladder6 'unlisted-goal))

    ;; One closed item blocks the whole flat conjunction.
    (has-location gate-agent gate-start)
    (not (open closed-gate))
    (not (one-way-clear gate-agent '(ladder7 closed-gate)))
    (not (use-ladder-applicable-p
           state 'gate-agent 'ladder7 'gate-goal))

    ;; Destination safety is an independent final precondition after means clearance.
    (has-location unsafe-agent unsafe-start)
    (one-way-clear unsafe-agent '(ladder8))
    (lethal unsafe-gun)
    (not (safe unsafe-goal))
    (not (use-ladder-applicable-p
           state 'unsafe-agent 'ladder8 'unsafe-goal))))


(define-goal
  (ladder-scenarios-valid))
