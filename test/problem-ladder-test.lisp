;;; Filename: problem-ladder-test.lisp
;;;
;;; Focused regression for ladders as a transparent mobility provider.  The planning lane
;;; composes WALK, LADDER, LADDER, and WALK segments into one MOVE action.  The first climb
;;; requires both ladder1 and screen1; the second requires ladder2.  Because both fixtures
;;; are reached only after earlier segments, the route proves exact positioning is evaluated
;;; at each hypothetical intermediate source rather than only at the agent's initial location.
;;;
;;; Independent probes reject carrying cargo, using a ladder positioned elsewhere, omitting
;;; the positioned ladder from the edge means, crossing a closed gate, and landing at a lethal
;;; destination.  A supported agent retains a hypothetical grounded closure but cannot invoke
;;; MOVE until an explicit configuration transition leaves the support.
;;;
;;; Expected minimum solution: one MOVE from ENTRY to GOAL with the complete four-segment
;;; route witness.

(in-package :ww)


(ww-set *problem-name* ladder-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (climber carrying-agent supported-agent misplaced-agent
         unlisted-agent gate-agent unsafe-agent canonical-agent)
  location (entry lower middle upper goal
            carry-start carry-goal
            supported-start supported-goal
            misplaced-start misplaced-ladder-site misplaced-goal
            unlisted-start unlisted-goal
            gate-start gate-goal
            unsafe-start unsafe-goal
            canonical-start canonical-goal)
  ladder (ladder1 ladder2 ladder3 ladder4 ladder5 ladder6 ladder7 ladder8
          canonical-ladder-a canonical-ladder-b)
  screen (screen1 unlisted-screen)
  gate (closed-gate)
  box (carried-box support-box)
  gun (unsafe-gun))


(include-tech walkability)
(include-tech ladder)
(include-tech gun)


(define-init
  ;; Planned heterogeneous route.  Both ladder positions are consulted from hypothetical
  ;; intermediate sources while mobility closure is being computed.
  (has-location climber entry)
  (has-position ladder1 lower)
  (has-position ladder2 middle)
  (walk-via> entry () lower)
  (climb-via> lower (ladder1 screen1) middle)
  (climb-via> middle (ladder2) upper)
  (walk-via> upper () goal)

  ;; Carrying blocks the ladder itself.
  (has-location carrying-agent carry-start)
  (holding carrying-agent carried-box)
  (has-position ladder3 carry-start)
  (climb-via> carry-start (ladder3) carry-goal)

  ;; The closure describes hypothetical ground travel, while MOVE enforces actual grounding.
  (has-location supported-agent supported-start)
  (has-location support-box supported-start)
  (on supported-agent support-box)
  (has-position ladder4 supported-start)
  (climb-via> supported-start (ladder4) supported-goal)

  ;; The edge starts where the agent stands, but its named ladder is fixed elsewhere.
  (has-location misplaced-agent misplaced-start)
  (has-position ladder5 misplaced-ladder-site)
  (climb-via> misplaced-start (ladder5) misplaced-goal)

  ;; The ladder is correctly positioned but absent from the edge's clear means list.
  (has-location unlisted-agent unlisted-start)
  (has-position ladder6 unlisted-start)
  (climb-via> unlisted-start (unlisted-screen) unlisted-goal)

  ;; Every item in the flat means conjunction must pass.
  (has-location gate-agent gate-start)
  (has-position ladder7 gate-start)
  (climb-via> gate-start (ladder7 closed-gate) gate-goal)

  ;; A clear edge may not land at a lethal destination.
  (has-location unsafe-agent unsafe-start)
  (has-position ladder8 unsafe-start)
  (climb-via> unsafe-start (ladder8) unsafe-goal)
  (threatens unsafe-gun unsafe-goal)

  ;; Two listed and positioned ladders are effect-equivalent.  The provider must select
  ;; exactly one by name, independently of their authored order.
  (has-location canonical-agent canonical-start)
  (has-position canonical-ladder-a canonical-start)
  (has-position canonical-ladder-b canonical-start)
  (climb-via> canonical-start
              (canonical-ladder-b canonical-ladder-a)
              canonical-goal))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper ladder-test-move-updates (state agent)
  "Return every MOVE update generated for AGENT in STATE."
  (let* ((action (find 'move *actions* :key #'action.name))
         (args (list agent)))
    (when (member args (get-precondition-args action state) :test #'equal)
      (let ((pre-result (apply (action.pre-defun-name action) state args)))
        (when pre-result
          (if (eql pre-result t)
              (funcall (action.eff-defun-name action) state)
              (apply (action.eff-defun-name action) state pre-result)))))))


(define-test-helper ladder-test-updates-to (state agent destination)
  "Return AGENT's MOVE updates whose endpoint is DESTINATION."
  (remove-if-not
    (lambda (update)
      (eql (third (update.instantiations update)) destination))
    (ladder-test-move-updates state agent)))


(define-test-claim ladder-move-retains-complete-route
  (let ((updates
          (ladder-test-updates-to *start-state* 'climber 'goal)))
    (and (= (length updates) 1)
         (equal
           (fourth (update.instantiations (first updates)))
           '((walk entry nil lower)
             (ladder lower (ladder1 screen1) middle)
             (ladder middle (ladder2) upper)
             (walk upper nil goal))))))


(define-test-claim ladder-provider-selects-one-fixture
  (let ((updates
          (ladder-test-updates-to
            *start-state* 'canonical-agent 'canonical-goal)))
    (and (= (length updates) 1)
         (equal
           (fourth (update.instantiations (first updates)))
           '((ladder canonical-start
               (canonical-ladder-a canonical-ladder-b)
               canonical-goal))))))


(define-query ladder-climber-at-goal ()
  (has-location climber goal))


(define-test-claim ladder-route-replays-exactly
  (multiple-value-bind (state success-p failure)
      (apply-action-to-state
        '(move climber entry goal
          ((walk entry nil lower)
           (ladder lower (ladder1 screen1) middle)
           (ladder middle (ladder2) upper)
           (walk upper nil goal)))
        *start-state*
        nil)
    (declare (ignore failure))
    (and success-p
         (funcall (symbol-function 'ladder-climber-at-goal) state)))
  (multiple-value-bind (state success-p failure)
      (apply-action-to-state
        '(move climber entry goal
          ((walk entry nil lower)
           (ladder lower (ladder2 screen1) middle)
           (ladder middle (ladder1) upper)
           (walk upper nil goal)))
        *start-state*
        nil)
    (declare (ignore state))
    (and (not success-p)
         (consp failure)
         (eql (first failure) :state-mismatch))))


(define-test-claim ladder-action-boundaries-are-preserved
  (and (not (find 'use-ladder *actions* :key #'action.name))
       (not (ladder-test-move-updates *start-state* 'carrying-agent))
       (not (ladder-test-move-updates *start-state* 'supported-agent))
       (not (ladder-test-move-updates *start-state* 'misplaced-agent))
       (not (ladder-test-move-updates *start-state* 'unlisted-agent))
       (not (ladder-test-move-updates *start-state* 'gate-agent))
       (not (ladder-test-move-updates *start-state* 'unsafe-agent))))


(define-query ladder-scenarios-valid ()
  (and
    ;; The heterogeneous route is one directed mobility closure and leaves the agent grounded.
    (has-location climber goal)
    (not (has-location climber entry))
    (not (exists (?support support)
           (on climber ?support)))
    (traversable climber entry goal)
    (not (traversable climber goal entry))

    ;; The means list is a flat conjunction.  Its empty boundary is vacuously clear.
    (all-clear climber '())
    (all-clear climber '(ladder1 screen1))
    (all-clear climber '(ladder1))
    (all-clear climber '(screen1))

    ;; Carrying blocks ladder and screen passability.
    (has-location carrying-agent carry-start)
    (holding carrying-agent carried-box)
    (not (all-clear carrying-agent '(ladder3)))
    (not (all-clear carrying-agent '(unlisted-screen)))
    (not (traversable carrying-agent carry-start carry-goal))

    ;; The provider is grounded in configuration space; MOVE owns the check that the
    ;; actual agent is currently grounded.
    (has-location supported-agent supported-start)
    (on supported-agent support-box)
    (traversable supported-agent supported-start supported-goal)

    ;; Exact fixture position and membership are both required.
    (has-position ladder5 misplaced-ladder-site)
    (not (traversable misplaced-agent misplaced-start misplaced-goal))
    (all-clear unlisted-agent '(unlisted-screen))
    (not (traversable unlisted-agent unlisted-start unlisted-goal))

    ;; A closed item blocks the conjunction, and an unsafe destination blocks the segment.
    (not (open closed-gate))
    (not (all-clear gate-agent '(ladder7 closed-gate)))
    (not (traversable gate-agent gate-start gate-goal))
    (lethal unsafe-gun)
    (not (safe unsafe-goal))
    (not (traversable unsafe-agent unsafe-start unsafe-goal))))


(define-goal
  (ladder-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation ladder-obstacle-allows-carrying obstacle-clear
  (?agent agent
   ?obstacle (either gate screen ladder floor-gears wall-gears angled-gears))
  (or (and (gate ?obstacle) (gate-open-for-object ?agent ?obstacle))
      (and (screen ?obstacle) (not (bind (holding ?agent $any-held-object))))
      (ladder ?obstacle)
      (and (or (floor-gears ?obstacle)
               (wall-gears ?obstacle)
               (angled-gears ?obstacle))
           (stream-obstacle-clear ?agent ?obstacle)))
  "Drops the not-holding guard from ladder passability.  The carrying-agent
   probe must then make this characterization fail.")


(define-query-mutation ladder-obstacle-ignores-closed-gate obstacle-clear
  (?agent agent
   ?obstacle (either gate screen ladder floor-gears wall-gears angled-gears))
  (or (gate ?obstacle)
      (and (screen ?obstacle) (not (bind (holding ?agent $any-held-object))))
      (and (ladder ?obstacle) (not (bind (holding ?agent $any-held-object))))
      (and (or (floor-gears ?obstacle)
               (wall-gears ?obstacle)
               (angled-gears ?obstacle))
           (stream-obstacle-clear ?agent ?obstacle)))
  "Drops the open-state guard from gate passability.  The closed-gate probe
   must then make this characterization fail.")


(define-query-mutation ladder-provider-ignores-position usable-ladder-at-source
  (?ladder ladder ?source location ?means)
  (do ?source (member ?ladder ?means))
  "Drops exact fixture positioning.  The misplaced-agent probe must then make
   this characterization fail.")


(define-query-mutation ladder-provider-ignores-means-membership usable-ladder-at-source
  (?ladder ladder ?source location ?means)
  (do ?means (has-position ?ladder ?source))
  "Drops edge-means membership.  The unlisted-agent probe must then make this
   characterization fail.")
