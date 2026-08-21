;;; Filename: problem-ladder-test.lisp
;;;
;;; Focused regression for ladders as a transparent mobility provider.  The planning lane
;;; composes WALK, LADDER, LADDER, and WALK segments into one MOVE action.  The first climb
;;; requires both ladder1 and screen1; the second requires ladder2.  Because both fixtures
;;; are reached only after earlier segments, the route proves exact positioning is evaluated
;;; at each hypothetical intermediate source rather than only at the agent's initial location.
;;;
;;; Independent probes reject carrying cargo, crossing a closed gate, and landing at a lethal
;;; destination.  Two valid climbs contain decoy ladders that respectively violate exact
;;; positioning and means-list membership, while isolated initialization probes reject edges
;;; having no valid source ladder at all.  A supported agent climbs through an explicit
;;; singleton configuration transition, leaving its support and landing on destination ground
;;; without composing farther through the transparent grounded closure in the same MOVE.
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
            supported-start supported-goal supported-beyond
            misplaced-start misplaced-ladder-site misplaced-goal
            unlisted-start unlisted-goal
            gate-start gate-goal
            unsafe-start unsafe-goal
            canonical-start canonical-goal)
  ladder (ladder1 ladder2 ladder3 ladder4 ladder5 ladder6 ladder7 ladder8
          misplaced-source-ladder listed-source-ladder
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
  (traverse-via> walking entry () lower)
  (traverse-via> climbing lower ((ladder1 screen1)) middle)
  (traverse-via> climbing middle ((ladder2)) upper)
  (traverse-via> walking upper () goal)

  ;; Carrying blocks the ladder itself.
  (has-location carrying-agent carry-start)
  (holding carrying-agent carried-box)
  (has-position ladder3 carry-start)
  (traverse-via> climbing carry-start ((ladder3)) carry-goal)

  ;; A supported source uses one explicit ladder configuration transition.  The walking edge
  ;; beyond its landing proves that transition remains a single support-state boundary.
  (has-location supported-agent supported-start)
  (has-location support-box supported-start)
  (on supported-agent support-box)
  (has-position ladder4 supported-start)
  (traverse-via> climbing supported-start ((ladder4)) supported-goal)
  (traverse-via> walking supported-goal () supported-beyond)

  ;; A misplaced listed ladder is only a decoy; the source-positioned listed ladder must
  ;; provide the valid segment.
  (has-location misplaced-agent misplaced-start)
  (has-position ladder5 misplaced-ladder-site)
  (has-position misplaced-source-ladder misplaced-start)
  (traverse-via> climbing misplaced-start
              ((ladder5 misplaced-source-ladder))
              misplaced-goal)

  ;; A source-positioned ladder omitted from the means list is likewise only a decoy.
  (has-location unlisted-agent unlisted-start)
  (has-position ladder6 unlisted-start)
  (has-position listed-source-ladder unlisted-start)
  (traverse-via> climbing unlisted-start
              ((listed-source-ladder unlisted-screen))
              unlisted-goal)

  ;; Every item in the flat means conjunction must pass.
  (has-location gate-agent gate-start)
  (has-position ladder7 gate-start)
  (traverse-via> climbing gate-start ((ladder7 closed-gate)) gate-goal)

  ;; A clear edge may not land at a lethal destination.
  (has-location unsafe-agent unsafe-start)
  (has-position ladder8 unsafe-start)
  (traverse-via> climbing unsafe-start ((ladder8)) unsafe-goal)
  (threatens unsafe-gun unsafe-goal)

  ;; Two listed and positioned ladders are effect-equivalent.  The provider must select
  ;; exactly one by name, independently of their authored order.
  (has-location canonical-agent canonical-start)
  (has-position canonical-ladder-a canonical-start)
  (has-position canonical-ladder-b canonical-start)
  (traverse-via> climbing canonical-start
              ((canonical-ladder-b canonical-ladder-a))
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


(define-test-helper ladder-test-route-endpoint-location (route)
  "Return ROUTE's final location from either a grounded or configuration endpoint."
  (let ((endpoint (fourth (car (last route)))))
    (if (consp endpoint)
      (first endpoint)
      endpoint)))


(define-test-helper ladder-test-updates-to (state agent destination)
  "Return AGENT's MOVE updates whose endpoint is DESTINATION."
  (remove-if-not
    (lambda (update)
      (eql (ladder-test-route-endpoint-location
             (second (update.instantiations update)))
           destination))
    (ladder-test-move-updates state agent)))


(define-test-claim ladder-move-retains-complete-route
  (let ((updates
          (ladder-test-updates-to *start-state* 'climber 'goal)))
    (and (= (length updates) 1)
         (equal
           (second (update.instantiations (first updates)))
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
           (second (update.instantiations (first updates)))
           '((ladder canonical-start
               (canonical-ladder-a canonical-ladder-b)
               canonical-goal))))))


(define-test-claim ladder-provider-respects-position-and-membership
  (let ((misplaced-updates
          (ladder-test-updates-to
            *start-state* 'misplaced-agent 'misplaced-goal))
        (unlisted-updates
          (ladder-test-updates-to
            *start-state* 'unlisted-agent 'unlisted-goal)))
    (and (= (length misplaced-updates) 1)
         (equal
           (second (update.instantiations (first misplaced-updates)))
           '((ladder misplaced-start
               (misplaced-source-ladder ladder5)
               misplaced-goal)))
         (= (length unlisted-updates) 1)
         (equal
           (second (update.instantiations (first unlisted-updates)))
           '((ladder unlisted-start
               (listed-source-ladder unlisted-screen)
               unlisted-goal))))))


(define-query ladder-climber-at-goal ()
  (has-location climber goal))


(define-test-claim ladder-route-replays-exactly
  (multiple-value-bind (state success-p failure)
      (apply-action-to-state
        '(move climber
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
        '(move climber
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


(define-test-claim ladder-supported-source-transition
  (equal
    (configuration-transition-results *start-state* 'supported-agent)
    '((ladder
        (supported-start support-box)
        (ladder4)
        (supported-goal ground))))
  (let ((landing-updates
          (ladder-test-updates-to
            *start-state* 'supported-agent 'supported-goal)))
    (and (= (length landing-updates) 1)
         (equal
           (second (update.instantiations (first landing-updates)))
           '((ladder
               (supported-start support-box)
               (ladder4)
               (supported-goal ground))))
         (not
           (ladder-test-updates-to
             *start-state* 'supported-agent 'supported-beyond))))
  (multiple-value-bind (state success-p failure)
      (apply-action-to-state
        '(move supported-agent
          ((ladder
             (supported-start support-box)
             (ladder4)
             (supported-goal ground))))
        *start-state*
        nil)
    (declare (ignore failure))
    (and success-p
         (equal
           (agent-configuration state 'supported-agent)
           '(supported-goal ground)))))


(define-test-claim ladder-action-boundaries-are-preserved
  (and (not (find 'use-ladder *actions* :key #'action.name))
       (not (ladder-test-move-updates *start-state* 'carrying-agent))
       (not (ladder-test-move-updates *start-state* 'gate-agent))
       (not (ladder-test-move-updates *start-state* 'unsafe-agent))))


(define-test-claim ladder-authoring-validation
  (null
    (validate-init-literals
      '((has-position ladder6 unlisted-start)
        (traverse-via> climbing unlisted-start ((ladder6)) unlisted-goal))
      :checks '(ladder-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-position ladder6 unlisted-start)
          (traverse-via climbing unlisted-start ((ladder6)) unlisted-goal))
        :checks '(ladder-init-check)))
    'init-check-failure
    :containing "Climbing traversal must be directed"
    :check 'ladder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-position ladder6 unlisted-start)
          (traverse-via> climbing unlisted-start ((unlisted-screen)) unlisted-goal))
        :checks '(ladder-init-check)))
    'init-check-failure
    :containing "has no listed ladder positioned at its source"
    :check 'ladder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-position ladder5 misplaced-ladder-site)
          (traverse-via> climbing misplaced-start ((ladder5)) misplaced-goal))
        :checks '(ladder-init-check)))
    'init-check-failure
    :containing "has no listed ladder positioned at its source"
    :check 'ladder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-position ladder6 unlisted-start)
          (traverse-via> climbing unlisted-start () unlisted-goal))
        :checks '(ladder-init-check)))
    'init-check-failure
    :containing "Climbing clause NIL"
    :check 'ladder-init-check))


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

    ;; The transparent closure still describes hypothetical ground travel through and beyond
    ;; the climb, while the actual supported agent gets exactly one configuration transition.
    (has-location supported-agent supported-start)
    (on supported-agent support-box)
    (traversable supported-agent supported-start supported-goal)
    (traversable supported-agent supported-start supported-beyond)
    (equal
      (configuration-transition-results supported-agent)
      '((ladder
          (supported-start support-box)
          (ladder4)
          (supported-goal ground))))

    ;; Valid edges ignore decoys that fail one of the provider's two requirements.
    (has-position ladder5 misplaced-ladder-site)
    (traversable misplaced-agent misplaced-start misplaced-goal)
    (all-clear unlisted-agent '(unlisted-screen))
    (traversable unlisted-agent unlisted-start unlisted-goal)

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
