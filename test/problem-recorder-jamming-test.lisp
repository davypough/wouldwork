;;; Filename: problem-recorder-jamming-test.lisp

;;; Recorder jamming characterization.  Four dedicated jammers must establish the two
;;; environmental views for each supported target kind:
;;;
;;;   - a live jammer opens an ordinary gate but not its recording shadow;
;;;   - a ghost jammer opens both ordinary and recording gate state;
;;;   - a live jammer stops ordinary wall gears while recording gears keep turning;
;;;   - a ghost jammer stops both ordinary and recording wall-gears state.
;;;
;;; A separate live-only plate opens an intervening sight gate during playback.  Untouched
;;; mapped live/ghost probe actors keep equivalent JAM-TARGET requests available at the goal:
;;; the live request sees the playback gate as open, while the ghost request sees its closed
;;; recording shadow and is inapplicable.  Expected minimum path length: four.

(in-package :ww)


(ww-set *problem-name* recorder-jamming-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)


;;;; TYPES ;;;;


(define-types
  agent (live-gate-agent ghost-gate-agent
         live-gears-agent ghost-gears-agent
         live-visibility-agent ghost-visibility-agent)
  jammer (live-gate-jammer ghost-gate-jammer
          live-gears-jammer ghost-gears-jammer
          live-visibility-jammer ghost-visibility-jammer)
  recorder (recorder1)
  pressure-plate (visibility-control)
  gate (sight-gate live-target-gate ghost-target-gate visibility-probe-gate)
  wall-gears (live-target-gears ghost-target-gears)
  location (live-gate-site ghost-gate-site
            live-gears-site ghost-gears-site
            live-visibility-site ghost-visibility-site))


;;;; TECHNOLOGY INCLUDES ;;;;


;; This characterizes recorder mechanics without installing the public solution policy.
(include-tech -recorder-gate-shadow)
(include-tech -recorder-wall-gears-shadow)
(include-tech -recorder-init-checks)
(include-tech plate)
(include-tech jammer)
(include-tech gate)
(include-tech -gears-fan)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (recording-copy> live-gate-agent ghost-gate-agent)
  (recording-copy> live-gears-agent ghost-gears-agent)
  (recording-copy> live-visibility-agent ghost-visibility-agent)
  (recording-copy> live-gate-jammer ghost-gate-jammer)
  (recording-copy> live-gears-jammer ghost-gears-jammer)
  (recording-copy> live-visibility-jammer ghost-visibility-jammer)

  ;; The ghost jammers below must actually JAM-TARGET, which reaches
  ;; OBJECT-MANIPULATION-ALLOWED through PLACEMENT-OPTIONS and so requires an open session.
  (recording-in-progress)

  (has-location live-gate-agent live-gate-site)
  (has-location ghost-gate-agent ghost-gate-site)
  (has-location live-gears-agent live-gears-site)
  (has-location ghost-gears-agent ghost-gears-site)
  (has-location live-visibility-agent live-visibility-site)
  (has-location ghost-visibility-agent ghost-visibility-site)

  (holding live-gate-agent live-gate-jammer)
  (holding ghost-gate-agent ghost-gate-jammer)
  (holding live-gears-agent live-gears-jammer)
  (holding ghost-gears-agent ghost-gears-jammer)
  (holding live-visibility-agent live-visibility-jammer)
  (holding ghost-visibility-agent ghost-visibility-jammer)

  ;; Only the live probe actor occupies this plate.  Shared playback opens SIGHT-GATE;
  ;; ghost-only recording occupancy leaves the recording gate closed.
  (has-position recorder1 live-visibility-site)
  (has-position visibility-control live-visibility-site)
  (on live-visibility-agent visibility-control)
  (controls ((visibility-control)) sight-gate normal)

  ;; The live gate jam itself also crosses SIGHT-GATE.  The ghost gate jam is direct so it
  ;; can establish the positive recording-side gate result independently of that disparity.
  (los-via live-gate-site (sight-gate) live-target-gate)
  (los-via ghost-gate-site () ghost-target-gate)

  ;; Equivalent probe sightlines differ only by the actor's environmental view.
  (los-via live-visibility-site (sight-gate) visibility-probe-gate)
  (los-via ghost-visibility-site (sight-gate) visibility-probe-gate)

  ;; Exact-location placement exercises wall-gears jamming without an unrelated LOS
  ;; dependency.  Both devices are uncontrolled and therefore initially turn in both views.
  (has-position live-target-gears live-gears-site)
  (has-position ghost-target-gears ghost-gears-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS AND INIT CHECKS ;;;;


(define-test-helper recorder-jamming-action-applicable-p
    (state agent target location)
  "Whether JAM-TARGET accepts AGENT, TARGET, and LOCATION in STATE."
  (let ((action (find 'jam-target *actions* :key #'action.name)))
    (and (member (list agent target location)
                 (get-precondition-args action state)
                 :test #'equal)
         (funcall (action.pre-defun-name action)
                  state agent target location))))


(define-test-claim recorder-jamming-init-validation
  ;; Both recording sides may carry authored initial jam state when each jammer is mapped.
  (null
    (validate-init-literals
      '((recording-copy> live-gate-jammer ghost-gate-jammer)
        (recording-copy> live-gears-jammer ghost-gears-jammer)
        (recording-copy> live-visibility-jammer ghost-visibility-jammer)
        (jamming live-gate-jammer live-target-gate)
        (jamming ghost-gate-jammer ghost-target-gate))
      :checks '(recorder-init-check)))

  ;; A location-less active jammer could evade the located-mobile completeness check, so
  ;; JAMMING itself owns an explicit mapping requirement.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((jamming live-gate-jammer live-target-gate))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "JAMMING uses an unmapped jammer"
    :check 'recorder-init-check))


;;;; GOAL ;;;;


(define-goal
  (and
    ;; Shared playback sees every jam; recording state sees ghost jammers only.
    (jamming live-gate-jammer live-target-gate)
    (open live-target-gate)
    (not (recording-jammed live-target-gate))
    (not (recording-open live-target-gate))

    (jamming ghost-gate-jammer ghost-target-gate)
    (open ghost-target-gate)
    (recording-jammed ghost-target-gate)
    (recording-open ghost-target-gate)

    (jamming live-gears-jammer live-target-gears)
    (not (turning live-target-gears))
    (not (recording-jammed live-target-gears))
    (recording-turning live-target-gears)

    (jamming ghost-gears-jammer ghost-target-gears)
    (not (turning ghost-target-gears))
    (recording-jammed ghost-target-gears)
    (not (recording-turning ghost-target-gears))

    ;; The sight gate still differs between layers after all four required actions.
    (depressed visibility-control)
    (not (recording-depressed visibility-control))
    (open sight-gate)
    (not (recording-open sight-gate))
    (visible live-visibility-site visibility-probe-gate)
    (visible ghost-visibility-site visibility-probe-gate)
    (visible-for-object
      live-visibility-agent live-visibility-site visibility-probe-gate)
    (not (visible-for-object
           ghost-visibility-agent ghost-visibility-site visibility-probe-gate))

    ;; Keep both probes untouched so applicability differs only by their visibility view.
    (holding live-visibility-agent live-visibility-jammer)
    (holding ghost-visibility-agent ghost-visibility-jammer)
    (recorder-jamming-action-applicable-p
      state 'live-visibility-agent 'visibility-probe-gate 'live-visibility-site)
    (not (recorder-jamming-action-applicable-p
           state 'ghost-visibility-agent 'visibility-probe-gate 'ghost-visibility-site))))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation recording-jammed-uses-all-jammers recording-jammed
  (?target (either gate wall-gears wall-blower))
  (exists (?jammer jammer)
    (jamming ?jammer ?target))
  "Drops RECORDING-JAMMED's ghost filter.  Live jams must not affect recording state.")


(define-update-mutation recording-gate-update-ignores-jamming
    update-recording-gate-status!
  ()
  (doall (?gate gate)
    (if (recording-control-on ?gate nil)
      (recording-open ?gate)
      (not (recording-open ?gate))))
  "Drops the recording gate's jam override.  The ghost-jammed gate must then stay closed.")


(define-update-mutation recording-gears-update-ignores-jamming
    update-recording-gears-status!
  ()
  (doall (?gears (either wall-gears wall-blower))
    (if (recording-control-on ?gears t)
      (recording-turning ?gears)
      (not (recording-turning ?gears))))
  "Drops recording wall-gears jam suppression.  Ghost-jammed gears must then keep turning.")


(define-action-precondition-mutation jam-target-uses-playback-visibility jam-target
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (or (and (or (gate ?target) (gun ?target))
                (visible ?location ?target))
           (and (or (floor-gears ?target)
                    (wall-gears ?target)
                    (floor-blower ?target)
                    (wall-blower ?target))
                (bind (has-position ?target $t-location))
                (or (eql ?location $t-location)
                    (visible ?location $t-location))))
       (not (jam-disallowed> $a-location ?location ?target))
       (assign $places (placement-options ?agent ?location $any-jammer)))
  "Routes JAM-TARGET through ordinary playback visibility.  The ghost probe must remain
   blocked by the closed recording-side sight gate.")
