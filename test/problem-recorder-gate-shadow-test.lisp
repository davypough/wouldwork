;;; Filename: problem-recorder-gate-shadow-test.lisp

;;; Recorder gate-shadow characterization.  A four-step toggle sequence first establishes
;;; each direction of disagreement between the shared playback gate and the recording gate.
;;; Receiver-controlled gates simultaneously verify direct and relayed recording beams,
;;; that a live-only relay is absent from the recording view, and that a live-only beam
;;; blocker is absent there.
;;;
;;; Expected minimum path length: 4.

(in-package :ww)


(ww-set *problem-name* recorder-gate-shadow-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)

(defparameter *max-pairings* 2)


(define-types
  agent (live-agent ghost-agent live-blocker ghost-blocker)
  connector (live-recording-connector ghost-recording-connector
             live-playback-connector ghost-playback-connector)
  recorder (recorder1)
  toggle-plate (control-plate)
  gate (plate-gate direct-receiver-gate recording-receiver-gate playback-receiver-gate)
  transmitter (direct-transmitter recording-transmitter playback-transmitter)
  receiver (direct-receiver recording-receiver playback-receiver)
  hue (blue)
  location (plate-site recording-relay-site playback-relay-site blocker-site spare-site)
  test-phase (phase0 phase1 phase2 phase3 phase4))


;; This characterizes recorder mechanics without installing the public solution policy.
(include-tech -recorder-gate-shadow)
(include-tech -recorder-wall-gears-shadow)
(include-tech -recorder-init-checks)
(include-tech plate)
(include-tech gate)
(include-tech beam-direct)
(include-tech beam-relay)
(include-tech visibility)
(include-tech walkability)


(define-dynamic-relations
  (current-phase test-phase))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-blocker ghost-blocker)
  (recording-copy> live-recording-connector ghost-recording-connector)
  (recording-copy> live-playback-connector ghost-playback-connector)

  (has-location live-agent plate-site)
  (has-location ghost-agent plate-site)
  (has-location live-blocker blocker-site)
  (has-location ghost-blocker spare-site)
  (has-location live-recording-connector spare-site)
  (has-location ghost-recording-connector recording-relay-site)
  (has-location live-playback-connector playback-relay-site)
  (has-location ghost-playback-connector spare-site)

  (has-position recorder1 plate-site)
  (has-position control-plate plate-site)

  (controls ((control-plate)) plate-gate normal)
  (controls ((direct-receiver)) direct-receiver-gate normal)
  (controls ((recording-receiver)) recording-receiver-gate normal)
  (controls ((playback-receiver)) playback-receiver-gate normal)

  (has-chroma direct-transmitter blue)
  (has-chroma direct-receiver blue)
  (has-chroma recording-transmitter blue)
  (has-chroma playback-transmitter blue)
  (has-chroma recording-receiver blue)
  (has-chroma playback-receiver blue)

  ;; A mapped live blocker cuts the shared direct beam but was absent from recording.
  (coupled direct-transmitter direct-receiver)
  (beam-via direct-transmitter (blocker-site) direct-receiver)

  ;; The ghost relay is valid during recording.  Its playback beam is blocked only by the
  ;; mapped live blocker, which was absent from the recording phase.
  (paired ghost-recording-connector recording-transmitter)
  (paired ghost-recording-connector recording-receiver)
  (los-via recording-relay-site () recording-transmitter)
  (los-via recording-relay-site (blocker-site) recording-receiver)

  ;; The second relay exists only on the mapped live side, so it powers shared playback
  ;; while contributing nothing to recording-side receiver state.
  (paired live-playback-connector playback-transmitter)
  (paired live-playback-connector playback-receiver)
  (los-via playback-relay-site () playback-transmitter)
  (los-via playback-relay-site () playback-receiver)

  (current-phase phase0))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action press-live-side
  1
  ()
  (current-phase phase0)
  ("> shared plate gate opens while the recording gate stays closed")
  (assert (on live-agent control-plate)
          (not (current-phase phase0))
          (current-phase phase1)
          (finally (propagate-changes!))))


(define-action press-ghost-side
  1
  ()
  (and (current-phase phase1)
       (gate-open-for-object live-agent plate-gate)
       (not (gate-open-for-object ghost-agent plate-gate)))
  ("> ghost pressure opens the recording gate without retriggering shared playback")
  (assert (on ghost-agent control-plate)
          (not (current-phase phase1))
          (current-phase phase2)
          (finally (propagate-changes!))))


(define-action clear-both-sides
  1
  ()
  (and (current-phase phase2)
       (gate-open-for-object live-agent plate-gate)
       (gate-open-for-object ghost-agent plate-gate))
  ("> both actors clear the toggle plate")
  (assert (not (on live-agent control-plate))
          (not (on ghost-agent control-plate))
          (not (current-phase phase2))
          (current-phase phase3)
          (finally (propagate-changes!))))


(define-action repress-live-side
  1
  ()
  (current-phase phase3)
  ("> shared playback closes while the recording gate remains open")
  (assert (on live-agent control-plate)
          (not (current-phase phase3))
          (current-phase phase4)
          (finally (propagate-changes!))))


(define-goal
  (and (current-phase phase4)

       (not (open plate-gate))
       (recording-open plate-gate)
       (not (gate-open-for-object live-agent plate-gate))
       (gate-open-for-object ghost-agent plate-gate)
       (not (obstacle-clear live-agent plate-gate))
       (obstacle-clear ghost-agent plate-gate)

       (not (active direct-receiver))
       (recording-active direct-receiver)
       (not (open direct-receiver-gate))
       (recording-open direct-receiver-gate)
       (not (obstacle-clear live-agent direct-receiver-gate))
       (obstacle-clear ghost-agent direct-receiver-gate)

       (not (active recording-receiver))
       (recording-active recording-receiver)
       (not (open recording-receiver-gate))
       (recording-open recording-receiver-gate)
       (not (obstacle-clear live-agent recording-receiver-gate))
       (obstacle-clear ghost-agent recording-receiver-gate)

       (active playback-receiver)
       (not (recording-active playback-receiver))
       (open playback-receiver-gate)
       (not (recording-open playback-receiver-gate))
       (obstacle-clear live-agent playback-receiver-gate)
       (not (obstacle-clear ghost-agent playback-receiver-gate))))
