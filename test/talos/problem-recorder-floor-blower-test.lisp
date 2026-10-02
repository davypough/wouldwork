;;; Fixed floor-blower recorder isolation.  One live/ghost box pair exercises both
;;; asymmetric activity cases without duplicating any blower object:
;;;
;;;   * playback-only activation launches the live box, while the ghost remains resting
;;;     on an equivalently controlled source; and
;;;   * recording-only activation launches the ghost, while the live box remains resting
;;;     on an equivalently controlled source.
;;;
;;; Expected minimum path length: 4.

(in-package :ww)


(ww-set *problem-name* recorder-floor-blower-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  switch (live-switch recorded-switch)
  floor-blower (live-drive ghost-ignores-drive
                 ghost-drive live-ignores-drive)
  location (control-site storage
            live-source live-destination
            ghost-ignore-source ghost-ignore-destination
            ghost-source ghost-destination
            live-ignore-source live-ignore-destination)
  test-phase (phase0 phase1 phase2 phase3 phase4))


(include-tech switch)
(include-tech -recorder-blower-shadow)
(include-tech -recorder-init-checks)
(include-tech floor-blower)


(define-dynamic-relations
  (current-phase test-phase))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)

  (has-location live-agent control-site)
  (has-location ghost-agent control-site)
  (has-location live-box storage)
  (has-location ghost-box storage)
  (has-position recorder1 control-site)

  (apparatus-coords> live-switch 0 0)
  (apparatus-coords> recorded-switch 1 0)

  (has-position live-drive live-source)
  (aimed-at live-drive live-destination)
  (has-elevation live-destination 10)

  (has-position ghost-ignores-drive ghost-ignore-source)
  (aimed-at ghost-ignores-drive ghost-ignore-destination)
  (has-elevation ghost-ignore-destination 10)

  (has-position ghost-drive ghost-source)
  (aimed-at ghost-drive ghost-destination)
  (has-elevation ghost-destination 10)

  (has-position live-ignores-drive live-ignore-source)
  (aimed-at live-ignores-drive live-ignore-destination)
  (has-elevation live-ignore-destination 10)

  (controls ((live-switch)) live-drive normal)
  (controls ((live-switch)) ghost-ignores-drive normal)
  (controls ((recorded-switch)) ghost-drive normal)
  (controls ((recorded-switch)) live-ignores-drive normal)

  (current-phase phase0))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action launch-live-box
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (current-phase phase0))
  ("> playback-only blower launches live box")
  (assert (switched-on live-switch)
          (has-location live-box live-source)
          (on live-box live-drive)
          (not (current-phase phase0))
          (current-phase phase1)
          (finally (propagate-changes!))))


(define-action ghost-ignores-playback-blower
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (current-phase phase1)
       (has-location live-box live-destination)
       (not (recording-turning live-drive)))
  ("> ghost ignores playback-only blower")
  (assert (has-location ghost-box ghost-ignore-source)
          (on ghost-box ghost-ignores-drive)
          (not (current-phase phase1))
          (current-phase phase2)
          (finally (propagate-changes!))))


(define-action launch-ghost-box
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (current-phase phase2)
       (has-location ghost-box ghost-ignore-source)
       (on ghost-box ghost-ignores-drive))
  ("> recording-only blower launches ghost box")
  (assert (not (switched-on live-switch))
          (recording-switched-on recorded-switch)
          (not (on ghost-box ghost-ignores-drive))
          (has-location ghost-box ghost-source)
          (on ghost-box ghost-drive)
          (not (current-phase phase2))
          (current-phase phase3)
          (finally (propagate-changes!))))


(define-action live-ignores-recording-blower
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (current-phase phase3)
       (has-location ghost-box ghost-destination)
       (not (turning ghost-drive))
       (recording-turning ghost-drive))
  ("> live box ignores recording-only blower")
  (assert (has-location live-box live-ignore-source)
          (on live-box live-ignores-drive)
          (not (current-phase phase3))
          (current-phase phase4)
          (finally (propagate-changes!))))


(define-goal
  (and (current-phase phase4)
       (has-location ghost-box ghost-destination)
       (not (on ghost-box ghost-drive))
       (has-location live-box live-ignore-source)
       (on live-box live-ignores-drive)
       (not (has-location live-box live-ignore-destination))
       (not (turning live-ignores-drive))
       (recording-turning live-ignores-drive)))
