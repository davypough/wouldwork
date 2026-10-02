;;; Recorder switch-state separation.  The test applies the real recorder and switch
;;; actions directly: ghost toggles affect both ordinary and recording switch state;
;;; live toggles affect only ordinary state. Closing reseeds the recording shadow
;;; from the live baseline. This matches SWITCH's playback-effect contract.
;;; Expected harness path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-switch-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  switch (switch1)
  gate (gate1)
  location (switch-site))


(include-tech switch)
(include-tech recorder)
(include-tech gate)


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent switch-site)
  (has-position recorder1 switch-site)
  (apparatus-coords> switch1 0 0)
  (reach-via switch-site () switch1)
  (controls ((switch1)) gate1 normal))


(define-init-action initialize-recorder-switch-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper recorder-switch-fact-p (state proposition)
  (member proposition (database state) :test #'equal))


(define-test-helper recorder-switch-apply (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (unless valid-p
      (error "Recorder switch test action failed: ~S (~S)" action diagnostic))
    next-state))


(define-test-helper recorder-switch-action-rejected-p (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (declare (ignore next-state diagnostic))
    (not valid-p)))


(define-test-claim recorder-switch-state-isolation
  (let* ((prestart-ghost-rejected
           (recorder-switch-action-rejected-p
             *start-state* '(toggle-switch ghost-agent switch1)))
         (opened
           (recorder-switch-apply *start-state* '(start-recorder live-agent)))
         (ghost-on
           (recorder-switch-apply opened '(toggle-switch ghost-agent switch1)))
         (live-off
           (recorder-switch-apply ghost-on '(toggle-switch live-agent switch1)))
         (recording-off
           (recorder-switch-apply live-off '(toggle-switch ghost-agent switch1)))
         (closed
           (recorder-switch-apply recording-off '(stop-recorder ghost-agent))))
    (and
      prestart-ghost-rejected
      (not (recorder-switch-fact-p opened '(switched-on switch1)))
      (not (recorder-switch-fact-p opened '(recording-switched-on switch1)))

      (recorder-switch-fact-p ghost-on '(switched-on switch1))
      (recorder-switch-fact-p ghost-on '(recording-switched-on switch1))
      (recorder-switch-fact-p ghost-on '(open gate1))
      (recorder-switch-fact-p ghost-on '(recording-open gate1))

      (not (recorder-switch-fact-p live-off '(switched-on switch1)))
      (recorder-switch-fact-p live-off '(recording-switched-on switch1))
      (not (recorder-switch-fact-p live-off '(open gate1)))
      (recorder-switch-fact-p live-off '(recording-open gate1))

      (recorder-switch-fact-p recording-off '(switched-on switch1))
      (not (recorder-switch-fact-p
             recording-off '(recording-switched-on switch1)))
      (recorder-switch-fact-p recording-off '(open gate1))
      (not (recorder-switch-fact-p recording-off '(recording-open gate1)))

      (recorder-switch-fact-p closed '(switched-on switch1))
      (recorder-switch-fact-p closed '(recording-switched-on switch1))
      (recorder-switch-fact-p closed '(open gate1))
      (recorder-switch-fact-p closed '(recording-open gate1)))))


(define-goal
  (always-true))
