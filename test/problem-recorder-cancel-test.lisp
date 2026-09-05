;;; Filename: problem-recorder-cancel-test.lisp

;;; Focused characterization of live playback cancellation.  Cancellation consumes the
;;; open cycle, preserves live progress, removes an unfinished ghost and its physical
;;; dependencies immediately, and creates a generic closed boundary without pretending
;;; that the ghost performed STOP-RECORDER.  Expected ordinary harness path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-cancel-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(ww-set *max-recorder-cycles* 2)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  location (recorder-site away-site))


(include-tech recorder)


(define-dynamic-relations
  (cancel-progress))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)
  (has-location live-agent recorder-site)
  (has-location live-box recorder-site)
  (has-position recorder1 recorder-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action mark-cancel-progress
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress)
       (not (cancel-progress)))
  (">" ?agent "makes persistent progress before cancelling")
  (assert (cancel-progress)))


(define-action strand-recording-ghost
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "is unfinished away from the recorder")
  (assert (has-location ?agent away-site)
          (holding ?agent ghost-box)
          (not (has-location ghost-box recorder-site))))


(define-test-helper recorder-cancel-fact-p (state proposition)
  (member proposition (database state) :test #'equal))


(define-test-helper recorder-cancel-apply (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (unless valid-p
      (error "Recorder cancellation action failed: ~S (~S)" action diagnostic))
    next-state))


(define-test-helper recorder-cancel-action-rejected-p (state action)
  (multiple-value-bind (next-state valid-p diagnostic)
      (apply-action-to-state action state nil)
    (declare (ignore next-state diagnostic))
    (not valid-p)))


(define-test-helper recorder-cancel-solution (path)
  (let ((validation (validate-action-sequence *start-state* path)))
    (unless (action-sequence-validation-success-p validation)
      (error "Recorder cancellation path failed at ~S: ~S"
             (action-sequence-validation-failure-action validation)
             (action-sequence-validation-failure-reason validation)))
    (let ((final-state (action-sequence-validation-final-state validation)))
      (make-solution
        :depth (length path)
        :time (problem-state.time final-state)
        :value (problem-state.value final-state)
        :path path
        :goal final-state))))


(define-test-helper recorder-cancel-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (mark-cancel-progress live-agent))
    (3.0 (cancel-playback live-agent))))


(define-test-claim recorder-cancel-action-contract
  (let* ((opened
           (recorder-cancel-apply *start-state* '(start-recorder live-agent)))
         (unfinished
           (recorder-cancel-apply opened '(strand-recording-ghost ghost-agent))))
    (add-proposition '(on live-box ghost-box) (problem-state.idb unfinished))
    (add-proposition '(has-location live-box away-site) (problem-state.idb unfinished))
    (invalidate-problem-state-hash unfinished)
    (let ((cancelled
            (recorder-cancel-apply unfinished '(cancel-playback live-agent))))
      (and
        (funcall (symbol-function 'recorder-cycle-ended) cancelled)
        (not (funcall (symbol-function 'ghost-stops-recorder) cancelled))
        (= (funcall (symbol-function 'recorder-cycle-count) cancelled) 1)
        (not (recorder-state-contains-ghost-reference-p cancelled))
        (recorder-cancel-fact-p cancelled '(has-location live-agent recorder-site))
        (recorder-cancel-fact-p cancelled '(has-location live-box away-site))
        (not (recorder-cancel-fact-p cancelled '(on live-box ghost-box)))
        (not (recorder-cancel-fact-p cancelled '(recording-in-progress)))))))


(define-test-claim recorder-cancel-precondition-contract
  (let ((opened
          (recorder-cancel-apply *start-state* '(start-recorder live-agent))))
    (let ((holding (copy-problem-state opened))
          (away (copy-problem-state opened)))
      (add-proposition '(holding live-agent live-box) (problem-state.idb holding))
      (invalidate-problem-state-hash holding)
      (add-proposition '(has-location live-agent away-site) (problem-state.idb away))
      (invalidate-problem-state-hash away)
      (and
        (recorder-cancel-action-rejected-p
          *start-state* '(cancel-playback live-agent))
        (recorder-cancel-action-rejected-p
          opened '(cancel-playback ghost-agent))
        (recorder-cancel-action-rejected-p
          holding '(cancel-playback live-agent))
        (recorder-cancel-action-rejected-p
          away '(cancel-playback live-agent))))))


(define-test-claim recorder-normal-stop-remains-strict
  (let* ((opened
           (recorder-cancel-apply *start-state* '(start-recorder live-agent)))
         (stopped
           (recorder-cancel-apply opened '(stop-recorder ghost-agent))))
    (and (funcall (symbol-function 'recorder-cycle-ended) stopped)
         (funcall (symbol-function 'ghost-stops-recorder) stopped))))


(define-test-claim recorder-cancel-report-contract
  (let* ((solution (recorder-cancel-solution (recorder-cancel-path)))
         (report (build-recorder-report solution))
         (cycle (first (getf report :cycles))))
    (and
      (= (getf report :cycle-count) 1)
      (eql (getf cycle :closure) :cancelled)
      (equal (getf cycle :recording)
             '((1.0 (start-recorder live-agent))))
      (equal (getf cycle :playback)
             '((2.0 (mark-cancel-progress live-agent))
               (3.0 (cancel-playback live-agent))))
      (multiple-value-bind (cycles trailing diagnostic)
          (parse-recorder-path *start-state* (recorder-cancel-path))
        (and (null diagnostic)
             (null trailing)
             (= (length cycles) 1)
             (recorder-cycle-cancellation-p
               (recorder-path-cycle.ending (first cycles))))))))


(define-test-claim recorder-cancel-reforks-and-retains-no-progress-check
  (let* ((first-cycle (recorder-cancel-path))
         (path
           (append
             first-cycle
             '((4.0 (start-recorder live-agent))
               (5.0 (cancel-playback live-agent)))))
         (validation (validate-action-sequence *start-state* path)))
    (and
      (action-sequence-validation-success-p validation)
      (= (funcall
           (symbol-function 'recorder-cycle-count)
           (action-sequence-validation-final-state validation))
         2)
      (multiple-value-bind (valid-p diagnostic)
          (validate-recorder-cycle-boundary-prefix
            *start-state* path
            (action-sequence-validation-final-state validation))
        (and (not valid-p)
             (equal diagnostic
                    '(:phase :recording
                      :reason :no-persistent-progress
                      :cycle 2)))))))


(define-goal
  (always-true))
