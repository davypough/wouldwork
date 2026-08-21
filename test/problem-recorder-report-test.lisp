;;; Filename: problem-recorder-report-test.lisp

;;; Executable one-cycle characterization of recorder reporting.  The open-cycle path uses
;;; the documented live/ghost/live/ghost/live block pattern and leaves the ghost away from
;;; the recorder, exercising synthesized return and stop markers.  A second accepted path
;;; closes explicitly.  Expected ordinary harness path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-report-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (operator-alpha playback-echo)
  recorder (recorder1)
  location (site-a site-b))


(include-tech recorder)
(include-tech walkability)


(define-dynamic-relations
  (report-prepared)
  (report-live-before)
  (report-ghost-first)
  (report-live-middle)
  (report-live-after))


(define-init
  (recording-copy> operator-alpha playback-echo)
  (has-position recorder1 site-a)
  (has-location operator-alpha site-a)
  (traverse-via walking site-a () site-b))


(define-action prepare-report
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (recording-in-progress)))
  (">" ?agent "prepares the report fixture")
  (assert (report-prepared)))


(define-action report-live-before
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "acts before the first ghost block")
  (assert (report-live-before)))


(define-action report-ghost-first
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "performs the first ghost block")
  (assert (report-ghost-first)))


(define-action report-live-middle
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "acts between ghost blocks")
  (assert (report-live-middle)))


(define-action report-live-after
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "acts after the last ghost block")
  (assert (report-live-after)))


(define-test-helper recorder-report-solution (path)
  (let ((validation (validate-action-sequence *start-state* path)))
    (unless (action-sequence-validation-success-p validation)
      (error "Recorder report fixture path failed at ~S: ~S"
             (action-sequence-validation-failure-action validation)
             (action-sequence-validation-failure-reason validation)))
    (let ((final-state (action-sequence-validation-final-state validation)))
      (make-solution
        :depth (length path)
        :time (problem-state.time final-state)
        :value (problem-state.value final-state)
        :path path
        :goal final-state))))


(define-test-helper recorder-open-report-path ()
  '((1.0 (prepare-report operator-alpha))
    (2.0 (start-recorder operator-alpha))
    (3.0 (report-live-before operator-alpha))
    (4.0 (report-ghost-first playback-echo))
    (5.0 (report-live-middle operator-alpha))
    (6.0 (move playback-echo
           ((walk site-a nil site-b))))
    (7.0 (report-live-after operator-alpha))))


(define-test-helper recorder-explicit-report-path ()
  '((1.0 (prepare-report operator-alpha))
    (2.0 (start-recorder operator-alpha))
    (3.0 (report-ghost-first playback-echo))
    (4.0 (stop-recorder playback-echo))))


(define-test-claim recorder-report-contract
  (expect-registrations :solution-validator '(validate-recorder-solution))
  (expect-registrations :solution-printer '(print-recorder-report))
  (let* ((path (recorder-open-report-path))
         (saved-path (copy-tree path))
         (solution (recorder-report-solution path))
         (report (build-recorder-report solution))
         (cycle (first (getf report :cycles)))
         (printed
           (with-output-to-string (stream)
             (print-recorder-report solution stream)))
         (empty-solution (make-solution :goal *start-state*))
         (combined-printed
           (with-output-to-string (stream)
             (let ((*standard-output* stream))
               (funcall 'printout-solution empty-solution))))
         (solution-position (search "START-STATE" combined-printed))
         (setup-position (search "Setup phase:" combined-printed))
         (recording-position (search "Recording phase:" combined-printed))
         (playback-position (search "Playback phase:" combined-printed)))
    (and
      (eq (getf report :integrated) path)
      (equal path saved-path)
      (= (getf report :cycle-count) 1)
      (equal (getf report :setup) (list (first path)))
      (equal (getf cycle :integrated) path)
      (eql (getf cycle :closure) :synthesized)
      (= (getf cycle :depth) 7)
      (= (getf cycle :elapsed-time) 7)
      (zerop (getf cycle :value-change))
      (equal
        (getf report :recording)
        (list
          (second path)
          '(pause)
          (fourth path)
          '(pause)
          (sixth path)
          '(pause)
          '(move playback-echo
             ((walk site-b nil site-a)))
          '(stop-recorder)))
      (equal
        (getf report :playback)
        (list
          '(pause) (third path)
          '(resume) (fourth path)
          '(pause) (fifth path)
          '(resume) (sixth path)
          '(pause) (seventh path)))
      (equal (getf report :totals)
             '(:depth 7 :elapsed-time 7.0 :value-change 0.0))
      (search "Closure: synthesized." printed)
      (search "Cycle metrics: depth 7" printed)
      (search "Complete solution totals: depth 7" printed)
      solution-position
      setup-position
      recording-position
      playback-position
      (< solution-position setup-position recording-position playback-position))))


(define-test-claim recorder-report-explicit-setup-contract
  (let* ((path (recorder-explicit-report-path))
         (solution (recorder-report-solution path))
         (report (build-recorder-report solution))
         (cycle (first (getf report :cycles)))
         (printed
           (with-output-to-string (stream)
             (print-recorder-report solution stream))))
    (and
      (equal (getf report :setup) (list (first path)))
      (equal (getf report :recording)
             (list (second path) (third path) (fourth path)))
      (equal (getf report :playback) (list (third path)))
      (eql (getf cycle :closure) :explicit)
      (= (getf cycle :depth) 4)
      (= (getf cycle :elapsed-time) 4)
      (search "Closure: explicit." printed)
      (search "Complete solution totals: depth 4" printed))))


(define-test-claim recorder-report-legacy-no-boundary-contract
  (let* ((path '((1.0 (prepare-report operator-alpha))))
         (report (build-recorder-report (recorder-report-solution path)))
         (cycle (first (getf report :cycles))))
    (and
      (= (getf report :cycle-count) 1)
      (null (getf report :setup))
      (null (getf report :trailing-setup))
      (equal (getf cycle :integrated) path)
      (equal (getf report :recording)
             '((start-recorder) (pause) (stop-recorder)))
      (equal (getf report :playback)
             (list '(pause) (first path)))
      (eql (getf cycle :closure) :synthesized)
      (= (getf cycle :depth) 1)
      (= (getf (getf report :totals) :depth) 1))))


(define-goal
  (always-true))
