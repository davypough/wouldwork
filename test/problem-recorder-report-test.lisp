;;; Filename: problem-recorder-report-test.lisp

;;; Zero-action characterization of the two-phase recorder report.  The mapped agent names
;;; deliberately have no star convention.  A synthetic integrated solution follows the
;;; documented Windtunnel block pattern: live, ghost, live, ghost, live.  Two ghost agents
;;; cover both terminal cases of the recording sequence: one ends away from the recorder and
;;; has its return walk appended, one is already there and adds nothing.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-report-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (operator-alpha playback-echo operator-beta playback-beta)
  connector (tool-alpha tool-echo)
  recorder (recorder1)
  location (site-a site-b site-c site-d))


(include-tech recorder)
(include-tech walkability)


(enable-recorder-solution)


(define-init
  (recording-copy> operator-alpha playback-echo)
  (recording-copy> operator-beta playback-beta)
  (recording-copy> tool-alpha tool-echo)
  (has-position recorder1 site-a)

  ;; Two ghost agents in the two terminal situations the recording sequence must handle.
  ;; PLAYBACK-ECHO ends its recording away from the recorder with a route back, so the
  ;; report appends its return walk; PLAYBACK-BETA already stands on the recorder, so it
  ;; contributes no marker.  Real walkability is required for the distinction to exist at
  ;; all -- under -walkability's identity default the closure never leaves the agent's own
  ;; location, and a ghost away from the recorder could never close its recording.
  (has-location operator-alpha site-a)
  (has-location playback-echo site-b)
  (has-location operator-beta site-a)
  (has-location playback-beta site-a)
  (walk-via site-a () site-b))


(define-test-claim recorder-report-contract
  (expect-registrations :solution-validator '(validate-recorder-solution))
  (expect-registrations :solution-printer '(print-recorder-report))
  (let* ((path
             '((1.0 (pickup-connector operator-alpha tool-alpha site-a))
               (2.0 (pickup-connector playback-echo tool-echo site-a))
               (3.0 (connect-connector playback-echo tool-echo transmitter1 site-a))
               (4.0 (walk operator-alpha site-a site-b))
               (5.0 (connect-connector operator-alpha tool-alpha tool-echo site-b))
               (6.0 (walk operator-alpha site-b site-c))
               (7.0 (walk playback-echo site-a site-b))
               (8.0 (step-on playback-echo plate1))
               (9.0 (walk operator-alpha site-c site-d))))
           (saved-path (copy-tree path))
           (solution
             (make-solution
               :depth 9
               :time 9.0
               :path path
               :goal *start-state*))
           (report (build-recorder-report solution))
           (printed
             (with-output-to-string (stream)
               (print-recorder-report solution stream)))
           (empty-solution
             (make-solution :goal *start-state*))
           (combined-printed
             (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (funcall 'printout-solution empty-solution))))
           (solution-position (search "(START-STATE)" combined-printed))
           (recording-position (search "Recording phase:" combined-printed))
           (playback-position (search "Playback phase:" combined-printed)))
    (and
      (eq (getf report :integrated) path)
        (equal path saved-path)
        (equal
          (getf report :recording)
          '((start-recorder)
            (pause)
            (2.0 (pickup-connector playback-echo tool-echo site-a))
            (3.0 (connect-connector playback-echo tool-echo transmitter1 site-a))
            (pause)
            (7.0 (walk playback-echo site-a site-b))
            (8.0 (step-on playback-echo plate1))
            (pause)
            ;; Appended by RECORDER-RETURN-WALKS: the searched path stopped at the goal
            ;; with PLAYBACK-ECHO away from the recorder, and a recording cannot be stopped
            ;; from there.  PLAYBACK-BETA is already on the recorder and adds nothing, so
            ;; exactly one marker appears and it carries no step number.
            (walk playback-echo site-b site-a)
            (stop-recorder)))
        (equal
          (getf report :playback)
          '((pause)
            (1.0 (pickup-connector operator-alpha tool-alpha site-a))
            (resume)
            (2.0 (pickup-connector playback-echo tool-echo site-a))
            (3.0 (connect-connector playback-echo tool-echo transmitter1 site-a))
            (pause)
            (4.0 (walk operator-alpha site-a site-b))
            (5.0 (connect-connector operator-alpha tool-alpha tool-echo site-b))
            (6.0 (walk operator-alpha site-b site-c))
            (resume)
            (7.0 (walk playback-echo site-a site-b))
            (8.0 (step-on playback-echo plate1))
            (pause)
            (9.0 (walk operator-alpha site-c site-d))))
        (search "Recording phase:" printed)
        (search "Playback phase:" printed)
        solution-position
        recording-position
      playback-position
      (< solution-position recording-position playback-position))))


(define-goal
  (always-true))
