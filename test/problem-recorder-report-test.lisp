;;; Filename: problem-recorder-report-test.lisp

;;; Zero-action characterization of the two-phase recorder report.  The mapped agent names
;;; deliberately have no star convention.  A synthetic integrated solution follows the
;;; documented Windtunnel block pattern: live, ghost, live, ghost, live.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-report-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (operator-alpha playback-echo)
  connector (tool-alpha tool-echo)
  recorder (recorder1)
  location (site-a site-b site-c site-d))


(include-tech recorder)


(define-init
  (recording-copy> operator-alpha playback-echo)
  (recording-copy> tool-alpha tool-echo)
  (has-position recorder1 site-a))


(setf
  (symbol-function 'recorder-report-characterization-p)
  (lambda ()
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
                 (funcall (symbol-function 'printout-solution) empty-solution))))
           (solution-position (search "(START-STATE)" combined-printed))
           (recording-position (search "Recording phase:" combined-printed))
           (playback-position (search "Playback phase:" combined-printed)))
      (and
        (equal *solution-report-printers* '(print-recorder-report))
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
        (< solution-position recording-position playback-position)))))


(define-query recorder-report-scenarios-valid ()
  (recorder-report-characterization-p))


(define-goal
  (recorder-report-scenarios-valid))
