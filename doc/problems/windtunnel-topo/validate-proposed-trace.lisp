;;; Replay the proposed one-cycle trace; no search. Run in an existing WW REPL.
(in-package :ww)

(stage windtunnel-topo)
(ww-set *depth-cutoff* 8)

(defparameter *windtunnel-proposed-actions*
  '((start-recorder agent1)
    (pickup-connector agent1* connector1* location1)
    (move agent1* ((walk location1 nil location2)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    ;; Physical and recording latches both on; live connector sees source.
    (pickup-connector agent1 connector1 location1)
    (connect-connector agent1 connector1 ground (repeater1 transmitter1) location1)
    (move agent1 ((walk location1 nil location2)))
    (move agent1 ((step (location2 ground) nil (location2 plate1))))
    (move agent1 ((step (location2 plate1) nil (location2 ground))))
    ;; Physical off, recording on. Explicitly pass through location3.
    (move agent1 ((walk location2 (blower1) location3)))
    (move agent1 ((walk location3 (blower1) location4)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    ;; Physical on, recording off. Ghost now places its connector.
    (move agent1* ((walk location2 (blower1) location3)))
    (connect-connector agent1* connector1* ground (repeater1 receiver1) location3)
    (move agent1 ((walk location4 (gate2) location5)))))

(defparameter *windtunnel-trace-validation* nil)
(defparameter *windtunnel-recorder-validation* nil)

(with-open-file
    (stream (merge-pathnames "doc/problems/windtunnel-topo/Proposed-Trace-Validation.txt"
                             (asdf:system-source-directory :wouldwork))
            :direction :output :if-exists :supersede)
  (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
    (format t "~&Windtunnel proposed trace: 17 actions; replay only, no search.~%")
    (setf *windtunnel-trace-validation*
          (validate-action-sequence *start-state* *windtunnel-proposed-actions*
                                    :goal-test (symbol-function 'goal-fn)
                                    :verbose t))
    (format t "~&INTEGRATED: success=~S goal-checked=~S goal-satisfied=~S~%"
            (action-sequence-validation-success-p *windtunnel-trace-validation*)
            (action-sequence-validation-goal-checked-p *windtunnel-trace-validation*)
            (action-sequence-validation-goal-satisfied-p *windtunnel-trace-validation*))
    (cond
      ((not (action-sequence-validation-success-p *windtunnel-trace-validation*))
       (format t "FIRST FAILURE: action ~S ~S~%Reason: ~S~%"
               (action-sequence-validation-failure-index *windtunnel-trace-validation*)
               (action-sequence-validation-failure-action *windtunnel-trace-validation*)
               (action-sequence-validation-failure-reason *windtunnel-trace-validation*)))
      ((action-sequence-validation-goal-satisfied-p *windtunnel-trace-validation*)
       (setf *windtunnel-recorder-validation*
             (multiple-value-list
               (validate-recorder-solution
                 *start-state*
                 (loop for action in *windtunnel-proposed-actions*
                       for index from 1 collect (list index action))
                 (action-sequence-validation-final-state *windtunnel-trace-validation*))))
       (format t "RECORDER (valid-p diagnostic): ~S~%" *windtunnel-recorder-validation*)))))
