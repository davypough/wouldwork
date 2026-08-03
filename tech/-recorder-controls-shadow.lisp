;;; Filename: -recorder-controls-shadow.lisp

;;; Recording-side controller evaluation shared by gates and wall gears.  It mirrors
;;; -controls' DNF polarity but reads recording plate and receiver state.  The aggregate
;;; remains textually separate from CONTROL-ON so the propagation walker sees disjoint
;;; playback and recording read sets.
;;;
;;; REQUIRES:
;;;   nested : -controls (CONTROLS and ordinary control schema); -recorder-plate-shadow;
;;;            -recorder-receiver-shadow
;;; PROVIDES:
;;;   queries : recording-controller-energized, recording-control-on

(include-tech -controls)
(include-tech -recorder-plate-shadow)
(include-tech -recorder-receiver-shadow)

(in-package :ww)


(define-query recording-controller-energized (?controller (either receiver plate))
  (or (and (receiver ?controller)
           (recording-active ?controller))
      (and (pressure-plate ?controller)
           (recording-depressed ?controller))
      (and (toggle-plate ?controller)
           (recording-latched ?controller))))


(define-query recording-control-on (?device ?uncontrolled-default)
  ;; The recording-side twin of -controls' CONTROL-ON: identical DNF polarity and the same
  ;; uncontrolled-default argument, reading ghost-only controller state.
  (do (assign $control-on ?uncontrolled-default)
      (if (bind (controls $clauses ?device $mode))
        (do (assign $any-clause-on
              (ww-loop for $clause in $clauses
                       thereis
                         (ww-loop for $controller in $clause
                                  always
                                    (recording-controller-energized $controller))))
            (if (eql $mode 'normal)
              (assign $control-on $any-clause-on)
              (if (eql $mode 'inverted)
                (assign $control-on (not $any-clause-on))))))
      $control-on))
