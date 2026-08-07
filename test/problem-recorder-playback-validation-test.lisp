;;; Filename: problem-recorder-playback-validation-test.lisp

;;; Focused characterization of recorder solution validation.  The recording-final toggle
;;; state is deliberately different from the authored initial state.  A valid integrated
;;; path proves that playback restores the initial snapshot; an alternate ordering proves
;;; that playback actions are still checked at their exact prefixes.  Two further paths
;;; separate the two ways a recording can end away from its recorder: moving to AWAY-SITE
;;; is accepted, because the ghost can still move back and stop the recorder, while moving
;;; to STRANDED-SITE is rejected, because the one-way edge into it leaves no return.
;;;
;;; Expected minimum path length: one.

(in-package :ww)


(ww-set *problem-name* recorder-playback-validation-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  toggle-plate (plate1)
  location (recorder-site away-site stranded-site goal-site))


(include-tech recorder)
(include-tech plate)
(include-tech step)
(include-tech jump)
(include-tech walkability)


(enable-recorder-solution)


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
  (has-location live-box recorder-site)
  (has-location ghost-box recorder-site)
  (has-position recorder1 recorder-site)
  (has-position plate1 recorder-site)
  (walk-via recorder-site () away-site)
  (walk-via> away-site () stranded-site))  ;one-way: a ghost that moves in cannot close its recording


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action finish-while-plate-clear
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (has-location ?agent recorder-site)
       (not (latched plate1)))
  (">" ?agent "finishes while plate1 retains its restored initial value")
  (assert (has-location ?agent goal-site)))


(define-test-helper recorder-step-transition-action
    (agent source-place destination-place)
  "Return the exact recorded action form for a local plate transition."
  (let ((source (list 'recorder-site source-place))
        (destination (list 'recorder-site destination-place)))
    (list 'move agent
          (list (list 'step source nil destination)))))


(define-test-claim recorder-playback-validation-contract
  ;; The shared transition provider obeys recorder-side policy for mobile supports while
  ;; still allowing fixed plates in both views.
  (let ((ghost-transitions
          (configuration-transition-results *start-state* 'ghost-agent))
        (live-transitions
          (configuration-transition-results *start-state* 'live-agent)))
    (and
      (member '(jump (recorder-site ground) nil (recorder-site ghost-box))
              ghost-transitions :test #'equal)
      (not (member '(jump (recorder-site ground) nil (recorder-site live-box))
                   ghost-transitions :test #'equal))
      (member '(jump (recorder-site ground) nil (recorder-site live-box))
              live-transitions :test #'equal)
      (not (member '(jump (recorder-site ground) nil (recorder-site ghost-box))
                   live-transitions :test #'equal))
      (member '(step (recorder-site ground) nil (recorder-site plate1))
              ghost-transitions :test #'equal)
      (member '(step (recorder-site ground) nil (recorder-site plate1))
              live-transitions :test #'equal)))
  (let ((recording-validation
          (validate-action-sequence
            *start-state*
            (list
              (recorder-step-transition-action
                'ghost-agent 'ground 'plate1)
              (recorder-step-transition-action
                'ghost-agent 'plate1 'ground)))))
    (and (action-sequence-validation-success-p recording-validation)
         (member '(recording-latched plate1)
                 (list-database
                   (problem-state.idb
                     (action-sequence-validation-final-state
                       recording-validation)))
                 :test #'equal)))
  (validate-recorder-solution
    *start-state*
    (list
      '(1.0 (finish-while-plate-clear live-agent))
      (list 2.0
            (recorder-step-transition-action
              'ghost-agent 'ground 'plate1))
      (list 3.0
            (recorder-step-transition-action
              'ghost-agent 'plate1 'ground)))
    *start-state*)
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state*
        (list
          (list 1.0
                (recorder-step-transition-action
                  'ghost-agent 'ground 'plate1))
          '(2.0 (finish-while-plate-clear live-agent))
          (list 3.0
                (recorder-step-transition-action
                  'ghost-agent 'plate1 'ground)))
        *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :playback)
         (eql (getf diagnostic :reason) :action-failed)))
  (validate-recorder-solution
    *start-state*
    '((1.0 (finish-while-plate-clear live-agent))
      (2.0 (move ghost-agent
             ((walk recorder-site nil away-site)))))
    *start-state*)
  (multiple-value-bind (valid-p diagnostic)
      (validate-recorder-solution
        *start-state*
        '((1.0 (finish-while-plate-clear live-agent))
          (2.0 (move ghost-agent
                 ((walk recorder-site nil away-site)
                  (walk away-site nil stranded-site)))))
        *start-state*)
    (and (not valid-p)
         (eql (getf diagnostic :phase) :recording)
         (eql (getf diagnostic :reason) :agents-cannot-close))))


(define-goal
  (has-location live-agent goal-site))
