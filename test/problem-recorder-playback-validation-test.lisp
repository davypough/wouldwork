;;; Filename: problem-recorder-playback-validation-test.lisp

;;; Focused characterization of recorder solution validation.  The recording-final toggle
;;; state is deliberately different from the authored initial state.  A valid integrated
;;; path proves that playback restores the initial snapshot; an alternate ordering proves
;;; that playback actions are still checked at their exact prefixes.  Two further paths
;;; separate the two ways a recording can end away from its recorder: walking to AWAY-SITE
;;; is accepted, because the ghost can still walk back and stop the recorder, while walking
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
  recorder (recorder1)
  toggle-plate (plate1)
  location (recorder-site away-site stranded-site goal-site))


(include-tech recorder)
(include-tech plate)
(include-tech step)
(include-tech walkability)


(enable-recorder-solution)


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
  (has-position recorder1 recorder-site)
  (has-position plate1 recorder-site)
  (walk-via recorder-site () away-site)
  (walk-via> away-site () stranded-site))  ;one-way: a ghost that walks in cannot close its recording


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


(define-goal
  (has-location live-agent goal-site))
