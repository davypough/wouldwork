;;; Filename: problem-gun-blower-test.lisp

;;; Gun/blower regression for -threat's post-effect safety backstop.  Both gears are
;;; uncontrolled (always turning, -gears-fan's own default), so their mounted fans blow from
;;; initialization.  Stepping onto either fan launches its agent with no walk/jump/ladder
;;; destination precondition.
;;;
;;; The planning lane makes loft safe first: agent1 picks up jammer1, jams gun1 through a
;;; hand-authored clear sightline, then steps on fan1 and is launched.  Its characterization
;;; checks the complete resulting state, not just arrival at loft.
;;;
;;; The isolated negative lane leaves gun2 armed over loft2.  Its STEP-ON precondition is
;;; valid, but the forced landing must be marked inconsistent by enforce-threat-safety! and
;;; discarded by generate-children.  The goal probes that installed transition directly,
;;; so a broken backstop cannot pass by returning the previously possible one-step solution.
;;;
;;; Expected minimum solution (3 steps): pickup-jammer jammer1; jam-target gun1 at lower1;
;;; step-on fan1.


(in-package :ww)


(ww-set *problem-name* gun-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


;;;; TYPES ;;;;


(define-types
  agent (agent1 agent2)
  location (lower1 loft lower2 loft2)
  jammer (jammer1)
  gun (gun1 gun2)
  floor-gears (gears1 gears2)
  fan (fan1 fan2)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gun)
(include-tech jammer)
(include-tech floor-blower)
(include-tech step)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Positive planning lane.
  (has-location agent1 lower1)
  (has-location jammer1 lower1)
  (has-location fan1 lower1)
  (has-position gears1 lower1)
  (los-to-apparatus lower1 () gun1)
  (mounted-on fan1 gears1)
  (threatens gun1 (loft))
  (aimed-at gears1 loft)

  ;; Isolated negative lane.  gun2 has no jamming sightline and remains armed.
  (has-location agent2 lower2)
  (has-location fan2 lower2)
  (has-position gears2 lower2)
  (mounted-on fan2 gears2)
  (threatens gun2 (loft2))
  (aimed-at gears2 loft2)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-test-helper gun-blower-unsafe-step-rejected-p (state agent fixture)
  "Whether an applicable STEP-ON has no legitimate child from STATE."
  (let* ((action (find 'step-on *actions* :key #'action.name))
         (args (list agent fixture))
         (precondition-result
           (and (member args (get-precondition-args action state) :test #'equal)
                (apply (action.pre-defun-name action) state args))))
    (and precondition-result
         (let ((saved-dropped-count *inconsistent-states-dropped*))
           (unwind-protect
             (let ((*actions* (list action)))
               (null (generate-children
                       (make-node :state state :depth 0))))
             (setf *inconsistent-states-dropped* saved-dropped-count))))))


(define-query gun-blower-scenarios-valid ()
  (and
    ;; The positive lane completed a safe forced launch and retains its supporting state.
    (has-location agent1 loft)
    (not (on agent1 fan1))
    (has-location jammer1 lower1)
    (jamming jammer1 gun1)
    (not (holding agent1 jammer1))
    (not (lethal gun1))
    (safe loft)
    (turning gears1)
    (blowing fan1)
    (mounted-on fan1 gears1)
    (= (location-elevation loft) 10)

    ;; The isolated unjammed lane remains ready to launch, but its real STEP-ON transition
    ;; must have no legitimate successor because gun2 makes loft2 unsafe.
    (has-location agent2 lower2)
    (not (on agent2 fan2))
    (lethal gun2)
    (not (safe loft2))
    (turning gears2)
    (blowing fan2)
    (mounted-on fan2 gears2)
    (= (location-elevation loft2) 10)
    (gun-blower-unsafe-step-rejected-p state 'agent2 'fan2)))


(define-goal
  (gun-blower-scenarios-valid))
