;;; Filename: problem-stairs-test.lisp
;;;
;;; Focused regression coverage for stairs as a transparent mobility provider.
;;; The solution composes WALK, STAIRS, and WALK segments into one MOVE action.
;;;
;;; Independent scenarios characterize symmetric and directional stairs, unrestricted
;;; elevation change, carrying, enabling obstacles, unsafe intermediates, grounded action
;;; boundaries, exact route witnesses, and canonical selection among equivalent routes.
;;;
;;; Expected minimum path length: 1.

(in-package :ww)

(ww-set *problem-name* stairs-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (main-agent carrying-agent canonical-agent supported-agent)
  location (main-start stairs-foot stairs-top main-goal
            directional-low directional-high
            carrying-low carrying-high
            screen-low screen-high
            closed-low closed-high
            unsafe-low unsafe-middle unsafe-goal
            canonical-start canonical-a canonical-b canonical-goal
            supported-low supported-high)
  gate (open-gate closed-gate)
  screen (screen1)
  connector (carried-connector)
  box (support-box)
  gun (danger))


(include-tech walkability)
(include-tech stairs)


(define-init
  (has-location main-agent main-start)
  (has-location carrying-agent carrying-low)
  (holding carrying-agent carried-connector)
  (has-location canonical-agent canonical-start)
  (has-location supported-agent supported-low)
  (has-location support-box supported-low)
  (on supported-agent support-box)

  (open open-gate)
  (lethal danger)
  (threatens danger unsafe-middle)

  ;; A large elevation change is intentional: the staircase supplies the ascent.
  (has-elevation main-start 0)
  (has-elevation stairs-foot 0)
  (has-elevation stairs-top 20)
  (has-elevation main-goal 20)
  (traversal-via> walking main-start () stairs-foot)
  (traversal-via stairway stairs-foot ((open-gate)) stairs-top)
  (traversal-via> walking stairs-top () main-goal)

  (traversal-via> stairway directional-low () directional-high)

  ;; Carrying is allowed on unobstructed stairs.  A screen uses the shared
  ;; passability rule and therefore requires empty hands.
  (traversal-via stairway carrying-low () carrying-high)
  (traversal-via stairway screen-low ((screen1)) screen-high)
  (traversal-via stairway closed-low ((closed-gate)) closed-high)

  ;; An unsafe landing cannot become a closure through-node.
  (traversal-via stairway unsafe-low () unsafe-middle)
  (traversal-via walking unsafe-middle () unsafe-goal)

  ;; Equal-length heterogeneous routes retain one deterministic witness.
  (traversal-via> stairway canonical-start () canonical-a)
  (traversal-via> stairway canonical-start () canonical-b)
  (traversal-via> walking canonical-a () canonical-goal)
  (traversal-via> walking canonical-b () canonical-goal)

  (traversal-via stairway supported-low () supported-high))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper stairs-test-move-updates (state agent)
  "Return every MOVE update generated for AGENT in STATE."
  (let* ((action (find 'move *actions* :key #'action.name))
         (args (list agent)))
    (when (member args (get-precondition-args action state) :test #'equal)
      (let ((pre-result (apply (action.pre-defun-name action) state args)))
        (when pre-result
          (if (eql pre-result t)
              (funcall (action.eff-defun-name action) state)
              (apply (action.eff-defun-name action) state pre-result)))))))


(define-test-claim stairs-compose-with-walking
  (let* ((updates (stairs-test-move-updates *start-state* 'main-agent))
         (goal-update
           (find 'main-goal updates
                 :key (lambda (update)
                        (fourth
                          (car
                            (last
                              (second
                                (update.instantiations update)))))))))
    (and (= (count 'main-goal updates
                   :key (lambda (update)
                          (fourth
                            (car
                              (last
                                (second
                                  (update.instantiations update)))))))
            1)
         (equal
           (second (update.instantiations goal-update))
           '((walk main-start nil stairs-foot)
             (stairs stairs-foot (open-gate) stairs-top)
             (walk stairs-top nil main-goal))))))


(define-query stairs-scenarios-valid ()
  (and
    ;; TRAVERSAL-VIA in stairway mode is symmetric; TRAVERSAL-VIA> is not.
    (traversable main-agent stairs-foot stairs-top)
    (traversable main-agent stairs-top stairs-foot)
    (traversable main-agent directional-low directional-high)
    (not (traversable main-agent directional-high directional-low))

    ;; Stairs do not impose a total elevation-difference bound.
    (= (location-elevation stairs-foot) 0)
    (= (location-elevation stairs-top) 20)
    (traversable main-agent stairs-foot stairs-top)

    ;; Ordinary carrying is allowed, while passability-sensitive obstacles retain
    ;; their established restrictions.
    (holding carrying-agent carried-connector)
    (traversable carrying-agent carrying-low carrying-high)
    (traversable main-agent screen-low screen-high)
    (not (traversable carrying-agent screen-low screen-high))
    (not (traversable main-agent closed-low closed-high))

    ;; Unsafe endpoints cannot be used as intermediate nodes.
    (not (traversable main-agent unsafe-low unsafe-middle))
    (not (traversable main-agent unsafe-low unsafe-goal))

    ;; The closure may describe hypothetical grounded travel, but MOVE itself cannot
    ;; leave a support without an explicit configuration transition.
    (traversable supported-agent supported-low supported-high)))


(define-test-claim stairs-scenarios-are-valid
  (and (funcall (symbol-function 'stairs-scenarios-valid) *start-state*)
       (not (stairs-test-move-updates *start-state* 'supported-agent))))


(define-test-claim stairs-route-selection-is-canonical
  (let* ((updates (stairs-test-move-updates *start-state* 'canonical-agent))
         (goal-updates
           (remove-if-not
             (lambda (update)
               (eql (fourth
                      (car
                        (last
                          (second (update.instantiations update)))))
                    'canonical-goal))
             updates)))
    (and (= (length goal-updates) 1)
         (equal
           (second (update.instantiations (first goal-updates)))
           '((stairs canonical-start nil canonical-a)
             (walk canonical-a nil canonical-goal))))))


(define-query stairs-solution-valid ()
  (and (has-location main-agent main-goal)
       (not (has-location main-agent main-start))))


(define-goal
  (stairs-solution-valid))
