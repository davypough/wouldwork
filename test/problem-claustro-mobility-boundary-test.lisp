;;; Filename: problem-claustro-mobility-boundary-test.lisp

;;; Focused reproduction of Claustro's raised-slab boundary.  The ordinary lane requires
;;; three actions: mount BOX2 at LOCATION10, jump from that support to raised LOCATION12,
;;; then traverse LOCATION12 -> LOCATION13 -> LOCATION11 through one grounded MOVE.  The
;;; first two changes are explicit configuration transitions; only the final walk/jump
;;; route is transparent mobility.
;;;
;;; Independent probes preserve Claustro's one-way empty-handed ladder and demonstrate the
;;; intentional stairs variant: stairs from elevation 0 to 2 bypass the box and compose
;;; with the same raised walk/downward-jump route in one MOVE.
;;;
;;; Expected minimum path length: 3.

(in-package :ww)


(ww-set *problem-name* claustro-mobility-boundary-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


(define-types
  agent (boundary-agent ladder-agent stairs-agent)
  location (location1 location7 location10 location11 location12 location13
            stairs10 stairs11 stairs12 stairs13)
  box (box2)
  ladder (ladder1))


(include-tech jump)
(include-tech walkability)
(include-tech ladder)
(include-tech stairs)


(define-init
  ;; Claustro boundary lane.  Default agent and box heights are both 1, so ground elevation
  ;; 0 cannot reach LOCATION12 at elevation 2, while BOX2's top at elevation 1 can.
  (has-location boundary-agent location10)
  (has-location box2 location10)
  (has-elevation location12 2)
  (has-elevation location13 2)
  (traversal-via jumping location10 () location12)
  (traversal-via walking location12 () location13)
  (traversal-via jumping location13 () location11)

  ;; The real problem's directed, exactly-positioned, empty-handed ladder traversal.
  (has-location ladder-agent location7)
  (has-position ladder1 location7)
  (traversal-via> climbing location7 ((ladder1)) location1)

  ;; Variant lane: stairs deliberately supply the two-unit ascent without a support-state
  ;; boundary, so the entire stairs/walk/downward-jump route is one transparent MOVE.
  (has-location stairs-agent stairs10)
  (has-elevation stairs12 2)
  (has-elevation stairs13 2)
  (traversal-via stairway stairs10 () stairs12)
  (traversal-via walking stairs12 () stairs13)
  (traversal-via jumping stairs13 () stairs11))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper claustro-boundary-route ()
  '((walk location12 nil location13)
    (jump location13 nil location11)))


(define-test-claim claustro-configuration-boundary-contract
  (equal
    (mobility-results *start-state* 'boundary-agent 'location10)
    '((location10 nil)))
  (equal
    (configuration-transition-results *start-state* 'boundary-agent)
    '((jump (location10 ground) nil (location10 box2))))
  (not (funcall (symbol-function 'traversable)
                *start-state* 'boundary-agent 'location10 'location12)))


(define-test-claim claustro-ladder-contract
  (equal
    (remove-if-not (lambda (segment) (eql (first segment) 'ladder))
                   (traversal-segments *start-state* 'ladder-agent 'location7))
    '((ladder location7 (ladder1) location1)))
  (equal
    (assoc 'location1
           (mobility-results *start-state* 'ladder-agent 'location7))
    '(location1 ((ladder location7 (ladder1) location1))))
  (not (funcall (symbol-function 'traversable)
                *start-state* 'ladder-agent 'location1 'location7)))


(define-test-claim claustro-stairs-bypass-contract
  (equal
    (assoc 'stairs11
           (mobility-results *start-state* 'stairs-agent 'stairs10))
    '(stairs11
       ((stairs stairs10 nil stairs12)
        (walk stairs12 nil stairs13)
        (jump stairs13 nil stairs11))))
  (null (configuration-transition-results *start-state* 'stairs-agent)))


(define-test-claim claustro-boundary-replay-contract
  (let* ((route (claustro-boundary-route))
         (actions
           (list
             '(move boundary-agent
                ((jump (location10 ground) nil (location10 box2))))
             '(move boundary-agent
                ((jump (location10 box2) nil (location12 ground))))
             (list 'move 'boundary-agent route)))
         (validation (validate-action-sequence *start-state* actions)))
    (and
      (action-sequence-validation-success-p validation)
      (let ((final-state
              (action-sequence-validation-final-state validation)))
        (and
          (member '(has-location boundary-agent location11)
                  (database final-state)
                  :test #'equal)
          (not (member '(on boundary-agent box2)
                       (database final-state)
                       :test #'equal))
          (member '(has-location box2 location10)
                  (database final-state)
                  :test #'equal)
          (equal (problem-state.instantiations final-state)
                 (list 'boundary-agent route)))))))


(define-test-claim move-display-omits-route-endpoints
  (let ((route (claustro-boundary-route)))
    (and
      (equal
        (action.effect-variables
          (find 'move *actions* :key #'action.name))
        '(?agent $route))
      (equal
        (merge-effect-format 'move (list 'boundary-agent route))
        (list ">" 'boundary-agent "moves via" route)))))


(define-goal
  (and
    (has-location boundary-agent location11)
    (not (on boundary-agent box2))
    (has-location box2 location10)
    (cleartop box2)))
