;;; Filename: problem-blower-cycle-test.lisp

;;; Blower destination-cycle regression.  Two always-on fixed angled blowers aim at one
;;; another's source.  That authored graph is legal while empty: ESCAPE-CYCLE removes BOX1
;;; before propagation, after which both blowers turn and the state converges.  In contrast,
;;; TRIGGER-ACTIVE-CYCLE leaves BOX1 on BLOWER1.  Propagation then arcs it from BLOWER1 to
;;; BLOWER2 and back forever; the ten-pass cap marks that successor inconsistent and search
;;; discards it.  Expected minimum solution: the one safe escape action.

(in-package :ww)

(ww-set *problem-name* blower-cycle-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES AND TECHNOLOGY ;;;;


(define-types
  location (source1 source2 safe)
  box (box1)
  angled-blower (blower1 blower2))


(include-tech angled-blower)


;;;; INITIALIZATION ;;;;


(define-init
  (has-position blower1 source1)
  (aimed-at blower1 source2)
  (has-position blower2 source2)
  (aimed-at blower2 source1)
  (has-location box1 source1)
  (on box1 blower1))


;;;; CYCLIC AND SAFE TRANSITIONS ;;;;


(define-action trigger-active-cycle
  1
  ()
  (on box1 blower1)
  ("> test leaves box1 in the active blower cycle")
  (assert
    (finally (propagate-changes!))))


(define-action escape-cycle
  1
  ()
  (on box1 blower1)
  ("> test removes box1 before activating the cyclic blower graph")
  (assert
    (not (on box1 blower1))
    (not (has-location box1 source1))
    (has-location box1 safe)
    (finally (propagate-changes!))))


;;;; CHARACTERIZATION ;;;;


(define-test-helper active-blower-cycle-successor-rejected-p (state)
  "Whether the active cycle is discarded without changing its parent state."
  (let* ((action (find 'trigger-active-cycle *actions* :key #'action.name))
         (before (database state))
         (saved-dropped-count *inconsistent-states-dropped*)
         (precondition-result
           (and (member nil (get-precondition-args action state) :test #'equal)
                (funcall (action.pre-defun-name action) state))))
    (and
      precondition-result
      (unwind-protect
        (let* ((*actions* (list action))
               (children
                 (generate-children
                   (make-node :state state :depth 0))))
          (and (null children)
               (= *inconsistent-states-dropped* (1+ saved-dropped-count))
               (equal (database state) before)
               (not (state-is-inconsistent state))))
        (setf *inconsistent-states-dropped* saved-dropped-count)))))


(define-test-claim active-blower-cycle-is-state-dependent
  (active-blower-cycle-successor-rejected-p *start-state*))


(define-query blower-cycle-safe-state ()
  (and (has-location box1 safe)
       (not (has-location box1 source1))
       (not (has-location box1 source2))
       (not (exists (?support support)
              (on box1 ?support)))
       (turning blower1)
       (turning blower2)
       (blowing blower1)
       (blowing blower2)
       (not (inconsistent-state))))


(define-goal
  (blower-cycle-safe-state))
