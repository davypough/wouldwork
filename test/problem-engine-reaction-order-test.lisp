;;; Filename: problem-engine-reaction-order-test.lisp

;;; Two reactions in one derived driver, chained in both directions in a problem small
;;; enough to solve.  The characterization goal checks the driver structure and the
;;; converged physical state rather than relying on the historical experiment this file
;;; originally described.
;;;
;;; The retained driver candidates are two state derivations, two physical reactions,
;;; and the final safety backstop:
;;;
;;;   update-plate-status!
;;;   update-blower-status!
;;;   update-floor-blowing-status!
;;;   update-wall-blower-status!
;;;   enforce-threat-safety!
;;;
;;; UPDATE-RECEIVER-STATUS! is contributed through -controls but is removed because
;;; RECEIVER is empty.  The structural check verifies the exact candidate set, derived
;;; order, derivation strata, reaction classification, installed driver body, and the
;;; HAS-LOCATION/(on ...) dependencies shared by the two reactions.
;;;
;;; Forward lane, floor then wall.  BOX1 is placed on the fixed floor blower at PAD0.  The floor reaction
;;; launches it to PAD1, whose declared elevation 0 lets WGEARS1's stream at elevation 1
;;; strike its unit-height body at the inclusive upper boundary, 0 < 1 <= 0 + 1.  The wall
;;; reaction then sweeps it to FAR later in the same driver pass.
;;;
;;; Reverse lane, wall then floor.  BOX2 starts on bare ground at REVERSE-SOURCE, where
;;; WGEARS2 sweeps it onto the clear, flush top of FGEARS2 at REVERSE-FAN.  Because the floor
;;; reaction has already run in that pass, BOX2 remains there until the next fixpoint pass;
;;; then FGEARS2 launches it to the lofted destination REVERSE-FAR.  This lane makes failure
;;; to iterate observable without swapping or hand-authoring the driver.
;;;
;;; The lanes are isolated: BOX2 is outside the agent's reachable location and every blower
;;; is fixed.  One plate controls all four drives, so the expected minimum solution remains
;;; three steps: pick up BOX1, put it on FGEARS1, and step onto PLATE1, in
;;; either viable ordering.  No WALK-VIA is authored.


(in-package :ww)


(ww-set *problem-name* engine-reaction-order-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (pad0 pad1 far reverse-source reverse-fan reverse-far)
  pressure-plate (plate1)
  box (box1 box2)
  floor-blower (fgears1 fgears2)
  wall-blower (wgears1 wgears2)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)         ;depressed; update-plate-status!
(include-tech floor-blower)  ;update-floor-blowing-status!; blow-occupants-away!; drop-occupants!
(include-tech wall-blower)   ;update-wall-blower-status!; sweep-occupants-away!
(include-tech box)           ;pickup-box; put-box
(include-tech step)          ;step configuration transitions
(include-tech walkability)  ;walk-via; mobility-results; traversable; move


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects.
  (has-location agent1 pad0)
  (has-location box1 pad0)
  (has-location box2 reverse-source)

  ;; Fixed-position objects.  Each wall-gears location is the location its stream sweeps.
  (has-position plate1 pad0)
  (has-position fgears1 pad0)
  (has-position fgears2 reverse-fan)
  (has-position wgears1 pad1)
  (has-position wgears2 reverse-source)

  ;; PAD1 couples the forward reactions at ground level.  REVERSE-FAN is explicitly ground
  ;; level so the reverse wall reaction lands BOX2 on FFAN2's flush top.
  (has-elevation pad1 0)
  (has-elevation reverse-fan 0)

  ;; One plate turns every gear set, firing both independent lanes together.
  (controls ((plate1)) fgears1 normal)
  (controls ((plate1)) fgears2 normal)
  (controls ((plate1)) wgears1 normal)
  (controls ((plate1)) wgears2 normal)

  ;; Forward chain: floor -> wall.  Reverse chain: wall -> floor.
  (aimed-at fgears1 pad1)
  (aimed-at wgears1 far)
  (aimed-at wgears2 reverse-fan)
  (aimed-at fgears2 reverse-far)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-test-helper reaction-order-relations-valid-p (expected-order)
  (multiple-value-bind (reads writes base-facts adjacency components)
      (propagation-graph expected-order)
    (declare (ignore adjacency components))
    (let ((floor-reads (aref reads 2))
          (floor-writes (aref writes 2))
          (wall-reads (aref reads 3))
          (wall-writes (aref writes 3)))
      (and (equal (loop for index below (length expected-order)
                        collect (not (null (aref base-facts index))))
                  '(nil nil t t t))
           (gethash 'has-location floor-reads)
           (gethash 'on floor-reads)
           (gethash 'has-location floor-writes)
           (gethash 'on floor-writes)
           (gethash 'has-location wall-reads)
           (gethash 'on wall-reads)
           (gethash 'has-location wall-writes)
           (gethash 'on wall-writes)))))


(define-test-helper reaction-order-structure-valid-p ()
  (let* ((expected-order
           '(update-plate-status!
             update-blower-status!
             update-floor-blowing-status!
             update-wall-blower-status!
             enforce-threat-safety!))
         (expected-strata
           '((update-plate-status!)
             (update-blower-status!)))
         (candidates
           (remove-if #'update-quantifies-only-over-empty-types-p
                      (driver-candidate-updates))))
    (multiple-value-bind (derived component-alist strata)
        (derived-propagation-order candidates)
      (declare (ignore component-alist))
      (and (= (length candidates) (length expected-order))
           (subsetp candidates expected-order :test #'eq)
           (equal derived expected-order)
           (equal strata expected-strata)
           (equal (get 'propagate-consequences! :raw-body)
                  (derived-propagation-driver-body expected-order))
           (reaction-order-relations-valid-p expected-order)))))


(define-test-claim reaction-order-structure
  (reaction-order-structure-valid-p))


(define-query reaction-order-scenarios-valid ()
  (and ;; Shared activation lifecycle.
       (has-location agent1 pad0)
       (on agent1 plate1)
       (depressed plate1)
       (turning fgears1)
       (turning fgears2)
       (turning wgears1)
       (turning wgears2)
       (blowing fgears1)
       (blowing fgears2)
       (blowing wgears1)
       (blowing wgears2)

       ;; Forward lane has cleared its source and intermediate state.
       (has-location box1 far)
       (not (has-location box1 pad1))

       ;; Reverse lane required a later floor-reaction pass and remains hovering.
       (has-location box2 reverse-far)
       (not (has-location box2 reverse-source))
       (not (has-location box2 reverse-fan))
       (not (exists (?support support)
              (or (on box1 ?support)
                  (on box2 ?support))))

       ;; Geometry that makes the two reaction handoffs possible.
       (= (location-elevation pad1) 0)
       (= (blower-elevation wgears1) 1)
       (= (declared-height box1) 1)
       (= (location-elevation reverse-fan) 0)
       (= (top fgears2) 0)
       (= (blower-elevation wgears2) 1)
       (= (declared-height box2) 1)
       (= (location-elevation reverse-far) 10)))


(define-goal
  (reaction-order-scenarios-valid))
