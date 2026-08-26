;;; Cycle-specific lower-bound supplement for explicit rumin-topo subgoal experiments.
;;; Temporarily add (include-tech topo-lower-bound) to the problem, then paste this section
;;; immediately before its ";;;; GOAL ;;;;" heading.  TOPO-LOWER-BOUND independently
;;; registers the inexpensive finite-resource contributor; this file adds only the
;;; chunk-specific aggregate fallback.
;;;
;;; The cycles 3/4 branch (chunk 4) is VERIFIED: admissible at every depth of the known
;;; 10-action plan, and it cuts that search from 2.2M states unsolved to 6,472 states / 3 s.
;;; The cycles 2/3 branch (chunk 3) is admissible but too weak to pay off yet -- it counts
;;; 6 of the true 15 and leaves pruning until depth 9.  See rumin-topo-lower-bound.md.
;;; Do not install this as a standing Rumin bound: it has rejected a valid suffix when used
;;; at a recorder boundary outside those chunk-specific admissibility arguments.

;;;; LOWER BOUND ;;;;

;; The cycle term dispatches on RECORDER-CYCLE-COUNT and returns 0 at boundaries it does not
;; cover.  Each component counts actions of a disjoint kind (manipulation of one object /
;; session / agent movement), so the components sum validly within the documented chunks.

(define-query rt-some-agent-holds-connector ()
  (exists (?a agent)
    (exists (?c connector)
      (holding ?a ?c))))

(define-query rt-some-agent-holds-weight ()
  (exists (?a agent)
    (or (exists (?t tray) (holding ?a ?t))
        (exists (?b box) (holding ?a ?b)))))

(define-query rt4-blue-cost ()
  ;; RECEIVER1 must end lit.  From dark that needs a CONNECT at minimum, plus a PICKUP
  ;; unless some agent already holds a connector.  No plate-driven gate occludes the
  ;; loc2 -> loc17 -> receiver1 chain, so there is no cheaper indirect route to lighting it.
  (if (active receiver1)
    0
    (if (rt-some-agent-holds-connector) 1 2)))

(define-query rt4-plate-cost ()
  ;; PLATE3 must end depressed.  That needs a PUT at minimum, plus a PICKUP unless some
  ;; agent already holds something that can weigh a plate down.
  (if (depressed plate3)
    0
    (if (rt-some-agent-holds-weight) 1 2)))

(define-query rt4-session-cost ()
  ;; Cycle 4 must be opened and closed: one START-RECORDER and one STOP-RECORDER.
  (do (assign $cycles (recorder-cycle-count))
      (if (< $cycles 4)
        2
        (if (recording-in-progress) 1 0))))

(define-query rt4-move-cost ()
  ;; The agent must finish at LOCATION3.
  (do (bind (has-location agent1 $agent-location))
      (if (eql $agent-location 'location3) 0 1)))


(define-query rt3-box-cost ()
  ;; BOX1 must end at LOCATION2.  Only a live agent can move the live box: a PUT at
  ;; minimum, plus a PICKUP unless it is already held.
  (if (has-location box1 location2)
    0
    (if (exists (?a agent) (holding ?a box1)) 1 2)))

(define-query rt3-session-cost ()
  (do (assign $cycles (recorder-cycle-count))
      (if (< $cycles 3)
        2
        (if (recording-in-progress) 1 0))))

(define-query rt3-move-cost ()
  ;; While BOX1 is not at LOCATION2 the live agent must stand where the box is and then
  ;; carry it to LOCATION2 -- one move if it is already there, two otherwise.
  (if (has-location box1 location2)
    0
    (do (bind (has-location agent1 $agent-location))
        (bind (has-location box1 $box-location))
        (if (eql $agent-location $box-location) 1 2))))

(define-query rt-cycle-min-steps-remaining? ()
  (do (assign $cycles (recorder-cycle-count))
      (if (or (= $cycles 3) (= $cycles 4))
        (+ (rt4-blue-cost) (rt4-plate-cost) (rt4-session-cost) (rt4-move-cost))
        (if (or (= $cycles 2) (= $cycles 3))
          (+ (rt3-box-cost) (rt3-session-cost) (rt3-move-cost))
          0))))

(define-query min-steps-remaining? ()
  (rt-cycle-min-steps-remaining?))
