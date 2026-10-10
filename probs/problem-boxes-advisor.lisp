;;; Independent boxes specification, based on doc/problems/boxes/problem-analysis.txt.
;;; Revised 2026-10-10: named boxes with engine symmetry pruning replace the
;;; earlier ground counts and occupancy flags. Topology indexes and the all-open
;;; crossing bound are retained.

(in-package :ww)

;;; Objective: one globally shortest sequence of unit pickup, placement,
;;; and gate-crossing actions. Ordinary walking within an area is uncounted.
(ww-set *problem-name* boxes-advisor)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)

;;; Graph sharing compares complete arrangements and must retain shorter arrivals.
(ww-set *tree-or-graph* graph)

;;; Boxes are interchangeable: arrangements differing only by box names are merged.
(ww-set *symmetry-pruning* t)

;;; General rules have no instance-specific action limit.
(ww-set *depth-cutoff* 0)

(ww-set *threads* 16)

;;; Representation:
;;; - Each agent's and loose box's area; a box on a plate keeps its area.
;;; - Which box, if any, the agent holds; a held box has no area.
;;; - Which box, if any, is on each plate.
;;; Gate openness is queried from plate occupancy, with no stored gate memory.
;;; Agent plate visits and standing-on-plate state are omitted under reduction R1.
(define-types
  agent  (agent1)
  area   (area1 area2 area3 area4)
  box    (box1 box2)
  plate  (plate1 plate2 plate3)
  gate   (gate1 gate2 gate3)
  object (either agent box))

;;; Bijective ON looks up a box's plate or a plate's box directly.
(define-dynamic-relations
  (loc object $area)
  (holding agent $box)
  (on $box $plate :bijective))

;;; A gate joins both endpoint areas; movement will recognize either endpoint.
;;; Each plate has one location and controls exactly one gate, and vice versa.
;;; Area lists are derived once at staging and shared read-only during search.
(define-static-relations
  (gate-separates gate $area $area)
  (plate-area plate $area)
  (controls $plate $gate :bijective)
  (area-plates area $list)
  (area-gates area $list)
  (area-crossing-bound area $fixnum))

;;; Lookups assume the authored layout gives every agent, plate and gate its facts.
;;; That completeness is reviewed with the user, not checked at staging.
;;; With distinct endpoints, NIL means the gate is not incident to this area.
(define-query opposite-area (?gate gate ?area area)
  (do
    (bind (gate-separates ?gate $first-area $second-area))
    (cond ((eql ?area $first-area) $second-area)
          ((eql ?area $second-area) $first-area)
          (t nil))))

;;; Called with a loop variable; the typed parameter fixes the plate side of the
;;; bijective lookup at translation time.
(define-query cleartop? (?plate plate)
  (not (bind (on $box ?plate))))

(define-query gate-open? (?gate gate)
  (do (bind (controls $plate ?gate))
      (not (cleartop? $plate))))

;;; Each completion must cross at least this many gates even if all are open.
;;; Box handling adds actions. Goal-disconnected areas have the safe bound zero.
(define-query min-steps-remaining? ()
  (do (bind (loc agent1 $area))
      (bind (area-crossing-bound $area $steps))
      $steps))

;;; Pick up any local box, from the ground or from a plate. The box parameter
;;; lets symmetry pruning treat the given boxes as interchangeable.
(define-action pickup 1
  (?agent agent ?box box)
  (and (not (bind (holding ?agent $held)))
       (bind (loc ?agent $area))
       (loc ?box $area))
  (?agent ?box "in" $area)
  (assert (not (loc ?box $area))
          (holding ?agent ?box)
          (if (bind (on ?box $plate))
              (not (on ?box $plate)))))

;;; Ground placement and every empty local plate are independent alternatives.
;;; The precondition tests only what blocks every placement; the plate list is
;;; fetched in the effect, where each plate's own emptiness test also sits.
(define-action place 1
  (?agent agent)
  (and (bind (holding ?agent $box))
       (bind (loc ?agent $area)))
  (?agent $box "on" $site "in" $area)
  (do
    (bind (area-plates $area $plates))
    (assert (setq $site 'ground)
            (loc $box $area)
            (not (holding ?agent $box)))
    (doall (?plate $plates)
      (if (cleartop? ?plate)
        (assert (setq $site ?plate)
                (loc $box $area)
                (on $box ?plate)
                (not (holding ?agent $box)))))))

;;; Cross an open incident gate in either direction, carrying any held box.
;;; Each gate's open test belongs to that gate's crossing, so it sits in the loop.
(define-action cross-gate 1
  (?agent agent)
  (bind (loc ?agent $area))
  (?agent "through" ?gate "from" $area "to" $to)
  (do
    (bind (area-gates $area $gates))
    (doall (?gate $gates)
      (if (gate-open? ?gate)
        (assert (setq $to (opposite-area ?gate $area))
                (loc ?agent $to))))))

(define-init
  (loc agent1 area1)
  (loc box1 area1)
  (loc box2 area2)
  (gate-separates gate1 area1 area2)
  (gate-separates gate2 area1 area3)
  (gate-separates gate3 area3 area4)
  (plate-area plate1 area1)
  (plate-area plate2 area1)
  (plate-area plate3 area3)
  (controls plate1 gate1)
  (controls plate2 gate2)
  (controls plate3 gate3))

;;; Derives each area's plates and gates once, for efficiency: actions then
;;; consider only the local ones rather than every plate and gate.
(define-init-action initialize-topology-indexes 0
  ()
  (always-true)
  ()
  (assert
    (doall (?area area)
      (do (area-plates ?area nil)
          (area-gates ?area nil)))
    (doall (?plate plate)
      (do
        (bind (plate-area ?plate $area))
        (bind (area-plates $area $plates))
        (area-plates $area (cons ?plate $plates))))
    (doall (?gate gate)
      (do
        (bind (gate-separates ?gate $first-area $second-area))
        (bind (area-gates $first-area $gates))
        (area-gates $first-area (cons ?gate $gates))
        (bind (area-gates $second-area $gates))
        (area-gates $second-area (cons ?gate $gates))))
    (doall (?area area)
      (do
        (bind (area-plates ?area $plates))
        (bind (area-gates ?area $gates))
        (area-plates ?area (reverse $plates))
        (area-gates ?area (reverse $gates))))))

;;; Breadth-first layers give exact crossing distances in the all-open graph.
;;; Mark each area when discovered, so cycles and parallel gates add no visits.
;;; Missing distances denote disconnection, not an uninitialized numeric bound.
(defun boxes-all-open-distances (goal-area neighbors)
  (let ((distances (make-hash-table :test #'eq))
        (frontier (list goal-area))
        (distance 0))
    (setf (gethash goal-area distances) 0)
    (loop while frontier
          do (let ((next nil))
               (dolist (area frontier)
                 (dolist (neighbor (gethash area neighbors))
                   (unless (nth-value 1 (gethash neighbor distances))
                     (setf (gethash neighbor distances) (1+ distance))
                     (push neighbor next))))
               (setf frontier next)
               (incf distance)))
    distances))

;;; Derive static bounds from the gate endpoints, using a transient neighbor map.
;;; Read symbolic static facts here; the compiled search query runs after conversion.
;;; AREA4 is the same goal datum as in DEFINE-GOAL below, not extra state data.
(define-init-action initialize-crossing-bounds 0
  ()
  (always-true)
  ()
  (assert
    (setq $neighbors (make-hash-table :test #'eq))
    (doall (?area area)
      (setf (gethash ?area $neighbors) nil))
    (doall (?gate gate)
      (do
        (bind (gate-separates ?gate $first-area $second-area))
        (push $second-area (gethash $first-area $neighbors))
        (push $first-area (gethash $second-area $neighbors))))
    (setq $distances (boxes-all-open-distances 'area4 $neighbors))
    (doall (?area area)
      (area-crossing-bound ?area (gethash ?area $distances 0)))))

;;; No final holding or box-placement restriction beyond reaching the goal area.
(define-goal
  (loc agent1 area4))
