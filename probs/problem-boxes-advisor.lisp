;;; Independent boxes specification, based on doc/problems/boxes/problem-analysis.txt.
;;; Topology indexes passed focused user checks; ten-action minimum retained.
;;; All-open crossing bound passed user checks and was exercised by the solve.
;;; Staging, fidelity checks, replay, and search require their agreed check scope.

(in-package :ww)

;;; Objective: one globally shortest sequence of unit pickup, placement,
;;; and gate-crossing actions. Ordinary walking within an area is uncounted.
(ww-set *problem-name* boxes-advisor)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)

;;; Interchangeable boxes are represented directly by counts and occupancy.
;;; Graph sharing compares complete arrangements and must retain shorter arrivals.
(ww-set *tree-or-graph* graph)
(ww-set *symmetry-pruning* nil)

;;; General rules have no instance-specific action limit. A cutoff of ten may
;;; be selected at the REPL for a separately approved supplied-instance check.
(ww-set *depth-cutoff* 0)

;;; Provisional local execution baseline; no performance claim is implied.
;;; Explicitly select depth-first at the REPL when staging/checking is approved.
(ww-set *threads* 16)
(ww-set *randomize-search* nil)

;;; Representation:
;;; - Agent's area.
;;; - Number of loose boxes on the ground in each area, including zero.
;;; - Which plates contain a box.
;;; - Whether the agent carries a box.
;;; Gate openness is queried from plate occupancy, with no stored gate memory.
;;; Conserve loose boxes + occupied plates + carried box = total given boxes.
;;; Retain the given box objects and initial data, but omit individual identities
;;; from the changing state. Instructions identify pickup sources and placement
;;; destinations, e.g. a ground area or a named plate, and refer to the carried box.
;;; One ground pickup choice suffices regardless of the local pile's size.
;;; Agent plate visits and standing-on-plate state are omitted under reduction R1.

;;; Given objects. Box identities preserve the supplied initial data, but do not
;;; parameterize search actions or appear in the changing arrangement.
(define-types
  agent (agent1)
  area  (area1 area2 area3 area4)
  box   (box1 box2)
  plate (plate1 plate2 plate3)
  gate  (gate1 gate2 gate3))

;;; Every area has an explicit nonnegative loose-box count, including zero.
;;; Absence of occupied/carrying means empty; their presence means one box.
;;; Action preconditions will reject occupied-plate placement and full-hands pickup.
(define-dynamic-relations
  (agent-area agent $area)
  (loose-boxes area $fixnum)
  (occupied plate)
  (carrying agent))

;;; Initialization derives ground counts from the given initial box areas.
;;; Do not author loose-boxes facts in define-init; actions later change them.
(define-derived-relations
  loose-boxes)

;;; A gate joins both endpoint areas; movement will recognize either endpoint.
;;; Each plate has one location and controls exactly one gate, and vice versa.
;;; Bijective CONTROLS stores both lookup directions without changing input facts.
;;; Initial box areas are source data rather than search-time box placements.
;;; Area lists are derived once at staging and shared read-only during search.
(define-static-relations
  (gate-separates gate $area $area)
  (plate-area plate $area)
  (controls $plate $gate :bijective)
  (initial-box-area box $area)
  (area-plates area $list)
  (area-gates area $list)
  (area-crossing-bound area $fixnum))

;;; Lookups assume the authored layout gives every agent, plate and gate its facts.
;;; That completeness is reviewed with the user, not checked at staging.
(define-query current-area (?agent agent)
  (do (bind (agent-area ?agent $area))
      $area))

(define-query ground-count (?area area)
  (do (bind (loose-boxes ?area $count))
      $count))

(define-query plate-location (?plate plate)
  (do (bind (plate-area ?plate $area))
      $area))

(define-query local-plates (?area area)
  (do (bind (area-plates ?area $plates))
      $plates))

;;; With distinct endpoints, NIL means the gate is not incident to this area.
(define-query opposite-area (?gate gate ?area area)
  (do
    (bind (gate-separates ?gate $first-area $second-area))
    (cond ((eql ?area $first-area) $second-area)
          ((eql ?area $second-area) $first-area)
          (t nil))))

(define-query incident-gates (?area area)
  (do (bind (area-gates ?area $gates))
      $gates))

(define-query controlling-plate (?gate gate)
  (do (bind (controls $plate ?gate))
      $plate))

(define-query gate-open? (?gate gate)
  (do (setq $plate (controlling-plate ?gate))
      (occupied $plate)))

;;; Each completion must cross at least this many gates even if all are open.
;;; Box handling adds actions. Goal-disconnected areas have the safe bound zero.
(define-query min-steps-remaining? ()
  (do (bind (agent-area agent1 $area))
      (bind (area-crossing-bound $area $steps))
      $steps))

(define-query given-box-count ()
  (length (gethash 'box *types*)))

;;; For initialization and focused checks, not an extra full-state scan on every
;;; successor. The action transfers below preserve this total algebraically.
(define-query box-arrangement-valid? ()
  (do
    (setq $total 0)
    (setq $expected (given-box-count))
    (doall (?agent agent)
      (do
        (setq $area (current-area ?agent))
        (unless (member $area (gethash 'area *types*) :test #'eq)
          (error "AGENT-AREA for agent ~S names undeclared area ~S." ?agent $area))
        (if (carrying ?agent)
            (incf $total))))
    (doall (?area area)
      (do
        (setq $count (ground-count ?area))
        (unless (typep $count '(integer 0 *))
          (error "LOOSE-BOXES for area ~S must be nonnegative; found ~S."
                 ?area $count))
        (incf $total $count)))
    (doall (?plate plate)
      (if (occupied ?plate)
          (incf $total)))
    (unless (= $total $expected)
      (error "Box conservation failed for type BOX: expected ~D, found ~D."
             $expected $total))
    t))

;;; Common tests and lookups run once per agent. Each ASSERT below produces one
;;; alternative pickup from the same parent; no alternative consumes another's box.
(define-action pickup 1
  (?agent agent)
  (and (not (carrying ?agent))
       (assign $area (current-area ?agent))
       (assign $count (ground-count $area))
       (assign $plates (local-plates $area)))
  (?agent "from" $site "in" $area)
  (do
    (if (plusp $count)
        (assert (setq $site 'ground)
                (loose-boxes $area (1- $count))
                (carrying ?agent)))
    (ww-loop for $plate in $plates
             when (occupied $plate)
               do (assert (setq $site $plate)
                          (not (occupied $plate))
                          (carrying ?agent)))))

;;; Ground placement and every empty local plate are independent alternatives.
(define-action place 1
  (?agent agent)
  (and (carrying ?agent)
       (assign $area (current-area ?agent))
       (assign $count (ground-count $area))
       (assign $plates (local-plates $area)))
  (?agent "on" $site "in" $area)
  (do
    (assert (setq $site 'ground)
            (loose-boxes $area (1+ $count))
            (not (carrying ?agent)))
    (ww-loop for $plate in $plates
             unless (occupied $plate)
               do (assert (setq $site $plate)
                          (occupied $plate)
                          (not (carrying ?agent))))))

;;; Cross an open incident gate in either direction. Ground counts, occupied
;;; plates, and carrying status are unchanged, including the supporting box.
(define-action cross-gate 1
  (?agent agent)
  (and (assign $area (current-area ?agent))
       (assign $gates (incident-gates $area)))
  (?agent "through" $via "from" $area "to" $to)
  (ww-loop for $gate in $gates
           when (gate-open? $gate)
             do (assert (setq $via $gate)
                        (setq $to (opposite-area $gate $area))
                        (agent-area ?agent $to))))

(define-init
  (agent-area agent1 area1)
  (initial-box-area box1 area1)
  (initial-box-area box2 area2)
  (gate-separates gate1 area1 area2)
  (gate-separates gate2 area1 area3)
  (gate-separates gate3 area3 area4)
  (plate-area plate1 area1)
  (plate-area plate2 area1)
  (plate-area plate3 area3)
  (controls plate1 gate1)
  (controls plate2 gate2)
  (controls plate3 gate3))

;;; Build adjacency in linear passes. These BINDs use symbolic static keys,
;;; matching the static writes in the plain-compiled initialization action.
;;; Explicit empty lists cover isolated areas. Reverse into declared object order.
;;; The engine converts these facts to static integer keys after init actions.
;;; No query reads the newly built lists before that conversion.
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

;;; Derive a single initial arrangement, not alternative successors. Every area
;;; receives a count, including zero, and each given box is counted once.
;;; Conservation is checked after derivation, once during initialization. This also
;;; guards the ground-start input format: an authored OCCUPIED or CARRYING fact would
;;; count a box twice and fail the check.
;;; Init-action BIND reads symbolic keys; use the compiled ground-count query
;;; for the integer-keyed dynamic count written by this same ASSERT.
(define-init-action initialize-box-counts 0
  ()
  (always-true)
  ()
  (assert
    (doall (?area area)
      (loose-boxes ?area 0))
    (doall (?box box)
      (do
        (bind (initial-box-area ?box $initial-area))
        (loose-boxes $initial-area (1+ (ground-count $initial-area)))))
    (box-arrangement-valid?)))

;;; No final carrying or box-placement restriction beyond reaching the goal area.
(define-goal
  (agent-area agent1 area4))
