;;; Filename: problem-boxes-advisor.lisp

;;; Boxes problem, built with the spec advisor (see doc/problems/boxes/).
;;; The agent carries boxes onto pressure plates to open gates and reach area4.
;;;
;;; Design notes:
;;; - Boxes keep their names; symmetry pruning treats them as interchangeable.
;;; - Gate openness is not stored; it is read from the gate's plate.
;;; - The agent never stands on a plate itself, since that can't help it cross.
;;; - Each area's plates and gates are listed once at the start, so actions
;;;   look only at local ones.
;;; - The remaining-steps estimate is a simple placeholder.


(in-package :ww)


;;; Find a shortest sequence of pickups, placements and gate crossings.
(ww-set *problem-name* boxes-advisor)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *symmetry-pruning* t)
(ww-set *depth-cutoff* 0)
(ww-set *threads* 16)


(define-types
  agent  (agent1)
  area   (area1 area2 area3 area4)
  box    (box1 box2)
  plate  (plate1 plate2 plate3)
  gate   (gate1 gate2 gate3)
  object (either agent box))


;;; A box on a plate keeps its area; a held box has none.
(define-dynamic-relations
  (loc object $area)
  (holding agent $box)
  (on $box $plate :bijective))


;;; The area lists are filled in by initialize-topology-indexes.
(define-static-relations
  (gate-separates gate $area $area)
  (plate-area plate $area)
  (controls $plate $gate :bijective)
  (area-plates area $list)
  (area-gates area $list))


;;; The area on the other side of a gate, or nil if the gate doesn't touch ?area.
(define-query opposite-area (?gate gate ?area area)
  (do
    (bind (gate-separates ?gate $first-area $second-area))
    (cond ((eql ?area $first-area) $second-area)
          ((eql ?area $second-area) $first-area)
          (t nil))))


(define-query cleartop? (?plate plate)
  (not (bind (on $box ?plate))))


;;; A gate is open while a box is on its plate.
(define-query gate-open? (?gate gate)
  (do (bind (controls $plate ?gate))
      (not (cleartop? $plate))))


;;; Placeholder estimate of steps still needed, never too high: crossings
;;; (none in area4, one from a neighboring area, otherwise two), plus a
;;; placement, and a pickup if empty-handed, when every gate into area4 is closed.
(define-query min-steps-remaining? ()
  (if (loc agent1 area4)
    0
    (do (bind (loc agent1 $area))
        (bind (area-gates area4 $goal-gates))
        (+ (if (exists (?gate $goal-gates)
                 (eql (opposite-area ?gate area4) $area))
             1
             2)
           (cond ((exists (?gate $goal-gates) (gate-open? ?gate)) 0)
                 ((bind (holding agent1 $box)) 1)
                 (t 2))))))


;;; Pick up a box in the agent's area, from the ground or from a plate.
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


;;; Put the held box on the ground, or on any empty plate in the area.
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


;;; Cross any open gate in the area, carrying the held box if any.
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


(define-goal
  (loc agent1 area4))
