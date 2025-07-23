;;; Filename: problem-boxes.lisp

;;; Problem specification for using boxes to move to an 
;;; area through a sequence of gates controlled by 
;;; pressure plates.


(in-package :ww)  ;required

(ww-set *problem-name* boxes)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 15)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)



(define-types
  myself    (me)
  box       (box1 box2)
  gate      (gate1 gate2 gate3)
  plate     (plate1 plate2 plate3)
  area      (area1 area2 area3 area4)
  object    (either myself box plate))


(define-dynamic-relations
  (holding myself box)
  (loc (either myself box plate) area)
  (on box plate))


(define-static-relations
  (controls plate gate)
  (separates gate area area))


(define-query free? (?me)
  (not (exists (?b box) 
         (holding ?me ?b))))
  

(define-query cleartop? (?plate)
  (not (exists (?b box)
         (on ?b ?plate))))


(define-query open? (?gate ?area1 ?area2)
  (and (separates ?gate ?area1 ?area2)
       (exists (?p plate)
         (and (controls ?p ?gate)
              (exists (?b box)
                (on ?b ?p))))))










(define-action move
    1
  ((?area1 ?area2) area)
  (and (loc me ?area1)
       (exists (?g gate)
         (open? ?g ?area1 ?area2)))
  (?area1 ?area2)
  (assert (not (loc me ?area1))
          (loc me ?area2)))


(define-action pickup
    1
  (?box box ?area area)
  (and (loc me ?area)
       (loc ?box ?area)
       (free? me))
  (?box ?area)
  (assert (not (loc ?box ?area))
          (holding me ?box)
          (exists (?p plate)
            (if (on ?box ?p)
              (not (on ?box ?p))))))


(define-action drop
    1
  (?box box ?area area)
  (and (loc me ?area)
       (holding me ?box))
  (?box ?area)
  (assert (loc ?box ?area)
          (not (holding me ?box))))


(define-action put
    1
  (?box box ?plate plate ?area area)
  (and (loc me ?area)
       (holding me ?box)
       (loc ?plate ?area)
       (cleartop? ?plate))
  (?box ?plate ?area)
  (assert (loc ?box ?area)
          (not (holding me ?box))
          (on ?box ?plate)))


(define-init
  ;dynamic
  (loc me area1)
  (loc box1 area1)    
  (loc box2 area2)
  ;static
  (loc plate1 area1)
  (loc plate2 area1)
  (loc plate3 area3)
  (controls plate1 gate1)
  (controls plate2 gate2)
  (controls plate3 gate3)
  (separates gate1 area1 area2)
  (separates gate2 area1 area3)
  (separates gate3 area3 area4))


(define-goal
  (loc me area4))
