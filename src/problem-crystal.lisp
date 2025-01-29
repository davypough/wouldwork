;;;; Filename: problem-crystal.lisp

;;; Problem specification for crystal problem from Escape Simulator "The Lab".


(in-package :ww)  ;required


(ww-set *problem-name* crystal)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)  ; Use graph search to avoid repeated states


(define-types
  cell  (compute (loop for i from 1 to 10
                   collect (intern (format nil "CELL~D" i))))  ; Cells in the hexagonal grid
  piece (compute (loop for i from 1 to 12
                   collect (intern (format nil "PIECE~D" i))))  ; List of available pieces
  side (1 2 3 4 5 6)  ; Sides of a hexagon, indexed clockwise
)


(define-dynamic-relations
  ;; Whether a piece is currently on a cell
  (on piece cell)
  ;; Whether a side of a cell is currently covered, by having a piece (with a matching active side) placed on it 
  (covered cell side)
)


(define-static-relations
  (active-piece-side piece side) ; Whether a side of a piece is active (given)
  (active-cell-side cell side)  ; Whether a side of a cell is active (defines a given target graph)
)


(define-action put
  1
  (?piece piece ?cell cell)
  (and (forall (?c cell)  ; Piece is not already on some cell              ;(not (bind (on ?piece $cell)))  
         (not (on ?piece ?c)))
       (forall (?p piece)  ; Cell is not already occupied by some piece
         (not (on ?p ?cell)))
       (forall (?s side)  ; All active piece sides are active cell sides too
         (or (not (active-piece-side ?piece ?s))
             (active-cell-side ?cell ?s))))
  (?piece ?cell)  ; Prints as (put ?piece ?cell)
  (assert (on ?piece ?cell)  ; ?piece is now on ?cell
          (doall (?s side)  ; All the active sides of ?cell matching the ?piece on it, are now covered
            (if (and (active-cell-side ?cell ?s)
                     (active-piece-side ?piece ?s))
              (covered ?cell ?s))))
)


(define-init
  ;; Defines the active sides of each cell. The active cell sides define a target graph on the grid.
  (active-cell-side cell1 3)
  (active-cell-side cell2 3) (active-cell-side cell2 4)
  (active-cell-side cell3 2) (active-cell-side cell3 3) (active-cell-side cell3 4)
  (active-cell-side cell4 3) (active-cell-side cell4 4) (active-cell-side cell4 5)
  (active-cell-side cell5 1) (active-cell-side cell5 2) (active-cell-side cell5 3) (active-cell-side cell5 6)
  (active-cell-side cell6 1) (active-cell-side cell6 2) (active-cell-side cell6 3) (active-cell-side cell6 4) (active-cell-side cell6 5)
    (active-cell-side cell6 6)
  (active-cell-side cell7 1) (active-cell-side cell7 5) (active-cell-side cell7 6)
  (active-cell-side cell8 6)
  (active-cell-side cell9 1) (active-cell-side cell9 6)
  (active-cell-side cell10 6)

  ;; Defines the active sides of each piece
  (active-piece-side piece1 1) (active-piece-side piece1 6)
  (active-piece-side piece2 1) (active-piece-side piece2 6)
  (active-piece-side piece3 4) (active-piece-side piece3 5) (active-piece-side piece3 6)
  (active-piece-side piece4 3)
  (active-piece-side piece5 3)
  (active-piece-side piece6 4) (active-piece-side piece6 5)
  (active-piece-side piece7 3) (active-piece-side piece7 4)
  (active-piece-side piece8 3) (active-piece-side piece8 4)
  (active-piece-side piece9 3) (active-piece-side piece9 4)
  (active-piece-side piece10 2)
  (active-piece-side piece11 2)
  (active-piece-side piece12 1) (active-piece-side piece12 2) (active-piece-side piece12 3)
)


(define-goal
  ;; All active cell sides (forming a connected graph) must be covered exactly once
  (and (not (equivalent (covered cell5 1) (covered cell2 4)))  ;t,nil or nil,t (ie, exclusive or)
       (not (equivalent (covered cell5 2) (covered cell6 5)))
       (not (equivalent (covered cell5 3) (covered cell9 6)))
       (not (equivalent (covered cell5 6) (covered cell1 3)))

       (not (equivalent (covered cell6 1) (covered cell3 4)))
       (not (equivalent (covered cell6 2) (covered cell7 5)))
       (not (equivalent (covered cell6 3) (covered cell10 6)))
       (not (equivalent (covered cell6 4) (covered cell9 1)))
       (not (equivalent (covered cell6 6) (covered cell2 3)))

       (not (equivalent (covered cell3 3) (covered cell7 6)))

       (not (equivalent (covered cell4 3) (covered cell8 6)))
       (not (equivalent (covered cell4 4) (covered cell7 4)))
       (not (equivalent (covered cell4 5) (covered cell3 2))))
)
