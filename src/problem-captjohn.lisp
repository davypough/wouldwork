;;; Filename: problem-captjohn-csp.lisp

;;; Brain Teaser logic problem, 
;;; Capt John's Journey (part 1)


(in-package :ww)

(ww-set *problem-name* captjohn)

(ww-set *problem-type* csp)


(define-types
    captain (john)
    ship    (wasp)
    crew    (crew1 crew2)
    guard   (guard1 guard2)
    grass   (grass1 grass2 grass3)
    object  (either captain ship crew guard grass))


(define-dynamic-relations
  (remaining object $list))  ;list of coords--eg, ((0 2) ...)


(define-query get-remaining (?obj)
  (do (bind (remaining ?obj $coords))
      $coords))


(define-query in-same-row (?coord1 ?coord2)
  (= (first ?coord1) (first ?coord2)))


(define-query in-same-col (?coord1 ?coord2)
  (= (second ?coord1) (second ?coord2)))
  

(define-query in-row (?coord ?row)
  (= (first ?coord) ?row))


(define-query in-col (?coord ?col)
  (= (second ?coord) ?col))


(define-query vert-next-to (?coord1 ?coord2)
  (and (= (second ?coord1) (second ?coord2))
       (or (= (first ?coord1) (1+ (first ?coord2)))
           (= (first ?coord1) (1- (first ?coord2))))))

 
(define-query diag-next-to (?coord1 ?coord2)
  (or (and (= (1+ (first ?coord1)) (first ?coord2))
           (= (1- (second ?coord1)) (second ?coord2)))
      (and (= (1+ (first ?coord1)) (first ?coord2))
           (= (1+ (second ?coord1)) (second ?coord2)))
      (and (= (1- (first ?coord1)) (first ?coord2))
           (= (1- (second ?coord1)) (second ?coord2)))
      (and (= (1- (first ?coord1)) (first ?coord2))
           (= (1+ (second ?coord1)) (second ?coord2)))))


(define-update make-assignment (?assigned-obj ?assigned-coord)
  (doall (?obj object)
    (do (bind (remaining ?obj $initial-coords))
        (if (eql ?obj ?assigned-obj)
          (remaining ?obj (list ?assigned-coord))
          (remaining ?obj (remove ?assigned-coord $initial-coords :test #'equal))))))


; Make one rule for each object assignment with constraints
; Later actions can depend on prior assignments


(define-action assign-grass2
  ; One of the grass spaces is in the 2nd column, in the first row.
  1
  (?grass2-coord (get-remaining grass2))
  (equal ?grass2-coord '(0 1))
  (?grass2-coord)
  (assert (make-assignment grass2 ?grass2-coord)))


(define-action assign-guard1
  ; Neither guard is in the third column.
  1
  (?guard1-coord (get-remaining guard1))
  (not (in-col ?guard1-coord 2))
  (?guard1-coord)
  (assert (make-assignment guard1 ?guard1-coord)))


(define-action assign-guard2
  ; Neither guard is in the third column.
  ; The two guards are not in the same row or column.
  1
  (?guard2-coord (get-remaining guard2) ?guard1-coord (get-remaining guard1))  ;?guard1-coord previously assigned
  (and (not (in-col ?guard2-coord 2))
       (not (in-same-row ?guard2-coord ?guard1-coord))
       (not (in-same-col ?guard2-coord ?guard1-coord)))
  (?guard2-coord)
  (assert (make-assignment guard2 ?guard2-coord)))


(define-action assign-john
  ; John is not in the same row or column as either guard.
  1
  (?john-coord (get-remaining john) ?guard1-coord (get-remaining guard1)
     ?guard2-coord (get-remaining guard2))
  (and (not (in-same-row ?john-coord ?guard1-coord))
       (not (in-same-col ?john-coord ?guard1-coord))
       (not (in-same-row ?john-coord ?guard2-coord))
       (not (in-same-col ?john-coord ?guard2-coord)))
  (?john-coord)
  (assert (make-assignment john ?john-coord)))


(define-action assign-wasp
  ; The Wasp is not in the same row or column as John.
  ; The ship is in the same row as one guard, and the same column as the other guard.
  1
  (?wasp-coord (get-remaining wasp) ?john-coord (get-remaining john)
     ?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2))
  (and (not (in-same-row ?wasp-coord ?john-coord))
       (not (in-same-col ?wasp-coord ?john-coord))
       (or (and (in-same-row ?wasp-coord ?guard1-coord)
                (in-same-col ?wasp-coord ?guard2-coord))
           (and (in-same-row ?wasp-coord ?guard2-coord)
                (in-same-col ?wasp-coord ?guard1-coord))))
  (?wasp-coord)
  (assert (make-assignment wasp ?wasp-coord)))


(define-action assign-grass1
  ; Both guards are vertically next to grass.
  1
  (?grass1-coord (get-remaining grass1) ?guard1-coord (get-remaining guard1)
     ?guard2-coord (get-remaining guard2) ?grass2-coord (get-remaining grass2))
  (and (or (vert-next-to ?guard1-coord ?grass1-coord)
           (vert-next-to ?guard1-coord ?grass2-coord)
           (ww-loop for $possible-grass3-coord in (get-remaining grass3)
                    thereis (vert-next-to ?guard1-coord $possible-grass3-coord)))
       (or (vert-next-to ?guard2-coord ?grass1-coord)
           (vert-next-to ?guard2-coord ?grass2-coord)
           (ww-loop for $possible-grass3-coord in (get-remaining grass3)
                    thereis (vert-next-to ?guard2-coord $possible-grass3-coord))))
  (?grass1-coord)
  (assert (make-assignment grass1 ?grass1-coord)))


(define-action assign-grass3
  ; Both guards are vertically next to grass.
  1
  (?grass3-coord (get-remaining grass3) ?grass1-coord (get-remaining grass1)
     ?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2)
     ?grass2-coord (get-remaining grass2))
  (and (or (vert-next-to ?guard1-coord ?grass1-coord)
           (vert-next-to ?guard1-coord ?grass2-coord)
           (vert-next-to ?guard1-coord ?grass3-coord))
       (or (vert-next-to ?guard2-coord ?grass1-coord)
           (vert-next-to ?guard2-coord ?grass2-coord)
           (vert-next-to ?guard2-coord ?grass3-coord)))
  (?grass3-coord)
  (assert (make-assignment grass3 ?grass3-coord)))


(define-action assign-crew1
  ; One of the grass spaces is diagonally next to both crew mates.
  1
  (?crew1-coord (get-remaining crew1) ?grass1-coord (get-remaining grass1)
     ?grass2-coord (get-remaining grass2) ?grass3-coord (get-remaining grass3))
  (and (or (ww-loop for $possible-crew2-coord in (get-remaining crew2)
                    thereis (and (diag-next-to ?grass1-coord ?crew1-coord)
                                 (diag-next-to ?grass1-coord $possible-crew2-coord)))
           (ww-loop for $possible-crew2-coord in (get-remaining crew2)
                    thereis (and (diag-next-to ?grass2-coord ?crew1-coord)
                                 (diag-next-to ?grass2-coord $possible-crew2-coord)))
           (ww-loop for $possible-crew2-coord in (get-remaining crew2)
                    thereis (and (diag-next-to ?grass3-coord ?crew1-coord)
                                 (diag-next-to ?grass3-coord $possible-crew2-coord)))))
  (?crew1-coord)
  (assert (make-assignment crew1 ?crew1-coord)))


(define-action assign-crew2
  ; One of the grass spaces is diagonally next to both crew mates.
  1
  (?crew2-coord (get-remaining crew2) ?crew1-coord (get-remaining crew1)
     ?grass1-coord (get-remaining grass1) ?grass2-coord (get-remaining grass2)
     ?grass3-coord (get-remaining grass3))
  (or (and (diag-next-to ?grass1-coord ?crew1-coord)
           (diag-next-to ?grass1-coord ?crew2-coord))
      (and (diag-next-to ?grass2-coord ?crew1-coord)
           (diag-next-to ?grass2-coord ?crew2-coord))
      (and (diag-next-to ?grass3-coord ?crew1-coord)
           (diag-next-to ?grass3-coord ?crew2-coord)))
  (?crew2-coord)
  (assert (make-assignment crew2 ?crew2-coord)))


(define-init
  (remaining john   ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining wasp   ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining crew1  ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining crew2  ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining guard1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining guard2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass3 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))


(define-goal  ;when the last variable has been assigned
  (eq (problem-state.name state) (action.name (car (last *actions*)))))
