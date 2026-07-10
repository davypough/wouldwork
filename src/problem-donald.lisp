;;; Filename: problem-donald.lisp


;;; Problem specification for solving the crypt-arithmetic
;;; problem:  DONALD
;;;          +GERALD
;;;         = ROBERT

;;; This approach implements the method of forward-checking for
;;; constraint satisfaction problems (ref Russell & Norvig)


(in-package :ww)  ;required


(ww-set *problem-name* donald)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* tree)


(define-types
  letter (D O N A L G E R B T)
  carry (c0 c1 c2 c3 c4 c5 c6)
  variable (either letter carry))


(define-dynamic-relations
  (remaining variable $list))


(define-query get-remaining (?var)
  (and (bind (remaining ?var $digits))
       $digits))


(define-query consistent (?letter ?digit)
  (or (remaining ?letter (list ?digit))  ;already assigned
      (not (exists (?l letter)  ;no other letter has that assignment
             (and (not (eql ?l ?letter))
                  (bind (remaining ?l $digits))
                  (alexandria:length= 1 $digits)
                  (= (first $digits) ?digit))))))


(define-action assign-column-1
    1
  (product ?D (get-remaining D) ?G (get-remaining G) ?R (get-remaining R) ?c0 (get-remaining c0) ?c1 (get-remaining c1))
  (and (/= ?D ?G ?R)
       (= (+ ?c1 ?D ?G) (+ ?R (* 10 ?c0)))
       (consistent D ?D)
       (consistent G ?G)
       (consistent R ?R))
  (?D ?G ?R)
  (assert (remaining c1 (list ?c1))
          (remaining D (list ?D))
          (remaining G (list ?G))
          (remaining R (list ?R))
          (remaining c0 (list ?c0))))

          
(define-action assign-column-2
    1
  (product ?O (get-remaining O) ?E (get-remaining E) ?c1 (get-remaining c1) ?c2 (get-remaining c2))
  (and (/= ?O ?E)
       (= (+ ?c2 ?O ?E) (+ ?O (* 10 ?c1)))
       (consistent O ?O)
       (consistent E ?E))
  (?O ?E ?O)
  (assert (remaining c2 (list ?c2))
          (remaining O (list ?O))
          (remaining E (list ?E))
          (remaining c1 (list ?c1))))


(define-action assign-column-3
    1
  (product ?N (get-remaining N) ?R (get-remaining R) ?B (get-remaining B) ?c2 (get-remaining c2) ?c3 (get-remaining c3))
  (and (/= ?N ?R ?B)
       (= (+ ?c3 ?N ?R) (+ ?B (* 10 ?c2)))
       (consistent N ?N)
       (consistent R ?R)
       (consistent B ?B))
  (?N ?R ?B)
  (assert (remaining c3 (list ?c3))
          (remaining N (list ?N))
          (remaining R (list ?R))
          (remaining B (list ?B))
          (remaining c2 (list ?c2))))


(define-action assign-column-4
    1
  (product ?A (get-remaining A) ?E (get-remaining E) ?c3 (get-remaining c3) ?c4 (get-remaining c4))
  (and (/= ?A ?E)
       (= (+ ?c4 ?A ?A) (+ ?E (* 10 ?c3)))
       (consistent A ?A)
       (consistent E ?E))
  (?A ?A ?E)
  (assert (remaining c4 (list ?c4))
          (remaining A (list ?A))
          (remaining E (list ?E))
          (remaining c3 (list ?c3))))


(define-action assign-column-5
    1
  (product ?L (get-remaining L) ?R (get-remaining R) ?c4 (get-remaining c4) ?c5 (get-remaining c5))
  (and (/= ?L ?R)
       (= (+ ?c5 ?L ?L) (+ ?R (* 10 ?c4)))
       (consistent L ?L)
       (consistent R ?R))
  (?L ?L ?R)
  (assert (remaining c5 (list ?c5))
          (remaining L (list ?L))
          (remaining R (list ?R))
          (remaining c4 (list ?c4))))


(define-action assign-column-6
    1
  (product ?D (get-remaining D) ?T (get-remaining T) ?c5 (get-remaining c5) ?c6 (get-remaining c6))
  (and (/= ?D ?T)
       (= (+ ?c6 ?D ?D) (+ ?T (* 10 ?c5)))
       (consistent D ?D)
       (consistent T ?T))
  (?D ?D ?T)
  (assert (remaining c6 (list ?c6))
          (remaining D (list ?D))
          (remaining T (list ?T))
          (remaining c5 (list ?c5))))


(define-init
  (remaining D (0 1 2 3 4 5 6 7 8 9))
  (remaining O (0 1 2 3 4 5 6 7 8 9))
  (remaining N (0 1 2 3 4 5 6 7 8 9))
  (remaining A (0 1 2 3 4 5 6 7 8 9))
  (remaining L (0 1 2 3 4 5 6 7 8 9))
  (remaining G (0 1 2 3 4 5 6 7 8 9))
  (remaining E (0 1 2 3 4 5 6 7 8 9))
  (remaining R (0 1 2 3 4 5 6 7 8 9))
  (remaining B (0 1 2 3 4 5 6 7 8 9))
  (remaining T (0 1 2 3 4 5 6 7 8 9))
  (remaining c0 (0))
  (remaining c1 (0 1))
  (remaining c2 (0 1))
  (remaining c3 (0 1))
  (remaining c4 (0 1))
  (remaining c5 (0 1))
  (remaining c6 (0)))


(define-goal
  (and (forall (?l letter)
         (and (bind (remaining ?l $digits))
              (alexandria:length= 1 $digits)))
       (forall (?c carry)
         (and (bind (remaining ?c $digits))
              (alexandria:length= 1 $digits)))))
