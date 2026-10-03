;;; Filename: problem-donald.lisp


;;; Problem specification for solving the crypt-arithmetic
;;; problem:  DONALD
;;;          +GERALD
;;;         = ROBERT

;;; Variant of problem-donald.lisp (search-advisor run, 2026-10-03):
;;; - csp: the columns are assigned in a fixed order, one per depth, so the
;;;   6! orderings of the same assignment are no longer explored separately.
;;; - The columns are defined right to left (units column first), so each carry
;;;   is fixed by the column that produces it before the next column uses it.
;;; - Forward checking: assigning a letter removes its digit from every other
;;;   letter's remaining list, so each action draws only unused digits and the
;;;   all-different test against earlier assignments is no longer needed.
;;; - The goal tests the whole sum.  Forward checking can fix the last letter by
;;;   elimination, so "every letter has one digit" no longer means "every column
;;;   has been checked".


(in-package :ww)  ;required


(ww-set *problem-name* donald)

(ww-set *problem-type* csp)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(define-types
  letter (D O N A L G E R B T)
  carry (c0 c1 c2 c3 c4 c5 c6)
  variable (either letter carry))


(define-dynamic-relations
  (remaining variable $list))


(define-query get-remaining (?var)
  (do (bind (remaining ?var $digits))
      $digits))


(define-query word-value (?word)
  (ww-loop for $letter in ?word
           for $value = (first (get-remaining $letter))
             then (+ (* 10 $value) (first (get-remaining $letter)))
           finally (return $value)))


(define-update assign-letter (?letter ?digit)
  (doall (?l letter)
    (do (bind (remaining ?l $digits))
        (if (eql ?l ?letter)
          (remaining ?l (list ?digit))
          (remaining ?l (remove ?digit $digits))))))


(define-action assign-column-6
    1
  (product ?D (get-remaining D) ?T (get-remaining T) ?c5 (get-remaining c5) ?c6 (get-remaining c6))
  (and (/= ?D ?T)
       (= (+ ?c6 ?D ?D) (+ ?T (* 10 ?c5))))
  (?D ?D ?T)
  (assert (assign-letter D ?D)
          (assign-letter T ?T)
          (remaining c5 (list ?c5))))


(define-action assign-column-5
    1
  (product ?L (get-remaining L) ?R (get-remaining R) ?c4 (get-remaining c4) ?c5 (get-remaining c5))
  (and (/= ?L ?R)
       (= (+ ?c5 ?L ?L) (+ ?R (* 10 ?c4))))
  (?L ?L ?R)
  (assert (assign-letter L ?L)
          (assign-letter R ?R)
          (remaining c4 (list ?c4))))


(define-action assign-column-4
    1
  (product ?A (get-remaining A) ?E (get-remaining E) ?c3 (get-remaining c3) ?c4 (get-remaining c4))
  (and (/= ?A ?E)
       (= (+ ?c4 ?A ?A) (+ ?E (* 10 ?c3))))
  (?A ?A ?E)
  (assert (assign-letter A ?A)
          (assign-letter E ?E)
          (remaining c3 (list ?c3))))


(define-action assign-column-3
    1
  (product ?N (get-remaining N) ?R (get-remaining R) ?B (get-remaining B) ?c2 (get-remaining c2) ?c3 (get-remaining c3))
  (and (/= ?N ?R ?B)
       (= (+ ?c3 ?N ?R) (+ ?B (* 10 ?c2))))
  (?N ?R ?B)
  (assert (assign-letter N ?N)
          (assign-letter B ?B)
          (remaining c2 (list ?c2))))


(define-action assign-column-2
    1
  (product ?O (get-remaining O) ?E (get-remaining E) ?c1 (get-remaining c1) ?c2 (get-remaining c2))
  (and (/= ?O ?E)
       (= (+ ?c2 ?O ?E) (+ ?O (* 10 ?c1))))
  (?O ?E ?O)
  (assert (assign-letter O ?O)
          (remaining c1 (list ?c1))))


(define-action assign-column-1
    1
  (product ?D (get-remaining D) ?G (get-remaining G) ?R (get-remaining R) ?c0 (get-remaining c0) ?c1 (get-remaining c1))
  (and (/= ?D ?G ?R)
       (= (+ ?c1 ?D ?G) (+ ?R (* 10 ?c0))))
  (?D ?G ?R)
  (assert (assign-letter G ?G)))


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
       (= (+ (word-value '(D O N A L D)) (word-value '(G E R A L D)))
          (word-value '(R O B E R T)))))
