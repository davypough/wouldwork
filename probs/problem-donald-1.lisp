;;; Filename: problem-donald-1.lisp


;;; Problem specification for solving the crypt-arithmetic
;;; problem:  DONALD
;;;          +GERALD
;;;         = ROBERT

;;; Design notes (extends problem-donald.lisp with arc consistency):
;;; - Look ahead after every column.  Once a column is assigned, every remaining column sum
;;;   keeps only the digits and carries that still have some way to add up, and a letter
;;;   left with a single digit takes that digit from all the others.  This repeats until
;;;   nothing changes, so one narrowing can trigger others across the whole sum.
;;; - A dead branch dies at once.  If some letter or carry is left with no possible value,
;;;   the column choice is dropped before it becomes a state, rather than being discovered
;;;   columns later.
;;; - Sound by construction.  Only values that no completion could use are removed, so no
;;;   solution is lost.  The narrowing is done by the general engine library
;;;   (src/ww-arc-consistency.lisp); the spec only states the column sums and the rule that
;;;   letters differ.
;;; - Trade-off.  Each step does more checking, in exchange for far fewer states.
;;; - Everything else is as in problem-donald.lisp: columns right to left, one per step, and
;;;   assigning a letter removes its digit from the other letters.


(in-package :ww)  ;required


(ww-set *problem-name* donald-1)

(ww-set *problem-type* csp)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(define-types
  letter (D O N A L G E R B T)
  carry (c0 c1 c2 c3 c4 c5 c6)
  variable (either letter carry))


(define-dynamic-relations
  (remaining variable $list))


;------------------ arc consistency network ---------------------


(defparameter *ac-variables* '(D O N A L G E R B T c0 c1 c2 c3 c4 c5 c6)
  "The variables in network order: a variable's number is its position here.")


(defparameter *ac-letter-numbers* (loop for n below 10 collect n)
  "The numbers of the letters among *ac-variables*.")


(defun ac-numbers (&rest variables)
  (mapcar (lambda (var) (position var *ac-variables*)) variables))


(defun narrow-all-different (domains)
  ;A letter left with one digit removes it from every other letter, repeated until no new
  ;letter is fixed.  :empty if some letter loses its last digit.
  (let ((changed nil)
        (done nil))
    (loop for fixed = (find-if (lambda (n)
                                 (and (not (member n done))
                                      (alexandria:length= 1 (svref domains n))))
                               *ac-letter-numbers*)
          while fixed
          do (push fixed done)
             (let ((digit (first (svref domains fixed))))
               (dolist (n *ac-letter-numbers*)
                 (when (and (/= n fixed) (member digit (svref domains n)))
                   (setf (svref domains n) (remove digit (svref domains n)))
                   (when (null (svref domains n))
                     (return-from narrow-all-different :empty))
                   (pushnew n changed)))))
    changed))


(defparameter *ac-network*
  (make-constraint-network
    (length *ac-variables*)
    (list (make-support-constraint (ac-numbers 'c6 'D 'T 'c5)
                                   (lambda (c6 d tt c5)
                                     (and (/= d tt) (= (+ c6 d d) (+ tt (* 10 c5))))))
          (make-support-constraint (ac-numbers 'c5 'L 'R 'c4)
                                   (lambda (c5 l r c4)
                                     (and (/= l r) (= (+ c5 l l) (+ r (* 10 c4))))))
          (make-support-constraint (ac-numbers 'c4 'A 'E 'c3)
                                   (lambda (c4 a e c3)
                                     (and (/= a e) (= (+ c4 a a) (+ e (* 10 c3))))))
          (make-support-constraint (ac-numbers 'c3 'N 'R 'B 'c2)
                                   (lambda (c3 n r b c2)
                                     (and (/= n r b) (= (+ c3 n r) (+ b (* 10 c2))))))
          (make-support-constraint (ac-numbers 'c2 'O 'E 'c1)
                                   (lambda (c2 o e c1)
                                     (and (/= o e) (= (+ c2 o e) (+ o (* 10 c1))))))
          (make-support-constraint (ac-numbers 'c1 'D 'G 'R 'c0)
                                   (lambda (c1 d g r c0)
                                     (and (/= d g r) (= (+ c1 d g) (+ r (* 10 c0))))))
          (make-narrowing-constraint *ac-letter-numbers* #'narrow-all-different)))
  "The column sums (each column's letters differ) and the all-different rule.")


;------------------------ queries and updates ----------------------


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


(define-update arc-consistency! ()
  ;Narrows every remaining list to a fixpoint; drops the state if some list becomes empty.
  (do (setf $domains (make-array (length *ac-variables*)))
      (ww-loop for $var in *ac-variables*
               for $n from 0
        do (setf (svref $domains $n) (get-remaining $var)))
      (if (propagate-network *ac-network* $domains :all)
        (ww-loop for $var in *ac-variables*
                 for $n from 0
          do (remaining $var (svref $domains $n)))
        (inconsistent-state))))


;------------------------ actions ----------------------------


(define-action assign-column-6
    1
  (product ?D (get-remaining D) ?T (get-remaining T) ?c5 (get-remaining c5) ?c6 (get-remaining c6))
  (and (/= ?D ?T)
       (= (+ ?c6 ?D ?D) (+ ?T (* 10 ?c5))))
  (?D ?D ?T)
  (assert (assign-letter D ?D)
          (assign-letter T ?T)
          (remaining c5 (list ?c5))
          (arc-consistency!)))


(define-action assign-column-5
    1
  (product ?L (get-remaining L) ?R (get-remaining R) ?c4 (get-remaining c4) ?c5 (get-remaining c5))
  (and (/= ?L ?R)
       (= (+ ?c5 ?L ?L) (+ ?R (* 10 ?c4))))
  (?L ?L ?R)
  (assert (assign-letter L ?L)
          (assign-letter R ?R)
          (remaining c4 (list ?c4))
          (arc-consistency!)))


(define-action assign-column-4
    1
  (product ?A (get-remaining A) ?E (get-remaining E) ?c3 (get-remaining c3) ?c4 (get-remaining c4))
  (and (/= ?A ?E)
       (= (+ ?c4 ?A ?A) (+ ?E (* 10 ?c3))))
  (?A ?A ?E)
  (assert (assign-letter A ?A)
          (assign-letter E ?E)
          (remaining c3 (list ?c3))
          (arc-consistency!)))


(define-action assign-column-3
    1
  (product ?N (get-remaining N) ?R (get-remaining R) ?B (get-remaining B) ?c2 (get-remaining c2) ?c3 (get-remaining c3))
  (and (/= ?N ?R ?B)
       (= (+ ?c3 ?N ?R) (+ ?B (* 10 ?c2))))
  (?N ?R ?B)
  (assert (assign-letter N ?N)
          (assign-letter B ?B)
          (remaining c2 (list ?c2))
          (arc-consistency!)))


(define-action assign-column-2
    1
  (product ?O (get-remaining O) ?E (get-remaining E) ?c1 (get-remaining c1) ?c2 (get-remaining c2))
  (and (/= ?O ?E)
       (= (+ ?c2 ?O ?E) (+ ?O (* 10 ?c1))))
  (?O ?E ?O)
  (assert (assign-letter O ?O)
          (remaining c1 (list ?c1))
          (arc-consistency!)))


(define-action assign-column-1
    1
  (product ?D (get-remaining D) ?G (get-remaining G) ?R (get-remaining R) ?c0 (get-remaining c0) ?c1 (get-remaining c1))
  (and (/= ?D ?G ?R)
       (= (+ ?c1 ?D ?G) (+ ?R (* 10 ?c0))))
  (?D ?G ?R)
  (assert (assign-letter G ?G)
          (arc-consistency!)))


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
