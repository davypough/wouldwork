;;;; Filename: problem-socrates1.lisp

;;; The Death of Socrates picture problem from The Eyes of Ara game. First half of search: rotating disk 1,2.
;;; Takes ~4 min at 5 GHz

(in-package :ww)

(ww-set *problem-name* socrates1)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

;(ww-set *depth-cutoff* 14)

(ww-set *progress-reporting-interval* 1000000)



(define-types
   disk (disk1 disk2 disk3 disk4)                   ;rotatable disks in picture
   piece (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)   ;puzzle pieces
   position (A B C D E F G H I J K L M N O P))      ;puzzle piece positions


(define-dynamic-relations
   (at $piece position))


(define-action rotate-disk   ;rotate each disk 90 deg clockwise
   1
   (?disk disk)
   (always-true)
   (?disk)
   (assert (if (and (eql ?disk 'disk1)
                    (bind (at $p1 A)) (bind (at $p2 B)) (bind (at $p3 C)) (bind (at $p4 D)))
             (do (at $p1 B) (at $p2 C) (at $p3 D) (at $p4 A)))
           (if (and (eql ?disk 'disk2)
                    (bind (at $p1 F)) (bind (at $p2 G)) (bind (at $p3 H)) (bind (at $p4 B)))
             (do (at $p1 G) (at $p2 H) (at $p3 B) (at $p4 F)))
           (if (and (eql ?disk 'disk3)
                    (bind (at $p1 C)) (bind (at $p2 J)) (bind (at $p3 K)) (bind (at $p4 L)))
             (do (at $p1 J) (at $p2 K) (at $p3 L) (at $p4 C)))
           (if (and (eql ?disk 'disk4)
                    (bind (at $p1 H)) (bind (at $p2 N)) (bind (at $p3 O)) (bind (at $p4 J)))
             (do (at $p1 N) (at $p2 O) (at $p3 J) (at $p4 H)))))


(define-init
   (at 1 A) (at 2 B) (at 3 C) (at 4 D)   ;disk1
   (at 6 F) (at 7 G) (at 8 H)            ;disk2
   (at 10 J) (at 11 K) (at 12 L)         ;disk3
   (at 14 N) (at 15 O))                  ;disk4


(define-goal
   (and (at 6 A) (at 1 B) (at 8 C) (at 12 D)   ;initial goal
        (at 14 F) (at 3 G) (at 10 H)))
        ;(at 2 J) (at 11 K) (at 7 L)   ;plus final goal
        ;(at 15 N) (at 4 O)))
