;;; Isolated regression evidence for RO, the role-obligation analysis. From the repository
;;; root, run sbcl --noinform --no-userinit --no-sysinit --script <this-file>.
;;; Does not stage Wouldwork, run an extractor, write a report, or consult predictions.
;;; These are the acceptance cases committed in Constraint-Role-Obligations.txt section 3.
(defpackage :ro-profile-checks (:use :cl))
(in-package :ro-profile-checks)
(defparameter *checks* 0)
(defun check (condition)
  (incf *checks*)
  (assert condition))
(with-open-file (in "tech/constraint-profile.lisp")
  (loop for form = (read in nil :end) until (eq form :end)
        when (and (consp form) (eq (first form) 'defun)
                  (member (second form)
                    '(keeper-sorted-set keeper-pressure-clauses
                      role-subsets role-neighbourhood role-hall-violator role-augment
                      role-matching role-perfect-p role-eligibility-without-witness
                      role-eligibility-without-edge role-forced-witnesses
                      role-forced-pairings role-view-classes role-removal-reason
                      role-availability-known-p role-eligibility)))
          do (eval form)))

;; 1  Three supports and exactly three universally eligible witnesses force membership of
;;    all three, and fix no body to any particular support.
(let* ((supports '(s1 s2 s3))
       (eligibility (role-eligibility supports '(a b c))))
  (check (role-perfect-p supports eligibility))
  (check (equal '(a b c) (keeper-sorted-set (role-forced-witnesses supports eligibility))))
  (check (null (role-forced-pairings supports eligibility))))

;; 2  A fourth universally eligible witness forces nobody.
(let* ((supports '(s1 s2 s3))
       (eligibility (role-eligibility supports '(a b c d))))
  (check (role-perfect-p supports eligibility))
  (check (null (role-forced-witnesses supports eligibility)))
  (check (null (role-forced-pairings supports eligibility))))

;; 3  Two supports restricted to one witness are short however large the rest of the pool is.
(let ((eligibility '((s1 a) (s2 a) (s3 b c d e))))
  (check (not (role-perfect-p '(s1 s2 s3) eligibility)))
  (check (equal '(s1 s2) (role-hall-violator '(s1 s2 s3) eligibility)))
  (check (= 5 (length (role-neighbourhood '(s1 s2 s3) eligibility)))))

;; 4  Two references to one support cost one witness, not two.
(check (equal '((p q)) (keeper-pressure-clauses '(controls ((p p q)) d normal) '(p q))))
(check (= 2 (length (first (keeper-pressure-clauses '(controls ((p p q)) d normal) '(p q))))))

;; 5  Alternatives stay alternatives: a body forced in one is not globally forced.
(let* ((first-supports '(s1 s2))
       (second-supports '(s3))
       (first-forced (role-forced-witnesses first-supports
                                            (role-eligibility first-supports '(a b))))
       (second-forced (role-forced-witnesses second-supports
                                             (role-eligibility second-supports '(c)))))
  (check (equal '(a b) (keeper-sorted-set first-forced)))
  (check (equal '(c) second-forced))
  (check (null (intersection first-forced second-forced))))

;; 6  Unknown availability stays unknown; an absent key is not an empty set, and a declared
;;    extent is never promoted into an availability set.
(check (role-availability-known-p '(:available-witnesses (a b c))))
(check (not (role-availability-known-p '(:available-witnesses :unknown))))
(check (not (role-availability-known-p '(:view :physical))))

;; 7  Inversion demands no positive pressure, and a non-plate controller demands no witness.
(check (null (keeper-pressure-clauses '(controls ((p)) d inverted) '(p))))
(check (equal '(nil) (keeper-pressure-clauses '(controls ((sw)) d normal) '(p))))
(check (null (keeper-pressure-clauses '(controls nil d normal) '(p))))

;; 8  Eligibility is built over the demanded supports and nothing else, so no destination can
;;    appear for a support the requested aggregate did not name.
(let ((eligibility (role-eligibility '(s1 s2) '(a b))))
  (check (= 2 (length eligibility)))
  (check (equal '(s1 s2) (mapcar #'first eligibility))))

;; 9  Vacuity guard. With no admissible assignment the forced-membership test would call
;;    every witness forced, which is why the reporter tests the matching FIRST and prints a
;;    violator instead.  Both halves are asserted so the guard cannot be dropped silently.
(let ((eligibility '((s1 a) (s2))))
  (check (not (role-perfect-p '(s1 s2) eligibility)))
  (check (equal '(s2) (role-hall-violator '(s1 s2) eligibility)))
  (check (equal '(a) (role-forced-witnesses '(s1 s2) eligibility))))

;; 10  A shortage is a matching failure, not a count: three supports and three witnesses.
(let ((eligibility '((s1 a) (s2 a) (s3 a b c))))
  (check (= 3 (length (role-neighbourhood '(s1 s2 s3) eligibility))))
  (check (not (role-perfect-p '(s1 s2 s3) eligibility)))
  (check (equal '(s1 s2) (role-hall-violator '(s1 s2 s3) eligibility))))

;; 11  The matching augments rather than keeping a first greedy choice, in either order.
(check (= 2 (length (role-matching '(s1 s2) '((s1 a) (s2 a b))))))
(check (= 2 (length (role-matching '(s2 s1) '((s1 a) (s2 a b))))))

;; 12  Disjoint single-witness eligibility forces both membership and the pairings.
(let ((eligibility '((s1 a) (s2 b))))
  (check (role-perfect-p '(s1 s2) eligibility))
  (check (equal '(a b) (keeper-sorted-set (role-forced-witnesses '(s1 s2) eligibility))))
  (check (equal '((s1 a) (s2 b)) (role-forced-pairings '(s1 s2) eligibility))))

;; 13  Edge removal and witness removal touch only what they name.
(check (equal '((s1) (s2 a)) (role-eligibility-without-edge '((s1 a) (s2 a)) 's1 'a)))
(check (equal '((s1) (s2)) (role-eligibility-without-witness '((s1 a) (s2 a)) 'a)))

;; 14  Every removal carries its reason, and a body absent from both lists is available.
(let ((scenario '(:excluded ((x "holding cargo")) :committed ((y "needed elsewhere")))))
  (check (search "excluded" (role-removal-reason 'x scenario)))
  (check (search "needed elsewhere" (role-removal-reason 'y scenario)))
  (check (null (role-removal-reason 'z scenario))))

;; 15  An unpaired object is present in every view; a copy is present in one.
(check (equal '("live" "unpaired") (role-view-classes :physical)))
(check (equal '("ghost" "unpaired") (role-view-classes :recording)))
(check (null (role-view-classes :neither)))
(check (= 8 (length (role-subsets '(s1 s2 s3)))))

(format t "~&RO helper checks passed: ~D. All source forms read successfully.~%" *checks*)
