;;; Clean-image acceptance checks for the approved G14 reporting fix.
;;; From the repository root:
;;;   sbcl --noinform --script doc/problems/crelay-topo/constraint-evidence/g14-helper-checks-2026-09-22.lisp
;;; Stages the diagnostic exemplar but does not run a search, replay, validation, or writer.

(require :asdf)
(asdf:load-asd (truename "wouldwork.asd"))
(asdf:load-system :wouldwork)
(in-package :ww)

(defparameter *g14-checks* 0)


(defun g14-check (condition)
  (incf *g14-checks*)
  (assert condition))


(stage crelay-topo)
(format t "~&G14 LOAD BEGIN~%")
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(format t "~&G14 LOAD END~%")

;; 1-3. Pair position resolves the class, and the existing interprocedural
;; shadow predicate carries that result through its query call.
(g14-check (equal '("live")
                  (role-layer-test-classes '(recording-copy> ?object $ghost)
                                           '(recording-copy>) nil)))
(g14-check (equal '("ghost")
                  (role-layer-test-classes '(recording-copy> $live ?object)
                                           '(recording-copy>) nil)))
(g14-check (null (role-layer-test-classes '(recording-copy> ?one ?two)
                                           '(recording-copy>) nil)))
(g14-check (equal '("ghost")
                  (role-layer-test-classes '(recording-shadow-object ?object)
                                           (layer-pair-relations) nil)))

;; 4-6. IF branch polarity and AND governance are retained; OR is not.
(let ((then-sites (role-governed-relation-sites
                    '(if (recording-shadow-object ?object)
                         (open ?gate)
                         (recording-open ?gate))
                    'open nil 'test nil))
      (else-sites (role-governed-relation-sites
                    '(if (recording-shadow-object ?object)
                         (open ?gate)
                         (recording-open ?gate))
                    'recording-open nil 'test nil))
      (and-sites (role-governed-relation-sites
                   '(and (recording-shadow-object ?object) (open ?gate))
                   'open nil 'test nil))
      (or-sites (role-governed-relation-sites
                  '(or (recording-shadow-object ?object) (open ?gate))
                  'open nil 'test nil)))
  (g14-check (cdr (first (second (first then-sites)))))
  (g14-check (not (cdr (first (second (first else-sites))))))
  (g14-check (= 1 (length (second (first and-sites)))))
  (g14-check (null (second (first or-sites)))))

;; 7-9. Admission obeys positive and negated class tests, and contradictions
;; produce an empty admission rather than an in-view result.
(multiple-value-bind (admitted governed unresolved)
    (role-site-admission
      '(test (((recording-shadow-object ?object) . t)))
      (layer-pair-relations))
  (g14-check (equal '("ghost") admitted))
  (g14-check governed)
  (g14-check (zerop unresolved)))
(multiple-value-bind (admitted governed unresolved)
    (role-site-admission
      '(test (((recording-shadow-object ?object) . nil)))
      (layer-pair-relations))
  (g14-check (equal '("live" "unpaired") admitted))
  (g14-check governed)
  (g14-check (zerop unresolved)))
(multiple-value-bind (admitted governed)
    (role-site-admission
      '(test (((recording-shadow-object ?object) . t)
              ((recording-shadow-object ?object) . nil)))
      (layer-pair-relations))
  (g14-check (null admitted))
  (g14-check governed))

;; 10-13. The staged gate relations have the committed two-way classification,
;; include blind sites without losing the existential result, and terminate.
(let ((physical (role-axiom-index 'open :physical))
      (recording (role-axiom-index 'recording-open :physical)))
  (g14-check (eq :in-view (first physical)))
  (g14-check (eq :out-of-view (first recording)))
  (g14-check (>= (third physical) 2))
  (g14-check (zerop (third recording))))
(g14-check (listp (role-relation-read-sites 'open)))

;; 14. Allocation inputs and output remain unchanged for the committed scenario.
(let* ((supports '(plate6 plate7 plate8))
       (eligibility (role-eligibility supports '(box1 connector1 tray1))))
  (g14-check (role-perfect-p supports eligibility))
  (g14-check (equal '(box1 connector1 tray1)
                    (keeper-sorted-set
                      (role-forced-witnesses supports eligibility))))
  (g14-check (null (role-forced-pairings supports eligibility)))
  (g14-check (= 3 (length eligibility))))

;; 15. The S4 printer remains deterministic and is not altered by the RO-local
;; reporter.  The two strings compare the unchanged shared output directly.
(let* ((axioms (device-state-axioms (control-facts)))
       (gate9 (find 'gate9 (control-facts) :key #'third))
       (output1 (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (report-keeper-axioms 'gate9 axioms))))
       (output2 (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (report-keeper-axioms (third gate9) axioms)))))
  (g14-check (string= output1 output2)))

(format t "~&G14 helper checks passed: ~D.~%" *g14-checks*)
