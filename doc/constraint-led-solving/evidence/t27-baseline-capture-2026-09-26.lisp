;;; T27 A4 baseline.  Captures REPORT-REALIZATION-LEDGER's output for the crelay-topo
;;; version-1 ledger with the PRE-T27 tech/constraint-ledger.lisp, so the T27 checks can
;;; show the report is byte-identical after the change.  Run once, BEFORE any T27 code,
;;; from the repository root:
;;;   sbcl --noinform --no-userinit --no-sysinit --script doc/constraint-method/evidence/t27-baseline-capture-2026-09-26.lisp
;;; Loads no Wouldwork, stages nothing, changes no ledger.  An EMPTY :WW package is
;;; created so the ledger file can be evaluated, as in ledger-checks-2026-09-20.lisp.

(defpackage :ww (:use :cl))


(let ((*package* (find-package :ww)))
  (with-open-file (in "tech/constraint-ledger.lisp")
    (loop for form = (read in nil :end) until (eq form :end)
          unless (and (consp form) (eq (first form) 'in-package))
            do (eval form))))


(let* ((ledger (ww::read-realization-ledger
                 "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt"))
       (text (with-output-to-string (*standard-output*)
               (ww::report-realization-ledger ledger))))
  (with-open-file (out "doc/constraint-method/evidence/t27-crelay-report-baseline-2026-09-26.txt"
                       :direction :output :if-exists :supersede :if-does-not-exist :create
                       :external-format :utf-8)
    (write-string text out))
  (format t "~&Baseline written: ~D characters, ~D lines.~%"
          (length text) (count #\Newline text)))
