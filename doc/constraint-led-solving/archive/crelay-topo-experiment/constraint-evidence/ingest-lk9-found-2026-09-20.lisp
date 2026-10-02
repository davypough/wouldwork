;;; Replay after ingest-lk8-validation-2026-09-20.lisp. Data only, no search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)
(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path)))
  (ingest-ledger-result ledger 'lk9 :found
    :actions '("(TOGGLE-SWITCH > AGENT1 toggles SWITCH2)")
    :validated nil :run "constraint-evidence/lk9-found-2026-09-20.txt"
    :date "2026-09-20")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-20")
  (assert (eq :realized (getf (ledger-record (read-realization-ledger path) 'lk9) :status)))
  (format t "LK9 REALIZED; ledger checks and readback passed.~%"))