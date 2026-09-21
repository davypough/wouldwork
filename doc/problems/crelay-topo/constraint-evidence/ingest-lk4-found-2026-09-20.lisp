;;; Replay after ingest-lk9-found-2026-09-20.lisp; data only, no search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)
(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path)))
  (ingest-ledger-result ledger 'lk4 :found
    :actions '("(MOVE > AGENT1 moves via ((WALK LOCATION14 (GATE6 GATE7 SCREEN1) LOCATION15)))")
    :validated nil :run "constraint-evidence/lk4-found-2026-09-20.txt"
    :date "2026-09-20")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-20")
  (assert (eq :realized (getf (ledger-record (read-realization-ledger path) 'lk4) :status)))
  (format t "LK4 REALIZED; ledger checks and readback passed.~%"))