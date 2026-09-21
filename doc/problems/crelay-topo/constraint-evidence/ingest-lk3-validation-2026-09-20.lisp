;;; Run after ingest-lk4-bound-2026-09-20.lisp when rebuilding the ledger.
;;; Standalone data ingestion only: no Wouldwork initialization, staging or search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)

(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path)))
  (ingest-ledger-result
   ledger 'lk3 :found
   :actions '("(MOVE > AGENT1 moves via ((WALK LOCATION9 (GATE5) LOCATION13)))")
   :validated t
   :run "constraint-evidence/lk3-validation-2026-09-20.txt"
   :date "2026-09-20")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-20")
  (assert (eq :closed (getf (ledger-record (read-realization-ledger path) 'lk3) :status)))
  (format t "LK3 CLOSED; ledger checked and read back successfully.~%"))
