;;; Run after ingest-lk3-validation-2026-09-20.lisp when rebuilding the ledger.
;;; Data ingestion only; no initialization, staging or search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)

(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path)))
  (ingest-ledger-result
   ledger 'lk7 :found
   :actions '("(MOVE > AGENT1 moves via ((WALK LOCATION13 (GATE3 GATE5) LOCATION7)))"
              "(PICKUP-TRAY > AGENT1 picks up TRAY1 at LOCATION7 from LOCATION7)"
              "(MOVE > AGENT1 moves via ((WALK LOCATION7 (GATE3 GATE5) LOCATION12)))"
              "(PUT-TRAY > AGENT1 puts TRAY1 on PLATE4 at LOCATION13)"
              "(MOVE > AGENT1 moves via ((STEP (LOCATION12 GROUND) NIL (LOCATION12 PLATE5))))")
   :validated t :run "constraint-evidence/lk7-validation-2026-09-20.txt"
   :date "2026-09-20")
  (ledger-add-event
   (ledger-record ledger 'lk7) "2026-09-20" :amended
   "Validated the stated goal only: tray1 holds plate4 and agent1 holds plate5. The stronger non-agent witness intent is not established; lk8 must realize the excursion from this endpoint.")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-20")
  (assert (eq :closed (getf (ledger-record (read-realization-ledger path) 'lk7) :status)))
  (format t "LK7 CLOSED; ledger checked and read back successfully.~%"))
