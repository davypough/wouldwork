;;; Run after ingest-lk7-validation-2026-09-20.lisp to reproduce the ledger.
;;; Data ingestion only: no user initialization, staging or search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)
(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path)))
  (ingest-ledger-result ledger 'lk8 :found
    :actions '(      "(MOVE > AGENT1 moves via ((STEP (LOCATION12 PLATE5) NIL (LOCATION12 GROUND))))"
      "(MOVE > AGENT1 moves via ((WALK LOCATION12 (GATE5) LOCATION9)))"
      "(PICKUP-CONNECTOR-RETAINING-PAIRINGS > AGENT1 picks up CONNECTOR1 retaining pairings at LOCATION9)"
      "(MOVE > AGENT1 moves via ((WALK LOCATION9 (GATE5) LOCATION12)))"
      "(PUT-CONNECTOR > AGENT1 puts CONNECTOR1 on PLATE5 without pairings at LOCATION12)"
      "(MOVE > AGENT1 moves via ((WALK LOCATION12 (GATE6 SCREEN1) LOCATION14)))")
    :validated t :run "constraint-evidence/lk8-validation-2026-09-20.txt"
    :date "2026-09-20")
  (ledger-add-event (ledger-record ledger 'lk8) "2026-09-20" :amended
    "Connector1 replaced agent1 on plate5; tray1 held plate4; agent1 reached location14. This endpoint establishes non-agent witnesses for this realization only.")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-20")
  (assert (eq :closed (getf (ledger-record (read-realization-ledger path) 'lk8) :status)))
  (format t "LK8 CLOSED; ledger checked and read back successfully.~%"))