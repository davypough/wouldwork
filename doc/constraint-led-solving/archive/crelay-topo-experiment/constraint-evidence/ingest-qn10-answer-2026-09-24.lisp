;;; Data-only ingestion after ingest-lk5-cutoff10-bound-2026-09-21.lisp.
;;; Records D's answer to qn10 and nothing else.
;;; No Wouldwork initialization, problem loading, replay or search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)

(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path))
       (question (ledger-record ledger 'qn10))
       (lk4-before (copy-tree (ledger-record ledger 'lk4))))
  (assert (eq :open (getf question :status)))
  (assert (null (ledger-record ledger 'pr17)))
  (answer-ledger-question ledger 'qn10 :for-this-device-yes
    "for the device labelling the R3-R5 spine arc, opening it requires traversal outside that arc into a region the spine does not visit: its switch's only manipulation reach candidate lies in an off-spine cul-de-sac behind a two-keeper guard (pr15, pr16), and the excursion was realized as lk7-lk9; filed as schema gap G16"
    "D" "2026-09-24")
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-24")
  (let ((readback (read-realization-ledger path)))
    (check-ledger-well-formed readback)
    (assert (eq :answered (getf (ledger-record readback 'qn10) :status)))
    (assert (eq :for-this-device-yes (getf (ledger-record readback 'qn10) :answer)))
    (assert (eq 'pr17 (getf (ledger-record readback 'qn10) :answer-premise)))
    (assert (equalp lk4-before (ledger-record readback 'lk4)))
    (format t "~&QN10 answered :FOR-THIS-DEVICE-YES as PR17 (user-asserted); lk4 unchanged. Checks/readback passed.~%")))
