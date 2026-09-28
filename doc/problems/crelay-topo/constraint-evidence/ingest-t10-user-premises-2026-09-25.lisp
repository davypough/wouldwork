;;; Data-only ingestion after ingest-qn10-answer-2026-09-24.lisp.
;;; Records the user-asserted premises the validated T10 chain rests on (pr18-pr22), so the
;;; premise-provenance clause of T10's acceptance is met, then lists every premise in force
;;; grouped by provenance species (user-asserted apart from derived and search-measured).
;;; No Wouldwork initialization, problem loading, replay or search.
;;; Run from the repository root, like the earlier ingest scripts:
;;;   (load "doc/problems/crelay-topo/constraint-evidence/ingest-t10-user-premises-2026-09-25.lisp")
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)


(defparameter *t10-evidence*
  "constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt")


(defparameter *t10-user-premises*
  (list
    (make-ledger-premise 'pr18
      "at the start of the recording that uses the ghost-held tray, connector1 rests on plate1 holding gate1 open, tray1 is at location7, and agent1 is at recorder1 empty-handed; a first setup cycle, cancelled at the recorder, produces this arrangement"
      '(:user-asserted :by "D" :date "2026-09-24")
      :premise-gaps '("stated by D as the precondition of the ghost-held-tray cycle; realized in one validated sequence (actions 1-11), not derived from the profile")
      :segment '(:view :physical :cycle :closed :ghosts :absent)
      :sources (list (concatenate 'string *t10-evidence* " USER-ASSERTED PREMISE, STEP 1")
                     "constraint-evidence/validate-b2-ghost-tray-2026-09-24.lisp"))
    (make-ledger-premise 'pr19
      "a ghost of the agent holding tray1* at location5 is an elevated landing support (tray top 3/2): the live agent, lifted to location20 by blower1, jumps onto it, turns switch1 off so gate2 opens, and jumps through gate2 to location6, so box1 can leave the alcove"
      '(:user-asserted :by "D" :date "2026-09-24")
      :premise-gaps '("named by D after the B2 construction audit found no elevation resource reachable from B1; checked by hand, then validated in one concrete sequence (23 actions); the static profile does not derive it")
      :segment '(:view :physical :cycle :open :ghosts :present)
      :sources (list (concatenate 'string *t10-evidence* " STEPS 2-7 and VALIDATION BY D")
                     "constraint-evidence/validate-b2-ghost-tray-2026-09-24.lisp"))
    (make-ledger-premise 'pr20
      "the second recording cycle ends with box1 on plate2, connector1 on plate1 and tray1 on the ground at location7, the arrangement the third cycle forks from"
      '(:user-asserted :by "D" :date "2026-09-24")
      :premise-gaps '("D's correction of an earlier validated arrangement (tray1 on plate1, connector1 at location9), which is kept on record but not used")
      :segment '(:view :physical :cycle :closed :ghosts :absent)
      :sources (list (concatenate 'string *t10-evidence* " CORRECTION BY D and REVISED ENDPOINT VALIDATED")
                     "constraint-evidence/validate-b2-box-plate2-rev-2026-09-24.lisp"))
    (make-ledger-premise 'pr21
      "receiver1 is lit through repeater1 by two connectors: a ghost connector on the ground at location9 paired with transmitter1 and repeater1, and a live connector raised on box1 on a held tray, paired with repeater1 and receiver1, carried to location15"
      '(:user-asserted :by "D" :date "2026-09-24")
      :premise-gaps '("D's design choice for the gate8 beam; S6 has no connector-to-connector sightlines (G17), so the design was confirmed only by the validated end state (receiver1 active), not derived")
      :segment '(:view :physical :cycle :open :ghosts :present)
      :sources (list (concatenate 'string *t10-evidence* " EXTENSION VALIDATED BY D (D chose)")
                     "constraint-evidence/validate-c3-alt-2026-09-25.lisp"))
    (make-ledger-premise 'pr22
      "in the third recording cycle the ghost holds tray1* at location5 while the live agent, riding blower1 to location20, places box1 and then a connector paired with repeater1 and receiver1 on it; live tray1 on plate4 and the ghost on plate5 open gate6; switch2 on opens gate7; the ghost's released tray1* settles the stack onto the live agent's held tray1 on plate5; the ghost then puts tray1* on plate1, pairs connector1* at location9 with repeater1 and transmitter1 and stands on plate3; the live agent carries the lit stack through gate7 to location15"
      '(:user-asserted :by "D" :date "2026-09-25")
      :premise-gaps '("D's route, written by D as 50 actions and validated from the initial state (80 actions with the cycle-1/2 prefix); it supersedes D's earlier cycle-3 handoff plan (parts A-C) and the unrecorded D1-D3 files")
      :segment '(:view :physical :cycle :open :ghosts :present)
      :sources (list (concatenate 'string *t10-evidence* " ALTERNATIVE CYCLE 3 VALIDATED BY D")
                     "constraint-evidence/validate-c3-alt-2026-09-25.lisp"))))


(defun report-t10-premises-by-provenance (ledger)
  (dolist (species '(:derived :search-measured :user-asserted))
    (format t "~&~%  PREMISES IN FORCE, ~A~%" species)
    (dolist (record (getf ledger :records))
      (when (and (eq (getf record :kind) :premise)
                 (eq (getf record :status) :in-force)
                 (eq (first (getf record :provenance)) species))
        (report-ledger-premise-line ledger (getf record :id))))))


(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path))
       (count-before (length (getf ledger :records))))
  (assert (ledger-record ledger 'pr17))
  (dolist (record *t10-user-premises*)
    (assert (null (ledger-record ledger (getf record :id))))
    (ledger-set-value record :events
                      (list (list :date (getf (rest (getf record :provenance)) :date)
                                  :event :opened :by "ledger" :note "")))
    (add-ledger-record ledger record))
  (check-ledger-well-formed ledger)
  (write-realization-ledger ledger path "2026-09-25")
  (let ((readback (read-realization-ledger path)))
    (check-ledger-well-formed readback)
    (assert (= (length (getf readback :records)) (+ count-before 5)))
    (dolist (id '(pr18 pr19 pr20 pr21 pr22))
      (let ((record (ledger-record readback id)))
        (assert (eq :premise (getf record :kind)))
        (assert (eq :user-asserted (first (getf record :provenance))))
        (assert (eq :in-force (getf record :status)))))
    (format t "~&PR18-PR22 recorded as user-asserted premises by D. Checks/readback passed.~%")
    (report-t10-premises-by-provenance readback)))
