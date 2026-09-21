;;; Ingests every outcome D has reported after the first (T10).  Run from the repository root,
;;; standalone or in the ordinary image.  It STAGES NOTHING, LOADS NO PROBLEM, RUNS NO SEARCH.
;;;
;;; TO REPRODUCE THE LEDGER FROM NOTHING, in this order:
;;;   build-realization-ledger-2026-09-20.lisp
;;;   ingest-lk1-2026-09-20.lisp
;;;   ingest-runs-2026-09-20.lisp
;;;
;;; WHAT D REPORTED, in order:
;;;   1  VALIDATE-ACTION-SEQUENCE on the first crossing's raw phase path, from the chain's
;;;      origin: SUCCESS-P T, ACTION-COUNT 4, GOAL-CHECKED-P T, GOAL-SATISFIED-P T.
;;;   2  The second crossing: FOUND, one action.
;;;   3  VALIDATE-ACTION-SEQUENCE on the second crossing's path, FROM ITS OWN PHASE SOURCE
;;;      STATE: SUCCESS-P T, ACTION-COUNT 1, goal checked and satisfied.
;;;   4  The third crossing: FOUND, one action.
;;;
;;; TWO THINGS THE RUNS SHOWED THAT THE LEDGER DID NOT ALREADY SAY.
;;;   A  The replayable path of a recorder-bearing realization is SHORTER than its printed
;;;      transcript: the pause, the ghost's return walk, the recorder stop and the resume are
;;;      display structure, not replayed actions.  Four actions against a seven-line
;;;      transcript.  Filed as pr13, measured.
;;;   B  Two of the six devices are ALREADY OPEN in the initial state, from the instance's own
;;;      initial occupancy and switch settings.  That is why the second and third crossings
;;;      cost one walk each.  It is GRADE 1 AND WAS DERIVABLE FROM THE PROFILE BEFORE ANY
;;;      SEARCH RAN; the ledger did not carry it, and the runs are what made it visible.
;;;      Filed as pr14, derived, with that omission stated rather than glossed.

(unless (find-package :ww)
  (defpackage :ww (:use :cl)))

(unless (fboundp 'ww::read-realization-ledger)
  (let ((*package* (find-package :ww)))
    (with-open-file (in "tech/constraint-ledger.lisp")
      (loop for form = (read in nil :end) until (eq form :end)
            unless (and (consp form) (eq (first form) 'in-package))
              do (eval form)))))

(in-package :ww)


(defparameter *crelay-ledger-path* "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")


(defparameter *lk1-validated-path*
  '("(START-RECORDER > AGENT1 starts the recorder)"
    "(MOVE > AGENT1* moves via ((WALK LOCATION1 NIL LOCATION2)))"
    "(MOVE > AGENT1* moves via ((STEP (LOCATION2 GROUND) NIL (LOCATION2 PLATE1))))"
    "(MOVE > AGENT1 moves via ((WALK LOCATION1 (GATE1) LOCATION7)))")
  "The four actions the validator replayed, as it printed them.  The raw forms remain
   reproducible as (solution.path (goal-chain-phase-solution <phase>)).")


(defparameter *lk2-validated-path*
  '("(MOVE > AGENT1 moves via ((WALK LOCATION7 (GATE3) LOCATION9)))"))


(defparameter *lk3-path*
  '("(MOVE > AGENT1 moves via ((WALK LOCATION9 (GATE5) LOCATION13)))"))


(defun close-crelay-link (ledger id path seconds gap-note)
  "File a validated find: the link closes, and its discharged gaps are replaced by what the
   validation established rather than left standing."
  (ingest-ledger-result ledger id :found
                        :actions path :validated t
                        :run (format nil "constraint-evidence/~(~A~)-validation-2026-09-20.txt" id)
                        :nodes nil :seconds seconds :date "2026-09-20")
  (ledger-set-value (ledger-record ledger id) :premise-gaps nil)
  (ledger-add-event (ledger-record ledger id) "2026-09-20" :amended
                    (ledger-collapse-whitespace gap-note)))


(defun ingest-runs ()
  (let ((ledger (read-realization-ledger *crelay-ledger-path*)))
    (close-crelay-link ledger 'lk1 *lk1-validated-path* 4
                       "both gaps discharged: a ghost held the support without spending a live
                        occupant, and the raw path replaced the transcript and validated at
                        four actions")
    (close-crelay-link ledger 'lk2 *lk2-validated-path* 1
                       "gap discharged: the device on this arc was already open on the initial
                        state's own occupancy, so the crossing cost only the walk")
    (ingest-ledger-result ledger 'lk3 :found
                          :actions *lk3-path* :validated nil
                          :run "constraint-evidence/lk3-first-run-2026-09-20.txt"
                          :nodes nil :seconds 1 :date "2026-09-20")
    (add-ledger-record ledger
      (make-ledger-premise 'pr13
        (ledger-collapse-whitespace
          "the replayable path of a recorder-bearing realization is shorter than its printed
           transcript: the pause, the ghost's return walk, the recorder stop and the resume
           are display structure and are not replayed actions")
        (list :search-measured :outcome :found
              :start-state "the chain's origin state"
              :search-expression "validation of the first crossing's raw phase path"
              :cutoff 8 :threads 0
              :run "constraint-evidence/lk1-validation-2026-09-20.txt")
        :depends-on '((lk1))
        :premise-gaps (list (ledger-collapse-whitespace
                              "observed on one realization carrying one recorder cycle; not
                               established for every recorder-bearing path"))
        :segment '(:view :physical :cycle :open :ghosts :present)
        :sources '("the validator's per-action output, four actions against a seven-line transcript")))
    (add-ledger-record ledger
      (make-ledger-premise 'pr14
        (ledger-collapse-whitespace
          "two of the six devices on the spine are already open in the initial state, one from
           an occupant the instance places on its support at the start and one from the
           initial setting of the switch pair, so those two crossings demand no witness of
           their own")
        '(:derived :grade 1 :by "the instance's DEFINE-INIT against S1's control table")
        :premise-gaps (list (ledger-collapse-whitespace
                              "this is GRADE 1 and was derivable from the profile before any
                               search ran.  The ledger did not carry it, and two measured runs
                               are what made it visible.  A cheap crossing is not evidence
                               that the method found something; it is evidence that the
                               decomposition was built without a fact it had in hand"))
        :segment '(:view :physical :cycle :none :ghosts :unknown)
        :sources '("probs/problem-crelay-topo.lisp DEFINE-INIT"
                   "Constraint-Static-Profile.txt S1 control table")))
    (recommend-ledger-search ledger 'lk4 :date "2026-09-20")
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger *crelay-ledger-path* "2026-09-20")
    (report-realization-ledger ledger)
    (report-search-recommendations ledger)
    (report-chain-replay ledger)
    ledger))


(ingest-runs)
