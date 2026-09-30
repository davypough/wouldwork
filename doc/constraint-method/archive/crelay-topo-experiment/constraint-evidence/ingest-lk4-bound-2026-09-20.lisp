;;; Ingests the FIRST EXHAUSTION (T10), and refines the decomposition the exhaustion exposed
;;; as under-specified.  Run from the repository root, standalone or in the ordinary image.
;;; It STAGES NOTHING, LOADS NO PROBLEM and RUNS NO SEARCH.
;;;
;;; TO REPRODUCE THE LEDGER FROM NOTHING, in this order:
;;;   build-realization-ledger-2026-09-20.lisp
;;;   ingest-lk1-2026-09-20.lisp
;;;   ingest-runs-2026-09-20.lisp
;;;   ingest-lk4-bound-2026-09-20.lisp
;;;
;;; WHAT D REPORTED.  The fourth milestone, at cutoff 8, serial: NO SOLUTIONS, 23.1 seconds,
;;; 9.25 GB consed, and the engine's own words -- "Milestone 4 search was truncated by the
;;; depth cutoff" -- so *DEPTH-CUTOFF-TRUNCATED* was true.  The committed chain through the
;;; third crossing is unchanged and nothing became a nogood.
;;;
;;; WHAT THE PROFILE ALREADY SAID, AND THE LEDGER DID NOT ASK.  S4 prints, for the device on
;;; this arc, that its controller is a SWITCH whose only manipulation reach candidate is a
;;; location in a DIFFERENT region; that the arc into that region is a GRAPH CUT IN THIS
;;; DIRECTION with the region a cul-de-sac; and that the device on THAT arc demands two
;;; supports, both APPROACH-ONLY and both KEEPER-OBLIGATED, at endpoints in the region the
;;; crossing starts from.  So the fourth milestone was never one crossing.  It bundled an
;;; excursion into a cul-de-sac, held open by two witnesses that cannot be the agent, and the
;;; return.  EIGHT ACTIONS WAS NEVER GOING TO COVER IT, AND THE SEARCH WAS NOT NEEDED TO KNOW
;;; THAT.  This is the movement-coverage premise S4 prints and pr4 carries, now bitten.

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


(defparameter *crelay-segment* '(:view :physical :cycle :unknown :ghosts :unknown))


(defun add-lk4-findings (ledger)
  "The two grade-1 premises the exhaustion sent us back to the profile to read.  Both were
   derivable before any search ran."
  (add-ledger-record ledger
    (make-ledger-premise 'pr15
      (ledger-collapse-whitespace
        "the switch controlling the fourth crossing's device has exactly ONE manipulation
         reach candidate, at an endpoint in a region that is not on the spine; that region is
         entered by one arc, which S4 marks a graph cut, and it reaches nothing else")
      '(:derived :grade 1 :by "S4's controller row for that device, and its directional closure")
      :premise-gaps (list (ledger-collapse-whitespace
                            "S4's switch walk descends AND, OR, NOT, IF and the queries those
                             call, so a single reach candidate is what the walk FOUND, which
                             is weaker than what exists"))
      :segment *crelay-segment*
      :sources '("Constraint-Static-Profile.txt S4, controller row and spine directional rows"))) 
  (add-ledger-record ledger
    (make-ledger-premise 'pr16
      (ledger-collapse-whitespace
        "the device guarding that region demands TWO distinct depressed supports, both at
         endpoints in the region the excursion starts from, both APPROACH-ONLY, and S4 marks
         both KEEPER-OBLIGATED while the aggregate is required -- so neither can be the agent,
         which has to be on the far side")
      '(:derived :grade 1 :by "S4's controller rows and its approach/departure classification")
      :premise-gaps (list (ledger-collapse-whitespace
                            "S4 prints permanent stranding for these supports as UNRESOLVED,
                             on movement and lifecycle coverage; nothing here closes it"))
      :segment *crelay-segment*
      :sources '("Constraint-Static-Profile.txt S4, the guarding device's controller rows"
                 "probs/problem-crelay-topo.lisp HAS-POSITION facts"
                 "Constraint-Abstract-Model.txt AM1, the tight budget")))
  ledger)


(defparameter *lk4-sublinks*
  '((lk7 4 "the two supports the guarding device demands are simultaneously held"
     (and (depressed plate4) (depressed plate5))
     "two witnesses, neither of them the agent, onto two supports in the region the excursion
      starts from.  This is where the budget bites; an exhaustion here is informative."
     ((pr16) (pr7) (pr4)) 8)
    (lk8 5 "the agent stands at the switch's only manipulation reach candidate"
     (has-location agent1 location14)
     "the excursion into the cul-de-sac, which is a graph cut and possible only while the
      guarding device is held open"
     ((pr15) (pr16) (pr4)) 6)
    (lk9 6 "the switch is thrown, so the fourth crossing's device is open"
     (switched-on switch2)
     "throwing it also CLOSES the device the third crossing used, which the agent is already
      past; that is the exclusion pair doing what S1 says it does"
     ((pr15) (pr5)) 4))
  "Id, chain order, statement, milestone goal, what is particular about it, dependency
   clauses, and a first cutoff.  The goals use relations S1 and the validated states have
   already shown to be state propositions -- DEPRESSED on a support, SWITCHED-ON on a switch
   -- rather than derived device states, so each is testable as printed.")


(defun add-lk4-sublinks (ledger)
  "Split the fourth crossing into the three milestones the profile says it contains, and put
   the original crossing after them in the chain.  IDS ARE APPEND-ONLY, so the new links are
   appended to the file; :CHAIN-ORDER is what puts them where they belong in the chain."
  (dolist (entry *lk4-sublinks*)
    (add-ledger-record ledger
      (make-ledger-link (first entry)
                        (ledger-collapse-whitespace (third entry))
                        '(:derived :grade 2 :by "S4's controller rows under the quotient's stated relaxation")
                        :from "the agent in the region the third crossing reached"
                        :to (ledger-collapse-whitespace (third entry))
                        :intent (ledger-collapse-whitespace (fifth entry))
                        :depends-on (sixth entry)
                        :premise-gaps (list (ledger-collapse-whitespace
                                              "this milestone exists because the one-link-per-arc
                                               decomposition was wrong, not because the spine
                                               says so; it is a refinement, and its necessity
                                               is not established"))
                        :segment *crelay-segment*
                        :search-goal (fourth entry)
                        :search-start :chain
                        :search-preamble :continue
                        :search-cutoff (seventh entry)
                        :chain-order (second entry)))
    (recommend-ledger-search ledger (first entry) :date "2026-09-20"))
  (dolist (pair '((lk4 . 7) (lk5 . 8) (lk6 . 9)))
    (ledger-set-value (ledger-record ledger (car pair)) :chain-order (cdr pair))
    (ledger-add-event (ledger-record ledger (car pair)) "2026-09-20" :amended
                      "renumbered in the chain after the fourth crossing was split"))
  ledger)


(defun ingest-lk4-bound ()
  (let ((ledger (read-realization-ledger *crelay-ledger-path*)))
    (ingest-ledger-result ledger 'lk4 :exhausted
                          :run "constraint-evidence/lk4-bound-8-2026-09-20.txt"
                          :truncated t
                          :pruning "graph mode with repeated-state pruning, as staged"
                          :nodes nil :seconds 23
                          :surprise
                          (ledger-collapse-whitespace
                            "does opening the device that labels a spine arc require traversal
                             OUTSIDE that arc, and if so into a region the spine does not
                             visit?")
                          :surprise-candidates '(:never :sometimes :for-this-device-yes)
                          :date "2026-09-20")
    (ledger-set-value (ledger-record ledger 'lk4) :premise-gaps
                      (list (ledger-collapse-whitespace
                              "this milestone bundled an excursion into a cul-de-sac and the
                               return; it has been split into three milestones that precede
                               it, and what remains here is the walk through the device once
                               the switch is thrown")))
    (add-lk4-findings ledger)
    (add-lk4-sublinks ledger)
    (ledger-set-value (ledger-record ledger 'lk4) :search-cutoff 4)
    (recommend-ledger-search ledger 'lk4 :date "2026-09-20")
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger *crelay-ledger-path* "2026-09-20")
    (report-realization-ledger ledger)
    (report-chain-replay ledger)
    (report-ledger-gap-candidates ledger)
    ledger))


(ingest-lk4-bound)
