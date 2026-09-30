;;; Builds the crelay-topo realization ledger (T10, first half).  Run from the repository
;;; root, either standalone --
;;;
;;;   sbcl --noinform --no-userinit --no-sysinit --script \
;;;     doc/problems/crelay-topo/constraint-evidence/build-realization-ledger-2026-09-20.lisp
;;;
;;; -- or in the ordinary image after (ql:quickload :wouldwork), where it reuses the :WW
;;; package and the already-loaded ledger.  It STAGES NOTHING, LOADS NO PROBLEM and RUNS NO
;;; SEARCH; it only writes the ledger file and prints the questionnaire and recommendations.
;;;
;;; WHERE EVERY CLAIM BELOW COMES FROM, and nowhere else:
;;;   probs/problem-crelay-topo.lisp        DEFINE-GOAL and DEFINE-INIT, the authored instance
;;;   Constraint-Static-Profile.txt         S1's control table, S2's pools, S3's regions and
;;;                                         adjacency spine, S4's scope
;;;   Constraint-Abstract-Model.txt         AM1-AM6, and the approach-side correction
;;;   Constraint-Role-Obligations.txt       RO's unresolved premises, via T5's templates
;;; NO C1-SEALED FILE WAS OPENED.  Not Backward-*, not Forward-*, not Initial-Conditions.txt,
;;; and not subgoal-solution-*, which is sealed always.  The initial location and the goal are
;;; read from the INSTANCE, which C1 names as a permitted source, not from any analysis of it.

(unless (find-package :ww)
  (defpackage :ww (:use :cl)))

(unless (fboundp 'ww::make-realization-ledger)
  (let ((*package* (find-package :ww)))
    (with-open-file (in "tech/constraint-ledger.lisp")
      (loop for form = (read in nil :end) until (eq form :end)
            unless (and (consp form) (eq (first form) 'in-package))
              do (eval form)))))

(in-package :ww)


(defparameter *crelay-ledger-path* "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")


(defparameter *crelay-segment* '(:view :physical :cycle :none :ghosts :unknown)
  "Ghost presence is :UNKNOWN on purpose.  Whether a recorder fork is needed to hold a support
   is one of the things this loop exists to settle, and stating :ABSENT would settle it by
   assumption.")


(defparameter *crelay-crossings*
  '((lk1 "R1" "R8" gate1
     (or (has-location agent1 location20) (has-location agent1 location3)
         (has-location agent1 location4) (has-location agent1 location5)
         (has-location agent1 location7))
     "the first spine crossing, and the only one whose start state is the staged one")
    (lk2 "R8" "R2" gate3
     (or (has-location agent1 location10) (has-location agent1 location8)
         (has-location agent1 location9))
     "crosses the support S3 places at an endpoint of R8")
    (lk3 "R2" "R3" gate5
     (or (has-location agent1 location11) (has-location agent1 location12)
         (has-location agent1 location13))
     "zero body cost; gate5 is one polarity of the switch pair")
    (lk4 "R3" "R5" gate7
     (has-location agent1 location15)
     "zero body cost; gate7 is the OTHER polarity of the same switch pair as gate5")
    (lk5 "R5" "R6" gate8
     (or (has-location agent1 location16) (has-location agent1 location17)
         (has-location agent1 location18) (has-location agent1 location21))
     "the beam crossing; S1 places gate8 at depth 2 and does not decide its supplier")
    (lk6 "R6" "R7" gate9
     (has-location agent1 location19)
     "the last crossing, and its goal is the problem's own goal, so (solve) may finish the
      chain in place of this milestone"))
  "One entry per spine crossing between R1 and R7, in spine order: id, source region, target
   region, the device labelling the arc, the milestone goal as a disjunction over the target
   region's endpoints, and what is particular about it.  A DISJUNCTION rather than a chosen
   representative endpoint, because S3 warns that two endpoints of one region need not be
   mutually reachable -- picking one would smuggle in a premise the quotient does not carry.")


(defparameter *crelay-premises*
  '((pr1 1 nil
     "the problem's goal places the agent at the sole endpoint of the last region of the spine"
     "the instance's DEFINE-GOAL against S3's regions"
     ("probs/problem-crelay-topo.lisp DEFINE-GOAL"
      "Constraint-Static-Profile.txt S3 regions")
     nil)
    (pr2 1 nil
     "the agent's initial location lies in the first region of the spine"
     "the instance's DEFINE-INIT against S3's regions"
     ("probs/problem-crelay-topo.lisp DEFINE-INIT"
      "Constraint-Static-Profile.txt S3 regions")
     nil)
    (pr3 1 nil
     "the region quotient relaxes all other doors, elevation and cargo conditions, so a route
      through it is a candidate and not a plan"
     "S3's contraction NOTE and S4's stated scope"
     ("Constraint-Static-Profile.txt S3 NOTE and S4 SCOPE"
      "Constraint-Abstract-Model.txt AM5, AM6")
     nil)
    (pr4 2 ((pr3))
     "the adjacency spine joins the first region to the last only through five intermediate
      regions, crossing six controlled devices in one fixed order"
     "S3's adjacency spine, checked by S4 against full-quotient reachability"
     ("Constraint-Static-Profile.txt S3 adjacency spine"
      "Constraint-Abstract-Model.txt AM5")
     ("that all relevant concrete movement is covered by the route model -- S4 prints this
       premise as missing"))
    (pr5 1 nil
     "two of the six devices are controlled by opposite polarities of one switch and are
      therefore never open together"
     "S1's exclusion pairs"
     ("Constraint-Static-Profile.txt S1 control pairs"
      "Constraint-Abstract-Model.txt AM2.1, AM6")
     nil)
    (pr6 1 nil
     "ON is keyed by the occupant, so the map from an occupied support to its witness is
      injective"
     "S2's functional-relation census"
     ("Constraint-Static-Profile.txt S2 functional relations")
     nil)
    (pr7 2 ((pr6))
     "support occupancy is layer-blind, so distinct required supports need distinct witnesses
      and a ghost counts as one"
     "S2's consumer classification over the eight-member pool"
     ("Constraint-Static-Profile.txt S2 occupancy pools"
      "Constraint-Abstract-Model.txt AM1, section 2.2")
     nil)
    (pr8 1 nil
     "two of the six devices each require one depressed support, at endpoints the quotient
      places in the region the crossing starts from"
     "S1's control table against the instance's authored support positions and S3's regions"
     ("Constraint-Static-Profile.txt S1 control table"
      "probs/problem-crelay-topo.lisp HAS-POSITION facts"
      "Constraint-Static-Profile.txt S3 regions")
     nil)
    (pr9 1 nil
     "the last device requires three distinct depressed supports, all at endpoints the
      quotient places on the APPROACH side of its crossing"
     "S1's control table against the instance's positions and S3's regions"
     ("Constraint-Static-Profile.txt S1 control table"
      "probs/problem-crelay-topo.lisp HAS-POSITION facts"
      "Constraint-Abstract-Model.txt section 6, approach-side correction")
     nil)
    (pr10 1 nil
     "one of the six devices sits at control depth 2 behind a receiver, and the identity of the
      device supplying it is not decidable from the control algebra"
     "S1, which says so at the claim site"
     ("Constraint-Static-Profile.txt S1 control DAG"
      "Constraint-Abstract-Model.txt AM2.3, AM8")
     ("which device supplies the receiver; S5 and S6 are not written")))
  "Id, grade, dependency clauses, statement, what derived it, sources, and premise gaps.  Every
   statement is written across lines for readability and collapsed to one line on the way in.")


(defun add-crelay-premises (ledger)
  "The premises the chain rests on, each from a permitted source and each carrying its grade."
  (dolist (entry *crelay-premises* ledger)
    (add-ledger-record ledger
      (make-ledger-premise (first entry)
                           (ledger-collapse-whitespace (fourth entry))
                           (list :derived :grade (second entry)
                                 :by (ledger-collapse-whitespace (fifth entry)))
                           :depends-on (third entry)
                           :premise-gaps (mapcar #'ledger-collapse-whitespace (seventh entry))
                           :segment *crelay-segment*
                           :sources (sixth entry)))))


(defun crelay-crossing-depends-on (device)
  "What one crossing rests on: the spine always, plus the premise about its own device."
  (case device
    ((gate1) '((pr2) (pr4) (pr8) (pr7)))
    ((gate3) '((pr4) (pr8) (pr7)))
    ((gate5 gate7) '((pr4) (pr5)))
    ((gate8) '((pr4) (pr10)))
    ((gate9) '((pr1) (pr4) (pr9) (pr7)))))


(defun crelay-crossing-gaps (device)
  "What each crossing does not yet know, stated at the claim site rather than in prose."
  (case device
    ((gate1 gate3) '("whether a witness can be put on the required support without spending
                      the agent, which the budget says costs one of eight occupants"))
    ((gate5 gate7) '("the order in which the switch is thrown, since the two crossings need
                      opposite polarities and no extractor supplies an ordering"))
    ((gate8) '("whether a relay is required at all, and whether a relay role can share a
                support with a pressure role"))
    ((gate9) '("whether three witnesses can be present on the approach side simultaneously,
                which the budget makes tight and RO reports as conditional"))))


(defun add-crelay-links (ledger)
  "One link per spine crossing.  Each is chained: its start is the previous milestone's end
   state, which is what the engine's goal chain already maintains."
  (dolist (crossing *crelay-crossings* ledger)
    (let ((id (first crossing))
          (source (second crossing))
          (target (third crossing))
          (device (fourth crossing))
          (goal (fifth crossing))
          (note (sixth crossing)))
      (add-ledger-record ledger
        (make-ledger-link id
                          (ledger-collapse-whitespace
                            (format nil "the agent crosses from ~A to ~A through the device
                                         labelling that spine arc" source target))
                          '(:derived :grade 2 :by "the S3 adjacency spine under its stated relaxation")
                          :from (format nil "the agent somewhere in ~A" source)
                          :to (format nil "the agent somewhere in ~A" target)
                          :intent (ledger-collapse-whitespace
                                    (format nil "cross ~A -> ~A: ~A" source target note))
                          :depends-on (crelay-crossing-depends-on device)
                          :premise-gaps (mapcar #'ledger-collapse-whitespace
                                                (crelay-crossing-gaps device))
                          :segment *crelay-segment*
                          :search-goal goal
                          :search-start :chain
                          :search-preamble (if (eq id 'lk1) :stage :continue)
                          :search-final (eq id 'lk6)
                          :chain-order (1+ (position crossing *crelay-crossings*))
                          :search-cutoff 8
                          :sources (list "Constraint-Static-Profile.txt S3 adjacency spine"
                                         (format nil "device labelling the ~A-~A arc: ~(~A~)"
                                                 source target device)))))))


(defun build-crelay-ledger ()
  "Builds the ledger, recommends the first probe for every crossing, generates the questions
   the first crossing is blocked by, writes the file and prints the three reports."
  (let ((ledger (make-realization-ledger "crelay-topo" "2026-09-20")))
    (add-crelay-premises ledger)
    (add-crelay-links ledger)
    (dolist (crossing *crelay-crossings*)
      (recommend-ledger-search ledger (first crossing) :date "2026-09-20"))
    (generate-ledger-questions ledger
      (list :view :physical :cycle :none :ghosts :unknown
            :provenance "the physical view, outside any recorder cycle, crossing the first
                         spine arc from the staged start state"
            :device-conditions '((gate1 :active "the first spine crossing")))
      :blocks '(lk1)
      :supports '(plate1)
      :pool '(agent1 agent1* box1 box1* connector1 connector1* tray1 tray1*)
      :date "2026-09-20")
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger *crelay-ledger-path* "2026-09-20")
    (report-realization-ledger ledger)
    (report-search-recommendations ledger)
    (report-ledger-questionnaire ledger)
    ledger))


(build-crelay-ledger)
