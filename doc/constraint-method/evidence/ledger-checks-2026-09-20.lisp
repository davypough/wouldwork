;;; Isolated acceptance evidence for the realization ledger (T2).  From the repository root,
;;; run sbcl --noinform --no-userinit --no-sysinit --script <this-file>.
;;; Does not load Wouldwork, stage a problem, run an extractor or consult any prediction.
;;; The ledger reads no staged data by design, so a clean image is the whole environment it
;;; needs.  These cases are the acceptance criteria of T2 plus the guarantees that
;;; doc/constraint-method/Status-Algebra-and-Record-Schema.md claims for the schema.
;;;
;;; DISCLOSED STUB.  tech/constraint-ledger.lisp is plain Common Lisp that lives in the :WW
;;; package and interns generated ids there.  This script creates an EMPTY :WW package so the
;;; forms can be evaluated without Wouldwork.  Nothing of Wouldwork is loaded or shadowed, and
;;; the stub is a package definition and nothing else.

(defpackage :ww (:use :cl))
(defpackage :ledger-checks (:use :cl))
(in-package :ledger-checks)

(defparameter *checks* 0)


(defun check (condition)
  (incf *checks*)
  (assert condition))


(defun same-set (one other)
  (and (= (length one) (length other))
       (null (set-difference one other))
       (null (set-difference other one))))


(defmacro check-error (&body body)
  `(progn (incf *checks*)
          (assert (nth-value 1 (ignore-errors (progn ,@body t))))))

;; Every form of the source is evaluated, so the checks also establish that the whole file
;; reads and that no form is malformed.  Only the IN-PACKAGE is skipped, and *PACKAGE* is
;; bound to :WW around the read so the definitions land where loading the file would put them.
(let ((*package* (find-package :ww)))
  (with-open-file (in "tech/constraint-ledger.lisp")
    (loop for form = (read in nil :end) until (eq form :end)
          unless (and (consp form) (eq (first form) 'in-package))
            do (eval form))))

(defparameter *physical* '(:view :physical :cycle :none :ghosts :absent))
(defparameter *recording* '(:view :recording :cycle :open :ghosts :present))


(defun sample-ledger ()
  "The worked example of section 14: a grade-1 derivation, three guesses, a link resting on
   one of them through an alternative clause, a link resting on the derivation alone, and a
   link with an exhausted attempt against it."
  (let ((ledger (ww::make-realization-ledger "check" "2026-09-20")))
    (ww::add-ledger-record ledger
      (ww::make-ledger-premise 'ww::pr1 "three distinct witnesses are necessary"
                               '(:derived :grade 1 :by "control table and injective keying")
                               :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-question 'ww::qn1 "are ghosts present in this segment?"
                                :candidates '(:absent :present) :blocks '(ww::lk2)
                                :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-premise 'ww::pr3 "no ghost occupants exist in the segment"
                               '(:user-asserted :by "D" :asked-as ww::qn1 :date "2026-09-20")
                               :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-premise 'ww::pr4 "the agent occupies none of the required supports"
                               '(:user-asserted :by "D" :asked-as nil :date "2026-09-20")
                               :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-premise 'ww::pr7 "the agent is committed elsewhere for the segment"
                               '(:user-asserted :by "D" :asked-as nil :date "2026-09-20")
                               :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-link 'ww::lk2 "the three supports are simultaneously occupied"
                            '(:derived :grade 1 :by "injective matching over the stated pool")
                            :from "supports unoccupied" :to "supports occupied"
                            :intent "hold the aggregate open across the segment"
                            :depends-on '((ww::pr1) (ww::pr3) (ww::pr4 ww::pr7))
                            :premise-gaps '("necessity of this segment")
                            :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-link 'ww::lk5 "a step resting on the derivation alone"
                            '(:derived :grade 1 :by "the same table")
                            :from "a" :to "b" :intent "an independent step"
                            :depends-on '((ww::pr1)) :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-link 'ww::lk4 "a step a search was run for"
                            '(:derived :grade 1 :by "the graph")
                            :from "c" :to "d" :intent "cross the gap"
                            :depends-on '((ww::pr1) (ww::pr3)) :segment *physical*))
    (ww::add-ledger-record ledger
      (ww::make-ledger-bound 'ww::bd1 "no realization of lk4 was found within depth 6"
                             '(:search-measured :outcome :exhausted
                               :start-state "the staged state, described"
                               :search-expression "the subgoal chain as run"
                               :cutoff 6 :threads 0 :run "evidence/lk4-2026-09-20.txt")
                             :for-link 'ww::lk4 :measured '(:nodes 148203 :seconds 91)
                             :interpretation-committed "exhaustion establishes a depth-6 cost
                                                        bound and nothing more"
                             :depends-on '((ww::pr1) (ww::pr3)) :segment *physical*))
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :attempts '(ww::bd1))
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :status :answered)
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :answer :absent)
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :answer-premise 'ww::pr3)
    ledger))

;; 1  The sample ledger is well formed, which exercises all eight checks on a ledger holding
;;    every kind at once.
(check (ww::check-ledger-well-formed (sample-ledger)))

;; 2  Standing is computed, not stored.  A grade-1 derivation resting on nothing is
;;    established; a link resting on it alone is established; a link resting on a live guess
;;    is conditional however the guess reached the ledger.
(let ((ledger (sample-ledger)))
  (check (eq :established (ww::ledger-standing ledger 'ww::pr1)))
  (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk2)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk4)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::pr3)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::bd1))))

;; 3  The closure and the dependents set are exact.  This is the pair the retraction rule
;;    reads, so an error in either would make the cascade wrong in a way nothing else shows.
(let ((ledger (sample-ledger)))
  (check (same-set '(ww::pr1 ww::pr3 ww::pr4 ww::pr7) (ww::ledger-closure ledger 'ww::lk2)))
  (check (same-set '(ww::pr1) (ww::ledger-closure ledger 'ww::lk5)))
  (check (null (ww::ledger-closure ledger 'ww::pr1)))
  (check (same-set '(ww::lk2 ww::lk4 ww::bd1) (ww::ledger-dependents ledger 'ww::pr3)))
  (check (same-set '(ww::lk2) (ww::ledger-dependents ledger 'ww::pr4)))
  (check (same-set '(ww::lk2 ww::lk4 ww::lk5 ww::bd1)
                   (ww::ledger-dependents ledger 'ww::pr1))))

;; 4  Retracting a guess invalidates EXACTLY its dependents.  lk2 and lk4 each hold a clause
;;    that pr3 alone occupied; lk5 and pr1 never rested on it and are untouched.
(let ((ledger (sample-ledger)))
  (ww::retract-ledger-premise ledger 'ww::pr3 "D withdrew the ghost-absence guess"
                              "2026-09-21")
  (check (eq :retracted (getf (ww::ledger-record ledger 'ww::pr3) :status)))
  (check (eq :invalidated (getf (ww::ledger-record ledger 'ww::lk2) :status)))
  (check (eq :invalidated (getf (ww::ledger-record ledger 'ww::lk4) :status)))
  (check (eq :unfounded (ww::ledger-standing ledger 'ww::lk2)))
  (check (eq :open (getf (ww::ledger-record ledger 'ww::lk5) :status)))
  (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
  (check (eq :in-force (getf (ww::ledger-record ledger 'ww::pr1) :status)))
  (check (eq :in-force (getf (ww::ledger-record ledger 'ww::pr4) :status)))
  (check (eq :in-force (getf (ww::ledger-record ledger 'ww::pr7) :status))))

;; 5  A bound is never invalidated.  The measurement happened; what a retraction changes is
;;    whether the analysis still reaches the state it was measured from.
(let ((ledger (sample-ledger)))
  (ww::retract-ledger-premise ledger 'ww::pr3 "withdrawn" "2026-09-21")
  (check (eq :orphaned (getf (ww::ledger-record ledger 'ww::bd1) :status)))
  (check (equal '(:nodes 148203 :seconds 91)
                (getf (ww::ledger-record ledger 'ww::bd1) :measured))))

;; 6  Cascade is by empty clause, not by mention.  pr4 sits in the clause (pr4 pr7), so
;;    retracting it leaves lk2 standing on pr7 and the link is not invalidated.
(let ((ledger (sample-ledger)))
  (ww::retract-ledger-premise ledger 'ww::pr4 "withdrawn" "2026-09-21")
  (check (eq :retracted (getf (ww::ledger-record ledger 'ww::pr4) :status)))
  (check (eq :open (getf (ww::ledger-record ledger 'ww::lk2) :status)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk2)))
  (check (find :standing-changed (getf (ww::ledger-record ledger 'ww::lk2) :events)
               :key (lambda (event) (getf event :event)))))

;; 7  Retracting every disjunct of one clause does invalidate: the clause is then empty.
(let ((ledger (sample-ledger)))
  (ww::retract-ledger-premise ledger 'ww::pr4 "withdrawn" "2026-09-21")
  (ww::retract-ledger-premise ledger 'ww::pr7 "withdrawn too" "2026-09-21")
  (check (eq :invalidated (getf (ww::ledger-record ledger 'ww::lk2) :status)))
  (check (eq :unfounded (ww::ledger-standing ledger 'ww::lk2))))

;; 8  A retracted premise is never un-retracted, and a premise left with no surviving support
;;    is itself withdrawn rather than merely marked.
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr8 "a premise resting on the guess alone"
                             '(:derived :grade 2 :by "a derivation under it")
                             :depends-on '((ww::pr3)) :segment *physical*))
  (ww::retract-ledger-premise ledger 'ww::pr3 "withdrawn" "2026-09-21")
  (check (eq :retracted (getf (ww::ledger-record ledger 'ww::pr8) :status))))

;; 9  The discharge upgrade path.  A guess later derived is marked :DISCHARGED, standing looks
;;    through it, and no dependent is rewritten to make that happen.
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-link 'ww::lk6 "a step resting on the guess pr7 alone"
                          '(:derived :grade 1 :by "the table")
                          :from "e" :to "f" :intent "a step" :depends-on '((ww::pr7))
                          :segment *physical*))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk6)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr9 "the agent is committed elsewhere for the segment"
                             '(:derived :grade 2 :by "initial case and preservation")
                             :depends-on '((ww::pr1)) :segment *physical*))
  (ww::discharge-ledger-premise ledger 'ww::pr7 'ww::pr9 "derived at grade 2" "2026-09-21")
  (check (eq :discharged (getf (ww::ledger-record ledger 'ww::pr7) :status)))
  (check (eq :established (ww::ledger-standing ledger 'ww::lk6)))
  (check (equal '((ww::pr7)) (getf (ww::ledger-record ledger 'ww::lk6) :depends-on)))
  (check (ww::check-ledger-well-formed ledger)))

;; 10  An amendment may lower standing, which is correct behaviour, and it says so in an event.
(let ((ledger (sample-ledger)))
  (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
  (ww::amend-ledger-depends-on ledger 'ww::lk5 '(ww::pr3) "a later extractor added a premise"
                               "2026-09-21")
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk5)))
  (check (find :standing-changed (getf (ww::ledger-record ledger 'ww::lk5) :events)
               :key (lambda (event) (getf event :event)))))

;; 11  Ids are append-only and never reused.
(let ((ledger (sample-ledger)))
  (check (eq 'ww::pr8 (ww::ledger-next-id ledger "PR")))
  (check (eq 'ww::lk6 (ww::ledger-next-id ledger "LK")))
  (check (eq 'ww::bd2 (ww::ledger-next-id ledger "BD")))
  (check (eq 'ww::qn2 (ww::ledger-next-id ledger "QN")))
  (check-error (ww::add-ledger-record ledger
                 (ww::make-ledger-premise 'ww::pr1 "a reused id" '(:derived :grade 1 :by "x")
                                          :segment *physical*))))

;; 12  A bound closes nothing and refutes nothing.  This is guard X1 and WF8, tested on both
;;     fields, and it is the error that produced T22 being made unwritable.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :closed-by 'ww::bd1)
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :status :refuted)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :refuted-by 'ww::bd1)
  (check-error (ww::check-ledger-well-formed ledger)))

;; 13  A link is refuted only by a grade-1 or grade-2 derivation, and closed only when the
;;     composition validated (M4).
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :status :refuted)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :refuted-by 'ww::pr3)
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :status :closed)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :closed-by
                        '(:search-measured :outcome :found))
  (check-error (ww::check-ledger-well-formed ledger)))

;; 14  A found segment that has not been validated leaves the link conditional however
;;     established its premises are.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :status :realized)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :closed-by
                        '(:search-measured :outcome :found :start-state "s"
                          :search-expression "e" :cutoff 6 :threads 0 :run "f"))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk5)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :validated t)
  (check (eq :established (ww::ledger-standing ledger 'ww::lk5))))

;; 15  Grade 3 is the cost-bound grade and occurs nowhere else; no grade-2 record may rest on
;;     a search cutoff; a grade-4 record carries its own obligation.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :provenance
                        '(:derived :grade 3 :by "a search"))
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr11 "an induction resting on a cutoff"
                             '(:derived :grade 2 :by "initial case and preservation")
                             :depends-on '((ww::bd1)) :segment *physical*))
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-link 'ww::lk8 "an ordering claim with nothing outstanding"
                          '(:derived :grade 4 :by "a trace argument")
                          :from "g" :to "h" :intent "order two steps"
                          :depends-on '((ww::pr1)) :segment *physical*))
  (check-error (ww::check-ledger-well-formed ledger)))

;; 16  A user assertion is a premise and nothing else, and :SEGMENT :NONE needs a grade-1 or
;;     grade-2 derivation behind it.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :provenance
                        '(:user-asserted :by "D" :asked-as nil :date "2026-09-21"))
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-link 'ww::lk9 "a claim about every segment"
                          '(:derived :grade 4 :by "a trace argument")
                          :from "i" :to "j" :intent "a step" :depends-on '((ww::pr1))
                          :premise-gaps '("its own obligation") :segment :none))
  (check-error (ww::check-ledger-well-formed ledger)))

;; 17  A dependency across an incompatible segment needs an explicit bridging premise.  A
;;     ledger that composed a physical-view premise with a recording-view one silently would
;;     reproduce G14's defect one level up.
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr12 "a premise stated in the recording view"
                             '(:derived :grade 1 :by "the shadow relation")
                             :segment *recording*))
  (ww::amend-ledger-depends-on ledger 'ww::lk5 '(ww::pr12) "crossing views" "2026-09-21")
  (check-error (ww::check-ledger-well-formed ledger))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :segment-bridge
                        "stated bridge: the shadow agrees with the physical state here")
  (check (ww::check-ledger-well-formed ledger)))

;; 18  An answer creates a premise and never edits a standing; an answered question that does
;;     not point at one is ill-formed.
(let ((ledger (sample-ledger)))
  (check (eq :answered (getf (ww::ledger-record ledger 'ww::qn1) :status)))
  (check (eq :conditional (ww::ledger-standing ledger 'ww::lk2)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :answer-premise 'ww::pr4)
  (check-error (ww::check-ledger-well-formed ledger)))

;; 19  Guard X4: a bound may not say in prose what its type forbids it to mean.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::bd1) :statement
                        "the crossing is impossible")
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::bd1) :statement
                        "no route was found, so the region is unreachable")
  (check-error (ww::check-ledger-well-formed ledger)))

;; 20  Dangling references and cycles are caught rather than allowed to look like dead
;;     disjuncts.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :depends-on '((ww::pr99)))
  (check-error (ww::check-ledger-well-formed ledger)))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::pr1) :depends-on '((ww::lk5)))
  (check-error (ww::check-ledger-well-formed ledger)))

;; 21  Round trip.  The ledger goes out to a file and comes back EQUAL, including a key the
;;     reader does not recognise, which is what makes the file safely user-amendable.
(let ((ledger (sample-ledger))
      (path "doc/constraint-method/evidence/ledger-roundtrip-scratch.txt"))
  (let ((record (ww::ledger-record ledger 'ww::lk2)))
    (setf (cdr (last record)) (list :hand-note "an annotation the reader does not know")))
  (ww::write-realization-ledger ledger path "2026-09-20")
  (let ((back (ww::read-realization-ledger path)))
    (check (equal (getf ledger :records) (getf back :records)))
    (check (string= "check" (getf back :problem)))
    (check (eql 1 (getf back :version)))
    (check (equal "an annotation the reader does not know"
                  (getf (ww::ledger-record back 'ww::lk2) :hand-note)))
    (ww::write-realization-ledger back path "2026-09-20")
    (let ((again (ww::read-realization-ledger path)))
      (check (equal (getf back :records) (getf again :records)))))
  (delete-file path))

;; 22  The reporter prints open links, what blocks each, and what would close it, and prints
;;     cost bounds under their own heading with the fixed caveat.
(let* ((ledger (sample-ledger))
       (text (with-output-to-string (*standard-output*)
               (ww::report-realization-ledger ledger))))
  (check (search "OPEN LINKS" text))
  (check (search "blocking premises" text))
  (check (search "would close" text))
  (check (search "VALIDATE-ACTION-SEQUENCE" text))
  (check (search "COST BOUNDS  (GRADE 3 -- NOT IMPOSSIBILITY)" text))
  (check (search "licenses no" text))
  (check (search "no obligation, no necessity and no plan witness" text))
  (check (search "pr3" text))
  (check (search "a cost bound, not a refutation" text)))

;; 23  Only a premise is retracted, and only a derivation discharges one.
(let ((ledger (sample-ledger)))
  (check-error (ww::retract-ledger-premise ledger 'ww::lk5 "wrong kind" "2026-09-21"))
  (check-error (ww::discharge-ledger-premise ledger 'ww::pr7 'ww::pr3 "by a guess"
                                             "2026-09-21")))

;; 24  Answering a question writes a user-asserted premise, points the question at it, and
;;     leaves the question resting on it, so an answered question never reads ESTABLISHED
;;     while the answer is still a guess.  :UNKNOWN writes nothing.
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-question 'ww::qn2 "is the recorder cycle open in this segment?"
                              :candidates '(:open :closed) :blocks '(ww::lk5)
                              :segment *physical*))
  (check (eq :established (ww::ledger-standing ledger 'ww::qn2)))
  (ww::answer-ledger-question ledger 'ww::qn2 :closed "the recorder cycle is closed here" "D"
                              "2026-09-21")
  (let ((premise (ww::ledger-record ledger 'ww::pr8)))
    (check (eq :answered (getf (ww::ledger-record ledger 'ww::qn2) :status)))
    (check (eq 'ww::pr8 (getf (ww::ledger-record ledger 'ww::qn2) :answer-premise)))
    (check (eq :user-asserted (first (getf premise :provenance))))
    (check (eq 'ww::qn2 (ww::ledger-provenance-value (getf premise :provenance) :asked-as)))
    (check (eq :conditional (ww::ledger-standing ledger 'ww::qn2)))
    (check (ww::check-ledger-well-formed ledger))))
(let ((ledger (sample-ledger)))
  (ww::add-ledger-record ledger
    (ww::make-ledger-question 'ww::qn3 "a question left unanswered"
                              :candidates '(:yes :no) :segment *physical*))
  (ww::answer-ledger-question ledger 'ww::qn3 :unknown "" "D" "2026-09-21")
  (check (eq :open (getf (ww::ledger-record ledger 'ww::qn3) :status)))
  (check (null (getf (ww::ledger-record ledger 'ww::qn3) :answer-premise)))
  (check-error (ww::answer-ledger-question ledger 'ww::qn3 :maybe "not a candidate" "D"
                                           "2026-09-21")))

;; 25  T3.  A link carrying its search terms produces the exact commands, in order, with the
;;     settings after STAGE and *threads* before *depth-cutoff*.  The goal prints UNQUOTED,
;;     and a string inside the goal keeps its case.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-goal
                        '(ww::and (ww::loc ww::agent1 ww::location19) "Keep This Case"))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-start :chain)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-cutoff 6)
  (ww::recommend-ledger-search ledger 'ww::lk4 :date "2026-09-21")
  (let ((lines (ww::ledger-search-commands ledger (ww::ledger-record ledger 'ww::lk4) 6 0)))
    (check (equal (first lines) "(progn (ql:quickload :wouldwork) (in-package :ww))"))
    (check (equal (second lines) "(stage check)"))
    (check (equal (third lines) "(ww-set *threads* 0)"))
    (check (equal (fourth lines) "(ww-set *depth-cutoff* 6)"))
    (check (equal (fifth lines)
                  "(solve-subgoal (and (loc agent1 location19) \"Keep This Case\"))"))
    (check (= 5 (length lines)))))

;; 26  A stated start produces the two-argument form, and the extra caution that it discards
;;     an active chain.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal
                        '(ww::loc ww::agent1 ww::location19))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start
                        '((ww::loc ww::agent1 ww::location7)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-cutoff 4)
  (let ((lines (ww::ledger-search-commands ledger (ww::ledger-record ledger 'ww::lk5) 4 0)))
    (check (equal (fifth lines) "(solve-subgoal ((loc agent1 location7)) (loc agent1 location19))")))
  (check (= 6 (length (ww::ledger-search-cautions (ww::ledger-record ledger 'ww::lk5)))))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-start :chain)
  (check (= 5 (length (ww::ledger-search-cautions (ww::ledger-record ledger 'ww::lk4)))))
  (check (find-if (lambda (line) (search "*threads* 0 only" line))
                  (ww::ledger-search-cautions (ww::ledger-record ledger 'ww::lk4))))
  (check (find-if (lambda (line) (search "any thread mode" line))
                  (ww::ledger-search-cautions (ww::ledger-record ledger 'ww::lk5)))))

;; 27  NO SILENT DEEPENING.  bd1 measured lk4 to cutoff 6; recommending 8 signals unless the
;;     caller says :DEEPEN T, and the recommendation then names the bound it goes past.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-start :chain)
  (check (eq 'ww::bd1 (ww::ledger-deepest-attempt ledger 'ww::lk4)))
  (check-error (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 8 :date "2026-09-21"))
  (check (null (getf (ww::ledger-record ledger 'ww::lk4) :recommendation)))
  (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 6 :date "2026-09-21")
  (check (null (getf (getf (ww::ledger-record ledger 'ww::lk4) :recommendation) :deepens)))
  (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 8 :deepen t :date "2026-09-21")
  (check (equal '(ww::bd1 6)
                (getf (getf (ww::ledger-record ledger 'ww::lk4) :recommendation) :deepens))))

;; 28  An uncapped or missing cutoff is refused rather than defaulted: *depth-cutoff* 0 means
;;     no cutoff at all, so a recommendation carrying it would not be a bounded one.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (check-error (ww::recommend-ledger-search ledger 'ww::lk5 :date "2026-09-21"))
  (check-error (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 0 :date "2026-09-21"))
  (check-error (ww::recommend-ledger-search ledger 'ww::pr1 :cutoff 4 :date "2026-09-21")))

;; 29  The readings are written before the run and stored on the record, so T4 files a bound
;;     against the committed text rather than against a memory of it.  The exhaustion reading
;;     says grade 3 and denies impossibility in the same sentence.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 6 :date "2026-09-21")
  (let ((recommendation (getf (ww::ledger-record ledger 'ww::lk4) :recommendation)))
    (check (equal "2026-09-21" (getf recommendation :date)))
    (check (eql 6 (getf recommendation :cutoff)))
    (check (eql 0 (getf recommendation :threads)))
    (check (equal '(ww::pr3) (getf recommendation :guesses)))
    (check (search "GRADE-3 COST BOUND" (getf recommendation :exhaustion)))
    (check (search "not that lk4 is impossible" (getf recommendation :exhaustion)))
    (check (search "cannot close or refute a link" (getf recommendation :exhaustion)))
    (check (search "VALIDATE-ACTION-SEQUENCE" (getf recommendation :success)))
    (check (search "pr3" (getf recommendation :success)))
    (check (ww::check-ledger-well-formed ledger))))

;; 30  The recommender's report prints the commands, the cautions and both readings, and says
;;     plainly of a link that is not ready what it still needs instead of a half command.
(let* ((ledger (sample-ledger))
       (text (progn
               (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-goal
                                     '(ww::loc ww::a ww::b))
               (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :search-start :chain)
               (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 6 :date "2026-09-21")
               (with-output-to-string (*standard-output*)
                 (ww::report-search-recommendations ledger)))))
  (check (search "RECOMMENDED SEARCHES" text))
  (check (search "(stage check)" text))
  (check (search "(ww-set *depth-cutoff* 6)" text))
  (check (search "(solve-subgoal (loc a b))" text))
  (check (search "committed 2026-09-21, before the run" text))
  (check (search "GRADE-3 COST BOUND" text))
  (check (search "NO RECOMMENDATION COMMITTED" text))
  (check (search "a recommendation is not an approval to run it" text)))

;; 31  A link whose recommendation was committed but whose terms are incomplete prints what is
;;     missing, and never a command the user would have to edit.
(let* ((ledger (sample-ledger))
       (link (ww::ledger-record ledger 'ww::lk2))
       (text (progn (ww::ledger-set-value link :recommendation '(:date "2026-09-21"))
                    (with-output-to-string (*standard-output*)
                      (ww::report-ledger-recommendation ledger link)))))
  (check (search "NOT YET RUNNABLE" text))
  (check (search ":SEARCH-GOAL" text))
  (check (search ":SEARCH-START" text))
  (check (search ":SEARCH-CUTOFF" text))
  (check (not (search "solve-subgoal" text))))

;; 32  THREADS.  A link with its own start takes the configured default of 16; a chain link
;;     takes 0, because VALIDATE-CONTINUATION-PRECONDITIONS signals unless *threads* is 0.
;;     Nothing is coerced: asking for a parallel chain search signals.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start
                        '((ww::loc ww::a ww::c)))
  (check (eql 16 (ww::ledger-default-threads (ww::ledger-record ledger 'ww::lk5))))
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 4 :date "2026-09-21")
  (check (eql 16 (getf (getf (ww::ledger-record ledger 'ww::lk5) :recommendation) :threads)))
  (check (equal "(ww-set *threads* 16)"
                (third (ww::ledger-search-commands ledger (ww::ledger-record ledger 'ww::lk5)
                                                   4 16)))))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (check (eql 0 (ww::ledger-default-threads (ww::ledger-record ledger 'ww::lk5))))
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 4 :date "2026-09-21")
  (check (eql 0 (getf (getf (ww::ledger-record ledger 'ww::lk5) :recommendation) :threads)))
  (check-error (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 4 :threads 16
                                            :date "2026-09-21")))

;; 33  T4.  An outcome cannot be filed against a link with no committed recommendation: that
;;     would be a reading chosen after the outcome was known.
(let ((ledger (sample-ledger)))
  (check-error (ww::ingest-ledger-result ledger 'ww::lk2 :exhausted :run "r.txt"))
  (check-error (ww::ingest-ledger-result ledger 'ww::lk2 :found :actions '((ww::move ww::a))))) 

;; 34  An exhausted run files a grade-3 cost bound carrying the committed exhaustion reading,
;;     attaches it to the link as an ATTEMPT, and leaves the link open.  It closes nothing.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (let* ((committed (getf (getf (ww::ledger-record ledger 'ww::lk5) :recommendation) :exhaustion))
         (bound-id (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted
                                             :run "evidence/lk5-2026-09-21.txt"
                                             :truncated t :nodes 900 :seconds 12
                                             :date "2026-09-21"))
         (bound (ww::ledger-record ledger bound-id))
         (link (ww::ledger-record ledger 'ww::lk5)))
    (check (eq :bound (getf bound :kind)))
    (check (eq :standing (getf bound :status)))
    (check (equal committed (getf bound :interpretation-committed)))
    (check (eql 5 (ww::ledger-provenance-value (getf bound :provenance) :cutoff)))
    (check (eq :exhausted (ww::ledger-provenance-value (getf bound :provenance) :outcome)))
    (check (member bound-id (getf link :attempts)))
    (check (eq :open (getf link :status)))
    (check (null (getf link :closed-by)))
    (check (null (getf link :refuted-by)))
    (check (ww::check-ledger-well-formed ledger))))

;; 35  A find makes the link :REALIZED and only validation closes it.  Until it validates the
;;     standing stays CONDITIONAL however established its premises are.  That is M4 in code.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk5 :found :actions '((ww::move ww::a ww::b))
                            :run "evidence/lk5-found.txt" :date "2026-09-21")
  (let ((link (ww::ledger-record ledger 'ww::lk5)))
    (check (eq :realized (getf link :status)))
    (check (equal '((ww::move ww::a ww::b)) (getf link :evidence)))
    (check (null (getf link :validated)))
    (check (eq :conditional (ww::ledger-standing ledger 'ww::lk5)))
    (check (ww::check-ledger-well-formed ledger)))
  (ww::ingest-ledger-result ledger 'ww::lk5 :found :actions '((ww::move ww::a ww::b))
                            :validated t :run "evidence/lk5-found.txt" :date "2026-09-21")
  (let ((link (ww::ledger-record ledger 'ww::lk5)))
    (check (eq :closed (getf link :status)))
    (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
    (check (ww::check-ledger-well-formed ledger))))

;; 36  A find with no action sequence is refused: the sequence IS the evidence.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (check-error (ww::ingest-ledger-result ledger 'ww::lk5 :found :actions nil))
  (check-error (ww::ingest-ledger-result ledger 'ww::lk5 :timed-out)))

;; 37  M5.  A surprise is filed alongside the outcome as a schema-gap candidate, and the
;;     report prints it for hand-appending; nothing writes the problem's gap file.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :run "r.txt" :truncated nil
                            :surprise "does a support transition preserve occupancy across a
 cycle boundary?"
                            :surprise-candidates '(:yes :no :depends-on-view)
                            :date "2026-09-21")
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-ledger-gap-candidates ledger))))
    (check (search "SCHEMA-GAP CANDIDATES" text))
    (check (search "support transition" text))
    (check (search "BY HAND" text))
    (check (search "Nothing here writes to that file" text)))
  (check (ww::check-ledger-well-formed ledger)))

;; 38  The engine's own reading of how much an exhaustion covered is recorded, not assumed:
;;     A reliable NIL reports no observed truncation at cutoff nodes; it does not certify
;;     unpruned exhaustion. Unknown instrumentation has a separate reading (case 52).
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :run "r.txt" :truncated nil
                            :pruning "symmetry pruning and repeated-state pruning"
                            :date "2026-09-21")
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-realization-ledger ledger))))
    (check (search "no truncation observed at depth-cutoff nodes" text))
    (check (search "still not an impossibility" text))
    (check (search "pruning in force" text))
    (check (search "excludes solutions in pruned branches" text))))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :run "r.txt" :truncated t
                            :date "2026-09-21")
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-realization-ledger ledger))))
    (check (search "the cutoff truncated the space" text))))

;; 39  A second, deeper run against the same link files a second bound; neither replaces the
;;     other, and the link is still open.  A cost bound is never superseded by fiat.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 5 :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :run "a.txt" :truncated t
                            :date "2026-09-21")
  (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 9 :deepen t :date "2026-09-22")
  (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :run "b.txt" :truncated t
                            :date "2026-09-22")
  (let ((link (ww::ledger-record ledger 'ww::lk5)))
    (check (= 2 (length (getf link :attempts))))
    (check (eq :open (getf link :status)))
    (check (every (lambda (bound) (eq :standing (getf (ww::ledger-record ledger bound) :status)))
                  (getf link :attempts)))
    (check (ww::check-ledger-well-formed ledger))))

(defparameter *ro-scenario*
  '(:view :physical :cycle :none :ghosts :absent
    :device-conditions ((dev9 :active "RO1 audit"))
    :provenance "physical view, no ghosts, dev9 open, agent off those supports"))

;; 40  T5.  Every unresolved premise RO narrates comes out as a question with candidates and
;;     a default of :unknown.  Generation is idempotent: ids are append-only, so a duplicate
;;     could never be cleaned up afterwards.
(let* ((ledger (sample-ledger))
       (first-pass (ww::generate-ledger-questions ledger *ro-scenario* :blocks '(ww::lk2)
                                                  :supports '(s6 s7 s8) :pool '(b1 b2 b3)
                                                  :date "2026-09-21")))
  (check (= 9 (length first-pass)))
  (dolist (record first-pass)
    (check (eq :question (getf record :kind)))
    (check (eq :open (getf record :status)))
    (check (eq :unknown (getf record :default)))
    (check (getf record :template))
    (check (or (getf record :candidates) (eq :stated (getf record :answer-kind))))
    (check (equal '(ww::lk2) (getf record :blocks))))
  (let ((second-pass (ww::generate-ledger-questions ledger *ro-scenario* :blocks '(ww::lk2)
                                                    :supports '(s6 s7 s8) :pool '(b1 b2 b3)
                                                    :date "2026-09-21")))
    (check (= 9 (length second-pass)))
    (check (equal (mapcar (lambda (r) (getf r :id)) first-pass)
                  (mapcar (lambda (r) (getf r :id)) second-pass))))
  (check (ww::check-ledger-well-formed ledger)))

;; 41  A stated provenance suppresses the provenance question; a stated availability set
;;     suppresses the availability question; an undeclared device adds one per device.
(let ((ledger (sample-ledger)))
  (check (= 8 (length (ww::generate-ledger-questions
                        ledger (append '(:available-witnesses (b1 b2 b3)) *ro-scenario*)
                        :supports '(s6 s7 s8) :date "2026-09-21")))))
(let ((ledger (sample-ledger)))
  (check (= 11 (length (ww::generate-ledger-questions
                         ledger '(:view :physical :cycle :none :ghosts :absent)
                         :supports '(s6) :pool '(b1) :undeclared '(dev4)
                         :date "2026-09-21")))))

;; 42  ANTI-DRIFT.  Every template names the fragment of RO's printed text it stands for, and
;;     each fragment is still in tech/constraint-profile.lisp.  A change to RO's narration
;;     that this table has not followed fails here rather than going unnoticed.
(let ((profile (with-open-file (in "tech/constraint-profile.lisp")
                 (let ((text (make-string (file-length in))))
                   (subseq text 0 (read-sequence text in))))))
  (dolist (template ww::*ledger-question-templates*)
    (check (search (getf template :narrated-as) profile)))
  (dolist (fragment '("necessity of this segment" "ghost absence" "agent occupancy"
                      "replacement witnesses" "recorder transitions"))
    (check (search fragment profile))
    (check (find-if (lambda (template) (search fragment (getf template :narrated-as)))
                    ww::*ledger-question-templates*))))

;; 43  Answering writes a user-asserted premise naming the question, and the record the
;;     question blocks stays CONDITIONAL.  Nothing is upgraded, silently or otherwise.
(let* ((ledger (sample-ledger))
       (questions (ww::generate-ledger-questions ledger *ro-scenario* :blocks '(ww::lk5)
                                                 :supports '(s6 s7 s8) :pool '(b1 b2 b3)
                                                 :date "2026-09-21"))
       (ghosts (find :ghost-absence questions :key (lambda (r) (getf r :template))))
       (id (getf ghosts :id)))
  (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
  (check-error (ww::answer-ledger-question ledger id :maybe "not a candidate" "D" "2026-09-21"))
  (ww::answer-ledger-question ledger id :no-ghost-occupants-exist
                              "no ghost occupants exist in this segment" "D" "2026-09-21")
  (let* ((question (ww::ledger-record ledger id))
         (premise (ww::ledger-record ledger (getf question :answer-premise))))
    (check (eq :answered (getf question :status)))
    (check (eq :user-asserted (first (getf premise :provenance))))
    (check (eq id (ww::ledger-provenance-value (getf premise :provenance) :asked-as)))
    (check (eq :conditional (ww::ledger-standing ledger id)))
    (check (eq :established (ww::ledger-standing ledger 'ww::lk5)))
    (check (eq :open (getf (ww::ledger-record ledger 'ww::lk5) :status)))
    (ww::amend-ledger-depends-on ledger 'ww::lk5 (list (getf premise :id))
                                 "answer adopted as a premise of this link" "2026-09-21")
    (check (eq :conditional (ww::ledger-standing ledger 'ww::lk5)))
    (check (ww::check-ledger-well-formed ledger))))

;; 44  The three answer kinds validate as they should.  Some underdetermination is a choice,
;;     some is a subset of a pool, and some is a description nobody has written down.
(let* ((ledger (sample-ledger))
       (questions (ww::generate-ledger-questions ledger '(:view :physical :cycle :none
                                                          :ghosts :absent)
                                                 :supports '(s6) :pool '(b1 b2 b3)
                                                 :date "2026-09-21"))
       (availability (getf (find :availability questions
                                 :key (lambda (r) (getf r :template))) :id))
       (provenance (getf (find :segment-provenance questions
                               :key (lambda (r) (getf r :template))) :id)))
  (check (eq :subset-of (getf (ww::ledger-record ledger availability) :answer-kind)))
  (check (equal '(b1 b2 b3) (getf (ww::ledger-record ledger availability) :candidates)))
  (check-error (ww::answer-ledger-question ledger availability 'b1 "not a list" "D" "2026-09-21"))
  (check-error (ww::answer-ledger-question ledger availability '(b1 b9) "b9 is not in the pool"
                                           "D" "2026-09-21"))
  (ww::answer-ledger-question ledger availability '(b1 b3) "only b1 and b3 are free" "D"
                              "2026-09-21")
  (check (equal '(b1 b3) (getf (ww::ledger-record ledger availability) :answer)))
  (check (eq :stated (getf (ww::ledger-record ledger provenance) :answer-kind)))
  (check-error (ww::answer-ledger-question ledger provenance :some-keyword "not a string" "D"
                                           "2026-09-21"))
  (check-error (ww::answer-ledger-question ledger provenance "" "empty" "D" "2026-09-21"))
  (ww::answer-ledger-question ledger provenance "physical view, cycle closed, no ghosts"
                              "the segment as stated" "D" "2026-09-21")
  (check (eq :answered (getf (ww::ledger-record ledger provenance) :status)))
  (check (ww::check-ledger-well-formed ledger)))

;; 45  :UNKNOWN is the default and costs nothing: it writes no premise and leaves the question
;;     open, in every answer kind.
(let* ((ledger (sample-ledger))
       (questions (ww::generate-ledger-questions ledger *ro-scenario* :supports '(s6)
                                                 :pool '(b1) :date "2026-09-21")))
  (dolist (record questions)
    (ww::answer-ledger-question ledger (getf record :id) :unknown "" "D" "2026-09-21")
    (check (eq :open (getf (ww::ledger-record ledger (getf record :id)) :status)))
    (check (null (getf (ww::ledger-record ledger (getf record :id)) :answer-premise)))))

;; 46  The questionnaire prints each question enumerated, with the :unknown default and the
;;     exact call that answers it.  A question the reader must turn back into a command is
;;     prose again.
(let* ((ledger (sample-ledger))
       (questions (ww::generate-ledger-questions ledger *ro-scenario* :blocks '(ww::lk2)
                                                 :supports '(s6 s7 s8) :pool '(b1 b2 b3)
                                                 :date "2026-09-21"))
       (first-id (getf (first questions) :id)))
  (ww::answer-ledger-question ledger first-id :occurs-in-every-solution
                              "this segment occurs in every solution" "D" "2026-09-21")
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-ledger-questionnaire ledger))))
    (check (search "QUESTIONS" text))
    (check (search "AN ANSWER IS A PREMISE, NOT A FINDING" text))
    (check (search ":unknown  -- the default" text))
    (check (search "(answer-ledger-question ledger '" text))
    (check (search "why it matters:" text))
    (check (search "1. :no-ghost-occupants-exist" text))
    (check (search "blocks lk2" text))
    (check (search "ANSWERED (2)" text))
    (check (search "answering settles what rests on the answer" text))))

;; 47  A generated statement carries its problem terms as data and prints on one line, with no
;;     run of spaces left over from the template's own layout.
(let* ((ledger (sample-ledger))
       (questions (ww::generate-ledger-questions ledger *ro-scenario* :supports '(s6 s7 s8)
                                                 :pool '(b1 b2 b3) :date "2026-09-21"))
       (agent (find :agent-occupancy questions :key (lambda (r) (getf r :template))))
       (statement (getf agent :statement)))
  (check (search "(s6 s7 s8)" statement))
  (check (search "physical view, no ghosts" statement))
  (check (not (search "  " statement)))
  (check (not (find #\Newline statement)))
  (check (not (find #\Newline (first (getf agent :sources))))))

;; 48  A milestone that CONTINUES an open goal chain emits the SOLVE-SUBGOAL line alone.  The
;;     first real use of the recommender printed the staging preamble for every chained
;;     milestone, which would have told the reader to discard the chain being continued.
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-preamble :continue)
  (let ((lines (ww::ledger-search-commands ledger (ww::ledger-record ledger 'ww::lk5) 4 0))
        (cautions (ww::ledger-search-cautions (ww::ledger-record ledger 'ww::lk5))))
    (check (= 1 (length lines)))
    (check (equal "(solve-subgoal (loc a b))" (first lines)))
    (check (find-if (lambda (line) (search "do NOT re-stage" line)) cautions))
    (check (find-if (lambda (line) (search "CONTINUES an open goal chain" line)) cautions))
    (check (notany (lambda (line) (search "(stage " line)) lines))))
(let ((ledger (sample-ledger)))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (check (eq :stage (getf (ww::ledger-record ledger 'ww::lk5) :search-preamble)))
  (check (= 5 (length (ww::ledger-search-commands ledger (ww::ledger-record ledger 'ww::lk5)
                                                  4 0)))))

;; 49  A recommendation says WHERE a find leaves its action sequence, and the three cases are
;;     genuinely different.  A mid-chain milestone sets *solution-paths* to NIL by design; its
;;     path is in the session's phase record.  Only the final milestone publishes a cumulative
;;     path.  The method lost a command to this the first time it mattered.
(let ((ledger (sample-ledger))
      (link nil))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
  (setf link (ww::ledger-record ledger 'ww::lk5))
  (let ((lines (ww::ledger-evidence-retrieval link)))
    (check (find-if (lambda (line) (search "*solution-paths* is NIL by design" line)) lines))
    (check (find-if (lambda (line) (search "goal-chain-session-phases" line)) lines))
    (check (find-if (lambda (line) (search "the chain survives it" line)) lines)))
  (ww::ledger-set-value link :search-final t)
  (let ((lines (ww::ledger-evidence-retrieval link)))
    (check (find-if (lambda (line) (search "FINAL milestone" line)) lines))
    (check (find-if (lambda (line) (search "cumulative path" line)) lines))
    (check (notany (lambda (line) (search "NIL by design" line)) lines)))
  (ww::ledger-set-value link :search-final nil)
  (ww::ledger-set-value link :search-start '((ww::loc ww::a ww::c)))
  (let ((lines (ww::ledger-evidence-retrieval link)))
    (check (find-if (lambda (line) (search "stated start" line)) lines))
    (check (notany (lambda (line) (search "goal-chain-session" line)) lines))))

;; 50  A goal chain is session state and does not survive a REPL restart, so the replay block
;;     prints every chain milestone in order, marked with what is already settled.  The
;;     per-link recommendation prints only the next line, which is right in a live session and
;;     useless after a restart.
(let ((ledger (sample-ledger)))
  (dolist (id '(ww::lk2 ww::lk4 ww::lk5))
    (ww::ledger-set-value (ww::ledger-record ledger id) :search-start :chain)
    (ww::ledger-set-value (ww::ledger-record ledger id) :search-goal
                          (list 'ww::at 'ww::agent1 id))
    (ww::ledger-set-value (ww::ledger-record ledger id) :search-cutoff 8))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk2) :status :closed)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk2) :validated t)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk2) :closed-by
                        '(:derived :grade 1 :by "for the check"))
  (check (= 3 (length (ww::ledger-chain-links ledger))))
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-chain-replay ledger))))
    (check (search "CHAIN REPLAY" text))
    (check (search "does not survive a REPL restart" text))
    (check (search "(stage check)" text))
    (check (search "(ww-set *depth-cutoff* 8)" text))
    (check (search "(solve-subgoal (at agent1 lk2))   ; closed" text))
    (check (search "(solve-subgoal (at agent1 lk4))" text))
    (check (search "1 of 3 milestones are already settled" text))))

;; 51  CHAIN ORDER IS NOT FILE ORDER once a decomposition is refined.  A link inserted to
;;     split an earlier one is appended to the file but belongs before it in the chain, and
;;     the replay must follow the chain or it re-runs the milestones in the wrong order.
(let ((ledger (sample-ledger)))
  (dolist (pair '((ww::lk2 . 3) (ww::lk4 . 1) (ww::lk5 . 2)))
    (ww::ledger-set-value (ww::ledger-record ledger (car pair)) :search-start :chain)
    (ww::ledger-set-value (ww::ledger-record ledger (car pair)) :search-goal
                          (list 'ww::at (car pair)))
    (ww::ledger-set-value (ww::ledger-record ledger (car pair)) :chain-order (cdr pair)))
  (check (equal '(ww::lk4 ww::lk5 ww::lk2)
                (mapcar (lambda (r) (getf r :id)) (ww::ledger-chain-links ledger))))
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-chain-replay ledger))))
    (check (< (search "(at lk4)" text) (search "(at lk5)" text)))
    (check (< (search "(at lk5)" text) (search "(at lk2)" text)))))

;; 52  Missing or unreliable instrumentation must never become a completeness claim.
(dolist (mode '(:explicit :default))
  (let ((ledger (sample-ledger)))
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-goal '(ww::loc ww::a ww::b))
    (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk5) :search-start :chain)
    (ww::recommend-ledger-search ledger 'ww::lk5 :cutoff 8 :date "2026-09-21")
    (let* ((id (if (eq mode :explicit)
                 (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted :truncated :unknown)
                 (ww::ingest-ledger-result ledger 'ww::lk5 :exhausted)))
           (bound (ww::ledger-record ledger id))
           (text (with-output-to-string (*standard-output*)
                   (ww::report-ledger-bound-strength bound))))
      (check (eq :unknown (ww::ledger-provenance-value (getf bound :provenance) :cutoff-truncated)))
      (check (search "cutoff coverage UNKNOWN" text))
      (check (not (search "the cutoff truncated the space" text)))
      (check (not (search "no truncation observed" text)))
      (check (eq :open (getf (ww::ledger-record ledger 'ww::lk5) :status)))
      (check (ww::check-ledger-well-formed ledger)))))
(check-error (ww::ledger-run-provenance nil :exhausted nil :invalid nil))

(format t "~&Interactive-phase acceptance checks passed: ~D.  All source forms read and evaluated.~%"
        *checks*)
