(defpackage :ww (:use :cl))
(defpackage :ledger-demo (:use :cl))
(in-package :ledger-demo)
(let ((*package* (find-package :ww)))
  (with-open-file (in "tech/constraint-ledger.lisp")
    (loop for form = (read in nil :end) until (eq form :end)
          unless (and (consp form) (eq (first form) 'in-package))
            do (eval form))))
(defparameter *physical* '(:view :physical :cycle :none :ghosts :absent))
(let ((ledger (ww::make-realization-ledger "crelay-topo" "2026-09-20")))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr1 "the normal aggregate requires three distinct supports depressed, and occupancy is keyed by occupant, so three distinct witnesses are necessary"
      '(:derived :grade 1 :by "S1 control table; S2 injective keying")
      :segment *physical* :sources '("CONTROLS clauses and polarity" "ON's functional keying")))
  (ww::add-ledger-record ledger
    (ww::make-ledger-premise 'ww::pr3 "no ghost occupants exist in the segment under analysis"
      '(:user-asserted :by "D" :asked-as ww::qn1 :date "2026-09-20") :segment *physical*))
  (ww::add-ledger-record ledger
    (ww::make-ledger-question 'ww::qn1 "are ghosts present in the segment under analysis?"
      :candidates '(:absent :present) :blocks '(ww::lk2) :segment *physical*))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :status :answered)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :answer :absent)
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::qn1) :answer-premise 'ww::pr3)
  (ww::add-ledger-record ledger
    (ww::make-ledger-link 'ww::lk2 "the three supports are simultaneously occupied"
      '(:derived :grade 1 :by "injective matching over the stated pool")
      :from "the three supports unoccupied" :to "the three supports simultaneously occupied"
      :intent "hold the aggregate open across the segment"
      :depends-on '((ww::pr1) (ww::pr3)) :segment *physical*
      :premise-gaps '("necessity of this segment" "replacement witnesses" "recorder transitions")))
  (ww::add-ledger-record ledger
    (ww::make-ledger-link 'ww::lk4 "cross from the approach side to the far side"
      '(:derived :grade 1 :by "the verified spine")
      :from "the actor on the approach side" :to "the actor on the far side"
      :intent "cross the gap the spine requires" :depends-on '((ww::pr1)) :segment *physical*
      :search-goal '(ww::and (ww::has-location ww::agent1 ww::location19))
      :search-start :chain :search-cutoff 6))
  (ww::add-ledger-record ledger
    (ww::make-ledger-bound 'ww::bd1 "no realization of lk4 was found within depth 6 from the stated start state"
      '(:search-measured :outcome :exhausted :start-state "the staged state after the aggregate opens"
        :search-expression "(solve-subgoal ...) with *threads* 0" :cutoff 6 :threads 0
        :run "constraint-evidence/lk4-bound-2026-09-21.txt")
      :for-link 'ww::lk4 :measured '(:nodes 148203 :seconds 91)
      :interpretation-committed "success would close lk4 with its action sequence, subject to validation; exhaustion would establish a depth-6 cost bound relative to this start state and nothing more"
      :depends-on '((ww::pr1)) :segment *physical*))
  (ww::ledger-set-value (ww::ledger-record ledger 'ww::lk4) :attempts '(ww::bd1))
  (ww::recommend-ledger-search ledger 'ww::lk4 :cutoff 8 :deepen t :date "2026-09-21")
  (ww::ingest-ledger-result ledger 'ww::lk4 :exhausted
    :run "constraint-evidence/lk4-bound-8-2026-09-21.txt" :truncated t
    :pruning "repeated-state pruning in graph mode" :nodes 981204 :seconds 640
    :surprise "does a support transition preserve occupancy across a cycle boundary?"
    :surprise-candidates '(:yes :no :depends-on-the-view) :date "2026-09-21")
  (ww::generate-ledger-questions ledger
    '(:view :physical :cycle :none :ghosts :absent
      :provenance "physical view, no ghosts, the aggregate open, agent off those supports")
    :blocks '(ww::lk2) :supports '(ww::s6 ww::s7 ww::s8)
    :pool '(ww::b1 ww::b2 ww::b3 ww::b4) :date "2026-09-21")
  (ww::check-ledger-well-formed ledger)
  (ww::report-realization-ledger ledger)
  (ww::report-search-recommendations ledger)
  (ww::report-ledger-gap-candidates ledger)
  (ww::report-ledger-questionnaire ledger))
