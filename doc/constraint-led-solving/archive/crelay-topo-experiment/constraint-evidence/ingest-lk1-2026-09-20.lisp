;;; Ingests the FIRST MEASURED RUN of the constraint-led method (T10).  Run from the
;;; repository root, standalone or in the ordinary image:
;;;
;;;   sbcl --noinform --no-userinit --no-sysinit --script \
;;;     doc/problems/crelay-topo/constraint-evidence/ingest-lk1-2026-09-20.lisp
;;;
;;; It STAGES NOTHING, LOADS NO PROBLEM and RUNS NO SEARCH.  It reads the ledger, files the
;;; outcome D reported against the reading committed BEFORE that run, and writes the ledger
;;; back.
;;;
;;; THE RUN.  D ran the recommended first probe verbatim on lumpy: staged, *threads* 0,
;;; *depth-cutoff* 8, the chained milestone for the first crossing.  It FOUND a realization
;;; at depth 4 and stopped at a recorder subgoal checkpoint.
;;;
;;; WHAT THE EVIDENCE IS, AND IS NOT.  :EVIDENCE below is the run's PRINTED TRANSCRIPT, not
;;; the raw action list.  VALIDATE-ACTION-SEQUENCE needs the raw list, so this link is
;;; :REALIZED and NOT :CLOSED, and it stays that way until the raw path is validated from the
;;; staged start state.  That is M4 and it is not a formality here: the transcript is a
;;; display format and has not been replayed.

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


(defparameter *lk1-transcript*
  '("recording phase: (start-recorder) by the live agent"
    "recording phase: the GHOST agent walks from its start location to the support's location"
    "recording phase: the GHOST agent steps from ground onto the required support"
    "recording phase: (pause)"
    "recording phase: the GHOST agent walks back off the support's location"
    "recording phase: (stop-recorder)"
    "playback phase: the LIVE agent walks through the now-open device into the target region"
    "playback phase: (resume)")
  "The run's printed transcript, stated domain-generally.  It is a DISPLAY FORMAT, not the raw
   action list VALIDATE-ACTION-SEQUENCE replays, which is why this link is :REALIZED and not
   :CLOSED.")


(defun ingest-lk1 ()
  (let ((ledger (read-realization-ledger *crelay-ledger-path*)))
    (ingest-ledger-result ledger 'lk1 :found
                          :actions *lk1-transcript*
                          :validated nil
                          :run "constraint-evidence/lk1-first-run-2026-09-20.txt"
                          :nodes nil :seconds nil
                          :date "2026-09-20")
    (ledger-set-value (ledger-record ledger 'lk1) :premise-gaps
                      (append (getf (ledger-record ledger 'lk1) :premise-gaps)
                              (list (ledger-collapse-whitespace
                                      "the raw action list must replace the transcript in
                                       :EVIDENCE before VALIDATE-ACTION-SEQUENCE can replay
                                       it; until then this link is realized, not closed"))))
    (add-ledger-record ledger
      (make-ledger-premise 'pr11
        (ledger-collapse-whitespace
          "at the checkpoint this realization reaches, a recorder cycle is OPEN, all four
           ghost occupants have locations, and two further devices on the spine are already
           open without any action having been spent on them")
        (list :search-measured :outcome :found
              :start-state "the staged start state"
              :search-expression "the recommended first probe for the first crossing"
              :cutoff 8 :threads 0
              :run "constraint-evidence/lk1-first-run-2026-09-20.txt")
        :depends-on '((lk1))
        :premise-gaps (list (ledger-collapse-whitespace
                              "this describes the end state of ONE realization, not a property
                               of every realization of this crossing"))
        :segment '(:view :physical :cycle :open :ghosts :present)
        :sources '("the run's reported checkpoint state")))
    (add-ledger-record ledger
      (make-ledger-premise 'pr12
        (ledger-collapse-whitespace
          "a support can be held by a GHOST of the agent produced by a recorder fork while the
           live agent crosses, so holding a support does not have to spend a live occupant")
        (list :search-measured :outcome :found
              :start-state "the staged start state"
              :search-expression "the recommended first probe for the first crossing"
              :cutoff 8 :threads 0
              :run "constraint-evidence/lk1-first-run-2026-09-20.txt")
        :depends-on '((lk1))
        :premise-gaps (list (ledger-collapse-whitespace
                              "observed once, in one crossing.  It is not established that the
                               same mechanism is available at the other crossings, whose
                               supports lie in different regions and whose cycle budget is
                               shared with this one"))
        :segment '(:view :physical :cycle :open :ghosts :present)
        :sources '("the run's reported recording and playback phases")))
    ;; NO DEPENDENCY IS ADDED FROM A DOWNSTREAM LINK TO LK1 OR TO PR11, AND THE FIRST ATTEMPT
    ;; TO ADD ONE WAS REFUSED BY WF9.  It is worth recording why, because the refusal was
    ;; right and the mistake was natural.  A chained milestone's SEARCH starts from its
    ;; predecessor's end state, but its CLAIM -- that the spine requires this crossing -- is a
    ;; graph fact that does not depend on how the previous crossing happened to be realized.
    ;; Writing the chaining into :DEPENDS-ON would have made a grade-2 claim rest on a search
    ;; result, which is exactly what "no grade-2 proof may depend on a search cutoff" forbids.
    ;; The chaining is operational and is already carried by :SEARCH-START :CHAIN.
    (recommend-ledger-search ledger 'lk2 :date "2026-09-20")
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger *crelay-ledger-path* "2026-09-20")
    (report-realization-ledger ledger)
    (report-search-recommendations ledger)
    ledger))


(ingest-lk1)
