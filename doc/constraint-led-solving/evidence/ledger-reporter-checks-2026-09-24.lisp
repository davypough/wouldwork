;;; Acceptance evidence for T14 of doc/constraint-method/Constraint-Implementation-Plan.md:
;;; the ledger's search recommendations brought up to date with the standalone checkpoint
;;; workflow and the corrected exhaustion wording.  From the repository root, run
;;;   sbcl --noinform --no-userinit --no-sysinit --script doc/constraint-method/evidence/ledger-reporter-checks-2026-09-24.lisp
;;; Does not load Wouldwork, stage a problem, run a search or consult any prediction.
;;;
;;; DISCLOSED STUB, as in ledger-checks-2026-09-20.lisp: an EMPTY :WW package is created so
;;; tech/constraint-ledger.lisp can be evaluated without Wouldwork.  Nothing else is stubbed.
;;;
;;; READ-ONLY INPUTS.  The dated LK5 cutoff-10 recommendation, whose hand-written command
;;; forms are the standard the generated ones must equal, and the crelay-topo realization
;;; ledger, which must round-trip byte-identical.  The round trip writes one temporary file
;;; beside this script and deletes it.

(require :asdf)   ; only so the hand-written commands' ASDF:SYSTEM-SOURCE-DIRECTORY reads
(defpackage :ww (:use :cl))
(defpackage :ledger-reporter-checks (:use :cl))
(in-package :ledger-reporter-checks)

(defparameter *checks* 0)


(defmacro check (condition)
  `(progn (incf *checks*)
          (assert ,condition)))


(let ((*package* (find-package :ww)))
  (with-open-file (in "tech/constraint-ledger.lisp")
    (loop for form = (read in nil :end) until (eq form :end)
          unless (and (consp form) (eq (first form) 'in-package))
            do (eval form))))

(defparameter *recommendation-file*
  "doc/problems/crelay-topo/constraint-evidence/lk5-cutoff10-recommendation-2026-09-21.txt")

(defparameter *crelay-ledger* "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")

(defparameter *archive*
  "doc/problems/crelay-topo/constraint-evidence/t10-location15-checkpoint.txt")


(defun file-text (path)
  (with-open-file (in path :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))


(defun read-forms-from (text marker count)
  "COUNT forms read from TEXT starting at MARKER, read in :WW."
  (let ((*package* (find-package :ww))
        (*read-eval* nil)
        (position (search marker text))
        (forms nil))
    (assert position)
    (dotimes (i count (nreverse forms))
      (multiple-value-bind (form next) (read-from-string text t nil :start position)
        (push form forms)
        (setf position next)))))


(defun read-lines-as-forms (lines)
  (let ((*package* (find-package :ww))
        (*read-eval* nil))
    (mapcar #'read-from-string lines)))


(defparameter *hand-commands*
  (read-forms-from (file-text *recommendation-file*) "(stage crelay-topo)" 5)
  "stage, threads, import, cutoff and the assigned search, as D ran them for LK5 at cutoff 10.")

(defparameter *hand-metadata*
  (first (read-forms-from (file-text *recommendation-file*) "(list :outcome" 1)))


(defun checkpoint-ledger (&key (preamble :stage) (archive *archive*) final)
  "A ledger with one checkpoint-start link carrying LK5's cutoff-10 search terms."
  (let ((ledger (ww::make-realization-ledger "crelay-topo" "2026-09-24"))
        (search (third (fifth *hand-commands*))))
    (ww::add-ledger-record ledger
      (ww::make-ledger-premise 'ww::pr1 "a derived premise"
                               '(:derived :grade 1 :by "a table")
                               :segment '(:view :physical :cycle :none :ghosts :unknown)))
    (ww::add-ledger-record ledger
      (ww::make-ledger-link 'ww::lk1 "a crossing searched from a saved checkpoint"
                            '(:derived :grade 2 :by "a spine")
                            :from "a" :to "b" :intent "cross"
                            :depends-on '((ww::pr1))
                            :segment '(:view :physical :cycle :none :ghosts :unknown)
                            :search-goal (third search)
                            :search-start "*t10-checkpoint*"
                            :search-archive archive
                            :search-cutoff 10
                            :search-preamble preamble
                            :search-final final))
    ledger))


(defun link (ledger)
  (ww::ledger-record ledger 'ww::lk1))


;; 1  A fresh checkpoint start prints, as Lisp forms, exactly the five commands and the
;;    metadata form of the dated hand-written LK5 recommendation.  It is runnable as printed.
(let* ((ledger (checkpoint-ledger))
       (lines (ww::ledger-search-commands ledger (link ledger) 10 16)))
  (check (= 6 (length lines)))
  (check (equal (subseq (read-lines-as-forms lines) 0 5) *hand-commands*))
  (check (equal (sixth (read-lines-as-forms lines)) *hand-metadata*))
  (check (not (find-if (lambda (line) (search "quickload" line)) lines))))

;; 2  A :CONTINUE checkpoint start omits stage, threads and import, and still sets the cutoff.
(let* ((ledger (checkpoint-ledger :preamble :continue :archive nil))
       (forms (read-lines-as-forms (ww::ledger-search-commands ledger (link ledger) 10 16))))
  (check (= 3 (length forms)))
  (check (equal (first forms) (fourth *hand-commands*)))
  (check (equal (second forms) (fifth *hand-commands*)))
  (check (equal (third forms) *hand-metadata*))
  (check (ww::ledger-search-ready-p (link ledger))))

;; 3  Without :CONTINUE and without an archive there is nothing to import from: the link is
;;    not runnable and the report names :SEARCH-ARCHIVE and prints no commands.
(let ((ledger (checkpoint-ledger :archive nil)))
  (check (not (ww::ledger-search-ready-p (link ledger))))
  (ww::recommend-ledger-search ledger 'ww::lk1 :date "2026-09-24")
  (let ((text (with-output-to-string (*standard-output*)
                (ww::report-ledger-recommendation ledger (link ledger)))))
    (check (search "NOT YET RUNNABLE" text))
    (check (search ":SEARCH-ARCHIVE" text))
    (check (not (search "solve-subgoal" text)))))

;; 4  Checkpoint cautions carry no goal-chain caution in either variant.
(dolist (preamble '(:stage :continue))
  (let* ((ledger (checkpoint-ledger :preamble preamble))
         (cautions (ww::ledger-search-cautions (link ledger))))
    (check (not (find-if (lambda (line) (search "chain" line)) cautions)))
    (check (not (find-if (lambda (line) (search "ww-undo" line)) cautions)))
    (check (find-if (lambda (line) (search "returns *t10-checkpoint* unchanged" line)) cautions))
    (check (find-if (lambda (line) (search "no automatic deepening" line)) cautions))))
(let* ((ledger (checkpoint-ledger))
       (cautions (ww::ledger-search-cautions (link ledger))))
  (check (find-if (lambda (line) (search "BEFORE the import" line)) cautions)))

;; 5  On a find: export to a NEW archive beside the old one, file REALIZED with validated NIL,
;;    no routine replay.  Only the final milestone is told to run VALIDATE-SEARCH-CHECKPOINT.
(let ((ledger (checkpoint-ledger)))
  (ww::recommend-ledger-search ledger 'ww::lk1 :date "2026-09-24")
  (let ((lines (ww::ledger-evidence-retrieval (link ledger)))
        (path (ww::ledger-checkpoint-export-path (link ledger))))
    (check (not (find-if (lambda (line) (search "validate-action-sequence" line)) lines)))
    (check (not (find-if (lambda (line) (search "(validate-search-checkpoint" line)) lines)))
    (check (find-if (lambda (line) (search "(export-search-checkpoint *t10-checkpoint*" line))
                    lines))
    (check (find-if (lambda (line) (search "validated NIL" line)) lines))
    (check (not (equal path *archive*)))
    (check (equal path "doc/problems/crelay-topo/constraint-evidence/lk1-checkpoint-2026-09-24.txt"))))
(let ((ledger (checkpoint-ledger :final t)))
  (check (find-if (lambda (line) (search "(validate-search-checkpoint *t10-checkpoint*)" line))
                  (ww::ledger-evidence-retrieval (link ledger)))))

;; 6  The recommendation for a checkpoint start defaults to 16 threads and records its archive.
(let ((ledger (checkpoint-ledger)))
  (ww::recommend-ledger-search ledger 'ww::lk1 :date "2026-09-24")
  (let ((recommendation (getf (link ledger) :recommendation)))
    (check (eql 16 (getf recommendation :threads)))
    (check (equal *archive* (getf recommendation :archive)))
    (check (equal "*t10-checkpoint*" (getf recommendation :start))))
  (check (ww::check-ledger-well-formed ledger)))

;; 7  The exhaustion reading says what was FOUND, qualifies coverage by measured truncation,
;;    and carries neither "exist" nor any bound lint word.  An exhaustion filed against it is
;;    well formed and leaves the link open.
(let ((reading (ww::ledger-exhaustion-reading 'ww::lk1 10)))
  (check (search "found no realization of lk1 within 10 actions" reading))
  (check (search "GRADE-3 COST BOUND" reading))
  (check (search "UNKNOWN means coverage was not measured" reading))
  (check (not (search "exist" reading)))
  (dolist (word ww::*ledger-bound-lint-words*)
    (check (not (search word (string-downcase reading))))))
(let ((ledger (checkpoint-ledger)))
  (ww::recommend-ledger-search ledger 'ww::lk1 :date "2026-09-24")
  (let ((bound (ww::ingest-ledger-result ledger 'ww::lk1 :exhausted :truncated t)))
    (check (eq :open (getf (link ledger) :status)))
    (check (equal (ww::ledger-exhaustion-reading 'ww::lk1 10)
                  (getf (ww::ledger-record ledger bound) :interpretation-committed)))
    (check (ww::check-ledger-well-formed ledger))))

;; 8  The cold-restart chain printout names checkpoint-start links as restored by import, and
;;    says nothing of the kind when there are none.
(let* ((ledger (checkpoint-ledger))
       (text (with-output-to-string (*standard-output*) (ww::report-chain-replay ledger))))
  (check (search "CHECKPOINT STARTS: lk1" text)))
(let ((ledger (checkpoint-ledger)))
  (ww::ledger-set-value (link ledger) :search-start :chain)
  (check (not (search "CHECKPOINT STARTS"
                      (with-output-to-string (*standard-output*)
                        (ww::report-chain-replay ledger))))))

;; 9  The crelay-topo ledger, whose LK5 already starts from a checkpoint, is well formed under
;;    the new code and round-trips byte-identical when written back with its own date.
(let* ((ledger (ww::read-realization-ledger *crelay-ledger*))
       (copy "doc/constraint-method/evidence/ledger-roundtrip-check.tmp.txt"))
  (check (ww::check-ledger-well-formed ledger))
  (ww::write-realization-ledger ledger copy (getf ledger :written))
  (check (equal (file-text *crelay-ledger*) (file-text copy)))
  (delete-file copy))

;; 10 The crelay-topo ledger's own LK5 recommendation, a hand transcription whose :DEEPENS
;;    holds the bound id alone, prints in full: the continue-form commands, the deepening
;;    line read from that bound, and an export path in the problem's evidence directory.
(let* ((ledger (ww::read-realization-ledger *crelay-ledger*))
       (lk5 (ww::ledger-record ledger 'ww::lk5))
       (text (with-output-to-string (*standard-output*)
               (ww::report-ledger-recommendation ledger lk5))))
  (check (search "(ww-set *depth-cutoff* 10)" text))
  (check (search "(setf *t10-checkpoint* (solve-subgoal *t10-checkpoint*" text))
  (check (search "DEEPENING: bd2 measured this link to cutoff 8; this run raises it to 10." text))
  (check (search "doc/problems/crelay-topo/constraint-evidence/lk5-checkpoint-2026-09-21.txt" text))
  (check (not (search "(stage " text))))

(format t "~&T14 ledger reporter acceptance checks passed: ~D.~%" *checks*)
