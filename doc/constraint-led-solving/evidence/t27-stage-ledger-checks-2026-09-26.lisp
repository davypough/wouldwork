;;; T27 acceptance checks: the stage-organized ledger and the file of record.
;;; From the repository root:
;;;   sbcl --noinform --no-userinit --no-sysinit --script doc/constraint-method/evidence/t27-stage-ledger-checks-2026-09-26.lisp
;;; Loads no Wouldwork, stages nothing, runs no search.  An EMPTY :WW package is created so
;;; tech/constraint-ledger.lisp can be evaluated, as in ledger-checks-2026-09-20.lisp.
;;;
;;; READ-ONLY INPUTS: the A2 evidence file (fixture and expected report, both written
;;; before any T27 code), the A4 baseline report captured by D with the pre-T27 code, and
;;; the crelay-topo version-1 ledger.  Scratch files are written beside this script and
;;; deleted at the end.  Every check prints pass or FAIL; a FAIL does not stop the run.
;;;
;;; Corrected after the first run (2026-09-26): A7's blank-line test now starts at the
;;; form's own "(defun", so a section comment before a definition is not read as part of it.

(defpackage :ww (:use :cl))
(defpackage :t27-checks (:use :cl))
(in-package :t27-checks)


(defparameter *passed* 0)


(defparameter *failed* 0)


(defun report-check (label ok)
  (if ok (incf *passed*) (incf *failed*))
  (format t "  ~:[FAIL~;pass~]  ~A~%" ok label)
  ok)


(defmacro check (label form)
  `(report-check ,label (handler-case (and ,form t)
                          (error (condition)
                            (format t "    error: ~A~%" condition)
                            nil))))


(defmacro check-error (label form)
  `(report-check ,label (handler-case (progn ,form nil)
                          (error () t))))


(let ((*package* (find-package :ww)))
  (with-open-file (in "tech/constraint-ledger.lisp")
    (loop for form = (read in nil :end) until (eq form :end)
          unless (and (consp form) (eq (first form) 'in-package))
            do (eval form))))


(defparameter *evidence* "doc/constraint-method/evidence/t27-stage-ledger-2026-09-26.txt")


(defparameter *baseline* "doc/constraint-method/evidence/t27-crelay-report-baseline-2026-09-26.txt")


(defparameter *source* "tech/constraint-ledger.lisp")


(defparameter *crelay-ledger* "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")


(defparameter *fixture* "doc/constraint-method/evidence/t27-fixture-scratch.txt")


(defparameter *roundtrip* "doc/constraint-method/evidence/t27-roundtrip-scratch.txt")


(defparameter ww::*fixture-path* *fixture*)


(defun file-text (path)
  "PATH's text with carriage returns removed, so a CRLF checkout compares like an LF one."
  (with-open-file (in path :external-format :utf-8)
    (let* ((text (make-string (file-length in)))
           (end (read-sequence text in)))
      (remove #\Return (subseq text 0 end)))))


(defun write-text (path text)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))


(defun marked-section (text begin end)
  "The text between the line holding BEGIN and the line holding END."
  (let* ((start (+ (search begin text) (length begin) 1))
         (stop (search end text :start2 start)))
    (subseq text start stop)))


(defun read-forms (text)
  "Every form in TEXT, read in :WW with *READ-EVAL* off."
  (let ((*package* (find-package :ww))
        (*read-eval* nil)
        (forms nil)
        (position 0))
    (loop (multiple-value-bind (form next) (read-from-string text nil :eof :start position)
            (when (eq form :eof)
              (return (nreverse forms)))
            (push form forms)
            (setf position next)))))


(defparameter *fixture-forms*
  (read-forms (marked-section (file-text *evidence*) "BEGIN FIXTURE" "END FIXTURE"))
  "The 20 LEDGER-FILE-APPLY forms of A2 part 1.3, as written before the code.")


(defparameter *expected-report*
  (marked-section (file-text *evidence*) "BEGIN EXPECTED" "END EXPECTED")
  "A2 part 1.4, from the report's first printed character to its last.")


(defun build-fixture-file ()
  "A2 part 1.3: an empty version-2 ledger written to *FIXTURE*, then the 20 forms."
  (when (probe-file *fixture*)
    (delete-file *fixture*))
  (ww::write-realization-ledger (ww::make-stage-ledger "crelay-topo" "2026-09-26")
                                *fixture* "2026-09-26")
  (dolist (form *fixture-forms*)
    (eval form)))


(defun build-fixture-in-memory ()
  "The same 20 operations applied to a ledger in memory, never touching a file.  Each form is
   (ledger-file-apply path #'operation argument ...)."
  (let ((ledger (ww::make-stage-ledger "crelay-topo" "2026-09-26")))
    (dolist (form *fixture-forms* ledger)
      (apply (symbol-function (second (third form))) ledger (mapcar #'eval (cdddr form))))))


(defun report-text (ledger)
  (with-output-to-string (*standard-output*)
    (ww::report-realization-ledger ledger)))


(defun record-without-event-dates (record)
  (let ((copy (copy-list record)))
    (setf (getf copy :events)
          (mapcar (lambda (event)
                    (let ((plain (copy-list event)))
                      (remf plain :date)
                      plain))
                  (getf copy :events)))
    copy))


(defun records-without-event-dates (ledger)
  (mapcar #'record-without-event-dates (getf ledger :records)))


(defun split-lines (text)
  (let ((lines nil)
        (start 0))
    (loop for position = (position #\Newline text :start start)
          do (push (subseq text start position) lines)
             (if position
               (setf start (1+ position))
               (return (nreverse lines))))))


(defun print-first-difference (expected actual)
  "Diagnostics for a failed text comparison: the first line on which the two differ."
  (let ((expected-lines (split-lines expected))
        (actual-lines (split-lines actual)))
    (loop for index from 1
          for one in expected-lines
          for other in actual-lines
          unless (string= one other)
            do (format t "    first difference at line ~D~%      expected: ~A~%      actual:   ~A~%"
                       index one other)
               (return))
    (format t "    lines: expected ~D, actual ~D~%" (length expected-lines) (length actual-lines))))


(defun record-of (ledger id)
  (ww::ledger-record ledger id))


(defun status-of (ledger id)
  (getf (record-of ledger id) :status))


(defun set-key (ledger id key value)
  (ww::ledger-set-value (record-of ledger id) key value))


(defun has-event-p (ledger id event)
  (find event (getf (record-of ledger id) :events) :key (lambda (entry) (getf entry :event))))


(defun top-level-definitions (text)
  "Each top-level form of TEXT with its source text: (form text).  The text runs from the end
   of the previous form, so it may begin with comment lines; A7 starts at the form itself."
  (let ((*package* (find-package :ww))
        (*read-eval* nil)
        (definitions nil)
        (position 0))
    (loop (multiple-value-bind (form next) (read-from-string text nil :eof :start position)
            (when (eq form :eof)
              (return (nreverse definitions)))
            (push (list form (string-left-trim '(#\Space #\Newline #\Tab)
                                               (subseq text position next)))
                  definitions)
            (setf position next)))))


(defun tree-symbols (tree)
  (cond ((symbolp tree) (list tree))
        ((consp tree) (append (tree-symbols (car tree)) (tree-symbols (cdr tree))))
        (t nil)))


(defparameter *t27-functions*
  '(ww::make-stage-ledger ww::make-ledger-stage ww::ledger-stage-record
    ww::supersede-ledger-stage ww::set-ledger-stage-check ww::realize-ledger-stage
    ww::close-ledger-stage ww::file-ledger-stage-bound ww::check-ledger-stage-shape
    ww::check-ledger-stage-lifecycle ww::check-ledger-stages ww::check-ledger-bound-targets
    ww::ledger-file-apply ww::ledger-stage-order ww::report-ledger-stage
    ww::report-ledger-stages ww::ledger-apply-retraction ww::retract-ledger-premise
    ww::check-ledger-references ww::check-ledger-closure-fields ww::check-ledger-well-formed
    ww::ledger-standing ww::make-ledger-bound ww::report-ledger-bounds
    ww::report-realization-ledger)
  "The functions T27 added or changed.  A7's convention checks cover these.")


(defparameter *problem-names*
  '("crelay" "agent1" "location1" "connector1" "tray1" "box1" "plate1" "gate1" "receiver1"
    "repeater1" "blower1" "switch1" "transmitter1" "recorder1")
  "Object names of the test problem.  None may appear in the ledger code (C3).")


;;; ---------------------------------------------------------------------------
;;; Run
;;; ---------------------------------------------------------------------------

(format t "~&T27 stage-ledger checks~%")

(check "A2 inputs: 20 fixture operations read from the evidence file"
       (= 20 (length *fixture-forms*)))

(build-fixture-file)

(defparameter *final* (ww::read-realization-ledger *fixture*))

;; A3 -- the generated report equals the expected report of A2 part 1.4.
(let ((actual (report-text *final*)))
  (unless (check "A3: fixture report equals A2's expected report, character for character"
                 (string= *expected-report* actual))
    (print-first-difference *expected-report* actual)))
(check "A3: the fixture file is version 2 and well formed"
       (and (eql 2 (getf *final* :version)) (ww::check-ledger-well-formed *final*)))
(check "A6 N9: plan order is st1 st2 st3 st4 st5"
       (equal '(ww::st1 ww::st2 ww::st3 ww::st4 ww::st5) (ww::ledger-stage-order *final*)))

;; A4 -- compatibility with the version-1 ledger and the unchanged T2/T14 checks (run
;; separately by D: 346 and 54 passed).
(let ((crelay (ww::read-realization-ledger *crelay-ledger*)))
  (check "A4: crelay-topo ledger reads as version 1 and is well formed"
         (and (eql 1 (getf crelay :version)) (ww::check-ledger-well-formed crelay)))
  (ww::write-realization-ledger crelay *roundtrip* (getf crelay :written))
  (check "A4: crelay-topo ledger writes back byte-identical"
         (string= (file-text *crelay-ledger*) (file-text *roundtrip*)))
  (let ((actual (report-text crelay))
        (baseline (file-text *baseline*)))
    (unless (check "A4: crelay-topo report equals the pre-T27 baseline" (string= baseline actual))
      (print-first-difference baseline actual))
    (check "A4: no STAGES section for a version-1 ledger" (not (search "STAGES (" actual)))))

;; A5 -- the file is the record.
(check "A5: the file read back equals the same operations applied in memory"
       (equal (records-without-event-dates *final*)
              (records-without-event-dates (build-fixture-in-memory))))
(let ((before (file-text *fixture*)))
  (check-error "A5: an operation breaking WF21 signals"
               (ww::ledger-file-apply *fixture* #'ww::realize-ledger-stage 'ww::st2 :hand nil nil))
  (check "A5: ... and leaves the file byte-identical" (string= before (file-text *fixture*))))
(let* ((text (file-text *fixture*))
       (next (search "(:id st2" text))
       (close (position #\) text :end next :from-end t)))
  (write-text *fixture* (concatenate 'string (subseq text 0 close)
                                     (format nil "~% :hand-note \"kept\"")
                                     (subseq text close)))
  (ww::ledger-file-apply *fixture* #'ww::set-ledger-stage-check 'ww::st5 :pass
                         "doc/constraint-method/evidence/t24-cycle-plan-check-2026-09-26.txt"
                         "2026-09-26")
  (let ((st1 (record-of (ww::read-realization-ledger *fixture*) 'ww::st1)))
    (check "A5: a hand-added unknown key survives a file-level edit"
           (equal "kept" (getf st1 :hand-note)))
    (check "A5: ... and stays last among the record's keys"
           (equal '(:hand-note "kept") (last st1 2)))))
(let* ((text (file-text *fixture*))
       (start (search "(:id st3" text))
       (at (search ":starts-from st1" text :start2 start))
       (broken (concatenate 'string (subseq text 0 at) ":starts-from st9"
                            (subseq text (+ at (length ":starts-from st1"))))))
  (write-text *fixture* broken)
  (check-error "A5: a hand edit breaking WF2 is caught by the next file-level edit"
               (ww::ledger-file-apply *fixture* #'ww::set-ledger-stage-check 'ww::st5 :pass
                                      "run" "2026-09-26"))
  (check "A5: ... and the file is left as the hand edit made it"
         (string= broken (file-text *fixture*))))

;; A6 -- negative tests on in-memory copies of the finished fixture.
(let ((copy (copy-tree *final*)))
  (set-key copy 'ww::st1 :validated nil)
  (check-error "A6 N1: a closed stage without validation signals (WF21)"
               (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (set-key copy 'ww::st2 :superseded-by nil)
  (check-error "A6 N2: a superseded stage without a successor signals (WF22)"
               (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (ww::ledger-set-value copy :version 1)
  (check-error "A6 N3: a stage in a version-1 ledger signals (WF19)"
               (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (set-key copy 'ww::st4 :depends-on '((ww::pr1) (ww::pr5) (ww::pr6)))
  (check-error "A6 N3: a stage whose start is named in no clause signals (WF19)"
               (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (set-key copy 'ww::st4 :check '(:label :conflict :date "2026-09-26" :run "run"))
  (check-error "A6 N4: a closed stage whose check is CONFLICT signals (WF25)"
               (ww::check-ledger-well-formed copy)))
(let* ((copy (copy-tree *final*))
       (bound (ww::file-ledger-stage-bound copy 'ww::st5 :start-state "st4's endpoint"
                                           :search-expression "the search as run"
                                           :cutoff 10 :threads 16 :run "run.txt"
                                           :interpretation "committed before the run"
                                           :depends-on '((ww::st4)) :date "2026-09-26")))
  (check "A6 N5: a stage bound is filed, listed in the stage's attempts, and well formed"
         (and (member bound (getf (record-of copy 'ww::st5) :attempts))
              (eq 'ww::st5 (getf (record-of copy bound) :for-stage))
              (eq :closed (status-of copy 'ww::st5))
              (ww::check-ledger-well-formed copy)))
  (let ((text (report-text copy)))
    (check "A6 N5: the stage section lists the attempt by id"
           (search "attempts: bd1" text))
    (check "A6 N5: the cost-bound section names the stage it attempted"
           (search "bd1  attempted st5; status standing" text)))
  (set-key copy bound :for-link 'ww::st4)
  (check-error "A6 N5: a bound naming both a link and a stage signals (WF23)"
               (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (check-error "A6 N5: a stage bound without its committed reading signals"
               (ww::file-ledger-stage-bound copy 'ww::st5 :start-state "s" :search-expression "e"
                                            :cutoff 10 :threads 16 :run "r"))
  (check-error "A6 N5: a stage bound without a cutoff signals (X2)"
               (ww::file-ledger-stage-bound copy 'ww::st5 :start-state "s" :search-expression "e"
                                            :threads 16 :run "r" :interpretation "i")))
(let ((copy (copy-tree *final*)))
  (ww::retract-ledger-premise copy 'ww::pr4 "withdrawn for the check" "2026-09-26")
  (check "A6 N6: retracting pr4 invalidates st3, st4 and st5"
         (every (lambda (id) (eq :invalidated (status-of copy id))) '(ww::st3 ww::st4 ww::st5)))
  (check "A6 N6: ... leaves st1 closed and st2 superseded"
         (and (eq :closed (status-of copy 'ww::st1)) (eq :superseded (status-of copy 'ww::st2))))
  (check "A6 N6: ... and leaves pr1-pr3, pr5, pr6 in force"
         (every (lambda (id) (eq :in-force (status-of copy id)))
                '(ww::pr1 ww::pr2 ww::pr3 ww::pr5 ww::pr6)))
  (check "A6 N6: ... and the copy is still well formed" (ww::check-ledger-well-formed copy)))
(let ((copy (copy-tree *final*)))
  (ww::add-ledger-record copy
    (ww::make-ledger-premise 'ww::pr7 "an alternative to pr5, for the check"
                             '(:user-asserted :by "D" :date "2026-09-26")
                             :segment '(:view :physical :cycle :open :ghosts :present)))
  (set-key copy 'ww::st4 :depends-on '((ww::pr1) (ww::st3) (ww::pr5 ww::pr7) (ww::pr6)))
  (ww::retract-ledger-premise copy 'ww::pr5 "withdrawn for the check" "2026-09-26")
  (check "A6 N7: with an alternative, st4 survives the retraction of pr5, closed and conditional"
         (and (eq :closed (status-of copy 'ww::st4))
              (eq :conditional (ww::ledger-standing copy 'ww::st4))
              (has-event-p copy 'ww::st4 :standing-changed)))
  (check "A6 N7: ... while st5, resting on pr5 alone, is invalidated"
         (eq :invalidated (status-of copy 'ww::st5))))
(let ((copy (copy-tree *final*)))
  (set-key copy 'ww::st2 :status :open)
  (set-key copy 'ww::st2 :superseded-by nil)
  (ww::add-ledger-record copy
    (ww::make-ledger-stage 'ww::st6 "a stage started from st2, for the check"
                           "doc/constraint-method/evidence/t24-cycle-plan-check-2026-09-26.txt"
                           "c2x" "a check stage" 'ww::st2 '((ww::pr1) (ww::st2))
                           "the endpoint validates from the initial state"
                           :segment '(:view :physical :cycle :closed :ghosts :absent)))
  (let ((bound (ww::file-ledger-stage-bound copy 'ww::st6 :start-state "st2's endpoint"
                                            :search-expression "the search as run"
                                            :cutoff 8 :threads 16 :run "run.txt"
                                            :interpretation "committed before the run"
                                            :depends-on '((ww::st2)) :date "2026-09-26")))
    (check "A6 N8: before supersession the copy is well formed"
           (ww::check-ledger-well-formed copy))
    (ww::supersede-ledger-stage copy 'ww::st2 'ww::st3 "revised for the check" "2026-09-26")
    (check "A6 N8: superseding st2 invalidates st6, which started from it"
           (eq :invalidated (status-of copy 'ww::st6)))
    (check "A6 N8: ... orphans the bound measured from st2's endpoint"
           (eq :orphaned (status-of copy bound)))
    (check "A6 N8: ... leaves st1 and st3 closed, and st2 conditional"
           (and (eq :closed (status-of copy 'ww::st1))
                (eq :closed (status-of copy 'ww::st3))
                (eq :conditional (ww::ledger-standing copy 'ww::st2))))
    (check "A6 N8: ... and the copy is still well formed" (ww::check-ledger-well-formed copy))))
(let ((copy (copy-tree *final*)))
  (check-error "A6: a stage cannot supersede itself"
               (ww::supersede-ledger-stage copy 'ww::st3 'ww::st3 "no" "2026-09-26"))
  (check-error "A6: a check label outside the CP set signals"
               (ww::set-ledger-stage-check copy 'ww::st5 :maybe "run" "2026-09-26"))
  (check-error "A6: only a realized stage is closed"
               (ww::close-ledger-stage copy 'ww::st5 "run" "2026-09-26")))

;; A7 -- conventions in the T27 code.
(let* ((text (file-text *source*))
       (definitions (top-level-definitions text))
       (names (loop for (form) in definitions
                    when (and (consp form) (eq (first form) 'defun))
                      collect (second form))))
  (check "A7: no LABELS or FLET anywhere in the ledger code"
         (not (or (search "(labels " text) (search "(flet " text))))
  (let ((found (remove-if-not (lambda (name) (search name (string-downcase text)))
                              *problem-names*)))
    (unless (check "A7: no problem object name in the ledger code (C3)" (null found))
      (format t "    found: ~{~A~^ ~}~%" found)))
  (dolist (name *t27-functions*)
    (let* ((entry (find-if (lambda (definition)
                             (and (consp (first definition))
                                  (eq (first (first definition)) 'defun)
                                  (eq (second (first definition)) name)))
                           definitions))
           (later (member name names))
           (forward (intersection (tree-symbols (cdddr (first entry))) (rest later)))
           (own-text (and entry                                          ; corrected after the first run
                          (subseq (second entry) (search "(defun" (second entry))))))  ; corrected after the first run
      (check (format nil "A7: ~(~A~) is defined, callees-first, with no blank line inside" name)
             (and entry
                  (null forward)
                  (not (search (format nil "~%~%") own-text)))))))               ; corrected after the first run

(dolist (path (list *fixture* *roundtrip*))
  (when (probe-file path)
    (delete-file path)))

(format t "~&T27 checks: ~D passed, ~D failed.~%" *passed* *failed*)
