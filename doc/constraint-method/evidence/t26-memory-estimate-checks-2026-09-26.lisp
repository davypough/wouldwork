;;; ME memory estimate -- acceptance checks, T26, 2026-09-26.
;;; Expected readings: t26-memory-estimate-2026-09-26.txt, part 1 (written before code).
;;; Run after t26-run-pilot-2026-09-26.lisp, in the same image:
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t26-memory-estimate-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; A3 and A4 N1 read the pilot file (no search).  The checks then stage crelay-topo afresh at
;;; threads 16: A5 reads the sources, A6 regenerates the profile, A4 N2 and N3 run three small
;;; searches from the fresh staging.  A failed check is reported and counted, and the run
;;; continues; the last line gives the totals.

(in-package :ww)


(load (merge-pathnames "tech/constraint-memory-estimate.lisp" (asdf:system-source-directory :wouldwork)))


(defparameter *me-passed* 0)


(defparameter *me-failed* nil)


(defparameter *me-pilot-path*
  (merge-pathnames "doc/constraint-method/evidence/t26-pilot-results-2026-09-26.lisp"
                   (asdf:system-source-directory :wouldwork)))


(defparameter *me-t10-ceiling* (* 16000 1048576)
  "The heap of the T10 runs, 16,000 MiB, in bytes (part 1.2).")


(defparameter *me-probe-goal* '(has-location agent1 location14)
  "T23's P1.1 goal, for A4 N2 and N3 (part 1.3).")


(defun me-check (label passed)
  (if passed
    (progn (incf *me-passed*)
           (format t "~&  pass  ~A~%" label))
    (progn (push label *me-failed*)
           (format t "~&  FAIL  ~A~%" label))))


(defun me-file-lines (relative)
  (with-open-file (stream (merge-pathnames relative (asdf:system-source-directory :wouldwork)))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim '(#\Return) line))))


(defun me-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun me-run (cutoff runs)
  (find cutoff runs :key (lambda (run) (getf run :cutoff))))


(defun me-row-label (cutoff rows)
  (getf (find cutoff rows :key (lambda (row) (getf row :cutoff))) :label))


(defun me-check-pilot (pilot)
  "A3: E1 a 7-action plan first found at cutoff 7, none below; E2 every run 1-9 truncated;
 the pilot reached depth 9; E3 cutoff 12 LIKELY TO EXHAUST and E4 cutoff 10 not, at the
 T10 heap.  E5 and E6 are provisional and printed only."
  (let* ((runs (getf pilot :runs))
         (rows (memory-estimate-rows pilot '(7 8 9 10 11 12) *me-t10-ceiling*))
         (seven (me-run 7 runs)))
    (me-check (format nil "A3: pilot reached depth 9, end ~S" (getf pilot :end))
              (equal (getf pilot :end) '(:depth 9)))
    (me-check "A3 E1: no plan at cutoffs 1-6"
              (loop for cutoff from 1 to 6
                    never (getf (me-run cutoff runs) :found)))
    (me-check (format nil "A3 E1: plan at cutoff 7 of ~A actions, 7 expected" (getf seven :plan-length))
              (and (getf seven :found) (eql 7 (getf seven :plan-length))))
    (me-check "A3 E2: every run 1-9 truncated"
              (and (= 9 (length runs))
                   (every (lambda (run) (getf run :truncated)) runs)))
    (me-check (format nil "A3 E3: cutoff 12 LIKELY TO EXHAUST (~A)" (me-row-label 12 rows))
              (eq (me-row-label 12 rows) :likely-to-exhaust))
    (me-check (format nil "A3 E4: cutoff 10 not LIKELY TO EXHAUST (~A)" (me-row-label 10 rows))
              (member (me-row-label 10 rows) '(:safe :at-risk)))
    (format t "~&  info  E5 (provisional): cutoff 11 ~A~%" (me-row-label 11 rows))
    (format t "~&  info  E6 (provisional): s(9) ~:D, b ~,1F bytes/state~%"
            (getf (me-run 9 runs) :states)
            (float (getf (memory-estimate-basis pilot) :bytes-per-state) 1d0))))


(defun me-check-tenth (pilot)
  "A4 N1: at a tenth of the ceiling, every cutoff 10-12 SAFE at the real ceiling is AT RISK
 or LIKELY TO EXHAUST."
  (let ((real (memory-estimate-rows pilot '(10 11 12) *me-t10-ceiling*))
        (tenth (memory-estimate-rows pilot '(10 11 12) (floor *me-t10-ceiling* 10))))
    (loop for row in real
          for cutoff = (getf row :cutoff)
          do (me-check (format nil "A4 N1: cutoff ~D ~A at the real ceiling, ~A at a tenth"
                               cutoff (getf row :label) (me-row-label cutoff tenth))
                       (or (not (eq (getf row :label) :safe))
                           (member (me-row-label cutoff tenth) '(:at-risk :likely-to-exhaust)))))))


(defun me-unguarded (checkpoint cutoff)
  "One search for the probe goal from CHECKPOINT at CUTOFF with no states limit; its states
 and outcome."
  (setf *depth-cutoff* cutoff
        *max-states-processed* nil)
  (solve-search-checkpoint checkpoint *me-probe-goal*)
  (values *total-states-processed*
          (list (search-outcome-status *last-search-outcome*)
                (search-outcome-reason *last-search-outcome*))))


(defun me-check-guard ()
  "A4 N2: a guarded search at cutoff 11 with limit 10,000 is STOPPED with states in
 [10,000, 100,000), the setting is NIL afterwards, and the next search completes with 437
 states at cutoff 6.  A4 N3: with the setting NIL, cutoff 8 gives 10,021 states and
 (:EXHAUSTED-NO-SOLUTION :DEPTH-CUTOFF-TRUNCATED).  Solution type FIRST; restored."
  (let ((checkpoint (capture-search-checkpoint))
        (solution-type *solution-type*)
        (depth-cutoff *depth-cutoff*))
    (unwind-protect
        (progn
          (setf *solution-type* 'first)
          (multiple-value-bind (result found label)
              (run-guarded-search checkpoint *me-probe-goal* 11 nil 10000)
            (declare (ignore result))
            (me-check (format nil "A4 N2: guarded cutoff 11, limit 10,000: ~A, found ~A, states ~:D"
                              label found *total-states-processed*)
                      (and (eq label :stopped)
                           (not found)
                           (<= 10000 *total-states-processed* 99999))))
          (me-check "A4 N2: *MAX-STATES-PROCESSED* is NIL after the guard"
                    (null *max-states-processed*))
          (multiple-value-bind (states outcome) (me-unguarded checkpoint 6)
            (me-check (format nil "A4 N2: next search, cutoff 6: ~:D states (437), ~(~S~)" states outcome)
                      (= states 437)))
          (multiple-value-bind (states outcome) (me-unguarded checkpoint 8)
            (me-check (format nil "A4 N3: setting NIL, cutoff 8: ~:D states (10,021), ~(~S~)" states outcome)
                      (and (= states 10021)
                           (equal outcome '(:exhausted-no-solution :depth-cutoff-truncated))))))
      (setf *solution-type* solution-type
            *depth-cutoff* depth-cutoff
            *max-states-processed* nil))))


(defun me-source-forms ()
  "The top-level forms of the ME source, read in :WW."
  (with-open-file (stream (merge-pathnames "tech/constraint-memory-estimate.lisp"
                                           (asdf:system-source-directory :wouldwork)))
    (let ((*package* (find-package :ww)))
      (loop for form = (read stream nil stream)
            until (eq form stream)
            collect form))))


(defun me-check-source ()
  "A5: no staged object name, no LABELS or FLET, definitions callees-first, no blank line
 inside a definition, the source reads; the engine setting is present in both engine files."
  (let* ((lines (me-file-lines "tech/constraint-memory-estimate.lisp"))
         (text (string-downcase (format nil "~{~A~%~}" lines)))
         (forms (me-source-forms))
         (names (loop for form in forms
                      when (member (first form) '(defun defparameter))
                        collect (second form)))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants)))))
         (found (remove-if-not
                  (lambda (object)
                    (let ((name (string-downcase (symbol-name object))))
                      (loop for start = (search name text) then (search name text :start2 (1+ start))
                            while start
                            thereis (and (or (zerop start) (not (alphanumericp (char text (1- start)))))
                                         (let ((end (+ start (length name))))
                                           (or (= end (length text))
                                               (not (or (alphanumericp (char text end))
                                                        (char= (char text end) #\-)
                                                        (char= (char text end) #\*)))))))))
                  objects))
         (forward (loop for form in forms
                        for position = (position (second form) names)
                        when (eq (first form) 'defun)
                          append (loop for symbol in (remove-duplicates (alexandria:flatten (cddr form)))
                                       for callee = (position symbol names)
                                       when (and callee (> callee position))
                                         collect (list (second form) symbol))))
         (inside (loop for (line next) on lines
                       for number from 1
                       when (and (zerop (length (string-trim " " line)))
                                 next
                                 (plusp (length next))
                                 (not (member (char next 0) '(#\( #\;)))
                                 (plusp (length (string-trim " " next))))
                         collect number))
         (settings (string-downcase (format nil "~{~A~%~}" (me-file-lines "src/ww-settings.lisp"))))
         (parallel (string-downcase (format nil "~{~A~%~}" (me-file-lines "src/ww-parallel.lisp")))))
    (me-check (format nil "A5: source reads as ~D forms, ~D definitions" (length forms) (length names))
              (> (length names) 15))
    (me-check "A5: no LABELS or FLET" (not (or (search "(labels " text) (search "(flet " text))))
    (format t "~&  staged names found: ~(~{~A~^ ~}~)~%" found)
    (me-check (format nil "A5: no staged object name (~D checked)" (length objects)) (null found))
    (format t "~&  forward references: ~(~S~)~%" forward)
    (me-check "A5: definitions callees-first" (null forward))
    (format t "~&  blank lines followed by indented text: ~S~%" inside)
    (me-check "A5: no blank line inside a definition" (null inside))
    (me-check "A5: engine setting defined, default NIL (src/ww-settings.lisp)"
              (search "(defvar *max-states-processed* nil" settings))
    (me-check "A5: WORKER-LOCAL-DFS reads it and stops by REQUEST-PARALLEL-WORKER-SHUTDOWN"
              (let ((start (search "(defun worker-local-dfs" parallel)))
                (and start
                     (let ((end (search "(defun " parallel :start2 (1+ start))))
                       (and (search "*max-states-processed*" parallel :start2 start :end2 end)
                            (search "(request-parallel-worker-shutdown task-queue)" parallel
                                    :start2 start :end2 end))))))))


(defun me-check-profile ()
  "A6: the regenerated profile equals the committed profile file, line for line."
  (let ((file (me-file-lines "doc/problems/crelay-topo/Constraint-Static-Profile.txt"))
        (generated (with-input-from-string
                       (stream (with-output-to-string (*standard-output*)
                                 (funcall 'report-static-constraint-profile)))
                     (loop for line = (read-line stream nil) while line collect line))))
    (me-check (format nil "A6: regenerated profile (~D lines) equals the file (~D lines)"
                      (length generated) (length file))
              (equal (me-trim-blank-tail generated) (me-trim-blank-tail file)))))


(format t "~&ME checks, T26~%")
(let ((pilot (read-memory-pilot *me-pilot-path*)))
  (me-check-pilot pilot)
  (me-check-tenth pilot))
(stage crelay-topo)
(ww-set *threads* 16)
(load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
(me-check-source)
(me-check-profile)
(me-check-guard)
(format t "~&ME checks: ~D passed, ~D failed~%~{  failed: ~A~%~}"
        *me-passed* (length *me-failed*) (reverse *me-failed*))
