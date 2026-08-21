;;; Filename: ww-problem-tests.lisp

;;; Runs through some test problems, checking that they stage properly
;;; and solve correctly.


(in-package :ww)


;Any additions to this list requires rebuilding problem-test-solutions.lisp
;in the run-test-problems function below.
(defvar *test-problem-files*
  '("problem-blocks3.lisp" "problem-blocks3a.lisp" "problem-blocks4.lisp" "problem-boxes.lisp"
    "problem-jugs2.lisp" "problem-jugs4.lisp" "problem-queens4.lisp" "problem-queens8.lisp"
    "problem-captjohn.lisp" "problem-quern.lisp" "problem-graveyard.lisp" "problem-sentry.lisp"
    ;"problem-crossword5-11.lisp"  ;runs out of default memory
    ;"problem-crossword15-18.lisp"  ;runs out of default memory
    "problem-crossword13.lisp" "problem-array-path.lisp"
    "problem-tiles0a-csp.lisp" "problem-tiles1a-heuristic.lisp"
    "problem-tiles1e-heuristic.lisp"
    ;"problem-tiles1b.lisp"  ;takes too long
    ;"problem-tiles1c.lisp"  ;takes too long
    ;"problem-tiles1d.lisp"  ;needs debugging
    ;"problem-tiles2a.lisp"  ;takes too long
    ;"problem-tiles2a-heuristic.lisp"  ;takes too long
    ;"problem-tiles2b.lisp"  ;takes too long
    ;"problem-tiles2c.lisp"  ;takes too long
    ;"problem-tiles3a-heuristic.lisp"  ;takes too long
    ;"problem-tiles5a-heuristic.lisp"  ;takes too long
    ;"problem-tiles5b-heuristic.lisp"  ;needs debugging
    ;"problem-tiles7a-heuristic2.lisp"  ;takes too long
    ;"problem-tiles7a-heuristic3.lisp"  ;takes too long
    ;"problem-tiles0b-csp.lisp"  ;takes too long
    ;"problem-tiles7a-heuristic.lisp"  ;takes too long
    "problem-hanoi.lisp"
    ;"problem-triangle.lisp"  ;needs debugging
    ;"problem-triangle-backward.lisp"  ;takes too long
    "problem-triangle-xy.lisp" "problem-triangle-xyz.lisp" "problem-triangle-heuristic.lisp"
    "problem-triangle-macros.lisp" "problem-triangle-macros-one.lisp" "problem-triangle-xyz-one.lisp"
    "problem-tsp.lisp"
    "problem-u2.lisp" "problem-donald.lisp" "problem-knap4a.lisp" "problem-knap4b.lisp"
    ;"problem-crater.lisp"  ;needs debugging
    "problem-knap19.lisp"
    ;"problem-socrates1.lisp"  ;needs debugging
    ;"problem-socrates2.lisp"  ;needs debugging
    ;"problem-smallspace-macro.lisp"  ;needs debugging
    ;"problem-smallspace2.lisp"  ;takes too long
    "problem-smallspace.lisp"))


;Any additions to this list requires deleting problem-test-bt-solutions.lisp
;and re-running (test-bt) to rebuild it.
;One representative chosen per problem class to avoid redundancy.
;hanoi and donald have no native depth-cutoff; overrides set in run-bt-test-problems.
(defvar *test-bt-problem-files*
  '("problem-blocks3.lisp"              ;tree, every, non-fluent assertions
    "problem-blocks3a.lisp"             ;graph->tree, every, fluent bind
    ;"problem-blocks4.lisp"             ;redundant with blocks3
    "problem-boxes.lisp"                ;graph->tree, min-length, multi-object
    ;"problem-jugs2.lisp"               ;min-time solution type not supported by bt (use quern for fluent arithmetic)
    ;"problem-jugs4.lisp"               ;redundant with jugs2
    "problem-queens4.lisp"              ;tree, every, structured assignment
    ;"problem-queens8.lisp"             ;redundant with queens4
    "problem-captjohn.lisp"             ;csp, variable-per-level assignment
    "problem-quern.lisp"                ;first, depth-cutoff 8, conditional assert
    ;"problem-graveyard.lisp"            ;min-length on tree requires exhausting 12^14 nodes; no solution within native depth-cutoff 10
    ;"problem-sentry.lisp"              ;has define-happening, incompatible with bt
    "problem-crossword13.lisp"          ;tree, first, string state, nested updates
    "problem-array-path.lisp"           ;tree, min-length, no-solution case
    ;"problem-tiles0a-csp.lisp"         ;takes too long
    ;"problem-tiles1a.lisp"              ;graph->tree, min-length, list-coord state--takes too long with bt
    ;"problem-tiles1a-heuristic.lisp"   ;heuristic unused by bt, redundant with tiles1a
    ;"problem-tiles1e-heuristic.lisp"   ;same
    "problem-hanoi.lisp"                ;min-length, depth-cutoff 9 set in run-bt-test-problems
    "problem-triangle-xyz.lisp"         ;first, canonical triangle form
    ;"problem-triangle-xy.lisp"         ;redundant with triangle-xyz
    "problem-triangle-macros.lisp"      ;tree, first, multiple asserts per action
    ;"problem-triangle-macros-one.lisp" ;redundant with triangle-macros
    ;"problem-triangle-xyz-one.lisp"    ;redundant with triangle-xyz
    ;"problem-tsp.lisp"                 ;min-value solution type not supported by bt
    "problem-u2.lisp"                   ;min-length, time-constrained preconditions
    "problem-donald.lisp"               ;tree, first, depth-cutoff 6 set in run-bt-test-problems
    ;"problem-knap4a.lisp"              ;max-value solution type not supported by bt
    ;"problem-knap4b.lisp"              ;max-value solution type not supported by bt
    ;"problem-knap19.lisp"              ;too slow
    ;"problem-crater.lisp"              ;ok
    ;"problem-smallspace2.lisp"         ;takes too long
    ;"problem-smallspace.lisp"          ;takes too long
))


;;; Helper Functions ;;;

(defun parse-problem-name (problem-filename)
  "Extract problem name from filename (e.g., 'problem-blocks3.lisp' -> 'blocks3')"
  (if (string-prefix-p "problem-" problem-filename)
      (subseq problem-filename 8 (- (length problem-filename) 5))
      (subseq problem-filename 0 (- (length problem-filename) 5))))


(defun prompt-user-action (problem-name)
  "Prompt user for Continue/Skip/All/Quit. Returns (values should-process continue-all)"
  (format t "~%=====================================================~%")
  (format t "Process problem: ~A~%" (string-upcase problem-name))
  (format t "=====================================================~%")
  (format t "Continue, Skip, All, Quit: ")
  (force-output)
  (let* ((response (read-line))
         (choice (if (> (length response) 0)
                     (char-upcase (char response 0))
                     #\C)))
    (case choice
      (#\Q (values nil :quit))
      (#\S (values nil nil))
      (#\A (values t t))
      (#\C (values t nil))
      (t (values t nil)))))


(defun cleanup-test-files ()
  "Delete temporary problem.lisp and vals.lisp files"
  (let ((root (asdf:system-source-directory :wouldwork)))
    (uiop:delete-file-if-exists (instance-problem-file root))
    (uiop:delete-file-if-exists (instance-vals-file root))))


(defun print-test-header (problem-name &optional (algorithm ""))
  "Print test header for a problem"
  (format t "~%=====================================================~%")
  (format t "Process problem~A: ~A~%"
          (if (string= algorithm "") "" (format nil " (~A)" algorithm))
          (string-upcase problem-name))
  (format t "=====================================================~%"))


(defun collect-solution-data ()
  "Collect best solution and state from current problem results"
  (let ((best-solution (ut::if-it (first *solution-paths*) (solution.path ut::it)))
        (best-state (when *best-states*
                      (alexandria:hash-table-alist (problem-state.idb (first *best-states*))))))
    (list best-solution best-state)))


(defun run-test-problems ()
  (cleanup-test-files)
  (reset-parameters)  ; Initial reset for the test suite
  (with-silenced-compilation
    (let* ((problems-to-run *test-problem-files*)
           (test-solutions-file (merge-pathnames "problem-test-solutions.lisp"
                                                 (asdf:system-source-directory :wouldwork)))
           (problem-test-solutions (if (probe-file test-solutions-file)
                                     (read-hash-table-from-file test-solutions-file)
                                     (make-hash-table :test #'equal)))
           (problems-processed 0)
           (continue-all nil)
           failed-problems)
      (loop for problem in problems-to-run
            do (let* ((problem-name (parse-problem-name problem))
                      (should-process t))
                 (print-test-header problem-name)

                 (unless continue-all
                   (format t "Continue, Skip, All, Quit: ")
                   (force-output)
                   (let* ((response (read-line))
                          (choice (if (> (length response) 0)
                                      (char-upcase (char response 0))
                                      #\C)))
                     (case choice
                       (#\Q (return-from run-test-problems nil))
                       (#\S (setf should-process nil))
                       (#\A (setf continue-all t))
                       (#\C nil)
                       (t nil))))

                 (when should-process
                   (reset-parameters)  ; RESET PARAMETERS BEFORE EACH TEST
                   (uiop:delete-file-if-exists (instance-vals-file (asdf:system-source-directory :wouldwork)))
                   (load-problem problem-name)
                   (incf problems-processed)
                   (ww-solve)
                   (let ((solution-data (collect-solution-data)))
                     (unless (equalp solution-data
                                     (gethash problem-name problem-test-solutions))
                       (format t "~%The problem solution above does not match the expected solution:")
                       (format t "~%~A~2%" (gethash problem-name problem-test-solutions))
                       (push problem-name failed-problems))
                     (unless (probe-file test-solutions-file)
                       (setf (gethash problem-name problem-test-solutions)
                             solution-data))
                     t))))
      (cleanup-test-files)
      (stage blocks3)
      (format t "~%~%Final Summary:~%")
      (format t "Total test problems run: ~D~%" (length *test-problem-files*))
      (format t "Test failures: ~D~%" (length failed-problems))
      (format t "Failed problems: ~A~%" (reverse failed-problems))
      (format t "Note: A failed problem solution is not necessarily wrong, but different from the reference solution,")
      (format t "a common occurrence when running in parallel mode.")
      (progn (unless (probe-file test-solutions-file)
               (write-hash-table-to-file problem-test-solutions
                 (merge-pathnames "problem-test-solutions.lisp" (asdf:system-source-directory :wouldwork))))
             t)
      t)))


(defun test ()
  "Run standard test suite using depth-first search."
  (run-test-problems))


(defvar *expected-min-length* nil
  "Test-only.  When a test/problem-*.lisp file sets this (plain SETF, not WW-SET --
   it is test metadata, not a search-control parameter, and must not be persisted to
   vals.lisp), TEST-TALOS requires the solved plan to have exactly this length under
   min-length search.  NIL performs no check.")


;;; MUTATION VALIDATION ;;;

;;; Mutation declarations live in their owning characterization problems.  The
;;; ordinary sweep records only each problem path and mutation name.  Validation
;;; then restages that problem with the named mutation selected, so its broken
;;; definition is installed before compilation and initial derivation.


(defun run-mutation-case (problem-path mutation-name)
  "Restage PROBLEM-PATH with MUTATION-NAME active and require the test to fail."
  (let ((problem-name
          (parse-problem-name (file-namestring problem-path)))
        (*requested-test-mutation* mutation-name)
        (*test-mutation-applied* nil))
    (print-test-header problem-name "VALIDATE")
    (setf *expected-min-length* nil)
    (multiple-value-bind (mutation stage-condition)
        (stage-test-mutation problem-path mutation-name)
      (if stage-condition
        (progn
          (format t
                  "~%Mutation detected during staging after ~A was installed: ~A~%"
                  mutation-name stage-condition)
          t)
        (progn
          (format t "Breaking ~A with ~A -- ~A~%"
                  (test-mutation-target mutation)
                  mutation-name
                  (test-mutation-note mutation))
          (mutation-solve-detected-p problem-name mutation-name))))))


(defun stage-test-mutation (problem-path mutation-name)
  "Stage a selected mutation, distinguishing installation errors from detection."
  (let (stage-condition)
    (handler-case
        (%stage problem-path)
      (error (condition)
        (if (eq *test-mutation-applied* mutation-name)
          (setf stage-condition condition)
          (error condition))))
    (cond
      (stage-condition
        (values nil stage-condition))
      ((not (eq *test-mutation-applied* mutation-name))
        (error "Test mutation ~S was not applied while staging ~A."
               mutation-name problem-path))
      (t
        (values
          (or (find mutation-name *test-mutations*
                    :key #'test-mutation-name
                    :test #'eq)
              (error "Staged problem did not register mutation ~S."
                     mutation-name))
          nil)))))


(defun mutation-solve-detected-p (problem-name mutation-name)
  "Run the mutated claims and search, returning true when either detects it.  A
   broken guard can let the search wander into states a corrupted invariant makes
   look perpetually new, exhausting the control stack rather than cleanly finding
   no solution; STORAGE-CONDITION is a sibling of ERROR under SERIOUS-CONDITION, not
   a subtype, so it needs its own clause to count as detection instead of dropping
   into the debugger."
  (handler-case
      (progn
        (run-test-claims)
        (ww-solve)
        (mutation-outcome-detected-p problem-name mutation-name))
    (storage-condition (condition)
      (format t
              "~%Mutation detected: exhausted resources instead of solving: ~A~%"
              condition)
      t)
    (error (condition)
      (format t
              "~%Mutation detected: signaled an error instead of solving: ~A~%"
              condition)
      t)))


(defun mutation-outcome-detected-p (problem-name mutation-name)
  "Classify the completed mutated search as detected or surviving."
  (cond
    ((not *solution-paths*)
      (format t "~%Mutation detected: no solution found.~%")
      t)
    ((and *expected-min-length*
          (eq *solution-type* 'min-length)
          (/= (solution.depth (first *solution-paths*))
              *expected-min-length*))
      (format t "~%Mutation detected: solved at the wrong length.~%")
      t)
    (t
      (format t
              "~%SURVIVING MUTANT: ~A still solves correctly with ~A active.~%"
              problem-name mutation-name)
      nil)))


(defun talos-problem-failed-p (problem-name)
  "Run staged claims and search, reporting attributed characterization failures."
  (handler-case
      (progn
        (run-test-claims)
        (ww-solve)
        (cond
          ((not *solution-paths*)
            (format t "~%Talos test ~A completed without a solution.~%"
                    problem-name)
            t)
          ((and *expected-min-length*
                (eq *solution-type* 'min-length)
                (/= (solution.depth (first *solution-paths*))
                    *expected-min-length*))
            (format t "~%Talos test ~A solved at length ~D, expected ~D.~%"
                    problem-name
                    (solution.depth (first *solution-paths*))
                    *expected-min-length*)
            t)))
    (test-claim-failure (condition)
      (format t "~%Talos test ~A failed before search:~A~%"
              problem-name condition)
      t)))


(defun test-talos ()
  "Stage and solve every problem file in the test directory.
   Registered characterization claims run after staging and before search.  An
   attributed claim failure, no solution, or wrong solved length is recorded and
   the run continues; a genuine Lisp error still halts the run immediately, as it
   does for TEST and TEST-BT.  After the ordinary sweep, every problem's
   registered mutations are restaged and run individually, each one required to
   make its problem fail.  A final summary lists every failed problem and every
   surviving mutant."
  (let ((problem-files
          (sort (directory (merge-pathnames "problem-*.lisp"
                                            (get-test-folder-path)))
                #'string-lessp
                :key #'file-namestring))
        failed-problems
        mutation-schedule
        surviving-mutants)
    (cleanup-test-files)
    (unwind-protect
      (progn
        (dolist (problem-file problem-files)
          (let ((problem-name (parse-problem-name (file-namestring problem-file)))
                (problem-path (format nil "test/~A" (file-namestring problem-file))))
            (print-test-header problem-name "TALOS")
            (setf *expected-min-length* nil)
            (%stage problem-path)
            (dolist (mutation *test-mutations*)
              (push (list problem-path (test-mutation-name mutation))
                    mutation-schedule))
            (when (talos-problem-failed-p problem-name)
              (push problem-name failed-problems))))
        (setf mutation-schedule (nreverse mutation-schedule))
        (format t "~%~%Validating check teeth (~D mutation case~:P)...~%"
                (length mutation-schedule))
        (dolist (scheduled-mutation mutation-schedule)
          (destructuring-bind (problem-path mutation-name) scheduled-mutation
            (unless (run-mutation-case problem-path mutation-name)
              (push mutation-name surviving-mutants))))
        (format t "~%~%Final Summary:~%")
        (format t "Total Talos test problems run: ~D~%" (length problem-files))
        (format t "Test failures: ~D~%" (length failed-problems))
        (format t "Failed problems: ~A~%" (reverse failed-problems))
        (format t "Mutation cases run: ~D~%" (length mutation-schedule))
        (format t "Surviving mutants: ~D~%" (length surviving-mutants))
        (format t "Surviving mutant names: ~A~%" (reverse surviving-mutants))
        (format t "Overall: ~:[FAILED~;PASSED~]~%"
                (and (null failed-problems) (null surviving-mutants)))
        (and (null failed-problems) (null surviving-mutants)))
      (cleanup-test-files))))


(defun write-hash-table-to-file (hash-table filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print hash-table out))))


(defun read-hash-table-from-file (filename)
  (with-open-file (in filename :direction :input)
    (with-standard-io-syntax
      (read in))))


;;; TOPO TEST ;;;


(defparameter *topo-derived-relations*
  '(traverse-via traverse-via> los-via los-barrier-crossings>
    beam-crossing> crossings-along-beam> beam-crossings-before-gate>)
  "The relations the coordinate derivations produce.  Every one is derived rather than
   authored, so anything that moves in them means a derivation moved -- which is the thing
   the topo problems have no other guard against, since none of them is in (TEST-TALOS).")


(defun test-topo ()
  "Stage every probs/problem-*-topo.lisp and check the geometry it derives against a
   recorded expectation.  These five are the live Talos work, and none of them is in
   (TEST-TALOS) -- which is how problem-phobia-topo staged with an initialization error
   across two whole phases before anyone noticed.  Staging alone runs every init check and
   every coordinate derivation and the universal terrain invariant.  The suite then applies
   the topology-specific terrain connectivity policy before comparing the recorded
   expectation.  That expectation additionally pins what the derivations produced, so one
   that quietly starts emitting different facts fails here rather than in a search weeks
   later.

   No search is run.  These problems take minutes each to solve, and this suite is meant to
   stay cheap enough to run after every change to tech/.

   Expectations live in problem-test-topo-geometry.lisp beside the system: recorded on the
   first run, compared on every later one.  After a change that legitimately moves the
   geometry, delete that file, re-run to re-record, and say in the commit what moved and
   why."
  (let* ((problem-files
           (sort (directory (merge-pathnames "problem-*-topo.lisp"
                                             (get-probs-folder-path)))
                 #'string-lessp
                 :key #'file-namestring))
         (geometry-file (merge-pathnames "problem-test-topo-geometry.lisp"
                                         (asdf:system-source-directory :wouldwork)))
         (recording (not (probe-file geometry-file)))
         (recorded (if recording
                     (make-hash-table :test #'equal)
                     (read-hash-table-from-file geometry-file)))
         failed-problems)
    (unwind-protect
      (progn
        (dolist (problem-file problem-files)
          (let ((problem-name (parse-problem-name (file-namestring problem-file))))
            (print-test-header problem-name "TOPO")
            (when (topo-problem-failed-p
                    problem-name
                    (format nil "probs/~A" (file-namestring problem-file))
                    recorded recording)
              (push problem-name failed-problems))))
        (when recording
          (write-hash-table-to-file recorded geometry-file)
          (format t "~%Recorded derived geometry in ~A~%" geometry-file))
        (format t "~%~%Final Summary:~%")
        (format t "Total topo problems staged: ~D~%" (length problem-files))
        (format t "Failures: ~D~%" (length failed-problems))
        (format t "Failed problems: ~A~%" (reverse failed-problems))
        (format t "Overall: ~:[FAILED~;PASSED~]~%" (null failed-problems))
        (null failed-problems))
      (cleanup-test-files))))


(defun topo-problem-failed-p (problem-name problem-path recorded recording)
  "Stage one topo problem and compare its derived geometry with the recorded expectation.  A
   staging error is reported and attributed rather than allowed to halt the run, so one
   broken problem never hides the state of the other four -- the failure mode this whole
   suite exists to close."
  (handler-case
      (progn
        (%stage problem-path)
        (validate-topo-terrain)
        (let ((geometry (topo-derived-geometry)))
          (cond (recording
                 (setf (gethash problem-name recorded) geometry)
                 (format t "~%Recording derived geometry for ~A:~%~{  ~A~%~}"
                         problem-name (topo-geometry-report geometry))
                 nil)
                ((equal geometry (gethash problem-name recorded))
                 (format t "~%~A derived geometry matches:~%~{  ~A~%~}"
                         problem-name (topo-geometry-report geometry))
                 nil)
                (t
                 (format t "~%~A derived geometry changed:~%~{~A~%~}"
                         problem-name
                         (topo-geometry-differences
                           (gethash problem-name recorded) geometry))
                 t))))
    (error (condition)
      (format t "~%Topo problem ~A failed to stage:~%~A~%" problem-name condition)
      t)))


(defun validate-topo-terrain ()
  "Apply the terrain connectivity assumptions that distinguish complete *-TOPO specs from
   focused walking technology models.  WALKABILITY supplies the checker internally; a
   problem specification never includes that latent substrate itself."
  (unless (fboundp 'terrain-policy-complaints-for-state)
    (error "A topology problem must include WALKABILITY for terrain validation."))
  (let ((complaints
          (funcall (symbol-function 'terrain-policy-complaints-for-state) *start-state*)))
    (when complaints
      (report-terrain-complaints complaints))))


(defun topo-derived-geometry ()
  "What the staged problem's coordinate derivations produced: one (relation count digest)
   entry per derived relation, ordered by relation name.

   The count is what a failure report quotes, because a number that moved from 48 to 63 says
   something a digest never can.  The digest is what makes the check exact: a derivation that
   swapped two rows of one relation, or changed a payload without changing how many rows
   there are, leaves every count alone and would otherwise pass unnoticed.

   A relation the problem's technologies never declared contributes no entry rather than a
   zero, so giving one problem a new capability leaves the other four's expectations
   untouched."
  (let ((grouped (make-hash-table :test #'eq))
        (*print-case* :upcase)
        (*print-pretty* nil)
        (*package* (find-package :ww)))
    (loop for key being the hash-keys of *static-db* using (hash-value value)
          when (and (consp key)
                    (member (first key) *topo-derived-relations*))
            do (push (format nil "~S = ~S" key value)
                     (gethash (first key) grouped)))
    (sort (loop for relation being the hash-keys of grouped using (hash-value rows)
                collect (list relation
                              (length rows)
                              (topo-row-digest (sort rows #'string<))))
          #'string<
          :key (lambda (entry)
                 (symbol-name (first entry))))))


(defun topo-row-digest (rows)
  "A 64-bit FNV-1a digest of ROWS, which must already be sorted.  Computed arithmetically
   rather than with SXHASH, whose value is implementation- and version-dependent: a recorded
   expectation has to stay comparable between the maintainer's Lisp and any other.  Each row
   is terminated before the next is folded in, so two different row lists cannot digest alike
   merely by concatenating the same way."
  (let ((digest 14695981039346656037))
    (dolist (row rows digest)
      (loop for character across row
            do (setf digest (ldb (byte 64 0)
                                 (* (logxor digest (char-code character))
                                    1099511628211))))
      (setf digest (ldb (byte 64 0)
                        (* (logxor digest (char-code #\Newline))
                           1099511628211))))))


(defun topo-geometry-report (geometry)
  "GEOMETRY as one readable line per relation, for a recording or matching run."
  (loop for (relation count digest) in geometry
        collect (format nil "~A: ~D row~:P, digest ~(~16,'0X~)" relation count digest)))


(defun topo-geometry-differences (expected actual)
  "One line per derived relation whose count or contents changed, naming the relation and
   saying which of the two moved.  A count that moved is nearly always an authoring change;
   a count that held while the digest moved is a derivation change, and the two want looking
   at in quite different places."
  (let ((relations (sort (remove-duplicates
                           (append (mapcar #'first expected)
                                   (mapcar #'first actual)))
                         #'string<
                         :key #'symbol-name))
        (differences nil))
    (dolist (relation relations (nreverse differences))
      (let ((was (assoc relation expected))
            (now (assoc relation actual)))
        (cond ((null was)
               (push (format nil "  ~A: newly derived, ~D row~:P" relation (second now))
                     differences))
              ((null now)
               (push (format nil "  ~A: no longer derived, was ~D row~:P"
                             relation (second was))
                     differences))
              ((/= (second was) (second now))
               (push (format nil "  ~A: ~D row~:P, was ~D"
                             relation (second now) (second was))
                     differences))
              ((/= (third was) (third now))
               (push (format nil "  ~A: ~D row~:P as before, but their contents changed"
                             relation (second now))
                     differences)))))))


;;; BACKTRACKING TEST ;;;


(defun run-bt-test-problems ()
  (cleanup-test-files)
  (reset-parameters)
  (with-silenced-compilation
    (let* ((problems-to-run *test-bt-problem-files*)
           (test-solutions-file (merge-pathnames "problem-test-bt-solutions.lisp"
                                                 (asdf:system-source-directory :wouldwork)))
           (problem-test-solutions (if (probe-file test-solutions-file)
                                      (read-hash-table-from-file test-solutions-file)
                                      (make-hash-table :test #'equal)))
           (problems-processed 0)
           (continue-all nil)
           failed-problems)
      (loop for problem in problems-to-run
            do (let* ((problem-name (parse-problem-name problem))
                      (should-process t))
                 (print-test-header problem-name "BACKTRACKING")
                 (unless continue-all
                   (format t "Continue, Skip, All, Quit: ")
                   (force-output)
                   (let* ((response (read-line))
                          (choice (if (> (length response) 0)
                                      (char-upcase (char response 0))
                                      #\C)))
                     (case choice
                       (#\Q (return-from run-bt-test-problems nil))
                       (#\S (setf should-process nil))
                       (#\A (setf continue-all t))
                       (#\C nil)
                       (t nil))))
                 (when should-process
                   (reset-parameters)
                   (uiop:delete-file-if-exists (instance-vals-file (asdf:system-source-directory :wouldwork)))
                   ;; Load silently to acquire native settings from define-problem,
                   ;; suppressing the initial depth-first parameter display.
                   ;; Justified here as this is a test routine only.
                   (let ((*standard-output* (make-broadcast-stream)))
                     (%stage problem-name))
                   ;; Override for backtracking; native depth-cutoff already preserved.
                   (setf *algorithm*     'backtracking
                         *tree-or-graph* 'tree)
                   ;; Problems with no native depth-cutoff need one for bt tree search.
                   (when (string-equal problem-name "hanoi")
                     (setf *depth-cutoff* 9))
                   (when (string-equal problem-name "donald")
                     (setf *depth-cutoff* 6))
                   ;; refresh saves bt overrides to vals.lisp and does one visible
                   ;; load, producing a single parameter display showing backtracking.
                   (refresh)
                   (incf problems-processed)
                   (ww-solve)
                   (let ((solution-data (collect-solution-data)))
                     (unless (equalp solution-data
                                     (gethash problem-name problem-test-solutions))
                       (format t "~%The problem solution above does not match the expected solution:")
                       (format t "~%~A~2%" (gethash problem-name problem-test-solutions))
                       (push problem-name failed-problems))
                     (unless (probe-file test-solutions-file)
                       (setf (gethash problem-name problem-test-solutions)
                             solution-data))
                     t))))
      (cleanup-test-files)
      (stage blocks3)
      (format t "~%~%Final Summary:~%")
      (format t "Total test problems run: ~D~%" (length *test-bt-problem-files*))
      (format t "Test failures: ~D~%" (length failed-problems))
      (format t "Failed problems: ~A~%" (reverse failed-problems))
      (format t "Note: A failed BT solution is not necessarily wrong, but different from the reference solution.~%")
      (progn (unless (probe-file test-solutions-file)
               (write-hash-table-to-file problem-test-solutions test-solutions-file))
             t)
      t)))


(defun test-bt ()
  "Run standard test suite using backtracking search."
  (run-bt-test-problems))


;;; START-STATE GOAL TEST ;;;


(defun check-start-is-goal-test (condition control &rest args)
  "Signal a focused start-is-goal test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control args)))
  t)


(defun run-start-is-goal-case (algorithm tree-or-graph threads solution-type
                               expected-count &key terminal expected-value)
  "Run one start-is-goal configuration and validate its solution records."
  (setf *algorithm* algorithm
        *tree-or-graph* tree-or-graph
        *threads* threads
        *solution-type* solution-type
        *depth-cutoff* 1
        *randomize-search* nil
        *branch* -1
        *symmetry-pruning* nil
        *debug* 0
        *probe* nil
        *auto-wait* nil)
  (ww-solve)
  (let ((root-solution (find 0 *solution-paths* :key #'solution.depth)))
    (check-start-is-goal-test
      root-solution
      "No depth-zero solution for ~A/~A with ~D thread~:P and solution type ~A."
      algorithm tree-or-graph threads solution-type)
    (check-start-is-goal-test
      (null (solution.path root-solution))
      "The depth-zero solution path is not empty: ~S"
      (solution.path root-solution))
    (check-start-is-goal-test
      (equalp (problem-state.idb (solution.goal root-solution))
              (problem-state.idb *start-state*))
      "The depth-zero goal state differs from the initialized start state.")
    (check-start-is-goal-test
      (= (solution.time root-solution) (problem-state.time *start-state*))
      "The depth-zero solution time differs from the start time.")
    (check-start-is-goal-test
      (= (solution.value root-solution) (problem-state.value *start-state*))
      "The depth-zero solution value differs from the start value.")
    (check-start-is-goal-test
      (= (length *solution-paths*) expected-count)
      "Expected ~D solution~:P for ~A/~A/~A, but found ~D."
      expected-count algorithm tree-or-graph solution-type
      (length *solution-paths*))
    (check-start-is-goal-test
      (= (length *unique-solution-states*) expected-count)
      "Expected ~D unique goal state~:P for ~A/~A/~A, but found ~D."
      expected-count algorithm tree-or-graph solution-type
      (length *unique-solution-states*))
    (when terminal
      (check-start-is-goal-test
        (and (= *program-cycles* 0)
             (= *total-states-processed* 1)
             (= *max-depth-explored* 0))
        "A terminal depth-zero solution expanded the search: cycles=~D, states=~D, depth=~D."
        *program-cycles* *total-states-processed* *max-depth-explored*))
    (when expected-value
      (check-start-is-goal-test
        (find expected-value *solution-paths* :key #'solution.value :test #'=)
        "No solution has the expected objective value ~A."
        expected-value)))
  (format t "~&Passed start-is-goal case: ~A / ~A / ~D thread~:P / ~A~%"
          algorithm tree-or-graph threads solution-type)
  t)


(defun test-start-is-goal ()
  "Verify depth-zero goal handling across solution types and search engines."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-start-is-goal-test.lisp")
        (run-start-is-goal-case 'depth-first 'graph 0 'first 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-length 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-time 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 1 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 2 2)
        (run-start-is-goal-case 'depth-first 'graph 0 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 0 'all-paths 3)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-value 2
                                :expected-value -10)
        (run-start-is-goal-case 'depth-first 'graph 0 'max-value 2
                                :expected-value 10)
        (run-start-is-goal-case 'backtracking 'tree 0 'first 1 :terminal t)
        (run-start-is-goal-case 'backtracking 'tree 0 2 2)
        (run-start-is-goal-case 'backtracking 'tree 0 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 2 'first 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 2 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 2 'min-value 3
                                :expected-value -10)
        (run-start-is-goal-case 'depth-first 'graph 2 'max-value 3
                                :expected-value 10)
        (format t "~2&All start-is-goal cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; INITIALIZATION CHECK TEST ;;;


(defun check-init-check-test (condition control &rest arguments)
  "Signal a focused initialization-check test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control arguments)))
  t)


(defun capture-init-check-test-condition (literal)
  "Run initialization validation for LITERAL and return any signaled error."
  (handler-case
      (progn
        (validate-init-literals (list literal))
        nil)
    (error (condition)
      condition)))


(defun test-init-check ()
  "Verify initialization-check ordering, attribution, error transparency, and reset."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-init-check-test.lisp")
        (check-init-check-test
          (equal *init-checks*
                 '(first-init-check
                   second-init-check
                   erroneous-init-check))
          "Initialization checks were registered in the wrong order: ~S"
          *init-checks*)
        (check-init-check-test
          (and (member 'init-check-marker-value
                       *problem-function-names*)
               (member 'first-init-check
                       *problem-function-names*))
          "Initialization-check helper lifecycle metadata is incomplete: ~S"
          *problem-function-names*)
        (check-init-check-test
          (equal (get 'first-init-check :init-check-consumed-types)
                 '(init-check-token))
          "Initialization-check consumed-type metadata is incorrect: ~S"
          (get 'first-init-check :init-check-consumed-types))
        (let ((condition
                (capture-init-check-test-condition
                  '(init-check-marker rejected-by-both))))
          (check-init-check-test
            (typep condition 'init-check-failure)
            "The first rejection did not signal INIT-CHECK-FAILURE: ~S"
            condition)
          (check-init-check-test
            (eql (init-check-failure-check condition)
                 'first-init-check)
            "The first rejection was attributed to ~S."
            (init-check-failure-check condition))
          (check-init-check-test
            (equal (init-check-failure-literal condition)
                   '(init-check-marker rejected-by-both))
            "The first rejection reported the wrong literal: ~S"
            (init-check-failure-literal condition)))
        (let ((condition
                (capture-init-check-test-condition
                  '(init-check-marker rejected-by-second))))
          (check-init-check-test
            (and (typep condition 'init-check-failure)
                 (eql (init-check-failure-check condition)
                      'second-init-check))
            "The second-only rejection was not attributed correctly: ~S"
            condition))
        (let ((condition
                (capture-init-check-test-condition
                  '(init-check-marker coding-error))))
          (check-init-check-test
            (and (typep condition 'error)
                 (not (typep condition 'init-check-failure))
                 (search "Deliberate initialization-check coding error."
                         (princ-to-string condition)))
            "A coding error was hidden or converted into an authoring failure: ~S"
            condition))
        (stage blocks3)
        (check-init-check-test
          (null *init-checks*)
          "Initialization-check registrations leaked into BLOCKS3: ~S"
          *init-checks*)
        (check-init-check-test
          (null *problem-function-names*)
          "Problem-function lifecycle metadata leaked into BLOCKS3: ~S"
          *problem-function-names*)
        (dolist
            (check
              '(init-check-marker-value
                first-init-check
                second-init-check
                erroneous-init-check))
          (check-init-check-test
            (not (fboundp check))
            "Initialization-check function ~S leaked into BLOCKS3."
            check))
        (format t "~2&All initialization-check cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; TEST CHARACTERIZATION LIFECYCLE TEST ;;;


(defun check-characterization-test (condition control &rest arguments)
  "Signal a focused characterization test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control arguments)))
  t)


(defun capture-characterization-condition (function)
  "Call FUNCTION and return any attributed characterization failure."
  (handler-case
      (progn
        (funcall function)
        nil)
    (test-claim-failure (condition)
      condition)))


(defun test-characterization ()
  "Verify claim ordering, clause attribution, assertions, and lifecycle cleanup."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-test-claim-test.lisp")
        (check-characterization-test
          (equal *test-claims* '(engine-test-claim-contract))
          "Test claims were registered in the wrong order: ~S"
          *test-claims*)
        (check-characterization-test
          (and (member 'engine-test-claim-helper *problem-function-names*)
               (member 'engine-test-claim-contract *problem-function-names*))
          "Test helper lifecycle metadata is incomplete: ~S"
          *problem-function-names*)
        (run-test-claims)
        (let* ((*debug* 1)
               (condition
                 (capture-characterization-condition
                   'run-test-claims)))
          (check-characterization-test
            (typep condition 'test-claim-failure)
            "A false claim clause did not signal TEST-CLAIM-FAILURE: ~S"
            condition)
          (check-characterization-test
            (eql (test-claim-failure-claim condition)
                 'engine-test-claim-contract)
            "The false clause was attributed to ~S."
            (test-claim-failure-claim condition))
          (check-characterization-test
            (equal (test-claim-failure-clause condition)
                   '(zerop *debug*))
            "The wrong clause was attributed: ~S"
            (test-claim-failure-clause condition)))
        (let ((*debug* 1))
          (check-characterization-test
            (talos-problem-failed-p 'engine-test-claim-test)
            "TEST-TALOS did not record an attributed claim failure."))
        (stage blocks3)
        (check-characterization-test
          (null *test-claims*)
          "Test claims leaked into BLOCKS3: ~S"
          *test-claims*)
        (dolist (function
                  '(engine-test-claim-helper engine-test-claim-contract))
          (check-characterization-test
            (not (fboundp function))
            "Test function ~S leaked into BLOCKS3."
            function))
        (format t "~2&All test-characterization cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; DERIVED RELATION TEST ;;;


(defun check-derived-relation-test (condition control &rest arguments)
  "Signal a focused derived-relation test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control arguments)))
  t)


(defun capture-derived-relation-test-condition (function)
  "Call FUNCTION and return any signaled error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error (condition)
      condition)))


(defun test-derived-relations ()
  "Verify derived-relation declaration, init rejection, idempotence, and reset."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-derived-relation-test.lisp")
        (check-derived-relation-test
          (and (= (hash-table-count *derived-relations*) 1)
               (gethash 'computed-marker *derived-relations*))
          "Derived-relation metadata was installed incorrectly: ~S"
          (alexandria:hash-table-alist *derived-relations*))
        (dolist (literal
                  '((computed-marker computed)
                    (not (computed-marker computed))))
          (let ((condition
                  (capture-derived-relation-test-condition
                    (lambda ()
                      (validate-init-literals (list literal))))))
            (check-derived-relation-test
              (and (typep condition 'error)
                   (search "DEFINE-INIT contains a derived fact"
                           (princ-to-string condition)))
              "Derived literal was not rejected: ~S"
              literal)))
        (let ((condition
                (capture-derived-relation-test-condition
                  (lambda ()
                    (install-derived-relations '(undeclared-marker))))))
          (check-derived-relation-test
            (and (typep condition 'error)
                 (search "declared dynamic relation"
                         (princ-to-string condition)))
            "An undeclared derived relation was not rejected: ~S"
            condition))
        (let ((condition
                (capture-derived-relation-test-condition
                  (lambda ()
                    (install-derived-relations '(static-marker))))))
          (check-derived-relation-test
            (and (typep condition 'error)
                 (search "declared dynamic relation"
                         (princ-to-string condition)))
            "A static derived relation was not rejected: ~S"
            condition))
        (install-derived-relations '(computed-marker))
        (check-derived-relation-test
          (= (hash-table-count *derived-relations*) 1)
          "Repeating a derived declaration changed the registry: ~S"
          (alexandria:hash-table-alist *derived-relations*))
        (stage blocks3)
        (check-derived-relation-test
          (zerop (hash-table-count *derived-relations*))
          "Derived-relation metadata leaked into BLOCKS3: ~S"
          (alexandria:hash-table-alist *derived-relations*))
        (format t "~2&All derived-relation cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; CANDIDATE SOLUTION VALIDATOR TEST ;;;


(defun check-solution-validator-test (condition control &rest args)
  "Signal a focused candidate-validator test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control args)))
  t)


(defun solution-validator-accepted-goal-p (state)
  "Return true when STATE contains the repaired test goal."
  (member '(validator-at validator-accepted)
          (list-database (problem-state.idb state))
          :test #'equal))


(defun run-solution-validator-case
    (algorithm tree-or-graph threads solution-type)
  "Run one search configuration and require the rejected depth-one goal to be repaired."
  (setf *algorithm* algorithm
        *tree-or-graph* tree-or-graph
        *threads* threads
        *solution-type* solution-type
        *depth-cutoff* 2
        *randomize-search* nil
        *branch* -1
        *symmetry-pruning* nil
        *debug* 0
        *probe* nil
        *auto-wait* nil)
  (ww-solve)
  (check-solution-validator-test
    (= *nominal-solution-candidates*
       (+ *accepted-solution-candidates* *rejected-solution-candidates*))
    "Candidate-validation totals do not balance: ~D checked, ~D accepted, ~D rejected."
    *nominal-solution-candidates*
    *accepted-solution-candidates*
    *rejected-solution-candidates*)
  (check-solution-validator-test
    (plusp *accepted-solution-candidates*)
    "The accepted candidate was not counted.")
  (check-solution-validator-test
    (plusp
      (gethash
        '(accept-only-repaired-validator :unspecified :repair-required)
        *solution-validator-rejections*
        0))
    "The rejected candidate diagnostic was not grouped by validator and reason.")
  (let ((report
          (with-output-to-string (stream)
            (print-candidate-solution-validation-statistics stream))))
    (check-solution-validator-test
      (and (search "Candidate solution validation:" report)
           (search "Nominal goal paths checked" report)
           (search "REPAIR-REQUIRED" report))
      "The candidate-validation report is incomplete:~%~A"
      report))
  (check-solution-validator-test
    *solution-paths*
    "No validated solution for ~A/~A with ~D thread~:P and solution type ~A."
    algorithm tree-or-graph threads solution-type)
  (dolist (solution *solution-paths*)
    (check-solution-validator-test
      (= (solution.depth solution) 2)
      "A rejected candidate was recorded at depth ~D: ~S"
      (solution.depth solution) (solution.path solution))
    (check-solution-validator-test
      (solution-validator-accepted-goal-p (solution.goal solution))
      "A recorded solution does not contain the repaired goal: ~S"
      (list-database (problem-state.idb (solution.goal solution)))))
  (format t "~&Passed solution-validator case: ~A / ~A / ~D thread~:P / ~A~%"
          algorithm tree-or-graph threads solution-type)
  t)


(defun test-solution-validator ()
  "Verify candidate validation, rejected-goal expansion, and reusable action replay."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-solution-validator-test.lisp")
        (reset-candidate-solution-validation-statistics)
        (let ((empty-report
                (with-output-to-string (stream)
                  (print-candidate-solution-validation-statistics stream))))
          (check-solution-validator-test
            (search "No path reached the problem goal." empty-report)
            "The zero-candidate validation report is incomplete:~%~A"
            empty-report))
        (let ((result
                (validate-action-sequence
                  *start-state*
                  '((advance-to-rejected-goal)
                    (repair-rejected-goal))
                  :goal-test #'solution-validator-accepted-goal-p)))
          (check-solution-validator-test
            (action-sequence-validation-success-p result)
            "The reusable action-sequence validator rejected the valid repair path: ~S"
            (action-sequence-validation-failure-reason result))
          (check-solution-validator-test
            (action-sequence-validation-goal-satisfied-p result)
            "The reusable action-sequence validator did not recognize the repaired goal."))
        (run-solution-validator-case 'depth-first 'graph 0 'min-length)
        (run-solution-validator-case 'backtracking 'tree 0 'first)
        (run-solution-validator-case 'depth-first 'graph 2 'min-length)
        (run-solution-validator-case 'depth-first 'graph 0 'all-paths)
        (format t "~2&All candidate solution-validator cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; SIMPLE DEPTH-FIRST VS BACKTRACKING BENCHMARK ;;;


(defun bench-depth&back (problem-name depth-cutoff &key (solution-type 'first))
  "Runs depth-first and backtracking once each on PROBLEM-NAME.
   Enforces tree mode and identical depth cutoff/settings for both runs.
   Returns plist of the two result rows."
  (let* ((problem-str (string-downcase (string problem-name)))
         (results nil))
    (format t "~%========================================~%")
    (format t "BENCH DEPTH-FIRST VS BACKTRACKING~%")
    (format t "problem=~A depth-cutoff=~D solution-type=~A~%"
            (string-upcase problem-str) depth-cutoff solution-type)
    (format t "========================================~%")
    (dolist (algorithm '(depth-first backtracking))
      (%stage problem-str)
      (setf *algorithm* algorithm
            *tree-or-graph* 'tree
            *depth-cutoff* depth-cutoff
            *solution-type* solution-type
            *threads* 0
            *randomize-search* nil
            *debug* 0
            *probe* nil)
      ;; Keep native CSP problems as CSP; otherwise default to planning.
      (unless (member *problem-type* '(planning csp))
        (setf *problem-type* 'planning))
      (refresh)
      (let ((start (get-internal-real-time)))
        (ww-solve)
        (let ((row (list :algorithm algorithm
                         :problem-type *problem-type*
                         :elapsed (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)
                         :cycles *program-cycles*
                         :states *total-states-processed*
                         :max-depth *max-depth-explored*
                         :solutions (length *solution-paths*)
                         :unique-solutions (length *unique-solution-states*))))
          (push row results)
          (format t "~%~A: elapsed=~,6F s cycles=~D states=~D max-depth=~D solutions=~D unique=~D type=~A~%"
                  algorithm
                  (getf row :elapsed)
                  (getf row :cycles)
                  (getf row :states)
                  (getf row :max-depth)
                  (getf row :solutions)
                  (getf row :unique-solutions)
                  (getf row :problem-type)))))
    (setf results (nreverse results))
    (let* ((dfs (find 'depth-first results :key (lambda (r) (getf r :algorithm))))
           (bt (find 'backtracking results :key (lambda (r) (getf r :algorithm))))
           (dfs-elapsed (getf dfs :elapsed))
           (bt-elapsed (getf bt :elapsed))
           (speedup (if (> bt-elapsed 0.0) (/ dfs-elapsed bt-elapsed) 0.0)))
      (format t "~%----------------------------------------~%")
      (format t "Summary: depth-first=~,6F s backtracking=~,6F s speedup(df/bt)=~,3Fx~%"
              dfs-elapsed bt-elapsed speedup)
      (format t "----------------------------------------~%"))
    results))
