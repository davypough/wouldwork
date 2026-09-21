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
    ;"problem-tiles5b.lisp"  ;needs debugging
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
    ;"problem-queens8.lisp"              ;redundant with queens4
    "problem-captjohn.lisp"              ;csp, variable-per-level assignment
    "problem-quern.lisp"                ;first, depth-cutoff 8, conditional assert
    ;"problem-graveyard.lisp"            ;min-length on tree requires exhausting 12^14 nodes; no solution within native depth-cutoff 10
    ;"problem-sentry.lisp"              ;has define-happening, incompatible with bt
    "problem-crossword13.lisp"          ;tree, first, string state, nested updates
    "problem-array-path.lisp"           ;tree, min-length, no-solution case
    ;"problem-tiles0a-csp.lisp"         ;takes too long
    ;"problem-tiles1a.lisp"             ;graph->tree, min-length, list-coord state--takes too long with bt
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
  "Print test header for problem"
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
      (format t "a common occurrence when running in parallel mode." )
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

(defvar *expected-search-status* nil
  "Test-only expected SEARCH-OUTCOME status for a characterization problem.
   NIL retains the ordinary Talos requirement that the problem produce a solution.")

(defvar *expected-search-reason* nil
  "Test-only expected SEARCH-OUTCOME reason.  Checked only when
   *EXPECTED-SEARCH-STATUS* is non-NIL.")


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
    (setf *expected-min-length* nil
          *expected-search-status* nil
          *expected-search-reason* nil)
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
  "Restage a selected mutation, distinguishing installation errors from detection."
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
  "Run the mutated claims and search, returning true when either detects it."
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
      (format t "~%SURVIVING MUTANT: ~A still solves correctly with ~A active.~%"
              problem-name mutation-name)
      nil)))


(defun talos-problem-failed-p (problem-name)
  "Run staged claims and search, reporting attributed characterization failures.
   Problems may explicitly declare an expected no-solution search outcome."
  (handler-case
      (progn
        (run-test-claims)
        (ww-solve)
        (cond
          ((and *expected-search-status*
                (not (eq (search-outcome-status *last-search-outcome*)
                          *expected-search-status*)))
           (format t "~%Talos test ~A produced status ~A, expected ~A.~%"
                   problem-name
                   (search-outcome-status *last-search-outcome*)
                   *expected-search-status*)
           t)
          ((and *expected-search-status*
                *expected-search-reason*
                (not (eq (search-outcome-reason *last-search-outcome*)
                          *expected-search-reason*)))
           (format t "~%Talos test ~A produced reason ~A, expected ~A.~%"
                   problem-name
                   (search-outcome-reason *last-search-outcome*)
                   *expected-search-reason*)
           t)
          ((and (null *expected-search-status*)
                (not *solution-paths*))
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
   attributed claim failure, unexpected no-solution result, or wrong solved length
   is recorded; intentional expected outcomes are checked against SEARCH-OUTCOME."
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
            (setf *expected-min-length* nil
                  *expected-search-status* nil
                  *expected-search-reason* nil)
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
