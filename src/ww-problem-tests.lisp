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
  (uiop:delete-file-if-exists (in-src "problem.lisp"))
  (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))


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
                   (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
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


(defun write-hash-table-to-file (hash-table filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print hash-table out))))


(defun read-hash-table-from-file (filename)
  (with-open-file (in filename :direction :input)
    (with-standard-io-syntax
      (read in))))


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
                   (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
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
