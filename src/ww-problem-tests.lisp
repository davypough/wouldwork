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
    "problem-tiles0a-csp.lisp" "problem-tiles1a.lisp" "problem-tiles1a-heuristic.lisp"
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


;Any additions to this list requires rebuilding problem-test-bt-solutions.lisp
;in the test-bt function below.
(defvar *test-bt-problem-files*
  '("problem-blocks3.lisp" "problem-blocks3a.lisp" "problem-blocks4.lisp" "problem-boxes.lisp"
    "problem-jugs2.lisp" "problem-jugs4.lisp" "problem-queens4.lisp" "problem-queens8.lisp"
    "problem-captjohn.lisp" "problem-quern.lisp" "problem-graveyard.lisp" "problem-sentry.lisp"
    ;"problem-crossword5-11.lisp"  ;runs out of default memory
    ;"problem-crossword15-18.lisp"  ;runs out of default memory
    "problem-crossword13.lisp" "problem-array-path.lisp"
    ;"problem-tiles0a-csp.lisp"  ;takes too long
    "problem-tiles1a.lisp" 
    ;"problem-tiles1a-heuristic.lisp"
    ;"problem-tiles1e-heuristic.lisp"
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
    "problem-triangle-xy.lisp" "problem-triangle-xyz.lisp"
    ;"problem-triangle-heuristic.lisp"
    "problem-triangle-macros.lisp" "problem-triangle-macros-one.lisp" "problem-triangle-xyz-one.lisp"
    "problem-tsp.lisp"
    "problem-u2.lisp"
    ;"problem-donald.lisp"
    "problem-knap4a.lisp" "problem-knap4b.lisp"
    ;"problem-crater.lisp"  ;needs debugging
    ;"problem-knap19.lisp"
    ;"problem-socrates1.lisp"  ;needs debugging
    ;"problem-socrates2.lisp"  ;needs debugging
    ;"problem-smallspace-macro.lisp"  ;needs debugging
    ;"problem-smallspace2.lisp"  ;takes too long
    "problem-smallspace.lisp"))


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
  (let ((best-solution (ut::if-it (first *solutions*) (solution.path ut::it)))
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


(defun test-bt ()
  "Run test problems using backtracking algorithm to certify backtracking functionality.
   This parallels the standard test function but forces *algorithm* to backtracking."
  (format t "~%========================================~%")
  (format t "BACKTRACKING ALGORITHM TEST SUITE~%")
  (format t "========================================~%")

  (cleanup-test-files)
  (reset-parameters)

  ;; Verify no parallel processing
  (when (> *threads* 0)
    (error "Backtracking test requires *threads* = 0. Please reset in ww-settings.lisp"))

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
                       (#\Q (return-from test-bt nil))
                       (#\S (setf should-process nil))
                       (#\A (setf continue-all t))
                       (#\C nil)
                       (t nil))))

                 (when should-process
                   ;; Custom loading sequence for backtracking
                   (reset-parameters)

                   ;; Stage the problem first (loads with defaults)
                   (exchange-problem-file problem-name)

                   ;; Set all parameters BEFORE any recompilation
                   (setf *problem-name* (intern problem-name)
                         *algorithm* 'backtracking
                         *tree-or-graph* 'tree)  ; Required for backtracking

                   ;; Set appropriate depth cutoff based on problem type
                   (setf *depth-cutoff*
                         (cond
                           ;; CSP problems need more depth
                           ((member problem-name '("donald" "queens4" "queens8"
                                                  "knap4a" "knap4b" "knap19")
                                    :test #'string-equal)
                            20)
                           ;; Simple planning problems
                           ((member problem-name '("blocks3" "blocks3a" "blocks4" "jugs2" "jugs4")
                                    :test #'string-equal)
                            8)
                           ;; Puzzle problems
                           ((member problem-name '("captjohn" "quern" "graveyard" "sentry" "boxes")
                                    :test #'string-equal)
                            15)
                           ;; Complex problems
                           ((member problem-name '("tiles1a-heuristic" "tiles1e-heuristic"
                                                  "triangle-xy" "triangle-xyz" "triangle-heuristic"
                                                  "triangle-xyz-one" "u2" "crossword13")
                                    :test #'string-equal)
                            25)
                           ;; Default for others
                           (t 10)))

                   ;; Save parameters and trigger ONE recompilation with backtracking
                   (save-globals)

                   ;; Load system ONCE with saved backtracking parameters
                   (with-silenced-compilation
                     (asdf:load-system :wouldwork :force t))

                   ;; Verify algorithm is still backtracking after load
                   (unless (eq *algorithm* 'backtracking)
                     (format t "~%WARNING: Algorithm reset to ~A. Forcing back to backtracking.~%"
                             *algorithm*)
                     ;; Force correction without another ASDF reload
                     (setf *algorithm* 'backtracking)
                     (save-globals))

                   (incf problems-processed)

                   ;; Run the solver with error handling
                   (handler-case
                       (ww-solve)
                     (error (e)
                       (format t "~%ERROR solving ~A with backtracking: ~A~%"
                               problem-name e)
                       (push problem-name failed-problems)))

                   ;; Collect and compare results
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

      ;; Cleanup and restore defaults
      (cleanup-test-files)
      (reset-parameters)
      (stage blocks3)

      ;; Final summary
      (format t "~%~%========================================~%")
      (format t "Backtracking Test Summary~%")
      (format t "========================================~%")
      (format t "Total test problems run: ~D~%" (length *test-bt-problem-files*))
      (format t "Test failures: ~D~%" (length failed-problems))
      (format t "Failed problems: ~A~%" (reverse failed-problems))
      (format t "Note: A failed problem solution is not necessarily wrong, but unexpected in backtracking,")
      (format t "and so should be reanalyzed.")

      ;; Save results
      (progn (unless (probe-file test-solutions-file)
               (write-hash-table-to-file problem-test-solutions test-solutions-file))
             t)
      t)))


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
                         :solutions (length *solutions*)
                         :unique-solutions (length *unique-solutions*))))
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
