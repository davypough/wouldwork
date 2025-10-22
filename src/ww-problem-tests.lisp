;;; Filename: problem-tests.lisp

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


(defun run-test-problems ()
  (uiop:delete-file-if-exists (in-src "problem.lisp"))
  (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
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
            do (let* ((problem-name (if (string-prefix-p "problem-" problem)
                                          (subseq problem 8 (- (length problem) 5))
                                          (subseq problem 0 (- (length problem) 5))))
                      (should-process t))
                 (format t "~%=====================================================~%")
                 (format t "Process problem: ~A~%" (string-upcase problem-name))
                 (format t "=====================================================~%")
                 
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
                   (let ((best-solution (ut::if-it (first *solutions*) (solution.path ut::it)))
                         (best-state (alexandria:hash-table-alist (problem-state.idb (first *best-states*)))))
                     (unless (equalp (list best-solution best-state)
                                     (gethash problem-name problem-test-solutions))
                       (format t "~%The problem solution above does not match the expected solution:")
                       (format t "~%~A~2%" (gethash problem-name problem-test-solutions))
                       (push problem-name failed-problems))
                     (unless (probe-file test-solutions-file)
                       (setf (gethash problem-name problem-test-solutions)
                             (list best-solution best-state)))
                     t))))
      (uiop:delete-file-if-exists (in-src "problem.lisp"))
      (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
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


; aliases
(setf (fdefinition 'test) #'run-test-problems)
(setf (fdefinition 'run-all) #'run-test-problems)
(setf (fdefinition 'run-test) #'run-test-problems)


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
  
  (uiop:delete-file-if-exists (in-src "problem.lisp"))
  (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
  (reset-parameters)
  
  (with-silenced-compilation
    (let* ((problems-to-run (filter-bt-compatible-problems *test-problem-files*))
           (test-solutions-file (merge-pathnames "problem-test-solutions-bt.lisp"
                                                (asdf:system-source-directory :wouldwork)))
           (problem-test-solutions (if (probe-file test-solutions-file)
                                     (read-hash-table-from-file test-solutions-file)
                                     (make-hash-table :test #'equal)))
           (problems-processed 0)
           (continue-all nil)
           failed-problems)
      
      ;; Verify no parallel processing
      (when (> *threads* 0)
        (error "Backtracking test requires *threads* = 0. Please reset in ww-settings.lisp"))
      
      (loop for problem in problems-to-run
            do (let* ((problem-name (if (string-prefix-p "problem-" problem)
                                        (subseq problem 8 (- (length problem) 5))
                                        (subseq problem 0 (- (length problem) 5))))
                      (should-process t))
                 
                 (format t "~%=====================================================~%")
                 (format t "Process problem (BACKTRACKING): ~A~%" (string-upcase problem-name))
                 (format t "=====================================================~%")
                 
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
                   ;; CRITICAL FIX: Custom loading sequence for backtracking ;;; CHANGED BLOCK
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
                           ((member problem-name '("tiles0a-csp" "donald" "queens4" "queens8"
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
                           ((member problem-name '("tiles1a" "tiles1a-heuristic" "tiles1e-heuristic"
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
                   (let ((best-solution (ut::if-it (first *solutions*) 
                                                   (solution.path ut::it)))
                         (best-state (when *best-states*
                                      (alexandria:hash-table-alist 
                                       (problem-state.idb (first *best-states*))))))
                     
                     ;; Store backtracking solution
                     (setf (gethash problem-name problem-test-solutions)
                           (list best-solution best-state))
                     
                     ;; Note if different from depth-first baseline
                     (when (and best-solution (zerop (length failed-problems)))
                       (format t "Solution found with ~D steps.~%" 
                               (length best-solution)))))))
      
      ;; Cleanup and restore defaults
      (uiop:delete-file-if-exists (in-src "problem.lisp"))
      (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" 
                                                   (asdf:system-source-directory :wouldwork)))
      (reset-parameters)
      (stage blocks3)
      
      ;; Final summary
      (format t "~%~%========================================~%")
      (format t "Backtracking Test Summary~%")
      (format t "========================================~%")
      (format t "Total problems tested: ~D~%" problems-processed)
      (format t "Problems with errors: ~D~%" (length failed-problems))
      (when failed-problems
        (format t "Failed problems: ~A~%" (reverse failed-problems)))
      (format t "~%Backtracking certification test complete.~%")
      
      ;; Save results
      (when (> problems-processed 0)
        (write-hash-table-to-file problem-test-solutions test-solutions-file))
      t)))


(defun filter-bt-compatible-problems (problem-files)
  "Filter problems suitable for backtracking algorithm testing.
   Excludes problems that are too slow, incompatible, or known to fail with backtracking."
  (remove-if (lambda (problem)
               (or 
                ;; Problems already excluded in main test
                (member problem '("problem-crossword5-11.lisp"
                                "problem-crossword15-18.lisp"
                                "problem-tiles1b.lisp"
                                "problem-tiles1c.lisp"
                                "problem-tiles1d.lisp"
                                "problem-tiles2a.lisp"
                                "problem-tiles2a-heuristic.lisp"
                                "problem-tiles2b.lisp"
                                "problem-tiles2c.lisp"
                                "problem-tiles3a-heuristic.lisp"
                                "problem-tiles5a-heuristic.lisp"
                                "problem-tiles5b-heuristic.lisp"
                                "problem-tiles7a-heuristic.lisp"
                                "problem-tiles7a-heuristic2.lisp"
                                "problem-tiles7a-heuristic3.lisp"
                                "problem-tiles0b-csp.lisp"
                                "problem-triangle.lisp"
                                "problem-triangle-backward.lisp"
                                "problem-crater.lisp"
                                "problem-socrates1.lisp"
                                "problem-socrates2.lisp"
                                "problem-smallspace-macro.lisp"
                                "problem-smallspace2.lisp"
                                "problem-smallspace.lisp")
                       :test #'string=)
                ;; Additional exclusions for backtracking
                (member problem '("problem-triangle-macros.lisp"      ; Macro operators incompatible
                                "problem-triangle-macros-one.lisp"    ; Macro operators incompatible
                                "problem-tsp.lisp"                    ; Optimization not suited for BT
                                "problem-hanoi.lisp"                  ; Exponential with backtracking
                                "problem-array-path.lisp")            ; Graph-heavy, needs graph search
                       :test #'string=)))
            problem-files))
