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
