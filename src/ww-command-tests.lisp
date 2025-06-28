;;; Filename: command-tests.lisp

;;; Runs a series of tests of various user commands from the REPL.
;;; First load wouldwork with (progn (ql:quickload :wouldwork :force t) (in-package :ww))
;;; Then enter (test-commands) to start the tests.


(in-package :ww)


(defun test-commands ()
  "Run a series of tests to exercise potential user REPL commands."
  (command-test-1)
  (command-test-2)
  (command-test-3)
  (command-test-4)
  (format t "~%All command tests completed. Check printout to verify details.")
  t)


(defun command-test-1 ()
  "Verify default blocks3 runs properly if no prior problem.lisp or vals.lisp"
  (format t "~%COMMAND-TEST-1: testing (run blocks3)...~2%")
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root))
         (problem-name-str (string *problem-name*)))
    (uiop:delete-file-if-exists vals-file)
    (uiop:delete-file-if-exists problem-file)
    (run blocks3)
    (format t "COMMAND-TEST-1 completed successfully.~2%")
    ;(assert (string-equal problem-name-str "blocks3"))
    t))


(defun command-test-2 ()
  "Verify wouldwork properly switches to a new problem"
  (format t "~%COMMAND-TEST-2: switching to a new problem...~2%")
  (if (string-equal (string *problem-name*) "blocks3")
    (progn (run blocks4)
           (assert (string-equal (string *problem-name*) "blocks4")))
    (progn (run blocks3)
           (assert (string-equal (string *problem-name*) "blocks3"))))
  (format t "COMMAND-TEST-2 completed successfully.~2%")
  t)


(defun command-test-3 ()
  "Verify proper notification if requested problem file does not exist"
  (format t "~%COMMAND-TEST-3: testing when problem file does not exist...~2%")
  (let* ((output (detect-output (lambda () (%stage "non-existent-problem"))))
         ;; Normalize the expected string to use just LF endings
         (expected (with-output-to-string (s)
                    (loop for char across "The problem non-existent-problem was not found.
Enter (list-all-problems) for a complete list of problems."
                          do (case (char-code char)
                               (#.(char-code #\Return)) ; skip CR
                               (otherwise (write-char char s)))))))
    (unless (string-equal output expected)
      (format t "Incorrect notification of non-existent problem:~%~S" output)))
  (format t "COMMAND-TEST-3 completed successfully.~2%")
  t)


(defun command-test-4 ()
  "Verify basic ww-set for *depth-cutoff* *tree-or-graph* *solution-type*
   *progress-reporting-interval* *randomize-search* *branch* *probe* *debug*"
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (format t "~%COMMAND-TEST-4: testing that all parameters reset to defaults...~2%")
  (reset-parameters)
  (assert (not (member :ww-debug *features*)))
  (format t "~&Testing that blocks3 loads and runs with baseline parameters...~2%")
  (run blocks3)  ;verify blocks3 runs successfully before testing commands
  (format t "~%Testing ww-set command adjustments for blocks3...~2%")
  (setf *ww-loading* t)
  (ww-set *problem-name* blocks3)
  (setf *ww-loading* nil)
  (ww-set *depth-cutoff* 2)
  (ww-set *tree-or-graph* graph)
  (ww-set *solution-type* first)
  (ww-set *progress-reporting-interval* 10)
  (ww-set *randomize-search* t)
  (ww-set *branch* 4)
  (ww-set *probe* (put (C A) 3))  ;produces a compiler note, troubleshoot later
  (ww-set *debug* 1)
  (format t "~&Testing that blocks3 runs successfully with new parameters...~2%")
  (assert (and (eq *problem-name* 'blocks3) (= *depth-cutoff* 2) (eq *tree-or-graph* 'graph)
               (eq *solution-type* 'first) (= *progress-reporting-interval* 10)
               (eq *randomize-search* t) (= *branch* 4) (equal *probe* '(put (C A) 3))  ;compiler note
               (= *debug* 1)))
  (assert (member :ww-debug *features*))
  (let ((vals-file (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))
    (with-open-file (in-file vals-file :direction :input)
      (assert (equal (read in-file nil nil)
                     '(blocks3 2 graph first 10 t 4 (put (C A) 3) 1))))
    (delete-file vals-file))
  (solve)  ;solve with new parameters
  (format t "~%Resetting all parameters to defaults...")
  (reset-parameters)
  (format t "~&Restaging blocks3...~2%")
  (stage blocks3)  ;reset all blocks3 parameters
  (format t "COMMAND-TEST-4 completed successfully.~2%")
  t)


(defun detect-output (lambda-expr)
  "If a function prints to *standard-output*, then capture the output, otherwise return nil.
   Returns a fresh simple string to ensure consistent behavior."
  (let* ((str (make-string-output-stream))
         (*standard-output* str))
    (funcall lambda-expr)
    (let ((output (get-output-stream-string str)))
      (when (> (length output) 0)
        ;; Create and return a fresh simple string
        (let ((result (make-string (length output))))
          (replace result output)
          result)))))
