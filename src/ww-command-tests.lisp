;;; Filename: command-tests.lisp

;;; Runs a series of tests of various user commands from the REPL.
;;; First load wouldwork with (progn (ql:quickload :wouldwork :force t) (in-package :ww))
;;; Then enter (test-commands) to start the tests.


(in-package :ww)


(defun command-test-1 ()
  "Verify default blocks3 runs properly if no prior problem.lisp or vals.lisp"
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root))
         (problem-name-str (string *problem-name*)))
    (uiop:delete-file-if-exists vals-file)
    (uiop:delete-file-if-exists problem-file)
    (run blocks3)
    (assert (string-equal problem-name-str "blocks3"))
    t))


(defun command-test-2 ()
  "Verify problem properly switches to new problem"
  (if (string-equal (string *problem-name*) "blocks3")
    (progn (run blocks4)
           (assert (string-equal (string *problem-name*) "blocks4")))
    (progn (run blocks3)
           (assert (string-equal (string *problem-name*) "blocks3"))))
  t)


(defun command-test-3 ()
  "Verify proper notification if requested problem file does not exist"
  (let* ((output (detect-output (lambda () (%stage "non-existent-problem"))))
         ;; Normalize the expected string to use just LF endings
         (expected (with-output-to-string (s)
                    (loop for char across "The problem non-existent-problem was not found.
Enter (list-all-problems) for a complete list of problems."
                          do (case (char-code char)
                               (#.(char-code #\Return)) ; skip CR
                               (otherwise (write-char char s)))))))
    (unless (string-equal output expected)
      (error "Incorrect notification of non-existent problem:~%~S" output)))
  t)


(defun command-test-4 ()
  "Verify basic ww-set for *depth-cutoff* *tree-or-graph* *solution-type*
   *progress-reporting-interval* *randomize-search* *branch* *probe* *debug*"
  (format t "~%Resetting all parameters to defaults...")
  (reset-parameters)
  (format t "~&Verifying that blocks3 loads and runs normally...~2%")
  (run blocks3)  ;verify blocks3 runs successfully before testing commands
  (format t "~&Verifying that blocks3 runs successfully with new parameters...~2%")
  (setf *ww-loading* t)
  (ww-set *problem-name* blocks3)
  (setf *ww-loading* nil)
  (ww-set *depth-cutoff* 2)
  (ww-set *tree-or-graph* graph)
  (ww-set *solution-type* first)
  (ww-set *progress-reporting-interval* 10)
  (ww-set *randomize-search* t)
  (ww-set *branch* 4)
  ;(ww-set *probe* (put (C A) 3))  ;produces a compiler note, troubleshoot later
  (ww-set *debug* 1)
  (assert (and (eq *problem-name* 'blocks3) (= *depth-cutoff* 2) (eq *tree-or-graph* 'graph)
               (eq *solution-type* 'first) (= *progress-reporting-interval* 10)
               (eq *randomize-search* t) (= *branch* 4) ;(equal *probe* '(put (C A) 3))  ;compiler note
               (= *debug* 1)))
  (let ((vals-file (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))
    (with-open-file (in-file vals-file :direction :input)
      (assert (equal (read in-file nil nil)
                     '(blocks3 2 graph first 10 t 4 nil 1))))
    (delete-file vals-file))
  (solve)  ;solve with new parameters
  (format t "~%Resetting all parameters to defaults...")
  (reset-parameters)
  (format t "~&Restaging blocks3...~2%")
  (stage blocks3))  ;reset all blocks3 parameters


#+ignore (defun command-test-5 ()
  "Verify detection of incorrect setting for the basic parameter *tree-or-graph*"
  (run blocks3)
  (let* ((output (detect-output (lambda () (ww-set *tree-or-graph* alternate))))
         ;; Normalize the expected string to use just LF endings
         (expected (with-output-to-string (s)
                    (loop for char across "Can't set *tree-or-graph* to ALTERNATE. Must be either tree or graph."
                          do (case (char-code char)
                               (#.(char-code #\Return)) ; skip CR
                               (otherwise (write-char char s)))))))
    (unless (string-equal output expected)
      (error "Incorrect ww-set of *tree-or-graph* not detected:~%~S" output)))
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

#|
(defmacro with-test (name &body body)
  `(progn
     (format t "~%Running test: ~A~%" ,name)
     (handler-case
         (progn
           ,@body
           (incf *command-tests-passed*)
           (format t "~A: Passed~%" ,name))
       (error (c)
         (incf *command-tests-failed*)
         (format t "~A: Failed - ~A~%" ,name c)))))


(defun test-commands ()
  (setf *command-tests-passed* 0)
  (setf *command-tests-failed* 0)

  (with-test "List available problems"
    (let ((problems (list-all)))
      (unless problems
        (error "No problems found"))
      (unless (member "blocks3" problems :test #'string=)
        (error "Basic test problem blocks3 not found"))))

  (with-test "Parameter setting sequence"
    (ww-set *solution-type* min-length)
    (ww-set *tree-or-graph* tree)
    (unless (eq *solution-type* 'min-length)
      (error "Parameter not set correctly")))

  (with-test "Invalid parameter setting"
    (handler-case
        (progn 
          (ww-set *solution-type* 'invalid-type)
          (error "Should not accept invalid solution type"))
      (error () t)))

  (with-test "Stage and solve blocks3"
    (stage blocks3)
    (solve)
    (unless *solutions*
      (error "No solutions found for blocks3")))

  (with-test "Direct run blocks4" 
    (run blocks4)
    (unless *solutions*
      (error "No solutions found from direct run")))

  (with-test "Switch problems"
    (stage blocks3)
    (solve)
    (let ((blocks3-solns *solutions*))
      (stage blocks4)
      (solve)
      (when (equal blocks3-solns *solutions*)
        (error "Solutions should differ between problems"))))

  (with-test "Debug level changes"
    (ww-set *debug* 0)
    (stage blocks3)
    (solve)
    (ww-set *debug* 1)
    (solve))

  (with-test "Stage nonexistent problem"
    (handler-case
        (progn
          (stage nonexistent-problem)
          (error "Should not stage nonexistent problem"))
      (error () t)))

  ;; Final status
  (format t "~%Command Tests Complete~%")
  (format t "Passed: ~A~%" *command-tests-passed*)
  (format t "Failed: ~A~%" *command-tests-failed*)
  (values))
|#