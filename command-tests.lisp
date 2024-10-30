;;; Filename: command-tests.lisp

;;; Runs a series of tests of various user commands from the REPL.
;;; First load wouldwork with (progn (ql:quickload :wouldwork :force t) (in-package :ww))
;;; Then enter (test-commands) to start the tests.


(defparameter *command-tests-passed* 0)
(defparameter *command-tests-failed* 0)


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
