;;; Filename: command-tests.lisp

;;; Runs a series of tests of various user commands from the REPL.
;;; First load wouldwork with (progn (ql:quickload :wouldwork :force t) (in-package :ww))
;;; Then enter (test-commands) to start the tests.


(in-package :ww)


(defun command-test-resolved-goal-spec ()
  "Return a stable goal spec for command tests."
  (cond
    ((fboundp 'goal-fn) 'goal-fn)
    ((get 'goal-fn :form) (get 'goal-fn :form))
    (t (list 'and t))))


(defun run-command-test-safe (fn)
  "Run FN and return (:NAME sym :PASS-P bool :ERROR obj-or-nil)."
  (handler-case
      (progn
        (funcall fn)
        (list :name fn :pass-p t :error nil))
    (error (e)
      (list :name fn :pass-p nil :error e))))


(defun detect-output (thunk)
  "Capture THUNK's printed output and return it with trailing newlines removed."
  (string-trim '(#\Newline #\Return)
               (with-output-to-string (stream)
                 (let ((*standard-output* stream)
                       (*error-output* stream))
                   (funcall thunk)))))


(defun run-enumerator-checks (&key (stream *standard-output*))
  "CI entrypoint: run enumerator command test(s) and return compact pass/fail plist."
  (let* ((tests '(command-test-5))
         (results (mapcar #'run-command-test-safe tests))
         (failed (remove-if (lambda (r) (getf r :pass-p)) results))
         (summary (list :pass-p (null failed)
                        :total (length results)
                        :passed (- (length results) (length failed))
                        :failed (length failed)
                        :failures (mapcar (lambda (r)
                                            (list :name (getf r :name)
                                                  :error (getf r :error)))
                                          failed))))
    (when stream
      (format stream "~&Enumerator checks: ~D passed, ~D failed.~%"
              (getf summary :passed)
              (getf summary :failed)))
    summary))


(defun test-commands ()
  "Run a series of tests to exercise potential user REPL commands."
  (command-test-1)
  (command-test-2)
  (command-test-3)
  (command-test-4)
  (command-test-5)
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
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
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
  (ww-set *algorithm* depth-first)
  (ww-set *tree-or-graph* graph)
  (ww-set *solution-type* first)
  (ww-set *progress-reporting-interval* 10)
  (ww-set *randomize-search* t)
  (ww-set *branch* 4)
  (ww-set *probe* (put (C A) 3))
  (ww-set *debug* 1)
  (format t "~&Testing that blocks3 runs successfully with new parameters...~2%")
  (assert (and (eq *problem-name* 'blocks3) (= *depth-cutoff* 2) (eq *tree-or-graph* 'graph)
               (eq *solution-type* 'first) (= *progress-reporting-interval* 10)
               (eq *randomize-search* t) (= *branch* 4) (equal *probe* '(put (C A) 3))
               (eq *algorithm* 'depth-first) (= *debug* 1)))
  (assert (member :ww-debug *features*))
  (let ((vals-file (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))
    (with-open-file (in-file vals-file :direction :input)
      (assert (equal (read in-file nil nil)
                     '(blocks3 2 depth-first graph first 10 t 4 (put (C A) 3) 1))))
    (delete-file vals-file))
  (solve)  ;solve with new parameters
  (format t "~%Resetting all parameters to defaults...")
  (reset-parameters)
  (format t "~&Restaging blocks3...~2%")
  (stage blocks3)  ;reset all blocks3 parameters
  (format t "COMMAND-TEST-4 completed successfully.~2%")
  t)


(defun command-test-5 ()
  "Verify corner goal-state count regression."
  (format t "~%COMMAND-TEST-5: testing corner goal-state count regression...~2%")
  (let ((saved-problem *problem-name*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (stage corner)
          (find-goal-states-fn (command-test-resolved-goal-spec))
          (let* ((report (get 'find-goal-states :last-report))
                 (summary (getf report :summary))
                 (goal-count (getf summary :goal-states)))
            (assert (= goal-count 113))))
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-5 completed successfully.~2%")
  t)
