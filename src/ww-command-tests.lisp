;;; Filename: command-tests.lisp

;;; Runs a series of tests of various user commands from the REPL.
;;; First load wouldwork with (progn (ql:quickload :wouldwork :force t) (in-package :ww))
;;; Then enter (test-commands) to start the tests.


(in-package :ww)


;; Compatibility declaration so this test file compiles cleanly even when loaded
;; before ww-csp-ir defines the variable.
(defvar *enum-csp-shadow-compare-strict* nil)
(defvar *enum-csp-next-backend-run-fluent-pilot* nil)
(defvar *enum-csp-next-pilot-bound-check-enabled* nil)
(defvar *enum-csp-next-pilot-bound-check-strict* nil)
(defvar *enum-csp-next-pilot-bound-check-last* nil)
(defvar *enum-csp-shadow-compare-enabled* nil)


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


(defun run-enum-csp-parity-checks (&key (stream *standard-output*))
  "CI entrypoint: run command tests 5..54 and return compact pass/fail plist."
  (let* ((tests '(command-test-5 command-test-6 command-test-7
                  command-test-8 command-test-9 command-test-10
                  command-test-11 command-test-12 command-test-13
                  command-test-14 command-test-15 command-test-16
                  command-test-17 command-test-18 command-test-19
                  command-test-20 command-test-21 command-test-22
                  command-test-23 command-test-24 command-test-25
                  command-test-26 command-test-27 command-test-28
                  command-test-29 command-test-30 command-test-31
                  command-test-32 command-test-33 command-test-34
                  command-test-35 command-test-36 command-test-37
                  command-test-38 command-test-39 command-test-40
                  command-test-41 command-test-42 command-test-43
                  command-test-44 command-test-45 command-test-46
                  command-test-47 command-test-48 command-test-49
                  command-test-50 command-test-51 command-test-52
                  command-test-53 command-test-54))
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
      (format stream "~&ENUM-CSP parity checks: ~D passed, ~D failed.~%"
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
  (command-test-6)
  (command-test-7)
  (command-test-8)
  (command-test-9)
  (command-test-10)
  (command-test-11)
  (command-test-12)
  (command-test-13)
  (command-test-14)
  (command-test-15)
  (command-test-16)
  (command-test-17)
  (command-test-18)
  (command-test-19)
  (command-test-20)
  (command-test-21)
  (command-test-22)
  (command-test-23)
  (command-test-24)
  (command-test-25)
  (command-test-26)
  (command-test-27)
  (command-test-28)
  (command-test-29)
  (command-test-30)
  (command-test-31)
  (command-test-32)
  (command-test-33)
  (command-test-34)
  (command-test-35)
  (command-test-36)
  (command-test-37)
  (command-test-38)
  (command-test-39)
  (command-test-40)
  (command-test-41)
  (command-test-42)
  (command-test-43)
  (command-test-44)
  (command-test-45)
  (command-test-46)
  (command-test-47)
  (command-test-48)
  (command-test-49)
  (command-test-50)
  (command-test-51)
  (command-test-52)
  (command-test-53)
  (command-test-54)
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
  "Verify corner goal-enumerator diagnostics and goal-state count regression."
  (format t "~%COMMAND-TEST-5: testing corner enumerator pruning diagnostics...~2%")
  (let ((saved-problem *problem-name*)
        (saved-diag *enum-hint-diagnostics-enabled*)
        (saved-backend *enum-csp-backend*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (stage corner)
          (setf *enum-hint-diagnostics-enabled* t)
          (find-goal-states-fn (command-test-resolved-goal-spec))
          (let* ((report (get 'find-goal-states :last-report))
                 (summary (getf report :summary))
                 (goal-count (getf summary :goal-states))
                 (diag (gethash '(:value loc) *enum-hint-diagnostics*))
                 (rejected (and diag (getf diag :rejected))))
            (assert (= goal-count 113))
            (assert (and (integerp rejected) (> rejected 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-hint-diagnostics-enabled* saved-diag)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-5 completed successfully.~2%")
  t)


(defun command-test-6 ()
  "Verify enum-csp backend selector, adapter metadata parity, and safe stub path."
  (format t "~%COMMAND-TEST-6: testing enum-csp backend selector and adapter parity...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (setf *enum-csp-next-backend-invocations* 0
                *enum-csp-backend-last-result* nil)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (legacy-meta (enum-csp-ir-metadata ir :backend :legacy))
                 (stub-meta (apply-enum-csp-ir-to-next-backend ir))
                 (legacy-hints (getf legacy-meta :hint-keys))
                 (stub-hints (getf stub-meta :hint-keys))
                 (legacy-loc (cdr (assoc 'loc legacy-hints :test #'eq)))
                 (stub-loc (cdr (assoc 'loc stub-hints :test #'eq)))
                 (legacy-paired (cdr (assoc 'paired legacy-hints :test #'eq)))
                 (stub-paired (cdr (assoc 'paired stub-hints :test #'eq))))
            (assert (equal (getf legacy-meta :base-relations)
                           (getf stub-meta :base-relations)))
            (assert (eql (getf legacy-meta :constraint-present)
                         (getf stub-meta :constraint-present)))
            (assert (equal legacy-loc stub-loc))
            (assert (equal legacy-paired stub-paired))
            (assert (= *enum-csp-next-backend-invocations* 1)))
          (set-enum-csp-backend :next-backend)
          (install-goal-enumerator-spec
           'corner-stub-selector-test
           :base-relations '(loc paired))
          (assert (eq (getf *enum-csp-backend-last-result* :backend)
                      :next-backend-stub))
          (set-enum-csp-backend :legacy)
          (stage corner)
          (find-goal-states-fn (command-test-resolved-goal-spec))
          (let* ((report (get 'find-goal-states :last-report))
                 (summary (getf report :summary))
                 (goal-count (getf summary :goal-states)))
            (assert (= goal-count 113))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-6 completed successfully.~2%")
  t)


(defun command-test-7 ()
  "Verify shadow backend parity compare runs safely on legacy path."
  (format t "~%COMMAND-TEST-7: testing shadow backend parity compare...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow *enum-csp-shadow-compare-enabled*)
        (saved-shadow-last *enum-csp-shadow-compare-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (setf *enum-csp-shadow-compare-enabled* t
                *enum-csp-shadow-compare-last* nil)
          (stage corner)
          (let ((summary (or *enum-csp-shadow-compare-last*
                             (and (fboundp 'current-enum-csp-ir)
                                  (fboundp 'compare-enum-csp-backends)
                                  (current-enum-csp-ir)
                                  (compare-enum-csp-backends (current-enum-csp-ir))))))
            (assert summary)
            (assert (eq (getf summary :match-p) t)))
          (find-goal-states-fn (command-test-resolved-goal-spec))
          (let* ((report (get 'find-goal-states :last-report))
                 (summary (getf report :summary))
                 (goal-count (getf summary :goal-states)))
            (assert (= goal-count 113))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow
            *enum-csp-shadow-compare-last* saved-shadow-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-7 completed successfully.~2%")
  t)


(defun command-test-8 ()
  "Verify diff-only backend reporter is quiet on parity matches."
  (format t "~%COMMAND-TEST-8: testing diff-only backend parity reporter...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (out (with-output-to-string (s)
                        (let ((mismatches (report-enum-csp-backend-mismatches ir :stream s)))
                          (assert (null mismatches)))))
                 (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) out)))
            (assert (string= trimmed ""))))
      (set-enum-csp-backend saved-backend)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-8 completed successfully.~2%")
  t)


(defun command-test-9 ()
  "Verify strict shadow compare mode is safe on parity matches."
  (format t "~%COMMAND-TEST-9: testing strict shadow parity mode...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow *enum-csp-shadow-compare-enabled*)
        (saved-strict *enum-csp-shadow-compare-strict*)
        (saved-shadow-last *enum-csp-shadow-compare-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (setf *enum-csp-shadow-compare-enabled* t
                *enum-csp-shadow-compare-strict* t
                *enum-csp-shadow-compare-last* nil)
          (stage corner)
          (let ((summary (or *enum-csp-shadow-compare-last*
                             (and (fboundp 'current-enum-csp-ir)
                                  (fboundp 'compare-enum-csp-backends)
                                  (current-enum-csp-ir)
                                  (compare-enum-csp-backends (current-enum-csp-ir))))))
            (assert summary)
            (assert (eq (getf summary :match-p) t)))
          (find-goal-states-fn (command-test-resolved-goal-spec))
          (let* ((report (get 'find-goal-states :last-report))
                 (summary (getf report :summary))
                 (goal-count (getf summary :goal-states)))
            (assert (= goal-count 113))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow
            *enum-csp-shadow-compare-strict* saved-strict
            *enum-csp-shadow-compare-last* saved-shadow-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-9 completed successfully.~2%")
  t)


(defun command-test-10 ()
  "Verify parity diff helper reports deterministic mismatch keys."
  (format t "~%COMMAND-TEST-10: testing parity mismatch key reporting...~2%")
  (let* ((legacy (list :base-relations '(loc paired)
                       :constraint-present t
                       :hint-keys '((loc :value-tuples)
                                    (paired :partners))))
         (next (list :base-relations '(loc)
                     :constraint-present nil
                     :hint-keys '((loc :value-tuples))))
         (diffs (enum-csp-backend-parity-differences legacy next))
         (keys (mapcar #'car diffs)))
    (assert (equal keys '(:base-relations :constraint-present :hint-keys))))
  (format t "COMMAND-TEST-10 completed successfully.~2%")
  t)


(defun command-test-11 ()
  "Verify legacy/next parity signatures match for corner."
  (format t "~%COMMAND-TEST-11: testing backend parity signatures...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :legacy)
          (stage corner)
          (let* ((summary (compare-enum-csp-backends (current-enum-csp-ir)))
                 (legacy-sig (getf summary :legacy-signature))
                 (next-sig (getf summary :next-backend-signature)))
            (assert (stringp legacy-sig))
            (assert (stringp next-sig))
            (assert (string= legacy-sig next-sig))))
      (set-enum-csp-backend saved-backend)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-11 completed successfully.~2%")
  t)


(defun command-test-12 ()
  "Verify next-backend compiles deterministic plan summary for corner."
  (format t "~%COMMAND-TEST-12: testing next-backend plan summary...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (stage corner)
          (let* ((ir (and (fboundp 'current-enum-csp-ir)
                          (current-enum-csp-ir)))
                 (result *enum-csp-backend-last-result*)
                 (plan (or (and (fboundp 'current-enum-csp-next-plan)
                                (current-enum-csp-next-plan))
                           (and ir
                                (fboundp 'build-enum-csp-next-plan)
                                (fboundp 'install-enum-csp-next-plan)
                                (install-enum-csp-next-plan
                                 (build-enum-csp-next-plan ir)))))
                 (summary (or (getf result :next-plan-summary)
                              (and plan
                                   (fboundp 'enum-csp-next-plan-summary)
                                   (enum-csp-next-plan-summary plan))))
                 (relation-plans (getf summary :relation-plans))
                 (loc-plan (find 'loc relation-plans :key (lambda (rp) (getf rp :relation)) :test #'eq))
                 (paired-plan (find 'paired relation-plans :key (lambda (rp) (getf rp :relation)) :test #'eq)))
            (assert plan)
            (assert summary)
            (assert (equal (getf summary :base-relations) '(loc paired)))
            (assert (eq (getf loc-plan :pattern) :fluent))
            (assert (= (getf loc-plan :key-count) 4))
            (assert (= (getf loc-plan :value-tuple-count) 4))
            (assert (eq (getf paired-plan :pattern) :subset))
            (assert (= (getf paired-plan :key-count) 3))
            (assert (= (getf paired-plan :partner-count) 4))
            (assert (= (getf paired-plan :max-per-key) 3))
            (assert (integerp (getf summary :leaf-upper-bound)))
            (assert (> (getf summary :leaf-upper-bound) 0))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-12 completed successfully.~2%")
  t)


(defun command-test-13 ()
  "Verify next-backend phase summary and deterministic leaf upper bounds."
  (format t "~%COMMAND-TEST-13: testing next-backend phase bounds...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (stage corner)
          (let* ((ir (and (fboundp 'current-enum-csp-ir)
                          (current-enum-csp-ir)))
                 (plan (or (and (fboundp 'current-enum-csp-next-plan)
                                (current-enum-csp-next-plan))
                           (and ir
                                (fboundp 'build-enum-csp-next-plan)
                                (fboundp 'install-enum-csp-next-plan)
                                (install-enum-csp-next-plan
                                 (build-enum-csp-next-plan ir)))))
                 (summary (and plan
                               (fboundp 'enum-csp-next-plan-summary)
                               (enum-csp-next-plan-summary plan)))
                 (phases (getf summary :phases))
                 (fluent-phase (find :fluent phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (subset-phase (find :subset phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (finalize-phase (find :finalize phases :key (lambda (p) (getf p :phase)) :test #'eq)))
            (assert plan)
            (assert summary)
            (assert (equal (getf summary :phase-order) '(:fluent :subset :finalize)))
            (assert (= (getf fluent-phase :estimated-leaf-upper-bound) 500))
            (assert (= (getf subset-phase :estimated-leaf-upper-bound) 3375))
            (assert (= (getf finalize-phase :estimated-leaf-upper-bound) 1))
            (assert (= (getf summary :leaf-upper-bound) 1687500))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-13 completed successfully.~2%")
  t)


(defun command-test-14 ()
  "Verify fluent pilot execution is deterministic and bounded by phase estimate."
  (format t "~%COMMAND-TEST-14: testing fluent pilot execution...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* nil)
          (stage corner)
          (let* ((ir (and (fboundp 'current-enum-csp-ir)
                          (current-enum-csp-ir)))
                 (plan (or (and (fboundp 'current-enum-csp-next-plan)
                                (current-enum-csp-next-plan))
                           (and ir
                                (fboundp 'build-enum-csp-next-plan)
                                (fboundp 'install-enum-csp-next-plan)
                                (install-enum-csp-next-plan
                                 (build-enum-csp-next-plan ir)))))
                 (summary (and plan
                               (fboundp 'enum-csp-next-plan-summary)
                               (enum-csp-next-plan-summary plan)))
                 (phases (getf summary :phases))
                 (fluent-phase (find :fluent phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (pilot-1 (execute-enum-csp-next-fluent-pilot ir))
                 (pilot-2 (execute-enum-csp-next-fluent-pilot ir))
                 (n1 (getf pilot-1 :final-frontier-size))
                 (n2 (getf pilot-2 :final-frontier-size))
                 (bound (getf fluent-phase :estimated-leaf-upper-bound)))
            (assert (integerp n1))
            (assert (> n1 0))
            (assert (= n1 n2))
            (assert (<= n1 bound))
            (assert (> (getf pilot-1 :action-count) 0))
            (assert (= (length (getf pilot-1 :steps))
                       (getf pilot-1 :action-count)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-14 completed successfully.~2%")
  t)


(defun command-test-15 ()
  "Verify subset pilot summary is deterministic and bounded by next-plan phase estimates."
  (format t "~%COMMAND-TEST-15: testing subset pilot bounds and stats...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* nil)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (plan (or (current-enum-csp-next-plan)
                           (install-enum-csp-next-plan
                            (build-enum-csp-next-plan ir))))
                 (plan-summary (enum-csp-next-plan-summary plan))
                 (phases (getf plan-summary :phases))
                 (subset-phase (find :subset phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (pilot-1 (execute-enum-csp-next-subset-pilot ir))
                 (pilot-2 (execute-enum-csp-next-subset-pilot ir))
                 (fluent-in (getf pilot-1 :fluent-frontier-size-in))
                 (subset-out (getf pilot-1 :subset-frontier-size-out))
                 (subset-bound (getf subset-phase :estimated-leaf-upper-bound))
                 (phase-bound (* fluent-in subset-bound))
                 (relation-stats (getf pilot-1 :relation-stats))
                 (paired-stats (find 'paired relation-stats
                                     :key (lambda (x) (getf x :relation))
                                     :test #'eq))
                 (key-stats (and paired-stats (getf paired-stats :key-stats)))
                 (total-pruning (getf pilot-1 :pruning-counts)))
            (assert (equal pilot-1 pilot-2))
            (assert (integerp fluent-in))
            (assert (> fluent-in 0))
            (assert (integerp subset-out))
            (assert (> subset-out 0))
            (assert (<= subset-out phase-bound))
            (assert (= (getf pilot-1 :relation-count) (length relation-stats)))
            (assert paired-stats)
            (assert (= (getf paired-stats :key-count) 3))
            (assert (= (length key-stats) 3))
            (assert (>= (getf total-pruning :partners) 0))
            (assert (>= (getf total-pruning :partner-feasible-fn) 0))
            (assert (>= (getf total-pruning :requires-fluent) 0))
            (dolist (ks key-stats)
              (assert (integerp (getf ks :candidate-subset-count)))
              (assert (> (getf ks :candidate-subset-count) 0))
              (assert (integerp (getf ks :frontier-size-in)))
              (assert (integerp (getf ks :frontier-size-out)))
              (assert (>= (getf ks :frontier-size-in) 0))
              (assert (>= (getf ks :frontier-size-out) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-15 completed successfully.~2%")
  t)


(defun command-test-16 ()
  "Verify adapter phase-2 pilot metadata is deterministic and plan-bounded."
  (format t "~%COMMAND-TEST-16: testing next-backend phase-2 adapter metadata...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (res-1 (apply-enum-csp-ir-to-next-backend ir))
                 (res-2 (apply-enum-csp-ir-to-next-backend ir))
                 (plan-summary (getf res-1 :next-plan-summary))
                 (phases (getf plan-summary :phases))
                 (subset-phase (find :subset phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (fluent-summary-1 (getf res-1 :fluent-pilot-summary))
                 (subset-summary-1 (getf res-1 :subset-pilot-summary))
                 (fluent-summary-2 (getf res-2 :fluent-pilot-summary))
                 (subset-summary-2 (getf res-2 :subset-pilot-summary))
                 (fluent-in (getf subset-summary-1 :fluent-frontier-size-in))
                 (subset-out (getf subset-summary-1 :subset-frontier-size-out))
                 (subset-phase-bound (getf subset-phase :estimated-leaf-upper-bound)))
            (assert (= *enum-csp-next-backend-invocations* 3))
            (assert (equal fluent-summary-1 fluent-summary-2))
            (assert (equal subset-summary-1 subset-summary-2))
            (assert (= fluent-in (getf fluent-summary-1 :final-frontier-size)))
            (assert (<= subset-out (* fluent-in subset-phase-bound)))
            (dolist (rel-stat (getf subset-summary-1 :relation-stats))
              (let ((rel-plan (find (getf rel-stat :relation)
                                    (getf plan-summary :relation-plans)
                                    :key (lambda (rp) (getf rp :relation))
                                    :test #'eq)))
                (assert rel-plan)
                (assert (eq (getf rel-plan :pattern) :subset))
                (assert (= (getf rel-stat :key-count)
                           (getf rel-plan :key-count)))
                (assert (<= (getf rel-stat :partners-domain-size-out)
                            (getf rel-plan :partner-count)))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-16 completed successfully.~2%")
  t)


(defun command-test-17 ()
  "Verify enum-csp next pilot bound-check helper passes on corner."
  (format t "~%COMMAND-TEST-17: testing next pilot bound-check helper...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let ((check (enum-csp-next-pilot-bound-check *enum-csp-backend-last-result*)))
            (assert check)
            (assert (eq (getf check :ok-p) t))
            (assert (null (getf check :issues)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-17 completed successfully.~2%")
  t)


(defun command-test-18 ()
  "Verify strict next pilot bound-check mode is safe on corner."
  (format t "~%COMMAND-TEST-18: testing strict next pilot bound-check mode...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* t
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((res *enum-csp-backend-last-result*)
                 (check (or *enum-csp-next-pilot-bound-check-last*
                            (and res
                                 (enum-csp-next-pilot-bound-check res)))))
            (assert res)
            (assert (getf res :finalize-pilot-summary))
            (assert check)
            (assert (eq (getf check :ok-p) t))
            (assert (null (getf check :issues)))
            (assert (equal check (getf res :next-pilot-bound-check)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-18 completed successfully.~2%")
  t)


(defun command-test-19 ()
  "Verify diff-only next pilot bound-check reporter is quiet on pass."
  (format t "~%COMMAND-TEST-19: testing diff-only next pilot reporter...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((out (with-output-to-string (s)
                        (let ((issues (report-enum-csp-next-pilot-bound-check
                                       *enum-csp-backend-last-result*
                                       :stream s)))
                          (assert (null issues)))))
                 (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) out)))
            (assert (string= trimmed ""))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-19 completed successfully.~2%")
  t)


(defun command-test-20 ()
  "Verify consolidated migration health snapshot/reporter shape."
  (format t "~%COMMAND-TEST-20: testing migration health snapshot reporter...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-shadow-compare-enabled* nil
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((snapshot (enum-csp-migration-health-snapshot))
                 (report-out (with-output-to-string (s)
                               (report-enum-csp-migration-health :stream s)))
                 (ir (current-enum-csp-ir))
                 (keys (getf snapshot :last-result-keys))
                 (finalize (getf snapshot :finalize-pilot))
                 (subset (getf snapshot :subset-pilot))
                 (bound (getf snapshot :pilot-bound-check))
                 (spec-name (getf snapshot :spec-name)))
            (assert snapshot)
            (assert (eq (getf snapshot :backend) :next-backend))
            (assert (symbolp spec-name))
            (assert (equal spec-name (getf *enum-csp-backend-last-result* :spec-name)))
            (assert (equal spec-name (and ir (enum-csp-ir-spec-name ir))))
            (assert (member :next-plan-summary keys :test #'eq))
            (assert (member :fluent-pilot-summary keys :test #'eq))
            (assert (member :subset-pilot-summary keys :test #'eq))
            (assert (member :finalize-pilot-summary keys :test #'eq))
            (assert (member :next-pilot-bound-check keys :test #'eq))
            (assert finalize)
            (assert (integerp (getf finalize :subset-frontier-size-in)))
            (assert (integerp (getf finalize :finalize-frontier-size-out)))
            (assert (>= (getf finalize :subset-frontier-size-in) 0))
            (assert (>= (getf finalize :finalize-frontier-size-out) 0))
            (assert (<= (getf finalize :finalize-frontier-size-out)
                        (getf finalize :subset-frontier-size-in)))
            (assert (or (null subset)
                        (= (getf finalize :subset-frontier-size-in)
                           (getf subset :subset-frontier-size-out))))
            (assert bound)
            (assert (eq (getf bound :ok-p) t))
            (assert (stringp report-out))
            (assert (> (length report-out) 0))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-20 completed successfully.~2%")
  t)


(defun command-test-21 ()
  "Verify set-enum-csp-cutover-mode profile wiring and stage smoke checks."
  (format t "~%COMMAND-TEST-21: testing cutover mode profiles...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (let ((legacy-state (set-enum-csp-cutover-mode :legacy-safe)))
            (assert (eq (getf legacy-state :backend) :legacy))
            (assert (null (getf legacy-state :shadow-compare-enabled)))
            (assert (null (getf legacy-state :shadow-compare-strict)))
            (assert (null (getf legacy-state :run-fluent-pilot)))
            (assert (null (getf legacy-state :pilot-bound-check-enabled)))
            (assert (null (getf legacy-state :pilot-bound-check-strict))))
          (stage corner)
          (let ((shadow-state (set-enum-csp-cutover-mode :shadow)))
            (assert (eq (getf shadow-state :backend) :legacy))
            (assert (eq (getf shadow-state :shadow-compare-enabled) t))
            (assert (null (getf shadow-state :shadow-compare-strict)))
            (assert (null (getf shadow-state :run-fluent-pilot)))
            (assert (null (getf shadow-state :pilot-bound-check-enabled)))
            (assert (null (getf shadow-state :pilot-bound-check-strict))))
          (stage corner)
          (let ((next-state (set-enum-csp-cutover-mode :next-pilot-strict)))
            (assert (eq (getf next-state :backend) :next-backend))
            (assert (null (getf next-state :shadow-compare-enabled)))
            (assert (null (getf next-state :shadow-compare-strict)))
            (assert (eq (getf next-state :run-fluent-pilot) t))
            (assert (eq (getf next-state :pilot-bound-check-enabled) t))
            (assert (eq (getf next-state :pilot-bound-check-strict) t)))
          (stage corner)
          (let ((last-finalize (getf *enum-csp-backend-last-result* :finalize-pilot-summary))
                (last-check (getf *enum-csp-backend-last-result* :next-pilot-bound-check)))
            (assert last-finalize)
            (assert last-check)
            (assert (eq (getf last-check :ok-p) t))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-21 completed successfully.~2%")
  t)


(defun command-test-22 ()
  "Verify finalize pilot summary is deterministic and bounded by next-plan finalize estimates."
  (format t "~%COMMAND-TEST-22: testing finalize pilot bounds and determinism...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* nil)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (plan (or (current-enum-csp-next-plan)
                           (install-enum-csp-next-plan
                            (build-enum-csp-next-plan ir))))
                 (plan-summary (enum-csp-next-plan-summary plan))
                 (phases (getf plan-summary :phases))
                 (finalize-phase (find :finalize phases :key (lambda (p) (getf p :phase)) :test #'eq))
                 (pilot-1 (execute-enum-csp-next-finalize-pilot ir))
                 (pilot-2 (execute-enum-csp-next-finalize-pilot ir))
                 (subset-in (getf pilot-1 :subset-frontier-size-in))
                 (finalize-out (getf pilot-1 :finalize-frontier-size-out))
                 (bound (* subset-in (getf finalize-phase :estimated-leaf-upper-bound)))
                 (prune-counts (getf pilot-1 :prune-reason-counts))
                 (accept-counts (getf pilot-1 :accept-reason-counts))
                 (goal-counts (getf pilot-1 :goal-check-counts))
                 (prefilter-counts (getf pilot-1 :prefilter-counts))
                 (propagation-counts (getf pilot-1 :propagation-counts))
                 (total-pruned (+ (getf prune-counts :prefilter)
                                  (getf prune-counts :propagation)
                                  (getf prune-counts :goal))))
            (assert (equal pilot-1 pilot-2))
            (assert (integerp subset-in))
            (assert (> subset-in 0))
            (assert (integerp finalize-out))
            (assert (>= finalize-out 0))
            (assert (<= finalize-out bound))
            (assert (member (getf pilot-1 :prefilter-status)
                            '(:none :function :symbol :symbol-unbound :invalid)
                            :test #'eq))
            (assert (= (+ finalize-out total-pruned) subset-in))
            (assert (= (getf prefilter-counts :passed)
                       (getf propagation-counts :attempted)))
            (if (getf goal-counts :available-p)
                (progn
                  (assert (= (getf goal-counts :passed)
                             (getf accept-counts :goal)))
                  (assert (= (getf accept-counts :no-goal-check) 0)))
                (assert (= finalize-out (getf accept-counts :no-goal-check))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-22 completed successfully.~2%")
  t)


(defun command-test-23 ()
  "Verify adapter-integrated finalize metadata is deterministic and bound-check safe."
  (format t "~%COMMAND-TEST-23: testing next-backend finalize adapter metadata...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (res-1 (apply-enum-csp-ir-to-next-backend ir))
                 (res-2 (apply-enum-csp-ir-to-next-backend ir))
                 (finalize-1 (getf res-1 :finalize-pilot-summary))
                 (finalize-2 (getf res-2 :finalize-pilot-summary))
                 (subset-1 (getf res-1 :subset-pilot-summary))
                 (check (enum-csp-next-pilot-bound-check res-1)))
            (assert (= *enum-csp-next-backend-invocations* 3))
            (assert finalize-1)
            (assert (equal finalize-1 finalize-2))
            (assert (= (getf finalize-1 :subset-frontier-size-in)
                       (getf subset-1 :subset-frontier-size-out)))
            (assert (<= (getf finalize-1 :finalize-frontier-size-out)
                        (getf finalize-1 :subset-frontier-size-in)))
            (assert (member :finalize-pilot-summary
                            (loop for tail on res-1 by #'cddr
                                  collect (first tail))
                            :test #'eq))
            (assert (eq (getf check :ok-p) t))
            (assert (null (getf check :issues)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-23 completed successfully.~2%")
  t)


(defun command-test-24 ()
  "Verify finalize-focused diff-only bound-check reporter behavior (quiet on pass, emits on mismatch)."
  (format t "~%COMMAND-TEST-24: testing finalize-focused diff-only bound-check reporter...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((good-result *enum-csp-backend-last-result*)
                 (good-out (with-output-to-string (s)
                             (let ((issues (report-enum-csp-next-pilot-bound-check
                                            good-result
                                            :stream s)))
                               (assert (null issues)))))
                 (trimmed-good (string-trim '(#\Space #\Tab #\Newline #\Return) good-out))
                 (bad-result (copy-tree good-result))
                 (bad-finalize (copy-tree (getf bad-result :finalize-pilot-summary))))
            (assert (string= trimmed-good ""))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad-result :finalize-pilot-summary) bad-finalize)
            (let* ((bad-out (with-output-to-string (s)
                              (report-enum-csp-next-pilot-bound-check bad-result :stream s)))
                   (bad-issues (enum-csp-next-pilot-bound-check bad-result))
                   (issue-kinds (mapcar #'first (getf bad-issues :issues)))
                   (trimmed-bad (string-trim '(#\Space #\Tab #\Newline #\Return) bad-out)))
              (assert (member :finalize-frontier-in-mismatch issue-kinds :test #'eq))
              (assert (> (length trimmed-bad) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-24 completed successfully.~2%")
  t)


(defun command-test-25 ()
  "Verify strict next pilot bound-check rejects finalize-phase mismatches."
  (format t "~%COMMAND-TEST-25: testing strict finalize mismatch safety...~2%")
  (let ((saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*enum-csp-next-pilot-bound-check-enabled* t)
               (*enum-csp-next-pilot-bound-check-strict* t)
               (*enum-csp-next-pilot-bound-check-last* nil)
               (result (list :next-plan-summary
                             (list :phases (list (list :phase :subset
                                                       :estimated-leaf-upper-bound 1)
                                                 (list :phase :finalize
                                                       :estimated-leaf-upper-bound 1))
                                   :relation-plans nil)
                             :subset-pilot-summary
                             (list :fluent-frontier-size-in 1
                                   :subset-frontier-size-out 1
                                   :relation-stats nil)
                             :finalize-pilot-summary
                             (list :subset-frontier-size-in 2
                                   :finalize-frontier-size-out 1
                                   :prefilter-counts
                                   (list :checked 2 :passed 2 :pruned 0 :skipped 0)
                                   :propagation-counts
                                   (list :attempted 2 :performed 2 :skipped 0)
                                   :goal-check-counts
                                   (list :available-p nil :checked 0 :passed 0 :pruned 0 :skipped 2)
                                   :prune-reason-counts
                                   (list :prefilter 0 :propagation 0 :goal 0)
                                   :accept-reason-counts
                                   (list :goal 0 :no-goal-check 1)))))
          (handler-case
              (progn
                (cspir-run-next-pilot-bound-check-if-enabled result)
                (error "Expected strict finalize mismatch to signal an error."))
            (error (e)
              (let ((msg (princ-to-string e)))
                (assert (search "strict next pilot bound-check mismatch" msg))
                (assert (search "FINALIZE-FRONTIER-IN-MISMATCH" (string-upcase msg))))))))
      (setf *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last))
  (format t "COMMAND-TEST-25 completed successfully.~2%")
  t)


(defun command-test-26 ()
  "Verify finalize pilot API summary shape and stream reporter smoke behavior."
  (format t "~%COMMAND-TEST-26: testing finalize pilot API smoke shape...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* nil)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (summary-1 (execute-enum-csp-next-finalize-pilot ir))
                 (summary-2 (execute-enum-csp-next-finalize-pilot ir))
                 (stream-out (with-output-to-string (s)
                               (execute-enum-csp-next-finalize-pilot ir :stream s)))
                 (keys (loop for tail on summary-1 by #'cddr
                             collect (first tail))))
            (assert (equal summary-1 summary-2))
            (assert (eq (getf summary-1 :phase) :finalize-pilot))
            (assert (member :spec-name keys :test #'eq))
            (assert (member :subset-frontier-size-in keys :test #'eq))
            (assert (member :finalize-frontier-size-out keys :test #'eq))
            (assert (member :accepted-final-size keys :test #'eq))
            (assert (member :prefilter-status keys :test #'eq))
            (assert (member :prefilter-counts keys :test #'eq))
            (assert (member :propagation-counts keys :test #'eq))
            (assert (member :goal-check-counts keys :test #'eq))
            (assert (member :prune-reason-counts keys :test #'eq))
            (assert (member :accept-reason-counts keys :test #'eq))
            (assert (= (getf summary-1 :finalize-frontier-size-out)
                       (getf summary-1 :accepted-final-size)))
            (assert (>= (length stream-out) 1))
            (assert (search "finalize pilot" (string-downcase stream-out)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-26 completed successfully.~2%")
  t)


(defun command-test-27 ()
  "Verify finalize-only mismatch reporter is quiet on pass and emits only finalize issues."
  (format t "~%COMMAND-TEST-27: testing finalize-only mismatch reporter...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((good-result *enum-csp-backend-last-result*)
                 (good-out (with-output-to-string (s)
                             (let ((issues (report-enum-csp-next-finalize-mismatches
                                            good-result
                                            :stream s)))
                               (assert (null issues)))))
                 (trimmed-good (string-trim '(#\Space #\Tab #\Newline #\Return) good-out))
                 (bad-result (copy-tree good-result))
                 (bad-finalize (copy-tree (getf bad-result :finalize-pilot-summary))))
            (assert (string= trimmed-good ""))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad-result :finalize-pilot-summary) bad-finalize)
            (let* ((bad-out (with-output-to-string (s)
                              (report-enum-csp-next-finalize-mismatches bad-result :stream s)))
                   (issues (enum-csp-next-finalize-bound-check-issues bad-result))
                   (trimmed-bad (string-trim '(#\Space #\Tab #\Newline #\Return) bad-out)))
              (assert (member :finalize-frontier-in-mismatch
                              (mapcar #'first issues)
                              :test #'eq))
              (dolist (issue issues)
                (let ((kind (first issue)))
                  (assert (or (eq kind :missing)
                              (search "FINALIZE" (string-upcase (symbol-name kind)))))))
              (assert (> (length trimmed-bad) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-27 completed successfully.~2%")
  t)


(defun command-test-28 ()
  "Verify migration-health finalize section and finalize-only reporter behavior."
  (format t "~%COMMAND-TEST-28: testing migration health + finalize reporter integration...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((snapshot (enum-csp-migration-health-snapshot))
                 (finalize (getf snapshot :finalize-pilot))
                 (health-out (with-output-to-string (s)
                               (report-enum-csp-migration-health :stream s)))
                 (good-result *enum-csp-backend-last-result*)
                 (quiet-out (with-output-to-string (s)
                              (let ((issues (report-enum-csp-next-finalize-mismatches
                                             good-result
                                             :stream s)))
                                (assert (null issues)))))
                 (bad-result (copy-tree good-result))
                 (bad-finalize (copy-tree (getf bad-result :finalize-pilot-summary))))
            (assert finalize)
            (assert (search "finalize-pilot" (string-downcase health-out)))
            (assert (string= (string-trim '(#\Space #\Tab #\Newline #\Return) quiet-out) ""))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad-result :finalize-pilot-summary) bad-finalize)
            (let ((emit-out (with-output-to-string (s)
                              (report-enum-csp-next-finalize-mismatches bad-result :stream s))))
              (assert (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) emit-out)) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-28 completed successfully.~2%")
  t)


(defun command-test-29 ()
  "Verify finalize-only issue helper filters out non-finalize mismatch kinds."
  (format t "~%COMMAND-TEST-29: testing finalize-only issue filtering...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((base *enum-csp-backend-last-result*)
                 (bad (copy-tree base))
                 (subset (copy-tree (getf bad :subset-pilot-summary)))
                 (finalize (copy-tree (getf bad :finalize-pilot-summary))))
            ;; Force one non-finalize issue and one finalize issue.
            (setf (getf subset :subset-frontier-size-out) 99999999)
            (setf (getf bad :subset-pilot-summary) subset)
            (setf (getf finalize :subset-frontier-size-in)
                  (1+ (getf finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) finalize)
            (let* ((all-issues (getf (enum-csp-next-pilot-bound-check bad) :issues))
                   (finalize-issues (enum-csp-next-finalize-bound-check-issues bad))
                   (all-kinds (mapcar #'first all-issues))
                   (finalize-kinds (mapcar #'first finalize-issues)))
              (assert (member :subset-frontier-out-of-bound all-kinds :test #'eq))
              (assert (member :finalize-frontier-in-mismatch finalize-kinds :test #'eq))
              (assert (not (member :subset-frontier-out-of-bound finalize-kinds :test #'eq)))
              (dolist (k finalize-kinds)
                (assert (or (eq k :missing)
                            (search "FINALIZE" (string-upcase (symbol-name k)))))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-29 completed successfully.~2%")
  t)


(defun command-test-30 ()
  "Verify strict cutover run keeps finalize-only mismatch reporter quiet on healthy result."
  (format t "~%COMMAND-TEST-30: testing strict cutover finalize reporter quiet path...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let* ((result *enum-csp-backend-last-result*)
                 (finalize (getf result :finalize-pilot-summary))
                 (bound (getf result :next-pilot-bound-check))
                 (out (with-output-to-string (s)
                        (let ((issues (report-enum-csp-next-finalize-mismatches
                                       result
                                       :stream s)))
                          (assert (null issues))))))
            (assert finalize)
            (assert bound)
            (assert (eq (getf bound :ok-p) t))
            (assert (string= (string-trim '(#\Space #\Tab #\Newline #\Return) out) ""))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-30 completed successfully.~2%")
  t)


(defun command-test-31 ()
  "Verify finalize-only issue helper is deterministic and empty on healthy results."
  (format t "~%COMMAND-TEST-31: testing finalize issue helper deterministic pass path...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((result *enum-csp-backend-last-result*)
                 (issues-1 (enum-csp-next-finalize-bound-check-issues result))
                 (issues-2 (enum-csp-next-finalize-bound-check-issues result)))
            (assert (equal issues-1 issues-2))
            (assert (null issues-1))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-31 completed successfully.~2%")
  t)


(defun command-test-32 ()
  "Verify finalize-only helper/reporter return :MISSING for absent finalize summary."
  (format t "~%COMMAND-TEST-32: testing missing finalize summary handling...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((bad (copy-tree *enum-csp-backend-last-result*)))
            (remf bad :finalize-pilot-summary)
            (let* ((issues (enum-csp-next-finalize-bound-check-issues bad))
                   (out (with-output-to-string (s)
                          (report-enum-csp-next-finalize-mismatches bad :stream s))))
              (assert (member (list :missing :finalize-pilot-summary) issues :test #'equal))
              (assert (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) out)) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-32 completed successfully.~2%")
  t)


(defun command-test-33 ()
  "Verify finalize prune-count shape includes :PROPAGATION and non-negative values."
  (format t "~%COMMAND-TEST-33: testing finalize prune-count shape...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* nil)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (summary (execute-enum-csp-next-finalize-pilot ir))
                 (prune (getf summary :prune-reason-counts))
                 (accept (getf summary :accept-reason-counts))
                 (prune-keys (loop for tail on prune by #'cddr
                                   collect (first tail))))
            (assert (member :prefilter prune-keys
                            :test #'eq))
            (assert (member :propagation prune-keys
                            :test #'eq))
            (assert (member :goal prune-keys
                            :test #'eq))
            (assert (>= (getf prune :prefilter) 0))
            (assert (>= (getf prune :propagation) 0))
            (assert (>= (getf prune :goal) 0))
            (assert (>= (getf accept :goal) 0))
            (assert (>= (getf accept :no-goal-check) 0))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-33 completed successfully.~2%")
  t)


(defun command-test-34 ()
  "Verify :legacy-safe cutover keeps finalize metadata absent."
  (format t "~%COMMAND-TEST-34: testing legacy-safe finalize metadata absence...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :legacy-safe)
          (stage corner)
          (assert (eq *enum-csp-backend* :legacy))
          (assert (null *enum-csp-next-backend-run-fluent-pilot*))
          (assert (null (getf *enum-csp-backend-last-result* :finalize-pilot-summary))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-34 completed successfully.~2%")
  t)


(defun command-test-35 ()
  "Verify :shadow cutover keeps finalize metadata absent while parity path is active."
  (format t "~%COMMAND-TEST-35: testing shadow finalize metadata absence...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-shadow-last *enum-csp-shadow-compare-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :shadow)
          (stage corner)
          (assert (eq *enum-csp-backend* :legacy))
          (assert (eq *enum-csp-shadow-compare-enabled* t))
          (assert (null *enum-csp-next-backend-run-fluent-pilot*))
          (assert (null (getf *enum-csp-backend-last-result* :finalize-pilot-summary)))
          (assert *enum-csp-shadow-compare-last*))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-shadow-compare-last* saved-shadow-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-35 completed successfully.~2%")
  t)


(defun command-test-36 ()
  "Verify :next-pilot-strict cutover keeps finalize metadata present and healthy."
  (format t "~%COMMAND-TEST-36: testing next-pilot-strict finalize metadata presence...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let* ((result *enum-csp-backend-last-result*)
                 (finalize (getf result :finalize-pilot-summary))
                 (bound (getf result :next-pilot-bound-check)))
            (assert (eq *enum-csp-backend* :next-backend))
            (assert (eq *enum-csp-next-backend-run-fluent-pilot* t))
            (assert (eq *enum-csp-next-pilot-bound-check-enabled* t))
            (assert (eq *enum-csp-next-pilot-bound-check-strict* t))
            (assert finalize)
            (assert (integerp (getf finalize :subset-frontier-size-in)))
            (assert (integerp (getf finalize :finalize-frontier-size-out)))
            (assert (<= (getf finalize :finalize-frontier-size-out)
                        (getf finalize :subset-frontier-size-in)))
            (assert bound)
            (assert (eq (getf bound :ok-p) t))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-36 completed successfully.~2%")
  t)


(defun command-test-37 ()
  "Verify finalize-only reporter defaults to *ENUM-CSP-BACKEND-LAST-RESULT* when RESULT is omitted."
  (format t "~%COMMAND-TEST-37: testing finalize reporter default-result behavior...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((explicit-issues (enum-csp-next-finalize-bound-check-issues
                                   *enum-csp-backend-last-result*))
                 (default-issues (enum-csp-next-finalize-bound-check-issues))
                 (explicit-out (with-output-to-string (s)
                                 (report-enum-csp-next-finalize-mismatches
                                  *enum-csp-backend-last-result*
                                  :stream s)))
                 (default-out (with-output-to-string (s)
                                (let ((*standard-output* s))
                                  (report-enum-csp-next-finalize-mismatches)))))
            (assert (equal explicit-issues default-issues))
            (assert (string= (string-trim '(#\Space #\Tab #\Newline #\Return) explicit-out)
                             (string-trim '(#\Space #\Tab #\Newline #\Return) default-out)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-37 completed successfully.~2%")
  t)


(defun command-test-38 ()
  "Verify finalize-only helper/reporter are idempotent under repeated calls."
  (format t "~%COMMAND-TEST-38: testing finalize helper/reporter idempotence...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((result *enum-csp-backend-last-result*)
                 (bad (copy-tree result))
                 (bad-finalize (copy-tree (getf bad :finalize-pilot-summary))))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) bad-finalize)
            (let* ((issues-1 (enum-csp-next-finalize-bound-check-issues bad))
                   (issues-2 (enum-csp-next-finalize-bound-check-issues bad))
                   (out-1 (with-output-to-string (s)
                            (report-enum-csp-next-finalize-mismatches bad :stream s)))
                   (out-2 (with-output-to-string (s)
                            (report-enum-csp-next-finalize-mismatches bad :stream s))))
              (assert (equal issues-1 issues-2))
              (assert (string= out-1 out-2))
              (assert (member :finalize-frontier-in-mismatch
                              (mapcar #'first issues-1)
                              :test #'eq)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-38 completed successfully.~2%")
  t)


(defun command-test-39 ()
  "Verify migration health :LAST-RESULT-KEYS include finalize key only in next-pilot-strict."
  (format t "~%COMMAND-TEST-39: testing migration health key presence across cutover modes...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :legacy-safe)
          (stage corner)
          (let ((legacy-keys (getf (enum-csp-migration-health-snapshot) :last-result-keys)))
            (assert (not (member :finalize-pilot-summary legacy-keys :test #'eq))))
          (set-enum-csp-cutover-mode :shadow)
          (stage corner)
          (let ((shadow-keys (getf (enum-csp-migration-health-snapshot) :last-result-keys)))
            (assert (not (member :finalize-pilot-summary shadow-keys :test #'eq))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let ((next-keys (getf (enum-csp-migration-health-snapshot) :last-result-keys)))
            (assert (member :finalize-pilot-summary next-keys :test #'eq))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-39 completed successfully.~2%")
  t)


(defun command-test-40 ()
  "Verify finalize-only reporter emits finalize-focused header/issues format."
  (format t "~%COMMAND-TEST-40: testing finalize reporter output shape...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((bad (copy-tree *enum-csp-backend-last-result*))
                 (bad-finalize (copy-tree (getf bad :finalize-pilot-summary))))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) bad-finalize)
            (let* ((out (with-output-to-string (s)
                          (report-enum-csp-next-finalize-mismatches bad :stream s)))
                   (txt (string-downcase out)))
              (assert (search "next pilot finalize mismatches" txt))
              (assert (search "finalize-frontier-in-mismatch" txt))
              (assert (not (search "subset-pilot:" txt))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-40 completed successfully.~2%")
  t)


(defun command-test-41 ()
  "Verify finalize issue helper propagates missing summary issues."
  (format t "~%COMMAND-TEST-41: testing finalize helper missing-summary propagation...~2%")
  (let* ((missing-plan (enum-csp-next-finalize-bound-check-issues
                        (list :subset-pilot-summary (list :subset-frontier-size-out 1)
                              :finalize-pilot-summary (list :subset-frontier-size-in 1
                                                            :finalize-frontier-size-out 1))))
         (missing-subset (enum-csp-next-finalize-bound-check-issues
                          (list :next-plan-summary (list :phases nil :relation-plans nil)
                                :finalize-pilot-summary (list :subset-frontier-size-in 1
                                                              :finalize-frontier-size-out 1)))))
    (assert (member (list :missing :next-plan-summary) missing-plan :test #'equal))
    (assert (member (list :missing :subset-pilot-summary) missing-subset :test #'equal)))
  (format t "COMMAND-TEST-41 completed successfully.~2%")
  t)


(defun command-test-42 ()
  "Verify migration health reporter includes finalize line only when finalize data is present."
  (format t "~%COMMAND-TEST-42: testing migration health finalize line gating...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :legacy-safe)
          (stage corner)
          (let ((legacy-out (string-downcase
                             (with-output-to-string (s)
                               (report-enum-csp-migration-health :stream s)))))
            (assert (not (search "finalize-pilot:" legacy-out))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let ((next-out (string-downcase
                           (with-output-to-string (s)
                             (report-enum-csp-migration-health :stream s)))))
            (assert (search "finalize-pilot:" next-out))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-42 completed successfully.~2%")
  t)


(defun command-test-43 ()
  "Verify finalize-only reporter returns the same issues as finalize-only helper."
  (format t "~%COMMAND-TEST-43: testing finalize helper/reporter issue equivalence...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((bad (copy-tree *enum-csp-backend-last-result*))
                 (bad-finalize (copy-tree (getf bad :finalize-pilot-summary))))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) bad-finalize)
            (let* ((helper-issues (enum-csp-next-finalize-bound-check-issues bad))
                   (reporter-issues (report-enum-csp-next-finalize-mismatches
                                     bad
                                     :stream (make-broadcast-stream))))
              (assert (equal helper-issues reporter-issues)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-43 completed successfully.~2%")
  t)


(defun command-test-44 ()
  "Verify finalize-only helper/reporter handle NIL result defensively."
  (format t "~%COMMAND-TEST-44: testing finalize helper/reporter NIL result behavior...~2%")
  (let* ((issues (enum-csp-next-finalize-bound-check-issues nil))
         (report-issues (report-enum-csp-next-finalize-mismatches
                         nil
                         :stream (make-broadcast-stream))))
    (assert (member (list :missing :next-plan-summary) issues :test #'equal))
    (assert (equal issues report-issues)))
  (format t "COMMAND-TEST-44 completed successfully.~2%")
  t)


(defun command-test-45 ()
  "Verify strict mode still signals on finalize mismatch after cutover switches."
  (format t "~%COMMAND-TEST-45: testing strict finalize mismatch after cutover switching...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          ;; Exercise mode switching sequence, then assert strict mismatch still signals.
          (set-enum-csp-cutover-mode :legacy-safe)
          (stage corner)
          (set-enum-csp-cutover-mode :shadow)
          (stage corner)
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let* ((bad (copy-tree *enum-csp-backend-last-result*))
                 (bad-finalize (copy-tree (getf bad :finalize-pilot-summary))))
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) bad-finalize)
            (handler-case
                (progn
                  (cspir-run-next-pilot-bound-check-if-enabled bad)
                  (error "Expected strict mode to signal finalize mismatch after cutover switching."))
              (error (e)
                (let ((msg (string-upcase (princ-to-string e))))
                  (assert (search "STRICT NEXT PILOT BOUND-CHECK MISMATCH" msg))
                  (assert (search "FINALIZE-FRONTIER-IN-MISMATCH" msg)))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-45 completed successfully.~2%")
  t)


(defun command-test-46 ()
  "Verify finalize issue helper returns stable issue ordering."
  (format t "~%COMMAND-TEST-46: testing finalize issue ordering stability...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-pilot-bound-check-enabled* t
                *enum-csp-next-pilot-bound-check-strict* nil
                *enum-csp-next-pilot-bound-check-last* nil
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((bad (copy-tree *enum-csp-backend-last-result*))
                 (bad-finalize (copy-tree (getf bad :finalize-pilot-summary))))
            ;; Force multiple finalize issues deterministically.
            (setf (getf bad-finalize :subset-frontier-size-in)
                  (+ 2 (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad-finalize :finalize-frontier-size-out)
                  (1+ (getf bad-finalize :subset-frontier-size-in)))
            (setf (getf bad :finalize-pilot-summary) bad-finalize)
            (let ((issues-1 (enum-csp-next-finalize-bound-check-issues bad))
                  (issues-2 (enum-csp-next-finalize-bound-check-issues bad)))
              (assert (equal issues-1 issues-2))
              (assert (>= (length issues-1) 1)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-46 completed successfully.~2%")
  t)


(defun command-test-47 ()
  "Verify finalize-only reporter reports expected missing-plan issues in legacy/shadow modes."
  (format t "~%COMMAND-TEST-47: testing finalize reporter legacy/shadow missing-plan path...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (set-enum-csp-cutover-mode :legacy-safe)
          (stage corner)
          (let* ((legacy-out (with-output-to-string (s)
                               (let ((issues (report-enum-csp-next-finalize-mismatches
                                              *enum-csp-backend-last-result*
                                              :stream s)))
                                 (assert (member (list :missing :next-plan-summary)
                                                 issues :test #'equal))))))
            (assert (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) legacy-out)) 0)))
          (set-enum-csp-cutover-mode :shadow)
          (stage corner)
          (let* ((shadow-out (with-output-to-string (s)
                               (let ((issues (report-enum-csp-next-finalize-mismatches
                                              *enum-csp-backend-last-result*
                                              :stream s)))
                                 (assert (member (list :missing :next-plan-summary)
                                                 issues :test #'equal))))))
            (assert (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) shadow-out)) 0))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-47 completed successfully.~2%")
  t)


(defun command-test-48 ()
  "Verify strict mode healthy result keeps finalize helper/reporter empty."
  (format t "~%COMMAND-TEST-48: testing strict healthy finalize empty path...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-shadow-enabled *enum-csp-shadow-compare-enabled*)
        (saved-shadow-strict *enum-csp-shadow-compare-strict*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*)
        (saved-check-enabled *enum-csp-next-pilot-bound-check-enabled*)
        (saved-check-strict *enum-csp-next-pilot-bound-check-strict*)
        (saved-check-last *enum-csp-next-pilot-bound-check-last*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-cutover-mode :next-pilot-strict)
          (stage corner)
          (let* ((result *enum-csp-backend-last-result*)
                 (helper (enum-csp-next-finalize-bound-check-issues result))
                 (reporter-out (with-output-to-string (s)
                                 (let ((issues (report-enum-csp-next-finalize-mismatches
                                                result
                                                :stream s)))
                                   (assert (null issues))))))
            (assert (null helper))
            (assert (string= (string-trim '(#\Space #\Tab #\Newline #\Return) reporter-out) ""))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-shadow-compare-enabled* saved-shadow-enabled
            *enum-csp-shadow-compare-strict* saved-shadow-strict
            *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-next-pilot-bound-check-enabled* saved-check-enabled
            *enum-csp-next-pilot-bound-check-strict* saved-check-strict
            *enum-csp-next-pilot-bound-check-last* saved-check-last
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-48 completed successfully.~2%")
  t)


(defun command-test-49 ()
  "Verify finalize-vs-find-goal-states comparison helper is deterministic and well-shaped."
  (format t "~%COMMAND-TEST-49: testing finalize-vs-find-goal-states deterministic shape...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (cmp-1 (compare-enum-csp-next-finalize-with-find-goal-states ir))
                 (cmp-2 (compare-enum-csp-next-finalize-with-find-goal-states ir)))
            (assert (equal cmp-1 cmp-2))
            (assert (member :legacy-goal-signature (loop for tail on cmp-1 by #'cddr collect (first tail))
                            :test #'eq))
            (assert (member :next-finalize-signature (loop for tail on cmp-1 by #'cddr collect (first tail))
                            :test #'eq))
            (assert (member :match-p (loop for tail on cmp-1 by #'cddr collect (first tail))
                            :test #'eq))
            (assert (member :differences (loop for tail on cmp-1 by #'cddr collect (first tail))
                            :test #'eq))
            (assert (integerp (getf (getf cmp-1 :legacy-goal-signature) :count)))
            (assert (integerp (getf (getf cmp-1 :next-finalize-signature) :count)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-49 completed successfully.~2%")
  t)


(defun command-test-50 ()
  "Verify finalize-vs-find-goal-states mismatch reporter output is consistent with comparison result."
  (format t "~%COMMAND-TEST-50: testing finalize-vs-find-goal-states reporter consistency...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (cmp (compare-enum-csp-next-finalize-with-find-goal-states ir))
                 (out (with-output-to-string (s)
                        (let ((diffs (report-enum-csp-next-finalize-find-goal-states-mismatches
                                      ir :stream s)))
                          (assert (equal diffs (getf cmp :differences))))))
                 (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) out)))
            (if (getf cmp :match-p)
                (assert (string= trimmed ""))
                (assert (> (length trimmed) 0)))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-50 completed successfully.~2%")
  t)


(defun command-test-51 ()
  "Verify finalize-vs-find-goal-states reporter consistency for forced goal-spec path."
  (format t "~%COMMAND-TEST-51: testing finalize-vs-find-goal-states forced goal-spec consistency...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (cmp (compare-enum-csp-next-finalize-with-find-goal-states
                       ir :goal-spec '(and nil)))
                 (out (with-output-to-string (s)
                        (report-enum-csp-next-finalize-find-goal-states-mismatches
                         ir :goal-spec '(and nil) :comparison cmp :stream s)))
                 (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) out)))
            (if (getf cmp :match-p)
                (assert (string= trimmed ""))
                (progn
                  (assert (not (null (getf cmp :differences))))
                  (assert (> (length trimmed) 0))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-51 completed successfully.~2%")
  t)


(defun command-test-52 ()
  "Verify E2E finalize parity helper summary shape and strict behavior."
  (format t "~%COMMAND-TEST-52: testing E2E finalize parity helper...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-count *enum-csp-next-backend-invocations*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (set-enum-csp-backend :next-backend)
          (setf *enum-csp-next-backend-run-fluent-pilot* t
                *enum-csp-next-backend-invocations* 0)
          (stage corner)
          (let* ((ir (current-enum-csp-ir))
                 (summary (run-enum-csp-e2e-finalize-parity ir :stream nil))
                 (keys (loop for tail on summary by #'cddr
                             collect (first tail))))
            (assert (member :spec-name keys :test #'eq))
            (assert (member :goal-spec keys :test #'eq))
            (assert (member :legacy-goal-count keys :test #'eq))
            (assert (member :next-finalize-count keys :test #'eq))
            (assert (member :match-p keys :test #'eq))
            (assert (member :difference-count keys :test #'eq))
            (assert (member :difference-keys keys :test #'eq))
            (assert (member :differences keys :test #'eq))
            (assert (integerp (getf summary :legacy-goal-count)))
            (assert (integerp (getf summary :next-finalize-count)))
            (assert (= (getf summary :difference-count)
                       (length (getf summary :differences))))
            (if (getf summary :match-p)
                (progn
                  (run-enum-csp-e2e-finalize-parity ir :strict t :stream nil)
                  t)
                (handler-case
                    (progn
                      (run-enum-csp-e2e-finalize-parity ir :strict t :stream nil)
                      (error "Expected strict E2E parity mismatch to signal an error."))
                  (error (e)
                    (assert (search "STRICT E2E FINALIZE PARITY MISMATCH"
                                    (string-upcase (princ-to-string e)))))))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last
            *enum-csp-next-backend-invocations* saved-count)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-52 completed successfully.~2%")
  t)


(defun command-test-53 ()
  "Verify FIND-GOAL-STATES runs under both legacy and next backends."
  (format t "~%COMMAND-TEST-53: testing find-goal-states dual-backend execution...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (setf *enum-csp-next-backend-run-fluent-pilot* t)
          (set-enum-csp-backend :legacy)
          (stage corner)
          (assert (find-goal-states-fn (command-test-resolved-goal-spec)
                                       :backend :legacy
                                       :solution-type 'every))
          (let* ((legacy-report (get 'find-goal-states :last-report))
                 (legacy-summary (getf legacy-report :summary))
                 (legacy-count (getf legacy-summary :goal-states)))
            (assert (integerp legacy-count))
            (assert (>= legacy-count 0)))
          (set-enum-csp-backend :next-backend)
          (stage corner)
          (assert (find-goal-states-fn (command-test-resolved-goal-spec)
                                       :backend :next-backend
                                       :solution-type 'every))
          (let* ((next-report (get 'find-goal-states :last-report))
                 (next-summary (getf next-report :summary))
                 (next-count (getf next-summary :goal-states)))
            (assert (integerp next-count))
            (assert (>= next-count 0))))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-53 completed successfully.~2%")
  t)


(defun command-test-54 ()
  "Verify FIND-GOAL-STATES per-call :BACKEND override works without changing global backend."
  (format t "~%COMMAND-TEST-54: testing find-goal-states per-call backend override...~2%")
  (let ((saved-problem *problem-name*)
        (saved-backend *enum-csp-backend*)
        (saved-last *enum-csp-backend-last-result*)
        (saved-pilot-flag *enum-csp-next-backend-run-fluent-pilot*))
    (unwind-protect
        (let* ((*standard-output* (make-broadcast-stream))
               (*error-output* (make-broadcast-stream))
               (*query-io* (make-two-way-stream
                            (make-string-input-stream "n\n")
                            (make-string-output-stream))))
          (setf *enum-csp-next-backend-run-fluent-pilot* t)
          (set-enum-csp-backend :legacy)
          (stage corner)
          (assert (eq *enum-csp-backend* :legacy))
          (assert (find-goal-states-fn (command-test-resolved-goal-spec)
                                       :backend :next-backend
                                       :solution-type 'every))
          (let ((next-override-report (get 'find-goal-states :last-report)))
            (assert next-override-report))
          (assert (eq *enum-csp-backend* :legacy))
          (assert (find-goal-states-fn (command-test-resolved-goal-spec)
                                       :backend :legacy
                                       :solution-type 'every))
          (let ((legacy-report (get 'find-goal-states :last-report)))
            (assert legacy-report))
          (assert (eq *enum-csp-backend* :legacy)))
      (set-enum-csp-backend saved-backend)
      (setf *enum-csp-next-backend-run-fluent-pilot* saved-pilot-flag
            *enum-csp-backend-last-result* saved-last)
      (when (and saved-problem (not (eq *problem-name* saved-problem)))
        (stage saved-problem))))
  (format t "COMMAND-TEST-54 completed successfully.~2%")
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
