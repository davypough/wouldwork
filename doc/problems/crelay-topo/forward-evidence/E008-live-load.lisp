;;; User approved one FIRST/16-thread/cutoff-12 search with cumulative validation.
;;; Fresh stage CRELAY-TOPO, threads 16, cutoff 12, solution-type FIRST first.
;;; No old REPL globals, sealed solutions, serial imports, or automatic follow-up.
(in-package :ww)

(defun forward-e008-read (path)
  (with-open-file (stream path)
    (let ((*read-eval* nil) (*package* (find-package :ww)))
      (read stream))))

(defun forward-e008-write (path data)
  (with-open-file (stream path :direction :output :if-exists :error
                               :if-does-not-exist :create)
    (let ((*print-readably* t) (*print-pretty* t) (*package* (find-package :ww)))
      (write data :stream stream)
      (terpri stream))))

(defun forward-e008-parent (origin archive checkpoint)
  (assert (equalp (make-subgoal-progress-state-signature origin)
                  (getf archive :origin)) () "Fresh stage and thread setup required.")
  (let* ((actions (getf checkpoint :actions))
         (replay (validate-action-sequence origin actions)))
    (assert (= (length actions) 21))
    (assert (action-sequence-validation-success-p replay))
    (let ((parent (action-sequence-validation-final-state replay)))
      (assert (equalp (make-subgoal-progress-state-signature parent)
                      (getf checkpoint :endpoint)))
      (install-compiled-goal (getf archive :goal))
      (assert (funcall (symbol-function 'goal-fn) parent))
      (multiple-value-bind (valid diagnostic)
          (validate-recorder-solution origin actions parent)
        (format t "~&E008-PARENT-C008-RECORDER=~S ~S~%" valid diagnostic)
        (assert valid))
      parent)))

(defun forward-e008-raw-candidate (solution index prefix)
  (list :candidate index :parent 'c008 :status :unvalidated
        :segment (copy-tree (solution.path solution))
        :actions (append (copy-tree prefix) (copy-tree (solution.path solution)))
        :endpoint (make-subgoal-progress-state-signature (solution.goal solution))))

(defun forward-e008-replay-matches (replay signature)
  (and (action-sequence-validation-success-p replay)
       (action-sequence-validation-goal-satisfied-p replay)
       (equalp signature
               (make-subgoal-progress-state-signature
                 (action-sequence-validation-final-state replay)))))

(defun forward-e008-validate (origin parent raw)
  (let* ((record (copy-tree raw))
         (signature (getf raw :endpoint))
         (segment (validate-action-sequence parent (getf raw :segment)
                    :goal-test (symbol-function 'goal-fn)))
         (cumulative (validate-action-sequence origin (getf raw :actions)
                       :goal-test (symbol-function 'goal-fn)))
         (segment-ok (forward-e008-replay-matches segment signature))
         (cumulative-ok (forward-e008-replay-matches cumulative signature)))
    (multiple-value-bind (recorder-ok diagnostic)
        (if cumulative-ok
            (validate-recorder-solution origin (getf raw :actions)
              (action-sequence-validation-final-state cumulative))
            (values nil :cumulative-replay-failed))
      (setf (getf record :status)
            (if (and segment-ok cumulative-ok recorder-ok)
                :accepted-prefix :rejected-candidate)
            (getf record :segment-valid) segment-ok
            (getf record :cumulative-valid) cumulative-ok
            (getf record :recorder-valid) recorder-ok
            (getf record :recorder-diagnostic) diagnostic
            (getf record :segment-failure)
            (action-sequence-validation-failure-reason segment)
            (getf record :cumulative-failure)
            (action-sequence-validation-failure-reason cumulative))
      (format t "~&E008-CANDIDATE-~D STATUS=~S SEGMENT=~S CUMULATIVE=~S RECORDER=~S ~S~%"
              (getf raw :candidate) (getf record :status)
              segment-ok cumulative-ok recorder-ok diagnostic)
      record)))

(defun forward-e008-settings ()
  (loop for symbol in '(*threads* *depth-cutoff* *solution-type* *algorithm*
                       *tree-or-graph* *randomize-search* *symmetry-pruning*
                       *min-steps-pruning-enabled* *recorder-prefix-pruning*
                       *max-recorder-cycles* *max-connector-pairings*)
        collect (list symbol (symbol-value symbol))))

(defun forward-e008-run (raw-path accepted-path)
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (assert (= *depth-cutoff* 12))
  (assert (eq *solution-type* 'first))
  (assert (null *goal-chain-session*))
  (let* ((archive (forward-e008-read
                   "doc/problems/crelay-topo/forward-evidence/E006-candidates.sexp"))
         (checkpoint (find 1 (getf archive :candidates)
                           :key (lambda (row) (getf row :candidate))))
         (origin (copy-problem-state *start-state*))
         (original-goal (copy-tree *goal*))
         (settings (forward-e008-settings)))
    (assert (eq (getf archive :problem) 'crelay-topo))
    (assert (equal original-goal '(has-location agent1 location19)))
    (let* ((parent (forward-e008-parent origin archive checkpoint))
           (parent-signature (make-subgoal-progress-state-signature parent))
           (started (get-internal-real-time)))
      (format t "~&E008-SETTINGS=~S~%E008-PARENT=~S~%" settings parent-signature)
      (solve-subgoal parent
        (and (holding agent1 tray1)
             (on box1 tray1)
             (on connector1 box1)
             (has-location agent1 location5)
             (recording-in-progress)
             (recorder-cycles-used 2)))
      ;; Save returned actions before replay/recorder checking can fail.
      (let* ((elapsed (/ (- (get-internal-real-time) started)
                         (float internal-time-units-per-second)))
             (raw (loop for solution in *solution-paths* for index from 1
                        collect (forward-e008-raw-candidate
                                  solution index (getf checkpoint :actions))))
             (bundle (list :problem 'crelay-topo :experiment 'e008 :parent 'c008
                           :origin (make-subgoal-progress-state-signature origin)
                           :original-goal original-goal :goal (copy-tree *goal*)
                           :settings settings :elapsed-seconds elapsed
                           :solutions-valid *solutions-valid*
                           :outcome (search-outcome-status *last-search-outcome*)
                           :reason (search-outcome-reason *last-search-outcome*)
                           :candidates raw)))
        (forward-e008-write raw-path bundle)
        (assert (equalp parent-signature
                        (make-subgoal-progress-state-signature *start-state*)))
        (format t "~&E008-OUTCOME=~S REASON=~S ELAPSED=~,3F RAW-CANDIDATES=~D~%"
                (getf bundle :outcome) (getf bundle :reason) elapsed (length raw))
        (if *solutions-valid*
            (setf (getf bundle :candidates)
                  (loop for row in raw collect (forward-e008-validate origin parent row)))
            (format t "E008: non-normal result; raw candidates remain unaccepted.~%"))
        (forward-e008-write accepted-path bundle)
        (format t "E008: records saved; inspect each STATUS. No final solution claimed.~%")
        (format t "E008: no further search; assess keeper, gate3 and north-access obligations.~%")
        bundle))))

(defun forward-e008 (&optional
                       (stem "doc/problems/crelay-topo/forward-evidence/E008"))
  "One approved search with reconstruction, raw capture and cumulative validation."
  (let ((log-path (concatenate 'string stem "-output.txt"))
        (raw-path (concatenate 'string stem "-raw.sexp"))
        (accepted-path (concatenate 'string stem "-candidates.sexp")))
    (dolist (path (list log-path raw-path accepted-path))
      (assert (not (probe-file path)) () "Refusing to overwrite ~A" path))
    (with-open-file (stream log-path :direction :output :if-exists :error
                                    :if-does-not-exist :create)
      (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
        (forward-e008-run raw-path accepted-path)))))
