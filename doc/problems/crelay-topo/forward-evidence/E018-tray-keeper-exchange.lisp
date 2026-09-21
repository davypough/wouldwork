;;; One FIRST/16-thread/cutoff-10 ghost-tray keeper-exchange capability search.
;;; Fresh stage CRELAY-TOPO, threads 16, cutoff 10, solution-type FIRST first.
;;; Set max-recorder-cycles to 3: the exact target uses cycle count 3.
;;; No old REPL globals, sealed solutions, serial imports, or automatic follow-up.
(in-package :ww)

(defun forward-e018-read (path)
  (with-open-file (stream path)
    (let ((*read-eval* nil) (*package* (find-package :ww)))
      (read stream))))

(defun forward-e018-write (path data)
  (with-open-file (stream path :direction :output :if-exists :error
                               :if-does-not-exist :create)
    (let ((*print-readably* t) (*print-pretty* t) (*package* (find-package :ww)))
      (write data :stream stream)
      (terpri stream))))

(defun forward-e018-parent (origin archive checkpoint)
  (assert (equalp (make-subgoal-progress-state-signature origin)
                  (getf archive :origin)) () "Fresh stage and thread setup required.")
  (let* ((actions (getf checkpoint :actions))
         (replay (validate-action-sequence origin actions)))
    (assert (= (length actions) 61))
    (assert (action-sequence-validation-success-p replay))
    (let ((parent (action-sequence-validation-final-state replay)))
      (assert (equalp (make-subgoal-progress-state-signature parent)
                      (getf checkpoint :endpoint)))
      (install-compiled-goal (getf archive :goal))
      (assert (funcall (symbol-function 'goal-fn) parent))
      (multiple-value-bind (valid diagnostic)
          (validate-recorder-solution origin actions parent)
        (format t "~&E018-PARENT-C016-RECORDER=~S ~S~%" valid diagnostic)
        (assert valid))
      parent)))

(defun forward-e018-raw-candidate (solution index prefix)
  (list :candidate index :parent 'c016 :status :unvalidated
        :segment (copy-tree (solution.path solution))
        :actions (append (copy-tree prefix) (copy-tree (solution.path solution)))
        :endpoint (make-subgoal-progress-state-signature (solution.goal solution))))

(defun forward-e018-replay-matches (replay signature)
  (and (action-sequence-validation-success-p replay)
       (action-sequence-validation-goal-satisfied-p replay)
       (equalp signature
               (make-subgoal-progress-state-signature
                 (action-sequence-validation-final-state replay)))))

(defun forward-e018-validate (origin parent raw)
  (let* ((record (copy-tree raw))
         (signature (getf raw :endpoint))
         (segment (validate-action-sequence parent (getf raw :segment)
                    :goal-test (symbol-function 'goal-fn)))
         (cumulative (validate-action-sequence origin (getf raw :actions)
                       :goal-test (symbol-function 'goal-fn)))
         (segment-ok (forward-e018-replay-matches segment signature))
         (cumulative-ok (forward-e018-replay-matches cumulative signature)))
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
      (format t "~&E018-CANDIDATE-~D STATUS=~S SEGMENT=~S CUMULATIVE=~S RECORDER=~S ~S~%"
              (getf raw :candidate) (getf record :status)
              segment-ok cumulative-ok recorder-ok diagnostic)
      record)))

(defun forward-e018-settings ()
  (loop for symbol in '(*threads* *depth-cutoff* *solution-type* *algorithm*
                       *tree-or-graph* *randomize-search* *symmetry-pruning*
                       *min-steps-pruning-enabled* *recorder-prefix-pruning*
                       *max-recorder-cycles* *max-connector-pairings*)
        collect (list symbol (symbol-value symbol))))

(defun forward-e018-run (request-path raw-path accepted-path)
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (assert (= *depth-cutoff* 10))
  (assert (eql *max-recorder-cycles* 3))
  (assert (eq *solution-type* 'first))
  (assert (null *goal-chain-session*))
  (let* ((archive (forward-e018-read
                   "doc/problems/crelay-topo/forward-evidence/E017-candidates.sexp"))
         (checkpoint (find 1 (getf archive :candidates)
                           :key (lambda (row) (getf row :candidate))))
         (origin (copy-problem-state *start-state*))
         (original-goal (copy-tree *goal*))
         (settings (forward-e018-settings)))
    (assert (eq (getf archive :problem) 'crelay-topo))
    (assert (eq (getf checkpoint :status) :accepted-prefix))
    (assert (equal original-goal '(has-location agent1 location19)))
    (let* ((parent (forward-e018-parent origin archive checkpoint))
           (parent-signature (make-subgoal-progress-state-signature parent))
           (started (get-internal-real-time)))
      (format t "~&E018-SETTINGS=~S~%E018-PARENT=~S~%" settings parent-signature)
      (forward-e018-write request-path
        (list :problem 'crelay-topo :experiment 'e018 :parent 'c016
              :origin (make-subgoal-progress-state-signature origin)
              :original-goal original-goal :settings settings
              :parent-actions (getf checkpoint :actions)
              :parent-endpoint parent-signature
              :goal '(and (on box1 plate5) (on box1* plate1)
             (on tray1 plate2) (on connector1* plate2)
             (holding agent1* tray1*) (has-location agent1* location7)
             (holding agent1 connector1) (has-location agent1 location12)
             (paired connector1* transmitter1) (paired connector1* repeater1)
             (switched-on switch2) (not (recording-switched-on switch2))
             (open gate7) (recording-open gate5) (recording-open gate3)
             (recording-in-progress) (recorder-cycles-used 3))
              :status :request-saved-before-search))
      (finish-output)
      (solve-subgoal parent
        (and (on box1 plate5) (on box1* plate1)
             (on tray1 plate2) (on connector1* plate2)
             (holding agent1* tray1*) (has-location agent1* location7)
             (holding agent1 connector1) (has-location agent1 location12)
             (paired connector1* transmitter1) (paired connector1* repeater1)
             (switched-on switch2) (not (recording-switched-on switch2))
             (open gate7) (recording-open gate5) (recording-open gate3)
             (recording-in-progress) (recorder-cycles-used 3)))
      ;; Save returned actions before replay/recorder checking can fail.
      (let* ((elapsed (/ (- (get-internal-real-time) started)
                         (float internal-time-units-per-second)))
             (raw (loop for solution in *solution-paths* for index from 1
                        collect (forward-e018-raw-candidate
                                  solution index (getf checkpoint :actions))))
             (bundle (list :problem 'crelay-topo :experiment 'e018 :parent 'c016
                           :origin (make-subgoal-progress-state-signature origin)
                           :original-goal original-goal :goal (copy-tree *goal*)
                           :settings settings :elapsed-seconds elapsed
                           :solutions-valid *solutions-valid*
                           :outcome (search-outcome-status *last-search-outcome*)
                           :reason (search-outcome-reason *last-search-outcome*)
                           :candidates raw)))
        (forward-e018-write raw-path bundle)
        (assert (equalp parent-signature
                        (make-subgoal-progress-state-signature *start-state*)))
        (format t "~&E018-OUTCOME=~S REASON=~S ELAPSED=~,3F RAW-CANDIDATES=~D~%"
                (getf bundle :outcome) (getf bundle :reason) elapsed (length raw))
        (finish-output)
        (if *solutions-valid*
            (setf (getf bundle :candidates)
                  (loop for row in raw collect (forward-e018-validate origin parent row)))
            (format t "E018: non-normal result; raw candidates remain unaccepted.~%"))
        (forward-e018-write accepted-path bundle)
        (format t "E018: records saved; inspect each STATUS. No final solution claimed.~%")
        (format t "E018: no further search; assess northern tray delivery and live boarding.~%")
        (finish-output)
        bundle))))

(defun forward-e018 (&optional
                       (stem "doc/problems/crelay-topo/forward-evidence/E018"))
  "One ghost-tray keeper-exchange search with reconstruction, capture and validation."
  (let ((log-path (concatenate 'string stem "-output.txt"))
        (request-path (concatenate 'string stem "-request.sexp"))
        (raw-path (concatenate 'string stem "-raw.sexp"))
        (accepted-path (concatenate 'string stem "-candidates.sexp")))
    (dolist (path (list log-path request-path raw-path accepted-path))
      (assert (not (probe-file path)) () "Refusing to overwrite ~A" path))
    (with-open-file (stream log-path :direction :output :if-exists :error
                                    :if-does-not-exist :create)
      (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
        (unwind-protect
             (forward-e018-run request-path raw-path accepted-path)
          (finish-output))))))
