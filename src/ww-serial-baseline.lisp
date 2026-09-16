;;; REPL-only diagnostics. Load explicitly; no search runs on load.
(in-package :ww)

(defvar *serial-baseline-results* nil
  "Newest baseline first. Survives reloading this helper.")

(defun serial-baseline-settings ()
  "Record effective controls without changing the staged problem."
  (loop for name in '(*problem-name* *algorithm* *tree-or-graph*
                     *solution-type* *depth-cutoff* *threads* *debug*
                     *randomize-search* *symmetry-pruning* *branch*
                     *progress-reporting-interval* *split-depth-max*
                     *tasks-per-thread* *min-tasks* *num-closed-shards*
                     *bound-refresh-interval* *enable-work-donation*
                     *donation-check-interval* *donation-threshold*
                     *donation-fraction*)
        collect (list name (symbol-value name))))

(defun serial-baseline-best-solution ()
  (first (sort (copy-list *solution-paths*) #'< :key #'solution.depth)))

(defun serial-baseline-record (label settings wall cpu allocated gc-time)
  "Capture measurements before replay can affect any search counters."
  (let ((best (serial-baseline-best-solution)))
    (list :label label :settings settings
          :lisp (lisp-implementation-version) :machine (machine-type)
          :outcome (search-outcome-status *last-search-outcome*)
          :reason (search-outcome-reason *last-search-outcome*)
          :wall-seconds wall :process-cpu-seconds cpu
          :allocated-bytes allocated :gc-seconds gc-time
          :states *total-states-processed* :cycles *program-cycles*
          :states-per-second (/ *total-states-processed* wall)
          :duplicates *repeated-states*
          :symmetry-pruned (if (use-canonical-symmetry-p)
                              *symmetric-duplicates-pruned*
                              *symmetry-pruning-count*)
          :best-depth (when best (solution.depth best))
          :best-path (when best (copy-tree (solution.path best)))
          :replay-valid nil)))

(defun serial-baseline-preflight (expected-root snapshot-code-p)
  "Check version admission and source selection without starting a search."
  (assert (equal (truename expected-root)
                 (truename (asdf:system-source-directory :wouldwork))))
  (assert (and (zerop *threads*) (< *branch* 1) (null *probe*)
               (not *parallel-search-active*)))
  (assert (eql (not (null (find-symbol "*WORKER-READ-SNAPSHOTS*" :ww)))
               snapshot-code-p))
  (dolist (name '("*WORKER-READ-SNAPSHOTS*" "*WORKER-READ-PHASE*"
                  "*WORKER-STATIC-READ-VIEW*" "*WORKER-CODE-READ-VIEW*"))
    (let ((symbol (find-symbol name :ww)))
      (when symbol
        (assert (boundp symbol))
        (assert (null (symbol-value symbol))))))
  (format t "~&SERIAL SOURCE ~A SNAPSHOT-CODE ~S~%"
          (asdf:system-source-directory :wouldwork) snapshot-code-p)
  t)

(defun run-serial-baseline (label expected-root snapshot-code-p)
  "Run the staged Claustro baseline once, then replay its best solution.
Timing includes WW-SOLVE initialization and reporting, excludes staging and replay.
Use WW-SET to configure threads before calling; this helper never rebinds them."
  (serial-baseline-preflight expected-root snapshot-code-p)
  (assert (and (eq *problem-name* 'claustro-topo)
               (eq *algorithm* 'depth-first) (eq *tree-or-graph* 'graph)
               (eq *solution-type* 'min-length) (= *depth-cutoff* 34)
               *symmetry-pruning* (not *randomize-search*)
               (zerop *debug*)))
  (let ((settings (serial-baseline-settings)))
    (format t "~&BASELINE START ~S~%~S~%" label settings)
    (finish-output)
    (let ((wall (get-internal-real-time))
          (cpu (get-internal-run-time))
          (allocated (sb-ext:get-bytes-consed))
          (gc-time sb-ext:*gc-run-time*))
      (ww-solve)
      (let* ((elapsed (/ (- (get-internal-real-time) wall)
                         (float internal-time-units-per-second)))
             (result (serial-baseline-record
                      label settings elapsed
                      (/ (- (get-internal-run-time) cpu)
                         (float internal-time-units-per-second))
                      (- (sb-ext:get-bytes-consed) allocated)
                      (/ (- sb-ext:*gc-run-time* gc-time)
                         (float internal-time-units-per-second)))))
        (push result *serial-baseline-results*)
        (when (getf result :best-depth)
          (setf (getf result :replay-valid)
                (not (null (%validate-solution (getf result :best-path) nil)))))
        (format t "~&BASELINE RESULT BEGIN~%~S~%BASELINE RESULT END~%" result)
        (unless (and (eq (getf result :outcome) :exhausted-with-solutions)
                     (eql (getf result :best-depth) 33)
                     (getf result :replay-valid))
          (warn "Baseline did not meet expected completion/depth/replay checks."))
        result))))
