;;; Passive CLOSED-arrival checks for the supplied BOXES-ADVISOR instance.
;;; Load after staging and spec-analysis-index-checks.lisp.
;;; Loading defines functions only. Call (check-boxes-graph-model) explicitly.
;;; Eight fixed cases; no solve, full-path replay or concurrent worker launch.

(in-package :ww)

(defun boxes-graph-check-require (condition control &rest arguments)
  (unless condition
    (error "Boxes GRAPH check: ~A" (apply #'format nil control arguments))))

(defun boxes-graph-check-child (parent signature)
  (let ((matches (remove-if-not
                  (lambda (child) (equal (boxes-check-signature child) signature))
                  (generate-children parent))))
    (boxes-graph-check-require
     (= (length matches) 1) "Expected one legal successor ~S, got ~D."
     signature (length matches))
    (first matches)))

(defun boxes-graph-check-table (mode state)
  (ecase mode
    (serial *closed*)
    (worker (closed-shard state))))

(defun boxes-graph-check-entry-count (mode)
  ;; Count entries, not hash keys: distinct arrangements can share a hash bucket.
  (loop for table in (ecase mode
                      (serial (list *closed*))
                      (worker (coerce *closed-shards* 'list)))
        sum (loop for bucket being the hash-values of table
                  sum (length bucket))))

(defun boxes-graph-check-arrival (mode context parent source retained-source
                                incoming-depth retained-depth accepted-p distinct-p)
  (clrhash *closed*)
  (loop for table across *closed-shards* do (clrhash table))
  (let* ((*threads* (ecase mode (serial 0) (worker 16)))
         (incoming (copy-problem-state source))
         (retained (copy-problem-state retained-source))
         (current (make-node :state (copy-problem-state (node.state parent))
                             :depth (1- incoming-depth)))
         (stats (make-worker-stats))
         (before-repeats *repeated-states*)
         (before-paths *duplicate-num-paths*)
         (before-depths *duplicate-accumulated-depths*)
         (old-entry nil)
         (nodes nil))
    ;; Costs are controlled ledger fixtures, not a replay of the longer route.
    (setf (problem-state.time (node.state current)) (float (1- incoming-depth))
          (problem-state.time incoming) (float incoming-depth)
          (problem-state.time retained) (float retained-depth)
          old-entry (make-closed-entry retained retained-depth))
    (closed-bucket-insert old-entry retained retained-depth
                          (boxes-graph-check-table mode retained))
    (setf nodes
          (ecase mode
            (serial (process-successors (list incoming) current (hs::make-hstack)))
            (worker (worker-process-successors-phase1 (list incoming) current 0 stats))))
    (boxes-graph-check-require
     (and (listp nodes) (= (length nodes) (if accepted-p 1 0)))
     "~S/~S: expected ~D returned node(s), got ~S."
     mode context (if accepted-p 1 0) nodes)
    (when accepted-p
      (boxes-graph-check-require
       (and (= (node.depth (first nodes)) incoming-depth)
            (eq (node.parent (first nodes)) current)
            (eq (node.state (first nodes)) incoming))
       "~S/~S: returned node lost the incoming state, depth or new parent."
       mode context))
    (let ((entry (closed-bucket-find incoming incoming-depth
                                     (boxes-graph-check-table mode incoming))))
      (boxes-graph-check-require
       (and entry
            (= (second entry) (if accepted-p incoming-depth retained-depth))
            (= (third entry) (if accepted-p incoming-depth retained-depth))
            (equalp (first entry) (problem-state.idb incoming))
            (if accepted-p (not (eq entry old-entry)) (eq entry old-entry)))
       "~S/~S: CLOSED failed to replace/preserve the expected arrangement and cost."
       mode context))
    (when distinct-p
      (boxes-graph-check-require
       (eq old-entry (closed-bucket-find retained retained-depth
                                         (boxes-graph-check-table mode retained)))
       "~S/~S: admitting PLATE2 allocation lost the PLATE1 allocation." mode context))
    (boxes-graph-check-require
     (= (boxes-graph-check-entry-count mode) (if distinct-p 2 1))
     "~S/~S: unexpected number of retained arrangements." mode context)
    (let ((repeats (ecase mode
                     (serial (- *repeated-states* before-repeats))
                     (worker (ws-repeated-states stats))))
          (paths (ecase mode
                   (serial (- *duplicate-num-paths* before-paths))
                   (worker (ws-duplicate-num-paths stats))))
          (depths (ecase mode
                    (serial (- *duplicate-accumulated-depths* before-depths))
                    (worker (ws-duplicate-accumulated-depths stats)))))
      (boxes-graph-check-require
       (and (= repeats (if distinct-p 0 1))
            (= paths (if accepted-p 0 1))
            (= depths (if accepted-p 0 incoming-depth)))
       "~S/~S: wrong repeated/terminated-path counts ~S."
       mode context (list repeats paths depths)))
    1))

(defun boxes-graph-check-cases ()
  (let* ((start (make-node :state (copy-problem-state *start-state*) :depth 0))
         (pickup (boxes-graph-check-child start '(area1 (0 1 0 0) nil t)))
         (holding (make-node :state pickup :depth 1 :parent start))
         (plate1 (boxes-graph-check-child holding '(area1 (0 1 0 0) (plate1) nil)))
         (plate2 (boxes-graph-check-child holding '(area1 (0 1 0 0) (plate2) nil)))
         (count 0))
    (dolist (mode '(serial worker) count)
      (incf count (boxes-graph-check-arrival mode 'cheaper start pickup pickup 1 3 t nil))
      (incf count (boxes-graph-check-arrival mode 'equal start pickup pickup 1 1 nil nil))
      (incf count (boxes-graph-check-arrival mode 'worse start pickup pickup 3 1 nil nil))
      (incf count (boxes-graph-check-arrival mode 'distinct-plates holding plate2 plate1
                                            2 2 t t)))))

(defun check-boxes-graph-model ()
  (boxes-graph-check-require
   (and (eq *problem-name* 'boxes-advisor)
        (eq *problem-type* 'planning) (eq *algorithm* 'depth-first)
        (eq *tree-or-graph* 'graph) (eq *solution-type* 'min-length)
        (= *threads* 16) (zerop *depth-cutoff*)
        (not *symmetry-pruning*) (not *hybrid-mode*))
   "Stage BOXES-ADVISOR with DEPTH-FIRST/GRAPH/MIN-LENGTH, THREADS=16, cutoff 0, no symmetry.")
  (boxes-graph-check-require
   (and (not *parallel-search-active*) (not *worker-read-phase*)
        (null *search-prefix-validators*) (null *search-successor-pruners*)
        (null *global-invariants*) (null *happening-names*)
        (not *troubleshoot-current-node*) (not (fboundp 'heuristic?))
        (null *trace-action-name*) (boundp 'goal-fn))
   "Expected an idle supplied model without additional policies, tracing or happenings.")
  (let ((saved-closed *closed*)
        (saved-solutions *solution-paths*)
        (saved-repeats *repeated-states*)
        (saved-paths *duplicate-num-paths*)
        (saved-depths *duplicate-accumulated-depths*)
        (saved-inconsistent *inconsistent-states-dropped*)
        (start-before (copy-problem-state *start-state*))
        (static-before (boxes-check-static-snapshot))
        (*debug* 0)
        (*closed-shard-mask* (1- *num-closed-shards*))
        (*closed-shards* (make-array *num-closed-shards*
                                    :initial-contents
                                    (loop repeat *num-closed-shards*
                                          collect (make-hash-table :test #'eql))))
        (*closed-shard-locks* (make-array *num-closed-shards*
                                         :initial-contents
                                         (loop repeat *num-closed-shards*
                                               collect (sb-thread:make-mutex))))
        (count nil))
    ;; These six variables are SBCL DEFGLOBALs: save/set/restore, never LET-bind.
    ;; Shards, locks, DEBUG and THREADS are ordinary dynamically bound specials.
    (unwind-protect
         (progn
           (setf *closed* (make-hash-table :test #'eql)
                 *solution-paths* nil)
           (setf count (boxes-graph-check-cases))
           (boxes-graph-check-require
            (= *inconsistent-states-dropped* saved-inconsistent)
            "Generated fixture actions dropped an inconsistent successor.")
           (boxes-graph-check-require
            (and (equalp (problem-state.idb *start-state*)
                         (problem-state.idb start-before))
                 (equal static-before (boxes-check-static-snapshot)))
            "Checks changed the start arrangement or static topology indexes."))
      (setf *closed* saved-closed
            *solution-paths* saved-solutions
            *repeated-states* saved-repeats
            *duplicate-num-paths* saved-paths
            *duplicate-accumulated-depths* saved-depths
            *inconsistent-states-dropped* saved-inconsistent))
    (format t "~&BOXES-GRAPH-CHECKS-PASSED: ~D cases, serial and worker successor paths.~%"
            count)
    t))
