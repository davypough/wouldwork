;;; Filename: ww-parallel-infrastructure.lisp

;;; Low-level infrastructure for partitioned parallel search:
;;; - Task queue (thread-safe FIFO with blocking pop)
;;; - Worker-local statistics (contention-free counters)
;;; - Sharded closed table (Phase 2)

(in-package :ww)


;;; ============================================================
;;; PARALLEL SEARCH PARAMETERS
;;; ============================================================

(defparameter *split-depth-max* 4
  "Maximum depth for task generation. With branching factor 4, 
   depth 4 yields up to 256 tasks.")

(defparameter *tasks-per-thread* 8
  "Target multiplier: aim for (* *tasks-per-thread* *threads*) tasks minimum.")

(defparameter *min-tasks* 256
  "Minimum number of tasks to generate regardless of thread count.")

(defparameter *num-closed-shards* 64
  "Number of shards for parallel *closed* table. Must be power of 2.
   Default 64 provides good distribution for up to 64 cores.")

(defparameter *closed-shard-mask* (1- *num-closed-shards*)
  "Bitmask for fast shard computation: (logand hash mask).")


;;; ============================================================
;;; TASK QUEUE STRUCTURE
;;; ============================================================
;;; Thread-safe FIFO queue with blocking pop and termination signaling.
;;; Uses circular buffer for efficiency.

(defconstant +initial-task-queue-size+ 4096
  "Initial capacity of task queue. Will grow if needed during task generation.")


(defstruct (task-queue (:conc-name tq-))
  "Thread-safe task queue with blocking pop capability."
  (buffer (make-array +initial-task-queue-size+ :initial-element nil)
          :type simple-vector)
  (head 0 :type fixnum)                          ; Next position to read
  (tail 0 :type fixnum)                          ; Next position to write  
  (count 0 :type fixnum)                         ; Current number of items
  (mutex (sb-thread:make-mutex :name "task-queue-mutex"))
  (waitqueue (sb-thread:make-waitqueue :name "task-queue-waitqueue"))
  (done-p nil :type boolean)                     ; Signals no more tasks coming
  (active-workers 0 :type fixnum))               ; Workers not waiting


(defun make-new-task-queue (&optional (size +initial-task-queue-size+))
  "Create a new task queue with specified initial size."
  (make-task-queue 
   :buffer (make-array size :initial-element nil)))


(defun tq-empty-p (queue)
  "Returns T if queue has no items. Caller should hold lock."
  (declare (type task-queue queue))
  (zerop (tq-count queue)))


(defun tq-grow-buffer (queue)
  "Double the queue buffer size. Caller must hold lock."
  (declare (type task-queue queue))
  (let* ((old-buffer (tq-buffer queue))
         (old-size (length old-buffer))
         (new-size (* old-size 2))
         (new-buffer (make-array new-size :initial-element nil))
         (head (tq-head queue))
         (count (tq-count queue)))
    ;; Copy elements maintaining logical order
    (loop for i from 0 below count
          for old-idx = (mod (+ head i) old-size)
          do (setf (svref new-buffer i) (svref old-buffer old-idx)))
    (setf (tq-buffer queue) new-buffer
          (tq-head queue) 0
          (tq-tail queue) count)))


(defun tq-push (queue item)
  "Push ITEM onto the queue. Thread-safe, grows buffer if needed."
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    (let ((buffer (tq-buffer queue))
          (tail (tq-tail queue))
          (count (tq-count queue)))
      ;; Grow if full
      (when (>= count (length buffer))
        (tq-grow-buffer queue)
        (setf buffer (tq-buffer queue)
              tail (tq-tail queue)))
      ;; Insert item
      (setf (svref buffer tail) item)
      (setf (tq-tail queue) (mod (1+ tail) (length buffer)))
      (incf (tq-count queue))
      ;; Wake one waiting worker
      (sb-thread:condition-notify (tq-waitqueue queue))))
  item)


(defun tq-push-many (queue items)
  "Push multiple ITEMS onto the queue efficiently. Thread-safe."
  (declare (type task-queue queue) (type list items))
  (when items
    (sb-thread:with-mutex ((tq-mutex queue))
      (let ((buffer (tq-buffer queue))
            (tail (tq-tail queue))
            (count (tq-count queue))
            (num-items (length items)))
        ;; Grow if needed (may need multiple grows for large batches)
        (loop while (>= (+ count num-items) (length buffer))
              do (tq-grow-buffer queue)
                 (setf buffer (tq-buffer queue)
                       tail (tq-tail queue)))
        ;; Insert all items
        (dolist (item items)
          (setf (svref buffer tail) item)
          (setf tail (mod (1+ tail) (length buffer)))
          (incf count))
        (setf (tq-tail queue) tail
              (tq-count queue) count)
        ;; Wake all waiting workers (we added multiple items)
        (sb-thread:condition-broadcast (tq-waitqueue queue)))))
  items)


(defun tq-try-pop (queue)
  "Non-blocking pop. Returns (values item t) or (values nil nil).
   Thread-safe."
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    (if (tq-empty-p queue)
        (values nil nil)
        (let* ((buffer (tq-buffer queue))
               (head (tq-head queue))
               (item (svref buffer head)))
          (setf (svref buffer head) nil)  ; Help GC
          (setf (tq-head queue) (mod (1+ head) (length buffer)))
          (decf (tq-count queue))
          (values item t)))))


(defun tq-pop-blocking (queue)
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    (decf (tq-active-workers queue)) ; mark waiting
    (let ((got-item-p nil))
      (unwind-protect
          (loop
            (unless (tq-empty-p queue)
              (let* ((buffer (tq-buffer queue))
                     (head (tq-head queue))
                     (item (svref buffer head)))
                (setf (svref buffer head) nil)
                (setf (tq-head queue) (mod (1+ head) (length buffer)))
                (decf (tq-count queue))
                (setf got-item-p t)
                (return item)))

            (when (and (tq-done-p queue)
                       (tq-empty-p queue)
                       (zerop (tq-active-workers queue)))
              (sb-thread:condition-broadcast (tq-waitqueue queue))
              (return nil))

            (sb-thread:condition-wait (tq-waitqueue queue) (tq-mutex queue)))
        ;; Only restore "active" if we actually got work.
        (when got-item-p
          (incf (tq-active-workers queue)))))))



(defun tq-signal-done (queue)
  "Signal that no more tasks will be added. Wake all waiting workers."
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    (setf (tq-done-p queue) t)
    (sb-thread:condition-broadcast (tq-waitqueue queue))))


(defun tq-register-worker (queue)
  "Register a worker as active. Call before worker starts processing."
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    (incf (tq-active-workers queue))))


;;; ============================================================
;;; WORK DONATION SUPPORT
;;; ============================================================
;;; Functions for load balancing via work donation from workers
;;; with excess local work back to the global task queue.

(defun tq-needs-work-p (queue)
  "Check if the task queue needs work (empty or nearly empty).
   Non-blocking check for donation decisions."
  (declare (type task-queue queue))
  ;; Simple check: queue is empty
  ;; Could be enhanced to check if workers are waiting
  (sb-thread:with-mutex ((tq-mutex queue))
    (zerop (tq-count queue))))


(defun tq-waiting-workers-p (queue)
  "Check if any workers are waiting for work.
   Returns T if active-workers < total workers expected."
  (declare (type task-queue queue))
  (sb-thread:with-mutex ((tq-mutex queue))
    ;; If count is 0 and not all workers are active, some must be waiting
    (and (zerop (tq-count queue))
         (< (tq-active-workers queue) *threads*))))


(defun extract-donation-nodes (local-stack num-to-donate)
  "Extract NUM-TO-DONATE nodes from LOCAL-STACK for donation.
   Preferentially extracts shallower nodes (larger subtrees).
   Returns (values donated-nodes remaining-stack).
   
   Strategy: Sort by depth, donate the shallowest half."
  (declare (type list local-stack) (type fixnum num-to-donate))
  (when (or (<= num-to-donate 0) (null local-stack))
    (return-from extract-donation-nodes (values nil local-stack)))
  
  ;; Sort by depth (ascending - shallowest first)
  (let ((sorted (sort (copy-list local-stack) #'< 
                      :key (lambda (n) (node.depth n)))))
    ;; Split: first num-to-donate are donated, rest are kept
    (let ((donated (subseq sorted 0 (min num-to-donate (length sorted))))
          (remaining (nthcdr num-to-donate sorted)))
      (values donated remaining))))


(defun compute-donation-count (stack-size)
  "Compute how many nodes to donate based on stack size and donation fraction."
  (declare (type fixnum stack-size))
  (max 1 (floor (* stack-size *donation-fraction*))))


(defun maybe-donate-work (local-stack task-queue worker-id stats)
  "Check if donation is appropriate and donate if so.
   Returns the (possibly reduced) local stack.
   
   Donation occurs when:
   1. Work donation is enabled
   2. Queue needs work (empty)
   3. Local stack exceeds threshold
   4. Search is not terminating"
  (declare (type list local-stack) 
           (type task-queue task-queue)
           (type fixnum worker-id)
           (type worker-stats stats))
  
  ;; Quick checks before acquiring any locks
  (unless *enable-work-donation*
    (return-from maybe-donate-work local-stack))
  
  (when (or *shutdown-requested* *first-solution-found*)
    (return-from maybe-donate-work local-stack))
  
  (let ((stack-size (length local-stack)))
    ;; Check threshold
    (unless (> stack-size *donation-threshold*)
      (return-from maybe-donate-work local-stack))
    
    ;; Check if queue needs work
    (unless (tq-needs-work-p task-queue)
      (return-from maybe-donate-work local-stack))
    
    ;; Perform donation
    (let ((num-to-donate (compute-donation-count stack-size)))
      (multiple-value-bind (donated remaining)
          (extract-donation-nodes local-stack num-to-donate)
        (when donated
          ;; Push donated nodes to task queue
          (tq-push-many task-queue donated)
          ;; Update stats
          (incf (ws-nodes-donated stats) (length donated))
          (incf (ws-donation-events stats))
          ;; Log donation (optional, for debugging)
          (when (>= *debug* 2)
            (bt:with-lock-held (*lock*)
              (format t "~&[Worker ~D] Donated ~D nodes (had ~D, keeping ~D)~%"
                      worker-id (length donated) stack-size (length remaining))
              (finish-output))))
        remaining))))


;;; ============================================================
;;; WORKER-LOCAL STATISTICS
;;; ============================================================
;;; Each worker maintains its own counters to avoid synchronization.
;;; Aggregated only at search completion.

(defstruct (worker-stats (:conc-name ws-))
  "Per-worker statistics accumulated during search."
  (states-processed 0 :type fixnum)        ; States expanded by this worker
  (repeated-states 0 :type fixnum)         ; Duplicates detected
  (program-cycles 0 :type fixnum)          ; Nodes expanded
  (max-depth 0 :type fixnum)               ; Deepest node seen
  (accumulated-depths 0 :type fixnum)      ; Sum of terminal path depths
  (num-paths 0 :type fixnum)               ; Number of terminated paths
  (solutions-found 0 :type fixnum)         ; Solutions found by this worker
  (nodes-donated 0 :type fixnum)           ; Nodes donated to queue
  (donation-events 0 :type fixnum))        ; Number of donation events


(defparameter *worker-stats-vector* nil
  "Vector of worker-stats structures, one per worker thread.")


(defun initialize-worker-stats (num-workers)
  "Create fresh statistics structures for all workers."
  (setf *worker-stats-vector*
        (make-array num-workers
                    :initial-contents
                    (loop repeat num-workers 
                          collect (make-worker-stats)))))


(defun get-worker-stats (worker-id)
  "Return the stats structure for WORKER-ID."
  (declare (type fixnum worker-id))
  (svref *worker-stats-vector* worker-id))


(defun aggregate-worker-stats ()
  "Sum all worker statistics into global variables. Called after search."
  (loop for stats across *worker-stats-vector*
        sum (ws-states-processed stats) into total-states
        sum (ws-repeated-states stats) into total-repeated
        sum (ws-program-cycles stats) into total-cycles
        maximize (ws-max-depth stats) into max-depth
        sum (ws-accumulated-depths stats) into total-depths
        sum (ws-num-paths stats) into total-paths
        finally
        (setf *total-states-processed* (+ *total-states-processed* total-states))
        (setf *repeated-states* (+ *repeated-states* total-repeated))
        (setf *program-cycles* (+ *program-cycles* total-cycles))
        (setf *max-depth-explored* (max *max-depth-explored* max-depth))
        (setf *accumulated-depths* (+ *accumulated-depths* total-depths))
        (setf *num-paths* (+ *num-paths* total-paths))))


(defun compute-donation-totals ()
  "Compute total donation statistics across all workers.
   Returns (values total-nodes-donated total-donation-events)."
  (loop for stats across *worker-stats-vector*
        sum (ws-nodes-donated stats) into total-donated
        sum (ws-donation-events stats) into total-events
        finally (return (values total-donated total-events))))


;;; Worker-local update functions (no locking needed)

(defun ws-inc-states (stats count)
  "Increment states-processed for worker."
  (declare (type worker-stats stats) (type fixnum count))
  (incf (ws-states-processed stats) count))


(defun ws-inc-repeated (stats)
  "Increment repeated-states for worker."
  (declare (type worker-stats stats))
  (incf (ws-repeated-states stats)))


(defun ws-inc-cycles (stats)
  "Increment program-cycles for worker."
  (declare (type worker-stats stats))
  (incf (ws-program-cycles stats)))


(defun ws-update-max-depth (stats depth)
  "Update max-depth if DEPTH is greater."
  (declare (type worker-stats stats) (type fixnum depth))
  (when (> depth (ws-max-depth stats))
    (setf (ws-max-depth stats) depth)))


(defun ws-finalize-path (stats depth)
  "Record a terminated path at DEPTH."
  (declare (type worker-stats stats) (type fixnum depth))
  (incf (ws-accumulated-depths stats) depth)
  (incf (ws-num-paths stats)))


(defun ws-inc-solutions (stats)
  "Increment solutions-found for worker."
  (declare (type worker-stats stats))
  (incf (ws-solutions-found stats)))


;;; ============================================================
;;; SHARDED CLOSED TABLE
;;; ============================================================
;;; N independent hash tables with per-shard mutexes.
;;; Provides concurrent access with atomic check+update semantics.

(defparameter *closed-shards* nil
  "Vector of hash tables for sharded *closed*. One per shard.")

(defparameter *closed-shard-locks* nil
  "Vector of mutexes for sharded *closed*. One per shard.")


(defun shard-index-for-hash (hash)
  "Compute shard index from hash value using bitmask."
  (declare (type fixnum hash))
  (logand hash *closed-shard-mask*))


(defun shard-index-for-state (state)
  "Compute shard index for a problem state."
  (declare (type problem-state state))
  (shard-index-for-hash (ensure-idb-hash state)))


(defun initialize-closed-shard-locks ()
  "Create the vector of per-shard mutexes."
  (setf *closed-shard-locks*
        (let ((locks (make-array *num-closed-shards*)))
          (dotimes (i *num-closed-shards* locks)
            (setf (svref locks i)
                  (sb-thread:make-mutex 
                   :name (format nil "closed-shard-~D" i)))))))


(defun initialize-closed-shards (hash-test)
  "Initialize the sharded closed table infrastructure.
   HASH-TEST is 'equal (hybrid mode) or 'eql (standard mode).
   Creates N independent hash tables and N mutexes."
  ;; Create shard locks
  (initialize-closed-shard-locks)
  ;; Create hash table shards
  ;; Size each shard to hold roughly (total-expected / num-shards) entries
  (let ((shard-size (ceiling 200003 *num-closed-shards*)))
    (setf *closed-shards*
          (let ((shards (make-array *num-closed-shards*)))
            (dotimes (i *num-closed-shards* shards)
              (setf (svref shards i)
                    (make-hash-table :test hash-test
                                     :size shard-size
                                     :rehash-size 2.0
                                     :rehash-threshold 0.8
                                     :synchronized nil)))))))


(defun closed-shard (state)
  "Return the hash table shard for STATE.
   Caller should hold the shard lock via with-closed-shard-lock."
  (declare (type problem-state state))
  (svref *closed-shards* (shard-index-for-state state)))


(defun closed-shard-lock (state)
  "Return the mutex for STATE's shard."
  (declare (type problem-state state))
  (svref *closed-shard-locks* (shard-index-for-state state)))


(defmacro with-closed-shard-lock ((state) &body body)
  "Execute BODY while holding the shard lock for STATE.
   STATE must be a problem-state with idb-hash computed (or computable).
   Multiple threads can execute concurrently if accessing different shards."
  (let ((lock-var (gensym "SHARD-LOCK")))
    `(let ((,lock-var (closed-shard-lock ,state)))
       (sb-thread:with-mutex (,lock-var)
         ,@body))))


(defmacro with-closed-shard-lock-by-index ((shard-index) &body body)
  "Execute BODY while holding the shard lock for SHARD-INDEX.
   Use when you have the shard index but not the full state."
  (let ((lock-var (gensym "SHARD-LOCK")))
    `(let ((,lock-var (svref *closed-shard-locks* ,shard-index)))
       (sb-thread:with-mutex (,lock-var)
         ,@body))))


;;; Closed table statistics (for debugging/monitoring)

(defun closed-shards-total-count ()
  "Return total entry count across all closed shards."
  (if *closed-shards*
      (loop for shard across *closed-shards*
            sum (hash-table-count shard))
      0))


(defun closed-shards-total-size ()
  "Return total allocated size across all closed shards."
  (if *closed-shards*
      (loop for shard across *closed-shards*
            sum (hash-table-size shard))
      0))


(defun closed-shards-distribution ()
  "Return list of (shard-index . count) for non-empty shards.
   Useful for checking distribution quality."
  (when *closed-shards*
    (loop for i from 0 below *num-closed-shards*
          for count = (hash-table-count (svref *closed-shards* i))
          when (> count 0)
            collect (cons i count))))


;;; ============================================================
;;; GLOBAL CONTROL FLAGS AND BOUND MANAGEMENT
;;; ============================================================

(defparameter *parallel-search-active* nil
  "T when partitioned parallel search is running.")

(defparameter *first-solution-found* nil
  "Set to T by any worker finding a solution when *solution-type* is FIRST.")

(defparameter *best-solution-lock* 
  (sb-thread:make-mutex :name "best-solution-lock")
  "Protects updates to *solutions* and *unique-solutions* during parallel search.")

(defparameter *best-bound* most-positive-fixnum
  "Current best solution bound for pruning. 
   For min-length/min-time/min-value: lower is better.
   For max-value: stored as negated value (so lower is still better).
   Workers cache this locally and refresh periodically.")

(defparameter *best-bound-lock*
  (sb-thread:make-mutex :name "best-bound-lock")
  "Protects updates to *best-bound*.")

(defparameter *bound-refresh-interval* 1000
  "Number of cycles between bound cache refreshes in workers.
   Lower = more responsive to bound updates, higher = less overhead.")

(defparameter *donation-check-interval* 10000  ;500
  "Number of cycles between donation eligibility checks.
   Lower = more responsive load balancing, higher = less overhead.")

(defparameter *donation-threshold* 256  ;32
  "Minimum local stack size before a worker considers donating work.
   Must have at least this many nodes to donate.")

(defparameter *donation-fraction* 0.2  ;0.5
  "Fraction of local stack to donate when donating (0.0 to 1.0).
   Donates the shallowest nodes (largest subtrees).")

(defparameter *enable-work-donation* t
  "When T, workers can donate excess work back to the task queue.
   Set to NIL to disable load balancing (for debugging/comparison).")


(defun initial-best-bound ()
  "Return the initial value for *best-bound* based on *solution-type*.
   All optimization types use 'lower is better' convention internally."
  (case *solution-type*
    ((first every) most-positive-fixnum)  ; Not used for pruning
    ((min-length min-time min-value) most-positive-fixnum)
    (max-value most-negative-fixnum)  ; Will be negated, so this becomes most-positive
    (otherwise most-positive-fixnum)))


(defun reset-parallel-control-flags ()
  "Reset all parallel control flags for a new search."
  (setf *parallel-search-active* nil
        *first-solution-found* nil
        *best-bound* (initial-best-bound)
        *shutdown-requested* nil))


(defun compute-state-bound-value (state depth)
  "Compute the bound value for STATE at DEPTH based on *solution-type*.
   Returns value in 'lower is better' convention."
  (declare (type problem-state state) (type fixnum depth))
  (case *solution-type*
    ((min-length first) depth)
    (min-time (problem-state.time state))
    (min-value (problem-state.value state))
    (max-value (- (problem-state.value state)))  ; Negate for 'lower is better'
    (otherwise depth)))


(defun node-can-improve-bound-p (node local-bound)
  "Check if NODE can possibly lead to a solution better than LOCAL-BOUND.
   Uses optimistic estimate (current depth/value as lower bound on final solution).
   Returns T if worth exploring, NIL if should prune."
  (declare (type node node))
  (case *solution-type*
    ;; For FIRST/EVERY, always explore (no optimization)
    ((first every) t)
    ;; For MIN-LENGTH, prune if already at or past best depth
    (min-length 
     (< (node.depth node) local-bound))
    ;; For MIN-TIME, prune if current time already >= best
    (min-time
     (< (problem-state.time (node.state node)) local-bound))
    ;; For MIN-VALUE, prune if current value already >= best
    (min-value
     (< (problem-state.value (node.state node)) local-bound))
    ;; For MAX-VALUE, prune if current value already <= best (remember: negated)
    (max-value
     (< (- (problem-state.value (node.state node))) local-bound))
    (otherwise t)))


(defun state-bound-better-p (new-bound current-bound)
  "Returns T if NEW-BOUND is better than CURRENT-BOUND.
   Both values should be in 'lower is better' convention."
  (< new-bound current-bound))


;;; ============================================================
;;; SOLUTION REGISTRATION (Thread-Safe)
;;; ============================================================

(defun register-parallel-solution (current-node goal-state worker-id)
  "Thread-safe solution registration during parallel search.
   Updates *solutions*, *unique-solutions*, and *best-bound* as needed."
  (declare (type node current-node) 
           (type problem-state goal-state)
           (type fixnum worker-id))
  (let* ((state-depth (1+ (node.depth current-node)))
         (solution
           (make-solution
            :depth state-depth
            :time (problem-state.time goal-state)
            :value (problem-state.value goal-state)
            :path (append (record-solution-path current-node)
                          (list (record-move goal-state)))
            :goal goal-state)))
    
    ;; Thread-safe solution list update
    (sb-thread:with-mutex (*best-solution-lock*)
      (push solution *solutions*)
      ;; Update unique solutions
      (let* ((new-idb (problem-state.idb (solution.goal solution)))
             (existing (find new-idb *unique-solutions*
                             :key (lambda (soln)
                                    (problem-state.idb (solution.goal soln)))
                             :test #'equalp)))
        (cond (existing
               (when (solution-better-p solution existing)
                 (setf *unique-solutions*
                       (substitute solution existing *unique-solutions*))))
              (t
               (push solution *unique-solutions*)))))
    
    ;; Update best bound for optimization solution types
    (when (member *solution-type* '(min-length min-time min-value max-value))
      (let ((new-bound (compute-state-bound-value goal-state state-depth)))
        (sb-thread:with-mutex (*best-bound-lock*)
          (when (state-bound-better-p new-bound *best-bound*)
            (setf *best-bound* new-bound)
            ;; Log bound improvement
            (bt:with-lock-held (*lock*)
              (format t "~&[Worker ~D] New best bound: ~A~%" worker-id 
                      (if (eql *solution-type* 'max-value)
                          (- new-bound)  ; Display un-negated for max-value
                          new-bound))
              (finish-output))))))
    
    ;; Signal first solution found
    (when (eql *solution-type* 'first)
      (setf *first-solution-found* t))
    
    ;; Console output (with lock to prevent garbled output)
    (bt:with-lock-held (*lock*)
      (format t "~&[Worker ~D] Solution found at depth ~D~%" worker-id state-depth)
      (when (member *solution-type* '(min-time))
        (format t "  Time = ~A~%" (solution.time solution)))
      (when (member *solution-type* '(min-value max-value))
        (format t "  Objective value = ~A~%" (solution.value solution)))
      (finish-output))
    
    ;; Update worker stats
    (ws-inc-solutions (get-worker-stats worker-id))
    
    solution))


;;; ============================================================
;;; PROGRESS REPORTING (Thread-Safe)
;;; ============================================================

(defparameter *progress-lock* 
  (sb-thread:make-mutex :name "progress-lock")
  "Prevents interleaved progress output from multiple workers.")

(defparameter *last-progress-time* 0
  "Internal time of last progress report.")

(defparameter *progress-interval-seconds* 60
  "Minimum seconds between progress reports.")


(defun maybe-report-parallel-progress (worker-id)
  "Report progress if enough time has passed. Only one worker reports at a time."
  (declare (type fixnum worker-id))
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-progress-time*) 
             (* *progress-interval-seconds* internal-time-units-per-second))
      ;; Try to get lock without blocking
      (when (sb-thread:grab-mutex *progress-lock* :waitp nil)
        (unwind-protect
            (when (> (- now *last-progress-time*)
                     (* *progress-interval-seconds* internal-time-units-per-second))
              (setf *last-progress-time* now)
              (report-parallel-progress worker-id))
          (sb-thread:release-mutex *progress-lock*))))))


(defun report-parallel-progress (worker-id)
  "Print current search progress aggregated across workers."
  (declare (type fixnum worker-id))
  ;; Aggregate current stats
  (let ((total-states 0)
        (total-cycles 0)
        (max-depth 0)
        (total-solutions 0))
    (loop for stats across *worker-stats-vector*
          do (incf total-states (ws-states-processed stats))
             (incf total-cycles (ws-program-cycles stats))
             (incf total-solutions (ws-solutions-found stats))
             (setf max-depth (max max-depth (ws-max-depth stats))))
    (bt:with-lock-held (*lock*)
      (format t "~2%[Progress from worker ~D]~%" worker-id)
      (format t "  Total states processed: ~:D~%" (+ *total-states-processed* total-states))
      (format t "  Program cycles: ~:D~%" (+ *program-cycles* total-cycles))
      (format t "  Max depth explored: ~D~%" (max *max-depth-explored* max-depth))
      (format t "  Solutions found so far: ~D~%" (+ (length *solutions*) total-solutions))
      (when (member *solution-type* '(min-length min-time min-value max-value))
        (let ((bound *best-bound*))
          (unless (= bound most-positive-fixnum)
            (format t "  Current best bound: ~A~%" 
                    (if (eql *solution-type* 'max-value)
                        (- bound)
                        bound)))))
      (when (and (eql *tree-or-graph* 'graph) *closed-shards*)
        (format t "  Closed entries: ~:D~%" (closed-shards-total-count)))
      (format t "  Elapsed time: ~:D sec~%"
              (round (/ (- (get-internal-real-time) *start-time*)
                        internal-time-units-per-second)))
      (finish-output))))


;;; ============================================================
;;; PHASE 5: CONFIGURATION DISPLAY AND HELP
;;; ============================================================

(defun display-parallel-parameters ()
  "Display current parallel search parameter settings."
  (format t "~2%Parallel Search Parameters:~%")
  (format t "~%  Core Settings:~%")
  (format t "    *threads*                    = ~D~%" *threads*)
  (format t "    *tree-or-graph*              = ~A~%" *tree-or-graph*)
  (format t "    *solution-type*              = ~A~%" *solution-type*)
  (format t "~%  Task Generation:~%")
  (format t "    *split-depth-max*            = ~D~%" *split-depth-max*)
  (format t "    *tasks-per-thread*           = ~D~%" *tasks-per-thread*)
  (format t "    *min-tasks*                  = ~D~%" *min-tasks*)
  (format t "~%  Closed Table Sharding:~%")
  (format t "    *num-closed-shards*          = ~D~%" *num-closed-shards*)
  (format t "~%  Branch-and-Bound:~%")
  (format t "    *bound-refresh-interval*     = ~D cycles~%" *bound-refresh-interval*)
  (format t "~%  Work Donation (Load Balancing):~%")
  (format t "    *enable-work-donation*       = ~A~%" *enable-work-donation*)
  (format t "    *donation-check-interval*    = ~D cycles~%" *donation-check-interval*)
  (format t "    *donation-threshold*         = ~D nodes~%" *donation-threshold*)
  (format t "    *donation-fraction*          = ~,2F~%" *donation-fraction*)
  (format t "~%  Progress Reporting:~%")
  (format t "    *progress-interval-seconds*  = ~D sec~%" *progress-interval-seconds*)
  (terpri))


(defun parallel-help ()
  "Display help information for parallel search configuration."
  (format t "~2%=== Wouldwork Parallel Search Help ===~%")
  (format t "~%BASIC USAGE:~%")
  (format t "  (ww-set *threads* N)    ; Set number of worker threads (0 = serial)~%")
  (format t "  (ww-solve)              ; Run search~%")
  (format t "~%TASK GENERATION:~%")
  (format t "  *split-depth-max*       ; Max depth for initial task creation (default: 4)~%")
  (format t "                          ; Higher = more tasks, more parallelism~%")
  (format t "  *tasks-per-thread*      ; Target tasks per thread (default: 8)~%")
  (format t "  *min-tasks*             ; Minimum tasks regardless of threads (default: 256)~%")
  (format t "~%CLOSED TABLE (Graph Search):~%")
  (format t "  *num-closed-shards*     ; Number of hash table shards (default: 64)~%")
  (format t "                          ; Should be power of 2, >= thread count~%")
  (format t "~%BRANCH-AND-BOUND (Optimization):~%")
  (format t "  *bound-refresh-interval*; Cycles between bound cache refresh (default: 1000)~%")
  (format t "                          ; Lower = more responsive, higher = less overhead~%")
  (format t "~%WORK DONATION (Load Balancing):~%")
  (format t "  *enable-work-donation*  ; Enable/disable donation (default: T)~%")
  (format t "  *donation-threshold*    ; Min stack size to donate (default: 32)~%")
  (format t "  *donation-check-interval*; Cycles between donation checks (default: 500)~%")
  (format t "  *donation-fraction*     ; Fraction to donate (default: 0.5)~%")
  (format t "~%TUNING TIPS:~%")
  (format t "  - For problems with uneven branching: lower *donation-threshold*~%")
  (format t "  - For very deep searches: increase *split-depth-max*~%")
  (format t "  - For many threads (>16): increase *num-closed-shards*~%")
  (format t "  - For debugging: set *debug* >= 2 to see donation events~%")
  (format t "~%FUNCTIONS:~%")
  (format t "  (display-parallel-parameters)  ; Show current settings~%")
  (format t "  (validate-parallel-settings)   ; Check for configuration issues~%")
  (format t "  (parallel-help)                ; Show this help~%")
  (terpri))


(defun validate-parallel-settings ()
  "Validate parallel search settings and report any issues.
   Returns T if all settings are valid, NIL if issues found."
  (let ((issues nil))
    (format t "~%Validating parallel search settings...~%")
    
    ;; Check threads
    (when (< *threads* 0)
      (push "  - *threads* must be >= 0" issues))
    (when (and (> *threads* 0) (< *threads* 2))
      (push "  - *threads* = 1 has overhead vs serial; use 0 or >= 2" issues))
    
    ;; Check split depth
    (when (< *split-depth-max* 1)
      (push "  - *split-depth-max* should be >= 1" issues))
    (when (> *split-depth-max* 10)
      (push "  - *split-depth-max* > 10 may generate excessive tasks" issues))
    
    ;; Check closed shards
    (when (and (> *threads* 0) 
               (not (zerop (logand *num-closed-shards* (1- *num-closed-shards*)))))
      (push "  - *num-closed-shards* should be power of 2" issues))
    (when (and (> *threads* 0) (< *num-closed-shards* *threads*))
      (push "  - *num-closed-shards* should be >= *threads* for good distribution" issues))
    
    ;; Check donation settings
    (when (and *enable-work-donation* (< *donation-threshold* 4))
      (push "  - *donation-threshold* < 4 may cause excessive donation overhead" issues))
    (when (and *enable-work-donation* 
               (or (< *donation-fraction* 0.1) (> *donation-fraction* 0.9)))
      (push "  - *donation-fraction* should be between 0.1 and 0.9" issues))
    
    ;; Check bound refresh
    (when (< *bound-refresh-interval* 100)
      (push "  - *bound-refresh-interval* < 100 may add overhead" issues))
    
    ;; Check algorithm compatibility
    (when (and (> *threads* 0) (eql *algorithm* 'backtracking))
      (push "  - Backtracking algorithm not supported in parallel mode" issues))
    
    ;; Report results
    (if issues
        (progn
          (format t "~%Issues found:~%")
          (dolist (issue (nreverse issues))
            (format t "~A~%" issue))
          nil)
        (progn
          (format t "  All settings valid.~%")
          t))))


;;; ============================================================
;;; PERFORMANCE METRICS
;;; ============================================================

(defstruct (parallel-timing (:conc-name pt-))
  "Timing breakdown for parallel search phases."
  (task-generation-ms 0 :type fixnum)
  (worker-search-ms 0 :type fixnum)
  (finalization-ms 0 :type fixnum)
  (total-ms 0 :type fixnum))

(defparameter *parallel-timing* nil
  "Timing data from most recent parallel search.")


(defun format-timing (ms)
  "Format milliseconds as appropriate time unit."
  (cond ((< ms 1000) (format nil "~D ms" ms))
        ((< ms 60000) (format nil "~,2F sec" (/ ms 1000.0)))
        (t (format nil "~,2F min" (/ ms 60000.0)))))


(defun display-parallel-timing ()
  "Display timing breakdown from most recent parallel search."
  (unless *parallel-timing*
    (format t "~%No parallel timing data available.~%")
    (return-from display-parallel-timing))
  (format t "~%Parallel Search Timing Breakdown:~%")
  (format t "  Task generation: ~A~%" 
          (format-timing (pt-task-generation-ms *parallel-timing*)))
  (format t "  Worker search:   ~A~%" 
          (format-timing (pt-worker-search-ms *parallel-timing*)))
  (format t "  Finalization:    ~A~%" 
          (format-timing (pt-finalization-ms *parallel-timing*)))
  (format t "  Total:           ~A~%" 
          (format-timing (pt-total-ms *parallel-timing*)))
  (let ((search-ms (pt-worker-search-ms *parallel-timing*)))
    (when (> search-ms 0)
      (format t "~%  Search efficiency: ~,1F%~%"
              (* 100.0 (/ search-ms (pt-total-ms *parallel-timing*)))))))


(defun display-worker-stats ()
  "Display per-worker statistics from most recent parallel search."
  (unless *worker-stats-vector*
    (format t "~%No worker statistics available.~%")
    (return-from display-worker-stats))
  (format t "~%Per-Worker Statistics:~%")
  (format t "~%  Worker  States     Cycles    Max-Depth  Solutions  Donated~%")
  (format t "  ------  ---------  ---------  ---------  ---------  --------~%")
  (loop for i from 0 below (length *worker-stats-vector*)
        for stats = (svref *worker-stats-vector* i)
        do (format t "  ~6D  ~9:D  ~9:D  ~9D  ~9D  ~8:D~%"
                   i
                   (ws-states-processed stats)
                   (ws-program-cycles stats)
                   (ws-max-depth stats)
                   (ws-solutions-found stats)
                   (ws-nodes-donated stats)))
  (terpri))


;;; ============================================================
;;; CONFIGURATION PRESETS
;;; ============================================================

(defun apply-parallel-preset (preset)
  "Apply a named configuration preset for common scenarios.
   PRESET can be: :default, :aggressive, :conservative, :debug"
  (ecase preset
    (:default
     (setf *split-depth-max* 4
           *tasks-per-thread* 8
           *min-tasks* 256
           *num-closed-shards* 64
           *bound-refresh-interval* 1000
           *enable-work-donation* t
           *donation-check-interval* 500
           *donation-threshold* 32
           *donation-fraction* 0.5)
     (format t "~%Applied :default preset~%"))
    
    (:aggressive
     ;; For problems with lots of independent work
     (setf *split-depth-max* 6
           *tasks-per-thread* 16
           *min-tasks* 512
           *num-closed-shards* 128
           *bound-refresh-interval* 500
           *enable-work-donation* t
           *donation-check-interval* 250
           *donation-threshold* 16
           *donation-fraction* 0.4)
     (format t "~%Applied :aggressive preset (more parallelism)~%"))
    
    (:conservative
     ;; For problems where overhead matters
     (setf *split-depth-max* 3
           *tasks-per-thread* 4
           *min-tasks* 128
           *num-closed-shards* 32
           *bound-refresh-interval* 2000
           *enable-work-donation* t
           *donation-check-interval* 1000
           *donation-threshold* 64
           *donation-fraction* 0.5)
     (format t "~%Applied :conservative preset (less overhead)~%"))
    
    (:debug
     ;; For debugging parallel behavior
     (setf *split-depth-max* 2
           *tasks-per-thread* 2
           *min-tasks* 16
           *num-closed-shards* 16
           *bound-refresh-interval* 100
           *enable-work-donation* t
           *donation-check-interval* 100
           *donation-threshold* 8
           *donation-fraction* 0.5
           *progress-interval-seconds* 2)
     (format t "~%Applied :debug preset (verbose, small scale)~%")))
  (display-parallel-parameters))
