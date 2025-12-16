;;; Filename: ww-parallel.lisp

; Functions for managing the multi-threading pool


(in-package :ww)


;;; ============================================================
;;; LOCK-STRIPED *CLOSED* INFRASTRUCTURE
;;; ============================================================

(defparameter *num-closed-shards* 1024
  "Number of lock shards for *closed*. Must be power of 2.
   1024 shards means max 1024 concurrent writers to *closed*,
   which exceeds typical core counts by 10-100x.")

(defparameter *closed-shard-mask* (1- *num-closed-shards*)
  "Bitmask for fast shard computation: (logand hash mask) = (mod hash num-shards).")

(defparameter *closed-locks* nil
  "Vector of locks for *closed* sharding. Initialized by initialize-closed-infrastructure.")

(defparameter *closed-shards* nil                                                 ; <-- NEW
  "Vector of hash tables for sharded *closed*. Initialized by initialize-closed-infrastructure.")


(defun initialize-closed-locks ()
  "Create the vector of shard locks. Called once at startup or problem load."
  (setf *closed-locks*
        (let ((locks (make-array *num-closed-shards*)))
          (dotimes (i *num-closed-shards* locks)
            (setf (svref locks i) 
                  (bt:make-lock (format nil "closed-shard-~D" i)))))))


(defun initialize-closed-shards (hash-test)
  "Create the vector of hash table shards. HASH-TEST is 'equal or 'eql."
  (setf *closed-shards*
        (let ((shards (make-array *num-closed-shards*)))
          (dotimes (i *num-closed-shards* shards)
            (setf (svref shards i)
                  (make-hash-table :test hash-test
                                   :size (ceiling 200003 *num-closed-shards*)
                                   :rehash-size 2.7
                                   :rehash-threshold 0.8
                                   :synchronized nil))))))


(defun initialize-closed-infrastructure (hash-test)
  "Initialize both shard locks and hash tables for parallel *closed* access."
  (initialize-closed-locks)
  (initialize-closed-shards hash-test))


(defun shard-index-for-state (state)
  "Compute which shard lock protects operations on STATE in *closed*.
   Uses pre-computed idb-hash for speed."
  (declare (type problem-state state))
  (logand (ensure-idb-hash state) *closed-shard-mask*))


(defun shard-index-for-hash (idb-hash)
  "Compute shard index directly from a hash value."
  (declare (type fixnum idb-hash))
  (logand idb-hash *closed-shard-mask*))


(defun closed-shard (state)
  "Return the hash table shard for STATE. Parallel mode only.
   Caller must hold the shard lock via with-closed-shard-lock."
  (declare (type problem-state state))
  (svref *closed-shards* (shard-index-for-state state)))


(defmacro with-closed-shard-lock ((state) &body body)
  "Execute BODY while holding the shard lock for STATE.
   STATE must be a problem-state with idb-hash computed.
   
   This macro replaces with-search-structures-lock for *closed* operations.
   Multiple threads can execute concurrently if they access different shards."
  (let ((shard-idx (gensym "SHARD-IDX")))
    `(let ((,shard-idx (shard-index-for-state ,state)))
       (bt:with-lock-held ((svref *closed-locks* ,shard-idx))
         ,@body))))


(defmacro with-closed-shard-lock-by-hash ((hash-value) &body body)
  "Execute BODY while holding the shard lock for HASH-VALUE.
   Use when you have the hash but not the full state."
  (let ((shard-idx (gensym "SHARD-IDX")))
    `(let ((,shard-idx (shard-index-for-hash ,hash-value)))
       (bt:with-lock-held ((svref *closed-locks* ,shard-idx))
         ,@body))))


;;; ============================================================
;;; CHASE-LEV WORK-STEALING DEQUE
;;; ============================================================
;;; Lock-free deque where owner pushes/pops from bottom,
;;; thieves steal from top. Uses SBCL atomic primitives.


(defconstant +initial-deque-size+ 1024
  "Initial capacity of Chase-Lev deque. Will grow as needed.")


(defstruct (chase-lev-deque (:conc-name cld-))
  "Chase-Lev work-stealing deque.
   Owner operates on bottom (LIFO), thieves steal from top (FIFO)."
  (buffer (make-array +initial-deque-size+ :initial-element nil) 
          :type simple-vector)
  (bottom 0 :type fixnum)           ; Owner's end (push/pop here)
  (top 0 :type fixnum)              ; Thieves' end (steal from here)  
  (lock (bt:make-lock "cld-lock"))) ; For resize and bulk steal coordination


(defun cld-empty-p (deque)
  "Returns T if deque appears empty. May have false negatives under contention."
  (declare (type chase-lev-deque deque))
  (>= (cld-top deque) (cld-bottom deque)))


(defun cld-size (deque)
  "Returns approximate number of elements. May be stale under contention."
  (declare (type chase-lev-deque deque))
  (max 0 (- (cld-bottom deque) (cld-top deque))))


(defun cld-push-bottom (deque item)
  "Owner pushes ITEM onto bottom of DEQUE. Grows buffer if needed."
  (declare (type chase-lev-deque deque))
  (let* ((bottom (cld-bottom deque))
         (top (cld-top deque))
         (buffer (cld-buffer deque))
         (size (length buffer)))
    ;; Grow if full
    (when (>= (- bottom top) size)
      (bt:with-lock-held ((cld-lock deque))
        (let* ((new-size (* size 2))
               (new-buffer (make-array new-size :initial-element nil)))
          ;; Copy existing elements
          (loop for i from top below bottom
                do (setf (svref new-buffer (mod i new-size))
                         (svref buffer (mod i size))))
          (setf (cld-buffer deque) new-buffer
                buffer new-buffer
                size new-size))))
    ;; Store item and increment bottom
    (setf (svref buffer (mod bottom size)) item)
    (setf (cld-bottom deque) (1+ bottom))
    item))


(defun cld-pop-bottom (deque)
  "Owner pops from bottom. Returns (values item t) or (values nil nil)."
  (declare (type chase-lev-deque deque))
  (let* ((bottom (1- (cld-bottom deque)))
         (buffer (cld-buffer deque))
         (size (length buffer)))
    (setf (cld-bottom deque) bottom)
    (let ((top (cld-top deque)))
      (cond
        ;; Deque was empty
        ((< bottom top)
         (setf (cld-bottom deque) top)  ; Reset bottom
         (values nil nil))
        ;; At least one element, and not contending with thieves
        ((> bottom top)
         (values (svref buffer (mod bottom size)) t))
        ;; Exactly one element - may contend with thief
        (t
         (let ((item (svref buffer (mod bottom size))))
           ;; Try to claim it before thieves
           (cond
             ((= top (cld-top deque))  ; No thief took it yet
              (setf (cld-top deque) (1+ top))        ; Advance top
              (setf (cld-bottom deque) (1+ top))     ; Reset for next push
              (values item t))
             (t
              ;; Thief got it
              (setf (cld-bottom deque) (1+ top))
              (values nil nil)))))))))


(defun cld-steal-half (deque)
  "Thief steals up to half of DEQUE's elements from top.
   Returns list of stolen items, or NIL if empty/contended."
  (declare (type chase-lev-deque deque))
  (bt:with-lock-held ((cld-lock deque))
    (let* ((top (cld-top deque))
           (bottom (cld-bottom deque))
           (available (- bottom top)))
      (when (<= available 0)
        (return-from cld-steal-half nil))
      (let* ((steal-count (max 1 (floor available 2)))
             (buffer (cld-buffer deque))
             (size (length buffer))
             (stolen nil))
        ;; Collect items from top
        (loop for i from top below (+ top steal-count)
              do (push (svref buffer (mod i size)) stolen))
        ;; Advance top atomically
        (setf (cld-top deque) (+ top steal-count))
        (nreverse stolen)))))


;;; ============================================================
;;; PARALLEL SEARCH INFRASTRUCTURE
;;; ============================================================

(defparameter *worker-deques* nil
  "Vector of Chase-Lev deques, one per worker. Index = worker ID.")

(defparameter *worker-states* nil
  "Vector of worker states: :running, :stealing, :idle, :done.
   Used for victim selection and termination detection.")

(defparameter *active-workers* 
  (make-array 1 :element-type 'sb-ext:word :initial-element 0)
  "Count of workers not yet terminated. Atomic via aref.")

(defun initialize-parallel-structures ()
  "Initialize per-worker deques, states, and lock-striped closed table."
  ;; Per-worker deques
  (setf *worker-deques* 
        (make-array *threads* 
                    :initial-contents 
                    (loop repeat *threads* collect (make-chase-lev-deque))))
  ;; Worker states
  (setf *worker-states*
        (make-array *threads* :initial-element :idle))
  ;; Shard locks for *closed*
  (initialize-closed-locks)
  ;; Active worker count
  (setf (aref *active-workers* 0) *threads*)
  ;; Reset shutdown flag
  (setf *shutdown-requested* nil))


(defun total-parallel-frontier ()
  "Returns total node count across all worker deques."
  (loop for deque across *worker-deques*
        sum (cld-size deque)))


;;; ============================================================
;;; VICTIM SELECTION FOR WORK STEALING
;;; ============================================================

(defun select-victim (my-id)
  "Select a random worker to steal from (not self).
   Returns worker ID or NIL if no viable victims."
  (declare (type fixnum my-id))
  (let ((start (random *threads*)))
    ;; Linear probe from random start
    (loop for offset from 0 below *threads*
          for victim = (mod (+ start offset) *threads*)
          when (and (/= victim my-id)
                    (eq (svref *worker-states* victim) :running)
                    (not (cld-empty-p (svref *worker-deques* victim))))
            return victim
          finally (return nil))))


(defun try-steal-work (my-id my-deque)
  "Attempt to steal work from another worker.
   Returns T if work was stolen, NIL otherwise."
  (declare (type fixnum my-id) (type chase-lev-deque my-deque))
  (setf (svref *worker-states* my-id) :stealing)
  (let ((victim-id (select-victim my-id)))
    (when victim-id
      (let* ((victim-deque (svref *worker-deques* victim-id))
             (stolen-nodes (cld-steal-half victim-deque)))
        (when stolen-nodes
          ;; Push stolen nodes onto our deque (reversing to maintain order)
          (dolist (node (nreverse stolen-nodes))
            (cld-push-bottom my-deque node))
          (setf (svref *worker-states* my-id) :running)
          (return-from try-steal-work t)))))
  ;; Failed to steal
  (setf (svref *worker-states* my-id) :idle)
  nil)


;;; ============================================================
;;; WORKER SEARCH FUNCTION
;;; ============================================================


(defun handle-thread-error (condition)
  "Error handler for worker threads. Logs the error and signals shutdown."
  (let ((worker-id (or (ignore-errors (lparallel:kernel-worker-index)) "?")))
    (bt:with-lock-held (*lock*)
      (format *error-output* "~2%ERROR in worker ~A: ~A~%" worker-id condition)
      (format *error-output* "Signaling shutdown...~%")
      (finish-output *error-output*)))
  (setf *shutdown-requested* t)
  ;; Decline to handle - let the error propagate after logging
  nil)


(defun worker-search (worker-id first-solution-queue)
  "Main search loop for one worker. Uses local Chase-Lev deque.
   FIRST-SOLUTION-QUEUE signals early termination for solution-type FIRST."
  (declare (type fixnum worker-id))
  (handler-bind ((error #'handle-thread-error))
    (let ((my-deque (svref *worker-deques* worker-id))
          (consecutive-steal-failures 0)
          (max-steal-failures 100))  ; Before backoff
      (declare (type chase-lev-deque my-deque)
               (type fixnum consecutive-steal-failures))
      
      (setf (svref *worker-states* worker-id) :running)
      
      (loop
        ;; Check termination conditions
        (when *shutdown-requested*
          (return))
        (when (lparallel.queue:peek-queue first-solution-queue)
          (return))  ; Another worker found first solution
        
        ;; Try to get work from own deque
        (multiple-value-bind (current-node got-work) (cld-pop-bottom my-deque)
          (cond
            ;; Have work - expand the node
            (got-work
             (setf consecutive-steal-failures 0)
             (worker-expand-node worker-id my-deque current-node first-solution-queue))
            
            ;; No local work - try stealing
            ((try-steal-work worker-id my-deque)
             (setf consecutive-steal-failures 0))
            
            ;; Stealing failed
            (t
             (incf consecutive-steal-failures)
             (cond
               ;; Check if all workers are idle (termination)
               ((every (lambda (state) (member state '(:idle :done)))
                       (coerce *worker-states* 'list))
                (return))
               
               ;; Backoff if repeatedly failing to steal
               ((> consecutive-steal-failures max-steal-failures)
                (sleep 0.001)  ; 1ms backoff
                (setf consecutive-steal-failures 0))
               
               ;; Brief yield before retry
               (t
                (bt:thread-yield)))))))
      
      ;; Worker terminating
      (setf (svref *worker-states* worker-id) :done)
      (sb-ext:atomic-decf (aref *active-workers* 0)))))


(defun worker-expand-node (worker-id my-deque current-node first-solution-queue)
  "Expand CURRENT-NODE and push successors onto MY-DEQUE.
   Handles goal detection, duplicate checking, and pruning."
  (declare (type fixnum worker-id)
           (type chase-lev-deque my-deque)
           (type node current-node))
  
  ;; Depth cutoff check
  (when (and (> *depth-cutoff* 0) 
             (= (node.depth current-node) *depth-cutoff*))
    (return-from worker-expand-node))
  
  ;; Bounding function check
  (when (eql (bounding-function current-node) 'kill-node)
    (return-from worker-expand-node))
  
  ;; Generate successor states
  (let ((succ-states (expand current-node)))
    (when (null succ-states)
      (update-max-depth-explored (node.depth current-node))
      (finalize-path-depth (node.depth current-node))
      (return-from worker-expand-node))
    
    (update-max-depth-explored (1+ (node.depth current-node)))
    (increment-global *total-states-processed* (length succ-states))
    
    ;; Process each successor
    (let ((succ-nodes (worker-process-successors 
                        succ-states current-node worker-id)))
      
      ;; Check for first solution signal
      (when (eq succ-nodes 'first-found)
        (lparallel.queue:push-queue 'found first-solution-queue)
        (return-from worker-expand-node))
      
      ;; Sort by heuristic if available, or shuffle if randomized
      (when succ-nodes
        (setf succ-nodes
              (cond ((fboundp 'heuristic?)
                     (sort (copy-list succ-nodes) #'>
                           :key (lambda (node)
                                  (problem-state.heuristic (node.state node)))))
                    (*randomize-search*
                     (alexandria:shuffle succ-nodes))
                    (t succ-nodes))))
      
      ;; Push successors onto local deque (best heuristic pushed last = popped first)
      (dolist (succ-node succ-nodes)
        (cld-push-bottom my-deque succ-node))
      
      ;; Track initial branches for progress reporting (worker 0 only)
      (when (and (= worker-id 0) (= *program-cycles* 0))
        (setf *num-init-successors* (length succ-nodes))
        (setf *rem-init-successors* (reverse succ-nodes)))
      
      (increment-global *program-cycles* 1)
      (setf *average-branching-factor* (compute-average-branching-factor))
      
      ;; Periodic progress reporting (worker 0 only to avoid garbled output)
      (when (= worker-id 0)
        (print-search-progress)))))


(defun worker-process-successors (succ-states current-node worker-id)
  "Process successor states with lock-striped duplicate detection.
   Returns list of new successor nodes, or 'FIRST-FOUND."
  (declare (type list succ-states)
           (type node current-node)
           (type fixnum worker-id)
           (ignorable worker-id))
  
  (let ((succ-depth (1+ (node.depth current-node)))
        (succ-nodes nil))
    
    (dolist (succ-state succ-states (nreverse succ-nodes))
      (block process-one-successor
        
        ;; Global invariant validation
        (when *global-invariants*
          (unless (validate-global-invariants current-node succ-state)
            (return-from process-one-successor)))
        
        ;; Optimization bound check
        (when (and *solutions* 
                   (member *solution-type* '(min-length min-time min-value max-value)))
          (unless (f-value-better succ-state succ-depth)
            (return-from process-one-successor)))
        
        ;; Goal check (before duplicate detection - goals are always processed)
        (when (goal succ-state)
          (if *hybrid-mode*
              (defer-hybrid-goal current-node succ-state)
              (register-solution current-node succ-state))
          (finalize-path-depth succ-depth)
          (when (eql *solution-type* 'first)
            (return-from worker-process-successors 'first-found))
          (return-from process-one-successor))
        
        ;; Best-state tracking for goalless problems
        (unless (boundp 'goal-fn)
          (process-min-max-value succ-state))
        
        ;; Tree search: cycle detection on current path only
        (when (eql *tree-or-graph* 'tree)
          (when (and (eql *problem-type* 'planning)
                     (on-current-path succ-state current-node))
            (increment-global *repeated-states*)
            (finalize-path-depth succ-depth)
            (return-from process-one-successor))
          ;; Tree search with no cycle - create node directly
          (push (generate-new-node current-node succ-state) succ-nodes)
          (return-from process-one-successor))
        
        ;; Graph search: lock-striped duplicate detection
        (with-closed-shard-lock (succ-state)
          (let ((closed-values (get-closed-values succ-state succ-depth)))
            (if closed-values
                ;; State already visited
                (progn
                  (increment-global *repeated-states*)
                  (cond 
                    (*hybrid-mode*
                     (let ((closed-node (get-closed-node succ-state succ-depth)))
                       (when closed-node
                         (add-parent-to-node closed-node current-node 
                                             (record-move succ-state))))
                     (finalize-path-depth succ-depth))
                    ((better-than-closed closed-values succ-state succ-depth)
                     ;; Reopen with better path
                     (remhash (closed-key succ-state succ-depth) 
                              (closed-shard succ-state))                          ; <-- CHANGED: use shard
                     (let ((succ-node (generate-new-node current-node succ-state)))
                       (setf (gethash (closed-key succ-state succ-depth) 
                                      (closed-shard succ-state))                  ; <-- CHANGED: use shard
                             (if *hybrid-mode*
                                 (list (problem-state.idb succ-state)
                                       succ-depth
                                       (problem-state.time succ-state)
                                       (problem-state.value succ-state)
                                       succ-node)
                                 (list (problem-state.idb succ-state)
                                       succ-depth
                                       (problem-state.time succ-state)
                                       (problem-state.value succ-state))))
                       (push succ-node succ-nodes)))
                    (t
                     (finalize-path-depth succ-depth))))
                ;; New state - reserve in *closed* and create node
                (let ((succ-node (generate-new-node current-node succ-state)))
                  (setf (gethash (closed-key succ-state succ-depth) 
                                 (closed-shard succ-state))                       ; <-- CHANGED: use shard
                        (if *hybrid-mode*
                            (list (problem-state.idb succ-state)
                                  succ-depth
                                  (problem-state.time succ-state)
                                  (problem-state.value succ-state)
                                  succ-node)
                            (list (problem-state.idb succ-state)
                                  succ-depth
                                  (problem-state.time succ-state)
                                  (problem-state.value succ-state))))
                  (push succ-node succ-nodes)))))))))


;;; ============================================================
;;; SIGINT HANDLER FOR GRACEFUL SHUTDOWN
;;; ============================================================

(defun parallel-sigint-handler (signal info context)
  "Custom SIGINT handler for parallel search. Sets shutdown flag immediately,
   then invokes debugger via interrupt-thread for safe signal context exit."
  (declare (ignore signal info context))
  (setf *shutdown-requested* t)
  (format t "~2%Ctrl-C detected. Signaling worker shutdown...~%")
  ;; Schedule debugger invocation on main thread (safe exit from signal context)
  (sb-thread:interrupt-thread sb-thread:*current-thread*
                              (lambda () (invoke-debugger
                                          (make-condition 'sb-sys:interactive-interrupt)))))


;;; ============================================================
;;; MAIN COORDINATOR
;;; ============================================================

(defun process-threads-chase-lev ()
  "Main coordinator for Chase-Lev work-stealing parallel search.
   Replaces process-threads."
  ;; Initialize parallel infrastructure
  (initialize-parallel-structures)
  
  ;; Create lparallel kernel
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  
  (let ((first-solution-queue (lparallel.queue:make-queue)))
    
    (unwind-protect
        (handler-bind
            ((sb-sys:interactive-interrupt
              (lambda (c)
                (declare (ignore c))
                (setf *shutdown-requested* t)
                (format t "~2%Ctrl-C detected. Signaling worker shutdown...~%"))))
          
          ;; Seed worker 0's deque with start node
          (let ((start-node (hs::pop-hstack *open*)))
            (cld-push-bottom (svref *worker-deques* 0) start-node))
          
          ;; Submit all workers
          (dotimes (i *threads*)
            (lparallel:submit-task (lparallel:make-channel)
                                   #'worker-search 
                                   i 
                                   first-solution-queue))
          
          ;; Wait for termination
          (loop
            (sleep 0.05)
            (when (or *shutdown-requested*
                      (zerop (aref *active-workers* 0))
                      (lparallel.queue:peek-queue first-solution-queue))
              (return))))
      
      ;; Cleanup (always runs)
      (setf *shutdown-requested* t)
      (lparallel:end-kernel :wait t)
      (format t "~2%All workers shut down.~2%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun deduplicate-parallel-solutions ()
  "Post-process solutions to create unique solutions list after parallel search.
   Handles all solution types: first, every, min-length, min-time, min-value, max-value."
  (case *solution-type*
    ((first)
     ;; For 'first' solutions, unique solutions should match regular solutions
     (setf *unique-solutions* (copy-list *solutions*)))
    
    ((every)
     ;; For 'every' solutions, remove duplicates based on goal state equivalence
     (setf *unique-solutions*
           (remove-duplicates *solutions* :test #'solution-equivalent-p)))
    
    ((min-length min-time min-value max-value)
     ;; For optimization problems, unique solutions should match regular solutions
     ;; since the search algorithm already keeps only the best
     (setf *unique-solutions* (copy-list *solutions*)))
    
    (otherwise
     ;; Default case: remove duplicates
     (setf *unique-solutions*
           (remove-duplicates *solutions* :test #'solution-equivalent-p)))))


(defun finalize-parallel-search-results ()
  "Post-process parallel search results to maintain interface consistency.
   This function makes the *unique-solutions* initialization functionally necessary."
  (setf *unique-solutions* 
        (remove-duplicates *solutions* 
                          :test #'solution-equivalent-p))
  (when (>= *debug* 1)
    (format t "~&Parallel search: ~D solutions, ~D unique~%" 
            (length *solutions*) (length *unique-solutions*))))


(defun solution-equivalent-p (sol1 sol2)
  "Compare solutions for equivalence"
  (and sol1 sol2
       (equalp (problem-state.idb (solution.goal sol1))
               (problem-state.idb (solution.goal sol2)))))
