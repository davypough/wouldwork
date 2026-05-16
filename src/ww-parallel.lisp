;;; Filename: ww-parallel.lisp

;;; Partitioned parallel search implementation:
;;; - Root-partitioned task generation
;;; - Worker DFS with local stacks
;;; - Coordinator for parallel search orchestration

(in-package :ww)


;;; ============================================================
;;; TASK GENERATION (Serial Root Partitioning)
;;; ============================================================
;;; Expand from start state until the task-count target is met or the
;;; frontier is exhausted. *split-depth-max* is a safety cap only.
;;; This runs serially before workers start.

(defun compute-target-tasks ()
  "Compute target number of tasks based on thread count."
  (max *min-tasks* (* *tasks-per-thread* *threads*)))


(defun generate-root-tasks (start-node)
  "Generate initial task pool by expanding from START-NODE.
   Continues until the task-count target is reached, the frontier is
   exhausted, or *split-depth-max* (safety cap) is hit.
   Returns list of nodes (tasks) at the split frontier.
   
   For graph search: tracks visited states in *closed* shards during generation."
  (declare (type node start-node))
  (let ((target (compute-target-tasks))
        (frontier (list start-node))  ; Nodes to potentially expand further
        (tasks nil)                    ; Completed tasks (won't expand further)
        (current-depth 0))
    
    (format t "~&Generating tasks: target=~D, safety-cap-depth=~D~%"
            target *split-depth-max*)
    
    (loop
      ;; Termination: task-count target met, frontier exhausted, or safety cap hit  ; CHANGED
      (when (or (>= (+ (length tasks) (length frontier)) target)
                (null frontier)
                (>= current-depth *split-depth-max*))  ; CHANGED: safety cap last
        ;; All remaining frontier nodes become tasks
        (setf tasks (nconc tasks frontier))
        (format t "~&Generated ~D tasks at depths 0-~D~%" (length tasks) current-depth)
        (return tasks))
      
      ;; Expand one level: each frontier node produces successors
      (let ((next-frontier nil)
            (states-this-level 0))
        (dolist (node frontier)
          (block expand-node
            ;; Check depth cutoff
            (when (and (> *depth-cutoff* 0) 
                       (>= (node.depth node) *depth-cutoff*))
              (push node tasks)  ; Can't expand further, becomes a task
              (return-from expand-node))
            
            ;; Check bounding function
            (when (eql (bounding-function node) 'kill-node)
              (return-from expand-node))  ; Pruned, not a task
            
            ;; Expand node
            (let ((succ-states (expand node)))
              (incf states-this-level (length succ-states))
              
              (when (null succ-states)
                ;; No successors: terminal node, not a useful task
                (return-from expand-node))
              
              ;; Process each successor
              (dolist (succ-state succ-states)
                (block process-succ
                  (when (state-is-inconsistent succ-state)
                    (incf *inconsistent-states-dropped*)
                    (return-from process-succ))
                  ;; Goal check during task generation
                  (when (goal succ-state)
                    ;; Register solution immediately (serial, no locking needed)
                    (register-solution node succ-state)
                    (when (solution-count-reached-p)  ; CHANGED: was (eql *solution-type* 'first)
                    ;; Early termination: found requested number of solutions during task gen
                    (format t "~&Solution found during task generation!~%")
                    (return-from generate-root-tasks nil))
                    (return-from process-succ))
                  
                  ;; Tree search: check for cycle on current path
                  (when (eql *tree-or-graph* 'tree)
                    (when (and (eql *problem-type* 'planning)
                               (on-current-path succ-state node))
                      (return-from process-succ)))
                  
                  ;; Graph search: check/update closed (serial, no lock needed)
                  (when (eql *tree-or-graph* 'graph)
                    (ensure-idb-hash succ-state)
                    (let* ((succ-depth (1+ (node.depth node)))
                           (shard (closed-shard succ-state))
                           (closed-entry (closed-bucket-find succ-state succ-depth shard)))   ; CHANGED: bucket lookup with verification
                      (cond
                        ;; Already visited - check if better
                        (closed-entry
                         (incf *repeated-states*)
                         (unless (better-than-closed closed-entry succ-state succ-depth)
                           (return-from process-succ))
                         ;; Better path - remove old entry
                         (closed-bucket-remove succ-state succ-depth shard))                  ; CHANGED: was (remhash key shard)
                        ;; New state - will insert below
                        (t nil))
                      ;; Insert/update in closed
                      (closed-bucket-insert (make-closed-entry succ-state succ-depth)         ; CHANGED: was inline 4-element list
                                            succ-state succ-depth shard)))                    ; CHANGED
                  
                  ;; Create successor node for next frontier
                  (let ((succ-node (make-node 
                                    :state succ-state
                                    :depth (1+ (node.depth node))
                                    :parent (if *hybrid-mode*                            ; CHANGED: hybrid format vs plain node
                                                (list (cons node (record-move succ-state)))
                                                node))))
                    (push succ-node next-frontier)))))))
        
        ;; Update globals for states processed during task generation
        (incf *total-states-processed* states-this-level)
        
        ;; Move to next level
        (setf frontier (nreverse next-frontier))
        (incf current-depth)
        
        ;; Apply heuristic sorting or randomization if configured
        (when frontier
          (setf frontier
                (cond ((fboundp 'heuristic?)
                       (sort frontier #'>
                             :key (lambda (n) 
                                    (problem-state.heuristic (node.state n)))))
                      (*randomize-search*
                       (alexandria:shuffle frontier))
                      (t frontier))))))))


;;; ============================================================
;;; WORKER LOCAL DFS
;;; ============================================================
;;; Each worker performs complete DFS from its assigned task node.
;;; Uses local stack (no shared open list).

(defun worker-local-dfs (task-node worker-id stats task-queue)
  "Perform complete DFS subtree search starting from TASK-NODE.
   Uses local stack. Updates STATS (worker-local, no locking).
   May donate work back to TASK-QUEUE when other workers are starving.
   Returns when subtree exhausted or termination signaled."
  (declare (type node task-node)
           (type fixnum worker-id)
           (type worker-stats stats)
           (type task-queue task-queue))

  (let ((local-stack (list task-node))   ; Local open list
        (local-bound *best-bound*)       ; Cached bound for pruning
        (cycles-since-refresh 0)         ; Track cycles for bound refresh
        (cycles-since-donation-check 0)) ; Track cycles for donation check
    (declare (type fixnum cycles-since-refresh cycles-since-donation-check))

    (loop
      ;; Check termination conditions
      (when (or *shutdown-requested*
                *first-solution-found*
                (null local-stack))
        (return))

      ;; Periodic maintenance: bound refresh, progress, donation
      (incf cycles-since-refresh)
      (incf cycles-since-donation-check)

      ;; Refresh bound cache
      (when (>= cycles-since-refresh *bound-refresh-interval*)
        (setf local-bound *best-bound*)
        (setf cycles-since-refresh 0)
        (maybe-report-parallel-progress worker-id))

      ;; Check for work donation opportunity
      (when (>= cycles-since-donation-check *donation-check-interval*)
        (setf cycles-since-donation-check 0)
        (setf local-stack
              (maybe-donate-work local-stack task-queue worker-id stats))
        ;; Re-check if stack is now empty after donation
        (when (null local-stack)
          (return)))

      ;; Everything below is “one iteration”; RETURN-FROM skips to next iteration.
      (block :next-iteration
        ;; Pop next node
        (let ((current-node (pop local-stack)))
          (declare (type node current-node))

          ;; Depth cutoff check
          (when (and (> *depth-cutoff* 0)
                     (>= (node.depth current-node) *depth-cutoff*))
            (ws-finalize-path stats (node.depth current-node))
            (return-from :next-iteration nil))

          ;; Bounding function check (user-defined)
          (when (eql (bounding-function current-node) 'kill-node)
            (return-from :next-iteration nil))

          ;; Branch-and-bound pruning using cached bound
          ;; Applies to min-length, min-time, min-value, max-value
          (unless (node-can-improve-bound-p current-node local-bound)
            (ws-finalize-path stats (node.depth current-node))
            (return-from :next-iteration nil))

          ;; Expand node
          (let ((succ-states (expand current-node)))
            (ws-inc-cycles stats)

            (when (null succ-states)
              (ws-update-max-depth stats (node.depth current-node))
              (ws-finalize-path stats (node.depth current-node))
              (return-from :next-iteration nil))

            (ws-update-max-depth stats (1+ (node.depth current-node)))
            (ws-inc-states stats (length succ-states))

            ;; Process successors
            (let ((succ-nodes (worker-process-successors-phase1
                               succ-states current-node worker-id stats)))

              ;; Check for first-solution signal
              (when (eq succ-nodes 'first-found)
                (return))

              ;; Sort successors by heuristic or shuffle
              (when succ-nodes
                (setf succ-nodes
                      (cond ((fboundp 'heuristic?)
                             (sort succ-nodes #'>
                                   :key (lambda (n)
                                          (problem-state.heuristic
                                           (node.state n)))))
                            (*randomize-search*
                             (alexandria:shuffle succ-nodes))
                            (t succ-nodes)))

                ;; Push onto local stack (best-first will be popped first)
                (dolist (succ-node succ-nodes)
                  (push succ-node local-stack))))))))))


(defun worker-process-successors-phase1 (succ-states current-node worker-id stats)
  "Process successor states with full graph search support.
   Returns list of new successor nodes, or 'FIRST-FOUND.
   
   For tree search: cycle detection on current path only.
   For graph search: sharded closed table with atomic check+update."
  (declare (type list succ-states)
           (type node current-node)
           (type fixnum worker-id)
           (type worker-stats stats))
  
  (let ((succ-depth (1+ (node.depth current-node)))
        (succ-nodes nil))
    
    (dolist (succ-state succ-states)
      (block process-one
        (when (state-is-inconsistent succ-state)
          (ws-finalize-path stats succ-depth)
          (increment-global *inconsistent-states-dropped* 1)
          (return-from process-one))
        ;; Global invariant validation
        (when *global-invariants*
          (unless (validate-global-invariants current-node succ-state)
            (return-from process-one)))
        
        ;; Optimization bound check
        (when (and *solution-paths*
                   (member *solution-type* '(min-length min-time min-value max-value)))
          (unless (f-value-better succ-state succ-depth)
            (return-from process-one)))
        
        ;; Goal check (before duplicate detection - goals always processed)
        (when (goal succ-state)
          (if *hybrid-mode*                                                ; CHANGED: hybrid defers for path enumeration
              (defer-hybrid-goal current-node succ-state)                 ; CHANGED
              (register-parallel-solution current-node succ-state worker-id))
          (ws-finalize-path stats succ-depth)
          (when (solution-count-reached-p)  ; CHANGED: was (eql *solution-type* 'first)
            (return-from worker-process-successors-phase1 'first-found))
          (return-from process-one))
        
        ;; Best-state tracking for goalless problems
        (unless (boundp 'goal-fn)
          (process-min-max-value succ-state))
        
        ;; === TREE SEARCH ===
        (when (eql *tree-or-graph* 'tree)
          (when (and (eql *problem-type* 'planning)
                     (on-current-path succ-state current-node))
            (ws-inc-repeated stats)
            (ws-finalize-path stats succ-depth)
            (return-from process-one))
          ;; Tree search - create node
          (push (make-node :state succ-state
                           :depth succ-depth
                           :parent current-node)
                succ-nodes)
          (return-from process-one))
        
        ;; === GRAPH SEARCH with Sharded Closed ===
        (when (eql *tree-or-graph* 'graph)
          ;; Ensure hash is computed for shard selection
          (ensure-idb-hash succ-state)
          
          ;; Atomic check+update under shard lock
          (with-closed-shard-lock (succ-state)
            (let* ((shard (closed-shard succ-state))
                   (closed-entry (closed-bucket-find succ-state succ-depth shard)))   ; CHANGED: bucket lookup with verification
              
              (cond
                ;; State already in closed
                (closed-entry
                 (ws-inc-repeated stats)
                 (cond
                   ;; Hybrid mode: accumulate parent pointer
                   (*hybrid-mode*
                    (let ((closed-node (sixth closed-entry)))                          ; CHANGED: was (fifth closed-values) — node lives at position 6 in hybrid entries
                      (when closed-node
                        (add-parent-to-node closed-node current-node
                                            (record-move succ-state))))
                    (ws-finalize-path stats succ-depth))
                   
                   ;; Check if new path is better - reopen if so
                   ((better-than-closed closed-entry succ-state succ-depth)
                    ;; Remove old entry and insert new one
                    (closed-bucket-remove succ-state succ-depth shard)                 ; CHANGED: was (remhash key shard)
                    (let ((succ-node (make-node :state succ-state
                                                :depth succ-depth
                                                :parent (if *hybrid-mode*              ; CHANGED: hybrid format vs plain node
                                                            (list (cons current-node (record-move succ-state)))
                                                            current-node))))
                      (closed-bucket-insert (make-closed-entry succ-state succ-depth succ-node)   ; CHANGED: was inline list
                                            succ-state succ-depth shard)               ; CHANGED
                      (push succ-node succ-nodes)))
                   
                   ;; Not better - drop this successor
                   (t
                    (ws-finalize-path stats succ-depth))))
                
                ;; State not in closed - insert and create node
                (t
                 (let ((succ-node (make-node :state succ-state
                                             :depth succ-depth
                                             :parent (if *hybrid-mode*                 ; CHANGED: hybrid format vs plain node
                                                         (list (cons current-node (record-move succ-state)))
                                                         current-node))))
                   (closed-bucket-insert (make-closed-entry succ-state succ-depth succ-node)     ; CHANGED: was inline list
                                         succ-state succ-depth shard)                  ; CHANGED
                   (push succ-node succ-nodes)))))))))
    
    (nreverse succ-nodes)))


;;; ============================================================
;;; PARALLEL WORKER
;;; ============================================================

(defun parallel-worker (worker-id task-queue)
  "Main worker loop: fetch tasks and perform local DFS.
   May also receive donated work from other workers.
   Returns when no more tasks available."
  (declare (type fixnum worker-id)
           (type task-queue task-queue))
  
  ;; Register as active worker
  (tq-register-worker task-queue)
  
  (let ((stats (get-worker-stats worker-id))
        (tasks-completed 0))
    
    (handler-bind 
        ((error (lambda (c)
                  (bt:with-lock-held (*lock*)
                    (format *error-output* 
                            "~&[Worker ~D] ERROR: ~A~%" worker-id c)
                    (finish-output *error-output*))
                  (setf *shutdown-requested* t)
                  ;; Decline to handle - propagate after logging
                  nil)))
      
      (loop
        ;; Check for shutdown
        (when (or *shutdown-requested* *first-solution-found*)
          (return))
        
        ;; Get next task (blocking)
        (let ((task-node (tq-pop-blocking task-queue)))
          (unless task-node
            ;; Queue exhausted and done
            (return))
          
          ;; Perform local DFS from this task (may donate work back)
          (worker-local-dfs task-node worker-id stats task-queue)
          (incf tasks-completed))))
    
    ;; Worker finished - report summary if debug level high enough
    (when (>= *debug* 1)
      (bt:with-lock-held (*lock*)
        (format t "~&[Worker ~D] Done: ~D tasks, ~:D states, ~:D cycles~%" 
                worker-id tasks-completed
                (ws-states-processed stats)
                (ws-program-cycles stats))
        (finish-output)))))


;;; ============================================================
;;; COORDINATOR: Partitioned Parallel Search
;;; ============================================================

(defun process-partitioned-parallel ()
  "Main entry point for partitioned parallel search.
   Generates tasks, spawns workers, waits for completion, aggregates results."
  
  ;; Initialize timing
  (setf *parallel-timing* (make-parallel-timing))
  (let ((phase-start (get-internal-real-time))
        (total-start (get-internal-real-time)))
    
    ;; Initialize infrastructure
    (reset-parallel-control-flags)
    (setf *parallel-search-active* t)
    (setf *last-progress-time* (get-internal-real-time))
    
    ;; Initialize worker stats
    (initialize-worker-stats *threads*)
    
    ;; Create task queue
    (let ((task-queue (make-new-task-queue)))
      
      ;; Display configuration
      (format t "~2%========================================~%")
      (format t "Partitioned Parallel Search~%")
      (format t "========================================~%")
      (format t "  Threads: ~D~%" *threads*)
      (format t "  Mode: ~A / ~A~%" *tree-or-graph* *solution-type*)
      (when (member *solution-type* '(min-length min-time min-value max-value))
        (format t "  Branch-and-bound: ENABLED~%"))
      (format t "  Work donation: ~A~%" (if *enable-work-donation* "ENABLED" "DISABLED"))
      (when (and (eql *tree-or-graph* 'graph) (> *threads* 0))
        (format t "  Closed shards: ~D~%" *num-closed-shards*))
      (format t "----------------------------------------~%")
      
      ;; === PHASE 1: Task Generation ===
      (setf phase-start (get-internal-real-time))
      (format t "~%Generating tasks (split-depth-max=~D)...~%" *split-depth-max*)
      
      (let* ((start-node (hs::pop-hstack *open*))
             (tasks (generate-root-tasks start-node)))
        
        ;; Record task generation time
        (setf (pt-task-generation-ms *parallel-timing*)
              (round (* 1000 (/ (- (get-internal-real-time) phase-start)
                                internal-time-units-per-second))))
        
        ;; Check if we found solution during task generation
        (when (or (null tasks) 
                  (and (eql *solution-type* 'first) *solution-paths*))
          (format t "~&Search completed during task generation~%")
          (setf (pt-total-ms *parallel-timing*)
                (round (* 1000 (/ (- (get-internal-real-time) total-start)
                                  internal-time-units-per-second))))
          (setf *parallel-search-active* nil)
          (return-from process-partitioned-parallel))
        
        ;; Load tasks into queue
        (format t "Loading ~D tasks into queue...~%" (length tasks))
        (tq-push-many task-queue tasks)
        
        ;; Signal that all tasks are loaded (but allow donation to add more)
        ;; Note: We set done-p but workers can still add via donation
        (tq-signal-done task-queue)
        
        ;; === PHASE 2: Worker Search ===
        (setf phase-start (get-internal-real-time))
        (format t "~%Starting ~D workers...~%" *threads*)
        
        ;; Spawn worker threads
        (let ((threads nil))
          (dotimes (i *threads*)
            (let ((worker-id i))  ; Capture for closure
              (push (bt:make-thread 
                     (lambda () 
                       (parallel-worker worker-id task-queue))
                     :name (format nil "ww-worker-~D" worker-id))
                    threads)))
          
          ;; Wait for all workers; on abnormal exit only, signal shutdown
          (let ((completed-normally nil))
            (unwind-protect
                (progn
                  (dolist (thread threads)
                    (bt:join-thread thread))
                  (setf completed-normally t))
              (unless completed-normally
                (setf *shutdown-requested* t)
                (sb-thread:condition-broadcast (tq-waitqueue task-queue))
                (dolist (thread threads)
                  (when (sb-thread:thread-alive-p thread)
                    (sb-thread:join-thread thread :default nil))))))
        
        ;; Record worker search time
        (setf (pt-worker-search-ms *parallel-timing*)
              (round (* 1000 (/ (- (get-internal-real-time) phase-start)
                                internal-time-units-per-second))))
        
        (format t "~&All ~D workers completed.~%" *threads*))
      
      ;; === PHASE 3: Finalization ===
      (setf phase-start (get-internal-real-time))
      
      ;; Aggregate statistics
      (aggregate-worker-stats)
      
      ;; Record finalization time
      (setf (pt-finalization-ms *parallel-timing*)
            (round (* 1000 (/ (- (get-internal-real-time) phase-start)
                              internal-time-units-per-second))))
      
      ;; Record total time
      (setf (pt-total-ms *parallel-timing*)
            (round (* 1000 (/ (- (get-internal-real-time) total-start)
                              internal-time-units-per-second))))
      
      ;; Cleanup
      (setf *parallel-search-active* nil)))))


;;; ============================================================
;;; FINALIZATION
;;; ============================================================

(defun finalize-parallel-search-results ()
  "Post-process parallel search results for interface consistency.
   Builds *unique-solution-states* in O(N) using a hash-table keyed on goal
   idb-hash, replacing the prior O(N^2) remove-duplicates scan.
   Buckets handle hash collisions via equalp fallback (vanishingly rare).
   When duplicate goal states are found, retains the better solution
   per solution-better-p."
  (let ((seen (make-hash-table :test #'eql :size (max 1024 (length *solution-paths*)))))
    (dolist (solution *solution-paths*)
      (let* ((goal-state (solution.goal solution))
             (goal-idb (problem-state.idb goal-state))
             (hash (ensure-idb-hash goal-state))
             (bucket (gethash hash seen))
             (match (find goal-idb bucket
                          :key (lambda (s) (problem-state.idb (solution.goal s)))
                          :test #'equalp)))
        (cond (match
               (when (solution-better-p solution match)
                 (setf (gethash hash seen)
                       (cons solution (delete match bucket :test #'eq)))))
              (t
               (setf (gethash hash seen) (cons solution bucket))))))
    (setf *unique-solution-states*
          (loop for bucket being the hash-values of seen
                append bucket)))
  (when (>= *debug* 1)
    (format t "~&Parallel search: ~D solutions, ~D unique~%"
            (length *solution-paths*) (length *unique-solution-states*))))


;;; ============================================================
;;; LEGACY COMPATIBILITY
;;; ============================================================
;;; These functions maintain compatibility with existing code that
;;; may reference old parallel infrastructure.

(defun total-parallel-frontier ()
  "Legacy compatibility: returns 0 since we don't track global frontier."
  0)

(defun initialize-closed-infrastructure (hash-test)
  "Legacy compatibility: delegates to infrastructure module."
  (initialize-closed-shards hash-test))

(defun initialize-closed-locks ()
  "Legacy compatibility: no-op, locks are in shards now."
  nil)
