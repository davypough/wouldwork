;;; Filename:  ww-searcher.lisp

;;; Nonstandard Depth First Branch & Bound. Optional duplicate checking for nodes
;;; previously visited (graph search) as specified in problem spec. Open nodes are
;;; kept in an indexed stack and (optionally for graph search) visited nodes in a
;;; CLOSED hash table. Search
;;; follows the scheme described on p.55 of Problem Solving Methods in Artificial
;;; Intelligence by Nillson.
;;; A complete path from a goal to the start state is then available in OPEN.
;;; In graph search a visited node is
;;; either in OPEN or CLOSED at any particular time. A user-defined heuristic can be
;;; used to expand the best states first at each level (beam search), but
;;; the entire search graph may ultimately be searched (completeness).


(in-package :ww)


(defmacro lprt (&rest vars)
  "Print some variable values in a thread for debugging."
  `(bt:with-lock-held (*lock*)  ;grab lock for uninterrupted printing
     (let ((*package* (find-package :ww))  ;suppress printing package prefixes
           (thread-index  (lparallel:kernel-worker-index)))  ;get current thread
       (terpri)
       (ut::prt thread-index)
       (ut::prt ,@vars)
       (finish-output))))  ;make sure printout is complete before continuing


(defun simple-break ()
  "Call to simplify debugger printout on a break."
  (declare (optimize (debug 1)))
  (break))


(defun probe (current-node name instantiations depth &optional (count 1))
  "Breaks when the current node matches action name, instantiations, depth, and count from start--eg, (put (a b) 1)."
  (declare (type node current-node))  ;(ut::prt name instantiations depth current-node) (break)
  (let ((state (node.state current-node)))
    (when (and (eql (problem-state.name state) name)
               (equal (problem-state.instantiations state) instantiations)
               (= (node.depth current-node) depth))
      (if (= count *counter*)
        (setq *debug* 6)  ;(setq *debug* 5)  ;means probe found
        (incf *counter*)))))


(defparameter *shutdown-requested* nil
  "Variable used to signal threads shutdown, if *threads* > 0")


(defparameter *open* (hs::make-hstack)  ;initialized in dfs
  "The hash-stack structure containing the stack of open nodes as a vector,
   and hash table of idb -> node.")
(declaim (hs::hstack *open*))


(define-global *closed* (make-hash-table :synchronized (> *threads* 0))  ;initialized in dfs
  "Contains the set of closed state idbs for graph search, idb -> (depth time value).")


(defun node.state.idb (node)
  "Gets the idb of a node."
  (problem-state.idb (node.state node)))


(defun node.state.idb-hash (node)
  "Gets the cached idb-hash of a node's state."
  (problem-state.idb-hash (node.state node)))


(defun ensure-idb-hash (state)
  "Ensures the idb-hash is computed and cached for the given state.
   When canonical symmetry is active (graph search with symmetry pruning),
   computes a permutation-invariant hash so symmetric states hash identically.
   Returns the state's idb-hash."
  (unless (problem-state.idb-hash state)
    (setf (problem-state.idb-hash state)
          (if (use-canonical-symmetry-p)
              (compute-canonical-idb-hash (problem-state.idb state))
              (compute-idb-hash (problem-state.idb state)))))
  (problem-state.idb-hash state))


(defun closed-key (state depth)
  "Generates the appropriate key for *closed* hash table lookup/storage.
   Standard mode: idb-hash (state identity only)
   Hybrid mode: (cons idb-hash depth) for (state, depth) pair identity."
  (declare (type problem-state state) (type fixnum depth))
  (ensure-idb-hash state)
  (let ((hash (problem-state.idb-hash state)))
    (if *hybrid-mode*
        (cons hash depth)
        hash)))


(defun choose-ht-value-test (relations)
  "Chooses either #'equal or #'equalp as a test for *closed* ht (idb) keys."
  (let (lisp-$types)  ;eg, $list, $hash-table, $fixnum, $real
    (maphash (lambda (rel args)
               (declare (ignore rel))
               (when (listp args)
                 (iter (for arg in args)
                       (when ($varp arg)
                         (let ((lisp-$type (trim-1st-char arg)))
                           (unless (gethash lisp-$type *types*)  ;user defined type
                             (pushnew lisp-$type lisp-$types)))))))
             relations)
    (cond ((intersection '(hash-table vector array) lisp-$types) #'equalp)
          (t #'equal))))


(defparameter *fixed-ht-values-fn* (choose-ht-value-test *relations*)
  "Determines which equality test to use in fixed-keys-ht-equal.")


(defun fixed-keys-ht-equal (ht-key1 ht-key2)
  "Quick equality test with *closed* for two hash tables with the same fixed keys.
   The equality predicate tests the hash table values, skipping the keys."
  (declare (type hash-table ht-key1 ht-key2))
  (maphash (lambda (k v)
             (unless (funcall *fixed-ht-values-fn* v (gethash k ht-key2))
               (return-from fixed-keys-ht-equal nil)))
           ht-key1)
  t)


(defun fixed-keys-ht-hash (ht)
  (let ((hash 0))
    (maphash (lambda (key val)
               (declare (ignore key))
               (setf hash (logxor hash (sxhash val))))
             ht)
    hash))


(sb-ext:define-hash-table-test fixed-keys-ht-equal fixed-keys-ht-hash)


(defun fixedp (relations)
  "Determines if all relations have $var args, and thus have fixed keys idb."
  (maphash (lambda (rel args)
             (declare (ignore rel))
             (unless (and (listp args)
                          (member-if #'$varp args))
               (return-from fixedp nil)))
           relations)
  t)


(defun solution-count-reached-p ()
  "Returns T if we should stop searching because the requested solution count is reached.
   Handles both *solution-type* = 'first and *solution-type* = <positive-fixnum>."
  (or (eql *solution-type* 'first)
      (and (typep *solution-type* 'fixnum)
           (>= (length *unique-solutions*) *solution-type*))))


(defun initialize-hybrid-mode ()
  "Checks constraints for hybrid graph search mode.
   Returns T if hybrid mode should activate, NIL otherwise.
   Prints diagnostic messages when *solution-type* is EVERY but constraints prevent hybrid mode."
  ;; Only consider hybrid mode when seeking all solutions
  (unless (eql *solution-type* 'every)
    (return-from initialize-hybrid-mode nil))
  ;; Check remaining constraints with diagnostics
  (let ((constraints-met t))
    (unless (eql *algorithm* 'depth-first)
      (setf constraints-met nil))
    (unless (eql *tree-or-graph* 'graph)
      (setf constraints-met nil))
    (unless (> *depth-cutoff* 0)
      (setf constraints-met nil))
    ;(unless constraints-met
    ;  (format t "~&Note: Hybrid mode not activated: requires *solution-type* = EVERY, *algorithm* = DEPTH-FIRST, *tree-or-graph* = ;             GRAPH, and *depth-cutoff* > 0. Continuing with non-hybrid search.~%"))
    (when constraints-met
      (format t "~&Hybrid graph search mode active: finding all goal states (not paths) at depth ~D.~%" *depth-cutoff*))
    constraints-met))


;;; Search Functions


(defun dfs ()
  "Main search program."
  (when *global-invariants*
    (unless (validate-global-invariants nil *start-state*)
      (format t "~%Invariant validation failed on initial state.~%")
      (return-from dfs nil)))
  (when (fboundp 'bounding-function?)
    (setf *upper-bound*
          (funcall (symbol-function 'bounding-function?) *start-state*)))
  (setf *hybrid-mode* (initialize-hybrid-mode))
  (setf *open* 
      (hs::make-hstack :table (make-hash-table :test 'eql
                                               :synchronized nil)
                       :keyfn #'node.state.idb-hash))
  ;; Always reset closed-state storage so prior runs do not retain old graph tables.
  (setf *closed* (make-hash-table :test 'eql :synchronized nil))
  ;; Drop shard references unless graph mode with parallelism reinitializes them below.
  (when (boundp '*closed-shards*)
    (setf *closed-shards* nil))
  (when (boundp '*closed-shard-locks*)
    (setf *closed-shard-locks* nil))
  (when (eql *tree-or-graph* 'graph)
    (let ((hash-test (if *hybrid-mode* 'equal 'eql))) 
      (setf *closed* (make-hash-table :test hash-test
                                      :size 200003
                                      :rehash-size 2.7
                                      :rehash-threshold 0.8
                                      :synchronized nil))
      (when (> *threads* 0)
        (initialize-closed-infrastructure hash-test))))
  (when (> *threads* 0)  ;; Ensure start state has synchronized IDB tables for parallel mode
    (ensure-start-state-synchronized))
  (hs::push-hstack (make-node :state (copy-problem-state *start-state*)) *open* :new-only (eq *tree-or-graph* 'graph))
  ;; Reserve start state in *closed* for graph search (maintains consistency with process-successors)
  (when (eql *tree-or-graph* 'graph)
    (ensure-idb-hash *start-state*)
    (let ((closed-table (if (> *threads* 0)
                            (closed-shard *start-state*)
                            *closed*)))
      (setf (gethash (closed-key *start-state* 0) closed-table)
        (list (problem-state.idb *start-state*)
              0
              (problem-state.time *start-state*)
              (problem-state.value *start-state*)))))
  (setf *program-cycles* 0)
  (setf *average-branching-factor* 0.0)
  (setf *total-states-processed* 1)  ;start state is first
  (setf *prior-total-states-processed* 0)
  (setf *rem-init-successors* nil)  ;branch nodes from start state
  (setf *num-init-successors* 0)
  (setf *max-depth-explored* 0)
  (setf *num-idle-threads* 0)
  (setf *accumulated-depths* 0)
  (setf *repeated-states* 0)
  (setf *num-paths* 0)
  (setf *solutions* nil)
  (setf *hybrid-goals* nil)
  (setf *unique-solutions* nil)
  (setf *best-states* (list *start-state*))
  (setf *solution-count* 0)
  (setf *upper-bound* 1000000)
  (setf *search-tree* nil)
  (setf *start-time* (get-internal-real-time))
  (setf *prior-time* 0)
  (setf *inconsistent-states-dropped* 0)
  (clrhash *prop-key-cache*)
  (if (> *threads* 0)
    (if (eql *algorithm* 'backtracking)
      (error "Parallel processing not supported with backtracking algorithm")
      (progn
        (process-partitioned-parallel)
        (finalize-parallel-search-results)))
    (ecase *algorithm*
      (depth-first (search-serial))
      (backtracking (search-backtracking))))
  (when *hybrid-mode*
    (finalize-hybrid-solutions))
  (let ((*package* (find-package :ww)))  ;avoid printing package prefixes
    (unless *shutdown-requested*
      (summarize-search-results (if (solution-count-reached-p)  ; CHANGED: handle fixnum
                                  'first
                                  'exhausted)))))


(defun auto-wait-debug-find-prop (props pred &rest prefix)
  "Return the first proposition in PROPS whose (car ...) is PRED and whose
   arguments begin with PREFIX. Used for compact debug signatures."
  ;; ADDED
  (declare (type list props))
  (find-if (lambda (p)
             (and (consp p)
                  (eql (car p) pred)
                  (loop for x in prefix
                        for y in (cdr p)
                        always (eql x y))))
           props))


(defun auto-wait-debug-state-signature (state)
  "Compact signature to detect whether STATE drifted/mutated between re-push and backtrack-wait."
  ;; ADDED
  (declare (type problem-state state))
  (let* ((idb (problem-state.idb state))
         (props (list-database idb)))
    (list :time (problem-state.time state)
          :idb-count (hash-table-count idb)
          :idb-hash (compute-idb-hash idb)
          :elev-agent1 (auto-wait-debug-find-prop props 'ELEVATION 'AGENT1)
          :loc-buzzer1 (auto-wait-debug-find-prop props 'LOC 'BUZZER1)
          :loc-box2    (auto-wait-debug-find-prop props 'LOC 'BOX2))))


(defun search-serial ()
  "Branch & Bound DFS serial search."
  (iter
    (when (hs::empty-hstack *open*)
      (leave))  ;terminate *open*
    (for current-node = (hs::peek-hstack *open*))
    (for succ-nodes = (df-bnb1 *open*))
    (when (equal succ-nodes '(first))
      (return-from search-serial 'first))
    (when succ-nodes  ;nongoal succ-nodes
      (if (fboundp 'heuristic?)
        (setf succ-nodes (sort (copy-list succ-nodes) #'>
                               :key (lambda (node)
                                      (problem-state.heuristic (node.state node)))))
        (when *randomize-search*
          (setf succ-nodes (alexandria:shuffle succ-nodes)))))
    (when (= *program-cycles* 0)  ;ie, expanding the start state
      (when (>= *branch* 0)  ;choose an initial branch to explore, drop others
        (format t "~&Exploring only branch ~D of ~D~%" *branch* (length succ-nodes))
        (setf succ-nodes (subseq succ-nodes (1- *branch*) *branch*)))
      (setf *num-init-successors* (length succ-nodes))
      (setf *rem-init-successors* (reverse succ-nodes)))
    ;; Backtrack-triggered wait setup
    ;; If we have successors and auto-wait is enabled, re-push current-node with wait-tried=T.
    ;; This ensures that after all successors are exhausted, we'll encounter current-node again
    ;; and df-bnb1 will attempt backtrack-triggered wait before truly backtracking.
    (when (and succ-nodes
               (auto-wait-enabled-p)
               (not (node.wait-tried current-node)))
      (setf (node.wait-tried current-node) t)
      (hs::push-hstack current-node *open* :new-only nil))  ; Push underneath successors
    (iter (for succ-node in succ-nodes)
          (hs::push-hstack succ-node *open* :new-only (eq *tree-or-graph* 'graph)))  ;push lowest heuristic value last
    (increment-global *program-cycles* 1)  ;finished with this cycle
    (setf *average-branching-factor* (compute-average-branching-factor))
    (print-search-progress)  ;#nodes expanded so far
    (after-each ;; Probe facility - always available regardless of debug compilation
                (when (= *debug* 6)
                  (setf *debug* 0)
                  (format t "~2%Probing current node: ~A~2%" current-node)
                  (format t "~%Successor nodes (~D):~%" (length succ-nodes))
                  (when succ-nodes
                    (dolist (succ-node succ-nodes)
                      (format t "~%  ~A~%" succ-node)))
                  (simple-break))  ; Reset for next probe
                ;; Full debug output - only when :ww-debug compiled in
                #+:ww-debug (when (= *debug* 5)
                              (format t "~%---~%Restating current node for easy reference: ~A~%---~%" current-node)
                              (simple-break)))))  ;allows continuing search for next *probe*


;;; ============================================================================
;;; AUTO-WAIT STUCK HANDLER
;;; ============================================================================


(defun handle-auto-wait-stuck (current-node)
  "Attempts auto-wait when node has no successors (agent is stuck).
   Called from df-bnb1 when succ-states is nil and auto-wait is enabled.
   
   Returns one of:
     (:goal goal-state)        - Goal reached during wait; goal-state for registration
     (:continue wait-state)    - Action became possible; wait-state as single successor
     (:fail)                   - Auto-wait failed or not applicable; use normal dead-end handling
   
   The wait-state returned for :continue has name WAIT and instantiations (duration)
   for proper solution path recording."
  (declare (type node current-node))
  (let ((state (node.state current-node)))
    ;; Attempt macro-wait simulation
    (multiple-value-bind (outcome wait-duration sim-state)
        (simulate-until-action-applicable state *auto-wait-max-time*)
      (case outcome
        ;; Goal was reached during waiting
        (:goal
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Auto-wait: Goal reached after waiting ~A time units~%" wait-duration))
         (let ((wait-state (create-auto-wait-state state sim-state wait-duration)))
           (values :goal wait-state)))
        
        ;; An action became applicable after waiting
        (:action
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Auto-wait: Action became applicable after waiting ~A time units~%" wait-duration))
         (let ((wait-state (create-auto-wait-state state sim-state wait-duration)))
           (values :continue wait-state)))
        
        ;; Timeout - waited too long without progress
        (:timeout
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Auto-wait: Timeout after ~A time units~%" *auto-wait-max-time*))
         (values :fail nil))
        
        ;; Agent was killed during wait
        (:killed
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Auto-wait: Agent killed during wait~%"))
         (values :fail nil))
        
        ;; No happenings to simulate
        (:no-happenings
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Auto-wait: No happenings available~%"))
         (values :fail nil))
        
        ;; Unknown outcome - treat as failure
        (otherwise
         (values :fail nil))))))


(defun handle-auto-wait-backtrack (current-node)
  "Attempts backtrack-triggered wait when all regular successors have been exhausted.
   Called from df-bnb1 when node.wait-tried is T (second visit after children exhausted).
   
   Returns one of:
     (list wait-node)          - Wait succeeded, continue exploring from wait-node
     '(first)                  - Goal reached during wait and *solution-type* is first
     nil                       - Wait failed, continue backtracking
   
   This enables finding solutions that require waiting when regular actions exist
   but all lead to dead ends."
  (declare (type node current-node))
  (let ((state (node.state current-node)))
    #+:ww-debug (when (>= *debug* 3)
                  (format t "~&Backtrack-triggered wait attempt at depth ~D~%" (node.depth current-node)))
    ;; Attempt macro-wait simulation
    (multiple-value-bind (outcome wait-duration sim-state)
        (simulate-until-action-applicable state *auto-wait-max-time*)
      (case outcome
        ;; Goal was reached during waiting
        (:goal
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Backtrack-wait: Goal reached after waiting ~A time units~%" wait-duration))
         (let* ((wait-state (create-auto-wait-state state sim-state wait-duration))
                (succ-depth (1+ (node.depth current-node))))
           ;; Check if we can improve on existing solutions
           (when (and *solutions* (member *solution-type* '(min-length min-time min-value max-value)))
             (unless (f-value-better wait-state succ-depth)
               ;; Can't improve, continue backtracking
               (return-from handle-auto-wait-backtrack nil)))
           ;; Register solution
           #+:ww-debug (when (>= *debug* 1)
                         (update-search-tree wait-state (1+ (node.depth current-node)) "backtrack-wait->goal"))
           (register-solution current-node wait-state)
           (update-max-depth-explored succ-depth)
           (finalize-path-depth succ-depth)
           (increment-global *total-states-processed* 1)
           (if (solution-count-reached-p)  ; CHANGED: was (eql *solution-type* 'first)
               '(first)
               nil)))  ; Continue searching for more solutions
        
        ;; An action became applicable after waiting
        (:action
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Backtrack-wait: Action became applicable after waiting ~A time units~%" wait-duration))
         (let ((wait-state (create-auto-wait-state state sim-state wait-duration)))
           #+:ww-debug (when (>= *debug* 1)
                         (update-search-tree wait-state (1+ (node.depth current-node)) "backtrack-wait->continue"))
           (update-max-depth-explored (1+ (node.depth current-node)))
           (increment-global *total-states-processed* 1)
           ;; Return wait-node as successor for further exploration
           (list (make-node :state wait-state
                            :depth (1+ (node.depth current-node))
                            :parent current-node
                            :wait-tried nil))))  ; New node starts fresh
        
        ;; Timeout, killed, no-happenings, or unknown - continue backtracking
        (otherwise
         #+:ww-debug (when (>= *debug* 3)
                       (format t "~&Backtrack-wait: Failed with outcome ~A~%" outcome))
         nil)))))


(defun df-bnb1 (open)
  "Performs expansion of one node from open. Returns
   new successor nodes, (first), or nil if no new nodes generated."
  (declare (type hs::hstack open))
  (let ((current-node (get-next-node-for-expansion open)))  ;pop next node
   (when *probe*
     (apply #'probe current-node *probe*))
   (iter
    #+:ww-debug (when (>= *debug* 3)
                  (format t "~&-----------------------------------------------------------~%")
                  (format t "~%Current node selected:~%~S~2%" current-node))
    (when (null current-node)  ;open is empty
      (return-from df-bnb1 nil))
    (when (and (> *depth-cutoff* 0) (= (node.depth current-node) *depth-cutoff*))
      (narrate "State at max depth" (node.state current-node) (node.depth current-node))
      (return-from df-bnb1 nil))
    (when (eql (bounding-function current-node) 'kill-node)
      (return-from df-bnb1 nil))
    ;; Backtrack-triggered wait check
    ;; If this node was re-pushed with wait-tried=T, all regular successors were exhausted.
    ;; Now try waiting as a "second chance" before truly backtracking.
    (when (and (auto-wait-enabled-p) (node.wait-tried current-node))
      (return-from df-bnb1 (handle-auto-wait-backtrack current-node)))
    ;; States are now reserved in *closed* when added to open in process-successors
    (let ((succ-states (expand current-node)))  ;from generate-children
      (when *troubleshoot-current-node*
        (setf *debug* 5)
        (setf *troubleshoot-current-node* nil)
        (next-iteration))  ;redo current-node
      (when (null succ-states)  ;no successors
        ;; Auto-wait stuck handling
        (when (auto-wait-enabled-p)
          (multiple-value-bind (outcome wait-state) (handle-auto-wait-stuck current-node)
            (case outcome
              ;; Goal reached during auto-wait
              (:goal
               (let ((succ-depth (1+ (node.depth current-node))))
                 ;; Register the wait action in search tree for debugging
                 #+:ww-debug (when (>= *debug* 1)
                               (update-search-tree wait-state (node.depth current-node) "auto-wait->goal"))
                 ;; Check if we can improve on existing solutions
                 (when (and *solutions* (member *solution-type* '(min-length min-time min-value max-value)))
                   (unless (f-value-better wait-state succ-depth)
                     ;; Can't improve, treat as dead end
                     (update-max-depth-explored (node.depth current-node))
                     (finalize-path-depth (node.depth current-node))
                     (return-from df-bnb1 nil)))
                 ;; Register solution: current-node is predecessor, wait-state is goal
                 ;; The solution path will show: ... -> (current-node's action) -> (WAIT duration)
                 (register-solution current-node wait-state)
                 (update-max-depth-explored succ-depth)
                 (finalize-path-depth succ-depth)
                 (increment-global *total-states-processed* 1)
                 (if (solution-count-reached-p)  ; CHANGED: was (eql *solution-type* 'first)
                     (return-from df-bnb1 '(first))
                     (return-from df-bnb1 nil))))
              
              ;; Action became applicable after auto-wait
              (:continue
               #+:ww-debug (when (>= *debug* 1)
                             (update-search-tree wait-state (node.depth current-node) "auto-wait->continue"))
               ;; Return wait-state as the single successor for further expansion
               (update-max-depth-explored (1+ (node.depth current-node)))
               (increment-global *total-states-processed* 1)
               (return-from df-bnb1 
                 (list (make-node :state wait-state
                                  :depth (1+ (node.depth current-node))
                                  :parent current-node))))
              
              ;; Auto-wait failed - fall through to normal dead-end handling
              (:fail nil))))
        (update-max-depth-explored (node.depth current-node))
        (narrate "No successor states" (node.state current-node) (node.depth current-node))
        (finalize-path-depth (node.depth current-node)) 
        (return-from df-bnb1 nil))
      #+:ww-debug (when (>= *debug* 1)
                    (update-search-tree (node.state current-node) (node.depth current-node) ""))
      (update-max-depth-explored (1+ (node.depth current-node)))
      (increment-global *total-states-processed* (length succ-states))
;      #+:ww-debug (when (= *debug* 6) (simple-break))  ;probe found
      (return-from df-bnb1 (process-successors succ-states current-node open))))))  ;returns live successor nodes


(defun process-successors (succ-states current-node open)
  "Processes successor states: checks goals, handles duplicates, generates nodes.
   In hybrid mode, accumulates parent pointers for multi-path enumeration."
  (iter (with succ-depth = (1+ (node.depth current-node)))
        (for succ-state in succ-states)
        (when *global-invariants*
          (validate-global-invariants current-node succ-state))
        (when (and *solutions* (member *solution-type* '(min-length min-time min-value max-value)))
          (unless (f-value-better succ-state succ-depth)
            (next-iteration)))  ;throw out state if can't better best solution so far
        (when (goal succ-state)
          (if *hybrid-mode*
              (defer-hybrid-goal current-node succ-state)
              (register-solution current-node succ-state))
          (finalize-path-depth succ-depth)
          (if (solution-count-reached-p)  ; CHANGED: was (eql *solution-type* 'first)
            (return-from process-successors '(first))
            (next-iteration)))
        (unless (boundp 'goal-fn)
          (process-min-max-value succ-state))
        (when (and (eql *tree-or-graph* 'tree) (eql *problem-type* 'planning))
          (when (on-current-path succ-state current-node)
            (increment-global *repeated-states*)
            (finalize-path-depth succ-depth)
            (next-iteration)))
        (when (eql *tree-or-graph* 'graph)
          ;; Check if state already on open
          (let ((open-node (idb-in-open succ-state open succ-depth)))
            (when open-node
              (narrate "State already on open" succ-state succ-depth)
              (increment-global *repeated-states*)
              (cond (*hybrid-mode*
                     (add-parent-to-node open-node current-node (record-move succ-state))
                     (finalize-path-depth succ-depth))
                    (t
                     (if (update-open-if-succ-better open-node succ-state)
                       (setf (node.parent open-node) current-node)
                       (finalize-path-depth succ-depth))))
              (next-iteration)))
          ;; Check if state in closed
          (with-search-structures-lock
            (let ((closed-values (get-closed-values succ-state succ-depth)))
              (when closed-values
                (increment-global *repeated-states*)
                (cond (*hybrid-mode*
                       (let ((closed-node (get-closed-node succ-state succ-depth)))
                         (when closed-node
                           (add-parent-to-node closed-node current-node (record-move succ-state))))
                       (narrate "Accumulating parent for closed state" succ-state succ-depth)
                       (finalize-path-depth succ-depth)
                       (next-iteration))
                      ((better-than-closed closed-values succ-state succ-depth)
                       (narrate "Returning this previously closed state to open" succ-state succ-depth)
                       (remhash (closed-key succ-state succ-depth) *closed*))
                      (t
                       (narrate "Dropping this previously closed state" succ-state succ-depth)
                       (finalize-path-depth succ-depth)
                       (next-iteration)))))
            ;; State is new or reopened - reserve immediately
            (let ((succ-node (generate-new-node current-node succ-state)))
              (setf (gethash (closed-key succ-state succ-depth) *closed*)
                    (make-closed-entry succ-state succ-depth succ-node))
              (collecting succ-node))))
        ;; Tree search path - generate node without closed tracking
        (when (eql *tree-or-graph* 'tree)
          (collecting (generate-new-node current-node succ-state)))))


(defun validate-global-invariants (current-node succ-state)
  "Validate all registered global invariants on the given succ-state.
   Returns T if all invariants pass, NIL if any fail.
   If current-node is nil, this is a start state validation."
  (loop for invariant-name in *global-invariants*
        for fn = (symbol-function invariant-name)
        unless (funcall fn succ-state)
        do (if (null current-node)
             (troubleshoot "~%Invariant ~A failed on start state:~2%~A"
                          invariant-name
                          succ-state)
             (troubleshoot "~%Invariant ~A failed during transition:~2%Current node:~%~A~2%Successor state:~%~A" 
                          invariant-name
                          current-node
                          succ-state))
           (return-from validate-global-invariants nil))
  t)


(defun idb-in-open (succ-state open &optional succ-depth)
  "Determines if a state's idb matches the contents of a key in open's table.
   Uses idb-hash for O(1) lookup with idb verification for collision safety.
   In hybrid mode, also requires depth match (succ-depth must be provided).
   Returns the node in open or nil."
  (declare (type problem-state succ-state))
  (ensure-idb-hash succ-state)  ; ensure hash is cached
  (let ((hash-key (problem-state.idb-hash succ-state))
        (ht (hs::hstack.table open)))
    (let ((nodes (gethash hash-key ht)))  ; lookup by hash
      (when nodes
        ;; verify idb matches to handle hash collisions
        ;; in hybrid mode, also verify depth matches
        (find-if (lambda (node)
                   (and (equalp (problem-state.idb succ-state)
                                (problem-state.idb (node.state node)))
                        (or (not *hybrid-mode*)
                            (= succ-depth (node.depth node)))))
                 nodes)))))


(defun compute-idb-hash (idb-hash-table)
  "Computes a fixnum hash from an idb hash table.
   Uses XOR of sxhash values for deterministic hashing."
  (declare (type hash-table idb-hash-table))
  (let ((hash 0))
    (declare (type fixnum hash))
    (maphash (lambda (k v)
               (setf hash (logxor hash (sxhash (cons k v)))))  ; hash the pair as a unit
             idb-hash-table)
    hash))


(defun canonical-idb-equal-p (idb1 idb2)
  "Check if two idb hash-tables are equal under canonical symmetry.
   Returns T if the states are identical or symmetric permutations.
   For non-symmetric mode, uses standard equalp comparison."
  (declare (type hash-table idb1 idb2))
 (ww-with-timing :symm/canon-equal
  (if (use-canonical-symmetry-p)
      ;; Canonical comparison: build canonical forms and compare
      (let ((canon1 (build-canonical-idb-form idb1))
            (canon2 (build-canonical-idb-form idb2)))
        (equal canon1 canon2))
      ;; Standard comparison
      (equalp idb1 idb2))))


(defun build-canonical-idb-form (idb)
  "Build a canonical (sorted) representation of IDB for equality comparison.
  - Canonical proposition keys become packed integers (when any symmetric mapping occurs),
  which reduces consing and makes sorting cheaper."
  (declare (type hash-table idb))
  (ww-with-timing :symm/canon-form
    (let ((decoded-int-keys (make-hash-table :test #'eql)))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (integerp k)
                   (setf (gethash k decoded-int-keys)
                         (extract-integer-components k))))
               idb)
      (let ((canonical-map (build-canonical-mapping idb decoded-int-keys))
            (canon-props nil))
        (maphash (lambda (key val)
                   (let* ((components (and (integerp key) (gethash key decoded-int-keys)))
                          (canon-key (if (integerp key)
                                         (canonicalize-proposition-key key canonical-map components) ;; PH4C
                                         key))
                          (canon-val (canonicalize-value val canonical-map)))
                     (push (cons canon-key canon-val) canon-props)))
                 idb)
        (sort canon-props #'canonical-prop-less-p)))))


(defun canonical-prop-less-p (prop1 prop2)
  "Comparison function for sorting canonicalized propositions.

;; PH4C: fast path for integer keys."
  (let ((k1 (car prop1))
        (k2 (car prop2)))
    (cond ((and (integerp k1) (integerp k2)) (< k1 k2))
          ((integerp k1) t)
          ((integerp k2) nil)
          (t (string< (prin1-to-string k1)
                      (prin1-to-string k2))))))


(defun make-closed-entry (state depth &optional node)
  "Build the value stored in *closed* for STATE at DEPTH.
   PH3A: adds a CANON-FORM slot (initially NIL) for lazy canonical-form caching."
  (if *hybrid-mode*
      (list (problem-state.idb state)
            depth
            (problem-state.time state)
            (problem-state.value state)
            nil          ; <-- PH3A: cached canonical form goes here
            node)
      (list (problem-state.idb state)
            depth
            (problem-state.time state)
            (problem-state.value state)
            nil)))


(defun get-closed-values (state depth)
  "Retrieve the closed values for a state.
   When canonical symmetry is active, verifies canonical equality to prevent
   false positives from hash collisions.
   CALLER MUST HOLD THE APPROPRIATE SHARD LOCK in parallel mode."
  (let ((closed-values (gethash (closed-key state depth)
                                (if (> *threads* 0)
                                    (closed-shard state)
                                    *closed*))))
    (when closed-values
      (if (use-canonical-symmetry-p)
          (let ((succ-idb (problem-state.idb state))
                (closed-idb (first closed-values)))
            (cond
              ((equalp closed-idb succ-idb)
               ;; Exact duplicate
               closed-values)
              (t
               ;; lazy-cache canonical form for CLOSED entry
               (let ((closed-canon (fifth closed-values)))
                 (unless closed-canon
                   (setf (fifth closed-values)
                         (build-canonical-idb-form closed-idb))
                   (setf closed-canon (fifth closed-values)))
                 (if (equal (build-canonical-idb-form succ-idb) closed-canon)
                     (progn
                       (increment-global *symmetric-duplicates-pruned*)
                       closed-values)
                     ;; Hash collision - different states
                     nil)))))
          ;; Standard mode: hash match is sufficient
          closed-values))))


(defun get-closed-node (state depth)
  "Retrieve the node stored in *closed* for hybrid mode.
   CALLER MUST HOLD THE APPROPRIATE SHARD LOCK in parallel mode."
  (let ((values (gethash (closed-key state depth)
                         (if (> *threads* 0)
                             (closed-shard state)
                             *closed*))))
    (when values
      (sixth values))))  ; node moved from 5th -> 6th


(defun goal (state)
  "Returns t or nil depending on if state is a goal state."
  (declare (type problem-state state))
  (when (boundp 'goal-fn)
    (funcall (symbol-function 'goal-fn) state)))


(defun process-min-max-value (succ-state)
  "Determines if succ-state value is an improvement, and if so updates *best-states*."
  (let ((current-value (problem-state.value succ-state))
        (best-value (problem-state.value (first *best-states*))))
    (ecase *solution-type*
      (max-value (when (> current-value best-value)
                   (bt:with-lock-held (*lock*)
                     (format t "~%Higher value state found: ~A in thread ~D~%"
                             (problem-state.value succ-state) (lparallel:kernel-worker-index))
                     (finish-output))
                   (push-global succ-state *best-states*)))
      (min-value (when (< current-value best-value)
                   (bt:with-lock-held (*lock*)
                     (format t "~%Lower value state found: ~A in thread ~D~%"
                             (problem-state.value succ-state) (lparallel:kernel-worker-index))
                     (finish-output))
                   (push-global succ-state *best-states*))))))

 
(defun f-value-better (succ-state succ-depth)
  "Computes f-value of current-node to see if it's better than best solution so far."
  (let ((best-solution (first *solutions*)))
    (case *solution-type*
      ((min-length first)
        (< succ-depth (solution.depth best-solution)))
      (min-time
        (< (problem-state.time succ-state) (solution.time best-solution)))
      (min-value
        (< (problem-state.value succ-state) (solution.value best-solution)))
      (max-value
        (> (problem-state.value succ-state) (solution.value best-solution))))))


(defun update-open-if-succ-better (open-node succ-state)
  "Determines if f-value of successor is better than open state, and updates it."
  (let ((open-state (node.state open-node)))
    (ecase *solution-type*
      ((min-length first every)
         nil)  ;in depth first search succ depth is never better than open
      (min-time
         (when (< (problem-state.time succ-state) (problem-state.time open-state))
           (setf (problem-state.time open-state) (problem-state.time succ-state))))
      (min-value
         (when (< (problem-state.value succ-state) (problem-state.value open-state))
           (setf (problem-state.value open-state) (problem-state.value succ-state))))
      (max-value
         (when (> (problem-state.value succ-state) (problem-state.value open-state))
           (setf (problem-state.value open-state) (problem-state.value succ-state)))))))


(defun better-than-closed (closed-values succ-state succ-depth)
  "Check if succ-state is better than the closed version.
   CALLER MUST HOLD THE APPROPRIATE SHARD LOCK in parallel mode."
  (declare (ignorable succ-depth))
  (let ((closed-depth (second closed-values))
        (closed-time (third closed-values))
        (closed-value (fourth closed-values)))
    (case *solution-type*
      ((first every min-length)
       (< succ-depth closed-depth))
      (min-time
       (< (problem-state.time succ-state) closed-time))
      (min-value
       (< (problem-state.value succ-state) closed-value))
      (max-value
       (> (problem-state.value succ-state) closed-value))
      (otherwise nil))))


(defun bounding-function (current-node)
  "Applies the bounding function, if there is one."
  (when (fboundp 'bounding-function?)
    (ut::mvb (current-cost current-upper) (funcall (symbol-function 'bounding-function?) (node.state current-node))
       #+:ww-debug (when (>= *debug* 3)
                     (format t "~&Cost bound = ~A, Upper bound = ~A~%" current-cost current-upper))
       (cond ((> current-cost *upper-bound*)
                (narrate "State killed by bounding" (node.state current-node) (node.depth current-node))
                #+:ww-debug (when (>= *debug* 3)
                              (format t "~&current-cost = ~F > *upper-bound* = ~F~%" current-cost *upper-bound*))
                (bt:with-lock-held (*lock*)
                  (format t "bounding a state...")
                  (finish-output))
                (return-from bounding-function 'kill-node))
             ((< current-upper *upper-bound*)
                #+:ww-debug (when (>= *debug* 3)
                              (format t "~&Updating *upper-bound* from ~F to ~F~%" *upper-bound* current-upper))
                (setf *upper-bound* current-upper))))))


(defun update-max-depth-explored (succ-depth)
  (when (> succ-depth *max-depth-explored*)
    (increment-global *max-depth-explored* (- succ-depth *max-depth-explored*))))


(defun get-next-node-for-expansion (open)
  "Returns the node at the top of open."
  (declare (type hs::hstack open))
  (unless (hs::empty-hstack open)
    (hs::pop-hstack open)))  ;return node at top of stack or nil


(defun compute-average-branching-factor ()
  "Average branching on each cycle."
  (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float))


(defun on-current-path (succ-state current-node)
  "Determines if a successor is already on the current path from the start state.
   Uses cached idb-hash for O(1) comparison with equalp verification on hash collision."  ; CHANGED: Updated docstring
  (ensure-idb-hash succ-state)  ; ensure hash is cached
  (when (iter (for node initially current-node then (node.parent node))
              (while node)
              (for node-state = (node.state node))  ; bind node-state for clarity
              (ensure-idb-hash node-state)  ; ensure hash is cached
              (thereis (and (= (problem-state.idb-hash succ-state)  ; hash comparison first
                              (problem-state.idb-hash node-state))
                           (equalp (problem-state.idb succ-state)  ; equalp only on collision
                                  (problem-state.idb node-state)))))
    (narrate "State already on current path" succ-state (1+ (node.depth current-node)))
    t))


(defun update-search-tree (state depth message)
  (declare (type problem-state state) (type fixnum depth) (type string message))
  (when (and (not (> *threads* 0)) (>= *debug* 1))
    (push `((,(problem-state.name state) 
             ,@(problem-state.instantiations state))
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state.idb state))))))
          *search-tree*)))


(defun narrate (string state depth)
  (declare (ignorable string state depth))
  #+:ww-debug (when (>= *debug* 3)
                (format t "~%~A:~%~A~%" string state))
  #+:ww-debug (when (>= *debug* 1)
                (update-search-tree state depth string))
  nil)


(defun generate-new-node (current-node succ-state)
  "Produces a new node for a given successor.
   In hybrid mode, stores parent as (parent-node . move) pair."
  (declare (type node current-node) (type problem-state succ-state))
  (let* ((depth (1+ (node.depth current-node)))
         (move (record-move succ-state))
         (parent-entry (if *hybrid-mode*
                           (list (cons current-node move))
                           current-node))
         (succ-node (make-node :state succ-state
                               :depth depth
                               :parent parent-entry)))
    #+:ww-debug (when (>= *debug* 3)
                  (format t "~%Installing new or updated successor:~%~S~%" succ-node))
    succ-node))


(defun at-max-depth (succ-depth)
  "Determines if installing a nongoal successor to the current node will be
   pointless, based on it being at the max allowable depth."
  (declare (type fixnum succ-depth))
  (and (> *depth-cutoff* 0) (>= succ-depth *depth-cutoff*)))


(defun best-states-last (state1 state2)
  "Used to sort a list of expanded states according to the user-defined heuristic."
  (declare (type problem-state state1 state2))
  (> (estimate-to-goal state1) (estimate-to-goal state2)))


(defun finalize-path-depth (depth)
  "Records the path depth of a path that terminates."
  (increment-global *accumulated-depths* depth)
  (increment-global *num-paths* 1))


;;; Solution Processing Functions


(defun record-solution-path (goal-node)
  "Recovers a path from a goal node back to the start node following parent links."
  (declare (type node goal-node))
  (let ((path nil))
    (do ((n goal-node (node.parent n)))
        ((null (node.parent n)) path)
      (push (record-move ;(node.state (node.parent n))
                         (node.state n))
            path))))


(defun enumerate-paths-to-node (node)
  "Enumerates all paths from the start node to NODE.
   Returns a list of paths, where each path is a list of (action instantiations) moves.
   Paths are in forward order (start to node).
   In hybrid mode, uses moves stored in (parent-node . move) pairs.
   In standard mode with single parents, returns a single-element list."
  (declare (type node node))
  (let ((parent-entries (node.parent node)))
    (if (null parent-entries)
        ;; At start node - return one path with no moves
        (list nil)
        ;; Branch based on mode
        (if *hybrid-mode*
            ;; Hybrid mode: parent-entries is list of (parent-node . move) pairs
            (mapcan (lambda (entry)
                      (let ((parent (car entry))
                            (move (cdr entry)))
                        (mapcar (lambda (path-to-parent)
                                  (append path-to-parent (list move)))
                                (enumerate-paths-to-node parent))))
                    parent-entries)
            ;; Standard mode: parent-entries is single node or list of nodes
            (let ((parents (if (listp parent-entries)
                               parent-entries
                               (list parent-entries)))
                  (current-move (record-move (node.state node))))
              (mapcan (lambda (parent)
                        (mapcar (lambda (path-to-parent)
                                  (append path-to-parent (list current-move)))
                                (enumerate-paths-to-node parent)))
                      parents))))))


(defun summarize-search-results (condition)
  (declare (type symbol condition))
  (format t "~2%In problem ~A, performed ~A~A search for ~A solution."
            *problem-name* 
            (if *hybrid-mode* "hybrid " "")
            *tree-or-graph* *solution-type*)
  (ecase condition
    (first
      (when *solutions*
        (if (typep *solution-type* 'fixnum)
            (format t "~2%Search ended after finding ~D solution~:P (as requested)." 
                    (length *solutions*))
            (format t "~2%Search ended with first solution found."))))
    (exhausted
      (format t "~2%~A search process completed normally." *algorithm*)
      (when (eql *solution-type* 'every)
        (cond (*hybrid-mode*
               (format t "~2%Hybrid mode enumerated all paths to goal states at depth ~D."
                       *depth-cutoff*))
              ((and (eql *tree-or-graph* 'tree) (eql *symmetry-pruning* nil))
               (format t "~2%Exhaustive search for every solution finished (up to the depth cutoff, if any)."))
              (t
               (format t "~2%Exhaustive search for every solution finished (except solutions in pruned branches)."))))))
  (format t "~2%Depth cutoff = ~:D" *depth-cutoff*)
  (format t "~2%Maximum depth explored = ~:D" *max-depth-explored*)
  (format t "~2%Program cycles = ~:D" *program-cycles*)
  (format t "~2%Total states processed = ~:D" *total-states-processed*)
  (when (eql *tree-or-graph* 'graph)
    (format t "~2%Repeated states = ~:D, ie, ~,1F percent"
              *repeated-states* (* (/ *repeated-states* *total-states-processed*) 100)))
  (when (> *inconsistent-states-dropped* 0)
    (format t "~%~%Abandoned ~D inconsistent state~:P due to convergence failure (non-terminating cyclical states)."
            *inconsistent-states-dropped*))
  (unless (eql *problem-type* 'csp)
    (format t "~2%Average branching factor = ~,1F~%" *average-branching-factor*))
  (let ((sym-stats (format-symmetry-statistics)))
    (when sym-stats
      (format t "~%~A~%" sym-stats)))
  (format t "~%Start state:~%~A" (list-database (problem-state.idb *start-state*)))
  (format t "~2%Goal:~%~A~2%" (when (boundp 'goal-fn)
                                (get 'goal-fn :form)))  ;(symbol-value 'goal-fn)
  (when (and (eql *solution-type* 'count)) (> *solution-count* 0)
    (format t "~%Total solution paths found = ~:D ~2%" *solution-count*))
  (when *solutions*  ;ie, recorded solutions
    (let* ((shallowest-depth (reduce #'min *solutions* :key #'solution.depth))
           (shallowest-depth-solution (find shallowest-depth *solutions* :key #'solution.depth))
           (minimum-time (reduce #'min *solutions* :key #'solution.time))
           (minimum-time-solution (find minimum-time *solutions* :key #'solution.time))
           (min-max-value-solution (first *solutions*))
           (min-max-value (solution.value min-max-value-solution)))
      (format t "~2%Total solution paths recorded = ~:D, of which ~:D is/are unique solution paths" 
                (length *solutions*) (length *unique-solutions*))
      (format t "~%Check *solutions* and *unique-solutions* for solution records.")
      (case *solution-type*
        (first
          (format t "~2%Number of steps in first solution found: = ~:D" shallowest-depth)
          (format t "~2%Duration of first solution found = ~:D" minimum-time)
          (format t "~2%Solution path of first solution found from start state to goal state:~%")
          (printout-solution shallowest-depth-solution))
        (min-length
          (format t "~2%Number of steps in a minimum path length solution = ~:D" shallowest-depth)
          (format t "~2%A minimum length solution path from start state to goal state:~%")
          (printout-solution shallowest-depth-solution))
        (min-time
          (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
          (format t "~2%A minimum time solution path from start state to goal state:~%")
          (printout-solution minimum-time-solution))
        (min-value
          (format t "~2%Value of a minimum value solution = ~:D" min-max-value)
          (format t "~2%A minimum value solution path from start state to goal state:~%")
          (printout-solution min-max-value-solution))
        (max-value
          (format t "~2%Value of a maximum value solution = ~:D" min-max-value)
          (format t "~2%A maximum value solution path from start state to goal state:~%")
          (printout-solution min-max-value-solution))
        (every
          (format t "~2%Number of steps in a minimum path length solution = ~:D" shallowest-depth)
          (format t "~2%A minimum length solution path from start state to goal state:~%")
          (printout-solution shallowest-depth-solution)
          (cond ((and (not (eq *problem-type* 'csp))
                      (equalp shallowest-depth-solution minimum-time-solution))
                   (format t "~%A shortest path solution is also a minimum duration solution.~2%"))
                (t (unless (eq *problem-type* 'csp)
                     (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
                     (format t "~2%A minimum time solution path from start state to goal state:~%")
                     (printout-solution minimum-time-solution))))))))
  (if (boundp 'goal-fn)  ;(get 'goal-fn 'formula)
    (when (or (and (eql *solution-type* 'count) (= *solution-count* 0))
              (and (not (eql *solution-type* 'count)) (null *solutions*)))
      (format t "~&No solutions found.~%"))
    (format t "~&No goal specified, but best results follow:"))
  (unless (boundp 'goal-fn)  ; (null (get 'goal-fn 'formula))
    (format t "~2%Total number of results recorded = ~:D." (length *best-states*))
    (format t "~%Check *best-states* for all result records.")
    (case *solution-type*
        (min-value
          (let ((best-state (reduce #'(lambda (a b)
                                        (if (<= (problem-state.value a) (problem-state.value b))
                                          a
                                          b))
                                    *best-states*)))
            (format t "~2%The minimum objective value found = ~:D" (problem-state.value best-state))
            (format t "~2%A minimum value state:~%")
            (print-problem-state best-state)
            (format t "~2%")))
        (max-value
          (let ((best-state (reduce #'(lambda (a b)
                                        (if (>= (problem-state.value a) (problem-state.value b))
                                          a
                                          b))
                                    *best-states*)))
            (format t "~2%The maximum objective value found = ~:D" (problem-state.value best-state))
            (format t "~2%A maximum value state:~%")
            (print-problem-state best-state)
            (format t "~2%")))))
  (print-search-tree))


(defun print-search-tree ()
  (when (and (not (> *threads* 0)) (or (= *debug* 1) (= *debug* 2)))
    (when (y-or-n-p "~%Display search tree?")
      (format t "~2%Search tree:~%")
      (loop for act in (reverse *search-tree*)
            do (if (alexandria:length= 2 act)
                 (format t "~vT~d:~a~%" (* 3 (second act)) (second act) (first act))
                 (case *debug*
                   (1 (format t "~vT~d:~a ~a~%"
                                (* 3 (second act)) (second act) (first act) (third act)))
                   (2 (format t "~vT~d:~a ~a~%" 
                                (* 3 (second act)) (second act) (first act) (third act))
                      (format t "~vT  ~a~%"
                                (* 3 (second act)) (fourth act))
                      (when (fifth act)
                        (format t "~vT  ~a~%"
                                  (* 3 (second act)) (fifth act))))))
            finally (terpri)))))

 
(defun register-solution (current-node goal-state)
  "Inserts a new solution on the list of *solutions*."
  (declare (type node current-node) (type problem-state goal-state))
  (let* ((state-depth (1+ (node.depth current-node)))
         (solution
           (make-solution
             :depth state-depth
             :time (problem-state.time goal-state)
             :value (problem-state.value goal-state)
             :path (let ((nominal-path (append (record-solution-path current-node)
                                               (list (record-move ;(node.state current-node)
                                                                  goal-state)))))
                     (if (= (hash-table-count *state-codes*) 0)  ;if in backward search
                       nominal-path
                       (append nominal-path 
                               (reverse (gethash (funcall (symbol-function 'encode-state)
                                                          (list-database (problem-state.idb goal-state)))
                                                 *state-codes*)))))
             :goal goal-state)))
    (cond ((> *threads* 0)
             #+:ww-debug (when (>= *debug* 1)
                           (lprt))
             (let ((ctrl-str "~&New path to goal found at depth = ~:D"))
               (bt:with-lock-held (*lock*)
                 (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                   (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                           state-depth (solution.value solution))
                   (format t ctrl-str state-depth)))))
          (t (format t "~%New path to goal found at depth = ~:D" state-depth)
             (when (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
               (format t "Objective value = ~:A~%" (solution.value solution)))
             (when (eql *solution-type* 'min-time)
               (format t "Time = ~:A~%" (solution.time solution)))))
    (when (eql *algorithm* 'depth-first)
      (narrate "Solution found ***" goal-state state-depth))
    (push-global solution *solutions*)
    ;; Replace existing unique solution if new one is better
    (with-search-structures-lock
      (let* ((new-idb (problem-state.idb (solution.goal solution)))
             (existing (find new-idb *unique-solutions*
                             :key (lambda (soln)
                                    (problem-state.idb (solution.goal soln)))
                             :test #'equalp)))
        (cond (existing
               ;; Replace if new solution is better
               (when (solution-better-p solution existing)
                 (setf *unique-solutions*
                       (substitute solution existing *unique-solutions*))))
              (t
               (push-global solution *unique-solutions*)))))))


(defun solution-better-p (new-soln old-soln)
  "Returns T if NEW-SOLN is better than OLD-SOLN based on *solution-type*."
  (case *solution-type*  ; CHANGED: ecase -> case to allow otherwise clause
    ((min-length first every)
     (< (solution.depth new-soln) (solution.depth old-soln)))
    (min-time
     (< (solution.time new-soln) (solution.time old-soln)))
    (min-value
     (< (solution.value new-soln) (solution.value old-soln)))
    (max-value
     (> (solution.value new-soln) (solution.value old-soln)))
    (otherwise  ; ADDED: fixnum case - prefer shorter depth
     (< (solution.depth new-soln) (solution.depth old-soln)))))


(defun printout-solution (soln)
  (declare (type solution soln))
  (printout-solution-with-states soln))


(defun printout-solution-with-states (soln)
  "Print solution path with database state after each action.
   Used when *debug* >= 2 to show state progression."
  (declare (type solution soln))
  (let ((path (solution.path soln))
        (current-state (copy-problem-state *start-state*)))
    ;; Print start state at time 0.0
    (write '(0.0 (START-STATE)) :pretty t)
    (terpri)
    (format t "~A~%" (list-database (problem-state.idb current-state)))
    (terpri)
    ;; Process each action, showing resulting state
    (dolist (item path)
      (let* ((action-form (second item))
             (new-state (replay-action-to-state action-form current-state)))
        (write item :pretty t)
        (terpri)
        (if new-state
            (progn
              (setf current-state new-state)
              (format t "~A~%" (list-database (problem-state.idb current-state)))
              (terpri))
            (format t "[Action replay failed]~%"))))
    (terpri)))


(defun replay-action-to-state (action-form state)
  "Replay a single action from a solution path to state.
   Returns the new state, or NIL if replay fails.
   
   ACTION-FORM is (action-name arg1 arg2 ...) from solution path.
   STATE is the current problem-state.
   
   Unlike apply-action-to-state, this trusts the solution is valid and
   selects the correct effect by matching update.instantiations."
  (let* ((action-name (first action-form))
         (provided-args (rest action-form))
         (action (find action-name *actions* :key #'action.name)))
    
    ;; Handle WAIT action: duration is informational, not an effect variable
    (when (eql action-name 'wait)
      (let ((wait-duration (first provided-args))
            (new-state (copy-problem-state state)))
        (incf (problem-state.time new-state) wait-duration)
        (setf (problem-state.name new-state) 'wait)
        (return-from replay-action-to-state new-state)))
    
    (unless action
      (return-from replay-action-to-state nil))
    
    ;; Get precondition argument combinations
    (let ((precondition-args (if (action.dynamic action)
                                  (eval-instantiated-spec (action.precondition-type-inst action) state)
                                  (action.precondition-args action))))
      
      ;; Find any passing precondition and execute effect
      (dolist (pre-args precondition-args)
        ;; Effects can mutate the state while generating updates, so probe each
        ;; precondition/effect on a trial copy to keep replay deterministic.
        (let* ((trial-state (copy-problem-state state))
               (pre-result (apply (action.pre-defun-name action) trial-state pre-args)))
          (when pre-result
            ;; Execute effect to get all possible updates
            (let ((updated-dbs (if (eql pre-result t)
                                   (funcall (action.eff-defun-name action) trial-state)
                                   (apply (action.eff-defun-name action) trial-state pre-result))))
              
              ;; Find update with matching instantiations
              (dolist (update updated-dbs)
                (when (equal (update.instantiations update) provided-args)
                  ;; Found the matching effect - apply it
                  (let ((new-state (copy-problem-state state)))
                    (apply-update-to-state new-state update action)
                    ;; Apply followups if any
                    (when (update.followups update)
                      (apply-followups new-state update))
                    ;; Process happenings if present
                    (when *happening-names*
                      (let ((net-state (amend-happenings state new-state)))
                        (when net-state
                          (setf new-state net-state))))
                    (return-from replay-action-to-state new-state))))))))
      
      ;; No matching update found
      nil)))


(defun print-search-progress ()
  "Print search progress using appropriate global variables"
  (bt:with-lock-held (*lock*)
    (printout-search-progress)))


(defun printout-search-progress ()
  "Printout of nodes expanded so far during search modulo reporting interval."
  (when (<= (- *progress-reporting-interval* 
               (- *total-states-processed* *prior-total-states-processed*))
            0)
    (format t "~2%program cycles = ~:D" *program-cycles*)
    (format t "~%total states processed so far = ~:D" *total-states-processed*)
    (when (eql *tree-or-graph* 'graph)
      (if (> *threads* 0)
          ;; Parallel mode: sum counts across all shards
          (format t "~%ht count: ~:D    ht size: ~:D (across ~D shards)"
                  (loop for shard across *closed-shards* sum (hash-table-count shard))
                  (loop for shard across *closed-shards* sum (hash-table-size shard))
                  *num-closed-shards*)
          ;; Serial mode: single hash table
          (format t "~%ht count: ~:D    ht size: ~:D"
                  (hash-table-count *closed*)
                  (hash-table-size *closed*)))
      (format t "~%repeated states = ~:D, ie, ~,1F percent"
                *repeated-states* (* (/ *repeated-states* *total-states-processed*) 100)))
    (format t "~%frontier nodes: ~:D"
            (if (> *threads* 0)
                (total-parallel-frontier)
                (ecase *algorithm*
                  (depth-first (hs::length-hstack *open*))
                  (backtracking (length *choice-stack*)))))
    (unless (eql *problem-type* 'csp)
      (format t "~%net average branching factor = ~:D" (round *average-branching-factor*)))
    (when (zerop *threads*)
      (iter (while (and *rem-init-successors*
                        (not (idb-in-open (node.state (first *rem-init-successors*))
                                          *open*
                                          (node.depth (first *rem-init-successors*))))))
            (pop-global *rem-init-successors*))
      (format t "~%current progress: in #~:D of ~:D initial branches"
              (the fixnum (- *num-init-successors*
                             (length *rem-init-successors*)))
              *num-init-successors*))
    (unless (eql *problem-type* 'csp)
      (format t "~%average search depth = ~A"
              (if (> *num-paths* 0)
                  (round (/ *accumulated-depths* *num-paths*))
                  'pending)))
    (format t "~%current average processing speed = ~:D states/sec" 
            (round (/ (the fixnum (- *total-states-processed* *prior-total-states-processed*))
                      (/ (- (get-internal-real-time) *prior-time*)
                         internal-time-units-per-second))))
    (let ((sym-stats (format-symmetry-statistics)))
      (when sym-stats
        (format t "~%~A" sym-stats)))
    (format t "~%elapsed time = ~:D sec~2%" (round (/ (- (get-internal-real-time) *start-time*)
                                                     internal-time-units-per-second)))
    (finish-output)
    (setf *prior-time* (get-internal-real-time))
    (setf *prior-total-states-processed* *total-states-processed*)))


(defun ww-solve ()
  "Runs a branch & bound search on the problem specification."
  (if (> *threads* 0)
    (format t "~%working with ~D thread(s)...~2%" *threads*)
    (format t "~%working...~2%"))
  (time (dfs))
  (in-package :ww))


(defun defer-hybrid-goal (current-node goal-state)
  "Stores a goal-reaching pair for deferred enumeration after search completes.
   Called in hybrid mode when a goal is reached.
   When canonical symmetry is active, skips canonically-equivalent goals."
  (declare (type node current-node) (type problem-state goal-state))
  (let ((goal-depth (1+ (node.depth current-node))))
    ;; For canonical symmetry, check if equivalent goal already deferred
    (when (use-canonical-symmetry-p)
      (ensure-idb-hash goal-state)
      (let ((goal-hash (problem-state.idb-hash goal-state)))
        (when (find-if (lambda (pair)
                         (let ((existing-goal (cdr pair)))
                           (ensure-idb-hash existing-goal)
                           (and (= (problem-state.idb-hash existing-goal)
                                   goal-hash)
                                (canonical-idb-equal-p
                                 (problem-state.idb goal-state)
                                 (problem-state.idb existing-goal)))))
                       *hybrid-goals*)
          (narrate "Duplicate solution found (via symmetry) ***" goal-state goal-depth)
          (increment-global *repeated-states*)
          (return-from defer-hybrid-goal))))
    (narrate "Solution found (hybrid mode) ***" goal-state goal-depth)
    (push-global (cons current-node goal-state) *hybrid-goals*)))


(defun finalize-hybrid-solutions ()
  "Enumerates all solutions from stored goal-reaching pairs after search completes.
   Called once when all parent DAGs are fully constructed."
  (dolist (pair *hybrid-goals*)
    (let* ((current-node (car pair))
           (goal-state (cdr pair))
           (state-depth (1+ (node.depth current-node)))
           (goal-move (record-move goal-state))
           (paths-to-current (enumerate-paths-to-node current-node))
           (num-paths (length paths-to-current))
           (goal-idb (problem-state.idb goal-state)))
      (declare (ignore num-paths))
      (dolist (path paths-to-current)
        (let* ((full-path (append path (list goal-move)))
               (solution (make-solution
                           :depth state-depth
                           :time (problem-state.time goal-state)
                           :value (problem-state.value goal-state)
                           :path full-path
                           :goal goal-state)))
          (push-global solution *solutions*)
          (with-search-structures-lock
            (let ((existing
                    (find goal-idb *unique-solutions*
                          :key (lambda (soln)
                                 (problem-state.idb (solution.goal soln)))
                          :test #'equalp)))
              (cond (existing
                    ;; Replace if new solution is better
                     (when (solution-better-p solution existing)
                       (setf *unique-solutions*
                             (substitute solution existing *unique-solutions*))))
                    (t
                     (push-global solution *unique-solutions*))))))))))
