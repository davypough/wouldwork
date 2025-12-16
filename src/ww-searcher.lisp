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


(defmacro with-closed-shard-lock ((state) &body body)
  "Execute BODY while holding the shard lock for STATE.
   In serial mode, executes BODY directly without locking."
  (if (> *threads* 0)
      (let ((shard-idx (gensym "SHARD-IDX")))
        `(let ((,shard-idx (shard-index-for-state ,state)))
           (bt:with-lock-held ((svref *closed-locks* ,shard-idx))
             ,@body)))
      `(progn ,@body)))


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
   Returns the state's idb-hash."
  (unless (problem-state.idb-hash state)
    (setf (problem-state.idb-hash state)
          (compute-idb-hash (problem-state.idb state))))
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
      (format t "~&Note: *solution-type* EVERY with *algorithm* ~A not supported for hybrid mode; using standard search.~%"
              *algorithm*)
      (setf constraints-met nil))
    (unless (eql *tree-or-graph* 'graph)
      (format t "~&Note: *solution-type* EVERY with *tree-or-graph* ~A uses standard search; hybrid mode requires GRAPH.~%"
              *tree-or-graph*)
      (setf constraints-met nil))
    (unless (> *depth-cutoff* 0)
      (format t "~&Note: *solution-type* EVERY requires *depth-cutoff* > 0 for hybrid mode; using standard search.~%")
      (setf constraints-met nil))
    (when constraints-met
      (format t "~&Hybrid graph search mode active: enumerating all solutions at depth ~D.~%" *depth-cutoff*))
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
    ;(with-open-stream (*standard-output* (make-broadcast-stream))) ;ignore *standard-output*
    (if (eql *algorithm* 'backtracking)
      (error "Parallel processing not supported with backtracking algorithm")
      (progn
        (process-threads-chase-lev)
        (finalize-parallel-search-results)))
    (ecase *algorithm*
      (depth-first (search-serial))
      (backtracking (search-backtracking))))
  (when *hybrid-mode*
    (finalize-hybrid-solutions))
  (let ((*package* (find-package :ww)))  ;avoid printing package prefixes
    (unless *shutdown-requested*
      (summarize-search-results (if (eql *solution-type* 'first)
                                  'first
                                  'exhausted)))))


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
        (setf succ-nodes (subseq succ-nodes *branch* (1+ *branch*))))
      (setf *num-init-successors* (length succ-nodes))
      (setf *rem-init-successors* (reverse succ-nodes)))
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
    ;; States are now reserved in *closed* when added to open in process-successors
    (let ((succ-states (expand current-node)))  ;from generate-children
      (when *troubleshoot-current-node*
        (setf *debug* 5)
        (setf *troubleshoot-current-node* nil)
        (next-iteration))  ;redo current-node
      (when (null succ-states)  ;no successors
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
          (if (eql *solution-type* 'first)
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


(defun get-closed-values (state depth)
  "Retrieve the closed values for a state.
   CALLER MUST HOLD THE APPROPRIATE SHARD LOCK in parallel mode."
  (gethash (closed-key state depth)
           (if (> *threads* 0) 
               (closed-shard state)
               *closed*)))


(defun get-closed-node (state depth)
  "Retrieve the node stored in *closed* for hybrid mode.
   CALLER MUST HOLD THE APPROPRIATE SHARD LOCK in parallel mode."
  (let ((values (gethash (closed-key state depth)
                         (if (> *threads* 0)
                             (closed-shard state)
                             *closed*))))
    (when values
      (fifth values))))  ; Node is 5th element in hybrid mode


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
               (2 (list (list-database (problem-state.idb state))
                        (list-database (problem-state.hidb state))))))
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


(defun defer-hybrid-goal (current-node goal-state)
  "Stores a goal-reaching pair for deferred enumeration after search completes.
   Called in hybrid mode when a goal is reached."
  (declare (type node current-node) (type problem-state goal-state))
  (push-global (cons current-node goal-state) *hybrid-goals*))


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
      (format t "~%Enumerating ~D path~:P to goal at depth ~D~%" num-paths state-depth)
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
            (unless (find goal-idb *unique-solutions*
                          :key (lambda (soln)
                                 (problem-state.idb (solution.goal soln)))
                          :test #'equalp)
              (push-global solution *unique-solutions*))))))))

  
(defun summarize-search-results (condition)
  (declare (type symbol condition))
  (format t "~2%In problem ~A, performed ~A~A search for ~A solution."
            *problem-name* 
            (if *hybrid-mode* "hybrid " "")
            *tree-or-graph* *solution-type*)
  (ecase condition
    (first
      (when *solutions*
        (format t "~2%Search ended with first solution found." )))
    (exhausted
      (format t "~2%~A search process completed normally." *algorithm*)
      (when (eql *solution-type* 'every)
        (cond (*hybrid-mode*
               (format t "~2%Hybrid mode enumerated all paths to goal states at depth â‰¤ ~D."
                       *depth-cutoff*))
              ((eql *tree-or-graph* 'tree)
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
  ;(format t "~2%Unique states encountered = ~:D" (unique-states-encountered-graph))
  (when (> *inconsistent-states-dropped* 0)
    (format t "~%~%Dropped ~D inconsistent successor state~:P due to convergence failure."
            *inconsistent-states-dropped*))
  (format t "~2%Average branching factor = ~,1F" *average-branching-factor*)
  (format t "~2%Start state:~%~A" (list-database (problem-state.idb *start-state*)))
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
          finally (terpri))))

 
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
             (let ((ctrl-str "~&New path to goal found at depth = ~:D~%"))
               (bt:with-lock-held (*lock*)
                 (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                   (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                           state-depth (solution.value solution))
                   (format t ctrl-str state-depth)))))
          (t (format t "~%New path to goal found at depth = ~:D~%" state-depth)
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
  (ecase *solution-type*
    ((min-length first every)
     (< (solution.depth new-soln) (solution.depth old-soln)))
    (min-time
     (< (solution.time new-soln) (solution.time old-soln)))
    (min-value
     (< (solution.value new-soln) (solution.value old-soln)))
    (max-value
     (> (solution.value new-soln) (solution.value old-soln)))))


(defun printout-solution (soln)
  (declare (type solution soln))
  (dolist (item (solution.path soln))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~2%"
    (list-database (problem-state.idb (solution.goal soln)))))


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
                  (hash-table-size *closed*))))
    (format t "~%frontier nodes: ~:D"
            (if (> *threads* 0)
                (total-parallel-frontier)
                (ecase *algorithm*
                  (depth-first (hs::length-hstack *open*))
                  (backtracking (length *choice-stack*)))))
    (format t "~%net average branching factor = ~:D" (round *average-branching-factor*))
    (when (zerop *threads*)
      (iter (while (and *rem-init-successors*
                        (not (idb-in-open (node.state (first *rem-init-successors*))
                                          *open*))))
            (pop-global *rem-init-successors*))
      (format t "~%current progress: in #~:D of ~:D initial branches"
              (the fixnum (- *num-init-successors*
                             (length *rem-init-successors*)))
              *num-init-successors*))
    (format t "~%average search depth = ~A"
            (if (> *num-paths* 0)
                (round (/ *accumulated-depths* *num-paths*))
                'pending))
    (format t "~%current average processing speed = ~:D states/sec" 
            (round (/ (the fixnum (- *total-states-processed* *prior-total-states-processed*))
                      (/ (- (get-internal-real-time) *prior-time*)
                         internal-time-units-per-second))))
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
