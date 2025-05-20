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

#|
(defun protect (fn &rest args)
  "If multi-threading, protect a function with a lock."
  (if (> *threads* 0)
    (bt:with-lock-held (*lock*)
      (apply fn args)
      (finish-output))
    (apply fn args)))
|#


#+sbcl
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
  (declare (optimize (debug 0)))
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


#+sbcl
(define-global *closed* (make-hash-table)  ;initialized in dfs
  "Contains the set of closed state idbs for graph search, idb -> (depth time value).")
#-sbcl
(define-global *closed* (genhash:make-generic-hash-table)  ;initialized in dfs
  "Contains the set of closed state idbs for graph search, idb -> (depth time value).")


(defun node.state.idb (node)
  "Gets the idb of a node."
  (problem-state.idb (node.state node)))


(defun choose-ht-value-test (relations)
  "Chooses either #'equal or #'equalp as a test for *closed* ht (idb) keys."
  (let (lisp-$types)  ;eg, $list, $hash-table, $fixnum, $real
    (maphash (lambda (rel args)
               (declare (ignore rel))
               (iter (for arg in args)
                     (when ($varp arg)
                       (let ((lisp-$type (trim-1st-char arg)))
                         (unless (gethash lisp-$type *types*)  ;user defined type
                           (pushnew lisp-$type lisp-$types))))))  ;should be a lisp type
             relations)
    (cond ((intersection '(hash-table vector array) lisp-$types) #'equalp)  ;for complex containers
          (t #'equal))))  ;remember ht-values are always lists of items, not atoms


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


#+sbcl
(sb-ext:define-hash-table-test fixed-keys-ht-equal fixed-keys-ht-hash)
#-sbcl
(progn (genhash:hashrem 'fixed-keys-ht-equal genhash::*hash-test-designator-map*)
       (genhash:register-test-designator 'fixed-keys-ht-equal #'fixed-keys-ht-hash #'fixed-keys-ht-equal))


(defun fixedp (relations)
  "Determines if all relations have $var args, and thus have fixed keys idb."
  (maphash (lambda (rel args)
             (declare (ignore rel))
             (unless (member-if #'$varp args)
               (return-from fixedp nil)))
           relations)
  t)


;;; Search Functions


(defun dfs ()
  "Main search program."
  (when *global-invariants*
    (unless (validate-global-invariants *start-state* t)
      (format t "~%Invariant validation failed on initial state.~%")
      (return-from dfs nil)))
  (when (fboundp 'bounding-function?)
    (setf *upper-bound*
          (funcall (symbol-function 'bounding-function?) *start-state*)))
  (let ((fixed-idb nil)  ;(fixedp *relations*))
        (parallelp (> *threads* 0)))
    (declare (ignorable parallelp))
    (setf *open* 
      (hs::make-hstack :table 
                         #+sbcl (make-hash-table :test 'equalp  ;(if fixed-idb 'fixed-keys-ht-equal 'equalp)
                                                 :synchronized parallelp)
                         #-sbcl (genhash:make-generic-hash-table :test 'equalp)  ;(if fixed-idb 'fixed-keys-ht-equal 'equalp))
                       :keyfn #'node.state.idb))
    (when (eql *tree-or-graph* 'graph)
      (setf *closed* 
        #+sbcl (make-hash-table :test 'equalp  ;(if fixed-idb 'fixed-keys-ht-equal 'equalp)
                                :size 100000
                                :synchronized parallelp)
        #-sbcl (genhash:make-generic-hash-table :test 'equalp  ;(if fixed-idb 'fixed-keys-ht-equal 'equalp)
                                                :size 100000))))
  (hs::push-hstack (make-node :state (copy-problem-state *start-state*)) *open* :new-only (eq *tree-or-graph* 'graph))
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
  (setf *unique-solutions* nil)
  (setf *best-states* (list *start-state*))
  (setf *solution-count* 0)
  (setf *upper-bound* 1000000)
  (setf *search-tree* nil)
  (setf *start-time* (get-internal-real-time))
  (setf *prior-time* 0)
  #+sbcl (if (> *threads* 0)
           ;(with-open-stream (*standard-output* (make-broadcast-stream))) ;ignore *standard-output*
           (process-threads)
           (search-serial))
  #-sbcl (search-serial)
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
    (print-search-progress *open*)  ;#nodes expanded so far
    (after-each #+:ww-debug (when (>= *debug* 5) (simple-break)))))  ;simplifies debugger printout


(defun df-bnb1 (open)
  "Performs expansion of one node from open. Returns
   new successor nodes, (first), or nil if no new nodes generated."
  (declare (type hs::hstack open))
  (let ((current-node (get-next-node-for-expansion open)))  ;pop next node
   (when *probe*
     (apply #'probe current-node *probe*))
   (iter
    (when (>= *debug* 3)
      (format t "~&-----------------------------------------------------------~%")
      (format t "~%Current node selected:~%~S~2%" current-node))
    (when (null current-node)  ;open is empty
      (return-from df-bnb1 nil))
    (when (and (> *depth-cutoff* 0) (= (node.depth current-node) *depth-cutoff*))
      (narrate "State at max depth" (node.state current-node) (node.depth current-node))
      (return-from df-bnb1 nil))
    (when (eql (bounding-function current-node) 'kill-node)
      (return-from df-bnb1 nil))
    (when (eql *tree-or-graph* 'graph)
      #+sbcl (setf (gethash (problem-state.idb (node.state current-node)) *closed*)
               (list (node.depth current-node)
                     (problem-state.time (node.state current-node))
                     (problem-state.value (node.state current-node))))
      #-sbcl (setf (genhash:hashref (problem-state.idb (node.state current-node)) *closed*)
               (list (node.depth current-node)
                     (problem-state.time (node.state current-node))
                     (problem-state.value (node.state current-node)))))
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
      (when (= *debug* 6) (simple-break))  ;probe found
      (return-from df-bnb1 (process-successors succ-states current-node open))))))  ;returns live successor nodes


(defun process-successors (succ-states current-node open)    ;(ut::print-ght-keys (hs::hstack.table open)) (print 'successors)
  (iter (with succ-depth = (1+ (node.depth current-node)))
        (for succ-state in succ-states)
        (when *global-invariants*
          (validate-global-invariants succ-state))
        (when (and *solutions* (member *solution-type* '(min-length min-time min-value max-value)))
          (unless (f-value-better succ-state succ-depth)
            (next-iteration)))  ;throw out state if can't better best solution so far
        (when (goal succ-state)
          (register-solution current-node succ-state)
          (finalize-path-depth succ-depth)
          (if (eql *solution-type* 'first)
            (return-from process-successors '(first))  ; Return immediately after first solution found
            (next-iteration)))
        (unless (boundp 'goal-fn)  ;looking for best results rather than goals
          (process-min-max-value succ-state))  ;only if not associated with a goal
        (when (and (eql *tree-or-graph* 'tree) (eql *problem-type* 'planning))  ;not for csp problems
          (when (on-current-path succ-state current-node)  ;stop infinite loop
            (increment-global *repeated-states*)
            (finalize-path-depth succ-depth)
            (next-iteration)))
        (when (eql *tree-or-graph* 'graph)   ;(ut::print-ht (problem-state.idb succ-state))
          (let ((open-node (idb-in-open (problem-state.idb succ-state) open)))
            (when open-node
              (narrate "State already on open" succ-state succ-depth)
              (increment-global *repeated-states*)
             (if (update-open-if-succ-better open-node succ-state)
               (setf (node.parent open-node) current-node)  ;succ is better
               (finalize-path-depth succ-depth))  ;succ is not better
             (next-iteration)))  ;drop this succ
          (let* ((succ-idb (problem-state.idb succ-state))
                 (closed-values (get-closed-values succ-idb)))
            (when closed-values
              (narrate "State previously closed" succ-state succ-depth)
              (increment-global *repeated-states*)
              (if (better-than-closed closed-values succ-state succ-depth)  ;succ has better value
                #+sbcl (remhash succ-idb *closed*)  ;then reactivate on open below
                #-sbcl (genhash:hashrem succ-idb *closed*)
                (progn (finalize-path-depth succ-depth) (next-iteration))))))  ;drop this succ
        (collecting (generate-new-node current-node succ-state))))  ;live successor


(defun validate-global-invariants (state &optional (is-start-state nil))
  "Validate all registered global invariants on the given state.
   Returns T if all invariants pass, NIL if any fail."
  (loop for invariant-name in *global-invariants*
        for fn = (symbol-function invariant-name)
        unless (funcall fn state)
        do (troubleshoot "Invariant ~A failed on ~A:~%~A" 
                        invariant-name
                        (if is-start-state 
                            "*START-STATE*" 
                            "state during planning")
                        (list-database (problem-state.idb state)))
           (return-from validate-global-invariants nil))
  t)


(defun idb-in-open (succ-idb open)
  "Determines if an idb hash table's contents match the contents of a key in open's table.
   Returns the node in open or nil."
   (declare (type hash-table succ-idb))
  #+sbcl (let ((ht (hs::hstack.table open)))
           (block equality-keys
             (maphash (lambda (open-idb nodes)  ;nodes should contain only one node
                        ;(unless (hash-table-p open-idb) (bt:with-lock-held (*lock*) (ut::print-ht ht) (terpri)))
                        (declare (type hash-table open-idb) (type cons nodes))
                        (when (funcall (hash-table-test ht) succ-idb open-idb)
                          (return-from equality-keys (car nodes))))
                      ht)))
  #-sbcl (let ((ght (hs::hstack.table open)))
           (block equalp-keys
             (genhash:hashmap (lambda (open-idb nodes)
                                (when (equalp succ-idb open-idb)  ;note does not ever test with fixed-keys-ht-equal
                                  (return-from equalp-keys (car nodes))))
                              ght))))


(defun get-closed-values (idb)
  #+sbcl (let ((ht *closed*))
           (block equality-keys
             (maphash (lambda (key value)
                        (when (funcall (hash-table-test ht) idb key)
                          (return-from equality-keys value)))
                      ht)))
  #-sbcl (let ((ght *closed*))
           (block equalp-keys
             (genhash:hashmap (lambda (key value)
                                (when (equalp idb key)  ;note does not ever test with fixed-keys-ht-equal
                                  (return-from equalp-keys value)))
                              ght))))


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
                   #+sbcl (bt:with-lock-held (*lock*)
                            (format t "~%Higher value state found: ~A in thread ~D~%"
                                      (problem-state.value succ-state) (lparallel:kernel-worker-index))
                            (finish-output))
                   #-sbcl (format t "~%Higher value state found: ~A~%"
                                    (problem-state.value succ-state))
                   (push-global succ-state *best-states*)))
      (min-value (when (< current-value best-value)
                   #+sbcl (bt:with-lock-held (*lock*)
                            (format t "~%Lower value state found: ~A in thread ~D~%"
                                      (problem-state.value succ-state) (lparallel:kernel-worker-index))
                            (finish-output))
                   #-sbcl (format t "~%Lower value state found: ~A~%"
                                    (problem-state.value succ-state))
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
  "Determines if f-value of successor is better than closed state."
  (ecase *solution-type*
    ((min-length first every)
       (< succ-depth (first closed-values)))
    (min-time
       (< (problem-state.time succ-state) (second closed-values)))
    (min-value
       (< (problem-state.value succ-state) (third closed-values)))
    (max-value
       (> (problem-state.value succ-state) (third closed-values)))))


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
                #+sbcl (bt:with-lock-held (*lock*)
                         (format t "bounding a state...")
                         (finish-output))
                #-sbcl (format t "bounding a state...")
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
  "Determines if a successor is already on the current path from the start state."
  (when (iter (for node initially current-node then (node.parent node))
              (while node)  
              (thereis (equalp (problem-state.idb succ-state) (problem-state.idb (node.state node)))))
    (narrate "State already on current path" succ-state (1+ (node.depth current-node)))
    t))


(defun narrate (string state depth)
  (declare (ignorable string state depth))
  (when (>= *debug* 3)
    (format t "~%~A:~%~A~%" string state))
  #+:ww-debug (when (>= *debug* 1)
                (update-search-tree state depth string))
  nil)


(defun generate-new-node (current-node succ-state)
  "Produces a new node for a given successor."
  (declare (type node current-node) (type problem-state succ-state))
  (let* ((depth (1+ (node.depth current-node)))
         (succ-node (make-node :state succ-state
                               :depth depth
                               :parent current-node)))
    (when (>= *debug* 3)
      (format t "~%Installing new or updated successor:~%~S~%" succ-node))
    succ-node))


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

  
(defun summarize-search-results (condition)
  (declare (type symbol condition))
  (format t "~2%In problem ~A, performed ~A search for ~A solution."
            *problem-name* *tree-or-graph* *solution-type*)
  (ecase condition
    (first
      (when *solutions*
        (format t "~2%Search ended with first solution found." )))
    (exhausted
      (format t "~2%Search process completed normally.")
      (when (eql *solution-type* 'every)
        (if (eql *tree-or-graph* 'tree)
          (format t "~2%Exhaustive search for every solution (up to the depth cutoff, if any).")
          (format t "~2%Exhaustive search for every solution (except solutions in pruned branches).")))))
  (format t "~2%Depth cutoff = ~:D" *depth-cutoff*)
  (format t "~2%Maximum depth explored = ~:D" *max-depth-explored*)
  (format t "~2%Program cycles = ~:D" *program-cycles*)
  (format t "~2%Total states processed = ~:D" *total-states-processed*)
  (when (eql *tree-or-graph* 'graph)
    (format t "~2%Repeated states = ~:D, ie, ~,1F percent"
              *repeated-states* (* (/ *repeated-states* *total-states-processed*) 100)))
  ;(format t "~2%Unique states encountered = ~:D" (unique-states-encountered-graph))
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
          (cond ((equalp shallowest-depth-solution minimum-time-solution)
                   (format t "~%A shortest path solution is also a minimum duration solution.~2%"))
                (t (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
                   (format t "~2%A minimum time solution path from start state to goal state:~%")
                   (printout-solution minimum-time-solution)))))))
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
             #+(and ww-debug sbcl) (when (>= *debug* 1)
                                       (lprt))
             (let ((ctrl-str "~&New path to goal found at depth = ~:D~%"))
               #+sbcl (bt:with-lock-held (*lock*)
                        (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                          (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                                    state-depth (solution.value solution))
                          (format t ctrl-str state-depth)))
               #-sbcl (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                        (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                                  state-depth (solution.value solution))
                        (format t ctrl-str state-depth))))
          (t (format t "~%New path to goal found at depth = ~:D~%" state-depth)
             (when (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
               (format t "Objective value = ~:A~%" (solution.value solution)))
             (when (eql *solution-type* 'min-time)
               (format t "Time = ~:A~%" (solution.time solution)))))
    (narrate "Solution found ***" goal-state state-depth)
    (push-global solution *solutions*)
    (when (not (member (problem-state.idb (solution.goal solution)) *unique-solutions* 
                       :key (lambda (soln) (problem-state.idb (solution.goal soln)))
                       :test #'equalp))
      (push-global solution *unique-solutions*))))


(defun printout-solution (soln)
  (declare (type solution soln))
  (dolist (item (solution.path soln))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~2%"
    (list-database (problem-state.idb (solution.goal soln)))))


(defun print-search-progress (open)
  #+sbcl (bt:with-lock-held (*lock*)
           (printout-search-progress open))
  #-sbcl (printout-search-progress open))


(defun printout-search-progress (open)
  "Printout of nodes expanded so far during search modulo reporting interval."
    (when (<= (- *progress-reporting-interval* (- *total-states-processed* *prior-total-states-processed*))
              0)
        (format t "~2%program cycles = ~:D" *program-cycles*)
        (format t "~%total states processed so far = ~:D" *total-states-processed*)
        (when (eql *tree-or-graph* 'graph)
          (format t "~%ht count: ~:D    ht size: ~:D"
                  #+sbcl (hash-table-count *closed*)
                  #-sbcl (genhash:generic-hash-table-count *closed*)
                  #+sbcl (hash-table-size *closed*)
                  #-sbcl (genhash:generic-hash-table-size *closed*)))
        (format t "~%net average branching factor = ~:D" (round *average-branching-factor*))
        (iter (while (and *rem-init-successors*
                          (not (idb-in-open (problem-state.idb (node.state (first *rem-init-successors*))) open))))
              (pop-global *rem-init-successors*))
        (when (< *threads* 2)
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
  #+sbcl (time (dfs))
  #-sbcl (dfs)  ;(the-cost-of-nothing:bench (dfs))
  (in-package :ww))
