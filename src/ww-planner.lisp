;;; Filename: ww-planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


(defun record-move (state)
  "Returns some user-friendly representation of the move from the parent state
   to the current state."
  (declare (type problem-state state))  ; (ignore parent))
  (list (problem-state.time state)
        (cons (problem-state.name state)
              (problem-state.instantiations state))))


(defun do-init-action-updates (state)
  "Checks precondition of each init-action and applies updates.
   For backtracking algorithm, uses incremental updates within each assert."
  (declare (type problem-state state))
  (when *init-actions*
    (format t "~&Adding init-action propositions to initial database...~%"))
  (iter (for init-action in *init-actions*)
    (with-slots (name precondition-params precondition-args
                 precondition-lambda effect-lambda)
        init-action
      (format t "~&~A...~%" name)
      (let ((pre-fn (compile nil precondition-lambda))
            (eff-fn (compile nil effect-lambda))
            pre-results)
        (setf pre-results
             (remove-if #'null (mapcar (lambda (pinsts)
                                         (apply pre-fn state pinsts))
                                       precondition-args)))
        (when (null pre-results)
          (next-iteration))
        
        ;; Process each precondition result
        (dolist (pre-result pre-results)
          (let ((updated-dbs
                  (if (eql pre-result t)
                      (funcall eff-fn state)
                      (apply eff-fn state pre-result))))
            
            (dolist (updated-db updated-dbs)
              (let ((changes (update.changes updated-db)))
                (etypecase changes
                  (hash-table
                   ;; Depth-first algorithm: changes contains integer keys → values
                   (maphash (lambda (key val)
                              (let ((proposition (convert-to-proposition key)))
                                (if (gethash (car proposition) *relations*)
                                  (progn
                                    (setf (gethash proposition *db*) val)
                                    (setf (gethash key (problem-state.idb state)) val))
                                  (progn
                                    (setf (gethash proposition *static-db*) val)
                                    (setf (gethash key (problem-state.idb state)) val)))))
                            changes))
                  (list
                   ;; Backtracking algorithm: changes contains (forward-list inverse-list)
                   ;; Apply forward operations sequentially to state database
                   (let ((forward-list (first changes)))
                     (revise (problem-state.idb state) forward-list)
                     ;; Update global databases using proper update mechanism
                     ;; This correctly handles fluent extraction and storage
                     (dolist (forward-op forward-list)
                       (let ((proposition forward-op))
                         ;; Check for negation wrapper
                         (when (and (listp proposition) (eql (car proposition) 'not))
                           (setf proposition (second proposition)))
                         ;; Use update instead of direct setf/gethash
                         ;; This ensures fluents are extracted and stored correctly
                         (if (gethash (car proposition) *relations*)
                           (update *db* forward-op)
                           (update *static-db* forward-op))))
                     )))))))))))


;(defun order-propositions (updated-db)
;  "NOTs first so addhash db not removed by later remhash."
;  (ut::sortf (update.changes updated-db)
;    #'(lambda (x y) 
;        (declare (ignore y))
;        (and (listp x) (eql (car x) 'not))))
;  updated-db)


(defun generate-children (current-node)
  "Returns the legitimate children of a state. Checks precondition of each action,
   and if true, then updates db according to action effects."
  (declare (type node current-node))
  (let ((actions *actions*)
        (state (node.state current-node))
        children)
    (when (and (eql *problem-type* 'csp) (< (node.depth current-node) (length *actions*)))
      (setf actions (list (nth (node.depth current-node) *actions*))))
    (iter (for action in actions)
      (with-slots (name pre-defun-name eff-defun-name  ;iprecondition
                       precondition-params precondition-variables
                   dynamic precondition-args)  ; ieffect)
                  action
        #+:ww-debug (when (>= *debug* 4)
                      (format t "~%~A" name))
        (when dynamic  ;holds the insts with query calls
          (unless (setf precondition-args  ;overrides previous arguments list if dynamic
                    (remove-if (lambda (sublist)
                                 (or (null sublist) (member nil sublist)))
                               (eval-instantiated-spec dynamic state)))
            (next-iteration)))
        ;; Filter symmetric instantiations if symmetry pruning enabled
        (when *symmetry-pruning*
          (setf precondition-args 
                (filter-symmetric-instantiations action precondition-args state)))
        (let (pre-results updated-dbs)
          (setf pre-results  ;process this action, collecting all ? and $ vars
            (remove-if #'null (mapcar (lambda (pinsts)  ;nil = failed precondition
                                        (apply pre-defun-name  ;iprecondition
                                               state pinsts))
                                      precondition-args)))
          #+:ww-debug (when (>= *debug* 5)
                               (let ((*package* (find-package :ww)))
                                 (ut::prt precondition-variables precondition-args pre-results)))
          (when (null pre-results)
            #+:ww-debug (when (>= *debug* 5)
                          (terpri))
            (next-iteration))
          #+:ww-debug (when (and *trace-action-name* (eq name *trace-action-name*))
                        (handle-trace-interception state action pre-results precondition-variables))
          (setf updated-dbs
            (mapcan (lambda (pre-result)
                      (if (eql pre-result t)
                        (funcall eff-defun-name state)
                        (apply eff-defun-name state pre-result)))
                    pre-results))
          #+:ww-debug (when (>= *debug* 4)
                        (let ((*package* (find-package :ww)))
                          (format t "  UPDATED-DBS/~D =>~%" (length updated-dbs))
                          (iter (for updated-db in updated-dbs)
                                (for pre-result in pre-results)
                                (format t "~A~%~A,~A~2%"
                                        (format-action-with-effect-order action pre-result updated-db)
                                        ;; For backtracking, update.changes is now (forward-list inverse-list), not a hash-table
                                        (or (etypecase (update.changes updated-db)
                                              (hash-table (list-database (update.changes updated-db)))
                                              (list (first (update.changes updated-db))))  ; Show forward operations
                                            nil)
                                        ;(or (list-database (update.changes updated-db)) nil)  ;old
                                        (update.value updated-db)))
                          (terpri)))
          ;; Filter out inconsistent updates before creating states
          (let ((original-count (length updated-dbs)))
            (setf updated-dbs (remove-if #'update-is-inconsistent updated-dbs))
            (when (< (length updated-dbs) original-count)
              (increment-global *inconsistent-states-dropped* (- original-count (length updated-dbs)))))
          ;; If all updates were inconsistent, skip to next action
          (when (null updated-dbs)
            #+:ww-debug (when (>= *debug* 4)
                          (next-iteration)))
          (when *troubleshoot-current-node*  ;signaled in process-ieffect below
             (return-from generate-children))
          (let ((child-states (case *algorithm*
                                (depth-first (get-new-states state action updated-dbs))  ;return new states
                                (backtracking updated-dbs))))  ;return update structures
            ;; Apply heuristics only for depth-first (complete states)
            (when (and (eql *algorithm* 'depth-first) (fboundp 'heuristic?))
              (dolist (child-state child-states)
                (setf (problem-state.heuristic child-state)
                (funcall (symbol-function 'heuristic?) child-state))))
            (alexandria:appendf children child-states)))))
    (nreverse children)))  ;put first action child states first


(defun update-is-inconsistent (updated-db)
  "Returns T if update contains the inconsistent-state marker.
   Handles both depth-first (hash-table) and backtracking (list) representations."
  (declare (type update updated-db))
  (let ((changes (update.changes updated-db)))
    (etypecase changes
      (hash-table
       ;; Depth-first algorithm: changes contains integer keys → values
       (gethash (convert-to-integer-memoized '(inconsistent-state)) changes))
      (list
       ;; Backtracking algorithm: changes now contains (forward-list inverse-list)
       ;; Check forward-list for inconsistent-state marker
       (let ((forward-list (first changes)))
         (some (lambda (forward-op)
                 (and forward-op
                      (listp forward-op)
                      (eql (car forward-op) 'inconsistent-state)
                      (null (cdr forward-op))))
               forward-list))))))


(defun format-action-with-effect-order (action pre-result updated-db)
  "Returns action name consed with instantiation values in effect-variables order."
  (let* ((action-name (action.name action))
         (effect-vars (action.effect-variables action))
         (precond-vars (action.precondition-variables action))
         ;; Create mapping from variable names to values
         (var-value-alist (pairlis precond-vars pre-result))
         ;; Extract effect values in effect-variables order
         (effect-values 
           (mapcar (lambda (var)
                     (cond 
                       ;; For precondition variables (starting with ?)
                       ((char= (char (symbol-name var) 0) #\?)
                        (cdr (assoc var var-value-alist)))
                       ;; For effect-computed variables (starting with $)
                       ((char= (char (symbol-name var) 0) #\$)
                        (if (update.instantiations updated-db)
                            (nth (position var effect-vars) 
                                 (update.instantiations updated-db))
                            var))
                       (t var)))
                   effect-vars)))
    (cons action-name effect-values)))


(defun get-new-states (state action updated-dbs)
  "Creates new states given current state and the new updates.
   For strategic-wait: skips amend-happenings since simulation already processed them."
  (mapcan
      (lambda (updated-db)  ;process one update structure
        (let ((act-state (initialize-act-state action state updated-db))  ;act-state from action = state+
              net-state new-state)
          (declare (ignorable net-state))
          (when act-state  ;no new act-state if wait action was cancelled
            (cond
              ;; Strategic-wait: happenings already processed by simulation
              ((eql (action.name action) 'strategic-wait)
               (setf net-state act-state)
               (if (and (boundp 'constraint-fn)
                        (symbol-value 'constraint-fn) 
                        (not (funcall (symbol-function 'constraint-fn) act-state)))
                   (setf new-state nil)
                   (setf new-state act-state)))
              ;; Normal actions with happenings
              (*happening-names*  ;note that act-state = state+
               (ut::mvs (net-state new-state) (amend-happenings state act-state)))  ;check for violation
              ;; Normal actions without happenings
              (t
               (setf net-state act-state)
               (if (and (boundp 'constraint-fn)
                        (symbol-value 'constraint-fn) 
                        (not (funcall (symbol-function 'constraint-fn) act-state))) ;violated
                   (setf new-state nil)
                   (setf new-state act-state))))
            #+:ww-debug (when (>= *debug* 4)
                          (if net-state
                            (when (and (boundp 'constraint-fn) (symbol-value 'constraint-fn))
                              (format t "~&  ***NO CONSTRAINT VIOLATION~%"))
                            (when (and (boundp 'constraint-fn) (symbol-value 'constraint-fn))
                              (format t "~&  ***CONSTRAINT VIOLATED~%"))))
            (when new-state
              (list (setf new-state 
                          (process-followups net-state updated-db)))))))
      updated-dbs))


(defun initialize-act-state (action state updated-db)
  "Returns a new child of state incorporating action updated-db list,
   or nil if repeating previous wait action."
  (declare (type action action) (type problem-state state) (type update updated-db))
  (unless (and *happening-names*
               (eql (action.name action) 'wait)
               (eql (problem-state.name state) 'wait))  ;previous state is also wait
    (create-action-state action state updated-db)))  ;create non-wait state


(defun create-action-state (action state updated-db)  ;if action is non-wait
  "Creates a new wait or non-wait state.
   For strategic-wait: extracts time and happenings from sim-state in update."
  (let* ((new-state-idb (update.changes updated-db))
         (new-state-instantiations (cond 
                                     ((eql (action.name action) 'wait)
                                      (list (- (get-next-event-time state) 
                                               (problem-state.time state))))
                                     (t (update.instantiations updated-db))))
         ;; For strategic-wait: get sim-state from update structure
         (sim-state (when (eql (action.name action) 'strategic-wait)
                      (update.sim-state updated-db)))
         (new-action-duration (cond
                                ((eql (action.name action) 'wait)
                                 (- (get-next-event-time state) 
                                    (problem-state.time state)))
                                ((eql (action.name action) 'strategic-wait)
                                 (- (problem-state.time sim-state)
                                    (problem-state.time state))) 
                                (t (action.duration action))))
         (new-happenings (when (eql (action.name action) 'strategic-wait)
                           (problem-state.happenings sim-state))))
    (when (eql (problem-state.name state) 'wait)
      (remhash (gethash 'waiting *constant-integers*) new-state-idb))  ;if prior was wait
    (make-problem-state
       :name (action.name action)
       :instantiations new-state-instantiations
       :happenings new-happenings  ;nil for normal actions, sim-state happenings for strategic-wait
       :time (+ (problem-state.time state) new-action-duration)
       :value (update.value updated-db)
       :idb new-state-idb)))


(defun get-wait-happenings (state)
  (iter (for (object (index time direction)) in (problem-state.happenings state))
    (for event in (problem-state.happenings state))
    (for ref-time = (car (aref (get object :events) index)))
    (if (<= time (problem-state.time state))
      (collect (get-following-happening state object index time direction ref-time))
      (collect event))))

           
(defun get-next-event-time (state)
  "Returns the time of the next happening event, considering all objects."
  (declare (type problem-state state))
  (iter (for (nil (nil time nil)) in (problem-state.happenings state))
    (minimizing time)))


(defun process-followups (net-state updated-db)
  "Triggering forms are saved previously during effect apply.
   Followups execute on post-happening state, seeing cumulative changes."
  (declare (ignorable updated-db))
  (iter (with state+ = (copy-problem-state-without-idb net-state))
        (initially (setf (problem-state.idb state+) (problem-state.idb net-state)))
        (for followup in (update.followups updated-db))
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt followup))
        (apply (car followup) state+ (cdr followup))
        (for updated-idb = (problem-state.idb state+))
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt (list-database updated-idb)))
        (setf (problem-state.idb net-state) updated-idb)
    (finally (setf (problem-state.idb-hash net-state) nil)
             (return-from process-followups net-state))))


(defun expand (current-node)  ;called from df-bnb1
  "Returns the new states."
  (declare (type node current-node))   
  (unless (and (fboundp 'prune?) (funcall (symbol-function 'prune?) (node.state current-node))) ;don't expand state further if bounded 
    (generate-children current-node)))


(defun estimate-to-goal (state)
  "Heuristic (h) for estimating distance to a goal state from this state;
   Return 0 to use no heuristic."
  (declare (type problem-state state) (ignorable state))
  0)
