;;; Filename: ww-planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


;(defun record-move (parent state)
;  "Returns some user-friendly representation of the move from the parent state
;   to the current state."
;  (declare (type problem-state state) (ignore parent))
;  (with-slots (name instantiations time) state
;    (list time (cons name instantiations))))


(defun record-move (state)
  "Returns some user-friendly representation of the move from the parent state
   to the current state."
  (declare (type problem-state state))  ; (ignore parent))
  (list (problem-state.time state)
        (cons (problem-state.name state)
              (problem-state.instantiations state))))


(defun do-init-action-updates (state)  ;add init actions to start-state
  "Checks precondition of each init-action,
   and if true, then updates db and static-db according to each init-action effect."
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
            ;(eff-fn (symbol-function (eval effect-lambda)))
            pre-results updated-dbs)
        (setf pre-results
             (remove-if #'null (mapcar (lambda (pinsts)
                                         (apply pre-fn state pinsts))
                                       precondition-args)))
        (when (null pre-results)
          (next-iteration))
        (setf updated-dbs  ;returns list of update structures
              (mapcan (lambda (pre-result)
                        (if (eql pre-result t)
                          (funcall eff-fn state)
                          (apply eff-fn state pre-result)))
                pre-results))
        (dolist (updated-db updated-dbs)  ;merge updates into *db* and *static-db*
          (maphash (lambda (key val)
                     (let ((proposition (convert-to-proposition key)))
                       (if (gethash (car proposition) *relations*)
                         (setf (gethash proposition *db*) val)
                         (setf (gethash proposition *static-db*) val))))
                   (update.changes updated-db)))))))


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
          (setf updated-dbs  ;returns list of update structures
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
                                        pre-result
                                        (or (list-database (update.changes updated-db)) nil)
                                        (update.value updated-db)))
                          (terpri)))
          (when *troubleshoot-current-node*  ;signaled in process-ieffect below
             (return-from generate-children))
          ;(let ((child-states (get-new-states state action updated-dbs)))  ;keep idb & hidb separate
          ;  (when (fboundp 'heuristic?)
          ;    (dolist (child-state child-states)
          ;      (setf (problem-state.heuristic child-state)
          ;        (funcall (symbol-function 'heuristic?) child-state))))
          ;  (alexandria:appendf children child-states)))))
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


(defun get-new-states (state action updated-dbs)
  "Creates new states given current state and the new updates."
  (mapcan
      (lambda (updated-db)  ;process one update structure
        (let ((act-state (initialize-act-state action state updated-db))  ;act-state from action = state+
              net-state new-state)
          (declare (ignorable net-state))
          (when act-state  ;no new act-state if wait action was cancelled
            (if *happening-names*  ;note that act-state = state+
              (ut::mvs (net-state new-state) (amend-happenings state act-state))  ;check for violation
              (if (and (boundp 'constraint-fn)
                       (symbol-value 'constraint-fn) 
                       (not (funcall (symbol-function 'constraint-fn) act-state))) ;violated
                (setf new-state nil)
                (setf new-state act-state)))
            #+:ww-debug (when (>= *debug* 4)
                          (if net-state
                            (when (and (boundp 'constraint-fn) (symbol-value 'constraint-fn))
                              (format t "~&  ***NO CONSTRAINT VIOLATION~%"))
                            (when (and (boundp 'constraint-fn) (symbol-value 'constraint-fn))
                              (format t "~&  ***CONSTRAINT VIOLATED~%"))))
            (when new-state
              (list (setf new-state 
                          (process-followups act-state updated-db)))))))
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
  "Creates a new wait or non-wait state."
  (let* ((new-state-idb (update.changes updated-db))
         (new-action-duration (if (eql (action.name action) 'wait)
                                (- (get-next-event-time state) (problem-state.time state))
                                (action.duration action)))
         (new-state-instantiations (if (eql (action.name action) 'wait)
                                     (list new-action-duration)
                                     (update.instantiations updated-db))))
    (when (eql (problem-state.name state) 'wait)
      (remhash (gethash 'waiting *constant-integers*) new-state-idb))  ;if prior was wait
    (make-problem-state
       :name (action.name action)
       :instantiations new-state-instantiations
       :happenings nil  ;to be updated by happenings
       :time (+ (problem-state.time state) new-action-duration)
       :value (update.value updated-db)
       :idb new-state-idb)))
;       :hidb (copy-idb (problem-state.hidb state)))))  ;to be updated by happenings


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


(defun process-followups (net-state updated-db)  ;followups for one update structure from effect
  "Triggering forms are saved previously during effect apply."
  (declare (ignorable updated-db))
  (iter (with state+ = (copy-problem-state net-state))  ;create state+ from net-state
        (setf (problem-state.idb state+) (update.changes updated-db))  ;set updated idb
        (for followup in (update.followups updated-db))
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt followup))
        (for updated-idb = (apply (car followup) state+ (cdr followup)))  ;pass state+ only
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt (list-database updated-idb)))
        (setf (problem-state.idb net-state) updated-idb)
    (finally (return-from process-followups net-state))))


#+ignore (defun process-followups (net-state updated-db)  ;followups for one update structure from effect
  "Triggering forms are saved previously during effect apply."
  (declare (ignorable updated-db))
  (iter (with idb = (update.changes updated-db))  ;post effect idb
        (for followup in (update.followups updated-db))
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt followup))
        (for updated-idb = (apply (car followup) net-state idb (cdr followup)))  ;rtn followup updated idb
        #+:ww-debug (when (>= *debug* 4)
                      (ut::prt (list-database updated-idb)))
        (setf (problem-state.idb net-state) updated-idb)
    (finally (return-from process-followups net-state))))


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
