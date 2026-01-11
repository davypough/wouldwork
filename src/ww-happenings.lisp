
;;; Filename: ww-happenings.lisp

;;; Functions for processing happening events in planning.


(in-package :ww)


(defun amend-happenings (state act-state)
  "Creates hap-state with happenings up through the action completion time for all happenings.
   Returns net-state = act-state + hap-state, and checks for constraint violation."
  (declare (type problem-state state act-state))
  (let ((hap-state (copy-problem-state state))  ;to be updated
        (next-happenings (copy-tree (problem-state.happenings state))) ;old next happenings
        (action-completion-time (problem-state.time act-state))
        next-happening following-happenings)  ;collect next happenings for net-state
    #+:ww-debug (when (>= *debug* 4)
                         (ut::prt action-completion-time))
    (setf (problem-state.time hap-state) action-completion-time)
    ;; Process each happening event up to action completion time
    (iter (while (setq next-happening (pop next-happenings)))
          (for (object (index time direction)) = next-happening)
          (when (> time action-completion-time)
            (push next-happening following-happenings)  ;keep same happening
            (next-iteration))
          (for (ref-time . hap-updates) = (aref (get object :events) index))
          (for following-happening = (get-following-happening act-state object index time direction ref-time))  ;not state
          #+:ww-debug (when (>= *debug* 4)
                               (ut::prt following-happening))
          (when (null following-happening)
            (next-iteration))
          ;; Apply happening updates to BOTH hidb and hap-state
          (when (/= (first (second following-happening)) index)  ;happening is not interrupted
            (revise (problem-state.hidb act-state) hap-updates)
            (revise (problem-state.idb act-state) hap-updates)
            (revise (problem-state.idb hap-state) hap-updates)  ;hap-state = state + updates
            (revise (problem-state.hidb hap-state) hap-updates)
            #+:ww-debug (when (>= *debug* 4)
                                 (ut::prt hap-updates))
            ;; Check for rebound after updates applied (reactive model)
            (when (rebound-condition object hap-state)
              (setf following-happening (apply-rebound following-happening)))
            ;; Apply aftereffect updates if defined (eg, relocate objects on patroller)
            (apply-aftereffect object hap-state)
            (apply-aftereffect object act-state))
          ;; Check for kill condition after updates applied (prunes state)
          ;; Use act-state which has agent's current location post-action
          (when (kill-condition object act-state)
            (return-from amend-happenings (values nil nil)))
          (push following-happening next-happenings)) ;keep looking until past action-completion-time
    (let ((net-state (copy-problem-state act-state))) ;add happenings to hap-state & net-state
      (maphash (lambda (key value)  ;merge hidb into idb
                 (setf (gethash key (problem-state.idb net-state))
                   value))
               (problem-state.hidb act-state))
      (setf (problem-state.happenings act-state) following-happenings)
      (setf (problem-state.happenings hap-state) following-happenings)
      (setf (problem-state.happenings net-state) following-happenings)
      #+:ww-debug (when (>= *debug* 4)
                           (ut::prt act-state hap-state net-state))
      (if (and (boundp 'constraint-fn)
               (constraint-violated-in-act-hap-net act-state hap-state net-state))
        (values nil nil)
        (values net-state act-state)))))  ;act-state is final state


(defun get-following-happening (act-state object index time direction ref-time)  ;not state
  "Derive the following happening update for an object."
  (let* ((events (get object :events))
         (n (1- (length events)))
         following-index following-time)
    (cond ((= index n)  ;at last index
             (when (null (get object :repeat))
               (return-from get-following-happening nil))
             (setf following-time (+ time (first (aref events 0))))
             (setf following-index 0))  ;setup for next update
          (t (setf following-time (+ time (- (first (aref events (1+ index))) ref-time)))
             (setf following-index (1+ index))))
    (if (interrupt-condition object act-state)  ;interrupted object results in no updates except time
      `(,object (,index ,following-time ,direction))
      `(,object (,following-index ,following-time ,direction)))))


(defun interrupt-condition (object act-state)
  "Determines if the interrupt function for object is satisfied in this state;
   eg, if the object is currently being jammed, and therefore disabled."
  (declare (type symbol object) (type problem-state act-state))
  (ut::if-it (get object :interrupt)
             (funcall ut::it act-state)
             nil))


(defun rebound-condition (object new-state)
  "Determines if a rebound condition is satisfied in this state."
  (declare (type symbol object) (type problem-state new-state))
  (ut::if-it (get object :rebound)
             (funcall ut::it new-state)))


(defun kill-condition (object state)
  "Determines if the kill condition for object is satisfied in this state;
   eg, if a mine has moved into the agent's location, killing them."
  (declare (type symbol object) (type problem-state state))
  (ut::if-it (get object :kill)
             (funcall ut::it state)
             nil))


(defun apply-aftereffect (object state)
  "Applies the aftereffect update for object to state if defined.
   Unlike condition functions, this executes an update rather than testing a predicate.
   Used to propagate consequences after a patroller moves (eg, relocating cargo on top)."
  (declare (type symbol object) (type problem-state state))
  (ut::if-it (get object :aftereffect)
             (let ((updated-dbs (funcall ut::it state)))
               (dolist (update updated-dbs)
                 (when update
                   (let ((changes (update.changes update)))
                     (when (hash-table-p changes)
                       (maphash (lambda (key value)
                                  (setf (gethash key (problem-state.idb state)) value))
                                changes))))))))


(defun apply-rebound (following-happening)
  "Compute mirror index for rebound. For :reverse mode patrollers,
   reverses direction by jumping to the mirror event in the events array.
   Mirror formula: for events array of length N, mirror of index I is (N - I)."
  (let* ((object (first following-happening))
         (params (second following-happening))
         (following-index (first params))
         (following-time (second params))
         (direction (third params))
         (events (get object :events))
         (n-events (length events))
         (mirror-index (mod (- n-events following-index) n-events)))
    (if (eq (get object :patroller-mode) :reverse)
        `(,object (,mirror-index ,following-time ,(- direction)))
        following-happening)))
 

(defun constraint-violated-in-act-hap-net (act-state hap-state net-state)
  "Determines whether the input states violate a constraint or not."
  (declare (type problem-state act-state hap-state net-state) (ignorable act-state))
  (or ;(and (not (funcall (symbol-function '*constraint*) act-state))
      (not (funcall (symbol-function 'constraint-fn) hap-state))  ;disallow swaps
      (not (funcall (symbol-function 'constraint-fn) net-state))))


;;; ============================================================================
;;; STRATEGIC WAIT SIMULATION
;;; ============================================================================


(defun find-earliest-happening (happenings)
  "Returns the happening with the earliest time from the happenings list.
   Each happening is (object (index time direction)).
   Returns NIL if happenings is empty."
  (when happenings
    (reduce (lambda (a b)
              (if (<= (second (second a)) (second (second b)))
                  a b))
            happenings)))


(defun simulate-happenings-until-true-fn (state max-wait-time target-lambda)
  "Forward-simulates happenings until target-lambda returns true or timeout.
   Processes happenings one step at a time, checking target condition after each.
   Returns (values sim-time sim-state) if target satisfied within deadline.
   Returns (values nil nil) on timeout or kill condition.
   Used by strategic-wait action to determine if waiting would be beneficial."
  (declare (type problem-state state) (type real max-wait-time) (type function target-lambda))
  ;; Early exit if no happenings to simulate
  (unless (problem-state.happenings state)
    (return-from simulate-happenings-until-true-fn (values nil nil)))
  (let* ((sim-state (copy-problem-state state))
         (start-time (problem-state.time state))
         (deadline (+ start-time max-wait-time)))
    (block simulation
      (dotimes (iteration 100 
           (error "simulate-happenings-until-true exceeded 100 events. ~
                   Possible malformed event timing or bug in happening machinery. ~
                   Current sim-time=~A, deadline=~A, happenings=~S"
                  (problem-state.time sim-state) deadline 
                  (problem-state.happenings sim-state)))  ; safety bound, returns nil if exceeded
        (let* ((happenings (problem-state.happenings sim-state))
               (earliest (find-earliest-happening happenings)))
          ;; No happenings left to process
          (unless earliest
            (return-from simulation (values nil nil)))
          (destructuring-bind (object (index time direction)) earliest
            ;; Timeout: earliest event is past deadline
            (when (> time deadline)
              (return-from simulation (values nil nil)))
            ;; Get event data from object's events array
            (let* ((events (get object :events))
                   (event (aref events index))
                   (ref-time (first event))
                   (hap-updates (rest event))
                   (following-happening 
                     (get-following-happening sim-state object index time direction ref-time)))
              ;; Handle non-repeating object reaching end of events
              (cond
                ((null following-happening)
                 ;; Remove this object from happenings and continue
                 (setf (problem-state.happenings sim-state)
                       (remove object happenings :key #'first)))
                (t
                 ;; Process happening normally
                 ;; Apply updates only if not interrupted (index advanced)
                 (when (/= (first (second following-happening)) index)
                   ;; Apply database updates to both hidb and idb
                   (revise (problem-state.hidb sim-state) hap-updates)
                   (revise (problem-state.idb sim-state) hap-updates)
                   ;; Check and apply rebound condition
                   (when (rebound-condition object sim-state)
                     (setf following-happening (apply-rebound following-happening)))
                   ;; Apply aftereffect if defined
                   (apply-aftereffect object sim-state))
                 ;; Check kill condition - agent died during wait
                 (when (kill-condition object sim-state)
                   (return-from simulation (values nil nil)))
                 ;; Update this object's entry in happenings list
                 (setf (problem-state.happenings sim-state)
                       (mapcar (lambda (h)
                                 (if (eq (first h) object)
                                     following-happening
                                     h))
                               happenings))
                 ;; Advance simulation time to when this event fired
                 (setf (problem-state.time sim-state) time)
                 ;; Check if target condition now satisfied
                 (when (funcall target-lambda sim-state)
                   (return-from simulation 
                     (values (- (problem-state.time sim-state) start-time) sim-state))))))))))))


(defun apply-simulated-state! (state sim-state)
  "Applies the simulated state's database changes to the current state.
   Copies idb entries from sim-state to state.
   Called from strategic-wait effect to transfer simulation results.
   Returns updated-dbs list for consistency with other update functions."
  (declare (type problem-state state sim-state))
  ;; Merge sim-state.idb into state.idb
  (maphash (lambda (key value)
             (setf (gethash key (problem-state.idb state)) value))
           (problem-state.idb sim-state))
  ;; Merge sim-state.hidb into state.hidb  
  (maphash (lambda (key value)
             (setf (gethash key (problem-state.hidb state)) value))
           (problem-state.hidb sim-state))
  ;; Return empty updated-dbs (changes already applied directly)
  nil)


;;; ============================================================================
;;; AUTO-WAIT INFRASTRUCTURE
;;; ============================================================================


(defun auto-wait-enabled-p ()
  "Returns T if auto-wait mechanism should be active.
   Requires: *auto-wait* is T, *happening-names* is non-nil,
   and *tree-or-graph* is tree."
  (and *auto-wait*
       *happening-names*
       (eql *tree-or-graph* 'tree)))


(defun any-action-applicable-p (state)
  "Returns T if any non-wait action has satisfied preconditions in STATE.
   Used by auto-wait mechanism to determine if agent is 'stuck'.
   Checks each action's precondition function without executing effects.
   Skips actions named WAIT (the user-defined wait action)."
  (declare (type problem-state state))
  (dolist (action *actions*)
    (let ((name (action.name action))
          (dynamic (action.dynamic action))
          (precondition-args (action.precondition-args action))
          (pre-defun-name (action.pre-defun-name action)))
      ;; Skip user-defined wait action; process all others
      (unless (eql name 'wait)
        ;; Handle dynamic precondition arguments (require re-evaluation)
        (let ((effective-args precondition-args))
          (when dynamic
            (let ((new-args (remove-if (lambda (sublist)
                                         (or (null sublist) (member nil sublist)))
                                       (eval-instantiated-spec dynamic state))))
              (if new-args
                  (setf effective-args new-args)
                  (setf effective-args nil))))
          ;; Check if any instantiation satisfies preconditions
          (when effective-args
            (dolist (pinsts effective-args)
              (let ((result (apply pre-defun-name state pinsts)))
                (when result
                  ;; Found an applicable action
                  (return-from any-action-applicable-p t)))))))))
  ;; No action was applicable
  nil)


(defun simulate-until-action-applicable (state max-wait-time)
  "Forward-simulates happenings until some action becomes applicable or goal is reached.
   Used by auto-wait mechanism when agent is stuck (no applicable actions).
   
   Returns multiple values indicating outcome:
     :goal sim-time sim-state    - Goal was reached during simulation
     :action sim-time sim-state  - An action became applicable
     :timeout nil nil            - Max wait time exceeded
     :killed nil nil             - Agent died during wait
     :no-happenings nil nil      - No happenings to simulate
   
   The sim-state has updated idb, hidb, happenings, and time fields."
  (declare (type problem-state state) (type real max-wait-time))
  ;; Early exit if no happenings to simulate
  (unless (problem-state.happenings state)
    (return-from simulate-until-action-applicable (values :no-happenings nil nil)))
  (let* ((sim-state (copy-problem-state state))
         (start-time (problem-state.time state))
         (deadline (+ start-time max-wait-time)))
    (loop
      (let* ((happenings (problem-state.happenings sim-state))
             (earliest (find-earliest-happening happenings)))
        ;; No happenings left to process
        (unless earliest
          (return (values :no-happenings nil nil)))
        
        (destructuring-bind (object (index time direction)) earliest
          ;; Timeout: earliest event is past deadline
          (when (> time deadline)
            (return (values :timeout nil nil)))
          
          ;; Get event data from object's events array
          (let* ((events (get object :events))
                 (event (aref events index))
                 (ref-time (first event))
                 (hap-updates (rest event))
                 (following-happening 
                   (get-following-happening sim-state object index time direction ref-time)))
            
            (cond
              ;; Handle non-repeating object reaching end of events
              ((null following-happening)
               (setf (problem-state.happenings sim-state)
                     (remove object happenings :key #'first)))
              
              ;; Process happening normally
              (t
               ;; Apply updates only if not interrupted (index advanced)
               (when (/= (first (second following-happening)) index)
                 ;; Apply database updates to both hidb and idb
                 (revise (problem-state.hidb sim-state) hap-updates)
                 (revise (problem-state.idb sim-state) hap-updates)
                 ;; Check and apply rebound condition
                 (when (rebound-condition object sim-state)
                   (setf following-happening (apply-rebound following-happening)))
                 ;; Apply aftereffect if defined
                 (apply-aftereffect object sim-state))
               
               ;; Check kill condition - agent died during wait
               (when (kill-condition object sim-state)
                 (return (values :killed nil nil)))
               
               ;; Update this object's entry in happenings list
               (setf (problem-state.happenings sim-state)
                     (mapcar (lambda (h)
                               (if (eq (first h) object)
                                   following-happening
                                   h))
                             happenings))
               
               ;; Advance simulation time to when this event fired
               (setf (problem-state.time sim-state) time)
               
               ;; Check if goal is now satisfied
               (when (and (boundp 'goal-fn)
                          (funcall (symbol-function 'goal-fn) sim-state))
                 (return
                   (values :goal 
                           (- (problem-state.time sim-state) start-time) 
                           sim-state)))
               
               ;; Check if any action is now applicable
               (when (any-action-applicable-p sim-state)
                 (return
                   (values :action 
                           (- (problem-state.time sim-state) start-time) 
                           sim-state)))))))))))


(defun create-auto-wait-state (state sim-state wait-duration)
  "Creates a new problem-state representing the result of an auto-wait.
   Used by the stuck-triggered wait mechanism.
   
   STATE is the original state before waiting.
   SIM-STATE is the simulated state after happenings occurred.
   WAIT-DURATION is the elapsed time (for recording in solution path).
   
   Returns a new problem-state with:
   - name: WAIT
   - instantiations: (wait-duration) for solution path recording
   - Updated idb, hidb, happenings from sim-state
   - Updated time from sim-state"
  (declare (type problem-state state sim-state) (type real wait-duration))
  (make-problem-state
    :name 'wait
    :instantiations (list wait-duration)
    :happenings (problem-state.happenings sim-state)
    :time (problem-state.time sim-state)
    :value (problem-state.value state)  ; Preserve value from original state
    :heuristic 0.0
    :idb (problem-state.idb sim-state)
    :hidb (problem-state.hidb sim-state)
    :idb-hash nil))  ; Will be computed when needed
