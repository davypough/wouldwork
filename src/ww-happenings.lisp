
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
              (setf following-happening (apply-rebound following-happening))))
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
