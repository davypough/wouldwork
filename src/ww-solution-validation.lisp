;;; Filename: ww-solution-validation.lisp

;;; Solution validation capability for verifying and troubleshooting action sequences.
;;; Given a sequence of actions, executes them in order from *start-state*,
;;; reporting success, failure point, or partial completion.

(in-package :ww)


;;; ==================== Main Interface ====================


(defmacro validate-solution (&rest args)
  "Validate a sequence of actions starting from *start-state*.
   
   Usage:
     (validate-solution
       (move-to agent1 area2)
       (pickup agent1 connector1)
       (connect agent1 transmitter1 receiver3))
   
   Options (must appear first):
     :verbose - Show detailed diagnostic output for each action
   
   Returns:
     - The final state if all actions succeed and goal is satisfied
     - The intermediate state if all actions succeed but goal not satisfied
     - NIL if any action fails (with diagnostic output)
   
   Output:
     - On success: displays final state
     - On partial success: displays intermediate state
     - On failure: displays failed action, reason, and state at failure"
  (let ((verbose (eq (first args) :verbose))
        (action-forms (if (eq (first args) :verbose) (rest args) args)))
    `(%validate-solution (list ,@(mapcar (lambda (form) `',form) action-forms))
                         ,verbose)))


;;; ==================== Core Implementation ====================


(defun %validate-solution (action-list &optional verbose)
  "Internal implementation of validate-solution.
   Executes ACTION-LIST sequentially from *start-state*.
   If VERBOSE is true, shows diagnostic output for each action."
  (when (null action-list)
    (format t "~%No actions provided to validate.~%")
    (return-from %validate-solution nil))
  
  ;; Initialize working state from *start-state*
  (let ((current-state (copy-problem-state *start-state*))
        (action-count (length action-list)))
    
    (format t "~%Validating ~D action~:P...~2%" action-count)
    
    ;; Process each action in sequence
    (loop for action-form in action-list
          for next-action-form in (append (rest action-list) '(nil))  ; nil marks last action
          for index from 1
          do (when verbose
               (format t "~%--- Action ~D: ~S ---~%" index action-form))
             (multiple-value-bind (new-state success-p failure-reason)
                 (apply-action-to-state action-form current-state next-action-form verbose)
               (if success-p
                   ;; Action succeeded - update current state
                   (progn
                     (when verbose
                       (format t "Action ~D succeeded.~%" index))
                     (setf current-state new-state))
                   ;; Action failed - report and return
                   (progn
                     (report-validation-failure index action-form failure-reason current-state)
                     (return-from %validate-solution nil)))))
    
    ;; All actions succeeded - check goal
    (check-validation-result current-state action-count)))


(defun apply-action-to-state (action-form state next-action-form &optional verbose)
  "Apply a single action form to state.
   Returns (values new-state success-p failure-reason).
   
   ACTION-FORM is (action-name arg1 arg2 ...)
   STATE is the current problem-state.
   NEXT-ACTION-FORM is the following action (for multi-effect disambiguation) or nil.
   VERBOSE if true shows diagnostic output."
  (let* ((action-name (first action-form))
         (provided-args (rest action-form))
         (action (find action-name *actions* :key #'action.name)))
    
    ;; Check if action exists
    (unless action
      (return-from apply-action-to-state
        (values nil nil (format nil "Action ~A not found in problem specification" action-name))))
    
    ;; Check argument count matches effect variables
    (let ((expected-count (length (action.effect-variables action)))
          (provided-count (length provided-args)))
      (unless (= expected-count provided-count)
        (return-from apply-action-to-state
          (values nil nil (format nil "Wrong number of arguments: expected ~D, got ~D"
                                  expected-count provided-count)))))
    
    (when verbose
      (format t "  Effect variables: ~S~%" (action.effect-variables action))
      (format t "  Precondition variables: ~S~%" (action.precondition-variables action))
      (format t "  Provided args: ~S~%" provided-args))
    
    ;; Get precondition argument combinations (handle dynamic vs static)
    (let ((precondition-args (get-precondition-args action state)))
      
      (when verbose
        (format t "  Number of precondition-args combinations: ~D~%" (length precondition-args))
        (format t "  First few combinations: ~S~%" (subseq precondition-args 0 (min 5 (length precondition-args)))))
      
      ;; Find matching precondition instantiation
      (let ((matching-pre-result nil)
            (checked-count 0)
            (passed-count 0))
        
        ;; Search through precondition argument combinations
        (dolist (pre-args precondition-args)
          (incf checked-count)
          (let ((pre-result (apply (action.pre-defun-name action) state pre-args)))
            (when pre-result
              (incf passed-count)
              ;; Check if this instantiation matches provided arguments
              (let ((effect-args (extract-effect-args action pre-args pre-result)))
                (when verbose
                  (format t "  Pre-args ~S passed, effect-args: ~S~%" pre-args effect-args))
                (when (args-match-with-combinations action effect-args provided-args)
                  (setf matching-pre-result pre-result)
                  (return))))))
        
        (when verbose
          (format t "  Checked ~D combinations, ~D passed precondition~%" checked-count passed-count))
        
        ;; If no matching instantiation found, precondition failed
        (unless matching-pre-result
          (return-from apply-action-to-state
            (values nil nil "Precondition not satisfied for given arguments")))
        
        ;; Apply effect to produce updated-dbs
        (let ((updated-dbs (if (eql matching-pre-result t)
                               (funcall (action.eff-defun-name action) state)
                               (apply (action.eff-defun-name action) state matching-pre-result))))
          
          (unless updated-dbs
            (return-from apply-action-to-state
              (values nil nil "Effect produced no state update")))
          
          ;; Select correct effect if multiple exist
          (let ((update (select-matching-effect updated-dbs state action next-action-form)))
            
            (unless update
              (return-from apply-action-to-state
                (values nil nil "No effect consistent with next action")))
            
            ;; Create new state with applied changes
            (let ((new-state (copy-problem-state state)))
              ;; Apply the update changes to new state
              (apply-update-to-state new-state update action)
              ;; Apply followups if any
              (when (update.followups update)
                (apply-followups new-state update))
              (values new-state t nil))))))))


(defun get-precondition-args (action state)
  "Get precondition argument combinations, handling dynamic vs static actions."
  (if (action.dynamic action)
      ;; Dynamic: compute combinations from current state
      (eval-instantiated-spec (action.precondition-type-inst action) state)
      ;; Static: use pre-computed combinations  
      (action.precondition-args action)))


(defun select-matching-effect (updated-dbs state action next-action-form)
  "Select the effect that is consistent with the next action.
   If only one effect, return it.
   If multiple effects and no next action, try goal satisfaction.
   If multiple effects with next action, find one that enables next action."
  (cond
    ;; Single effect - just use it
    ((= (length updated-dbs) 1)
     (first updated-dbs))
    
    ;; No next action - try each effect and prefer one satisfying goal
    ((null next-action-form)
     (or (find-if (lambda (update)
                    (let ((test-state (build-state-from-update state update action)))
                      (when (update.followups update)
                        (apply-followups test-state update))
                      (and (fboundp 'goal-fn)
                           (funcall (symbol-function 'goal-fn) test-state))))
                  updated-dbs)
         ;; No goal-satisfying effect found, return first
         (first updated-dbs)))
    
    ;; Has next action - find effect that enables it
    (t
     (let ((next-action-name (first next-action-form))
           (next-args (rest next-action-form)))
       (dolist (update updated-dbs)
         (let ((test-state (build-state-from-update state update action)))
           (when (update.followups update)
             (apply-followups test-state update))
           ;; Check if next action can execute from this state
           (when (next-action-valid-p test-state next-action-name next-args)
             (return-from select-matching-effect update))))
       ;; No matching effect found
       nil))))


(defun build-state-from-update (state update action)
  "Create a new state by applying update to state."
  (let ((new-state (copy-problem-state state)))
    (apply-update-to-state new-state update action)
    new-state))


(defun next-action-valid-p (state action-name args)
  "Check if action with given args can execute from state."
  (let ((action (find action-name *actions* :key #'action.name)))
    (when action
      (let ((precondition-args (get-precondition-args action state)))
        (dolist (pre-args precondition-args)
          (let ((pre-result (apply (action.pre-defun-name action) state pre-args)))
            (when pre-result
              (let ((effect-args (extract-effect-args action pre-args pre-result)))
                (when (args-match-with-combinations action effect-args args)
                  (return-from next-action-valid-p t))))))))))


(defun apply-followups (state update)
  "Apply followup functions to state, modifying it in place.
   Followups are (function-name . args) pairs."
  (let ((state+ (copy-problem-state state)))
    (dolist (followup (update.followups update))
      (let ((updated-idb (apply (car followup) state+ (cdr followup))))
        (setf (problem-state.idb state) updated-idb)
        (setf (problem-state.idb state+) updated-idb)))))


(defun extract-effect-args (action pre-args pre-result)
  "Extract effect variable values from precondition args and result.
   Maps precondition variables to effect variables."
  (let* ((precond-vars (action.precondition-variables action))
         (effect-vars (action.effect-variables action))
         ;; Build variable -> value mapping from precondition
         (var-values (if (eql pre-result t)
                         (pairlis precond-vars pre-args)
                         (pairlis precond-vars pre-result))))
    ;; Extract values for effect variables in order
    (mapcar (lambda (var)
              (let ((binding (assoc var var-values)))
                (if binding
                    (cdr binding)
                    var)))  ; Should not happen if well-formed
            effect-vars)))


(defun apply-update-to-state (state update action)
  "Apply an update structure to a state, modifying it in place."
  (let ((changes (update.changes update)))
    (etypecase changes
      ;; Depth-first algorithm: changes IS the complete new idb
      (hash-table
       (setf (problem-state.idb state) (copy-idb changes)))
      ;; Backtracking algorithm uses list of operations (forward-ops inverse-ops)
      (list
       (let ((forward-ops (first changes)))
         (dolist (op forward-ops)
           (destructuring-bind (operation key &optional val) op
             (ecase operation
               (:add (setf (gethash key (problem-state.idb state)) val))
               (:remove (remhash key (problem-state.idb state)))
               (:modify (setf (gethash key (problem-state.idb state)) val)))))))))
  ;; Update state metadata
  (setf (problem-state.name state) (action.name action))
  (incf (problem-state.time state) (action.duration action))
  (setf (problem-state.value state) (update.value update)))


(defun find-combination-var-groups (precondition-params)
  "Extract variable groups from combination parameters in precondition-params.
   Returns a list of variable groups, e.g., ((?T1 ?T2)) for a 2-way combination.
   Handles nested parameter structures recursively."
  (let ((groups nil))
    (dolist (item precondition-params)
      (when (listp item)
        (cond
          ;; Found a combination spec: (combination (?v1 ?v2 ...) type)
          ((eq (first item) 'combination)
           (let ((vars (second item)))
             (when (and (listp vars) (every #'?varp vars))
               (push vars groups))))
          ;; Nested parameter list with header - recurse
          ((member (first item) *parameter-headers*)
           (setf groups (nconc (find-combination-var-groups item) groups))))))
    (nreverse groups)))


(defun args-match-with-combinations (action effect-args provided-args)
  "Compare effect-args with provided-args, treating combination-derived
   positions as order-independent (set equality).
   Returns T if arguments match, NIL otherwise."
  (let ((combo-groups (find-combination-var-groups (action.precondition-params action))))
    ;; No combination parameters - use simple equality
    (when (null combo-groups)
      (return-from args-match-with-combinations (equal effect-args provided-args)))
    
    (let* ((effect-vars (action.effect-variables action))
           ;; Build list of all positions involved in combinations
           (combo-positions
             (loop for group in combo-groups
                   nconc (loop for var in group
                               for pos = (position var effect-vars)
                               when pos collect pos)))
           ;; Check non-combination positions match exactly
           (non-combo-match
             (loop for i from 0 below (length effect-vars)
                   always (or (member i combo-positions)
                              (equal (nth i effect-args)
                                     (nth i provided-args))))))
      
      (unless non-combo-match
        (return-from args-match-with-combinations nil))
      
      ;; Check each combination group has matching values (as sets)
      (dolist (group combo-groups t)
        (let* ((positions (loop for var in group
                                for pos = (position var effect-vars)
                                when pos collect pos))
               (effect-vals (mapcar (lambda (p) (nth p effect-args)) positions))
               (provided-vals (mapcar (lambda (p) (nth p provided-args)) positions))
               ;; Sort canonically for comparison
               (sorted-effect (sort (copy-list effect-vals) #'string< :key #'symbol-name))
               (sorted-provided (sort (copy-list provided-vals) #'string< :key #'symbol-name)))
          (unless (equal sorted-effect sorted-provided)
            (return-from args-match-with-combinations nil)))))))


;;; ==================== Output Functions ====================


(defun report-validation-failure (index action-form reason state)
  "Report a validation failure with diagnostics."
  (format t "~%VALIDATION FAILED at action ~D~%" index)
  (format t "~%Action: ~S~%" action-form)
  (format t "~%Reason: ~A~%" reason)
  (format t "~%State before failure:~%")
  (display-validation-state state))


(defun check-validation-result (final-state action-count)
  "Check if goal is satisfied and report result."
  (let ((goal-satisfied (and (fboundp 'goal-fn)
                             (funcall (symbol-function 'goal-fn) final-state))))
    (cond
      ;; Goal satisfied - full success
      (goal-satisfied
       (format t "~%VALIDATION SUCCESSFUL~%")
       (format t "~%All ~D action~:P executed successfully.~%" action-count)
       (format t "Goal satisfied.~%")
       (format t "~%Final state:~%")
       (display-validation-state final-state)
       final-state)
      
      ;; No goal defined
      ((not (fboundp 'goal-fn))
       (format t "~%VALIDATION COMPLETE (no goal defined)~%")
       (format t "~%All ~D action~:P executed successfully.~%" action-count)
       (format t "~%Final state:~%")
       (display-validation-state final-state)
       final-state)
      
      ;; Goal not satisfied - partial success
      (t
       (format t "~%PARTIAL VALIDATION~%")
       (format t "~%All ~D action~:P executed successfully.~%" action-count)
       (format t "Goal NOT satisfied.~%")
       (format t "~%Intermediate state:~%")
       (display-validation-state final-state)
       final-state))))


(defun display-validation-state (state)
  "Display a state in readable form."
  (let ((props (list-database (problem-state.idb state))))
    (format t "  Time: ~A~%" (problem-state.time state))
    (format t "  Value: ~A~%" (problem-state.value state))
    (format t "  Propositions:~%")
    (dolist (prop props)
      (format t "    ~S~%" prop))
    (terpri)))