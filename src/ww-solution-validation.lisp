;;; Filename: ww-solution-validation.lisp

;;; Solution validation capability for verifying and troubleshooting action sequences.
;;; Given a sequence of actions, executes them in order from *start-state*,
;;; reporting success, failure point, or partial completion.

(in-package :ww)


;;; ==================== Reusable Validation Results ====================


(defstruct action-sequence-validation
  "Result of replaying an action sequence from an explicit initial state."
  success-p
  final-state
  action-count
  failure-index
  failure-action
  failure-reason
  goal-checked-p
  goal-satisfied-p)


(defun normalize-validation-actions (action-list)
  "Return plain action forms from ACTION-LIST.

ACTION-LIST may contain plain forms or timestamped solution moves.  State snapshots
interleaved with timestamped moves are ignored."
  (let ((first-entry (first action-list)))
    (if (and (consp first-entry)
             (numberp (first first-entry))
             (consp (second first-entry)))
      (mapcar #'second
              (remove-if-not (lambda (entry)
                               (and (consp entry)
                                    (numberp (first entry))
                                    (consp (second entry))))
                             action-list))
      action-list)))


(defun validate-action-sequence (initial-state action-list &key goal-test verbose)
  "Replay ACTION-LIST from INITIAL-STATE and return an ACTION-SEQUENCE-VALIDATION.

The supplied state is never modified.  GOAL-TEST, when supplied, is called on the final
state and recorded separately from action executability.  VERBOSE prints per-action state
transitions for the interactive VALIDATE-SOLUTION interface."
  (declare (type problem-state initial-state))
  (let* ((actions (normalize-validation-actions action-list))
         (action-count (length actions))
         (current-state (copy-problem-state initial-state)))
    (loop for tail on actions
          for action-form = (first tail)
          for next-action-form = (second tail)
          for index from 1
          do (when verbose
               (format t "~%--- Action ~D: ~S ---~%" index action-form))
             (multiple-value-bind (new-state success-p failure-reason)
                 (apply-action-to-state
                   action-form current-state next-action-form verbose)
               (unless success-p
                 (return-from validate-action-sequence
                   (make-action-sequence-validation
                     :success-p nil
                     :final-state current-state
                     :action-count action-count
                     :failure-index index
                     :failure-action action-form
                     :failure-reason failure-reason
                     :goal-checked-p (not (null goal-test))
                     :goal-satisfied-p nil)))
               (when verbose
                 (format t "Action ~D succeeded.~%" index)
                 (format t "Resulting state:~%")
                 (display-validation-state new-state))
               (setf current-state new-state)))
    (make-action-sequence-validation
      :success-p t
      :final-state current-state
      :action-count action-count
      :goal-checked-p (not (null goal-test))
      :goal-satisfied-p (and goal-test (funcall goal-test current-state)))))


(defun reset-candidate-solution-validation-statistics ()
  "Reset the candidate-validation counters for a new search."
  (bt:with-lock-held (*solution-validation-lock*)
    (setf *nominal-solution-candidates* 0
          *accepted-solution-candidates* 0
          *rejected-solution-candidates* 0)
    (clrhash *solution-validator-rejections*)))


(defun solution-validator-diagnostic-category (validator diagnostic)
  "Return a stable report key for VALIDATOR's DIAGNOSTIC."
  (cond
    ((and (listp diagnostic)
          (keywordp (first diagnostic)))
     (list validator
           (or (getf diagnostic :phase) :unspecified)
           (or (getf diagnostic :reason) :unspecified)))
    ((symbolp diagnostic)
     (list validator :unspecified (or diagnostic :unspecified)))
    (t
     (list validator :unspecified :uncategorized))))


(defun record-candidate-solution-validation (valid-p validator diagnostic)
  "Record one complete candidate-validation result."
  (bt:with-lock-held (*solution-validation-lock*)
    (incf *nominal-solution-candidates*)
    (if valid-p
      (incf *accepted-solution-candidates*)
      (progn
        (incf *rejected-solution-candidates*)
        (incf
          (gethash
            (solution-validator-diagnostic-category validator diagnostic)
            *solution-validator-rejections*
            0)
          1)))))


(defun candidate-solution-valid-p (path goal-state)
  "Run every problem-local solution validator on PATH and GOAL-STATE.

Returns three values: validity, the rejecting validator symbol, and its diagnostic value.
With no registered validators every candidate is valid.  Every completed check contributes
to the search's candidate-validation diagnostics."
  (unless *solution-validators*
    (return-from candidate-solution-valid-p (values t nil nil)))
  (dolist (validator *solution-validators*)
    (multiple-value-bind (valid-p diagnostic)
        (funcall (symbol-function validator) *start-state* path goal-state)
      (unless valid-p
        (record-candidate-solution-validation nil validator diagnostic)
        (return-from candidate-solution-valid-p
          (values nil validator diagnostic)))))
  (record-candidate-solution-validation t nil nil)
  (values t nil nil))


(defun solution-validator-rejection-rows ()
  "Return rejection counts in stable report order."
  (let (rows)
    (bt:with-lock-held (*solution-validation-lock*)
      (maphash
        (lambda (category count)
          (push (cons category count) rows))
        *solution-validator-rejections*))
    (sort rows #'string<
          :key (lambda (row)
                 (prin1-to-string (car row))))))


(defun print-candidate-solution-validation-statistics
    (&optional (stream *standard-output*))
  "Print grouped candidate-validation diagnostics for the completed search."
  (when *solution-validators*
    (format stream "~2%Candidate solution validation:")
    (format stream "~%  Nominal goal paths checked = ~:D"
            *nominal-solution-candidates*)
    (format stream "~%  Accepted = ~:D" *accepted-solution-candidates*)
    (format stream "~%  Rejected = ~:D" *rejected-solution-candidates*)
    (if (zerop *nominal-solution-candidates*)
      (format stream "~%  No path reached the problem goal.")
      (dolist (row (solution-validator-rejection-rows))
        (destructuring-bind ((validator phase reason) . count) row
          (format stream "~%  ~A / ~A / ~A = ~:D"
                  validator phase reason count))))
    (terpri stream)))


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
   ACTION-LIST may be either:
     - Plain actions: ((ACTION arg1 arg2) (ACTION2 arg1) ...)
     - Timestamped actions: ((1.0 (ACTION arg1 arg2)) (2.0 (ACTION2 arg1)) ...)
   If VERBOSE is true, shows diagnostic output for each action."
  (let* ((actions (normalize-validation-actions action-list))
         (action-count (length actions)))
    (format t "~%Validating ~D action~:P...~2%" action-count)
    (when verbose
      (format t "Start state:~%")
      (display-validation-state *start-state*))
    (let ((result
            (validate-action-sequence
              *start-state* actions
              :goal-test (and (fboundp 'goal-fn) (symbol-function 'goal-fn))
              :verbose verbose)))
      (if (action-sequence-validation-success-p result)
        (check-validation-result
          (action-sequence-validation-final-state result) action-count)
        (progn
          (report-validation-failure
            (action-sequence-validation-failure-index result)
            (action-sequence-validation-failure-action result)
            (action-sequence-validation-failure-reason result)
            (action-sequence-validation-final-state result))
          nil)))))


(defun apply-action-to-state (action-form state next-action-form &optional verbose)
  "Apply a single action form to state.
   Returns (values new-state success-p failure-reason).
   
   ACTION-FORM is (action-name arg1 arg2 ...)
   STATE is the current problem-state.
   NEXT-ACTION-FORM is the following action (for multi-effect disambiguation) or nil.
   VERBOSE if true shows diagnostic output."
  (let* ((action-name (first action-form))
         (provided-args (rest action-form))
         (action (find action-name *actions* :key #'action.name))
         (wait-duration nil))
    
    ;; Check if action exists
    (unless action
      (return-from apply-action-to-state
        (values nil nil (format nil "Action ~A not found in problem specification" action-name))))
    
    ;; WAIT action special handling: duration in solution output is informational,
    ;; not an effect variable. Save it for time update, then strip for validation.
    (when (eql action-name 'wait)
      (setf wait-duration (first provided-args))
      (setf provided-args nil))
    
    ;; Strip any display connectives from a path pasted off annotated solution output,
    ;; so the count check and matching see the pure value list.
    (setf provided-args (strip-display-connectives action provided-args))
    
    ;; Check argument count matches effect variables
    (let ((expected-count (length (action.effect-variables action)))
          (provided-count (length provided-args)))
      (unless (= expected-count provided-count)
        (return-from apply-action-to-state
          (values nil nil (format nil "Wrong number of arguments: expected ~D, got ~D"
                                  expected-count provided-count)))))
    
    (when verbose
      (format t "  Effect variables: ~S~%" (action.effect-variables action))
      (format t "  Provided values : ~S~%" provided-args))
    
    ;; Get precondition argument combinations (handle dynamic vs static)
    (let ((precondition-args (get-precondition-args action state))
          (first-passing-pre-result nil)
          (first-passing-pre-args nil)
          (all-instantiations nil))
      
      ;; Find any passing precondition, execute effect, collect instantiations
      (dolist (pre-args precondition-args)
        ;; Preconditions and effects may bind or otherwise mutate their state argument.
        ;; Probe each candidate on a private copy, then apply the selected update to a
        ;; fresh copy of the caller's state.
        (let* ((trial-state (copy-problem-state state))
               (pre-result
                 (apply (action.pre-defun-name action) trial-state pre-args)))
          (when pre-result
            ;; Save first passing for diagnostics
            (unless first-passing-pre-result
              (setf first-passing-pre-result pre-result)
              (setf first-passing-pre-args pre-args))
            
            ;; Execute effect to get updated-dbs
            (let ((updated-dbs (if (eql pre-result t)
                                 (funcall (action.eff-defun-name action) trial-state)
                                 (apply (action.eff-defun-name action)
                                        trial-state pre-result))))
              
              ;; Check each update's instantiations for match
              (dolist (update updated-dbs)
                (let ((inst (update.instantiations update)))
                  (push inst all-instantiations)
                  (when (instantiations-match-p action inst provided-args)
                    (when verbose
                      (format t "  Precondition satisfied for: ~S~%" inst))
                    ;; Found matching update - apply it
                    (let ((new-state (copy-problem-state state)))
                      (apply-update-to-state new-state update action)
                      
                      ;; Remove (WAITING) when transitioning from a WAIT action
                      (when (eql (problem-state.name state) 'wait)
                        (remhash (gethash 'waiting *constant-integers*)
                                 (problem-state.idb new-state)))
                      
                      ;; For WAIT, override time with actual duration
                      (when wait-duration
                        (setf (problem-state.time new-state)
                              (+ (problem-state.time state) wait-duration)))
                      
                      ;; Apply followups if any
                      (when (update.followups update)
                        (apply-followups new-state update))
                      
                      ;; Process happenings if present
                      (when *happening-names*
                        (let ((net-state (amend-happenings state new-state)))
                          (unless net-state
                            (return-from apply-action-to-state
                              (values nil nil "Happening violation (kill condition)")))
                          (setf new-state net-state)))
                      
                      (return-from apply-action-to-state
                        (values new-state t nil))))))))))
      
      ;; No matching update found - determine failure type
      (if first-passing-pre-result
          ;; Precondition passed but no instantiation matched
          (values nil nil (cons :state-mismatch (first all-instantiations)))
          ;; No precondition passed at all
          (values nil nil :precondition-failure)))))


(defun strip-display-connectives (action provided-args)
  "Returns PROVIDED-ARGS with any display connectives removed, yielding the pure value
   list used for matching and diagnostics. Returns PROVIDED-ARGS unchanged when ACTION's
   effect-format template carries no string connectives, or when PROVIDED-ARGS is already
   the pure value list. Otherwise PROVIDED-ARGS was pasted from annotated solution output,
   and the connective tokens are dropped."
  (let ((template (action.effect-format action)))
    (if (or (notany #'stringp template)
            (= (length provided-args) (length (action.effect-variables action))))
      provided-args
      (extract-annotated-values template provided-args))))


(defun extract-annotated-values (template provided-args)
  "Walks TEMPLATE left to right against PROVIDED-ARGS, the token list read back from
   annotated solution output. Annotated output is printed with escaping off, so a string
   connective contributes one token per whitespace-delimited word rather than one token
   overall; those tokens are discarded. Each variable slot consumes one token, which is
   collected. Returns the pure value list."
  (let ((tokens provided-args)
        (values nil))
    (dolist (slot template (nreverse values))
      (if (stringp slot)
        (loop repeat (count-connective-words slot)
              do (pop tokens))
        (push (pop tokens) values)))))


(defun count-connective-words (string)
  "Number of whitespace-delimited words in STRING, ie the number of tokens STRING
   contributes when printed unescaped and read back."
  (let ((count 0)
        (in-word nil))
    (loop for char across string
          do (if (member char '(#\Space #\Tab #\Newline #\Return #\Page))
               (setf in-word nil)
               (unless in-word
                 (setf in-word t)
                 (incf count))))
    count))


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
              ;; Execute effect to get updated-dbs with actual instantiations
              (let ((updated-dbs (if (eql pre-result t)
                                     (funcall (action.eff-defun-name action) state)
                                     (apply (action.eff-defun-name action) state pre-result))))
                ;; Check if any update's instantiations match provided args
                (dolist (update updated-dbs)
                  (when (instantiations-match-p action (update.instantiations update) args)
                    (return-from next-action-valid-p t)))))))))))


(defun apply-followups (state update)
  "Apply followup functions to state, modifying it in place.
   Followups are (function-name . args) pairs that modify state in-place."
  (let ((state+ (copy-problem-state state)))
    (dolist (followup (update.followups update))
      (apply (car followup) state+ (cdr followup))
      (let ((updated-idb (problem-state.idb state+)))
        (setf (problem-state.idb state) updated-idb))))
  (setf (problem-state.idb-hash state) nil))                 ; clear stale hash


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
         (when forward-ops
           ;; Backtracking updates may be either explicit op triples
           ;; (:add/:remove/:modify key value) or literal updates compatible with REVISE.
           (if (and (consp (first forward-ops))
                    (keywordp (first (first forward-ops))))
               (dolist (op forward-ops)
                 (destructuring-bind (operation key &optional val) op
                   (ecase operation
                     (:add (setf (gethash key (problem-state.idb state)) val))
                     (:remove (remhash key (problem-state.idb state)))
                     (:modify (setf (gethash key (problem-state.idb state)) val)))))
               (revise (problem-state.idb state) forward-ops)))))))
  ;; Update state metadata
  (setf (problem-state.name state) (action.name action))
  (setf (problem-state.instantiations state)
        (copy-tree (update.instantiations update)))
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


(defun get-combination-positions (action)
  "Returns a list of position-lists for combination variable groups.
   Each position-list contains the indices in effect-variables where
   combination variables appear.
   Example: For effect-vars (?AGENT $CARGO ?T1 ?T2 $AREA $PLACE) with
   combination (?T1 ?T2), returns ((2 3))."
  (let* ((precond-params (action.precondition-params action))
         (effect-vars (action.effect-variables action))
         (var-groups (find-combination-var-groups precond-params))
         (position-groups nil))
    (dolist (group var-groups)
      (let ((positions (mapcar (lambda (var)
                                 (position var effect-vars))
                               group)))
        ;; Only include if all variables found in effect-vars
        (when (every #'identity positions)
          (push (sort (copy-list positions) #'<) position-groups))))
    (nreverse position-groups)))


(defun instantiations-match-p (action provided-args actual-args)
  "Compare instantiations, treating combination variable groups as unordered sets.
   For non-combination variables, uses strict positional equality.
   For combination variables, compares the group values as sets."
  (let ((combo-positions (get-combination-positions action)))
    (if (null combo-positions)
        ;; No combinations - use strict equality
        (equal provided-args actual-args)
        ;; Has combinations - compare carefully
        (and (= (length provided-args) (length actual-args))
             ;; Check all non-combination positions for equality
             (let ((combo-indices (reduce #'append combo-positions)))
               (loop for i from 0 below (length provided-args)
                     always (or (member i combo-indices)
                                (equal (nth i provided-args)
                                       (nth i actual-args)))))
             ;; Check each combination group as an unordered set
             (every (lambda (group)
                      (alexandria:set-equal
                       (mapcar (lambda (pos) (nth pos provided-args)) group)
                       (mapcar (lambda (pos) (nth pos actual-args)) group)))
                    combo-positions)))))


;;; ==================== Output Functions ====================


(defun report-validation-failure (index action-form reason state)
  "Report a validation failure with diagnostics."
  (format t "~%VALIDATION FAILED at action ~D~%" index)
  (format t "~%Action: ~S~%" action-form)
  (let ((action (find (first action-form) *actions* :key #'action.name)))
    (cond
      ;; True precondition failure
      ((eq reason :precondition-failure)
       (format t "~%Reason: Precondition not satisfied~%")
       (when action
         (format t "~%Precondition: ~S~%" (action.precondition-form action))
         (diagnose-precondition-failure action
                                        (strip-display-connectives action (rest action-form))
                                        state)))
      
      ;; State mismatch - precondition passed but bindings differ
      ((and (consp reason) (eq (car reason) :state-mismatch))
       (format t "~%Reason: State mismatch - Loss of synchronization with expected trajectory~%")
       (when action
         (report-state-mismatch action
                                (strip-display-connectives action (rest action-form))
                                (cdr reason))))
      
      ;; Other failure reasons (strings)
      (t
       (format t "~%Reason: ~A~%" reason)))))


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
       (display-validation-state final-state)))))


(defun display-validation-state (state)
  "Display a state in readable form."
  (let ((props (list-database (problem-state.idb state))))
    (format t "  Time: ~A~%" (problem-state.time state))
    (format t "  Value: ~A~%" (problem-state.value state))
    (format t "  Propositions:~%")
    (dolist (prop props)
      (format t "    ~S~%" prop))
    (terpri)))


;;; ==================== Precondition Diagnosis ====================


(defun diagnose-precondition-failure (action provided-args state)
  "Diagnose which clause in a precondition fails by evaluating each clause
   in sequence. Reports the first failing clause with bindings substituted.
   
   ACTION is the action structure.
   PROVIDED-ARGS is the list of argument values from the action form.
   STATE is the current problem-state."
  (let* ((precond-form (action.precondition-form action))
         (precond-vars (action.precondition-variables action))
         (effect-vars (action.effect-variables action))
         ;; Build effect-var -> value mapping from provided args
         (effect-var-map (pairlis effect-vars provided-args))
         ;; Split precondition-variables into ?vars (from parameters) and $vars (fluents)
         (?vars (remove-if-not #'?varp precond-vars))
         ($vars (remove-if-not #'$varp precond-vars))
         ;; Get ?var values from the effect-var mapping (they must appear in effect-vars)
         ;; $vars are initialized to nil (bound during clause evaluation)
         (?var-values (mapcar (lambda (v)
                                (let ((binding (assoc v effect-var-map)))
                                  (if binding (cdr binding) nil)))
                              ?vars))
         ($var-values (make-list (length $vars) :initial-element nil))
         ;; Extract clauses and any let-bindings
         (clauses nil)
         (let-bindings nil))
    
    ;; Handle (let bindings decl body) wrapper
    (when (and (consp precond-form) (eq (car precond-form) 'let))
      (setf let-bindings (second precond-form))
      (setf precond-form (if (and (third precond-form)
                                   (consp (third precond-form))
                                   (eq (car (third precond-form)) 'declare))
                              (fourth precond-form)
                              (third precond-form))))
    
    ;; Extract clauses from AND, or treat as single clause
    (setf clauses (if (and (consp precond-form) (eq (car precond-form) 'and))
                      (cdr precond-form)
                      (list precond-form)))
    
    ;; Evaluate each clause in sequence
    (dolist (clause clauses)
      (multiple-value-bind (result new-$var-values)
          (evaluate-precondition-clause clause state 
                                        ?vars ?var-values 
                                        $vars $var-values
                                        let-bindings)
        (if result
            ;; Clause passed - update $var bindings for next clause
            (setf $var-values new-$var-values)
            ;; Clause failed - report and return
            (let ((substituted (substitute-bindings clause 
                                                    ?vars ?var-values 
                                                    $vars $var-values)))
              (format t "~%Failed: ~S~%" substituted)
              (return-from diagnose-precondition-failure nil)))))))


(defun report-binding-mismatches ($vars $var-values effect-vars provided-args)
  "Report mismatches between $var values bound from database evaluation
   vs. values provided in the action form.
   
   $VARS is the list of fluent variable names from precondition.
   $VAR-VALUES is the list of values bound during clause evaluation.
   EFFECT-VARS is the list of effect variable names.
   PROVIDED-ARGS is the list of values from the action form."
  (let ((mismatches nil))
    ;; For each $var, find its position in effect-vars and compare values
    (loop for $var in $vars
          for bound-value in $var-values
          for pos = (position $var effect-vars)
          when pos
          do (let ((provided-value (nth pos provided-args)))
               (unless (equal bound-value provided-value)
                 (push (list $var bound-value provided-value) mismatches))))
    
    (if mismatches
        (progn
          (format t "~%Binding mismatch - Loss of state synchronization:~%")
          (dolist (mismatch (nreverse mismatches))
            (destructuring-bind ($var bound-value provided-value) mismatch
              (format t "  ~A: actual ~S, provided ~S~%" $var bound-value provided-value))))
        ;; No mismatches found - truly unexpected
        (format t "~%All clauses passed, no binding mismatches found (unexpected)~%"))))


(defun evaluate-precondition-clause (clause state ?vars ?var-values $vars $var-values let-bindings)
  "Evaluate a single precondition clause against state.
   Returns (values result new-$var-values) where result is T/NIL
   and new-$var-values contains updated fluent bindings."
  (let* (;; Build the lambda body with translated clause
         (translated (translate clause 'pre))
         (int-code (subst-int-code translated))
         ;; Lambda that evaluates clause and returns updated $var values
         (lambda-form
           `(lambda (state ,@?vars ,@$vars)
              (declare (ignorable state ,@?vars ,@$vars))
              ,(if let-bindings
                   `(let ,let-bindings
                      (let ((result ,int-code))
                        (values result (list ,@$vars))))
                   `(let ((result ,int-code))
                      (values result (list ,@$vars))))))
         (fn (compile nil lambda-form)))
    ;; Call with state, ?var values, then $var values
    (apply fn state (append ?var-values $var-values))))


(defun substitute-bindings (form ?vars ?var-values $vars $var-values)
  "Substitute variable bindings into a form for display.
   Replaces ?vars and $vars with their corresponding values."
  (let ((bindings (append (pairlis ?vars ?var-values)
                          (pairlis $vars $var-values))))
    (subst-bindings-recursive form bindings)))


(defun subst-bindings-recursive (form bindings)
  "Recursively substitute bindings in form."
  (cond
    ((null form) nil)
    ((atom form)
     (let ((binding (assoc form bindings)))
       (if binding
           (cdr binding)
           form)))
    (t (cons (subst-bindings-recursive (car form) bindings)
             (subst-bindings-recursive (cdr form) bindings)))))


(defun report-state-mismatch (action provided-args actual-effect-args)
  "Report mismatches between provided argument values and actual bound values.
   
   ACTION is the action structure.
   PROVIDED-ARGS is the list of values from the action form.
   ACTUAL-EFFECT-ARGS is the effect-args extracted from a passing precondition."
  (let ((effect-vars (action.effect-variables action))
        (mismatches nil))
    ;; Compare each position
    (loop for var in effect-vars
          for provided in provided-args
          for actual in actual-effect-args
          unless (equal provided actual)
          do (push (list var actual provided) mismatches))
    
    (when mismatches
      (format t "~%Mismatched bindings:~%")
      (dolist (mismatch (nreverse mismatches))
        (destructuring-bind (var actual provided) mismatch
          (format t "  ~A: actual ~S, provided ~S~%" var actual provided))))))
