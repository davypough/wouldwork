;;; Filename: ww-action-trace.lisp

;;; Action execution tracing for debugging problem specifications.


(in-package :ww)


;;;; Global Tracing State ;;;;

(defvar *trace-action-name* nil
  "When non-nil, traces execution of this action when encountered during search.")

(defvar *traced-instantiations* nil
  "Hash table tracking which instantiations have been traced to avoid duplicates.
   Key: normalized instantiation list, Value: T")

(defvar *trace-count* 0
  "Counts how many unique instantiations have been traced.")


;;;; Main Entry Point ;;;;

(defmacro trace-action (action-name)
  `(trace-action% ',action-name))


(defun trace-action% (action-name)
  "Runs search until ACTION-NAME's precondition succeeds, then traces a unique instantiation.
   User can interrupt search at any time with Ctrl-C."
  (unless (find action-name *actions* :key #'action.name)
    (format t "~%ERROR: Action ~A not found in problem specification.~%" action-name)
    (return-from trace-action% nil))
  (unless (= *debug* 0.5)
    (format t "~%Please enable debugging with (ww-set *debug* .5) before analyzing actions.~%")
    (return-from trace-action% nil))
  
  ;; Initialize tracing state
  (setf *trace-action-name* action-name
        *traced-instantiations* (make-hash-table :test #'equal)
        *trace-count* 0)
  
  (format t "~%Searching for unique occurrences of action ~A..." action-name)
  
  ;; Run search - will intercept in generate-children
  (handler-case
      (ww-solve)
    (sb-sys:interactive-interrupt ()
      (format t "~%~%Search interrupted by user.~%")))
  
  ;; Cleanup and report
  (setf *trace-action-name* nil
        *traced-instantiations* nil)
  
  t)


;;;; Interception Handler (called from generate-children) ;;;;

(defun handle-trace-interception (state action pre-results precondition-variables)
  "Called from generate-children when target action's precondition succeeds.
   Traces each unique instantiation."
  (declare (ignore precondition-variables))
  (dolist (pre-result pre-results)
    (let ((instantiation (extract-effect-instantiation action pre-result)))
      ;; Skip if we've already traced this instantiation
      (unless (gethash instantiation *traced-instantiations*)
        (setf (gethash instantiation *traced-instantiations*) t)
        (incf *trace-count*)
        (trace-action-instantiation state action pre-result instantiation)))))


;;;; Core Tracing Logic ;;;;

(defun trace-action-instantiation (state action pre-result instantiation)
  "Traces a single action instantiation from current state through final delta."
  (format t "~%INSTANTIATION #~D: (~A~{ ~A~})~%" 
          *trace-count*
          (action.name action)
          instantiation)
  ;; Show current state
  (format t "~%PRIOR STATE:~%")
  (print-state-propositions state)
  ;; Execute effect to get final state
  (let ((updated-dbs (if (eql pre-result t)
                         (funcall (action.eff-defun-name action) state)
                         (apply (action.eff-defun-name action) state pre-result))))
    ;; Show final state delta for each update
    (dolist (updated-db updated-dbs)
      (format t "~%UPDATED STATE DELTA:~%")
      (print-state-delta state updated-db)))
  (simple-break)
  (terpri))


;;;; Helper Functions ;;;;

(defun extract-effect-instantiation (action pre-result)
  "Extracts the effect variable instantiation in canonical form for duplicate detection."
  (let* ((effect-vars (action.effect-variables action))
         (precond-vars (action.precondition-variables action))
         (var-value-alist (pairlis precond-vars pre-result)))
    (mapcar (lambda (var)
              (let ((binding (assoc var var-value-alist)))
                (if binding
                    (cdr binding)
                    var)))  ; Use variable name only if not bound
            effect-vars)))


(defun print-state-propositions (state)
  "Prints all propositions in the state."
  (let ((props (list-database (problem-state.idb state))))
    (dolist (prop (sort (copy-list props) #'proposition-less-p))
      (format t "  ~A~%" prop))))


(defun proposition-less-p (p1 p2)
  "Comparison function for sorting propositions by predicate name."
  (string< (string (car p1)) (string (car p2))))


(defun print-state-delta (state updated-db)
  "Prints the difference between initial state and updated state."
  (let ((changes (update.changes updated-db)))
    (etypecase changes
      (hash-table
       (print-hash-table-delta state changes))
      (list
       (print-list-delta state changes)))))


(defun print-hash-table-delta (state changes)
  "Prints delta for depth-first algorithm (hash-table format)."
  (let ((initial-props (list-database (problem-state.idb state)))
        (final-props (list-database changes))
        added deleted)
    
    ;; Build lookup tables for efficient comparison
    (let ((initial-table (make-hash-table :test #'equal))
          (final-table (make-hash-table :test #'equal)))
      
      (dolist (prop initial-props)
        (setf (gethash prop initial-table) t))  ; CHANGED: use full prop as key
      
      (dolist (prop final-props)
        (setf (gethash prop final-table) t))    ; CHANGED: use full prop as key
      
      ;; Find additions and deletions
      (dolist (prop final-props)
        (unless (gethash prop initial-table)    ; CHANGED: lookup by full prop
          (push prop added)))
      
      (dolist (prop initial-props)
        (unless (gethash prop final-table)      ; CHANGED: lookup by full prop
          (push prop deleted))))
    
    ;; Display results
    (when deleted
      (format t "~%  DELETED:~%")
      (dolist (prop (sort (nreverse deleted) #'proposition-less-p))
        (format t "    ~A~%" prop)))
    
    (when added
      (format t "~%  ADDED:~%")
      (dolist (prop (sort (nreverse added) #'proposition-less-p))
        (format t "    ~A~%" prop)))
    
    (when (and (null added) (null deleted))
      (format t "~%  (no changes)~%"))))


(defun print-list-delta (state changes)
  "Prints delta for backtracking algorithm (list format).
   Format is (forward-list inverse-list) where forward-list contains
   propositions or (not proposition) forms."
  (let ((forward-list (first changes))
        (initial-props (list-database (problem-state.idb state)))
        added deleted)
    
    ;; Parse forward operations into additions and deletions
    (dolist (op forward-list)
      (if (and (consp op) (eq (car op) 'not))
          (push (second op) deleted)
          (push op added)))
    
    ;; Display results
    (when added
      (format t "~%  ADDED:~%")
      (dolist (prop (sort (nreverse added) #'proposition-less-p))
        (format t "    ~A~%" prop)))
    
    (when deleted
      (format t "~%  DELETED:~%")
      (dolist (prop (sort (nreverse deleted) #'proposition-less-p))
        (format t "    ~A~%" prop)))
    
    (when (and (null added) (null deleted))
      (format t "~%  (no changes)~%"))))