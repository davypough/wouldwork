;;; Filename: ww-initialize.lisp

;;; Initialization after installation.

(in-package :ww)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for qname in *query-names*
        do (proclaim `(ftype function ,qname)))
  (loop for uname in *update-names*
        do (proclaim `(ftype function ,uname))))


(defun init ()
  (format t "~&Initializing...")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (setf *init-actions* (nreverse *init-actions*))
  (setq *happening-names* (sort (copy-list *happening-names*) #'< :key (lambda (object)
                                                   (first (aref (get object :events) 0)))))
  (init-start-state)  ;finish start-state init later in converter.lisp
  (do-integer-conversion)                      ; Full conversion and compilation of baseline
  (finalize-patroller-happenings)              ; Check initial rebound conditions
  (do-init-action-updates *start-state*)       ; Add init action propositions
  (convert-databases-to-integers)              ; Only convert new propositions, no recompilation
  (validate-start-state-consistency)
  (if *actions*
    (setf *min-action-duration* (reduce #'min *actions* :key #'action.duration))
    (format t "~%NOTE: There are no defined actions.~%"))
  (when (fboundp 'heuristic?)
    (format t "~&Applying heuristic function to start state... = ~A "
              (setf (problem-state.heuristic *start-state*) (funcall (symbol-function 'heuristic?) *start-state*)))
    (format t "done~%")
    (when *randomize-search*
      (format t "~%NOTE: Defining a heuristic? search function is incompatible with randomize-search setting.")
      (format t "~%Ignoring randomization.~%")))
  (when (fboundp 'bounding-function?)
    (format t "~&Applying bounding function to start state...")
    (multiple-value-setq (*cost* *upper*)
                         (funcall (symbol-function 'bounding-function?) *start-state*))
    (format t "done~%"))
  (when (and *happening-names* (eql *tree-or-graph* 'graph))
    (format t "~%ERROR: Graph search is incompatible with exogenous happenings, since states cannot be closed.~%"))
  (iter (for (key value) in-hashtable *db*)
        (when (and (listp value)
                   (not (consp value))
                   (or (vectorp (first value))
                       (listp (first value))))
          (format t "~%CAUTION: One or more $ arguments in a dynamic relation is a list or vector: ~A"
                  (cons (first key) (gethash (first key) *relations*)))
          (format t "~&Two lists or vectors are the same only if their elements occur in exactly the same order.")
          (format t "~&Make sure the list or vector elements are always ordered canonically (eg, lexicographically),")
          (format t "~&so that Wouldwork can tell if two states are the same or not.~%")
         (return)))
  (when (and (> *threads* 0) (not (member :sbcl *features*)))
    (format t "~%Note: Multi-threading is not available unless running SBCL. Please reset *threads*
               in ww-settings.lisp to 0 and restart wouldwork.~%"))
  (when (and (> *threads* 0) (> *debug* 1))
    (setf *debug* 1)
    (format t "~%Note: Currently set to run parallel threads. Resetting *debug* to 1.~%"))
  (let ((vals-file (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))
    (cond (*refreshing*     ; skip read-globals on refresh
           (save-globals))  ; just save current in-memory state
          ((probe-file vals-file)
           (read-globals))       ; restore globals from vals.lisp
          (t
           (save-globals))))     ; save globals for new problem
  (when (eq *problem-name* 'unspecified)
    (format t "~%Note: Please specify the problem name in the problem specification file with (ww-set *problem-name* <name>).~%"))
  (when (and (eq *algorithm* 'backtracking) (> *threads* 0))
    (error "~%Note: Backtracking is not compatible with parallel processing.~%"))
  (when (and (eq *algorithm* 'backtracking) (eq *tree-or-graph* 'graph))
    (setf *tree-or-graph* 'tree)
    (format t "~2%Note: setting *tree-or-graph* to tree (graph not compatible with backtracking).~%"))
  (when (and (eq *algorithm* 'backtracking) (eq *problem-type* 'planning))
    (format t "~%Note: Backtracking works better with a CSP (constraint satisfaction problem) than a PLANNING problem.~%"))
  (when (and (eq *problem-type* 'csp) (eq *tree-or-graph* 'graph))
    (format t "~%Note: A CSP problem solution has no repeated states, so tree search is more efficient.~%"))
  (when (and (eq *algorithm* 'backtracking) (<= *depth-cutoff* 0))
    (if (eq *problem-type* 'csp)
      (format t "~%Note: For CSP problems, suggest setting *depth-cutoff* to the number of variables to avoid possible dive to infinite depth.~%")
      (format t "~%Note: With backtracking, suggest setting *depth-cutoff* > 0 to avoid possible dive to infinite depth.~%")))
  (display-current-parameters)
  (setf *ww-loading* nil))


(defun finalize-patroller-happenings ()
  "Check rebound conditions for patrollers at t=0 and adjust direction if needed.
   Must be called after compile-all-functions so rebound lambdas are available."
  (when *happening-names*
    (setf (problem-state.happenings *start-state*)
          (loop for happening in (problem-state.happenings *start-state*)
                collect (finalize-single-happening happening)))))


(defun finalize-single-happening (happening)
  "Check and possibly adjust a single happening for initial rebound.
   For patrollers in :reverse mode starting where rebound condition is satisfied,
   mirrors the index and reverses direction as if the object just arrived.
   Matches the behavior of apply-rebound in ww-happenings.lisp."
  (destructuring-bind (object (index time direction)) happening
    (let ((rebound-fn (get object :rebound)))
      (if (and rebound-fn 
               (funcall rebound-fn *start-state*))
          ;; Rebound condition satisfied at t=0 - apply same logic as apply-rebound
          (if (eq (get object :patroller-mode) :reverse)
              (let* ((events (get object :events))
                     (n-events (length events))
                     (mirror-index (mod (- n-events index) n-events)))
                (format t "~&DEBUG: Initial rebound for ~A: index ~A -> ~A, direction ~A -> ~A~%"
                        object index mirror-index direction (- direction))
                (list object (list mirror-index time (- direction))))
              ;; Non-reverse mode with rebound - keep unchanged (matches apply-rebound)
              happening)
          ;; No rebound condition or not satisfied
          happening))))


(defun init-start-state ()
  (with-slots (name instantiations happenings time value heuristic) *start-state*
    (let ((first-event-time (loop for object in *happening-names* 
                              minimize (car (aref (get object :events) 0)))))
      (loop for obj in *happening-names*
            do (format t "~&DEBUG: ~A start-index = ~S, initial-loc = ~S, path = ~S~%" 
                       obj 
                       (compute-happening-start-index obj)
                       (find-initial-location obj)
                       (get obj :patroller-path)))
      ;; Build happenings with computed start index
      (setf happenings 
            (loop for object in *happening-names*
                  collect (list object 
                               (list (compute-happening-start-index object)
                                     first-event-time
                                     +1))))
      (format t "~&DEBUG init-start-state: happenings = ~S~%" happenings)
      (setf time 0.0)
      (setf value 0.0)
      (setf heuristic 0.0)
      (setf instantiations nil)
      (setf name 'start))))  ;updates start-state db & static-db, but not idb & hidb yet


(defun compute-happening-start-index (object)
  "Compute the starting event index for a happening object.
   For patrollers: returns the path index matching the object's initial location.
   For regular happenings: returns 0 (events are explicitly enumerated)."
  (let ((path (get object :patroller-path)))
    (if (null path)
        0  ; Regular happening - use default index 0
        (let ((initial-loc (find-initial-location object)))
          (if initial-loc
              (or (position initial-loc path) 0)
              0)))))


(defun find-initial-location (object)
  "Find the initial location of an object from *db*.
   Handles both fluent relations (loc obj) -> (area) and 
   non-fluent relations (loc obj area) -> t."
  (let ((fluent-key (list 'loc object)))
    ;; First try fluent lookup: key = (loc object), value = (area)
    (let ((fluent-value (gethash fluent-key *db*)))
      (when (and fluent-value (listp fluent-value))
        (return-from find-initial-location (first fluent-value)))))
  ;; Fallback: non-fluent lookup by scanning keys
  (loop for key being the hash-keys of *db*
        when (and (listp key)
                  (eq (first key) 'loc)
                  (eq (second key) object)
                  (= (length key) 3))
        return (third key)))


(defun validate-start-state-consistency ()
  "Checks if start state contains inconsistent-state marker.
   Errors out if initial state failed to converge."
  (when (gethash (convert-to-integer-memoized '(inconsistent-state))
                 (problem-state.idb *start-state*))
    (error "~%FATAL ERROR: Initial state is inconsistent.~%~
            Receiver state convergence failed during initialization.~%~
            The system could not stabilize in ~A iterations.~%~
            Cannot begin planning from an oscillating state."
           10)))  ;; Or reference the max-iterations value


(defun ensure-start-state-synchronized ()
  "Converts start state's hash tables to synchronized versions for parallel mode.
   Must be called after init-start-state and before search begins."
  (when (> *threads* 0)
    ;; Create new synchronized tables
    (let ((new-idb (make-hash-table :test 'eql :synchronized t))
          (new-hidb (make-hash-table :test 'eql :synchronized t)))
      ;; Copy all entries with value deep-copying
      (maphash (lambda (k v)
                 (setf (gethash k new-idb)
                       (if (consp v) (copy-list v) v)))
               (problem-state.idb *start-state*))
      (maphash (lambda (k v)
                 (setf (gethash k new-hidb)
                       (if (consp v) (copy-list v) v)))
               (problem-state.hidb *start-state*))
      ;; Replace with synchronized versions
      (setf (problem-state.idb *start-state*) new-idb)
      (setf (problem-state.hidb *start-state*) new-hidb))))

  
(init)
