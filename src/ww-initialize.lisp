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
  (init-start-state)  ;finish start-state init later in converter.lisp
  (setq *happening-names* (sort (copy-list *happening-names*) #'< :key (lambda (object)
                                                   (first (aref (get object :events) 0)))))
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
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
    (if (probe-file vals-file)
      (read-globals)    ;restore globals for old problem.lisp from vals.lisp
      (save-globals)))  ;save globals for new problem.lisp into vals.lisp
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


(defun init-start-state ()
  (with-slots (name instantiations happenings time value heuristic) *start-state*
    (let ((first-event-time (loop for object in *happening-names* 
                              minimize (car (aref (get object :events) 0)))))
      (setf happenings (loop for object in *happening-names* ;property list of happening objects
                             collect (list object 
                                          (list 0 first-event-time +1))))  ;next (index time direction)
      (setf time 0.0)
      (setf value 0.0)
      (setf heuristic 0.0)
      (setf instantiations nil)
      (setf name 'start)))
  (do-init-action-updates *start-state*))  ;updates start-state db & static-db, but not idb & hidb yet

  
(init)
