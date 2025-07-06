;;; Filename: ww-parallel.lisp

; Functions for managing the multi-threading pool


(in-package :ww)


(define-condition thread-error (error)  ;condition for signaling errors across threads
  ((original-error :initarg :original-error :reader original-error)))


(defun handle-thread-error (condition)
  "Handle errors in threads by signaling shutdown and printing error information."
  (let ((*print-pretty* nil))
    (format *error-output* 
            "~2%Error in thread ~A: ~A~%Initiating shutdown.~%Backtrace:~%~A~%"
            (lparallel:kernel-worker-index)  ;(bt:current-thread)
            condition
            (with-output-to-string (s)
              (sb-debug:print-backtrace :stream s :count 20))))
  (force-output *error-output*)
  (setf *shutdown-requested* t)
  (signal 'thread-error :original-error condition))


(defun process-threads ()
  "The main consumer of parallel thread processing with controlled shutdown."
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  (let ((channel (lparallel:make-channel))
        (problems (lparallel.queue:make-queue))
        (first (lparallel.queue:make-queue)))
    (setf *num-idle-threads* *threads*)
    (setf *shutdown-requested* nil)  ; Reset shutdown flag
    (handler-case
        (progn
          (iter (for i from 1 to *threads*)
            (lparallel:submit-task channel #'search-parallel problems first))
          (lparallel.queue:push-queue *open* problems)
          (iter (sleep 0.1)
                (when (or *shutdown-requested* 
                          (= *num-idle-threads* *threads*))
                  (leave))))
      (thread-error ()
        (format t "~2%Thread error caught in main process. Shutting down.~%")
        (setf *shutdown-requested* t))
      (sb-sys:interactive-interrupt ()
        (format t "~2%Ctrl-C detected. Initiating shutdown.~2%")
        (setf *shutdown-requested* t)))
    (lparallel.queue:push-queue 'stop problems)
    (lparallel:end-kernel :wait t))
  (when *shutdown-requested*
    (format t "~2%All threads have been safely shut down. Returning to REPL.~2%")))


(defun search-parallel (problems first)
  "Branch & Bound DFS parallel search in each thread with shutdown check."
  (handler-bind ((error #'handle-thread-error))
    (iter
      (when *shutdown-requested*
        (return-from search-parallel))
      (let ((open (lparallel.queue:pop-queue problems)))
        (when (eql open 'stop)
          (lparallel.queue:push-queue 'stop problems)
          (return-from search-parallel))
        (increment-global *num-idle-threads* -1)
        #+:ww-debug (when (>= *debug* 1)
                      (let ((*package* (find-package :hs)))
                        (lprt *-* 'entering open)))
        (iter
          (when (or *shutdown-requested* 
                    (lparallel.queue:peek-queue first))
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'interrupted))
            (increment-global *num-idle-threads*)
            (leave))
          (when (and (> (hs::length-hstack open) 1)
                     (> *num-idle-threads* 0))
            (let ((subopen (split-off open)))
              (when subopen
                #+:ww-debug (when (>= *debug* 1)
                              (let ((*package* (find-package :hs)))
                                (lprt 'splitting subopen open)))
                (lparallel.queue:push-queue subopen problems))))
          (let ((current-node (hs::peek-hstack open))
                (succ-nodes (df-bnb1 open)))
            (declare (ignorable current-node))
            (when (= *program-cycles* 0)
              (when (>= *branch* 0)
                (format t "~&Exploring only branch ~D of ~D~%" *branch* (length succ-nodes))
                (setf succ-nodes (subseq succ-nodes *branch* (1+ *branch*))))
              (setf *num-init-successors* (length succ-nodes))
              (setf *rem-init-successors* (reverse succ-nodes)))
            #+:ww-debug (when (>= *debug* 1)
                          (lprt current-node))
            (when (equal succ-nodes '(first))
              #+:ww-debug (when (>= *debug* 1)
                            (lprt 'first-solution-found))
              (lparallel.queue:push-queue 'found first)
              (increment-global *num-idle-threads*)
              (leave))
            (when succ-nodes
              (if (fboundp 'heuristic?)
                  (setf succ-nodes
                        (sort (copy-list succ-nodes) #'>
                              :key (lambda (node)
                                     (problem-state.heuristic (node.state node)))))
                  (when *randomize-search*
                    (setf succ-nodes (alexandria:shuffle succ-nodes)))))
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'expanding (length succ-nodes) succ-nodes *-*)
                          (terpri))
            (iter (for succ-node in succ-nodes)
              (hs::push-hstack succ-node open :new-only (eq *tree-or-graph* 'graph)))
            (when (hs::empty-hstack open)
              #+:ww-debug (when (>= *debug* 1)
                            (lprt 'open-exhausted))
              (increment-global *num-idle-threads*)
              (leave)))
          (increment-global *program-cycles* 1)
          (setf *average-branching-factor* (compute-average-branching-factor))
          (print-search-progress))))))


(defun split-off (open)
  "Removes the bottom node on open and returns a new split-off subopen with it."
  (let ((subopen (hs::make-hstack :table 
                                  (make-hash-table :test 'equal
                                                   :synchronized (> *threads* 0))
                                  :keyfn (hs::hstack.keyfn open)))
        (bottom-node (hs::deletef-nth-hstack 0 open)))
    (hs::push-hstack bottom-node subopen :new-only (eq *tree-or-graph* 'graph))))


(defun ww-shutdown ()
  "Initiate a controlled shutdown of all threads."
  (setf *shutdown-requested* t)
  (format t "Shutdown requested. Waiting for all threads to complete...~%"))


(defun deduplicate-parallel-solutions ()
  "Post-process solutions to create unique solutions list after parallel search.
   Handles all solution types: first, every, min-length, min-time, min-value, max-value."
  (case *solution-type*
    ((first)
     ;; For 'first' solutions, unique solutions should match regular solutions
     (setf *unique-solutions* (copy-list *solutions*)))
    
    ((every)
     ;; For 'every' solutions, remove duplicates based on goal state equivalence
     (setf *unique-solutions*
           (remove-duplicates *solutions* :test #'solution-equivalent-p)))
    
    ((min-length min-time min-value max-value)
     ;; For optimization problems, unique solutions should match regular solutions
     ;; since the search algorithm already keeps only the best
     (setf *unique-solutions* (copy-list *solutions*)))
    
    (otherwise
     ;; Default case: remove duplicates
     (setf *unique-solutions*
           (remove-duplicates *solutions* :test #'solution-equivalent-p)))))


(defun finalize-parallel-search-results ()
  "Post-process parallel search results to maintain interface consistency.
   This function makes the *unique-solutions* initialization functionally necessary."
  (setf *unique-solutions* 
        (remove-duplicates *solutions* 
                          :test #'solution-equivalent-p))
  (when (>= *debug* 1)
    (format t "~&Parallel search: ~D solutions, ~D unique~%" 
            (length *solutions*) (length *unique-solutions*))))


(defun solution-equivalent-p (sol1 sol2)
  "Compare solutions for equivalence"
  (and sol1 sol2
       (equalp (problem-state.idb (solution.goal sol1))
               (problem-state.idb (solution.goal sol2)))))

