;;; Filename: ww-parallel-backtracker.lisp

;;; Parallel backtracking search:
;;; - A task is the list of choice ordinals from the root to a node at the split depth
;;; - Task generation reruns the serial backtracker, recording tasks at the split depth
;;; - Each worker replays a task prefix with an ordinary (backtrack 0) on its own state

(in-package :ww)


(defstruct (bt-worker (:conc-name bt-worker.))
  "Per-thread context of a parallel backtracking worker."
  (id 0 :type fixnum)
  stats                                    ; this worker's WORKER-STATS
  queue                                    ; the shared TASK-QUEUE
  (cycles-since-report 0 :type fixnum))


(defparameter *bt-trial-globals*
  '(*program-cycles* *total-states-processed* *max-depth-explored* *repeated-states*
    *duplicate-accumulated-depths* *duplicate-num-paths* *inconsistent-states-dropped*
    *lower-bound-pruned* *bounding-pruned* *search-prefix-validations* *search-prefix-pruned*
    *nominal-solution-candidates* *accepted-solution-candidates* *rejected-solution-candidates*
    *solution-paths* *unique-solution-states* *solution-count* *count-example*)
  "Search results a task-generation pass changes. A pass that is followed by a deeper
   one is a trial: these are restored so only the final pass's results remain.")


;;; ============================================================
;;; COORDINATOR
;;; ============================================================

(defun process-partitioned-parallel-bt ()
  (call-with-parallel-search-lifetime #'process-partitioned-parallel-bt-body))


(defun process-partitioned-parallel-bt-body ()
  "Generate ordinal-prefix tasks serially, then let the workers search their subtrees.
   Goals above the split depth are registered during generation; the workers' statistics
   are added to the generation pass's statistics after they all join."
  (validate-bt-path-mode)
  (setf *parallel-timing* (make-parallel-timing))
  (reset-parallel-control-flags)
  (setf *parallel-search-active* t)
  (initialize-worker-stats *threads*)
  (format t "~2%========================================~%")
  (format t "Partitioned Parallel Backtracking~%")
  (format t "========================================~%")
  (format t "  Threads: ~D~%" *threads*)
  (format t "  Problem type: ~A / ~A~%" *problem-type* *solution-type*)
  (format t "----------------------------------------~%")
  (let* ((total-start (get-internal-real-time))
         (tasks (generate-bt-tasks)))
    (setf (pt-task-generation-ms *parallel-timing*) (round (* 1000 (- (get-internal-real-time) total-start))
                 internal-time-units-per-second))
    (if tasks
        (run-bt-workers tasks)
        (format t "~&Search completed during task generation~%"))
    (aggregate-worker-stats)
    (set-bt-average-branching-factor)
    (setf (pt-total-ms *parallel-timing*) (round (* 1000 (- (get-internal-real-time) total-start))
                 internal-time-units-per-second)))
  (setf *parallel-search-active* nil)
  t)


(defun run-bt-workers (tasks)
  "Queue TASKS and run *THREADS* backtracking workers until all of them finish."
  (let ((queue (make-new-task-queue))
        (start (get-internal-real-time)))
    (format t "~%Starting ~D workers on ~D tasks...~%" *threads* (length tasks))
    (tq-push-many queue tasks)
    (tq-signal-done queue)
    (run-parallel-worker-group queue *threads* :function #'parallel-bt-worker)
    (setf (pt-worker-search-ms *parallel-timing*) (round (* 1000 (- (get-internal-real-time) start))
                 internal-time-units-per-second))
    (format t "~&All ~D workers completed.~%" *threads*)))


;;; ============================================================
;;; TASK GENERATION (serial, before workers start)
;;; ============================================================

(defun generate-bt-tasks ()
  "Rerun the serial backtracker from the root at split depths 1, 2, ... until the task
   target is met, *SPLIT-DEPTH-MAX* is reached, or generation ends the search itself
   (no node reaches the split depth, or the solution limit is reached). Only the final
   pass keeps its solutions and statistics. Returns the final pass's tasks, or NIL when
   no workers are needed."
  (let ((target (compute-target-tasks)))
    (format t "~&Generating tasks: target=~D, safety-cap-depth=~D~%" target *split-depth-max*)
    (loop for depth from 1
          for saved = (mapcar #'symbol-value *bt-trial-globals*)
          for tasks = (collect-bt-tasks depth)
          for solved = (and *solution-paths* (solution-count-reached-p))
          until (or solved (null tasks) (>= (length tasks) target) (>= depth *split-depth-max*))
          do (mapc #'set *bt-trial-globals* saved)
          finally (unless solved
                    (format t "~&Generated ~D tasks at split depth ~D~%" (length tasks) depth))
                  (return (unless solved tasks)))))


(defun collect-bt-tasks (split-depth)
  "Run one serial backtracking pass that records the ordinal path of every node reaching
   SPLIT-DEPTH as a task instead of exploring it. Returns the tasks in search order."
  (let ((*backtrack-state* (copy-problem-state *start-state*))
        (*choice-stack* nil)
        (*bt-path-fingerprints* nil)
        (*bt-path-search-active* *bt-cycle-check*)
        (*bt-split-depth* split-depth)
        (*bt-ordinal-path* nil)
        (*bt-collected-tasks* nil))
    (backtrack 0)
    (nreverse *bt-collected-tasks*)))


;;; ============================================================
;;; WORKERS
;;; ============================================================

(defun parallel-bt-worker (worker-id task-queue)
  "Run tasks from TASK-QUEUE until it is exhausted. Once the search stops, remaining
   tasks are popped without being run: every worker must keep returning to the queue,
   because TQ-POP-BLOCKING releases waiting workers only when all of them are waiting."
  (declare (type fixnum worker-id) (type task-queue task-queue))
  (tq-register-worker task-queue)
  (let ((*bt-worker* (make-bt-worker :id worker-id
                                     :stats (get-worker-stats worker-id)
                                     :queue task-queue)))
    (loop
      (let ((task (tq-pop-blocking task-queue)))
        (unless task
          (return))
        (unless (bt-worker-stop-p)
          (run-bt-task task))))))


(defun run-bt-task (prefix)
  "Search the subtree at the end of the ordinal PREFIX from a private copy of the start
   state. Every special the backtracking path assigns is bound here, so its value is
   private to this thread."
  (let ((*backtrack-state* (copy-problem-state *start-state*))
        (*choice-stack* nil)
        (*bt-path-fingerprints* nil)
        (*bt-path-search-active* *bt-cycle-check*)
        (*bt-undo-frame* nil)
        (*propagated-state-changed* nil)
        (*symmetry-idb-touched-p* nil)
        (*idb-hash-acc* nil)
        (*fixed-idb-hash-acc* nil)
        (*symmetry-idb-acc* nil)
        (*bt-forced-prefix* prefix)
        (*bt-ordinal-path* nil))
    (backtrack 0)))


(defun bt-worker-stop-p ()
  "True in a parallel worker once another worker's result or a failure ends the search:
   a first solution, a reached solution count, or a shutdown request."
  (and *bt-worker*
       (or *first-solution-found*
           *shutdown-requested*
           (and (typep *solution-type* 'fixnum) (solution-count-reached-p)))))


(defun record-bt-worker-node (level)
  "Count a worker's node in its own stats and report progress periodically. Levels on
   the replayed task prefix were already counted by task generation."
  (when (>= level (length *bt-forced-prefix*))
    (let ((stats (bt-worker.stats *bt-worker*)))
      (ws-inc-cycles stats)
      (ws-inc-states stats 1)
      (ws-update-max-depth stats (1+ level))
      (when (>= (incf (bt-worker.cycles-since-report *bt-worker*)) *bound-refresh-interval*)
        (setf (bt-worker.cycles-since-report *bt-worker*) 0)
        (maybe-report-parallel-progress (bt-worker.id *bt-worker*)
                                        (bt-worker.queue *bt-worker*))))))


(defun register-parallel-solution-bt (solution)
  "Register a worker's SOLUTION under *BEST-SOLUTION-LOCK*. A solution arriving after
   the requested count (or a first solution) is already reached is dropped, so the
   result matches serial search."
  (sb-thread:with-mutex (*best-solution-lock*)
    (unless (and *solution-paths* (solution-count-reached-p))
      (when (report-solution-found-p)
        (bt:with-lock-held (*lock*)
          (report-solution-bt solution)))
      (record-solution-bt solution)
      (ws-inc-solutions (bt-worker.stats *bt-worker*))
      (when (eql *solution-type* 'first)
        (setf *first-solution-found* t)))))
