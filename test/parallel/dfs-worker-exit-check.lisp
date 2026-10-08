;;; Explicit focused check of the parallel DFS worker exit protocol. Loading performs no
;;; staging or search; run (dfs-worker-exit-check) against any staged problem. Load it
;;; inside WITH-COMPILATION-UNIT to defer the forward-reference style warnings.
;;; Regression: a worker that stopped after a first solution returned without going back
;;; to the queue, so TQ-POP-BLOCKING never saw every worker waiting and an idle worker
;;; (with the coordinator's join behind it) waited forever. Each run holds one real
;;; PARALLEL-WORKER inside a stubbed WORKER-LOCAL-DFS until the other is blocked on the
;;; empty queue, then sets *FIRST-SOLUTION-FOUND*. The pre-fix worker hung on every run.
(in-package :ww)


(defparameter *dfs-exit-registered* nil
  "Signaled once per worker registration.")


(defparameter *dfs-exit-busy* nil
  "Signaled when a worker enters the stubbed search.")


(defparameter *dfs-exit-release* nil
  "Signaled to let the stubbed search return.")


(defparameter *dfs-exit-original-register* nil
  "The real TQ-REGISTER-WORKER while the check runs.")


(defun dfs-worker-exit-check (&optional (runs 20))
  "Run RUNS stop-while-idle cases; assert that every one completes."
  (let ((original-search (symbol-function 'worker-local-dfs))
        (results nil))
    (setf *dfs-exit-original-register* (symbol-function 'tq-register-worker))
    (unwind-protect
        (progn
          (setf (symbol-function 'worker-local-dfs) #'dfs-exit-stub-search
                (symbol-function 'tq-register-worker) #'dfs-exit-stub-register)
          (dotimes (i runs)
            (push (dfs-exit-check-run) results)))
      (setf (symbol-function 'worker-local-dfs) original-search
            (symbol-function 'tq-register-worker) *dfs-exit-original-register*)
      (reset-parallel-control-flags))
    (format t "~&DFS WORKER EXIT ~:[FAIL~;PASS~] ~D/~D runs completed~%"
            (every (lambda (result) (eq result :completed)) results)
            (count :completed results) runs)
    (assert (every (lambda (result) (eq result :completed)) results))
    t))


(defun dfs-exit-check-run ()
  "Two real PARALLEL-WORKERs share one queued task. Returns :COMPLETED; :HUNG when the
   group has not finished within 10 seconds (it is then shut down and reaped); or :ERROR
   when the group signaled one, which is printed."
  (setf *dfs-exit-registered* (sb-thread:make-semaphore)
        *dfs-exit-busy* (sb-thread:make-semaphore)
        *dfs-exit-release* (sb-thread:make-semaphore))
  (reset-parallel-control-flags)
  (initialize-worker-stats 2)
  (let ((queue (make-new-task-queue)))
    (tq-push queue (make-node))  ; PARALLEL-WORKER may check the NODE type of its task
    (tq-signal-done queue)
    (let* ((group (bt:make-thread (lambda () (dfs-exit-run-group queue)) :name "dfs-exit-group"))
           (result (sb-thread:join-thread group :timeout 10 :default :hung)))
      (when (eq result :hung)
        (request-parallel-worker-shutdown queue)
        (sb-thread:join-thread group :default nil))
      (when (typep result 'error)
        (format t "~&DFS WORKER EXIT error: ~A~%" result))
      (cond ((eq result :hung) :hung)
            ((typep result 'error) :error)
            (t :completed)))))


(defun dfs-exit-run-group (queue)
  "Return the group's error as a value: a background thread cannot reach the debugger."
  (handler-case
      (run-parallel-worker-group queue 2 :after-start (lambda () (dfs-exit-after-start queue)))
    (error (condition) condition)))


(defun dfs-exit-after-start (queue)
  "Stop the search while one worker is searching and the other waits on the empty queue."
  (assert (sb-thread:wait-on-semaphore *dfs-exit-registered* :n 2 :timeout 5))
  (assert (sb-thread:wait-on-semaphore *dfs-exit-busy* :timeout 5))
  (dfs-exit-wait-for-one-waiting queue)
  (setf *first-solution-found* t)
  (sb-thread:signal-semaphore *dfs-exit-release*))


(defun dfs-exit-wait-for-one-waiting (queue)
  "Return once exactly one worker is active, i.e. the other is blocked in TQ-POP-BLOCKING."
  (loop repeat 500
        when (sb-thread:with-mutex ((tq-mutex queue)) (= (tq-active-workers queue) 1))
          return t
        do (sleep 0.01)
        finally (error "No worker reached the queue wait within 5 seconds.")))


(defun dfs-exit-stub-search (task-node worker-id stats task-queue)
  "Stands in for WORKER-LOCAL-DFS: stays searching until the check releases it."
  (declare (ignore task-node worker-id stats task-queue))
  (sb-thread:signal-semaphore *dfs-exit-busy*)
  (sb-thread:wait-on-semaphore *dfs-exit-release*))


(defun dfs-exit-stub-register (queue)
  "Stands in for TQ-REGISTER-WORKER: registers, then reports the registration."
  (funcall *dfs-exit-original-register* queue)
  (sb-thread:signal-semaphore *dfs-exit-registered*))
