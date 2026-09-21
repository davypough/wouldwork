;;; Explicit REPL diagnostic. Loading this file does not run a search.
(in-package :ww)

(defvar *contention-row* nil)
(defvar *contention-probe-active* nil)
(defvar *contention-results* nil)
(defparameter *contention-source*
  (merge-pathnames "ww-parallel.lisp" *load-truename*))

(defun contention-seconds (ticks)
  (/ ticks (float internal-time-units-per-second)))

(defun contention-call-locked (mutex function)
  "Time acquisition and protected work separately, every 1024th acquisition.
Measurements include scheduling delays; they are not CPU execution times."
  (let ((row *contention-row*))
    (if (or (null row) (not (zerop (mod (incf (getf row :locks)) 1024))))
        (sb-thread:with-mutex (mutex) (funcall function))
        (let ((start (get-internal-real-time)))
          (sb-thread:with-mutex (mutex)
            (let ((acquired (get-internal-real-time)))
              (incf (getf row :samples))
              (incf (getf row :wait-ticks) (- acquired start))
              (unwind-protect (funcall function)
                (incf (getf row :hold-ticks)
                      (- (get-internal-real-time) acquired)))))))))

(defun contention-successor-function ()
  "Compile only the successor function with a diagnostic local lock macro.
The production macro and on-disk source are never changed."
  (with-open-file (stream *contention-source*)
    (let ((*package* (find-package :ww)))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            when (and (consp form) (eq (first form) 'defun)
                      (eq (second form) 'worker-process-successors-phase1))
              return
              (compile nil
                `(lambda ,(third form)
                   ,@(remove-if-not
                       (lambda (item) (and (consp item) (eq (car item) 'declare)))
                       (cdddr form))
                   (macrolet
                       ((with-closed-shard-lock ((state) &body body)
                          `(contention-call-locked (closed-shard-lock ,state)
                             (lambda () ,@body))))
                     (block worker-process-successors-phase1
                       ,@(remove-if
                           (lambda (item)
                             (or (stringp item)
                                 (and (consp item) (eq (car item) 'declare))))
                           (cdddr form))))))
            finally (error "Successor function not found in ~A" *contention-source*)))))

(defun contention-worker-wrapper (original rows)
  (lambda (id queue)
    (let* ((*contention-row*
             (list :worker id :locks 0 :samples 0 :wait-ticks 0 :hold-ticks 0
                   :queue-ticks 0 :queue-calls 0 :terminal-queue-ticks 0
                   :worker-ticks 0))
           (start (get-internal-real-time)))
      (setf (aref rows id) *contention-row*)
      (unwind-protect (funcall original id queue)
        (setf (getf *contention-row* :worker-ticks)
              (- (get-internal-real-time) start))))))

(defun contention-queue-wrapper (original)
  (lambda (queue)
    (if (null *contention-row*)
        (funcall original queue)
        (let* ((start (get-internal-real-time))
               (item (funcall original queue))
               (ticks (- (get-internal-real-time) start)))
          (incf (getf *contention-row* :queue-calls))
          (incf (getf *contention-row* :queue-ticks) ticks)
          (unless item (incf (getf *contention-row* :terminal-queue-ticks) ticks))
          item))))

(defun contention-report-row (row)
  (when row
    (let ((stats (get-worker-stats (getf row :worker))))
      (append row
        (list :worker-seconds (contention-seconds (getf row :worker-ticks))
              :queue-seconds (contention-seconds (getf row :queue-ticks))
              :terminal-queue-seconds
              (contention-seconds (getf row :terminal-queue-ticks))
              :sampled-wait-seconds (contention-seconds (getf row :wait-ticks))
              :sampled-hold-seconds (contention-seconds (getf row :hold-ticks))
              :states (ws-states-processed stats)
              :cycles (ws-program-cycles stats)
              :duplicates (ws-repeated-states stats)
              :donations (ws-donation-events stats))))))

(defun run-contention-probe (depth)
  "One search at explicit DEPTH and the current worker count, objective EVERY.
Run only while workers are idle. Restores functions and controls on exit.
Queue time includes queue locking, task retrieval, and terminal waiting.
Shard times are raw samples, not totals. No automatic replay or second solve."
  (assert (and (not *contention-probe-active*) (not *parallel-search-active*)
               (plusp *threads*) (eq *algorithm* 'depth-first)
               (eq *tree-or-graph* 'graph) (zerop *debug*) (null *probe*)
               (not *ww-timing-enabled*) (integerp depth) (plusp depth)))
  (let* ((*contention-probe-active* t)
         (rows (make-array *threads* :initial-element nil))
         (names '(parallel-worker tq-pop-blocking worker-process-successors-phase1))
         (originals (mapcar (lambda (name) (cons name (symbol-function name))) names))
         (instrumented (contention-successor-function))
         (old-depth *depth-cutoff*) (old-objective *solution-type*)
         (old-random *randomize-search*) (elapsed nil) (states nil))
    (unwind-protect
        (progn
          (setf *depth-cutoff* depth *solution-type* 'every *randomize-search* nil)
          (setf (symbol-function 'parallel-worker)
                (contention-worker-wrapper (cdr (assoc 'parallel-worker originals)) rows)
                (symbol-function 'tq-pop-blocking)
                (contention-queue-wrapper (cdr (assoc 'tq-pop-blocking originals)))
                (symbol-function 'worker-process-successors-phase1) instrumented)
          (multiple-value-setq (elapsed states) (timed-silent-solve)))
      (dolist (entry originals) (setf (symbol-function (car entry)) (cdr entry)))
      (setf *depth-cutoff* old-depth *solution-type* old-objective
            *randomize-search* old-random))
    (let ((result
            (list :problem *problem-name* :threads *threads* :depth depth
                  :objective 'every :snapshots *worker-read-snapshots*
                  :shards *num-closed-shards* :sample-interval 1024
                  :clock-ticks-per-second internal-time-units-per-second
                  :elapsed-seconds elapsed :states states
                  :symmetry-duplicates *symmetric-duplicates-pruned*
                  :outcome (search-outcome-status *last-search-outcome*)
                  :workers (map 'list #'contention-report-row rows))))
      (push result *contention-results*)
      (format t "~&CONTENTION PROBE BEGIN~%~S~%CONTENTION PROBE END~%" result)
      result)))
