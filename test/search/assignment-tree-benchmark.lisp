;;; Load after Wouldwork. Loading this file performs no staging or searches.
(in-package :ww)

(defvar *assignment-tree-depth* 8)
(defvar *assignment-tree-facts* 40)
(defvar *assignment-tree-pre-calls* 0)
(defvar *assignment-tree-effect-calls* 0)

(defun assignment-tree-counted-function (function counter)
  (lambda (&rest args)
    (incf (symbol-value counter))
    (apply function args)))

(defun assignment-tree-prepare (algorithm)
  (let ((*standard-output* (make-broadcast-stream)))
    (%stage "assignment-tree")
    (setf *algorithm* algorithm
          *threads* 0 *tree-or-graph* 'tree *solution-type* 'count
          *depth-cutoff* 0 *randomize-search* nil *symmetry-pruning* nil
          *debug* 0 *probe* nil *branch* 0)
    (refresh))
  (assert (not (fboundp 'heuristic?)))
  (assert (= *assignment-tree-facts*
             (length (list-database (problem-state.idb *start-state*))))))

(defun assignment-tree-check-result ()
  (assert (= *solution-count* (expt 3 *assignment-tree-depth*)))
  (assert (null *solution-paths*))
  (assert (= (solution.depth *count-example*) *assignment-tree-depth*))
  (when (eq *algorithm* 'backtracking)
    (assert (null *choice-stack*))
    (assert (equalp (problem-state.idb *start-state*)
                    (problem-state.idb *backtrack-state*)))))

(defun assignment-tree-warmup (algorithm)
  "One instrumented warm-up; restore translated functions even on failure."
  (assignment-tree-prepare algorithm)
  (let* ((action (first *actions*))
         (pre-name (action.pre-defun-name action))
         (effect-name (action.eff-defun-name action))
         (pre (symbol-function pre-name))
         (effect (symbol-function effect-name))
         (*assignment-tree-pre-calls* 0)
         (*assignment-tree-effect-calls* 0)
         (expected (* 3 (/ (1- (expt 3 *assignment-tree-depth*)) 2))))
    (unwind-protect
        (progn
          (setf (symbol-function pre-name)
                (assignment-tree-counted-function pre '*assignment-tree-pre-calls*)
                (symbol-function effect-name)
                (assignment-tree-counted-function effect '*assignment-tree-effect-calls*))
          (let ((*standard-output* (make-broadcast-stream))
                (*trace-output* (make-broadcast-stream)))
            (solve))
          (assignment-tree-check-result)
          (assert (= expected *assignment-tree-pre-calls*
                              *assignment-tree-effect-calls*))
          (list :algorithm algorithm :preconditions *assignment-tree-pre-calls*
                :effects *assignment-tree-effect-calls* :goals *solution-count*))
      (setf (symbol-function pre-name) pre
            (symbol-function effect-name) effect))))

(defun assignment-tree-measure (algorithm round)
  "Measure SOLVE only, including its initialization and suppressed reporting."
  (assignment-tree-prepare algorithm)
  (sb-ext:gc :full t)
  (let* ((*standard-output* (make-broadcast-stream))
         (*trace-output* (make-broadcast-stream))
         (bytes (sb-ext:get-bytes-consed))
         (cpu (get-internal-run-time))
         (start (get-internal-real-time)))
    (solve)
    (let ((row (list :algorithm algorithm :round round
                     :seconds (/ (- (get-internal-real-time) start)
                                 (float internal-time-units-per-second 1d0))
                     :cpu-seconds (/ (- (get-internal-run-time) cpu)
                                     (float internal-time-units-per-second 1d0))
                     :bytes (- (sb-ext:get-bytes-consed) bytes)
                     :goals *solution-count*)))
      (assignment-tree-check-result)
      row)))

(defun assignment-tree-median (rows algorithm key)
  (second (sort (loop for row in rows
                     when (eq algorithm (getf row :algorithm))
                       collect (getf row key)) #'<)))

(defun benchmark-assignment-tree (&key (facts 40) (depth 8))
  "One warm-up and exactly three measured runs per algorithm; leaves fixture staged."
  (let ((*assignment-tree-facts* facts)
        (*assignment-tree-depth* depth)
        (rows nil)
        (checks nil))
    (dolist (algorithm '(depth-first backtracking))
      (push (assignment-tree-warmup algorithm) checks))
    (loop for round from 1 to 3 do
      (dolist (algorithm (if (oddp round)
                            '(depth-first backtracking)
                            '(backtracking depth-first)))
        (push (assignment-tree-measure algorithm round) rows)))
    (setf rows (nreverse rows) checks (nreverse checks))
    (format t "~&Assignment tree: depth=~D facts=~D; serial CSP/tree/COUNT.~%" depth facts)
    (format t "Warm-up work checks: ~S~%" checks)
    (dolist (row rows) (format t "~S~%" row))
    (let ((dfs (assignment-tree-median rows 'depth-first :seconds))
          (bt (assignment-tree-median rows 'backtracking :seconds)))
      (dolist (algorithm '(depth-first backtracking))
        (format t "~A medians: ~,6F s elapsed, ~,6F s CPU, ~:D bytes.~%"
                algorithm (assignment-tree-median rows algorithm :seconds)
                (assignment-tree-median rows algorithm :cpu-seconds)
                (assignment-tree-median rows algorithm :bytes)))
      (if (plusp bt)
          (format t "Elapsed speedup DFS/BT: ~,3Fx~%" (/ dfs bt))
          (format t "Elapsed timer resolution insufficient for a ratio.~%")))
    (list :facts facts :depth depth :warmup-checks checks :runs rows)))
