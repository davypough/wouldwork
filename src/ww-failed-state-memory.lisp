;;; Filename: ww-failed-state-memory.lisp

;;; Failed-state memory for depth-first tree search (*failed-state-memory* t).
;;; A state whose subtree is searched to the current depth limit without reaching
;;; a goal is recorded with the moves that were left at that point.  When the same
;;; state is reached again with no more moves left, it is skipped.
;;; The key is the state's propositions plus each happening's event index, direction,
;;; and time to its next event (relative, not absolute), so repeats are found at
;;; different depths and times.  The check against states on the current path is
;;; turned off: a node on the path is never recorded, so its repeat is expanded
;;; again with fewer moves left, and the required depth cutoff ends the descent.
;;; No recorded failure then depends on the path.  A subtree partly donated to
;;; another worker is not recorded.  Serial search uses one table; each parallel
;;; worker keeps its own, with no locking.


(in-package :ww)


(defun validate-failed-state-memory-settings ()
  "Signals an error when *failed-state-memory* is combined with an unsupported setting."
  (unless (> *depth-cutoff* 0)
    (error "*failed-state-memory* requires a *depth-cutoff* (it replaces the check against states on the current path)."))
  (unless (eql *tree-or-graph* 'tree)
    (error "*failed-state-memory* requires *tree-or-graph* = TREE (graph search already closes states)."))
  (unless (eql *problem-type* 'planning)
    (error "*failed-state-memory* requires *problem-type* = PLANNING."))
  (unless (eql *algorithm* 'depth-first)
    (error "*failed-state-memory* requires *algorithm* = DEPTH-FIRST."))
  (unless (or (member *solution-type* '(first every count min-length))
              (typep *solution-type* 'fixnum))
    (error "*failed-state-memory* supports *solution-type* first, every, count, N, or min-length, not ~S."
           *solution-type*))
  (when *auto-wait*
    (error "*failed-state-memory* cannot be combined with *auto-wait*; use the spec's wait action."))
  (when (or *solution-validators*
            *goal-chain-candidate-rejector*
            (search-prefix-validation-enabled-p)
            (some (lambda (entry)
                    (funcall (symbol-function (search-successor-pruner.enabled-p entry))))
                  *search-successor-pruners*))
    (error "*failed-state-memory* cannot be combined with path-dependent validators or successor policies.")))


(defun reset-failed-state-memory ()
  "Clears the serial table, path, and counters before a search."
  (setf *failed-state-table* (make-hash-table :test 'equal)
        *failed-state-path* nil
        *failed-states-recorded* 0
        *failed-states-skipped* 0))


(defun failed-state-skip-p (node)
  "Whether NODE's state was already proven to fail with at least as many moves left."
  (let* ((state (node.state node))
         (entry (find (problem-state.idb state)
                      (gethash (failed-state-key state) *failed-state-table*)
                      :key #'car :test #'equalp)))
    (when (and entry
               (>= (cdr entry) (- (failed-state-limit) (node.depth node))))
      (increment-global *failed-states-skipped* 1)
      t)))


(defun close-failed-state-path (depth)
  "Pops the path entries at DEPTH or deeper, whose subtrees are now finished, and
   records each one that found no goal and whose failure does not depend on its path."
  (iter (while (and *failed-state-path*
                    (>= (node.depth (car (first *failed-state-path*))) depth)))
        (let ((entry (pop *failed-state-path*)))
          (unless (or (cdr entry) (node.goal-below (car entry)))
            (record-failed-state (car entry))))))


(defun taint-failed-state-path ()
  "Marks every path entry as unrecordable, after part of its subtree was donated to
   another worker."
  (dolist (entry *failed-state-path*)
    (setf (cdr entry) t)))


(defun mark-goal-ancestors (node)
  "Marks NODE and its ancestors as having a goal below them, so none is recorded as failed."
  (iter (for ancestor initially node then (node.parent ancestor))
        (while (and ancestor (not (node.goal-below ancestor))))
        (setf (node.goal-below ancestor) t)))


(defun record-failed-state (node)
  "Records NODE's state as failed with the moves left under the current depth limit."
  (let* ((state (node.state node))
         (moves-left (- (failed-state-limit) (node.depth node)))
         (key (failed-state-key state))
         (entry (find (problem-state.idb state) (gethash key *failed-state-table*)
                      :key #'car :test #'equalp)))
    (if entry
      (setf (cdr entry) (max (cdr entry) moves-left))
      (progn (push (cons (problem-state.idb state) moves-left)
                   (gethash key *failed-state-table*))
             (increment-global *failed-states-recorded* 1)))))


(defun failed-state-limit ()
  "The deepest depth at which a goal still counts: the depth cutoff, tightened under
   min-length to one less than the best solution's length."
  (let ((limit (if (> *depth-cutoff* 0) *depth-cutoff* most-positive-fixnum)))
    (cond ((not (eql *solution-type* 'min-length)) limit)
          ((> *threads* 0) (min limit (1- *best-bound*)))
          (*solution-paths* (min limit (1- (solution.depth (first *solution-paths*)))))
          (t limit))))


(defun failed-state-key (state)
  "The table key: the idb hash, whether the state was reached by a wait (a second wait
   is refused, so it has fewer moves), and each happening as
   (object index time-to-next direction), sorted by object.  The idb itself is
   compared with equalp within the key's bucket."
  (let ((now (problem-state.time state)))
    (list* (ensure-idb-hash state)
           (eql (problem-state.name state) 'wait)
           (sort (mapcar (lambda (happening)
                           (destructuring-bind (object (index time direction)) happening
                             (list object index (- time now) direction)))
                         (problem-state.happenings state))
                 #'string< :key #'first))))
