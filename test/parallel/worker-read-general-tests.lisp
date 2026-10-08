;;; Explicit focused verification. Loading performs no staging or search.
(in-package :ww)

(defun worker-general-copy-check ()
  (let ((context nil))
    (unwind-protect
        (progn
          (setf context (begin-worker-read-phase 2))
          (assert context)
          (snapshot-test-view-ownership context)
          (snapshot-test-writer-guards context))
      (end-worker-read-phase context)))
  (assert (null *worker-read-phase*))
  t)

(defun worker-general-print-key (object)
  (let ((*package* (find-package :ww)) (*print-pretty* nil)
        (*print-level* nil) (*print-length* nil) (*print-case* :upcase))
    (prin1-to-string object)))

(defun worker-general-signature (state)
  (append (snapshot-test-state-signature state)
          (list (problem-state.hidb state) (problem-state.happenings state))))

(defun worker-general-expansion (seed)
  (mapcar #'worker-general-signature
          (sort (snapshot-test-expand seed nil) #'string<
                :key (lambda (state)
                       (worker-general-print-key
                         (list (problem-state.name state) (problem-state.instantiations state)))))))

(defun worker-general-hash-expansion (seed)
  (list (let ((*symmetry-pruning* nil)) (worker-general-expansion seed))
        (let ((*symmetry-pruning* t)) (worker-general-expansion seed))))

(defun worker-general-expansion-check ()
  "One start-state expansion per hash mode and worker. No search loop."
  (let* ((expected (worker-general-hash-expansion *start-state*))
         (tables (make-array 2)) (old-shutdown *shutdown-requested*))
    (unwind-protect
        (progn
          (setf *shutdown-requested* nil)
          (call-with-parallel-search-lifetime
            (lambda ()
              (run-parallel-worker-group (make-new-task-queue) 2
                :function
                (lambda (id queue)
                  (declare (ignore queue))
                  (setf (aref tables id)
                        (mapcar #'symbol-value (current-worker-read-memo-symbols)))
                  (assert (equalp expected (worker-general-hash-expansion *start-state*)))))))
          (loop for policy in *worker-read-memo-policies*
                for a in (aref tables 0) for b in (aref tables 1)
                when (eq (cdr policy) :empty-table) do (assert (not (eq a b)))))
      (setf *shutdown-requested* old-shutdown)))
  (format t "~&GENERAL EXPANSION PASS ~S~%" *problem-name*)
  t)

(defun worker-general-stage-check (path &optional expand-p)
  (%stage path)
  (assert *worker-read-snapshots*)
  (validate-worker-read-snapshot-mode)
  (when (zerop *threads*)
    (assert (null (worker-read-snapshots-active-p))))
  (ww-set *threads* 2)
  (load (asdf:system-relative-pathname :wouldwork "test/parallel/worker-read-snapshot-tests.lisp"))
  (worker-general-copy-check)
  (when expand-p (worker-general-expansion-check))
  (format t "~&GENERAL STAGE/COPY PASS ~A memos=~D~%" path (length *worker-read-memo-symbols*))
  (finish-output)
  t)
