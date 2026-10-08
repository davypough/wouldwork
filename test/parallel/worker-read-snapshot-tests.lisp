;;; Explicitly loaded, user-run tests. No SOLVE, DFS or root-task generation.
(in-package :ww)

(define-condition snapshot-test-failure (error) ())

(defun snapshot-test-signals (type function)
  (assert (handler-case (progn (funcall function) nil)
            (error (condition)
              (if (typep condition type) t (error condition))))))

(defun snapshot-test-table-settings (source copy)
  (assert (not (eq source copy)))
  (assert (eq (hash-table-test source) (hash-table-test copy)))
  (assert (= (hash-table-size source) (hash-table-size copy)))
  (assert (eql (hash-table-rehash-size source) (hash-table-rehash-size copy)))
  (assert (eql (hash-table-rehash-threshold source) (hash-table-rehash-threshold copy)))
  (assert (not (sb-ext:hash-table-synchronized-p copy))))                          ; CHANGED

(defun snapshot-test-copy-policy ()
  (let* ((table (make-hash-table :test #'equal :synchronized t))
         (key (list 'key (copy-seq "name")))
         (value (list (list 'controller) (copy-seq "payload"))))
    (setf (gethash key table) value)
    (let* ((copy (worker-read-copy-table table))
           (owned (gethash (copy-tree key) copy)))
      (snapshot-test-table-settings table copy)
      (assert (equalp table copy))
      (setf (caar owned) 'changed (char (second owned) 0) #\X)
      (assert (eq (caar value) 'controller))
      (assert (string= (second value) "payload"))
      (let ((copied-key (loop for k being the hash-keys of copy return k)))
        (assert (not (eq copied-key key)))
        ;; Remove before changing a key: mutating a resident EQUAL key would
        ;; violate the hash-table contract even in this disposable test.
        (remhash copied-key copy)
        (setf (char (second copied-key) 0) #\X))
      (assert (string= (second key) "name"))))
  (dolist (value (list #(a b) #*01 (make-hash-table)))
    (snapshot-test-signals 'error (lambda () (worker-read-copy-value value))))
  (let ((table (make-hash-table :test #'eql)))
    (setf (gethash (list 'identity-key) table) t)
    (snapshot-test-signals 'error (lambda () (worker-read-copy-table table))))
  (let ((cycle (list 'cycle)))
    (setf (cdr cycle) cycle)
    (snapshot-test-signals 'error (lambda () (worker-read-copy-value cycle))))
  t)

(defun snapshot-test-compiled-routing ()
  ;; Deliberately incomplete DISPOSABLE views prove the installed generated
  ;; function uses both selectors, not merely that two equal tables agree.
  (let ((view (make-current-worker-read-view)))
    (assert (= (funcall (symbol-function 'fixed-base) *start-state* 'location12) 3/2))
    (clrhash (worker-read-view-static view))
    (call-with-worker-read-view view
      (lambda ()
        (assert (zerop (funcall (symbol-function 'fixed-base) *start-state* 'location12)))))
    (clrhash (worker-read-view-codes view))
    (snapshot-test-signals 'worker-read-snapshot-error
      (lambda ()
        (call-with-worker-read-view view
          (lambda () (funcall (symbol-function 'fixed-base) *start-state* 'location12)))))))

(defun snapshot-test-early-exit ()
  ;; The coordinator lifetime also surrounds the no-worker/root completion path.
  (assert (eq (call-with-parallel-search-lifetime
                (lambda ()
                  (assert (not *worker-read-phase*))
                  :no-workers))
              :no-workers))
  (snapshot-test-signals 'snapshot-test-failure
    (lambda ()
      (call-with-parallel-search-lifetime
        (lambda () (error 'snapshot-test-failure)))))
  (assert (not *parallel-search-active*))
  (assert (not *worker-read-phase*)))

(defun snapshot-test-view-ownership (context)
  (let* ((views (worker-read-context-views context))
         (first (first views)) (second (second views)))
    (assert (= (length views) 2))
    (snapshot-test-table-settings *static-idb* (worker-read-view-static first))
    (snapshot-test-table-settings *constant-integers* (worker-read-view-codes first))
    (snapshot-test-table-settings *integer-constants* (worker-read-view-names first))       ; CHANGED
    (assert (not (eq (worker-read-view-static first) (worker-read-view-static second))))
    (assert (not (eq (worker-read-view-codes first) (worker-read-view-codes second))))
    (assert (not (eq (worker-read-view-names first) (worker-read-view-names second))))     ; CHANGED
    (loop for name in (worker-read-view-memo-symbols first)
          for a in (worker-read-view-memo-tables first)
          for b in (worker-read-view-memo-tables second)
          do (ecase (cdr (assoc name *worker-read-memo-policies*))
               (:empty-table
                (snapshot-test-table-settings (symbol-value name) a)
                (assert (not (eq a b)))
                (assert (zerop (hash-table-count a))))
               (:nil (assert (and (null a) (null b))))))
    (call-with-worker-read-view
      first
      (lambda ()
        (loop for name in (worker-read-view-memo-symbols first)
              for table in (worker-read-view-memo-tables first)
              do (assert (eq table (symbol-value name))))))))

(defun snapshot-test-writer-guards (context)
  ;; These are real entry points. Invalid/disposable arguments must never be
  ;; inspected or installed: the dedicated phase error must occur first.
  (dolist (form '((convert-databases-to-integers) (do-integer-conversion)
                 (associate-objects-with-integers) (compile-all-functions)
                 (register-dynamic-object agent1 agent)
                 (ww-reset) (reset-user-syms nil) (reset-global-hash-tables)
                 (ensure-problem-staged nil) (refresh) (%stage "unused")
                 (load-problem "unused") (init)
                 (reset-problem-parameters-to-defaults)
                 (predeclare-type-names nil) (predeclare-optional-type-names nil)
                 (install-types nil) (install-optional-types nil)
                 (install-dynamic-relations nil) (install-derived-relations nil)
                 (install-static-relations nil) (install-complementary-relations nil)
                 (install-happening nil nil) (install-query nil nil nil)
                 (install-update nil nil nil) (install-constraint nil)
                 (install-action nil nil nil nil nil nil)
                 (install-init-action nil nil nil nil nil nil)
                 (install-init nil) (install-goal nil)
                 (register-worker-read-memo unused :nil)
                 (register-worker-read-configuration unused)
                 (register-min-steps-remaining-contributor unused)
                 (register-candidate-state-screener unused unused)
                 (register-search-successor-pruner unused unused)
                 (register-search-prefix-validator unused unused)
                 (register-symmetry-coupling unused)
                 (register-goal-chaining-policy unused unused)
                 (register-goal-chaining-checkpoint-extension unused unused unused)
                 (register-solution-validator unused)
                 (register-solution-report-printer unused)
                 (register-relaxed-hmax-model-builder unused)))
    (snapshot-test-signals
      'worker-read-snapshot-error
      (lambda () (apply (symbol-function (first form)) (rest form))))
    (verify-worker-read-context context))
  (snapshot-test-signals 'worker-read-snapshot-error
    (lambda () (eval '(ww-set *worker-read-snapshots* nil))))
  (snapshot-test-signals 'worker-read-snapshot-error
    (lambda () (do-init-action-updates *start-state*)))
  (dolist (table (list *static-idb* *static-db*
                       (worker-read-view-static (first (worker-read-context-views context)))))
    (let ((*worker-static-read-view*
            (worker-read-view-static (first (worker-read-context-views context)))))
      (snapshot-test-signals 'worker-read-snapshot-error
        (lambda () (fold-store 123 t table nil)))
      (snapshot-test-signals 'worker-read-snapshot-error
        (lambda () (fold-remove 123 table nil)))))
  (let ((missing (gensym "SNAPSHOT-MISSING-")))
    (snapshot-test-signals 'worker-read-snapshot-error
      (lambda () (convert-to-integer (list missing))))
    (snapshot-test-signals 'worker-read-snapshot-error
      (lambda () (convert-fluentless-prop-to-integer (list missing nil) '(1)))))
  (verify-worker-read-context context))

(defun snapshot-test-clear-state-hashes (state)
  (setf (problem-state.idb-hash state) nil
        (problem-state.fixed-idb-hash state) nil
        (problem-state.symmetry-idb state) nil
        (problem-state.canonical-symmetry-form state) :uncached
        (problem-state.canonical-form-hash state) nil)
  state)

(defun snapshot-test-copy-state (state)
  (let ((copy (copy-problem-state state)))
    (setf (problem-state.idb copy) (worker-read-copy-table (problem-state.idb state)))
    (snapshot-test-clear-state-hashes copy)))

(defun snapshot-test-seeds ()
  (loop for gates-open in '(nil t)
        append (loop for active in '(nil t)
                     collect
                     (let* ((state (snapshot-test-copy-state *start-state*))
                            (db (problem-state.idb state)))
                       (dolist (gate (gethash 'gate *types*))
                         (let ((key (convert-to-integer (list 'open gate))))
                           (if gates-open (setf (gethash key db) t) (remhash key db))))
                       (dolist (receiver (gethash 'receiver *types*))
                         (let ((key (convert-to-integer (list 'active receiver))))
                           (if active (setf (gethash key db) t) (remhash key db))))
                       state))))

(defun snapshot-test-update (function seed view)
  (let* ((state (snapshot-test-copy-state seed))
         (split-p (use-canonical-symmetry-p)))
    (ensure-idb-hash state)
    (let ((*detect-propagated-changes* t) (*propagated-state-changed* nil)
          (*idb-hash-acc* (unless split-p (problem-state.idb-hash state)))
          (*fixed-idb-hash-acc* (when split-p (problem-state.fixed-idb-hash state)))
          (*symmetry-idb-acc* (when split-p (copy-idb (problem-state.symmetry-idb state))))
          (*symmetry-idb-touched-p* nil))
      (let* ((result (call-with-worker-read-view
                       view (lambda () (multiple-value-list (funcall function state)))))
             (fresh (snapshot-test-copy-state state)))
        (ensure-idb-hash fresh)
        (if split-p
            (progn
              (assert (= *fixed-idb-hash-acc* (problem-state.fixed-idb-hash fresh)))
              (assert (equalp *symmetry-idb-acc* (problem-state.symmetry-idb fresh))))
            (assert (= *idb-hash-acc* (problem-state.idb-hash fresh))))
        (list result *propagated-state-changed* (problem-state.idb state)
              (problem-state.idb-hash fresh))))))

(defun snapshot-test-updates (seeds view)
  (dolist (name '(update-gate-status! update-receiver-status! propagate-changes!))
    (dolist (seed seeds)
      (dotimes (pass 2)
        (let ((expected (snapshot-test-update (symbol-function name) seed nil))
              (actual (snapshot-test-update (symbol-function name) seed view)))
          (assert (equalp expected actual))
          ;; Second pass is the settled result of this particular updater.
          (setf seed (snapshot-test-copy-state seed)
                (problem-state.idb seed) (third expected))))))
  (let ((lookup (compile nil
                 (subst-int-code
                   '(lambda (?gate) (gethash (list 'always-true ?gate) *static-db*))))))
    (assert (equal (multiple-value-list (funcall lookup 'gate1)) '(nil nil)))
    (call-with-worker-read-view view
      (lambda ()
        (assert (equal (multiple-value-list (funcall lookup 'gate1)) '(nil nil)))
        (snapshot-test-signals 'worker-read-snapshot-error
          (lambda () (funcall lookup (gensym "MISSING-"))))))))

(defun snapshot-test-state-signature (state)
  (let ((fresh (snapshot-test-copy-state state)))
    (ensure-idb-hash state)
    (ensure-idb-hash fresh)
    (assert (= (problem-state.idb-hash state) (problem-state.idb-hash fresh)))
    (assert (eql (problem-state.fixed-idb-hash state)
                 (problem-state.fixed-idb-hash fresh)))
    (list (problem-state.name state) (problem-state.instantiations state)
          (problem-state.time state) (problem-state.value state)
          (problem-state.heuristic state) (problem-state.idb state)
          (problem-state.idb-hash state) (problem-state.symmetry-idb state)
          (problem-state.canonical-symmetry-form state))))

(defun snapshot-test-expand (seed view)
  (call-with-worker-read-view
    view
    (lambda ()
      (let ((state (snapshot-test-copy-state seed)))
        (ensure-idb-hash state)
        (expand (make-node :state state :depth 0))))))

(defun snapshot-test-expansions (seeds view)
  (loop for seed in seeds
        for expected = (mapcar #'snapshot-test-state-signature
                               (snapshot-test-expand seed nil))
        for actual = (mapcar #'snapshot-test-state-signature
                             (snapshot-test-expand seed view))
        do (assert (equalp expected actual))
        sum (length actual)))

(defstruct snapshot-test-run
  mode enabled (queue (make-new-task-queue)) created (calls 0) (views 0)
  (ready (sb-thread:make-semaphore :count 0)))

(defun snapshot-test-make-view (run)
  (incf (snapshot-test-run-views run))
  (when (and (eq (snapshot-test-run-mode run) :preparation-error)
             (= (snapshot-test-run-views run) 2))
    (error 'snapshot-test-failure))
  (make-current-worker-read-view))

(defun snapshot-test-make-thread (run function name)
  (incf (snapshot-test-run-calls run))
  (when (and (eq (snapshot-test-run-mode run) :partial-start)
             (= (snapshot-test-run-calls run) 2))
    (error 'snapshot-test-failure))
  (let ((thread (bt:make-thread function :name name)))
    (push thread (snapshot-test-run-created run))
    thread))

(defun snapshot-test-worker (run id queue)
  (assert (eql (not (null *worker-code-read-view*)) (snapshot-test-run-enabled run)))
  (assert (eql (not (null *worker-read-phase*)) (snapshot-test-run-enabled run)))
  (sb-thread:signal-semaphore (snapshot-test-run-ready run))
  (when (and (eq (snapshot-test-run-mode run) :worker-error) (zerop id))
    (error 'snapshot-test-failure))
  (unless (member (snapshot-test-run-mode run) '(:normal :preparation-error))
    (tq-register-worker queue)
    (tq-pop-blocking queue)))

(defun snapshot-test-after-start (run)
  (when (member (snapshot-test-run-mode run) '(:cancel :coordinator-error :nonlocal-exit))
    (dotimes (i 2)
      (assert (sb-thread:wait-on-semaphore (snapshot-test-run-ready run) :timeout 10))))
  (case (snapshot-test-run-mode run)
    (:cancel (request-parallel-worker-shutdown (snapshot-test-run-queue run)))
    (:coordinator-error (error 'snapshot-test-failure))
    (:nonlocal-exit (throw 'snapshot-test-exit :expected))))

(defun snapshot-test-run-group (run)
  (run-parallel-worker-group
    (snapshot-test-run-queue run) 2
    :view-maker (lambda () (snapshot-test-make-view run))
    :maker (lambda (function &key name) (snapshot-test-make-thread run function name))
    :function (lambda (id queue) (snapshot-test-worker run id queue))
    :after-start (lambda () (snapshot-test-after-start run))))

(defun snapshot-test-lifecycle-case (mode enabled)
  (let ((*worker-read-snapshots* enabled)
        (run (make-snapshot-test-run :mode mode :enabled enabled))
        (old-shutdown *shutdown-requested*))
    (unwind-protect
        (progn
          (setf *shutdown-requested* nil)
          (let ((caught
                  (catch 'snapshot-test-exit
                    (handler-case
                        (call-with-parallel-search-lifetime
                          (lambda () (snapshot-test-run-group run)))
                      (snapshot-test-failure () :expected)))))
            (assert (eql (eq caught :expected)
                         (not (null (member mode '(:partial-start :worker-error :coordinator-error
                                                  :nonlocal-exit :preparation-error)))))))
          (assert (every (lambda (thread) (not (bt:thread-alive-p thread)))
                         (snapshot-test-run-created run)))
          (assert (null *worker-read-phase*))
          (assert (null *parallel-search-active*))
          (assert (null *worker-static-read-view*))
          (format t "~&SNAPSHOT LIFECYCLE ~S enabled=~S joined=~D PASS~%"
                  mode enabled (length (snapshot-test-run-created run))))
      (setf *shutdown-requested* old-shutdown))))

(defun snapshot-test-concurrent-reads (seeds)
  (let* ((expected (mapcar (lambda (seed)
                             (mapcar #'snapshot-test-state-signature
                                     (snapshot-test-expand seed nil))) seeds))
         (memos (loop for (name . policy) in *worker-read-memo-policies*
                      when (eq policy :empty-table) collect (symbol-value name)))
         (copies (mapcar #'worker-read-copy-table memos))
         (old-shutdown *shutdown-requested*))
    (unwind-protect
        (progn
          (setf *shutdown-requested* nil)
          (call-with-parallel-search-lifetime
            (lambda ()
              (run-parallel-worker-group
                (make-new-task-queue) 2
                :function
                (lambda (id queue)
                  (declare (ignore id queue))
                  ;; A NIL explicit view preserves the worker's surrounding view.
                  (assert (equalp expected
                                  (mapcar
                                    (lambda (seed)
                                      (mapcar #'snapshot-test-state-signature
                                              (snapshot-test-expand seed nil))) seeds)))))))
          (assert (every #'equalp memos copies))
          (format t "~&SNAPSHOT CONCURRENT READS private memos / hashes PASS~%"))
      (setf *shutdown-requested* old-shutdown))))

(defun run-worker-read-snapshot-tests ()
  "Bounded tests only. Run after rebuilding/staging Claustro with parallel settings."
  (reject-worker-read-write 'run-worker-read-snapshot-tests)
  (assert (not *parallel-search-active*))
  (let ((*worker-read-snapshots* t))
    (validate-worker-read-snapshot-mode)
    (snapshot-test-copy-policy)
    (snapshot-test-compiled-routing)
    (snapshot-test-early-exit)
    (let* ((synthetic (snapshot-test-seeds))
           (root-children (snapshot-test-expand *start-state* nil))
           (seeds (cons *start-state* (subseq root-children 0 (min 6 (length root-children)))))
           (context nil))
      (unwind-protect
          (progn
            (setf context (begin-worker-read-phase 2))
            (snapshot-test-view-ownership context)
            (snapshot-test-writer-guards context)
            (dolist (view (worker-read-context-views context))
              (snapshot-test-updates synthetic view)
              (let ((*symmetry-pruning* nil))
                (snapshot-test-updates synthetic view))
              (format t "~&SNAPSHOT READS seeds=~D successors=~D hash-mode=~S PASS~%"
                      (length seeds) (snapshot-test-expansions seeds view)
                      (if (use-canonical-symmetry-p) :canonical :standard)))
            (verify-worker-read-context context))
        (end-worker-read-phase context))
      (snapshot-test-concurrent-reads seeds))
    (dolist (enabled '(nil t))
      (dolist (mode '(:normal :worker-error :partial-start :coordinator-error :nonlocal-exit :cancel :normal))
        (snapshot-test-lifecycle-case mode enabled)))
    (snapshot-test-lifecycle-case :preparation-error t))
  (format t "~&SNAPSHOT FOCUSED TESTS PASS; no solve was run.~%")
  t)
