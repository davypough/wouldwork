;;; Explicitly loaded focused checks. Loading runs no check, staging or search.
(in-package :ww)

(defun generated-read-test-form-contains-p (item form)
  (or (equal item form)
      (and (consp form)
           (not (eq (car form) 'quote))
           (or (generated-read-test-form-contains-p item (car form))
               (generated-read-test-form-contains-p item (cdr form))))))

(defun generated-read-test-forms ()
  "Check translation in the CURRENT rebuilt mode; never rebind search settings."
  (validate-generated-read-mode)
  (let* ((serial-p (zerop *threads*))
         (variable '?gate)
         (code (generated-object-code variable))
         (form (subst-int-code
                 '(lambda (?gate)
                    (gethash (list 'always-true ?gate) *static-db*)))))
    (assert (equal code (if serial-p
                           '(gethash ?gate *constant-integers*)
                           '(worker-object-code ?gate))))
    (assert (equal (generated-static-read-table)
                   (if serial-p '*static-idb*
                       '(or *worker-static-read-view* *static-idb*))))
    (assert (generated-read-test-form-contains-p code form))
    (assert (generated-read-test-form-contains-p
              (generated-static-read-table) form))
    (assert (not (generated-read-test-form-contains-p
                   'worker-object-code
                   (convert-prop-list '(list 'always-true 'gate1))))))
  (format t "~&GENERATED READ FORMS PASS ~S~%" *generated-read-mode*)
  t)

(defun generated-read-test-inspection ()
  "Print current generated forms and machine code; no expansion or solve."
  (generated-read-test-forms)
  (dolist (name '(control-on energized fixed-base))
    (format t "~&GENERATED FORM ~S~%" name)
    (pprint (subst-int-code (symbol-value name)))
    (disassemble name))
  (dolist (name '(fold-store fold-remove))
    (format t "~&WRITE GUARD DISASSEMBLY ~S~%" name)
    (disassemble name))
  t)

(defun generated-read-test-rejects-p (function)
  (handler-case (progn (funcall function) nil)
    (error () t)))

(defun generated-read-test-admission ()
  "Disposable mode bindings exercise admission, without calling search."
  (validate-generated-read-mode)
  (dolist (mode (list nil (if (zerop *threads*) :worker-capable :serial)))
    (let ((*generated-read-mode* mode))
      (assert (generated-read-test-rejects-p #'validate-generated-read-mode))
      (assert (generated-read-test-rejects-p #'validate-worker-read-snapshot-mode))
      (let ((*worker-read-snapshots* t))
        (if (worker-read-snapshots-active-p)
            (assert (generated-read-test-rejects-p
                      (lambda () (begin-worker-read-phase 1))))
            (assert (null (begin-worker-read-phase 1))))
        (assert (null *worker-read-phase*)))))
  (format t "~&GENERATED READ ADMISSION PASS~%")
  t)

(defun generated-read-test-compile-failure ()
  "Fail a disposable generated pass before any installed problem function changes."
  (validate-generated-read-mode)
  (let* ((bad (gensym "INVALID-GENERATED-QUERY-"))
         (original (symbol-function 'compile-problem-functions))
         (*generated-read-mode* *generated-read-mode*))
    (unwind-protect
        (progn
          (setf (symbol-function 'compile-problem-functions)
                (lambda ()
                  (compile-generated-function bad
                    '(lambda () (go missing-generated-tag)))))
          (assert (generated-read-test-rejects-p #'compile-all-functions))
          (assert (null *generated-read-mode*))
          (assert (generated-read-test-rejects-p #'validate-generated-read-mode)))
      (setf (symbol-function 'compile-problem-functions) original)
      (when (fboundp bad) (fmakunbound bad))))
  (validate-generated-read-mode)
  (format t "~&GENERATED READ FAILED-COMPILE PASS (expected compiler error)~%")
  t)

(defun generated-read-test-serial-lookup ()
  "Compare compiled canonical and previous dynamic lookup forms on real tables."
  (validate-generated-read-mode)
  (assert (zerop *threads*))
  (let* ((generated (compile nil (subst-int-code
                       '(lambda (?gate)
                          (gethash (list 'always-true ?gate) *static-db*)))))
         (offset (gethash 'always-true *constant-integers*))
         (previous (compile nil
                     `(lambda (?gate)
                        (gethash (+ ,offset (* (worker-object-code ?gate) 1000))
                                 (or *worker-static-read-view* *static-idb*))))))
    (dolist (gate (gethash 'gate *types*))
      (assert (equal (multiple-value-list (funcall generated gate))
                     (multiple-value-list (funcall previous gate))))))
  (format t "~&GENERATED SERIAL ABSENT-FACT/PRESENCE PASS~%")
  t)

(defun generated-read-test-status ()
  "A small assertion step to call AFTER an explicitly approved mode transition."
  (generated-read-test-forms)
  (assert (null *worker-read-phase*))
  (assert (null *parallel-search-active*))
  (assert (null *worker-static-read-view*))
  (assert (null *worker-code-read-view*))
  (format t "~&GENERATED READ STATUS ~S THREADS ~S SNAPSHOTS ~S~%"
          *generated-read-mode* *threads* *worker-read-snapshots*)
  t)

(defun generated-read-test-dynamic-form (form)
  "Diagnostic-only reconstruction of the previous selectors from serial forms."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((and (eq (car form) 'gethash)
              (eq (third form) '*constant-integers*))
         `(worker-object-code ,(second form)))
        ((and (eq (car form) 'gethash) (eq (third form) '*static-idb*))
         `(gethash ,(generated-read-test-dynamic-form (second form))
                   (or *worker-static-read-view* *static-idb*)))
        (t (mapcar #'generated-read-test-dynamic-form form))))

(defun generated-read-test-function-forms ()
  (append
    (loop for name in (append *query-names* *update-names*
                              (remove-if-not #'boundp '(goal-fn constraint-fn)))
          collect (list name (symbol-value name)))
    (loop for action in *actions*
          append (list (list (action.pre-defun-name action)
                             (action.precondition-lambda action))
                       (list (action.eff-defun-name action)
                             (action.effect-lambda action))))))

(defun generated-read-test-variant (entry)
  (multiple-value-bind (function warnings failure)
      (compile nil (generated-read-test-dynamic-form (subst-int-code (second entry))))
    (declare (ignore warnings))
    (when failure (error "Diagnostic variant failed: ~S." (first entry)))
    (list (first entry) function (symbol-function (first entry)))))

(defun generated-read-test-call-dynamic (function)
  "Temporarily install diagnostic variants in an idle serial test image only."
  (let ((variants (mapcar #'generated-read-test-variant
                          (generated-read-test-function-forms))))
    (unwind-protect
        (progn
          (dolist (entry variants)
            (setf (symbol-function (first entry)) (second entry)))
          (funcall function))
      (dolist (entry variants)
        (setf (symbol-function (first entry)) (third entry))))))

(defun generated-read-test-update-results (seeds)
  (loop for symmetry in '(nil t)
        collect
        (let ((*symmetry-pruning* symmetry))
          (loop for name in '(update-gate-status! update-receiver-status! propagate-changes!)
                collect
                (loop for seed in seeds
                      collect
                      (let* ((first (snapshot-test-update (symbol-function name) seed nil))
                             (settled (snapshot-test-copy-state seed)))
                        (setf (problem-state.idb settled) (third first))
                        (list first (snapshot-test-update
                                      (symbol-function name) settled nil))))))))

(defun generated-read-test-behavior-results (synthetic seeds)
  (list (generated-read-test-update-results synthetic)
        (loop for seed in seeds
              collect (mapcar #'snapshot-test-state-signature
                               (snapshot-test-expand seed nil)))))

(defun generated-read-test-serial-behavior ()
  "Bounded update/successor equivalence; requires the existing snapshot test helper."
  (validate-generated-read-mode)
  (assert (and (zerop *threads*) (eq *problem-name* 'claustro-topo)
               (null (worker-read-snapshots-active-p)) (null *worker-read-phase*)
               (null *parallel-search-active*) (null *worker-static-read-view*)
               (null *worker-code-read-view*) (null *happening-names*)
               (null *enumerator-prefilter*)))
  (let* ((sources (worker-read-source-tables))
         (copies (mapcar #'worker-read-copy-table sources))
         (synthetic (snapshot-test-seeds))
         (children (snapshot-test-expand *start-state* nil))
         (seeds (cons *start-state* (subseq children 0 (min 6 (length children)))))
         (expected (generated-read-test-behavior-results synthetic seeds))
         (actual (generated-read-test-call-dynamic
                   (lambda () (generated-read-test-behavior-results synthetic seeds)))))
    (assert (equalp expected actual))
    (assert (every #'eq sources (worker-read-source-tables)))
    (assert (every #'equalp sources copies))
    (format t "~&GENERATED SERIAL BEHAVIOR PASS seeds=~D; no solve~%" (length seeds)))
  t)
