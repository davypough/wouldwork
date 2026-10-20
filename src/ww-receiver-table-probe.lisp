;;; Receiver factorial diagnostic. Explicitly load after ww-table-read-probe.
;;; No production function replacement or search.
(in-package :ww)

(defvar *receiver-table-probe-results* nil)

(defun receiver-probe-rewrite (form names)
  "Rewrite only executable references; preserve quoted data."
  (cond ((eq form '*static-idb*) 'probe-static)
        ((eq form '*constant-integers*) 'probe-codes)
        ((equal form '(or *worker-static-read-view* *static-idb*)) 'probe-static)
        ((and (consp form) (eq (first form) 'worker-object-code))
         `(gethash ,(second form) probe-codes))
        ((atom form) form)
        ((eq (first form) 'quote) form)
        (t
         (let ((replacement (assoc (first form) names)))
           (if replacement
               (append (list (cdr replacement))
                       (mapcar (lambda (part) (receiver-probe-rewrite part names)) (rest form))
                       '(probe-static probe-codes))
               (mapcar (lambda (part) (receiver-probe-rewrite part names)) form))))))

(defun receiver-probe-dependencies (form)
  "Collect generated-query calls; reject unaudited ordinary WW dependencies."
  (when (and (consp form) (not (eq (first form) 'quote)))
    (let ((head (first form)))
      (when (member head '(funcall apply symbol-function eval load))
        (error "Indirect receiver dependency requires audit: ~S" form))
      (when (and (symbolp head) (eq (symbol-package head) (find-package :ww))
                 (fboundp head)
                 (not (member head *query-names*))
                 (not (member head '(update-receiver-status! problem-state.idb
                                     add-int-prop-key del-int-prop-key worker-object-code
                                     beam-coordinates-projection-parameter))))
        (error "Unaudited receiver dependency: ~S" head))
      (append (when (member head *query-names*) (list head))
              (mapcan #'receiver-probe-dependencies form)))))

(defparameter *receiver-probe-audited-queries*
  '(update-receiver-status! beam-reaches-receiver direct-beam-reaches-receiver
    relay-beam-reaches-receiver fixed-beam-corridor-clear
    fixed-beam-corridor-clear-for-object los-barrier-crossings
    fixed-beam-recorded-barriers-clear-for-object recorded-barriers-clear-for-object
    barrier-crossing-clear-for-object gate-open-for-object recording-shadow-object
    recording-shadow-gate-open top base fixed-base object-height
    beam-clear-for-object beam-blocker-occludes-location
    beam-blocker-occludes-location-for-object recording-shadow-object-present
    beam-blocker-spans-elevation fixed-beam-elevation-at beam-elevation-at-location
    beam-coordinates-elevation-at beam-coordinates-endpoint-xy beam-cut))

(defun receiver-probe-closure ()
  (loop with pending = (list 'update-receiver-status!)
        with visited = nil
        for name = (pop pending)
        while name
        unless (member name visited)
          do (unless (member name *receiver-probe-audited-queries*)
               (error "Receiver query outside audited closure: ~S" name))
             (push name visited)
             (setf pending
                   (append (receiver-probe-dependencies
                            (subst-int-code (symbol-value name))) pending))
        finally (return (nreverse visited))))

(defun receiver-probe-uncompiled (&rest args)
  (declare (ignore args))
  (error "Receiver clone called before compilation completed."))

(defun receiver-probe-compile (names)
  ;; Permit recursive BASE/TOP calls without premature undefined-function warnings.
  (dolist (entry names) (setf (symbol-function (cdr entry)) #'receiver-probe-uncompiled))
  (dolist (entry names)
    (let ((form (subst-int-code (symbol-value (car entry)))))
      (assert (eq (first form) 'lambda))
      (compile (cdr entry)
               `(lambda (,@(second form) probe-static probe-codes)
                  (declare (optimize (speed 3) (safety 1) (debug 1)))
                  ,@(mapcar (lambda (part) (receiver-probe-rewrite part names))
                            (if (stringp (third form)) (cdddr form) (cddr form))))))))

(defun receiver-probe-call (function state static codes original-p)
  (let ((*detect-propagated-changes* t) (*propagated-state-changed* nil)
        (*idb-hash-acc* nil) (*fixed-idb-hash-acc* nil) (*symmetry-idb-acc* nil))
    (if original-p (funcall function state) (funcall function state static codes))
    *propagated-state-changed*))

(defun receiver-probe-verify (function states static codes)
  ;; Verify changing and settled branches, outside timing, against original code.
  (dolist (seed states)
    (let ((expected (copy-problem-state seed)) (actual (copy-problem-state seed)))
      (dotimes (pass 2)
        (let ((expected-change (receiver-probe-call #'update-receiver-status! expected nil nil t))
              (actual-change (receiver-probe-call function actual static codes nil)))
          (assert (eql expected-change actual-change))
          (assert (equalp (problem-state.idb expected) (problem-state.idb actual))))))))

(defun receiver-probe-kernel (context keys passes)
  (declare (ignore keys) (optimize (speed 3) (safety 1) (debug 1)))
  (destructuring-bind (function static codes states) context
    (let ((*detect-propagated-changes* t) (*propagated-state-changed* nil)
          (*idb-hash-acc* nil) (*fixed-idb-hash-acc* nil) (*symmetry-idb-acc* nil))
      (dotimes (pass passes)
        (dolist (state states) (funcall function state static codes))))
    (* passes (length states))))

(defun receiver-probe-case (function states passes workers private-static private-codes)
  (let* ((static (table-read-probe-copy *static-idb*
                                       (sb-ext:hash-table-synchronized-p *static-idb*)))
         (codes (table-read-probe-copy *constant-integers*
                                      (sb-ext:hash-table-synchronized-p *constant-integers*)))
         (contexts
           (loop repeat workers collect
             (list function
                   (if private-static
                       (table-read-probe-copy static (sb-ext:hash-table-synchronized-p static)) static)
                   (if private-codes
                       (table-read-probe-copy codes (sb-ext:hash-table-synchronized-p codes)) codes)
                   (mapcar #'copy-problem-state states)))))
    (dolist (context contexts)
      (receiver-probe-verify function states (second context) (third context)))
    (let ((result (table-read-probe-case contexts (coerce states 'vector) passes
                                        :kernel #'receiver-probe-kernel)))
      (dolist (context contexts)
        (assert (equalp (second context) *static-idb*))
        (assert (equalp (third context) *constant-integers*))
        (loop for seed in states for final in (fourth context)
              do (let ((expected (copy-problem-state seed)))
                   (receiver-probe-call #'update-receiver-status! expected nil nil t)
                   (assert (equalp (problem-state.idb expected)
                                   (problem-state.idb final))))))
      (append (list :private-static private-static :private-codes private-codes
                    :verification :passed :unit :receiver-update-calls)
              result))))

(defun receiver-probe-seeds ()
  "Start-derived gate-open/closed variants plus inverted ACTIVE for checks."
  (let* ((base (list (copy-problem-state *start-state*)
                     (copy-problem-state *start-state*)))
         (open-code (gethash 'open *constant-integers*))
         (active-key (+ (gethash 'active *constant-integers*)
                        (* 1000 (gethash 'receiver1 *constant-integers*)))))
    (dolist (gate '(gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9))
      (let ((key (+ open-code (* 1000 (gethash gate *constant-integers*)))) )
        (remhash key (problem-state.idb (first base)))
        (setf (gethash key (problem-state.idb (second base))) t)))
    (let ((altered (mapcar #'copy-problem-state base)))
      (dolist (state altered)
        (let ((db (problem-state.idb state)))
          (if (nth-value 1 (gethash active-key db))
              (remhash active-key db) (setf (gethash active-key db) t))))
      (append base altered))))

(defun receiver-probe-outcomes (states)
  (let ((key (+ (gethash 'active *constant-integers*)
                (* 1000 (gethash 'receiver1 *constant-integers*)))))
    (loop for seed in states
          for state = (copy-problem-state seed)
          do (receiver-probe-call #'update-receiver-status! state nil nil t)
          collect (gethash key (problem-state.idb state)))))

(defun run-receiver-table-probe (&key (target-seconds 1d0) (rounds 3))
  "One-worker reference plus four two-worker sharing combinations. No solve.
Timing repeats settled receiver updates on synthetic gate configurations, not search."
  (assert (eq *problem-name* 'claustro-topo))
  (assert (and (not *print-updates*) (null *happening-names*)
               (plusp target-seconds) (integerp rounds) (plusp rounds)))
  (let ((names (mapcar (lambda (name) (cons name (gensym (symbol-name name))))
                       (receiver-probe-closure)))
        (states (receiver-probe-seeds))
        (cases '((1 nil nil) (2 nil nil) (2 t nil) (2 nil t) (2 t t))))
    (unwind-protect
        (progn
          (format t "~&RECEIVER CLOSURE ~S~%" (mapcar #'car names))
          (receiver-probe-compile names)
          (format t "~&RECEIVER SEED ACTIVE RESULTS ~S~%" (receiver-probe-outcomes states))
          (let* ((function (symbol-function (cdar names)))
                 (passes
                   (loop for n = 2048 then (* 2 n)
                         for row = (receiver-probe-case function states n 1 nil nil)
                         do (format t "~&RECEIVER CALIBRATION ~S~%" row) (finish-output)
                         when (>= (getf row :wall-seconds) target-seconds) return n)))
            (format t "~&RECEIVER PROBE ~S~%"
                    (list :lisp (lisp-implementation-version) :passes passes
                          :states (length states)
                          :seed-source :synthetic-all-gates-open-and-closed
                          :workload :settled-receiver-updates
                          :static-sync (sb-ext:hash-table-synchronized-p *static-idb*)
                          :codes-sync (sb-ext:hash-table-synchronized-p *constant-integers*)
                          :hash-folding nil :clock internal-time-units-per-second
                          :policy '(speed 3 safety 1 debug 1)))
            (setf *receiver-table-probe-results* nil)
            (dotimes (round rounds)
              (let* ((offset (mod round (length cases)))
                     (order (append (nthcdr offset cases) (subseq cases 0 offset))))
                (dolist (entry (if (oddp round) (reverse order) order))
                  (let ((row (append (list :round (1+ round))
                                     (apply #'receiver-probe-case function states passes entry))))
                    (push row *receiver-table-probe-results*)
                    (format t "~&RECEIVER RESULT ~S~%" row) (finish-output)))))
            (setf *receiver-table-probe-results* (nreverse *receiver-table-probe-results*))))
      (dolist (entry names) (fmakunbound (cdr entry)))))
  (values))
