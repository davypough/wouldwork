;;; Worker-owned reads and technology memos; no views survive a worker group.
(in-package :ww)

(defstruct worker-read-view static codes names memo-symbols memo-tables)            ; CHANGED
(defstruct worker-read-context sources copies object-index generation views configuration)

(defparameter *worker-read-memo-symbols* nil)
(defparameter *worker-read-memo-policies* nil)
(defparameter *worker-read-configuration-symbols* nil)

(defun register-worker-read-memo (name policy)
  "A loaded technology declares a cold table or NIL-initialized worker memo."
  (reject-worker-read-write 'register-worker-read-memo)
  (check-type name symbol)
  (check-type policy (member :empty-table :nil))
  (when (assoc name *worker-read-memo-policies*)
    (error "Worker memo ~S is already registered." name))
  (setf *worker-read-memo-symbols* (append *worker-read-memo-symbols* (list name))
        *worker-read-memo-policies* (append *worker-read-memo-policies* (list (cons name policy))))
  name)

(defun register-worker-read-configuration (&rest names)
  "Declare values which must remain fixed while worker views are published."
  (reject-worker-read-write 'register-worker-read-configuration)
  (dolist (name names) (pushnew name *worker-read-configuration-symbols*))
  names)

(defun worker-read-snapshots-active-p ()
  (and *worker-read-snapshots* (plusp *threads*) (eq *algorithm* 'depth-first)))

(declaim (inline worker-object-code))
(defun worker-object-code (object)
  (if *worker-code-read-view*
      (or (gethash object *worker-code-read-view*)
          (error 'worker-read-snapshot-error
                 :format-control "Worker read snapshot has no object code for ~S."
                 :format-arguments (list object)))
      (gethash object *constant-integers*)))

(defun validate-worker-read-snapshot-mode ()
  (validate-generated-read-mode)
  (check-type *worker-read-snapshots* boolean)
  t)

(defun validate-worker-read-registries ()
  (assert (equal *worker-read-memo-symbols* (mapcar #'car *worker-read-memo-policies*)))
  (dolist (entry *worker-read-memo-policies*)
    (ecase (cdr entry)
      (:empty-table (check-type (symbol-value (car entry)) hash-table))
      (:nil (assert (boundp (car entry))))))
  t)

(defun worker-read-configuration ()
  (list (copy-tree *worker-read-memo-policies*)
        (loop for name in *worker-read-configuration-symbols*
              collect (cons name (worker-read-copy-value (symbol-value name))))))

(declaim (inline reject-worker-static-write))
(defun reject-worker-static-write (table)
  (when (and *worker-read-phase*
             (or (eq table *static-idb*) (eq table *static-db*)
                 (eq table *worker-static-read-view*)))
    (reject-worker-read-write 'static-fact-update)))

(defun worker-read-empty-table (table)
  "An unsynchronized table with TABLE's settings. Every copy is owned by one worker,
   or read by the coordinator only after all workers have joined."            ; CHANGED
  (make-hash-table :test (hash-table-test table)
                   :size (hash-table-size table)
                   :rehash-size (hash-table-rehash-size table)
                   :rehash-threshold (hash-table-rehash-threshold table)
                   :synchronized nil))                                          ; CHANGED

(defun worker-read-copy-value (value &optional ancestors)
  "Own finite cons trees and strings. Atoms retain identity; other types fail.
   General vectors/structures and circular static payloads are unsupported."
  (cond ((or (symbolp value) (numberp value) (characterp value)) value)
        ((stringp value) (copy-seq value))
        ((consp value)
         (when (member value ancestors :test #'eq)
           (error "Circular worker read payload is unsupported."))
         (let ((path (cons value ancestors)))
           (cons (worker-read-copy-value (car value) path)
                 (worker-read-copy-value (cdr value) path))))
        (t (error "Unsupported worker read payload type ~S." (type-of value)))))

(defun worker-read-copy-key (key table)
  (when (and (or (consp key) (stringp key))
             (not (eq (hash-table-test table) 'equal)))
    (error "Structured worker read key requires EQUAL, got ~S."
           (hash-table-test table)))
  ;; EQUAL compares strings/cons trees by content. Ordinary vectors use identity
  ;; under EQUAL and are deliberately rejected by the payload copier.
  (worker-read-copy-value key))

(defun worker-read-copy-table (table)
  (let ((copy (worker-read-empty-table table)))
    (maphash (lambda (key value)
               (setf (gethash (worker-read-copy-key key table) copy)
                     (worker-read-copy-value value)))
             table)
    copy))

(defun current-worker-read-memo-symbols ()
  (copy-list *worker-read-memo-symbols*))

(defun make-worker-read-memo (name)
  (ecase (cdr (assoc name *worker-read-memo-policies*))
    (:empty-table (worker-read-empty-table (symbol-value name)))
    (:nil nil)))

(defun make-current-worker-read-view ()
  (let ((names (current-worker-read-memo-symbols)))
    (make-worker-read-view
      :static (worker-read-copy-table *static-idb*)
      :codes (worker-read-copy-table *constant-integers*)
      :names (worker-read-copy-table *integer-constants*)                        ; CHANGED
      :memo-symbols names
      :memo-tables (mapcar #'make-worker-read-memo names))))

(defun call-with-worker-read-view (view function)
  (if (null view)
      (funcall function)
      (let ((*worker-static-read-view* (worker-read-view-static view))
            (*worker-code-read-view* (worker-read-view-codes view))
            (*worker-name-read-view* (worker-read-view-names view)))                ; CHANGED
        (progv (worker-read-view-memo-symbols view)
               (worker-read-view-memo-tables view)
          (funcall function)))))

(defun worker-read-source-tables ()
  (list *static-idb* *constant-integers* *integer-constants* *bijective-canonical*))

(defun begin-worker-read-phase (count &optional (view-maker #'make-current-worker-read-view))
  "Coordinator only, AFTER root task generation and BEFORE thread creation."
  (when (worker-read-snapshots-active-p)
    (validate-worker-read-snapshot-mode)
    (validate-worker-read-registries)
    (bt:with-lock-held (*integer-lock*)
      (reject-worker-read-write 'begin-worker-read-phase)
      (setf *worker-read-phase* t)
      (let ((published nil))
        (unwind-protect
            (prog1
                (make-worker-read-context
                  :sources (worker-read-source-tables)
                  :copies (mapcar #'worker-read-copy-table (worker-read-source-tables))
                  :object-index *last-object-index*
                  :generation *goal-chain-stage-generation*
                  :configuration (worker-read-configuration)
                  :views (loop repeat count collect (funcall view-maker)))
              (setf published t))
          (unless published (setf *worker-read-phase* nil)))))))

(defun verify-worker-read-context (context)
  (assert (every #'eq (worker-read-context-sources context)
                     (worker-read-source-tables)))
  (assert (= (worker-read-context-object-index context) *last-object-index*))
  (assert (= (worker-read-context-generation context) *goal-chain-stage-generation*))
  (assert (equal (worker-read-context-configuration context) (worker-read-configuration)))
  (assert (every #'equalp (worker-read-context-copies context)
                         (worker-read-source-tables)))
  (dolist (view (worker-read-context-views context))
    (assert (equalp (worker-read-view-static view)
                    (first (worker-read-context-copies context))))
    (assert (equalp (worker-read-view-codes view)
                    (second (worker-read-context-copies context))))
    (assert (equalp (worker-read-view-names view)                                   ; CHANGED
                    (third (worker-read-context-copies context)))))                 ; CHANGED
  t)

(defun end-worker-read-phase (context)
  "Called only after ALL created workers have joined, including failed workers."
  (when context
    (bt:with-lock-held (*integer-lock*)
      (unwind-protect (verify-worker-read-context context)
        (setf (worker-read-context-views context) nil
              *worker-read-phase* nil)))))
