;;; Filename: ww-enum-interpreter.lisp
;;;
;;; High-level goal-enumerator spec interpreter.
;;;
;;; Initial migration target:
;;; - Keep the existing ww-enumerator engine.
;;; - Compile DEFINE-GOAL-ENUMERATOR specs into legacy installs:
;;;   * base relations via INSTALL-BASE-RELATIONS
;;;   * constraint form via INSTALL-BASE-FILTER


(in-package :ww)


(defparameter *enumerator-specs* (make-hash-table :test 'eq)
  "Named goal-enumerator specs registered for the current problem load.")


(defparameter *active-enumerator-spec* nil
  "Most recently installed goal-enumerator spec name.")


(defun clear-enumerator-specs ()
  "Clear stored goal-enumerator specs and active marker."
  (clrhash *enumerator-specs*)
  (setf *active-enumerator-spec* nil)
  (clear-enumerator-domain-hints)
  (when (fboundp 'clear-enum-csp-ir)
    (clear-enum-csp-ir))
  nil)


(defun enumerator-spec (name)
  "Return the raw stored goal-enumerator spec plist for NAME, or NIL."
  (gethash name *enumerator-specs*))


(defun normalize-enumerator-spec (name plist)
  "Normalize and validate a DEFINE-GOAL-ENUMERATOR plist."
  (let ((base-relations (getf plist :base-relations))
        (base-filter-fn (getf plist :base-filter-fn)))
    (unless (and (listp base-relations) base-relations)
      (error "DEFINE-GOAL-ENUMERATOR ~S requires non-empty :BASE-RELATIONS list." name))
    (when (member :constraints plist :test #'eq)
      (error "DEFINE-GOAL-ENUMERATOR ~S no longer supports :CONSTRAINTS. Use DEFINE-BASE-FILTER instead." name))
    (when (and base-filter-fn
               (not (symbolp base-filter-fn))
               (not (functionp base-filter-fn)))
      (error "DEFINE-GOAL-ENUMERATOR ~S :BASE-FILTER-FN must be a symbol or function." name))
    (list :name name
          :base-relations base-relations
          :base-filter-fn base-filter-fn)))


(defun derive-enumerator-ir-config (spec)
  "Derive backend-neutral enum-csp-ir config plist from normalized SPEC."
  (let* ((base-relations (getf spec :base-relations))
         (name (getf spec :name))
         (base-filter-fn (getf spec :base-filter-fn))
         (domain-hints (make-hash-table :test 'eq)))
    (list :base-relations base-relations
          :spec-name name
          :constraint-filter-name nil
          :constraint-form nil
          ;; IR currently names this slot :prefilter; :base-filter-fn is the
          ;; exposed goal-enumerator keyword.
          :prefilter base-filter-fn
          :base-filter-fn base-filter-fn
          :domain-hints domain-hints)))


(defun install-goal-enumerator-spec (name &rest keys &key base-relations base-filter-fn)
  "Install a high-level goal-enumerator spec and derive current legacy settings.
   This is intentionally minimal for migration: it compiles to existing
   INSTALL-BASE-RELATIONS / INSTALL-PREFILTER behavior."
  (declare (ignore base-relations base-filter-fn))
  (let* ((spec (normalize-enumerator-spec name keys))
         (config (derive-enumerator-ir-config spec))
         (rels (getf config :base-relations))
         (filter-name (getf config :constraint-filter-name))
         (filter-form (getf config :constraint-form))
         (base-filter-fn-spec (or (getf config :base-filter-fn)
                                  (getf config :prefilter)))
         (domain-hints (getf config :domain-hints)))
    (setf (gethash name *enumerator-specs*) spec
          *active-enumerator-spec* name)
    (if (and (fboundp 'enum-csp-ir-from-config)
             (fboundp 'install-enum-csp-ir)
             (fboundp 'apply-enum-csp-ir))
        (let* ((ir (install-enum-csp-ir
                    (enum-csp-ir-from-config config)))
               (result (apply-enum-csp-ir ir)))
          (when (and (boundp '*enum-csp-shadow-compare-enabled*)
                     *enum-csp-shadow-compare-enabled*
                     (fboundp 'compare-enum-csp-backends)
                     (or (not (boundp '*enum-csp-backend*))
                         (eq *enum-csp-backend* :legacy)))
            (let ((shadow-summary (compare-enum-csp-backends ir)))
              (when (and (boundp '*enum-csp-shadow-compare-strict*)
                         *enum-csp-shadow-compare-strict*
                         (not (getf shadow-summary :match-p)))
                (error "[enum-csp-ir] strict shadow parity mismatch for ~S: ~S"
                       name
                       (getf shadow-summary :differences)))))
          result)
        ;; Fallback path if IR module is unavailable.
        (progn
          (install-base-relations rels)
          (setf *enumerator-domain-hints* (or domain-hints (make-hash-table :test 'eq)))
          (when filter-form
            (install-base-filter filter-name filter-form))
          (cond
            ((null base-filter-fn-spec) nil)
            ((functionp base-filter-fn-spec)
             (install-prefilter name base-filter-fn-spec))
            ((and (symbolp base-filter-fn-spec) (fboundp base-filter-fn-spec))
             (install-prefilter base-filter-fn-spec (symbol-function base-filter-fn-spec)))
            ((symbolp base-filter-fn-spec)
             ;; Allow forward reference in problem files; leave unresolved now.
             (format t "~&[define-goal-enumerator] base-filter-fn symbol ~S not fbound yet; leaving unchanged.~%"
                     base-filter-fn-spec)))))
    name))


(defun enum-quote-keyword-args (plist quote-keys)
  "Return PLIST with values for QUOTE-KEYS wrapped in QUOTE when needed."
  (let ((out nil))
    (loop for (k v) on plist by #'cddr do
      (push k out)
      (push (if (and (member k quote-keys :test #'eq)
                     (not (and (consp v) (eq (car v) 'quote))))
                (list 'quote v)
                v)
            out))
    (nreverse out)))


(defun enum-quote-base-filter-fn-value (v)
  "Quote symbol :BASE-FILTER-FN values so specs remain declarative."
  (if (and (symbolp v)
           (not (keywordp v))
           (not (null v)))
      (list 'quote v)
      v))


(defmacro define-goal-enumerator (&rest keys)
  "Problem-spec macro: declare high-level goal-enumerator settings.
   Keys currently supported:
  :BASE-RELATIONS  required list of base relations
  :BASE-FILTER-FN  optional symbol/function installed via INSTALL-PREFILTER"
  (let* ((name (or (and (boundp '*problem-name*) *problem-name*)
                   'goal-enumerator))
         (quoted-keys (enum-quote-keyword-args keys '(:base-relations)))
         (base-filter-fn (getf quoted-keys :base-filter-fn)))
    (when base-filter-fn
      (setf (getf quoted-keys :base-filter-fn)
            (enum-quote-base-filter-fn-value base-filter-fn)))
    `(install-goal-enumerator-spec ',name ,@quoted-keys)))


;; Backward-compatibility alias during migration.
(defmacro define-enumerator (&rest keys)
  `(define-goal-enumerator ,@keys))
