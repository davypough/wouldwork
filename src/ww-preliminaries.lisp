;;; Filename: ww-preliminaries.lisp

;;; Initial setup functions & macros for wouldwork.


(in-package :ww)


;Note: It is necessary to close & reopen the lisp environment after
;      changing here from nonparallel to parallel, or parallel to nonparallel.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *threads* 0
    "The number of parallel threads to use.
      0 means no parallelism (ie, serial processing)
      1 means use one parallel thread
        (in addition to parallel management, effectively serial, useful for debugging)
      2 means two or more parallel processing threads
      N up to the number of available CPU threads"))


(defmacro with-search-structures-lock (&body body)
  "Protects composite operations on *open* and *closed* search structures."
  (if (> *threads* 0)
      `(bt:with-lock-held (*search-lock*)
         ,@body)
      `(progn ,@body)))


#+ignore (defmacro with-search-structures-lock (&body body)
  "Protects composite operations on *open* and *closed* search structures."
  `(bt:with-lock-held (*lock*)
     ,@body))


(defparameter *ww-loading* t
  "Flag to indicate if Wouldwork is currently being loaded. Reset in ww-initialize.lisp")


(defparameter *lock* (bt:make-lock))  ;for general thread protection
(defparameter *search-lock*  (bt:make-lock "ww-search-lock"))
(defparameter *integer-lock* (bt:make-lock "ww-integer-lock"))


(defvar *debug* 0
    "Set the debug level for subsequent runs.
      0 - no debugging
      1 - display full search tree
      2 - display full search tree with states
      3 - display basic nodes
      4 - display full nodes
      5 - display full nodes + break after each expansion cycle")


(defmacro with-silenced-compilation (&body body)
  "Macro to suppress normal compilation output while preserving error reporting."
  `(let ((*compile-verbose* nil)
         (*compile-print* nil))
     ,@body))


(defun ww-reset ()
  "Delete problem.lisp, then reload wouldwork with default problem."
  (format t "~%Loading wouldwork defaults...~2%")
  (let* ((root (asdf:system-source-directory :wouldwork))
         (problem-file (merge-pathnames "src/problem.lisp" root)))
    (when (probe-file problem-file) (delete-file problem-file)))
  (asdf:clear-system :wouldwork)
  (with-silenced-compilation
    (asdf:load-system :wouldwork :force t))
  (setf *package* (find-package :ww)))



(defun cleanup-resources ()
  "Attempt to shutdown dangling threads safely in SBCL."
  (format t "~&Cleaning up resources and shutting down threads...~%")
  (let ((current-thread sb-thread:*current-thread*))
    (dolist (thread (sb-thread:list-all-threads))
      (unless (eq thread current-thread)
        (when (sb-thread:thread-alive-p thread)
          (format t "~&Terminating thread: ~A~%" thread)
          (ignore-errors
            (sb-thread:terminate-thread thread))))))
  (format t "~&Cleanup completed.~%"))


(pushnew 'cleanup-resources sb-ext:*exit-hooks*)


;Mainly for debugging
(setf *print-length* nil)  ; Don't limit number of elements printed
(setf *print-level* nil)   ; Don't limit nesting depth
(setf *print-circle* nil)  ; Don't include prior reference #n
;(setf *print-readably* t)
(setq *print-right-margin* 140) ;Allows non-wrap printing of *search-tree* for deep trees.


(defmacro define-global (var-name val-form &optional doc-string)
  `(progn
     ,(if (> *threads* 0)
          `(sb-ext:defglobal ,var-name ,val-form ,doc-string)
          `(defparameter ,var-name ,val-form ,doc-string))))


(defmacro increment-global (var-name &optional (delta-form 1))
  `(progn
     (declaim (type fixnum ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-incf ,var-name ,delta-form)
        `(incf ,var-name ,delta-form))))


(defmacro push-global (item var-name)
  `(progn
     (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-push ,item ,var-name)
        `(push ,item ,var-name))))


(defmacro pop-global (var-name)
  `(progn
     (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-pop ,var-name)
        `(pop ,var-name))))


(defun reset-user-syms (symbols)
  "Unintern symbols and unbind any functions stored in function name lists."
  (dolist (symbol symbols)
    (when (boundp symbol)
      ;; If this symbol holds a list of function names, unbind each function
      (let ((value (symbol-value symbol)))
        (when (and (listp value)
                   (every #'symbolp value))
          (dolist (fn-name value)
            (when (fboundp fn-name)
              (fmakunbound fn-name))))))
    (unintern symbol)))


;Reset certain user defined symbols, when defined on previous load.
(eval-when (:load-toplevel :execute)
  (reset-user-syms '(goal-fn constraint-fn heuristic? prune? bounding-function?)))


(defun reset-global-hash-tables ()
  "Clear all global hash tables and reset global lists between problem loads.
   Critical for parallel mode where sb-ext:defglobal prevents reinitialization.
   In parallel mode (*threads* > 0), define-global uses sb-ext:defglobal which
   only evaluates initialization forms ONCE. On subsequent ASDF reloads, the
   variables remain bound to their previous values, causing state contamination
   between sequential problem loads.
   This function must execute at top-level in ww-preliminaries.lisp so it runs
   on every system reload, before problem files populate the hash tables with
   new data."
  (when (> *threads* 0)  ; Only needed when using sb-ext:defglobal
    ;; Hash tables for type system and object mappings
    (when (boundp '*types*)
      (clrhash *types*))
    (when (boundp '*constant-integers*)
      (clrhash *constant-integers*))
    (when (boundp '*integer-constants*)
      (clrhash *integer-constants*))
    ;; Hash tables for relations
    (when (boundp '*relations*)
      (clrhash *relations*))
    (when (boundp '*static-relations*)
      (clrhash *static-relations*))
    (when (boundp '*symmetrics*)
      (clrhash *symmetrics*))
    (when (boundp '*complements*)
      (clrhash *complements*))
    (when (boundp '*fluent-relation-indices*)
      (clrhash *fluent-relation-indices*))
    ;; Hash tables for databases
    (when (boundp '*db*)
      (clrhash *db*))
    (when (boundp '*hdb*)
      (clrhash *hdb*))
    (when (boundp '*idb*)
      (clrhash *idb*))
    (when (boundp '*hidb*)
      (clrhash *hidb*))
    (when (boundp '*static-db*)
      (clrhash *static-db*))
    (when (boundp '*static-idb*)
      (clrhash *static-idb*))
    (when (boundp '*hap-db*)
      (clrhash *hap-db*))
    (when (boundp '*hap-idb*)
      (clrhash *hap-idb*))
    ;; Cache for proposition key conversions
    (when (boundp '*prop-key-cache*)
      (clrhash *prop-key-cache*))
    ;; Reset lists that accumulate problem definitions
    (when (boundp '*query-names*)
      (setf *query-names* nil))
    (when (boundp '*update-names*)
      (setf *update-names* nil))
    (when (boundp '*actions*)
      (setf *actions* nil))
    (when (boundp '*init-actions*)
      (setf *init-actions* nil))
    (when (boundp '*happening-names*)
      (setf *happening-names* nil))
    ;; Reset object index counter
    (when (boundp '*last-object-index*)
      (setf *last-object-index* 0))))


;; Call at top-level so it executes on every ASDF reload
(eval-when (:load-toplevel :execute)
  (reset-global-hash-tables))


(defun read-init-vals (vals-file)
  "Load critical initialization parameters from vals.lisp if it exists.
   Sets *problem-name*, *algorithm*, and *debug* for proper loading.
   Returns the problem-name string for eval-when path construction, or nil if file absent."
  (when (probe-file vals-file)
    (with-open-file (stream vals-file :direction :input)
      (let ((parameters (read stream)))
        (setf *problem-name* (first parameters)      ; position 0
              *algorithm* (third parameters)         ; position 2  
              *debug* (nth 10 parameters))           ; position 10
        ;; Handle debug feature flag based on loaded value
        (if (> *debug* 0)
            (pushnew :ww-debug *features*)
            (setf *features* (remove :ww-debug *features*)))
        ;; Return problem-name string for eval-when path logic
        (string *problem-name*)))))


(eval-when (:load-toplevel :execute)
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root))
         (blocks3-file (merge-pathnames "problem-blocks3.lisp" src-dir))
         (vals-problem-name (read-init-vals vals-file))
         (vals-problem-file (merge-pathnames (concatenate 'string "problem-" vals-problem-name ".lisp") src-dir)))
    (cond ((not (probe-file problem-file))  ;no problem.lisp file?
             (uiop:copy-file blocks3-file problem-file)  ;default problem.lisp
             (uiop:delete-file-if-exists vals-file))  ;rebuild in ww-initialize.lisp
          ((probe-file vals-file)  ;does vals.lisp exist?
             (if (probe-file vals-problem-file)  ;does problem-<vals-problem-name>.lisp exist?
               (uiop:copy-file vals-problem-file problem-file)  ;make sure problem.lisp corresponds with vals.lisp
               (delete-file vals-file))))))  ;vals.lisp inconsistent with problem.lisp
   