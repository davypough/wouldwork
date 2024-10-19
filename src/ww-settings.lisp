;;; Filename: ww-settings.lisp

;;; Default settings for planning program.

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


(unless (boundp '*probe*)
  (defvar *probe* nil
    "Inserts a probe to stop processing at a specific state."))
;Example probes:
;   Stops at specified node, for debugging given 
;   (<action name> <instantiations> <depth> &optional <count>)
;   (ww-set *probe* (put (C A) 3))  ;problem blocks3
;   (ww-set *probe* (wait (1 area4) 11))
;   (ww-set *probe* (pour (jug4 9 jug2 0 4) 5))
;   (ww-set *probe* (move (AREA1 AREA8) 3 5))  ;problem-crater
;   (ww-set *probe* (pickup-connector (CONNECTOR3 AREA8) 4))
;   (ww-set *probe* (JUMP (1 3 LD) 4))


(unless (boundp '*debug*)
  (defvar *debug* 0
    "Set the debug level for subsequent runs.
      0 - no debugging
      1 - display full search tree
      2 - display full search tree with states
      3 - display basic nodes
      4 - display full nodes
      5 - display full nodes + break after each expansion cycle"))


(defun cleanup-resources ()
  #+sbcl
  (progn (format t "~&Cleaning up resources and shutting down threads...~%")
         (let ((current-thread sb-thread:*current-thread*))
           (dolist (thread (sb-thread:list-all-threads))
             (unless (eq thread current-thread)
               (when (sb-thread:thread-alive-p thread)
                 (format t "~&Terminating thread: ~A~%" thread)
                 (ignore-errors
                   (sb-thread:terminate-thread thread))))))
           (format t "~&Cleanup completed.~%"))
  #-sbcl
  nil)


#+sbcl (pushnew 'cleanup-resources sb-ext:*exit-hooks*)


;; after loading set the global values to what was in "vals.lisp"
;(defvar *keep-globals-p* nil)   ;; this initializes *keep-globals-p* variable
;(read-globals)                  ;; this overtakes the globals only when
                                ;; keep-globals-p in vals.lisp was set to T


#-sbcl
(when (> *threads* 0)
  (error "Note that multi-threading is not available unless running in SBCL.
          Please set *threads* in ww-settings.lisp to 0."))


(if (> *debug* 0)
  (pushnew :ww-debug *features*)
  (setf *features* (remove :ww-debug *features*)))


#+sbcl (defparameter *lock* (bt:make-lock))  ;for thread protection

;#+sbcl
;(if (> *threads* 0)
;  (setf *debugger-hook* #'(lambda (condition original-hook)
;                            (declare (ignore original-hook))
;                            (bt:with-lock-held (*lock*)
;                              #+sbcl (sb-debug:print-backtrace)
;                              #-sbcl (trivial-backtrace:print-backtrace condition)
;                              (format *error-output* "~%~A~2%" condition)
;                              (finish-output *error-output*))
;                            (abort)))
;  (setf *debugger-hook* nil))


(defun eql* (&rest arguments)
  (every #'eql arguments (rest arguments)))


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.


(defparameter *global-locks* (make-hash-table))  ;Used for non-sbcl implementations


#+sbcl
(defun ensure-global-lock (var-name)
  "Store one lock for each global variable. Used for non-sbcl implementations."
  (or (gethash var-name *global-locks*)
      (setf (gethash var-name *global-locks*)
            (bordeaux-threads:make-lock (format nil "Lock for ~S" var-name)))))


(defmacro define-global (var-name val-form &optional doc-string)
  `(progn
     #+sbcl
     ,(if (> *threads* 0)
          (if (boundp var-name)
              `(setf ,var-name ,val-form)
              `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
          `(defparameter ,var-name ,val-form ,doc-string))
     #-sbcl
     (defparameter ,var-name ,val-form ,doc-string)))


(defmacro increment-global (var-name &optional (delta-form 1))
  `(progn
     (declaim (type fixnum ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-incf ,var-name ,delta-form)
        `(incf ,var-name ,delta-form))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
         ;(incf ,var-name ,delta-form))
        `(incf ,var-name ,delta-form))))


(defmacro push-global (item var-name)
  `(progn
     (declaim (type list ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-push ,item ,var-name)
        `(push ,item ,var-name))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
        ; (push ,item ,var-name))
        `(push ,item ,var-name))))


(defmacro pop-global (var-name)
  `(progn
     (declaim (type list ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-pop ,var-name)
        `(pop ,var-name))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
        ; (pop ,var-name))
       `(pop ,var-name))))


(defun reset-globals (symbols)
  (dolist (symbol symbols)
    (unintern symbol)))


(reset-globals '(goal-fn constraint-fn heuristic? prune? bounding-function?))
;Reset certain user defined functions, when defined on previous load.


(define-global *problem-name* 'unspecified  ;default name
  "Name of the current problem, assigned in problem.lisp by user.")


(define-global *current-problem-name* "unspecified")


(let* ((root (asdf:system-source-directory :wouldwork))
       (src-dir (merge-pathnames "src/" root))
       (target-path (merge-pathnames "problem.lisp" src-dir))
       (source-path (merge-pathnames "problem-blocks3.lisp" src-dir)))
  (unless (probe-file target-path)  ;does problem.lisp already exist?
    (uiop:copy-file source-path target-path)  ;create problem.lisp
    (setf *current-problem-name* "blocks3")))


;;;;;;;;;;;;;;;;;;;; Global Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *ww-loading* t
  "Flag to indicate if Wouldwork is currently being loaded. Reset in ww-initialize.lisp")

(define-global *troubleshoot-current-node* nil
  "A flag telling wouldwork to redo the current node for debugging.")

(define-global *counter* 1
  "For misc debugging with probe function")

(define-global *-* '---------------------------------------------------------
  "Division marker for debugging printout convenience.")

(define-global *solution-count* 0
  "Holds the total number of solutions found following search.")

(declaim (type fixnum *num-idle-threads*))
(define-global *num-idle-threads* 0
  "Holds the number of currently idle threads (shared).")

(declaim (type fixnum *total-states-processed*))
(define-global *total-states-processed* 0
  "Count of states either newly generated, updated, or regenerated while searching (shared).")

(define-global *prior-total-states-processed* 0
  "Count of states produced since last progress printing (shared).")

(define-global *prior-time* 0
  "Time since last progress printing (shared).")

(define-global *best-states* nil
  "Holds the best states encountered during a graph search.")

(declaim (type fixnum *repeated-states*))
(define-global *repeated-states* 0
  "Count of the repeated states during a graph search.")

(declaim (type fixnum *program-cycles*))
(define-global *program-cycles* 0
 "Count of complete cycles of searching (shared).")

(declaim (type fixnum *max-depth-explored*))
(define-global *max-depth-explored* 0
  "Keeps track of the maximum depth reached so far during the search (shared).")

(declaim (type fixnum *accumulated-depths*))
(define-global *accumulated-depths* 0
  "Sums the final depths of all terminated paths so far.")

(declaim (type fixnum *num-paths*))
(define-global *num-paths* 0
  "Tracks the total number of paths explored so far.")

(define-global *num-init-successors* 0
  "The number of branches completed so far from the start state.")

(define-global *rem-init-successors* nil
  "Holds the remaining initial branch nodes from the start state.")

(define-global *solutions* nil
  "Holds the solutions found during search.")

(define-global *average-branching-factor* 0.0
  "Average branching factor so far during search (shared).")

(define-global *search-tree* nil
  "DFS search tree for debugging (serial processing only).")

(define-global *start-time* 0
  "Stores time at beginning of the search.")

(define-global *problem-type* 'planning
  "Spedify whether it's a planning problem or constraint satisfaction problem.")

(define-global *solution-type* 'first
  "Specify whether to search for first, min-length, min-time, or every solution.")

(define-global *tree-or-graph* 'graph
  "Whether there are repeated states (graph) or not (tree); try both.")

(define-global *depth-cutoff* 0
  "Negative or 0 means no cutoff.")

(define-global *progress-reporting-interval* 100000
  "Print progress during search after each multiple n of states examined.")

(define-global *randomize-search* nil
  "Set to t or nil.")

(define-global *branch* -1
  "If n>0, explore only the nth branch from the *start-state*.")

(unless (boundp '*problem-name*)
  (define-global *problem-name* 'unspecified
    "Name of the current problem, assigned in problem.lisp by user."))
(declaim (type symbol *problem-name*))

(define-global *problem-type* 'planning
  "Spedify whether it's a planning problem or constraint satisfaction problem.")

(define-global *solution-type* 'first
  "Specify whether to search for first, min-length, min-time, or every solution.")

(define-global *tree-or-graph* 'graph
  "Whether there are repeated states (graph) or not (tree); try both.")

(define-global *depth-cutoff* 0
  "Negative or 0 means no cutoff.")

(define-global *progress-reporting-interval* 100000
  "Print progress during search after each multiple n of states examined.")

(unless (boundp '*randomize-search*)
  (define-global *randomize-search* nil
    "Set to t or nil."))
(declaim (type (member nil t) *randomize-search*))

(define-global *types* (make-hash-table :test #'eq)
  "Table of all types.")

(define-global *relations* (make-hash-table :test #'eq)
  "Dynamic relations.")

(define-global *static-relations* (make-hash-table :test #'eq)
  "Static relations.")

(define-global *connectives* '(and or not)
  "Logical connectives.")

(define-global *symmetrics* (make-hash-table :test #'eq)
  "Symmetric relations.")

(define-global *complements* (make-hash-table :test #'eq)
  "Table of complement relations.")

(define-global *fluent-relation-indices* (make-hash-table :test #'eq)
  "List of fluent argument indices for a relation.")

(define-global *db* (make-hash-table :test #'equal)
  "Initial database of dynamic db relations.")

(define-global *hdb* (make-hash-table :test #'equal)
  "Initial database of dynamic hdb relations.")

(define-global *idb* (make-hash-table)
  "Initial integer database of dynamic idb propositions.")

(define-global *hidb* (make-hash-table)
  "Initial integer database of dynamic hidb propositions.")

(define-global *constant-integers* (make-hash-table)
  "Integer codes for the problem's object constants.")

(define-global *integer-constants* (make-hash-table)
  "Translating codes back to constants for printout.")

(define-global *min-action-duration* 0.0
  "The least action duration among all actions.")

(define-global *query-names* nil
  "List of all user-defined query functions.")

(define-global *update-names* nil
  "List of all user-defined update functions.")

(define-global *actions* nil
  "List of all potential actions.")

(define-global *init-actions* nil
  "List of all initialization actions.")

(define-global *happening-names* nil
  "The list of objects having exogenous events.")

(define-global *static-db* (make-hash-table :test #'equal)
  "Initial database of static propositions.")

(define-global *static-idb* (make-hash-table)
  "Initial integer database of static propositions.")

(define-global *hap-db* (make-hash-table :test #'equal)
  "Initial database of happenings propositions.")

(define-global *hap-idb* (make-hash-table)
  "Initial integer database of happenings propositions.")

(define-global *last-object-index* 0
  "Last index of object constants seen so far in propositions.")

(define-global *objective-value-p* nil
  "Does the variable $objective-value appear in an action rule.")

(define-global *eff-param-vars* nil
  "Make eff-param-vars available in translate-assert.")

(define-global *unique-solutions* nil)
  ;The culled list of unique solutions.

(define-global *upper-bound* 1000000.0)
  ;The current upper bound if bounds are being calculated.

(define-global *cost* 0.0)
  ;The memoized cost bound for left search tree expansions. 

(define-global *upper* 0.0)
  ;The memoized upper bound for left search tree expansions.

(defvar *state-codes* (make-hash-table)
  "Holding place for integer state codes in bi-directional search.")

(define-global *parameter-headers* '(standard product combination dot-product)
  "The different ways values can be combined in a pre-parameter list.")
