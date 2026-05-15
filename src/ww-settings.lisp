;;; Filename: ww-settings.lisp

;;; Default settings for planning program.

(in-package :ww)


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


(defun display-current-parameters ()
  (format t "~2%Current parameter settings:")
  (ut::prt *problem-name* *problem-type* *algorithm* *tree-or-graph* *solution-type*
           *depth-cutoff* 
           *threads* *randomize-search* *debug* *probe* *goal* *auto-wait* *symmetry-pruning*)
  (format t "~&  *PROGRESS-REPORTING-INTERVAL* => ~:D" *progress-reporting-interval*)
  (format t "~&  *BRANCH* TO EXPLORE => ~A" (if (< *branch* 0) 'ALL *branch*))
  (format t "~&  HEURISTIC? => ~A" (when (fboundp 'heuristic?) 'YES))
  (format t "~&  EXOGENOUS HAPPENINGS => ~A" *happening-names*)
  (format t "~&  BOUNDING FUNCTION? => ~A" (when (fboundp 'bounding-function?) 'YES))
  (format t "~&  MIN STEPS REMAINING? => ~A" (when (fboundp 'min-steps-remaining?) 'YES))
  (when (> *threads* 0)
    (format t "~&~%  For parallel settings: (display-parallel-parameters)"))
  (terpri) (terpri))


(defun display-all ()  ;alias
  (display-current-parameters))
(defun params ()  ;alias
  (display-current-parameters))


;(if (> *threads* 0)
;  (setf *debugger-hook* #'(lambda (condition original-hook)
;                            (declare (ignore original-hook))
;                            (bt:with-lock-held (*lock*)
;                              (sb-debug:print-backtrace)
;                              (format *error-output* "~%~A~2%" condition)
;                              (finish-output *error-output*))
;                            (abort)))
;  (setf *debugger-hook* nil))


;;;;;;;;;;;;;;;;;;;; Global Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(sb-ext:defglobal *troubleshoot-current-node* nil
  "A flag telling wouldwork to redo the current node for debugging.")

(sb-ext:defglobal *counter* 1
  "For misc debugging with probe function")

(sb-ext:defglobal *-* '---------------------------------------------------------
  "Division marker for debugging printout convenience.")

(sb-ext:defglobal *solution-count* 0
  "Holds the total number of solutions found following search.")

(sb-ext:defglobal *num-idle-threads* 0
  "Holds the number of currently idle threads (shared).")
(declaim (type fixnum *num-idle-threads*))

(sb-ext:defglobal *total-states-processed* 0
  "Count of states either newly generated, updated, or regenerated while searching (shared).")
(declaim (type fixnum *total-states-processed*))

(sb-ext:defglobal *prior-total-states-processed* 0
  "Count of states produced since last progress printing (shared).")

(sb-ext:defglobal *prior-program-cycles* 0
  "Program cycles at last progress printing (shared).")
(declaim (type fixnum *prior-program-cycles*))

(sb-ext:defglobal *last-improvement-states* 0
  "Value of *total-states-processed* at the most recent solution registration
   (or hybrid-goal deferral). Used by the progress printout to measure
   'states since last improvement' for plateau detection.")
(declaim (type fixnum *last-improvement-states*))

(sb-ext:defglobal *bound-pruned* 0
  "Count of successor states dropped because their f-value could not improve
   the best solution found so far (the f-value-better check in process-successors).")
(declaim (type fixnum *bound-pruned*))

(sb-ext:defglobal *max-backtrack-distance* 0
  "Largest single-step decrease in expansion depth observed during search.
   Each time df-bnb1 pops a node whose depth is less than the previous
   node's depth, the difference is a candidate; the running maximum is
   retained. Diagnostic: large values indicate deep thrashing (early variable
   choices are wrong); small values indicate local repair (variable ordering
   is sound).")
(declaim (type fixnum *max-backtrack-distance*))

(sb-ext:defglobal *prev-expansion-depth* 0
  "Depth of the most recently expanded node, used to compute the per-step
   backtrack distance in df-bnb1.")
(declaim (type fixnum *prev-expansion-depth*))

(sb-ext:defglobal *prior-time* 0
  "Time since last progress printing (shared).")

(sb-ext:defglobal *best-states* nil
  "Holds the best states encountered during a graph search.")

(sb-ext:defglobal *repeated-states* 0
  "Count of the repeated states during a graph search.")
(declaim (type fixnum *repeated-states*))

(sb-ext:defglobal *program-cycles* 0
 "Count of complete cycles of searching (shared).")
(declaim (type fixnum *program-cycles*))

(sb-ext:defglobal *max-depth-explored* 0
  "Keeps track of the maximum depth reached so far during the search (shared).")
(declaim (type fixnum *max-depth-explored*))

(sb-ext:defglobal *accumulated-depths* 0
  "Sums the final depths of all terminated paths so far.")
(declaim (type fixnum *accumulated-depths*))

(sb-ext:defglobal *num-paths* 0
  "Tracks the total number of paths explored so far.")
(declaim (type fixnum *num-paths*))

(sb-ext:defglobal *num-init-successors* 0
  "The number of branches completed so far from the start state.")

(sb-ext:defglobal *rem-init-successors* nil
  "Holds the remaining initial branch nodes from the start state.")

(sb-ext:defglobal *solution-paths* nil
  "Holds all solution paths found during search.")

(sb-ext:defglobal *average-branching-factor* 0.0
  "Average branching factor so far during search (shared).")

(sb-ext:defglobal *search-tree* nil
  "DFS search tree for debugging (serial processing only).")

(sb-ext:defglobal *hybrid-mode* nil
  "When T, hybrid graph search is active for enumerating all solutions at *depth-cutoff*.")

(sb-ext:defglobal *hybrid-goals* nil
  "In hybrid mode, stores (current-node . goal-state) pairs for deferred enumeration.")
(declaim (type list *hybrid-goals*))

(sb-ext:defglobal *start-time* 0
  "Stores time at beginning of the search.")

(defvar *problem-name* 'unspecified  ;default name
  "Name of the current problem, reassigned in problem.lisp by user.")

(defvar *problem-type* 'planning ;
  "Spedify whether it's a planning problem or constraint satisfaction problem.")

(defvar *algorithm* 'depth-first
  "Specify search algorithm: 'depth-first (default) or 'backtracking.
   depth-first: Traditional DFS with state copying (current behavior)
   backtracking: DFS with single state and undo operations (memory efficient)")

(defvar *solution-type* 'first
  "Specify whether to search for first, min-length, min-time, every solution,
   or a positive integer N to find exactly N solutions.")

(defvar *tree-or-graph* 'graph  ;
  "Whether there are repeated states (graph) or not (tree); try both.")

(defvar *depth-cutoff* 0
  "Negative or 0 means no cutoff.")

(defvar *goal* nil
  "Holds the current user goal specification.")

(defvar *progress-reporting-interval* 100000
  "Print progress during search after each multiple n of states examined.")

(defvar *randomize-search* nil  ;
  "Set to t or nil.")

(defvar *branch* -1  ;
  "If n>0, explore only the nth branch from the *start-state*.")

(defvar *auto-wait* nil
  "When T, enables hybrid automatic wait mechanism for problems with happenings.
   Only activates when *happening-names* is non-nil and *tree-or-graph* is tree.
   Replaces explicit wait action with:
   1. Stuck-triggered macro-wait: auto-waits when no actions are applicable
   2. Backtrack-triggered wait: deferred wait tried after regular actions exhaust")

(defvar *auto-wait-max-time* 100
  "Maximum time units to wait during auto-wait simulation before giving up.
   Prevents infinite waiting in problems with no enabling happenings.")

(defvar *symmetry-pruning* nil
  "When T, detect symmetry groups and prune symmetric action instantiations.")

(sb-ext:defglobal *types*
  (make-hash-table :test #'eq :size 256 :rehash-threshold 1.0)  ;; CHANGED: was :synchronized (> *threads* 0)
  "Table of all types.
   Written only during init(); strictly read-only during search.
   Pre-allocated to 256 (well above any realistic type count)
   with rehash-threshold 1.0 so the table never resizes.
   Not :synchronized - lock-free reads on the worker hot path
   (precondition expansion reads here ~59M times/run in queensN-csp profile).")

(sb-ext:defglobal *relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))
  "Dynamic relations.")

(sb-ext:defglobal *static-relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))
  "Static relations.")

(sb-ext:defglobal *connectives* '(and or not)
  "Logical connectives.")

(sb-ext:defglobal *symmetrics*
  (make-hash-table :test #'eq :size 64 :rehash-threshold 1.0)  ;; CHANGED: was :synchronized (> *threads* 0)
  "Symmetric relations.
   Written only during init(); strictly read-only during search.
   Pre-allocated to 64 (well above any realistic symmetric-relation count)
   with rehash-threshold 1.0 so the table never resizes.
   Not :synchronized - lock-free reads on the worker hot path
   (add-proposition calls gethash here ~70M times/run in queensN-csp profile).")

(sb-ext:defglobal *complements*
  (make-hash-table :test #'eq :size 128 :rehash-threshold 1.0)  ;; CHANGED: was :synchronized (> *threads* 0)
  "Table of complement relations.
   Written only during init(); strictly read-only during search.
   Pre-allocated to 128 (well above any realistic complement count)
   with rehash-threshold 1.0 so the table never resizes.
   Not :synchronized - lock-free reads on the worker hot path
   (add-prop calls gethash here ~70M times/run in queensN-csp profile).")

(sb-ext:defglobal *fluent-relation-indices* (make-hash-table :test #'eq)
  "List of fluent argument indices for a relation.")

(sb-ext:defglobal *db* (make-hash-table :test #'equal :synchronized (> *threads* 0))
  "Initial database of dynamic db relations.")

(sb-ext:defglobal *hdb* (make-hash-table :test #'equal :synchronized (> *threads* 0))
  "Initial database of dynamic hdb relations.")

(sb-ext:defglobal *idb* (make-hash-table :synchronized (> *threads* 0))
  "Initial integer database of dynamic idb propositions.")

(sb-ext:defglobal *hidb* (make-hash-table :synchronized (> *threads* 0))
  "Initial integer database of dynamic hidb propositions.")

(sb-ext:defglobal *constant-integers*
  (make-hash-table :size 2003 :rehash-threshold 1.0)  ;; CHANGED: was :synchronized (> *threads* 0)
  "Integer codes for the problem's object constants.
   Pre-allocated to capacity 2003 (above the 999-object design limit
   in convert-to-integer/register-dynamic-object) with rehash-threshold
   1.0 so the table never resizes during search. Not :synchronized -
   reads on the worker hot path (convert-fluentless-prop-to-integer,
   ~12M calls/run in queensN-csp profile) are lock-free; the rare
   write path is already serialized by *integer-lock*.")

(sb-ext:defglobal *integer-constants* (make-hash-table :synchronized (> *threads* 0))
  "Translating codes back to constants for printout.")

(sb-ext:defglobal *min-action-duration* 0.0
  "The least action duration among all actions.")

(sb-ext:defglobal *query-names* nil
  "List of all user-defined query functions.")

(sb-ext:defglobal *update-names* nil
  "List of all user-defined update functions.")

(defparameter *actions* nil  ;don't use define-global
  "List of all potential actions.")

(sb-ext:defglobal *init-actions* nil
  "List of all initialization actions.")

(sb-ext:defglobal *happening-names* nil
  "The list of objects having exogenous events.")

(sb-ext:defglobal *static-db* (make-hash-table :test #'equal)
  "Initial database of static propositions.")

(sb-ext:defglobal *static-idb* (make-hash-table :synchronized (> *threads* 0))
  "Initial integer database of static propositions.")

(sb-ext:defglobal *hap-db* (make-hash-table :test #'equal)
  "Initial database of happenings propositions.")

(sb-ext:defglobal *hap-idb* (make-hash-table)
  "Initial integer database of happenings propositions.")

(sb-ext:defglobal *last-object-index* 0
  "Last index of object constants seen so far in propositions.")
(declaim (type (integer 0 999) *last-object-index*))

(sb-ext:defglobal *objective-value-p* nil
  "Does the variable $objective-value appear in an action rule.")

(sb-ext:defglobal *eff-param-vars* nil
  "Make eff-param-vars available in translate-assert.")

(sb-ext:defglobal *has-sim-state* nil
  "True when $sim-state appears in precondition variables.")

(sb-ext:defglobal *unique-solution-states* nil)
  ;The culled list of unique solutions.

(sb-ext:defglobal *upper-bound* 1000000.0)
  ;The current upper bound if bounds are being calculated.

(sb-ext:defglobal *cost* 0.0)
  ;The memoized cost bound for left search tree expansions. 

(sb-ext:defglobal *upper* 0.0)
  ;The memoized upper bound for left search tree expansions.

(defvar *state-codes* (make-hash-table)
  "Holding place for integer state codes in bi-directional search.")

(defvar *choice-stack* nil
  "Stack of choices for backtracking search. Defined here to avoid forward reference warnings.")

(sb-ext:defglobal *parameter-headers* '(standard product combination dot-product)
  "The different ways values can be combined in a pre-parameter list.")

(sb-ext:defglobal *print-updates* nil
  "Print each database update while T")

(sb-ext:defglobal *global-invariants* nil
  "List of invariant query functions to check on every state.")

(sb-ext:defglobal *inconsistent-states-dropped* 0
  "Count of successor states dropped due to convergence failure.")
(declaim (type fixnum *inconsistent-states-dropped*))


(sb-ext:defglobal *lower-bound-pruned* 0
  "Count of nodes pruned by user-defined min-steps-remaining? function.")
(declaim (type fixnum *lower-bound-pruned*))

(sb-ext:defglobal *prop-key-cache* 
  (make-hash-table :test #'equal :synchronized (> *threads* 0))
  "Cache for prop-key-to-integer conversions")

(sb-ext:defglobal *inconsistent-state-key* nil
  "Pre-computed integer code for the (inconsistent-state) proposition.
   Set in init() after do-integer-conversion. Read on the worker hot
   path in state-is-inconsistent and update-is-inconsistent to avoid
   mutex acquisitions on the synchronized *prop-key-cache* table.")

(sb-ext:defglobal *bijective-relations* (make-hash-table :test #'eq)
  "Maps canonical relation name to (index1-name index2-name).")

(sb-ext:defglobal *bijective-canonical* (make-hash-table :test #'eq)
  "Maps internal index name to (canonical-name . key-position).")


;; Reset parameters to defaults when vals.lisp doesn't exist
;; This ensures clean initialization for new problems without carrying over
;; settings from previous problem.lisp files
(eval-when (:load-toplevel :execute)
  (let ((vals-file (merge-pathnames "vals.lisp" 
                                    (asdf:system-source-directory :wouldwork))))
    (unless (probe-file vals-file)
      ;; No vals.lisp exists, reset to defaults before problem.lisp loads
      (setf *problem-name* 'unspecified
            *depth-cutoff* 0
            *algorithm* 'depth-first
            *tree-or-graph* 'graph
            *problem-type* 'planning
            *solution-type* 'first
            *progress-reporting-interval* 100000
            *randomize-search* nil
            *branch* -1
            *probe* nil
            *debug* 0
            *goal* nil
            *auto-wait* nil
            *symmetry-pruning* nil)
      ;; Ensure debug feature flag is cleared
      (setf *features* (remove :ww-debug *features*)))))
