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
           *depth-cutoff* *progress-reporting-interval*
           *threads* *randomize-search* *debug* *probe*)
  (format t "~&  BRANCH TO EXPLORE => ~A" (if (< *branch* 0) 'ALL *branch*))
  (format t "~&  HEURISTIC? => ~A" (when (fboundp 'heuristic?) 'YES))
  (format t "~&  EXOGENOUS HAPPENINGS => ~A" (when *happening-names* 'YES))
  (format t "~&  BOUNDING FUNCTION? => ~A" (when (fboundp 'bounding-function?) 'YES))
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

(defvar *problem-name* 'unspecified  ;default name
  "Name of the current problem, reassigned in problem.lisp by user.")

(defvar *problem-type* 'planning ;
  "Spedify whether it's a planning problem or constraint satisfaction problem.")

(defvar *algorithm* 'depth-first
  "Specify search algorithm: 'depth-first (default) or 'backtracking.
   depth-first: Traditional DFS with state copying (current behavior)
   backtracking: DFS with single state and undo operations (memory efficient)")

(defvar *solution-type* 'first
  "Specify whether to search for first, min-length, min-time, or every solution.")

(defvar *tree-or-graph* 'graph  ;
  "Whether there are repeated states (graph) or not (tree); try both.")

(defvar *depth-cutoff* 0
  "Negative or 0 means no cutoff.")

(defvar *progress-reporting-interval* 100000
  "Print progress during search after each multiple n of states examined.")

(defvar *randomize-search* nil  ;
  "Set to t or nil.")

(defvar *branch* -1  ;
  "If n>0, explore only the nth branch from the *start-state*.")

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
(declaim (type (integer 0 999) *last-object-index*))

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

(define-global *print-updates* nil
  "Print each database update while T")

(define-global *global-invariants* nil
  "List of invariant query functions to check on every state.")

(declaim (type fixnum *inconsistent-states-dropped*))
(define-global *inconsistent-states-dropped* 0
  "Count of successor states dropped due to convergence failure.")

(defparameter *processing-init-action* nil
  "Dynamic variable indicating when we're processing initialization actions.
   When T, assert statements should use depth-first translation regardless of *algorithm*.")

(defparameter *prop-key-cache* (make-hash-table :test #'equal)
  "Cache for prop-key-to-integer conversions")


;; *** ADDED: Reset parameters to defaults when vals.lisp doesn't exist ***
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
            *debug* 0)
      ;; Ensure debug feature flag is cleared
      (setf *features* (remove :ww-debug *features*)))))