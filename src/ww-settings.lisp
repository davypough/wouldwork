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
           *threads* *randomize-search* *debug* *probe* *goal* *symmetry-pruning*)
  (format t "~&  *NOVELTY-PRUNING* => ~A" *novelty-pruning*)
  (when *novelty-pruning*
    (format t "~&  *NOVELTY-PARTITION* => ~A" *novelty-partition*))
  (when *happening-names*
    (format t "~&  *AUTO-WAIT* => ~A" *auto-wait*))
  (when (and (member "recorder" *spliced-tech-names* :test #'string=)
             (gethash 'recorder *types*))
    (format t "~&  *RECORDER-PREFIX-PRUNING* => ~A" *recorder-prefix-pruning*)
    (format t "~&  *MAX-RECORDER-CYCLES* => ~D" *max-recorder-cycles*))
  (when (and (member "beam-relay" *spliced-tech-names* :test #'string=)
             (gethash 'connector *types*))
    (format t "~&  *MAX-CONNECTOR-PAIRINGS* => ~D" *max-connector-pairings*))
  (when (and (member "-beam-los-coordinates" *spliced-tech-names* :test #'string=)
             (gethash 'location *types*))
    (format t "~&  *BEAM-OCCLUSION-TOLERANCE* => ~A" *beam-occlusion-tolerance*))
  (when (and (member "visibility" *spliced-tech-names* :test #'string=)
             (gethash '(boundary-wall) *static-db*))
    (format t "~&  *BOUNDARY-WALL-HEIGHT* => ~A" *boundary-wall-height*))
  (when (and (member "-support-elevation" *spliced-tech-names* :test #'string=)
             (funcall (symbol-function 'vertical-reach-limit-relevant-p)
                      *start-state*))
    (format t "~&  *VERTICAL-REACH-LIMIT* => ~A" *vertical-reach-limit*))
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

(sb-ext:defglobal *prior-parallel-progress-time* 0
  "Internal real time at the previous parallel progress report. Used to compute
   windowed (recent) processing speed, independent of the report throttle's
   own timestamp bookkeeping.")

(sb-ext:defglobal *prior-parallel-progress-states* 0
  "Total states processed as of the previous parallel progress report.")
(declaim (type fixnum *prior-parallel-progress-states*))

(sb-ext:defglobal *prior-parallel-progress-cycles* 0
  "Total program cycles as of the previous parallel progress report.")
(declaim (type fixnum *prior-parallel-progress-cycles*))

(sb-ext:defglobal *last-improvement-states* 0
  "Value of *total-states-processed* at the most recent solution registration
   (or hybrid-goal deferral). Used by the progress printout to measure
   'states since last improvement' for plateau detection.")
(declaim (type fixnum *last-improvement-states*))

(sb-ext:defglobal *bound-pruned* 0
  "Count of successor states dropped because their f-value could not improve
   the best solution found so far (the f-value-better check in process-successors).")
(declaim (type fixnum *bound-pruned*))

(sb-ext:defglobal *accumulated-backtrack-distance* 0
  "Sums the depth-drop of every backtrack event during search. Each time
   df-bnb1 (or a parallel worker) pops a node shallower than the previously
   expanded node, that drop is a backtrack event. Paired with *num-backtracks*
   to give the average backtrack distance. Diagnostic: large values indicate
   deep thrashing (early choices are wrong); small values indicate local
   repair (ordering is sound).")
(declaim (type fixnum *accumulated-backtrack-distance*))

(sb-ext:defglobal *num-backtracks* 0
  "Count of backtrack events (node pops shallower than the previous
   expansion) during search.")
(declaim (type fixnum *num-backtracks*))

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

(sb-ext:defglobal *dead-end-accumulated-depths* 0
  "Sums the depths at which paths terminated with no successor states
   (genuine exploration exhaustion).")
(declaim (type fixnum *dead-end-accumulated-depths*))

(sb-ext:defglobal *dead-end-num-paths* 0
  "Tracks the number of paths terminated with no successor states.")
(declaim (type fixnum *dead-end-num-paths*))

(sb-ext:defglobal *duplicate-accumulated-depths* 0
  "Sums the depths at which paths terminated by colliding with an
   already-open or already-closed state.")
(declaim (type fixnum *duplicate-accumulated-depths*))

(sb-ext:defglobal *duplicate-num-paths* 0
  "Tracks the number of paths terminated by state duplication.")
(declaim (type fixnum *duplicate-num-paths*))

(sb-ext:defglobal *depth-cutoff-hits* 0
  "Count of nodes whose expansion was blocked because they reached *depth-cutoff*.")
(declaim (type fixnum *depth-cutoff-hits*))

(sb-ext:defglobal *num-init-successors* 0
  "The number of branches completed so far from the start state.")

(sb-ext:defglobal *rem-init-successors* nil
  "Holds the remaining initial branch nodes from the start state.")

(sb-ext:defglobal *solution-paths* nil
  "Holds all solution paths found during search.")

(sb-ext:defglobal *solutions-valid* nil
  "Whether *solution-paths* came from a search that completed normally.")

(sb-ext:defglobal *solution-report-printers* nil
  "Problem-local functions called after Wouldwork prints a solution.")


(sb-ext:defglobal *solution-validators* nil
  "Problem-local functions that must accept a candidate path before it is a solution.")


(defstruct (search-prefix-validator (:conc-name search-prefix-validator.))
  "A path-prefix validator, its enabling predicate, and optional newest-move trigger."
  validator
  enabled-p
  trigger-p)


(sb-ext:defglobal *search-prefix-validators* nil
  "Problem-local validators that may reject irreversible path-prefix failures.")


(defstruct (search-successor-pruner (:conc-name search-successor-pruner.))
  "A successor rejection predicate, enabling predicate, and optional search resetter."
  pruner
  enabled-p
  resetter)


(sb-ext:defglobal *search-successor-pruners* nil
  "Problem-local policies that may reject generated successor states.")


(defun register-search-successor-pruner (pruner enabled-p &optional resetter)
  "Register a problem-local successor rejection policy.

PRUNER receives CURRENT-NODE and SUCCESSOR-STATE and returns true only when that
successor may be discarded.  ENABLED-P prevents disabled policies from entering the
search hot path.  RESETTER, when supplied, is called before every search.  Pruners used
in parallel search must synchronize their own mutable policy state.  Every supplied
function argument must name an already-defined function."
  (dolist (function-name (remove nil (list pruner enabled-p resetter)))
    (unless (and (symbolp function-name) (fboundp function-name))
      (error "Search-successor pruner requires a defined function: ~S"
             function-name)))
  (unless (find pruner *search-successor-pruners*
                :key #'search-successor-pruner.pruner)
    (setf *search-successor-pruners*
          (append *search-successor-pruners*
                  (list (make-search-successor-pruner
                          :pruner pruner
                          :enabled-p enabled-p
                          :resetter resetter)))))
  pruner)


(defun reset-search-successor-pruners ()
  "Reset every registered successor policy before a new search begins."
  (dolist (entry *search-successor-pruners*)
    (when (search-successor-pruner.resetter entry)
      (funcall
        (symbol-function (search-successor-pruner.resetter entry)))))
  nil)


(defun search-successor-pruned-p (current-node successor-state)
  "Whether any enabled problem-local policy rejects SUCCESSOR-STATE."
  (when (search-successor-policy-rejects-p current-node successor-state)
    (incf *successor-policy-pruned*)
    t))


(defun search-successor-policy-rejects-p (current-node successor-state)
  "Whether any enabled problem-local policy rejects SUCCESSOR-STATE, uncounted."
  (some (lambda (entry)
          (and (funcall
                 (symbol-function (search-successor-pruner.enabled-p entry)))
               (funcall
                 (symbol-function (search-successor-pruner.pruner entry))
                 current-node successor-state)))
        *search-successor-pruners*))


(defun register-search-prefix-validator (validator enabled-p &optional trigger-p)
  "Register VALIDATOR for search-time path-prefix pruning.

VALIDATOR receives START-STATE, PATH, and CURRENT-STATE and returns true while the
prefix can still lead to a valid solution.  ENABLED-P is a zero-argument function
symbol.  TRIGGER-P, when supplied, receives START-STATE, the newest MOVE, and
CURRENT-STATE; returning NIL avoids reconstructing and validating an irrelevant path.
All supplied functions must be read-only and safe to call concurrently."
  (dolist (function-name (remove nil (list validator enabled-p trigger-p)))
    (unless (and (symbolp function-name) (fboundp function-name))
      (error "Search-prefix validator requires a defined function: ~S" function-name)))
  (unless (find validator *search-prefix-validators*
                :key #'search-prefix-validator.validator)
    (setf *search-prefix-validators*
          (append *search-prefix-validators*
                  (list (make-search-prefix-validator
                          :validator validator
                          :enabled-p enabled-p
                          :trigger-p trigger-p)))))
  validator)


(defun search-prefix-validation-enabled-p (&optional excluded-validators)
  "Whether any non-excluded search-prefix validator is currently enabled."
  (some (lambda (entry)
          (let ((validator (search-prefix-validator.validator entry)))
            (and (not (member validator excluded-validators :test #'eq))
                 (funcall
                   (symbol-function
                     (search-prefix-validator.enabled-p entry))))))
        *search-prefix-validators*))


(defun search-prefix-validator-triggered-p
    (entry newest-move current-state)
  "Whether enabled validator ENTRY is interested in NEWEST-MOVE."
  (let ((trigger-p (search-prefix-validator.trigger-p entry)))
    (or (null trigger-p)
        (funcall (symbol-function trigger-p)
                 *start-state* newest-move current-state))))


(defun search-prefix-validation-required-p
    (newest-move current-state &optional excluded-validators)
  "Whether any enabled non-excluded validator needs the path ending in NEWEST-MOVE."
  (some (lambda (entry)
          (let ((validator (search-prefix-validator.validator entry)))
            (and (not (member validator excluded-validators :test #'eq))
                 (funcall
                   (symbol-function (search-prefix-validator.enabled-p entry)))
                 (search-prefix-validator-triggered-p
                   entry newest-move current-state))))
        *search-prefix-validators*))


(defun candidate-search-prefix-valid-p
    (path current-state &optional excluded-validators)
  "Whether every enabled non-excluded validator accepts PATH ending at CURRENT-STATE."
  (let ((newest-move (car (last path))))
    (dolist (entry *search-prefix-validators* t)
      (let ((validator (search-prefix-validator.validator entry)))
        (when (and (not (member validator excluded-validators :test #'eq))
                   (funcall
                     (symbol-function
                       (search-prefix-validator.enabled-p entry)))
                   (search-prefix-validator-triggered-p
                     entry newest-move current-state))
          (unless (funcall
                    (symbol-function validator)
                    *start-state* path current-state)
            (return nil)))))))


(sb-ext:defglobal *goal-chaining-checkpoint-extensions* nil
  "Registered (NAME SNAPSHOTTER RESTORER) extensions for goal-chaining undo state.")


(sb-ext:defglobal *symmetry-coupling-relations* nil
  "Static relations whose tuples must be permuted as complete symmetry rows.")


(defun register-symmetry-coupling (relation)
  "Require symmetry to preserve every tuple of static RELATION column by column."
  (unless (symbolp relation)
    (error "Symmetry coupling must name a relation: ~S" relation))
  (pushnew relation *symmetry-coupling-relations* :test #'eq)
  relation)


(defstruct goal-chaining-policy
  "Problem-local implementations behind the public goal-chaining commands."
  subgoal-solver
  final-solver)


(sb-ext:defglobal *goal-chaining-policy* nil
  "Active problem-local goal-chaining policy, or NIL for generic goal chaining.")


(defun register-goal-chaining-policy (subgoal-solver final-solver)
  "Register specialized implementations for SOLVE-SUBGOAL and mid-chain SOLVE.

SUBGOAL-SOLVER names a function of one goal-form argument.  FINAL-SOLVER names a
function of no arguments.  Staging another problem clears the policy."
  (dolist (solver (list subgoal-solver final-solver))
    (unless (and (symbolp solver) (fboundp solver))
      (error "Goal-chaining solver must name a defined function: ~S" solver)))
  (when *goal-chaining-policy*
    (error "A goal-chaining policy is already registered: ~S"
           *goal-chaining-policy*))
  (setf *goal-chaining-policy*
        (make-goal-chaining-policy
          :subgoal-solver subgoal-solver
          :final-solver final-solver)))


(defun register-goal-chaining-checkpoint-extension (name snapshotter restorer)
  "Register problem-local checkpoint state without specializing UNDO-CHECKPOINT."
  (when (assoc name *goal-chaining-checkpoint-extensions*)
    (error "Goal-chaining checkpoint extension registered twice: ~S" name))
  (setf *goal-chaining-checkpoint-extensions*
        (append *goal-chaining-checkpoint-extensions*
                (list (list name snapshotter restorer))))
  name)


(sb-ext:defglobal *nominal-solution-candidates* 0
  "Number of paths that satisfied the problem goal and were submitted to validators.")


(sb-ext:defglobal *accepted-solution-candidates* 0
  "Number of nominal solution paths accepted by every registered validator.")


(sb-ext:defglobal *rejected-solution-candidates* 0
  "Number of nominal solution paths rejected by a registered validator.")


(sb-ext:defglobal *solution-validator-rejections*
  (make-hash-table :test #'equal)
  "Counts rejected candidate paths by validator, diagnostic phase, and reason.")


(defun register-solution-validator (validator)
  "Register a function symbol that validates a complete candidate path.

The function receives START-STATE, PATH, and GOAL-STATE.  It returns true to accept the
candidate.  A false result may be accompanied by a second diagnostic value.  A plist with
:PHASE and :REASON fields produces the most useful grouped search report.  Validators must
treat their arguments as read-only and be safe to call concurrently."
  (unless (and (symbolp validator) (fboundp validator))
    (error "Solution validator must name a defined function: ~S" validator))
  (unless (member validator *solution-validators*)
    (setf *solution-validators*
          (append *solution-validators* (list validator))))
  validator)


(defun register-solution-report-printer (printer)
  "Register a function symbol to print a supplement after each displayed solution."
  (unless (and (symbolp printer) (fboundp printer))
    (error "Solution report printer must name a defined function: ~S" printer))
  (unless (member printer *solution-report-printers*)
    (setf *solution-report-printers*
          (append *solution-report-printers* (list printer))))
  printer)

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
  "When T, detect symmetry families and prune symmetric actions or states.")

(defvar *recorder-prefix-pruning* nil
  "When T, recorder technology prunes paths whose recording prefix cannot replay.")

(defvar *novelty-pruning* nil
  "Width of the novelty test applied to generated states: NIL disables it, 1 keeps only
   states asserting an atom no earlier state asserted, 2 keeps those plus states asserting
   a new pair of atoms.  Incomplete by construction -- see NOVELTY-PRUNED-P.")

(defvar *novelty-partition* nil
  "How generated states are grouped before the novelty test compares them: NIL puts every
   state in one partition, DEPTH partitions by search depth, and QUERY partitions by the
   value of a problem-defined NOVELTY-PARTITION? query.  A partition that advances with
   real progress keeps a must-undo plan alive that one global partition would discard.")

(defvar *max-recorder-cycles* 1
  "Maximum number of START-RECORDER actions permitted in one search path.")

(defvar *max-connector-pairings* nil
  "Maximum PAIRED termini per connector, or NIL until beam-relay supplies its default.")

(defvar *beam-occlusion-tolerance* 1/2
  "Maximum perpendicular distance a location may sit off a beam's exact line and still
   count as a candidate occluder there.")

(defvar *boundary-wall-height* 6
  "The boundary polygon's height for sightline crossing tests. Its base is 0.")

(defvar *vertical-reach-limit* 1
  "Maximum elevation gap an agent can act across vertically, independent of height.")

(defvar *split-depth-max* 20
  "Safety cap on serial task-generation depth.")

(defvar *tasks-per-thread* 8
  "Target number of generated tasks per parallel worker.")

(defvar *min-tasks* 256
  "Minimum number of tasks generated for parallel search.")

(defvar *num-closed-shards* 64
  "Number of shards in the parallel closed table.")

(defvar *closed-shard-mask* 63
  "Bitmask derived from *NUM-CLOSED-SHARDS* for fast shard selection.")

(defvar *bound-refresh-interval* 1000
  "Worker cycles between cached branch-and-bound refreshes.")

(defvar *donation-check-interval* 10000
  "Worker cycles between work-donation checks.")

(defvar *donation-threshold* 256
  "Minimum local stack size before a worker may donate work.")

(defvar *donation-fraction* 0.2
  "Fraction of a donating worker's local stack to donate.")

(defvar *enable-work-donation* t
  "Whether parallel workers may donate work to the shared task queue.")


(defparameter *problem-parameter-defaults*
  '((*problem-name* . unspecified)
    (*depth-cutoff* . 0)
    (*algorithm* . depth-first)
    (*tree-or-graph* . graph)
    (*problem-type* . planning)
    (*solution-type* . first)
    (*progress-reporting-interval* . 100000)
    (*randomize-search*)
    (*branch* . -1)
    (*probe*)
    (*symmetry-pruning*)
    (*novelty-pruning*)
    (*novelty-partition*)
    (*debug* . 0)
    (*goal*)
    (*threads* . 0)
    (*recorder-prefix-pruning*)
    (*max-recorder-cycles* . 1)
    (*auto-wait*)
    (*max-connector-pairings*)
    (*beam-occlusion-tolerance* . 1/2)
    (*boundary-wall-height* . 6)
    (*vertical-reach-limit* . 1)
    (*split-depth-max* . 20)
    (*tasks-per-thread* . 8)
    (*min-tasks* . 256)
    (*num-closed-shards* . 64)
    (*bound-refresh-interval* . 1000)
    (*donation-check-interval* . 10000)
    (*donation-threshold* . 256)
    (*donation-fraction* . 0.2)
    (*enable-work-donation* . t))
  "Authoritative defaults restored before staging a problem specification.")


(defparameter *persisted-problem-parameters*
  '(*problem-name* *depth-cutoff* *algorithm* *tree-or-graph* *problem-type*
    *solution-type* *progress-reporting-interval* *randomize-search* *branch*
    *probe* *symmetry-pruning* *debug* *goal* *threads*
    *recorder-prefix-pruning* *max-recorder-cycles*)
  "Problem parameters saved in VALS.LISP, in positional file order.")


(defun problem-parameter-default (parameter)
  (let ((entry (assoc parameter *problem-parameter-defaults*)))
    (unless entry
      (error "No default is registered for problem parameter ~S." parameter))
    (cdr entry)))


(defparameter *default-parameters*
  (mapcar #'problem-parameter-default *persisted-problem-parameters*)
  "Persisted problem-parameter defaults in VALS.LISP order.")


(defun reset-problem-parameters-to-defaults (&optional (problem-name 'unspecified))
  "Restore every managed parameter before a new problem specification is loaded."
  (dolist (entry *problem-parameter-defaults*)
    (set (car entry) (cdr entry)))
  (setf *problem-name* problem-name
        *closed-shard-mask* (1- *num-closed-shards*)
        *features* (remove :ww-debug *features*)))

(sb-ext:defglobal *types*
  (make-hash-table :test #'eq :size 256 :rehash-threshold 1.0)
  "Table of all types.
   Written only during init(); strictly read-only during search.
   Pre-allocated to 256 (well above any realistic type count)
   with rehash-threshold 1.0 so the table never resizes.
   Not :synchronized - lock-free reads on the worker hot path
   (precondition expansion reads here ~59M times/run in queensN-csp profile).")

(sb-ext:defglobal *type-signatures* (make-hash-table :test #'eq :size 256 :rehash-threshold 1.0)
  "Maps each type name to its resolved instance list, as most recently installed by
   install-types. Used to detect a type declared with conflicting instance lists across
   multiple tech or problem files. Written only during init(); not consulted during search.")

(sb-ext:defglobal *type-components* (make-hash-table :test #'eq :size 256 :rehash-threshold 1.0)
  "Maps each composite type alias -- one declared as (either ...) -- to its declared
   component type list, captured by install-types before the alias is flattened to its
   instance union.  Lets the translator reason about schema-level type compatibility:
   two type-specs sharing a leaf type are compatible even in a problem that declares no
   instances of that leaf (the guarded branch is dead there, not mistyped).  Written
   only during init(); not consulted during search.")

(sb-ext:defglobal *relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))
  "Dynamic relations.")

(sb-ext:defglobal *derived-relations*
  (make-hash-table :test #'eq :size 32 :rehash-threshold 1.0)
  "Dynamic relations whose initial facts are computed rather than authored.")

(sb-ext:defglobal *static-relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))
  "Static relations.")

(sb-ext:defglobal *connectives* '(and or not)
  "Logical connectives.")

(sb-ext:defglobal *symmetrics*
  (make-hash-table :test #'eq :size 64 :rehash-threshold 1.0)
  "Symmetric relations.
   Written only during init(); strictly read-only during search.
   Pre-allocated to 64 (well above any realistic symmetric-relation count)
   with rehash-threshold 1.0 so the table never resizes.
   Not :synchronized - lock-free reads on the worker hot path
   (add-proposition calls gethash here ~70M times/run in queensN-csp profile).")

(sb-ext:defglobal *complements*
  (make-hash-table :test #'eq :size 128 :rehash-threshold 1.0)
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
  (make-hash-table :size 2003 :rehash-threshold 1.0)
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

(defparameter *deferred-action-installers* nil  ;don't use define-global
  "Installer functions, in registration order, for actions whose definition depends on
   technologies that may be spliced after the file defining them.  INIT drains this list
   before ordering *actions*, so a deferred action sees every type, relation, and instance
   the problem declares regardless of the order its (include-tech ...) directives appear
   in.")

(sb-ext:defglobal *init-actions* nil
  "List of all initialization actions.")


(sb-ext:defglobal *init-checks* nil
  "Technology-owned functions that validate raw DEFINE-INIT literals.")

(sb-ext:defglobal *init-literal-generators* nil
  "Technology-owned functions that derive additional DEFINE-INIT literals from the
   problem's declared types.  They run before any literal is checked or installed, so a
   derived literal is indistinguishable from an authored one to every later stage.")

(sb-ext:defglobal *init-literal-defaults*
  (make-hash-table :test #'eq :size 32 :rehash-threshold 1.0)
  "Default values for a relation's trailing arguments, keyed by relation name.  A
   DEFINE-INIT literal omitting those arguments is padded with them before any literal is
   checked or installed, so every later stage sees one arity.  The owning technology
   registers its own relation's defaults; the engine attaches no meaning to any of them.")

(sb-ext:defglobal *problem-function-names* nil
  "Problem-local Lisp functions removed when another problem is staged.")

(sb-ext:defglobal *test-claims* nil
  "Named characterization claims registered by the staged test problem.")

(sb-ext:defglobal *test-mutations* nil
  "Deliberately broken definitions registered by the staged test problem.")

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

(defvar *detect-propagated-changes* nil
  "Gate for automatic change detection in add-prop/del-prop. When T, those setters
   set *propagated-state-changed* on any write that actually changes stored state.
   A problem's propagate-changes! binds this to T around its fixpoint loop; default
   NIL leaves the search hot path untouched.")

(defvar *propagated-state-changed* nil
  "Dirty flag set by add-prop/del-prop (when *detect-propagated-changes* is T) on a
   real database mutation. A propagation pass binds it to NIL, runs its derivations,
   and returns it to signal whether another convergence pass is needed.")

(defvar *applying-init-action* nil
  "Dynamically true while an init-action effect constructs the initial world state.
   Stateful propagation may use this to establish its physical baseline without
   interpreting already-authored initial facts as transitions that happen during search.")

(defvar *idb-hash-acc* nil
  "When non-nil during effect application, holds the running XOR hash of the integer
   database being mutated, seeded from the parent state's idb-hash. fold-store and
   fold-remove fold each add/delete into it, so a successor's idb-hash is known
   without rescanning the whole idb. NIL disables folding, leaving non-effect database
   paths (init, propagation, replay) untouched. Per-effect/per-thread dynamic binding.")


(defvar *fixed-idb-hash-acc* nil
  "Running XOR hash of entries outside symmetry families during canonical effects.")


(defvar *symmetry-idb-acc* nil
  "Running symmetric IDB slice during canonical effects.")


(defvar *symmetry-idb-touched-p* nil
  "Set by fold-store/fold-remove when the active canonical effect writes a
   symmetry-family-referencing entry. NIL afterward means the symmetric slice
   is unchanged, so the parent's canonical form can be carried forward instead
   of rebuilt.")


(defvar *validate-idb-hash* nil
  "Debug gate. When T, every successor's incrementally-carried idb-hash is checked
   against a full compute-idb-hash rescan (validate-carried-hash); a mismatch signals
   an error. Leave NIL for production search; enable only to validate the fold.")

(sb-ext:defglobal *global-invariants* nil
  "List of invariant query functions to check on every state.")

(sb-ext:defglobal *inconsistent-states-dropped* 0
  "Count of generated successors discarded for carrying the INCONSISTENT-STATE marker.")
(declaim (type fixnum *inconsistent-states-dropped*))


(sb-ext:defglobal *lower-bound-pruned* 0
  "Count of nodes pruned by user-defined min-steps-remaining? function.")
(declaim (type fixnum *lower-bound-pruned*))

(sb-ext:defglobal *search-prefix-pruned* 0
  "Count of successor states rejected by enabled search-prefix validators.")
(declaim (type fixnum *search-prefix-pruned*))


(sb-ext:defglobal *novelty-pruned* 0
  "Count of successor states discarded by the novelty test.")
(declaim (type fixnum *novelty-pruned*))


(sb-ext:defglobal *successor-policy-pruned* 0
  "Count of successor states rejected by registered search-successor pruners, such as the
   recorder's live/ghost interleaving canonicalization.")
(declaim (type fixnum *successor-policy-pruned*))

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
  (let ((vals-file (instance-vals-file (asdf:system-source-directory :wouldwork))))
    (unless (probe-file vals-file)
      (reset-problem-parameters-to-defaults))))
