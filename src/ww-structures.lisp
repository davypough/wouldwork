;;; Filename: ww-structures.lisp

;;; Structure definitions.


(in-package :ww)


(defstruct (problem-state (:conc-name problem-state.) (:copier nil))
  "A planning state including the current propositional database."
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;a list of (object (next-index next-time next-direction)) pairs
  (time 0.0 :type real)
  (value 0.0 :type real)
  (heuristic 0.0 :type real)
  (idb (make-hash-table) :type hash-table)  ;integer hash table of propositions
  (hidb (make-hash-table) :type hash-table)  ;integer table for happening events
  (idb-hash nil :type (or null fixnum))  ;final hash used for graph identity
  (fixed-idb-hash nil :type (or null fixnum))  ;incremental hash of propositions outside symmetry families
  (symmetry-idb nil :type (or null hash-table))  ;propositions that reference symmetry-family objects
  (canonical-symmetry-form :uncached :type (or list (eql :uncached)))  ;canonical form of symmetry-idb
  (canonical-form-hash nil :type (or null fixnum)))  ;memoized hash of canonical-symmetry-form; cleared whenever the form is
;Note: hidb is separate from idb because otherwise each exogenous event will change
;the state, leading to endless revisiting of the same similar state
;Note: happenings contains an entry for each object's next event, updated as events occur


(defmethod print-object ((ps problem-state) stream)
  (if *print-readably*
    (call-next-method)  ;lisp readable
    (print-unreadable-object (ps stream :type t :identity nil)
      (print-problem-state ps stream))))  ;print to terminal human readable


(defun convert-to-proposition (integer)
  "Converts an integer code back to a proposition."
  (iter (with x = integer)
        (for (values int triple) = (truncate x 1000))
        (collecting triple into int-list)
        (until (zerop int))
        (setf x int)
        (finally (return (mapcar (lambda (i)
                                   (gethash i *integer-constants*))
                                 int-list)))))


(defun get-prop-fluent-indices (proposition)
  (gethash (car proposition) *fluent-relation-indices*))


(defun convert-to-fluent-proposition (key vals)
  "Converts an idb partial prop -> index values into literal prop."
  (loop with partial-prop = (convert-to-proposition key)
        for index in (get-prop-fluent-indices partial-prop)
        for val in vals
          do (setf partial-prop (ut::ninsert-list val index partial-prop))
        finally (return partial-prop)))


(defun list-database (idb)
  "Used to printout idb in propositional form."
  (let* ((propositions (iter (for (key val) in-hashtable idb)
                             (cond
                               ;; Skip entries where val is a list containing NIL
                               ;((and (listp val) (member nil val)) nil)
                               ;; Process non-fluent propositions with value T
                               ((eql val t) (collecting (convert-to-proposition key)))
                               ;; Process fluent propositions (lists without NIL)
                               (t (collecting (convert-to-fluent-proposition key val))))))
         (sorted-props (sort (copy-list propositions) #'string< :key (lambda (prop) (format nil "~A" (car prop))))))
    sorted-props))


(defun database (state)
  "Prints the current database for state.
   Use as (ut::prt (database state)) as diagnostic in rules & functions."
  (list-database (problem-state.idb state)))


(defun print-problem-state (state &optional (stream t) depth)  ;potential bug here?
  (declare (type problem-state state) (ignore depth))
  (format stream "<~A ~A ~A ~A ~A ~A~%  ~S~%  ~S>"
          (problem-state.name state)
          (problem-state.instantiations state)
          (problem-state.happenings state)
          (problem-state.time state)
          (problem-state.value state)
          (problem-state.heuristic state)
          (list-database (problem-state.idb state))
          (list-database (problem-state.hidb state))))


(defun %copy-problem-state (state copy-hidb-p)
  "Build a state copy, optionally sharing its happenings database.
   COPY-HIDB-P may be false only in a problem with no happenings.  In that mode every
   generated proposition access names IDB directly, and no planner path can mutate HIDB,
   so both ordinary and temporary effect states may share the unreachable table."
  (declare (type problem-state state)
           (type boolean copy-hidb-p))
  (make-problem-state
    :name (problem-state.name state)
    :instantiations (copy-tree (problem-state.instantiations state))
    :happenings (copy-tree (problem-state.happenings state))
    :time (problem-state.time state)
    :value (problem-state.value state)
    :heuristic (problem-state.heuristic state)
    :idb (copy-idb (problem-state.idb state))
    :hidb (if copy-hidb-p
            (copy-idb (problem-state.hidb state))
            (problem-state.hidb state))
    :idb-hash nil
    :fixed-idb-hash (problem-state.fixed-idb-hash state)
    :symmetry-idb (when (problem-state.symmetry-idb state)
                    (copy-idb (problem-state.symmetry-idb state)))
    :canonical-symmetry-form :uncached
    :canonical-form-hash nil))


(defun copy-problem-state (state)
  "Copy STATE, sharing its unreachable HIDB only when the problem has no happenings."
  (declare (type problem-state state))
  (%copy-problem-state state (and *happening-names* t)))


(defun copy-problem-state-for-effect (state copy-hidb-p)
  "Copy STATE for generated ASSERT evaluation.
   COPY-HIDB-P is fixed by the translator from whether the staged problem has happenings."
  (declare (type problem-state state)
           (type boolean copy-hidb-p))
  (%copy-problem-state state copy-hidb-p))


(defun copy-idb (idb)
  "Copy a Wouldwork database's table structure.
   Database values are immutable: fluent updates replace their value lists rather than
   modifying them.  Child states can therefore share those lists while retaining an
   independently mutable, pre-sized hash table."
  (declare (type hash-table idb))
  (let ((new-idb (make-hash-table :test (hash-table-test idb)
                                  :size (hash-table-count idb)
                                  :rehash-size (hash-table-rehash-size idb)
                                  :rehash-threshold (hash-table-rehash-threshold idb)
                                  :synchronized nil)))
    (maphash (lambda (k v)
               (setf (gethash k new-idb) v))
             idb)
    new-idb))


(defun invalidate-problem-state-hash (state)
  "Clear every cached hash component after STATE's IDB changes outside folding."
  (declare (type problem-state state))
  (setf (problem-state.idb-hash state) nil
        (problem-state.fixed-idb-hash state) nil
        (problem-state.symmetry-idb state) nil
        (problem-state.canonical-symmetry-form state) :uncached
        (problem-state.canonical-form-hash state) nil)
  state)


(defparameter *start-state* (make-problem-state)
  "Start search from this state.")
(declaim (problem-state *start-state*))


(defstruct (action (:conc-name action.))
  (name nil :type symbol)
  (pre-defun-name nil :type symbol)
  (eff-defun-name nil :type symbol)
  (duration 0.0 :type real)
  (precondition-params nil :type list)
  (precondition-variables nil :type list)
  (precondition-types nil :type list)
  (precondition-type-inst nil :type list)
  (dynamic nil :type list)  ;a dynamic rule requires recomputation of params on each execution
  (precondition-args nil :type (or list symbol))
  (precondition-form nil :type list)  ;the user's specified precondition
  (effect-form nil :type list)  ;the user's specified effect
  (init nil :type (member nil t))  ;signals if an init-action or a normal rule action
  (precondition-lambda nil :type list)
  (iprecondition-lambda nil :type list)
  (effect-variables nil :type list)
  (effect-format nil :type list)  ;annotated effect list w/ string connectives, display only
  (effect-adds nil :type list)  ;relation symbols modified by this action's effect (set by installer)
  (effect-lambda nil :type list)
  (ieffect-lambda nil :type list))


(defstruct (update (:conc-name update.))
  "Db updates resulting from a successful action instantiation."
  (changes nil :type (or hash-table list))
  (value 0.0 :type real)
  (instantiations nil :type list)
  (followups nil :type list)    ;next & finally followup function calls
  (sim-state nil)               ;strategic-wait simulation state
  (hash nil :type (or null fixnum))  ;incremental standard idb-hash carried out of the effect
  (fixed-idb-hash nil :type (or null fixnum))  ;incremental fixed component in canonical mode
  (symmetry-idb nil :type (or null hash-table))  ;symmetric slice carried out of the effect
  (canonical-symmetry-form :uncached :type (or list (eql :uncached)))  ;parent's form, carried forward when the slice is untouched
  (canonical-form-hash nil :type (or null fixnum)))  ;parent's memoized form-hash, carried forward alongside it


(defstruct (solution (:conc-name solution.))
  "The record of a solution."
  (depth 0 :type fixnum)
  (time 0.0 :type real)
  (value 0.0 :type real)
  (path nil :type list)
  (goal (make-problem-state) :type problem-state))


(defun solution-better-p (new-solution old-solution)
  "Whether NEW-SOLUTION is preferred to OLD-SOLUTION for *SOLUTION-TYPE*."
  (case *solution-type*
    ((min-length first every all-paths)
     (< (solution.depth new-solution) (solution.depth old-solution)))
    (min-time
     (< (solution.time new-solution) (solution.time old-solution)))
    (min-value
     (< (solution.value new-solution) (solution.value old-solution)))
    (max-value
     (> (solution.value new-solution) (solution.value old-solution)))
    (otherwise
     ;; A positive integer requests that many solutions; continuation uses the shortest.
     (< (solution.depth new-solution) (solution.depth old-solution)))))


(defun copy-solution-deeply (solution)
  "Copy SOLUTION, including its path and final problem state."
  (make-solution
    :depth (solution.depth solution)
    :time (solution.time solution)
    :value (solution.value solution)
    :path (copy-tree (solution.path solution))
    :goal (copy-problem-state (solution.goal solution))))


(defun copy-solutions-deeply (solutions)
  "Return independent copies of every solution in SOLUTIONS."
  (mapcar #'copy-solution-deeply solutions))


(defstruct (node (:conc-name node.)
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (type node node) (type stream stream))
                 (format stream "~&NODE: STATE=~A DEPTH=~:D"   ;PARENT=~S~%"
                   (node.state node) (node.depth node)))))
  (state (make-problem-state) :type problem-state)    ;problem state
  (depth 0 :type fixnum)           ;depth in the search tree
  (parent nil :type (or null node list))  ;this node's parent
  (wait-tried nil :type boolean))  ;tracks if backtrack-wait has been attempted


(defun node-parents-list (node)
  "Returns the parent node(s) of NODE as a list.
   Normalizes access for both standard mode (single parent) and hybrid mode (parent-move pairs).
   In hybrid mode, extracts just the parent nodes from (parent-node . move) pairs."
  (declare (type node node))
  (let ((parent (node.parent node)))
    (cond ((null parent) nil)
          (*hybrid-mode*
           ;; Hybrid mode: parent is list of (parent-node . move) pairs
           (mapcar #'car parent))
          ((listp parent) parent)
          (t (list parent)))))


(defun node-parent-entries (node)
  "Returns the parent entries of NODE for hybrid mode path enumeration.
   Returns list of (parent-node . move) pairs.
   Only valid in hybrid mode; returns nil otherwise."
  (declare (type node node))
  (when *hybrid-mode*
    (node.parent node)))


(defun add-parent-to-node (node new-parent &optional move)
  "Adds NEW-PARENT to NODE's parent slot for hybrid mode.
   In hybrid mode, stores (parent-node . move) pairs; checks for duplicate parents.
   In non-hybrid mode, stores just the parent node.
   Thread-safe: uses locking when *threads* > 0."
  (declare (type node node new-parent))
  (flet ((do-add ()
           (let ((current (node.parent node)))
             (cond (*hybrid-mode*
                    ;; Hybrid mode: store (parent-node . move) pairs
                    (let ((new-entry (cons new-parent move)))
                      (cond ((null current)
                             (setf (node.parent node) (list new-entry)))
                            ;; Check for duplicate parent (ignore move in comparison)
                            ((not (member new-parent current :key #'car :test #'eq))
                             (setf (node.parent node) (cons new-entry current))))))
                   ;; Non-hybrid mode: original behavior
                   ((null current)
                    (setf (node.parent node) (list new-parent)))
                   ((listp current)
                    (setf (node.parent node) (cons new-parent current)))
                   (t
                    (setf (node.parent node) (list new-parent current)))))))
    (if (> *threads* 0)
        (bt:with-lock-held (*lock*)
          (do-add))
        (do-add))))
