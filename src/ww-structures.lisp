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
  (idb-hash nil :type (or null fixnum)))  ;hash fixnum representing an idb
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
  (handler-case
      (format stream "<~A ~A ~A ~A ~A ~A~%  ~S~%  ~S>"
              (problem-state.name state)
              (problem-state.instantiations state)
              (problem-state.happenings state)
              (problem-state.time state)
              (problem-state.value state)
              (problem-state.heuristic state)
              (handler-case (list-database (problem-state.idb state))
                (error () "#<error printing idb>"))
              (handler-case (list-database (problem-state.hidb state))
                (error () "#<error printing hidb>")))
    (error (e)
      (format stream "#<ERROR PRINTING problem-state: ~A>" e))))


(defun copy-problem-state (state)
    (make-problem-state
      :name (problem-state.name state)
      :instantiations (copy-list (problem-state.instantiations state))
      :happenings (copy-tree (problem-state.happenings state))
      :time (problem-state.time state)
      :value (problem-state.value state)
      :heuristic (problem-state.heuristic state)
      :idb (copy-idb (problem-state.idb state))
      :hidb (copy-idb (problem-state.hidb state))
      :idb-hash (problem-state.idb-hash state)))


(defun copy-problem-state-without-idb (state)
  "Copies a problem-state but omits the idb field (leaves it as new empty hash table).
   Used when the idb will be immediately replaced with a different one."
  (make-problem-state
    :name (problem-state.name state)
    :instantiations (copy-list (problem-state.instantiations state))
    :happenings (copy-tree (problem-state.happenings state))
    :time (problem-state.time state)
    :value (problem-state.value state)
    :heuristic (problem-state.heuristic state)
    :idb (make-hash-table :test 'eql :synchronized nil)
    :hidb (copy-idb (problem-state.hidb state))
    :idb-hash nil))  ; will be recomputed when alist is created


(defun copy-idb (idb)
  "Copies a Wouldwork database with thread-safe hash table in parallel mode."
  (declare (type hash-table idb))
  (let ((new-idb (make-hash-table :test (hash-table-test idb)
                                  :synchronized nil)))  ;not shared (> *threads* 0))))
    (maphash (lambda (k v)
               (setf (gethash k new-idb)
                     (if (consp v)
                         (copy-list v)
                         v)))
             idb)
    new-idb))


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
  (init nil :type (member nil t))  ;signals if an init-action or a normal rule action
  (precondition-lambda nil :type list)
  (iprecondition-lambda nil :type list)
  (effect-variables nil :type list)
  (effect-adds nil :type list)  ;nonnegative literals only for backward search
  (effect-lambda nil :type list)
  (ieffect-lambda nil :type list))


(defstruct (update (:conc-name update.))
  "Db updates resulting from a successful action instantiation."
  (changes nil :type (or hash-table list))
  (value 0.0 :type real)
  (instantiations nil :type list)
  (followups nil :type list)    ;next & finally followup function calls
  (sim-state nil))              ;strategic-wait simulation state


(defstruct (solution (:conc-name solution.))
  "The record of a solution."
  (depth 0 :type fixnum)
  (time 0.0 :type real)
  (value 0.0 :type real)
  (path nil :type list)
  (goal (make-problem-state) :type problem-state))


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
