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
  (hidb (make-hash-table) :type hash-table))  ;integer table for happening events
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
          do (ut::ninsert-list val index partial-prop)
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
    :hidb (copy-idb (problem-state.hidb state))))


(defun copy-idb (idb)
  "Copies a Wouldwork database."
  (declare (type hash-table idb))
  (alexandria:copy-hash-table idb
    :key (lambda (val)
           (if (consp val)
             (copy-list val)
             val))))


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
  (followups nil :type list))  ;next & finally followup function calls


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
  (parent nil :type (or null node)))  ;this node's parent


