;;; Filename: constraint-state-probe.lisp

;;; Optional state probe for the constraint-led analysis method.  Replays a validated
;;; action prefix and lists every action the engine's own successor generator
;;; (GENERATE-CHILDREN) accepts in the resulting state, printed as replay-readable
;;; phrases.  One step ahead only; no search runs.
;;;
;;; THIS FILE IS A LOADABLE DIAGNOSTIC, like tech/constraint-profile.lisp: never named in
;;; an (include-tech ...) directive, not an ASDF component, plain Common Lisp in :WW, no
;;; problem object names.  Callees precede callers because it is reloaded by hand.
;;;
;;; Usage, after staging the problem (for example by loading a validation file, which
;;; stages and defines its action list):
;;;
;;;   (load (merge-pathnames "tech/constraint-state-probe.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;;   (report-applicable-actions <action-list>)
;;;   (report-applicable-actions <action-list> :action 'move :object 'agent1)
;;;
;;; ACTION is an action name or a list of names; OBJECT is a symbol that must appear
;;; somewhere in the action's arguments.  The prefix replays from the staged initial state,
;;; so stage fresh before calling.  Symmetry pruning is disabled for the enumeration so no
;;; equivalent instantiation is hidden.

(in-package :ww)


(defun applicable-action-forms (state)
  "Return the (name . arguments) form of every child GENERATE-CHILDREN produces from STATE."
  (let ((*algorithm* 'depth-first)
        (*symmetry-pruning* nil))
    (mapcar (lambda (child)
              (cons (problem-state.name child) (problem-state.instantiations child)))
            (generate-children (make-node :state state :depth 0)))))


(defun applicable-action-matches-p (form action object)
  "True when FORM passes the optional ACTION-name and OBJECT filters."
  (and (or (null action)
           (member (first form) (alexandria:ensure-list action)))
       (or (null object)
           (member object (alexandria:flatten (rest form))))))


(defun report-applicable-actions (action-list &key action object)
  "Replay ACTION-LIST from the staged initial state, then print and return the applicable
next actions, optionally filtered by ACTION name(s) and an OBJECT in their arguments."
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source action-list))
         (all-forms nil)
         (forms nil))
    (unless (action-sequence-validation-success-p validation)
      (error "Prefix replay failed at action ~S: ~S~%REASON: ~A"
             (action-sequence-validation-failure-index validation)
             (action-sequence-validation-failure-action validation)
             (action-sequence-validation-failure-reason validation)))
    (setf all-forms (applicable-action-forms
                      (action-sequence-validation-final-state validation)))
    (setf forms (remove-if-not (lambda (form)
                                 (applicable-action-matches-p form action object))
                               all-forms))
    (format t "~&APPLICABLE ACTIONS after ~D-action prefix: ~D total, ~D shown~@[ (action ~S)~]~@[ (object ~S)~]~%"
            (action-sequence-validation-action-count validation)
            (length all-forms) (length forms) action object)
    (loop for form in forms
          for index from 1
          do (format t "~3D  ~A~%" index (format-action-for-display form)))
    forms))
