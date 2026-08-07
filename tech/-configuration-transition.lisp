;;; Filename: -configuration-transition.lisp

;;; Explicit agent-configuration transitions.  Providers describe one state-changing
;;; transition at a time as
;;;
;;;   (mode source-configuration witness destination-configuration)
;;;
;;; where each configuration is (location ground-or-support).  Unlike mobility, these
;;; transitions are never transitively closed: changing support is a planning boundary
;;; because it can clear or occupy supports and trigger propagation.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy; -location; -propagation
;;; PROVIDES:
;;;   query     : agent-configuration, configuration-transition-results
;;;   functions : register-configuration-transition-provider and canonical selection helpers
;;;   update    : apply-agent-configuration!

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -propagation)

(in-package :ww)


(defparameter *configuration-transition-providers* nil
  "Problem-local query names that return explicit agent-configuration transitions.")


(define-problem-helper register-configuration-transition-provider (provider)
  "Register a pure configuration-transition provider query for the staged problem."
  (unless (and (symbolp provider)
               (member provider *query-names* :test #'eq))
    (error "Configuration-transition provider must name an installed query: ~S"
           provider))
  (pushnew provider *configuration-transition-providers* :test #'eq)
  provider)


(define-problem-helper configuration-transition-key (transition)
  "Return the stable lexical key for a normalized semantic transition."
  (let ((*print-case* :upcase)
        (*print-pretty* nil)
        (*package* (find-package :ww)))
    (prin1-to-string transition)))


(define-problem-helper configuration-transition-precedes-p
    (transition1 transition2)
  "Order transitions by their normalized semantic representation."
  (string< (configuration-transition-key transition1)
           (configuration-transition-key transition2)))


(define-problem-helper valid-agent-configuration-p (configuration)
  "Whether CONFIGURATION is a normalized (location ground-or-support) pair."
  (and (listp configuration)
       (= (length configuration) 2)
       (member (first configuration) (gethash 'location *types*) :test #'eq)
       (or (eql (second configuration) 'ground)
           (member (second configuration) (gethash 'support *types*) :test #'eq))))


(define-problem-helper validate-configuration-transition (transition source)
  "Signal an error unless TRANSITION obeys the normalized provider contract."
  (unless (and (listp transition)
               (= (length transition) 4)
               (symbolp (first transition))
               (equal (second transition) source)
               (valid-agent-configuration-p (second transition))
               (valid-agent-configuration-p (fourth transition))
               (not (equal (second transition) (fourth transition))))
    (error "Invalid configuration transition from ~S: ~S" source transition))
  transition)


(define-problem-helper configuration-provider-transitions
    (state agent source)
  "Collect, validate, deduplicate, and canonically order provider transitions."
  (let ((transitions nil))
    (dolist (provider *configuration-transition-providers*)
      (dolist (transition (funcall (symbol-function provider) state agent source))
        (push (validate-configuration-transition transition source)
              transitions)))
    (sort (remove-duplicates transitions :test #'equal)
          #'configuration-transition-precedes-p)))


(define-problem-helper canonical-configuration-transitions (transitions)
  "Retain the first canonical transition to each distinct destination configuration."
  (let ((destinations (make-hash-table :test #'equal))
        (selected nil))
    (dolist (transition transitions (nreverse selected))
      (let ((destination (fourth transition)))
        (unless (gethash destination destinations)
          (setf (gethash destination destinations) t)
          (push transition selected))))))


(define-query agent-configuration (?agent agent)
  (do (bind (has-location ?agent $location))
      (assign $place
              (if (bind (on ?agent $support))
                $support
                'ground))
      (list $location $place)))


(define-query configuration-transition-results (?agent agent)
  (do (assign $source (agent-configuration ?agent))
      (assign $transitions
              (configuration-provider-transitions state ?agent $source))
      (canonical-configuration-transitions $transitions)))


(define-update apply-agent-configuration!
    (?agent agent ?destination-configuration)
  (do (if (bind (on ?agent $source-support))
        (not (on ?agent $source-support)))
      (has-location ?agent (first ?destination-configuration))
      (if (not (eql (second ?destination-configuration) 'ground))
        (on ?agent (second ?destination-configuration)))))
