;;; Filename: -mobility-action.lisp

;;; Central movement action.  Transparent traversal providers compute grounded routes
;;; without mutating state, while configuration-transition providers expose one explicit
;;; support-changing boundary at a time.  MOVE presents both as routes between normalized
;;; (location ground-or-support) configurations and applies exactly one result.
;;;
;;; REQUIRES:
;;;   nested   : -mobility; -configuration-transition
;;; PROVIDES:
;;;   query    : movement-results
;;;   action   : move

(include-tech -mobility)
(include-tech -configuration-transition)

(in-package :ww)


(define-problem-helper grounded-movement-results-in-state
    (state agent source-location)
  "Return non-reflexive mobility results with normalized ground configurations."
  (loop for result in
          (funcall (symbol-function 'mobility-results)
                   state agent source-location)
        unless (eql (first result) source-location)
          collect (list (list (first result) 'ground)
                        (second result))))


(define-problem-helper transition-movement-results-in-state (state agent)
  "Return configuration transitions as singleton movement routes."
  (mapcar
    (lambda (transition)
      (list (fourth transition) (list transition)))
    (funcall (symbol-function 'configuration-transition-results)
             state agent)))


(define-problem-helper movement-results-in-state (state agent source-configuration)
  "Collect grounded routes before explicit support transitions for AGENT."
  (let ((transitions
          (transition-movement-results-in-state state agent)))
    (if (eql (second source-configuration) 'ground)
      (nconc
        (grounded-movement-results-in-state
          state agent (first source-configuration))
        transitions)
      transitions)))


(define-query movement-results (?agent agent)
  (do (assign $source-configuration (agent-configuration ?agent))
      (assign $results
              (movement-results-in-state
                state ?agent $source-configuration))
      $results))


(define-action move
  1
  (?agent agent)
  (do (assign $source-configuration (agent-configuration ?agent))
      (assign $movement-results
              (movement-results-in-state
                state ?agent $source-configuration)))
  (">" ?agent "moves via" $route)
  (ww-loop for $result in $movement-results
           do (assert
                (assign $destination-configuration (first $result))
                (assign $route (second $result))
                (apply-agent-configuration!
                  ?agent $destination-configuration)
                (finally (propagate-changes!)))))
