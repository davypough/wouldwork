;;; Filename: -mobility-action.lisp

;;; Central movement action.  Transparent traversal providers compute grounded routes
;;; without mutating state, while configuration-transition providers expose one explicit
;;; support-changing boundary at a time.  MOVE presents both as routes between normalized
;;; (location ground-or-support) configurations and applies exactly one result.
;;;
;;; REQUIRES:
;;;   nested   : -propagation; -mobility; -configuration-transition
;;;   driver   : propagate-changes! (master), called by MOVE's effect and nested above
;;;              rather than assumed from a peer
;;; PROVIDES:
;;;   queries  : movement-results, movement-results-in-state, action-movement-results,
;;;              replay-movement-results
;;;   action   : move

(include-tech -propagation)
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


(define-query movement-results-in-state (?agent agent ?source-configuration)
  ;; Grounded routes before explicit support transitions.  The source location comes from
  ;; HAS-LOCATION rather than the configuration, so no support reaches plain Lisp.
  (do (bind (has-location ?agent $source-location))
      (assign $routes
              (ww-loop for $transition in (configuration-transition-results ?agent)
                       collect (list (fourth $transition) (list $transition))))
      (if (eql (second ?source-configuration) 'ground)
        (append (grounded-movement-results-in-state state ?agent $source-location)
                $routes)
        $routes)))


(define-problem-helper replay-grounded-movement-result
    (state agent source-location route)
  "Check every supplied transparent segment of ROUTE from grounded SOURCE-LOCATION before
   constructing its endpoint.  A segment need not be the one search would choose, only a
   legal one: see MOBILITY-SEGMENT-REPLAYABLE-P.  An empty or closed route yields NIL."
  (let ((location source-location))
    (dolist (segment route)
      (unless (mobility-segment-replayable-p state agent location segment)
        (return-from replay-grounded-movement-result nil))
      (setf location (fourth segment)))
    (unless (eql location source-location)
      (list (list location 'ground) route))))


(define-query replay-movement-results (?agent agent ?source-configuration ?route)
  ;; Accept a legal transparent route from the ground, or one explicit support transition.
  (do (bind (has-location ?agent $source-location))
      (assign $grounded
              (if (eql (second ?source-configuration) 'ground)
                (replay-grounded-movement-result state ?agent $source-location ?route)))
      (if $grounded
        (list $grounded)
        (if (and (= (length ?route) 1)
                 (member (first ?route)
                         (configuration-provider-transitions ?agent ?source-configuration)
                         :test #'equal))
          (list (list (fourth (first ?route)) ?route))))))


(define-query action-movement-results (?agent agent ?source-configuration)
  ;; Use supplied MOVE routes during replay and canonical routes during search.
  (if (eq (first *replay-action*) 'move)
    (if (eq ?agent (second *replay-action*))
      (replay-movement-results ?agent ?source-configuration (third *replay-action*)))
    (movement-results-in-state ?agent ?source-configuration)))


(define-query movement-results (?agent agent)
  ;; A mapped ghost has no HAS-LOCATION until START-RECORDER forks it (rule 5), and an
  ;; agent that does not exist yet has no mobility.  Without this guard
  ;; AGENT-CONFIGURATION returns (NIL GROUND) -- BIND's failure is not propagated by DO --
  ;; and the configuration providers then compute an elevation for the NIL location.
  (if (bind (has-location ?agent $agent-location))
    (do (assign $source-configuration (agent-configuration ?agent))
        (assign $results
                (movement-results-in-state ?agent $source-configuration))
        $results)))


(define-action move
  1
  (?agent agent)
  (and (bind (has-location ?agent $agent-location))
       (assign $source-configuration (agent-configuration ?agent))
       (assign $movement-results
               (action-movement-results ?agent $source-configuration)))
  (">" ?agent "moves via" $route)
  (ww-loop for $result in $movement-results
           do (assert
                (assign $destination-configuration (first $result))
                (assign $route (second $result))
                (apply-agent-configuration!
                  ?agent $destination-configuration)
                (finally (propagate-changes!)))))
