;;; Filename: -mobility-action.lisp

;;; Central transparent-mobility action.  Traversal providers compute routes without
;;; mutating state; MOVE applies exactly one canonical result to a grounded agent.
;;;
;;; REQUIRES:
;;;   nested   : -mobility; -location; -support-occupancy; -propagation
;;; PROVIDES:
;;;   action   : move

(include-tech -mobility)
(include-tech -location)
(include-tech -support-occupancy)
(include-tech -propagation)

(in-package :ww)


(define-action move
  1
  (?agent agent)
  (and (bind (has-location ?agent $source))
       (not (bind (on ?agent $support)))
       (assign $mobility-results (mobility-results ?agent $source)))
  (">" ?agent "moves from" $source "to" $destination "via" $route)
  (ww-loop for $result in $mobility-results
           do (if (different $source (first $result))
                (assert
                  (has-location ?agent (first $result))
                  (assign $destination (first $result))
                  (assign $route (second $result))
                  (finally (propagate-changes!))))))
