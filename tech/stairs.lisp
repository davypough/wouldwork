;;; Filename: stairs.lisp

;;; Stairs mobility provider.  Authored STAIRS-VIA/STAIRS-VIA> edges connect grounded
;;; locations without imposing an elevation-difference limit.  Their enabling-means list
;;; is conjunctive: every listed obstacle must be passable for the moving agent.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -passability; -threat; -mobility-action
;;; PROVIDES:
;;;   relations : (stairs-via location $list location)
;;;               (stairs-via> location $list location)
;;;   queries   : stairs-traversal-segments
;;;   provider  : stairs-traversal-segments registered with -mobility
;;;   action    : move (from -mobility-action)

(include-tech -passability)
(include-tech -threat)
(include-tech -mobility-action)

(in-package :ww)


(define-static-relations
  (stairs-via location $list location)
  (stairs-via> location $list location))


(define-init-check stairs-init-check (literals)
  (:consumes gate screen ladder gears)
  (dolist (relation '(stairs-via stairs-via>))
    (check-init-list-relation-items-have-types
      literals relation
      '(gate screen ladder floor-gears wall-gears angled-gears))))


(define-problem-helper stairs-segment-for-means
    (state agent source destination means)
  "Return a normalized STAIRS segment when its means and destination are enabled."
  (let ((canonical-means (canonical-enabling-means means)))
    (when (and (funcall (symbol-function 'all-clear)
                        state agent canonical-means)
               (funcall (symbol-function 'safe) state destination))
      (list 'stairs source canonical-means destination))))


(define-query stairs-traversal-segments (?agent agent ?from location)
  (do (assign $segments nil)
      (doall (?to location)
        (do (assign $symmetric-segment nil)
            (assign $directional-segment nil)
            (if (bind (stairs-via ?from $symmetric-means ?to))
              (assign $symmetric-segment
                      (stairs-segment-for-means
                        state ?agent ?from ?to $symmetric-means)))
            (if (bind (stairs-via> ?from $directional-means ?to))
              (assign $directional-segment
                      (stairs-segment-for-means
                        state ?agent ?from ?to $directional-means)))
            (if $symmetric-segment
              (assign $segments (cons $symmetric-segment $segments)))
            (if $directional-segment
              (assign $segments (cons $directional-segment $segments)))))
      $segments))


(register-mobility-provider 'stairs-traversal-segments)
