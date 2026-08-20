;;; Filename: walkability.lisp

;;; Walking mobility provider.  Enabled WALK-VIA/WALK-VIA> edges become normalized WALK
;;; traversal segments.  The central mobility closure composes them and MOVE applies one
;;; canonical route per grounded destination.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy; -location; -passability; -elevation;
;;;               -walkability; -walkability-coordinates; -threat; -mobility-action
;;; PROVIDES:
;;;   relations : (walk-via location $list location), (walk-via> location $list location)
;;;   queries   : walking-traversal-segments, one-step-walkable
;;;   provider  : walking-traversal-segments registered with -mobility
;;;   action    : move (from -mobility-action)

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -vertical)
(include-tech -elevation)
(include-tech -walkability)
(include-tech -walkability-coordinates)
(include-tech -threat)
(include-tech -mobility-action)

(in-package :ww)


(define-problem-helper walking-segment-for-family
    (state agent source destination family)
  "Return a normalized WALK segment using the canonical passing DNF clause."
  (when (and (= (funcall (symbol-function 'location-elevation) state source)
                (funcall (symbol-function 'location-elevation) state destination))
             (funcall (symbol-function 'safe) state destination))
    (cond ((null family)
           (list 'walk source nil destination))
          (t
           (let ((passing
                   (remove-if-not
                     (lambda (clause)
                       (funcall (symbol-function 'all-clear) state agent clause))
                     (walkability-canonical-family family))))
             (when passing
               (list 'walk source (first passing) destination)))))))


(define-query walking-traversal-segments (?agent agent ?from location)
  (do (assign $segments nil)
      (doall (?to location)
        (do (assign $symmetric-segment nil)
            (assign $directional-segment nil)
            (if (bind (walk-via ?from $symmetric-family ?to))
              (assign $symmetric-segment
                      (walking-segment-for-family
                        state ?agent ?from ?to $symmetric-family)))
            (if (bind (walk-via> ?from $directional-family ?to))
              (assign $directional-segment
                      (walking-segment-for-family
                        state ?agent ?from ?to $directional-family)))
            (if $symmetric-segment
              (assign $segments (cons $symmetric-segment $segments)))
            (if $directional-segment
              (assign $segments (cons $directional-segment $segments)))))
      $segments))


(register-mobility-provider 'walking-traversal-segments)


(define-query one-step-walkable (?agent agent ?from location ?to location)
  (ww-loop for $segment in (walking-traversal-segments ?agent ?from)
           thereis (eql (fourth $segment) ?to)))
