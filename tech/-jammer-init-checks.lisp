;;; Filename: -jammer-init-checks.lisp

;;; Initialization validation for active jammer placement and target topology.
;;; A held jammer is deliberately unlocated, so it cannot also start in JAMMING state.


(in-package :ww)


(define-init-check jammer-init-check (literals)
  (check-init-jamming-consistency literals))


(define-init-check-helper init-check-jamming-target-sightline
    (literal jammer jammer-location target positions literals)
  (when (init-relation-signature 'los-via)
    (cond
      ((or (init-type-member-p target 'gate)
           (init-type-member-p target 'gun))
       (unless (init-apparatus-has-potential-sightline-p target literals)
         (fail-init-check nil "~%JAMMING target has no potential LOS-VIA from any location.~%~
                 Literal: ~S~%~
                 Jammer:  ~S~%~
                 Target:  ~S"
                literal jammer target)))
      ((gethash target positions)
       (let ((target-location (gethash target positions)))
         (unless (or (eql jammer-location target-location)
                     (init-location-has-potential-sightline-p
                       target-location literals))
           (fail-init-check nil "~%JAMMING target location has no potential LOS-VIA from any location.~%~
                   Literal:         ~S~%~
                   Jammer:          ~S~%~
                   Jammer location: ~S~%~
                   Target:          ~S~%~
                   Target location: ~S"
                  literal jammer jammer-location target target-location)))))))


(define-init-check-helper check-init-jamming-consistency (literals)
  "Require each initially active jammer to be placed, unheld, and physically targetable."
  (let ((locations (init-literal-map 'has-location literals 1 2))
        (positions (init-literal-map 'has-position literals 1 2))
        (held-objects (init-held-objects literals)))
    (dolist (literal (positive-init-literals-with-relation 'jamming literals))
      (destructuring-bind (jammer target)
          (rest (init-literal-proposition literal))
        (when (gethash jammer held-objects)
          (fail-init-check nil "~%JAMMING jammer is held and therefore unlocated.~%~
                  Literal: ~S~%~
                  Jammer:  ~S"
                 literal jammer))
        (let ((jammer-location (gethash jammer locations)))
          (unless jammer-location
            (fail-init-check nil "~%JAMMING jammer has no HAS-LOCATION.~%~
                    Literal: ~S~%~
                    Jammer:  ~S"
                   literal jammer))
          (init-check-jamming-target-sightline
            literal jammer jammer-location target positions literals))))))
