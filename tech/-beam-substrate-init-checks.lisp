;;; Filename: -beam-substrate-init-checks.lisp

;;; Initialization validation for repeaters and fixed beam corridors.


(in-package :ww)


(define-init-check beam-substrate-init-check (literals)
  (:consumes gate location)
  (check-init-repeater-consistency literals)
  (check-init-list-relation-items-have-types
    literals 'beam-via '(gate location))
  (check-init-coupled-beam-consistency literals))


(define-init-check-helper check-init-repeater-consistency (literals)
  "Checks repeater mounting, and coordinates when that capability is installed."
  (let ((coordinates-required (init-relation-signature 'apparatus-coords>))
        (coordinate-literals
          (positive-init-literals-with-relation 'apparatus-coords> literals)))
    (dolist (repeater (init-type-instances 'repeater))
      (let ((floor-mounted (init-type-member-p repeater 'floor-repeater))
            (wall-mounted (init-type-member-p repeater 'wall-repeater))
            (coordinate-count
              (count repeater coordinate-literals
                     :key (lambda (literal)
                            (second (init-literal-proposition literal))))))
        (when (eql floor-mounted wall-mounted)
          (fail-init-check nil "~%Repeater must have exactly one mounting orientation.~%~
                  Repeater: ~S~%~
                  FLOOR-REPEATER: ~S~%~
                  WALL-REPEATER:  ~S"
                 repeater floor-mounted wall-mounted))
        (when (and coordinates-required
                   (/= coordinate-count 1))
          (fail-init-check nil "~%Repeater must have exactly one APPARATUS-COORDS> functional point.~%~
                  Repeater: ~S~%~
                  Coordinate facts: ~D"
                 repeater coordinate-count))))))


(define-init-check-helper init-chroma-map (literals)
  (let ((chromas (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'has-chroma literals))
      (destructuring-bind (endpoint hue)
          (rest (init-literal-proposition literal))
        (setf (gethash endpoint chromas) hue)))
    chromas))


(define-init-check-helper init-coupled-p (source destination literals)
  (some (lambda (literal)
          (destructuring-bind (coupled-source coupled-destination)
              (rest (init-literal-proposition literal))
            (and (eql source coupled-source)
                 (eql destination coupled-destination))))
        (init-literals-with-relation 'coupled literals)))


(define-init-check-helper init-beam-via-p (source destination literals)
  (some (lambda (literal)
          (destructuring-bind (beam-source obstacles beam-destination)
              (rest (init-literal-proposition literal))
            (declare (ignore obstacles))
            (and (eql source beam-source)
                 (eql destination beam-destination))))
        (init-literals-with-relation 'beam-via literals)))


(define-init-check-helper check-init-coupled-beam-consistency (literals)
  "Checks directional fixed-apparatus beam declarations and their corridors."
  (let ((chromas (init-chroma-map literals)))
    (dolist (literal (init-literals-with-relation 'coupled literals))
      (destructuring-bind (source destination)
          (rest (init-literal-proposition literal))
        (let ((source-hue (gethash source chromas))
              (destination-hue (gethash destination chromas)))
          (when (and (init-type-member-p source 'transmitter)
                     (not source-hue))
            (fail-init-check nil "~%COUPLED transmitter has no HAS-CHROMA entry.~%~
                    Literal:     ~S~%~
                    Transmitter: ~S"
                   literal source))
          (when (and (init-type-member-p destination 'receiver)
                     (not destination-hue))
            (fail-init-check nil "~%COUPLED receiver has no HAS-CHROMA entry.~%~
                    Literal: ~S~%~
                    Receiver: ~S"
                   literal destination))
          (when (and (init-type-member-p source 'transmitter)
                     (init-type-member-p destination 'receiver)
                     (not (eql source-hue destination-hue)))
            (fail-init-check nil "~%COUPLED endpoints have mismatched HAS-CHROMA values.~%~
                    Literal:         ~S~%~
                    Transmitter hue: ~S~%~
                    Receiver hue:    ~S"
                   literal source-hue destination-hue))
          (unless (init-beam-via-p source destination literals)
            (fail-init-check nil "~%COUPLED pair has no matching BEAM-VIA corridor.~%~
                    Literal:       ~S~%~
                    Expected beam: (BEAM-VIA ~S ... ~S)"
                   literal source destination))))))
  (dolist (literal (init-literals-with-relation 'beam-via literals))
    (destructuring-bind (source obstacles destination)
        (rest (init-literal-proposition literal))
      (declare (ignore obstacles))
      (unless (init-coupled-p source destination literals)
        (fail-init-check nil "~%BEAM-VIA corridor has no matching COUPLED pair.~%~
                Literal:        ~S~%~
                Expected pair:  (COUPLED ~S ~S)"
               literal source destination)))))

