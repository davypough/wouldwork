;;; Filename: -controls.lisp

;;; Controls substrate: the shared DNF controller wiring for controlled devices (gates and
;;; gears) and the energized query that evaluates a single controller.  Owned in one
;;; place so gate.lisp and -gears-fan.lisp nest this file instead of each declaring
;;; controls/energized; both previously lived in gate.lisp.  Each consuming tech still
;;; evaluates its own DNF aggregate in its own update, because the uncontrolled default
;;; differs by device (an uncontrolled gate reduces to open <=> jammed; uncontrolled
;;; gears turn all the time).
;;;
;;; REQUIRES:
;;;   nested    : -beam-substrate ((active receiver))
;;;   conditional relations:
;;;               depressed (plate), guarded by plate  --  owned by plate.lisp; translation
;;;               removes the guarded reference when the plate type is empty
;;; PROVIDES:
;;;   types     : gate, floor-gears, wall-gears, angled-gears, plate, receiver, mode, gun --
;;;               declared optional here.  The gears leaf types appear directly (not via
;;;               the gears union) because this file splices before -gears-fan installs
;;;               the union; gun likewise appears directly since gun.lisp nests this file
;;;               rather than the other way around.
;;;   relations : (controls $list (either gate floor-gears wall-gears angled-gears gun)
;;;               $mode)  --  $list = DNF OR-list of AND-lists of controllers
;;;               (receiver/plate); mode: normal | inverted
;;;   query     : energized

(include-tech -beam-substrate)

(in-package :ww)


(define-optional-types gate floor-gears wall-gears angled-gears plate receiver mode gun)


(define-static-relations
  (controls $list (either gate floor-gears wall-gears angled-gears gun) $mode))  ;$list = DNF OR-list of AND-lists of controllers (receiver/plate); mode: normal | inverted


(define-query energized (?controller (either receiver plate))
  ;; A controller drives its output when on: a receiver when active, a plate when depressed.
  (or (and (receiver ?controller)
           (active ?controller))
      (and (plate ?controller)
           (depressed ?controller))))
