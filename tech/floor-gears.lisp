;;; Filename: floor-gears.lisp

;;; Public technology for floor-gears that accept a removable cargo fan.  The shared
;;; mounting machinery lives in -gears-fan; the floor-directed launch, hover, and drop
;;; behavior shared with fixed floor blowers lives in -floor-blowing.

(include-tech -floor-blowing)

(in-package :ww)
