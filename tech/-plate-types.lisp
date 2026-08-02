;;; Filename: -plate-types.lisp

;;; Plate type substrate.  A problem declares concrete plate instances by physical
;;; behavior, while every consuming technology programs against the shared PLATE union.
;;; Keeping the union in this early nested role makes it available before support,
;;; position, control, and action schemas are translated, independent of public
;;; technology include order.
;;;
;;; PROVIDES:
;;;   types : pressure-plate, toggle-plate -- optional concrete leaf kinds
;;;           plate (either pressure-plate toggle-plate) -- shared plate role

(in-package :ww)


(define-optional-types pressure-plate toggle-plate)


(define-types
  plate (either pressure-plate toggle-plate))
