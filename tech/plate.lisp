;;; Filename: plate.lisp

;;; Plate technology: plates physically depress under any movable object resting on them.
;;; Toggle plates additionally remember each clear-to-depressed transition.
;;;
;;; REQUIRES:
;;;   types  : pressure-plate, toggle-plate, and their plate union, from -plate-types
;;;   nested : -support-occupancy (support-occupant, support, on, cleartop)
;;;   special: *applying-init-action* (engine) distinguishes initial-state construction
;;;            from transitions during search
;;;   driver : the master propagate-consequences! must call update-plate-status!
;;; PROVIDES:
;;;   types    : pressure-plate, toggle-plate, plate -- from -plate-types
;;;   relations: (depressed plate)       -- current physical pressure
;;;              (latched toggle-plate)  -- remembered toggle output
;;;   update   : update-plate-status!

(include-tech -plate-types)
(include-tech -propagation)
(include-tech -support-occupancy)

(in-package :ww)


(define-dynamic-relations
  (depressed plate)
  (latched toggle-plate))


(define-derived-relations
  depressed)


(define-update update-plate-status! ()
  ;; A plate is depressed iff something rests on it.  During initial-state construction,
  ;; existing occupancy establishes that physical baseline without changing a toggle
  ;; plate's authored latch state.  Thereafter a toggle plate flips its latch only on the
  ;; physical transition from clear to depressed.  Additional occupants arriving while
  ;; it remains depressed, and occupants leaving while another remains, do not flip the
  ;; latch.  Occupancy is delegated to CLEARTOP, keeping this update independent of the
  ;; problem's support-occupant roster.
  (doall (?p plate)
    (if (cleartop ?p)
      (not (depressed ?p))
      (do (if (and (not *applying-init-action*)
                   (toggle-plate ?p)
                   (not (depressed ?p)))
            (if (latched ?p)
              (not (latched ?p))
              (latched ?p)))
          (depressed ?p)))))
