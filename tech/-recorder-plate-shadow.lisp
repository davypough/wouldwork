;;; Filename: -recorder-plate-shadow.lisp

;;; Recording-side plate state.  Shared ON facts carry both playback and recording
;;; occupants.  An open cycle reads mapped ghosts; a closed cycle reads their live
;;; counterparts, which seed the stable recording pressure/toggle state for the next
;;; start.  The resulting shadow remains distinct from ordinary plate state.
;;;
;;; REQUIRES:
;;;   nested      : -recorder-core (ghost identity); -support-occupancy (plate types, ON);
;;;                 -propagation
;;;   conditional : LATCHED from plate.lisp, guarded by optional TOGGLE-PLATE
;;; PROVIDES:
;;;   relations : recording-depressed, recording-latched
;;;   query     : recording-plate-occupied
;;;   update    : update-recording-plate-status!
;;;   lifecycle : reset and seed clauses for recording-depressed and recording-latched

(include-tech -recorder-core)
(include-tech -support-occupancy)
(include-tech -propagation)

(in-package :ww)


(define-dynamic-relations
  (recording-depressed plate)
  (recording-latched toggle-plate))


(define-derived-relations
  recording-depressed
  recording-latched)


(define-query recording-plate-occupied (?plate plate)
  (exists (?occupant support-occupant)
    (and (on ?occupant ?plate)
         (or (and (recorder-cycle-closed)
                  (live-recording-object ?occupant))
             (and (not (recorder-cycle-closed))
                  (ghost-recording-object ?occupant))))))


(define-update update-recording-plate-status! ()
  ;; An open or legacy view contains mapped ghost occupants.  An explicit stopped boundary
  ;; reads live playback occupants so its seeded edge memory remains stable under the same
  ;; propagation update.
  (doall (?plate plate)
    (do (if (and *applying-init-action*
                 (toggle-plate ?plate))
          (if (latched ?plate)
            (recording-latched ?plate)
            (not (recording-latched ?plate))))
        (if (recording-plate-occupied ?plate)
          (do (if (and (not *applying-init-action*)
                       (toggle-plate ?plate)
                       (not (recording-depressed ?plate)))
                (if (recording-latched ?plate)
                  (not (recording-latched ?plate))
                  (recording-latched ?plate)))
              (recording-depressed ?plate))
          (not (recording-depressed ?plate))))))


(register-recorder-shadow-lifecycle
  'plate
  ;; Reset: clear the recording plate edge state inherited from the preceding cycle.
  '(do (doall (?plate plate)
         (not (recording-depressed ?plate)))
       (doall (?plate toggle-plate)
         (not (recording-latched ?plate))))
  ;; Seed recording plate memory from the new playback baseline.  The toggle value starts
  ;; from the ordinary latch.  Depression starts from the active view: freshly forked
  ;; ghosts for an open cycle, or live playback state after a stop.  The first propagation
  ;; pass therefore cannot mistake an already occupied plate for a new edge.
  '(do (doall (?plate toggle-plate)
         (if (latched ?plate)
           (recording-latched ?plate)))
       (doall (?plate plate)
         (if (recording-plate-occupied ?plate)
           (recording-depressed ?plate)))))
