;;; Filename: -recorder-plate-shadow.lisp

;;; Recording-side plate state.  Shared ON facts carry both playback and recording
;;; occupants; this component filters occupancy to mapped ghosts and maintains a distinct
;;; pressure/toggle reading for the recording view.
;;;
;;; REQUIRES:
;;;   nested      : -recorder-core (ghost identity); -support-occupancy (plate types, ON);
;;;                 -propagation
;;;   conditional : LATCHED from plate.lisp, guarded by optional TOGGLE-PLATE
;;; PROVIDES:
;;;   relations : recording-depressed, recording-latched
;;;   query     : recording-plate-occupied
;;;   update    : update-recording-plate-status!
;;;   lifecycle : reset-recording-plate-shadow!, seed-recording-plate-shadow!

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
    (and (ghost-recording-object ?occupant)
         (on ?occupant ?plate))))


(defun reset-recording-plate-shadow! (state)
  "Clear the recording plate edge state inherited from the preceding cycle."
  (clear-recorder-shadow-relation! state 'recording-depressed)
  (clear-recorder-shadow-relation! state 'recording-latched)
  state)


(defun seed-recording-plate-shadow! (state)
  "Seed recording plate memory from the new playback baseline.

The toggle value starts from the ordinary latch.  Depression starts from ghost-only
occupancy so the first propagation pass does not mistake an already occupied plate for a
new clear-to-depressed edge."
  (let* ((idb (problem-state.idb state))
         (propositions (list-database idb)))
    (dolist (plate (gethash 'toggle-plate *types*))
      (when (member (list 'latched plate) propositions :test #'equal)
        (add-proposition (list 'recording-latched plate) idb)))
    (dolist (plate (gethash 'plate *types*))
      (when (funcall (symbol-function 'recording-plate-occupied) state plate)
        (add-proposition (list 'recording-depressed plate) idb))))
  (setf (problem-state.idb-hash state) nil)
  state)


(define-update update-recording-plate-status! ()
  ;; The recording view contains only mapped ghost occupants.  During initialization its
  ;; toggle latch starts from the authored playback latch; afterward it changes only on a
  ;; ghost-only clear-to-depressed transition.
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
  'plate 'reset-recording-plate-shadow! 'seed-recording-plate-shadow!)
