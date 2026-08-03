;;; Filename: -recorder-wall-gears-shadow.lisp

;;; Recording-side wall-gears state.  Ghost-only control and jamming readings derive a
;;; distinct turning value, exposed through -recording-shadow-policy's gears-view hook.
;;;
;;; REQUIRES:
;;;   nested : -recorder-controls-shadow; -recorder-jamming-shadow;
;;;            -recording-shadow-policy (recording-shadow-turning neutral hook);
;;;            -propagation
;;; PROVIDES:
;;;   relation : recording-turning
;;;   query    : recording-shadow-turning override
;;;   update   : update-recording-gears-status!
;;;   lifecycle: reset-recording-wall-gears-shadow!

(include-tech -recorder-controls-shadow)
(include-tech -recorder-jamming-shadow)
(include-tech -recording-shadow-policy)
(include-tech -propagation)

(in-package :ww)


(define-optional-types wall-gears)


(define-dynamic-relations
  (recording-turning wall-gears))


(define-derived-relations
  recording-turning)


(defun reset-recording-wall-gears-shadow! (state)
  "Clear wall-gears facts inherited from the preceding recording cycle."
  (clear-recorder-shadow-relation! state 'recording-turning))


(define-query recording-shadow-turning (?gears)
  (and (wall-gears ?gears)
       (recording-turning ?gears)))


(define-update update-recording-gears-status! ()
  ;; Uncontrolled wall gears turn; controlled wall gears evaluate their DNF against
  ;; recording-side controller state.  A mapped ghost jammer forces them stopped, while a
  ;; mapped live jammer affects only ordinary playback.  The plate-only control restriction
  ;; remains an initialization policy in -recorder-init-checks.
  (doall (?gears wall-gears)
    (if (and (recording-control-on ?gears t)
             (not (recording-jammed ?gears)))
      (recording-turning ?gears)
      (not (recording-turning ?gears)))))


(register-recorder-shadow-lifecycle
  'wall-gears 'reset-recording-wall-gears-shadow!)
