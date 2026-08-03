;;; Filename: -recorder-gate-shadow.lisp

;;; Recording-side gate state.  Ghost-only controller and jammer readings derive a distinct
;;; OPEN value, exposed through -recording-shadow-policy's gate-view hook.
;;;
;;; REQUIRES:
;;;   nested : -recorder-controls-shadow; -recorder-jamming-shadow; -gate
;;;            (recording-shadow-gate-open neutral hook); -propagation
;;; PROVIDES:
;;;   relation : recording-open
;;;   query    : recording-shadow-gate-open override
;;;   update   : update-recording-gate-status!
;;;   lifecycle: reset-recording-gate-shadow!

(include-tech -recorder-controls-shadow)
(include-tech -recorder-jamming-shadow)
(include-tech -gate)
(include-tech -propagation)

(in-package :ww)


(define-dynamic-relations
  (recording-open gate))


(define-derived-relations
  recording-open)


(defun reset-recording-gate-shadow! (state)
  "Clear gate facts inherited from the preceding recording cycle."
  (clear-recorder-shadow-relation! state 'recording-open))


(define-query recording-shadow-gate-open (?gate)
  (and (gate ?gate)
       (recording-open ?gate)))


(define-update update-recording-gate-status! ()
  ;; A mapped ghost jammer supplies the same force-open override as ordinary playback; a
  ;; mapped live jammer is absent from this view.
  (doall (?gate gate)
    (if (or (recording-jammed ?gate)
            (recording-control-on ?gate nil))
      (recording-open ?gate)
      (not (recording-open ?gate)))))


(register-recorder-shadow-lifecycle
  'gate 'reset-recording-gate-shadow!)
