;;; Filename: -recorder-receiver-shadow.lisp

;;; Recording-side receiver state.  Beam peers already provide recording-shadow arrival
;;; hooks; this component owns the receiver relation and the propagation update consuming
;;; their combined result.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (recording object/presence policy and the single shadow-view
;;;            selector); -beam-substrate (recording-shadow-beam-reaches-receiver and the
;;;            relay-lighting hook); -propagation
;;; PROVIDES:
;;;   relation : recording-active
;;;   update   : update-recording-receiver-status!
;;;   lifecycle: reset-recording-receiver-shadow!

(include-tech -recorder-core)
(include-tech -beam-substrate)
(include-tech -propagation)

(in-package :ww)


(define-dynamic-relations
  (recording-active receiver))


(define-derived-relations
  recording-active)


(defun reset-recording-receiver-shadow! (state)
  "Clear receiver facts inherited from the preceding recording cycle."
  (clear-recorder-shadow-relation! state 'recording-active))


(define-update update-recording-receiver-status! ()
  ;; Every mapped ghost observes the same recording-side environment.  Select that view
  ;; once and compute its relay lighting once, then reuse both for every receiver.
  (do (assign $view (recording-shadow-view-object))
      (assign $lighting (compute-relay-lighting-for-object $view nil))
      (doall (?receiver receiver)
        (if (recording-shadow-beam-reaches-receiver
              $view $lighting ?receiver)
          (recording-active ?receiver)
          (not (recording-active ?receiver))))))


(register-recorder-shadow-lifecycle
  'receiver 'reset-recording-receiver-shadow!)
