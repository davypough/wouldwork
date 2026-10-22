;;; Filename: -recorder-switch-shadow.lisp

;;; Persistent recording-side switch state. Ghost switch actions modify this shadow
;;; and ordinary SWITCHED-ON state; live actions modify only ordinary state.

(include-tech -switch)
(include-tech -recorder-core)
(include-tech -propagation)

(in-package :ww)


(define-dynamic-relations
  (recording-switched-on switch))


(define-update initialize-recording-switch-state! ()
  ;; DEFINE-INIT may seed a switch on.  Mirror that baseline only during initialization;
  ;; ordinary propagation must leave the action-controlled recording memory untouched.
  (if *applying-init-action*
    (doall (?switch switch)
      (if (switched-on ?switch)
        (recording-switched-on ?switch)
        (not (recording-switched-on ?switch))))))


(register-recorder-shadow-lifecycle
  'switch
  ;; Reset: clear the recording switch memory inherited from the preceding cycle.
  '(doall (?switch switch)
     (not (recording-switched-on ?switch)))
  ;; Seed recording switch memory from the ordinary playback baseline.
  '(doall (?switch switch)
     (if (switched-on ?switch)
       (recording-switched-on ?switch))))
