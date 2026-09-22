;;; Filename: -recorder-switch-shadow.lisp

;;; Persistent recording-side switch state. Ghost switch actions modify this shadow
;;; and ordinary SWITCHED-ON state; live actions modify only ordinary state.

(include-tech -switch)
(include-tech -recorder-core)
(include-tech -propagation)

(in-package :ww)


(define-dynamic-relations
  (recording-switched-on switch))


(defun reset-recording-switch-shadow! (state)
  (clear-recorder-shadow-relation! state 'recording-switched-on)
  state)


(defun seed-recording-switch-shadow! (state)
  "Seed recording switch memory from the ordinary playback baseline."
  (let* ((idb (problem-state.idb state))
         (propositions (list-database idb)))
    (dolist (switch (gethash 'switch *types*))
      (when (member (list 'switched-on switch) propositions :test #'equal)
        (add-proposition (list 'recording-switched-on switch) idb))))
  (invalidate-problem-state-hash state))


(define-update initialize-recording-switch-state! ()
  ;; DEFINE-INIT may seed a switch on.  Mirror that baseline only during initialization;
  ;; ordinary propagation must leave the action-controlled recording memory untouched.
  (if *applying-init-action*
    (doall (?switch switch)
      (if (switched-on ?switch)
        (recording-switched-on ?switch)
        (not (recording-switched-on ?switch))))))


(register-recorder-shadow-lifecycle
  'switch 'reset-recording-switch-shadow! 'seed-recording-switch-shadow!)
