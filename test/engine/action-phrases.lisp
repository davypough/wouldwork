;;; Load after Wouldwork. No search; tests run only when explicitly called.
(in-package :ww)

(defun phrase-test-precondition (state)
  (declare (ignore state))
  t)

(defun phrase-test-effect (state)
  (declare (ignore state))
  (list (make-update :instantiations '(agent1 tray1 location21 location21))))

(defun phrase-test-action ()
  (make-action
    :name 'pickup-tray
    :effect-variables '(?agent ?tray $tray-location $a-location)
    :effect-format '(">" ?agent "picks up" ?tray "at" $tray-location "from" $a-location)
    :pre-defun-name 'phrase-test-precondition
    :eff-defun-name 'phrase-test-effect
    :precondition-args '(nil)))

(defun phrase-test-rejects-p (action arguments)
  (handler-case
      (progn (strip-display-connectives action arguments) nil)
    (invalid-action-phrase () t)))

(defun test-action-phrases ()
  "Check phrase round trips, nested/string values, diagnostics, and replay lookahead."
  (let* ((*package* (find-package :ww))
         (action (phrase-test-action))
         (*actions* (list action))
         (plain '(pickup-tray agent1 tray1 location21 location21))
         (expected "(PICKUP-TRAY > AGENT1 picks up TRAY1 at LOCATION21 from LOCATION21)")
         (annotated (read-from-string expected)))
    (assert (string= expected (format-action-for-display plain)))
    (assert (string= expected (format-action-for-display annotated)))
    (assert (equal (rest plain) (strip-display-connectives action (rest annotated))))
    (assert (equal (rest plain) (strip-display-connectives action (rest plain))))
    (assert (equal (list 53 annotated)
                   (read-from-string (format-action-entry-for-display (list 53 plain)))))
    ;; Real data are escaped; phrase words alone are emitted without quotes.
    (dolist (arguments '((agent1 "tray with spaces" nil ((walk location1 nil location2)))
                         (agent1 tray1 (repeater1 receiver1) |Location Mixed Case|)))
      (let ((read-back (read-from-string
                        (format-action-for-display (cons 'pickup-tray arguments)))))
        (assert (equal arguments (strip-display-connectives action (rest read-back))))))
    (dolist (bad '((> agent1 drops up tray1 at location21 from location21)
                  (> agent1 picks tray1 at location21 from location21)
                  (> agent1 picks up tray1 at location21 from)
                  (> agent1 picks up tray1 at location21 from location21 extra)
                  (> agent1 picks up)
                  (">" agent1 "picks up" tray1 "at" location21 "from" location21)))
      (assert (phrase-test-rejects-p action bad)))
    ;; Bad user input remains visible in diagnostics rather than crashing the printer.
    (let ((bad '(pickup-tray > agent1 drops up tray1 at location21 from location21)))
      (assert (equal bad (read-from-string (format-action-for-display bad))))
      (multiple-value-bind (state success reason)
          (apply-action-to-state bad *start-state* nil)
        (declare (ignore state))
        (assert (not success))
        (assert (search "Malformed action phrase" reason))))
    (assert (next-action-valid-p *start-state* 'pickup-tray (rest plain)))
    (assert (next-action-valid-p *start-state* 'pickup-tray (rest annotated)))
    (assert (not (next-action-valid-p *start-state* 'pickup-tray
                                    '(> agent1 drops up tray1 at location21 from location21))))
    (let ((text (with-output-to-string (*standard-output*)
                  (report-validation-failure 1 plain "test diagnostic" *start-state*))))
      (assert (search expected text)))
    ;; Synthetic report markers and actions without templates retain readable forms.
    (dolist (form '((pause) (resume) (wait 2.5) (start-state)))
      (assert (equal form (read-from-string (format-action-for-display form)))))
    (let ((*actions* (list (make-action :name 'plain-action :effect-variables '(?x)
                                      :effect-format '(?x)))))
      (assert (string= "(PLAIN-ACTION X)" (format-action-for-display '(plain-action x))))))
  (format t "~&ACTION-PHRASE-TESTS: PASS~%")
  t)
