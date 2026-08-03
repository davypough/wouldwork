;;; Filename: -controls-init-checks.lisp

;;; Initialization validation for control wiring and modes.


(in-package :ww)


(define-init-check controls-init-check (literals)
  (:consumes receiver plate)
  (when (init-dnf-controls-relation-p)
    (check-init-controls-list-contents literals))
  (check-init-controls-modes literals))


(define-init-check-helper init-check-controls-clauses (literal clauses)
  (unless (listp clauses)
    (fail-init-check nil "~%CONTROLS relation must use a DNF list of controller clauses.~%~
            Literal: ~S~%~
            Clauses: ~S"
           literal clauses))
  (dolist (clause clauses)
    (unless (listp clause)
      (fail-init-check nil "~%CONTROLS relation must use a DNF list of controller clauses.~%~
              Literal: ~S~%~
              Clause:  ~S"
             literal clause))
    (init-check-list-items-have-types literal clause '(receiver plate))))


(define-init-check-helper check-init-controls-list-contents (literals)
  (dolist (literal (init-literals-with-relation 'controls literals))
    (init-check-controls-clauses
      literal
      (second (init-literal-proposition literal)))))


(define-init-check-helper init-dnf-controls-relation-p ()
  (equal (init-relation-fluent-indices 'controls) '(1 3)))


(define-init-check-helper check-init-controls-modes (literals)
  "Checks that CONTROLS uses only modes implemented by update-gate-status!."
  (when (init-dnf-controls-relation-p)
    (dolist (literal (init-literals-with-relation 'controls literals))
      (let ((mode (fourth (init-literal-proposition literal))))
        (unless (member mode '(normal inverted))
          (fail-init-check nil "~%CONTROLS uses an unsupported mode.~%~
                  Literal:          ~S~%~
                  Unsupported mode: ~S~%~
                  Supported modes:  (NORMAL INVERTED)"
                 literal mode))))))

