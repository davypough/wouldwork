;;; Passive checks for the supplied BOXES-ADVISOR all-open crossing bound.
;;; After staging, load spec-analysis-index-checks.lisp, then this file.
;;; Loading defines functions only. Call (check-boxes-bound-model) explicitly.

(in-package :ww)

(defun boxes-check-bound-value (state expected context)
  (let ((before (boxes-check-signature state))
        (bound (min-steps-remaining? state)))
    (boxes-check-require (eql bound expected)
                         "~S: expected remaining-crossing bound ~D, got ~S."
                         context expected bound)
    (boxes-check-require (equal before (boxes-check-signature state))
                         "~S: evaluating the bound changed the state." context)
    bound))

(defun boxes-check-bound-values ()
  (dolist (entry '((area1 2) (area2 3) (area3 1) (area4 0)))
    (destructuring-bind (area expected) entry
      (multiple-value-bind (value present-p)
          (gethash (convert-to-integer-memoized (list 'area-crossing-bound area))
                   *static-idb*)
        (boxes-check-require (and present-p (equal value (list expected)))
                             "Missing or incorrect static bound for area ~S: ~S."
                             area value))
      (let ((state (boxes-check-fixture-state (list area '(1 1 0 0) nil nil))))
        (boxes-check-bound-value state expected area)
        (when (eq area 'area4)
          (boxes-check-require (goal-fn state) "AREA4 fixture does not satisfy goal.")))))
  (dolist (fixture (boxes-check-fixtures))
    (boxes-check-bound-value (boxes-check-fixture-state (second fixture))
                             2 (first fixture)))
  (boxes-check-bound-value *start-state* 2 'start-state))

(defun boxes-check-pruning-boundary (entry start goal)
  (destructuring-bind (context state-name cutoff incumbent depth expected evaluations) entry
    (setf *solution-paths* (when incumbent (list (make-solution :depth incumbent :goal goal))))
    (let* ((*depth-cutoff* cutoff)
           (*min-steps-fallback-mode* :eager)
           (state (ecase state-name (start start) (goal goal)))
           (before-evaluations *min-steps-fallback-evaluations*)
           (before-prunes *min-steps-fallback-unique-prunes*)
           (actual (not (null (min-steps-remaining-prunes-node-p state depth)))))
      (boxes-check-require (eql actual expected)
                           "~S: expected prune ~S, got ~S." context expected actual)
      (boxes-check-require (= (- *min-steps-fallback-evaluations* before-evaluations)
                              evaluations)
                           "~S: expected ~D actual hook evaluations." context evaluations)
      (boxes-check-require (= (- *min-steps-fallback-unique-prunes* before-prunes)
                              (if expected 1 0))
                           "~S: wrong hook prune count." context))))

(defun boxes-check-pruning-boundaries ()
  (let ((goal (boxes-check-fixture-state '(area4 (1 1 0 0) nil nil)))
        (saved-solutions *solution-paths*)
        (saved-evaluations *min-steps-fallback-evaluations*)
        (saved-prunes *min-steps-fallback-unique-prunes*))
    ;; The solution list and two counters are SBCL DEFGLOBALs: restore, not bind.
    ;; Test cutoffs and adaptation modes are ordinary local specials.
    (unwind-protect
         (dolist (entry '((no-limit start 0 nil 8 nil 0)
                          (at-cutoff start 2 nil 0 nil 1)
                          (beyond-cutoff start 2 nil 1 t 1)
                          (goal-at-cutoff goal 2 nil 2 nil 1)
                          (can-improve-incumbent start 0 10 7 nil 1)
                          (ties-incumbent start 0 10 8 t 1)))
           (boxes-check-pruning-boundary entry *start-state* goal))
      (setf *solution-paths* saved-solutions
            *min-steps-fallback-evaluations* saved-evaluations
            *min-steps-fallback-unique-prunes* saved-prunes))))

(defun check-boxes-bound-model ()
  (boxes-check-require (eq *solution-type* 'min-length)
                       "Bound checks require MIN-LENGTH, found ~S." *solution-type*)
  (boxes-check-require *min-steps-pruning-enabled*
                       "MIN-STEPS pruning is disabled in the current REPL.")
  (boxes-check-require (null *min-steps-remaining-contributors*)
                       "Standalone boxes model unexpectedly has registered contributors.")
  (check-boxes-indexed-model)
  (let ((initial-before (boxes-check-signature *start-state*)))
    (boxes-check-bound-values)
    (boxes-check-pruning-boundaries)
    (boxes-check-require (equal initial-before (boxes-check-signature *start-state*))
                         "Bound checks changed the staged start state.")
    (format t "~&BOXES-BOUND-CHECKS-PASSED: bounds (2 3 1 0), 4 handling fixtures, 6 engine boundary cases (5 evaluations, 2 prunes).~%")
    t))
