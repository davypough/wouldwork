;;; Loaded only by sat-pilot.py in its temporary checkout.
(in-package :ww)

(defun sat-pilot-time-ms (start)
  (* 1000d0 (/ (- (get-internal-real-time) start)
               internal-time-units-per-second)))

(defun sat-pilot-stage (name)
  (let ((start (get-internal-real-time)))
    (assert (uiop:pathname-equal (asdf:system-source-directory :wouldwork)
                                 (uiop:getcwd)))
    (assert (%stage name))
    (format t "~&PILOT-STAGE ~A ~,3F~%" name (sat-pilot-time-ms start))))

(defun sat-pilot-check (bound expected actions)
  ;; Worker threads must see the same settings as the coordinator.
  (setf *depth-cutoff* bound *solution-type* 'first *time-limit* 60)
  (let* ((start (get-internal-real-time))
         ;; Wouldwork uses a zero cutoff for unlimited search.
         (found (if (zerop bound)
                    (goal *start-state*)
                    (progn (solve)
                           (assert (or *time-limit-reached*
                                       (member (search-outcome-status *last-search-outcome*)
                                           '(:solution :exhausted-with-solutions
                                             :exhausted-no-solution))))
                           (not (null *solution-paths*)))))
         (elapsed (sat-pilot-time-ms start))
         (timeout (and (plusp bound) *time-limit-reached* (not found))))
    (assert (or timeout (eql (not (null found)) expected)) ()
            "Wouldwork disagrees at bound ~D for ~A" bound *problem-name*)
    (when (and found (plusp bound))
      (assert (<= (solution.depth (first *solution-paths*)) bound)))
    (when (member *problem-name* '(sequence-base sequence-tuned pilot-five five-symmetry five-bound five-both))
      (format t "~&PILOT-FEATURES ~A H=~D symmetry=~A families=~D bound-evaluations=~D bound-prunes=~D~%"
              *problem-name* bound (if (use-canonical-symmetry-p) :active :inactive)
              (length *symmetry-families*) *min-steps-contributor-evaluations* *lower-bound-pruned*))
    (when actions
      (let ((final-state (%validate-solution actions)))
        (assert (and (<= (length actions) bound) final-state (goal final-state))
                () "SAT plan replay failed: ~S" actions)))
    (format t "~&PILOT-RUN ~D ~A ~,3F ~D~%" bound (cond (found 'sat) (timeout 'timeout) (t 'unsat)) elapsed
            (if (zerop bound) 1 *total-states-processed*))))
