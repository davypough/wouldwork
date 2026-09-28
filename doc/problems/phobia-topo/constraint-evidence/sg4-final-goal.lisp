;;; One approved final-goal search from accepted SG3. No restaging or retry.
(in-package :ww)

(ww-set *threads* 16)
(ww-set *depth-cutoff* 25)
(ww-set *solution-type* min-length)

(defparameter *phobia-final-candidate*
  (solve-subgoal *phobia-sg3-candidate*
                (has-location agent1 location11)))

(format t "~%Final-goal new checkpoint: ~S~%"
        (not (eq *phobia-final-candidate* *phobia-sg3-candidate*)))
(display-validation-state (search-checkpoint-state *phobia-final-candidate*))
;;; Review before export. Full-path goal validation remains required.
