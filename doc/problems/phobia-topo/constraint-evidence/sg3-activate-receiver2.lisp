;;; One approved search from accepted SG2. No restaging or automatic retry.
(in-package :ww)

(ww-set *threads* 16)
(ww-set *depth-cutoff* 25)
(ww-set *solution-type* min-length)

;;; Package the already accepted replay prefix with its original loft goal.
(defparameter *phobia-sg2-checkpoint*
  (let* ((initial (capture-search-checkpoint))
         (session (copy-goal-chain-session-deeply
                    (search-checkpoint-session initial)))
         (source (search-checkpoint-state initial))
         (endpoint (action-sequence-validation-final-state *phobia-sg2-validation*))
         (actions (append *phobia-sg1-actions* *phobia-sg2-actions*))
         (request
           (make-goal-chain-request
             :goal '(and (has-location agent1 location12)
                         (holding agent1 connector2)
                         (has-location connector1 location12)
                         (active receiver1)
                         (has-location jammer1 location12)
                         (jamming jammer1 gate2)
                         (has-location fan1 location4))
             :final-p nil
             :settings (capture-goal-chain-search-settings)))
         (solution (make-solution
                     :depth (length actions)
                     :time (problem-state.time endpoint)
                     :value (problem-state.value endpoint)
                     :path (copy-tree actions)
                     :goal (copy-problem-state endpoint))))
    (setf (goal-chain-session-phases session)
          (list (make-goal-chain-phase-from-solution request source nil solution)))
    (search-checkpoint-from-session session)))

(export-search-checkpoint
  *phobia-sg2-checkpoint*
  (merge-pathnames "doc/problems/phobia-topo/constraint-evidence/sg2-checkpoint.txt"
                   (asdf:system-source-directory :wouldwork)))

(defparameter *phobia-sg3-candidate*
  (solve-subgoal *phobia-sg2-checkpoint* (active receiver2)))

(format t "~%SG3 new checkpoint: ~S~%"
        (not (eq *phobia-sg3-candidate* *phobia-sg2-checkpoint*)))
(display-validation-state (search-checkpoint-state *phobia-sg3-candidate*))
;;; Retain SG2. Review the candidate before export or another search.
