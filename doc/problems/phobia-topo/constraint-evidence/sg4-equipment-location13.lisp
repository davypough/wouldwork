;;; Run only after the previous search and its workers have stopped.
;;; Restore accepted SG3; perform one approved equipment-transfer search.
(in-package :ww)

(stage phobia-topo)
(ww-set *threads* 16)
(defparameter *phobia-sg3-restored*
  (import-search-checkpoint
    (merge-pathnames
      "doc/problems/phobia-topo/constraint-evidence/sg3-checkpoint.txt"
      (asdf:system-source-directory :wouldwork))))
(ww-set *depth-cutoff* 25)
(ww-set *solution-type* min-length)

(defparameter *phobia-sg4-equipment-candidate*
  (solve-subgoal *phobia-sg3-restored*
    (and (has-location agent1 location13)
         (has-location fan1 location13)
         (has-location jammer1 location13)
         (jamming jammer1 wblower2)
         (active receiver2)
         (has-location connector1 location5)
         (has-location connector2 location2)
         (paired connector2 transmitter1)
         (paired connector1 connector2)
         (paired connector1 receiver2))))

(format t "~%Equipment subgoal new checkpoint: ~S~%"
        (not (eq *phobia-sg4-equipment-candidate* *phobia-sg3-restored*)))
(display-validation-state
  (search-checkpoint-state *phobia-sg4-equipment-candidate*))
;;; Review before export or any further search. No automatic retry/deepening.
