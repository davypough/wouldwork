;;; Retained reproduction evidence. One approved B2 search, no automatic follow-up.
(asdf:initialize-output-translations
  '(:output-translations
    (#p"D:/quicklisp/local-projects/wouldwork/**/*.*"
     #p"D:/quicklisp/local-projects/wouldwork/.b2-cache/**/*.*")
    :inherit-configuration))
(ql:quickload :wouldwork)
(in-package :ww)
(stage crelay-topo)
(ww-set *threads* 16)
(defparameter *t10-b1-checkpoint*
  (import-search-checkpoint
    #p"doc/problems/crelay-topo/constraint-evidence/t10-b1-checkpoint.txt"))
(ww-set *depth-cutoff* 12)
(ww-set *tree-or-graph* graph)
(assert (null *symmetry-pruning*))
(setf *min-steps-pruning-enabled* t)
(defparameter *t10-b2-checkpoint* nil)
(multiple-value-bind (checkpoint found-p)
    (solve-search-checkpoint
      *t10-b1-checkpoint*
      '(and (recording-in-progress)
            (on tray1 plate1) (on box1 plate2)
            (on tray1* plate1) (on box1* plate2)
            (has-location agent1 location1)
            (has-location agent1* location1)))
  (setf *t10-b2-checkpoint* checkpoint)
  ;; Persist first so later reporting cannot lose a successful result.
  (when found-p
    (export-search-checkpoint checkpoint
      #p"doc/problems/crelay-topo/constraint-evidence/t10-b2-checkpoint.txt"))
  (format t "~&B2-FOUND-P ~S~%B2-CHECKPOINT ~S~%" found-p checkpoint)
  (format t "B2-OUTCOME ~S~%B2-TRUNCATION ~S~%B2-CUTOFF-HITS ~S~%"
          *last-search-outcome* *depth-cutoff-truncated* *depth-cutoff-hits*)
  (format t "B2-SETTINGS ~S~%"
          (list :threads *threads* :cutoff *depth-cutoff*
                :graph *tree-or-graph* :symmetry *symmetry-pruning*
                :minimum-steps *min-steps-pruning-enabled*))
  (format t "B2-ENDPOINT ~S~%"
          (database (search-checkpoint-state checkpoint))))
