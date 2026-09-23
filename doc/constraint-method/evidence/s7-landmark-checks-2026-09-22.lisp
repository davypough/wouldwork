(ql:quickload :wouldwork)
(in-package :ww)

(stage crelay-topo)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))

(let ((output (with-output-to-string (stream)
                (let ((*standard-output* stream))
                  (report-landmark-graph-and-orderings)))))
  (assert (search "relaxation: delete relaxation; achieved landmarks persist." output))
  (assert (search "(has-location agent1 location19)  movement/query landmark" output))
  (assert (search "none: the explicit goal has no controlled-device condition." output))
  (assert (search "none: route/order extraction needs a separately stated movement relaxation."
                  output)))

(format t "S7 acceptance: 4 assertions passed.~%")
