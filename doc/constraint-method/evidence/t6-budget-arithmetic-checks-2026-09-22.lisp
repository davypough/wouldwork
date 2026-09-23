(ql:quickload :wouldwork)
(in-package :ww)

(stage crelay-topo)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))

(let ((output (with-output-to-string (stream)
                (let ((*standard-output* stream))
                  (report-budget-arithmetic)))))
  (assert (search "AM1 [grade 1 -> 2; S1 controls, S2 ON pool]" output))
  (assert (search "full occupant pool is 8" output))
  (assert (search "AM2 [grade 1 -> 2; S1 controls, S2 ON pool, goal form]" output))
  (assert (search "AM3a [grade 1 -> 2; S1 controls, S2 live ON pool]" output))
  (assert (search "at least 5 support cost must close." output))
  (assert (search "AM3b [grade 1 -> 2; S1 controls, S2 full ON pool]" output))
  (assert (search "at least 1 support cost must close." output)))

(assert (null (budget-arithmetic-constraints '((gate-a 1) (gate-b 1))
                                             '(1 2)
                                             t
                                             nil)))

(format t "T6 budget arithmetic: 8 assertions passed.~%")
