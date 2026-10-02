(ql:quickload :wouldwork)
(in-package :ww)

(stage crelay-topo)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))

(let ((output (with-output-to-string (stream)
                (let ((*standard-output* stream))
                  (report-beam-sightline-table)))))
  (assert (search "direct gate subsets: 512; no propagation applied" output))
  (assert (search "location8 @ 1 -> transmitter1  CONDITIONAL" output))
  (assert (search "location9 @ 1 -> receiver1  NEVER" output))
  (assert (search "location9 @ 2 -> receiver1  ALWAYS" output))
  (assert (search "location15 @ 1 -> receiver1  ALWAYS" output))
  (assert (search "location15 @ 2 -> repeater1  ALWAYS" output)))

(format t "S6 acceptance: 6 assertions passed.~%")
