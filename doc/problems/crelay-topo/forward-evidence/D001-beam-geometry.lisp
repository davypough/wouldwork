;;; Query-only check after fresh CRELAY-TOPO staging and WW-SET threads 16.
;;; No search, replay, goal installation, or hypothetical state construction.
(in-package :ww)

(defun forward-d001-origin ()
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (with-open-file (stream "doc/problems/crelay-topo/test13-progress.txt")
    (let* ((*read-eval* nil)
           (*package* (find-package :ww))
           (archive (read stream))
           (origin (copy-problem-state *start-state*)))
      (assert (eq (first archive) :wouldwork-subgoal-progress))
      (assert (eq (getf (rest archive) :problem) 'crelay-topo))
      (assert (equalp (make-subgoal-progress-state-signature origin)
                      (getf (rest archive) :origin)) ()
              "Stage CRELAY-TOPO afresh and finish thread setup before D001.")
      origin)))

(defun forward-d001-rays (origin)
  (loop for (from near to far expected) in
          '((location9 1 transmitter1 1 nil)
            (location9 1 repeater1 1 t)
            (location15 1 repeater1 1 nil)
            (location15 2 repeater1 1 t)
            (location15 5/2 repeater1 1 t)
            (location15 7/2 repeater1 1 t)
            (location15 7/2 receiver1 1 t))
        for actual = (not (null (beam-visible origin from near to far)))
        collect (list :from from :near near :to to :far far
                      :expected expected :visible actual
                      :crossings (los-barrier-crossings origin from to))))

(defun forward-d001 (&optional
                       (output "doc/problems/crelay-topo/forward-evidence/D001-output.txt"))
  (let* ((origin (forward-d001-origin))
         (before (make-subgoal-progress-state-signature origin))
         (rows (forward-d001-rays origin))
         (matches (every (lambda (row)
                           (eq (getf row :expected) (getf row :visible)))
                         rows)))
    (assert (equalp before (make-subgoal-progress-state-signature origin)))
    (with-open-file (stream output :direction :output :if-exists :error
                                  :if-does-not-exist :create)
      (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
        (format t "~&D001: original staged state; threads=~D; queries only.~%" *threads*)
        (dolist (row rows) (format t "~S~%" row))
        (format t "D001-PREDICTIONS-MATCH=~S~%" matches)
        (format t "Gate4 is closed at origin: the southern source ray should fail.~%")
        (format t "These are rays at supplied heights, not assembled objects or a relay witness.~%")
        (format t "No gate8 traversal, loading, fork, or recorder validation was performed.~%")))
    (assert matches () "D001 differed from the source prediction; inspect the saved rows.")
    rows))
