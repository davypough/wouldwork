;;; T26 A3 -- the memory pilot for the T10 final search, 2026-09-26.
;;; Expected readings: t26-memory-estimate-2026-09-26.txt, part 1.2 (written before the run).
;;; Start: the 80-action checkpoint t10-c3-location15-checkpoint.txt; goal and settings as
;;; run-t10-final-search-2026-09-25.lisp (threads 16, graph, symmetry NIL, minimum-steps
;;; pruning T, solution type as staged); pilot depth 9, pilot states limit 20,000,000.
;;; Writes t26-pilot-results-2026-09-26.lisp and prints ME at the T10 heap, 16,000 MiB.
;;; Run in a fresh image after (progn (ql:quickload :wouldwork) (in-package :ww)):
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t26-run-pilot-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(load (merge-pathnames "tech/constraint-memory-estimate.lisp" (asdf:system-source-directory :wouldwork)))


(defparameter *t26-pilot-path*
  (merge-pathnames "doc/constraint-method/evidence/t26-pilot-results-2026-09-26.lisp"
                   (asdf:system-source-directory :wouldwork)))


(defparameter *t26-t10-ceiling* (* 16000 1048576)
  "The heap of the T10 runs, 16,000 MiB, in bytes.")


(defun run-t26-pilot ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (let ((checkpoint (import-search-checkpoint
                      (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-c3-location15-checkpoint.txt"
                                       (asdf:system-source-directory :wouldwork)))))
    (ww-set *tree-or-graph* graph)
    (assert (null *symmetry-pruning*))
    (setf *min-steps-pruning-enabled* t)
    (format t "~&T26 PILOT SETTINGS ~S~%"
            (list :threads *threads* :graph *tree-or-graph* :symmetry *symmetry-pruning*
                  :minimum-steps *min-steps-pruning-enabled* :solution-type *solution-type*
                  :heap (sb-ext:dynamic-space-size)))
    (run-memory-pilot checkpoint '(has-location agent1 location19) 9 20000000 *t26-pilot-path*)
    (report-memory-estimate *t26-pilot-path* '(7 8 9 10 11 12) *t26-t10-ceiling*)))


(run-t26-pilot)
