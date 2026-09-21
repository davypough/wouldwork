;;; Data-only ingestion after ingest-lk5-bound-2026-09-21.lisp.
;;; No Wouldwork initialization, problem loading, replay or search.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)

(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path))
       (link (ledger-record ledger 'lk5))
       (old-bound (copy-tree (ledger-record ledger 'bd2)))
       (recommendation (copy-tree (getf link :recommendation))))
  (assert (equal '(bd2) (getf link :attempts)))
  ;; Transcribe the dated pre-run commitment; do not regenerate its reading.
  (setf (getf recommendation :cutoff) 10
        (getf recommendation :threads) 16
        (getf recommendation :date) "2026-09-21"
        (getf recommendation :deepens) 'bd2
        (getf recommendation :source)
          "constraint-evidence/lk5-cutoff10-recommendation-2026-09-21.txt"
        (getf recommendation :success)
          "SUCCESS: a found sequence realizes LK5 from this exact start. File REALIZED with validated NIL unless replay evidence exists. It establishes neither LK6 nor the original goal or any universal resource allocation. No routine phase replay."
        (getf recommendation :exhaustion)
          "EXHAUSTION: no solution found from this checkpoint at cutoff 10 under the actual settings/pruning. File a grade-3 bound with measured coverage, never a refutation. Reliable NIL means no observed direct cutoff truncation, not unpruned exhaustive coverage. The checkpoint returns unchanged; no predecessor retry or deepening.")
  (ledger-set-value link :recommendation recommendation)
  (ledger-set-value link :search-cutoff 10)
  (ledger-add-event link "2026-09-21" :amended
    "Transcribed the approved cutoff-10 pre-run recommendation; same location15 archive, threads 16; deepens BD2 without replacing it.")
  (let* ((id (ingest-ledger-result ledger 'lk5 :exhausted
               :run "constraint-evidence/lk5-cutoff10-result-2026-09-21.txt"
               :truncated t
               :pruning "GRAPH repeated-state pruning; symmetry NIL; minimum-steps pruning enabled T (runtime flag); direct cutoff truncation confirmed"
               :seconds 8.652 :date "2026-09-21"))
         (bound (ledger-record ledger id)))
    (assert (eq id 'bd3))
    (ledger-set-value bound :measured
      '(:nodes nil :seconds 8.652 :cpu-seconds 129.312500
        :bytes-consed 21972125904 :cutoff-hits 130956))
    (ledger-set-value bound :provenance
      (append (getf bound :provenance)
        '(:reported-cutoff-truncated t :reported-outcome :exhausted-no-solution
          :reported-reason :depth-cutoff-truncated
          :source-archive "constraint-evidence/t10-location15-checkpoint.txt"
          :source-sha256 "76B893835F609325098B4E6980179C0149DE9213EBCEF1C2FB3961BCF4259635")))
    (assert (equalp old-bound (ledger-record ledger 'bd2)))
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger path "2026-09-21")
    (let ((readback (read-realization-ledger path)))
      (check-ledger-well-formed readback)
      (assert (equalp old-bound (ledger-record readback 'bd2)))
      (assert (equal '(bd2 bd3) (getf (ledger-record readback 'lk5) :attempts)))
      (assert (eq :open (getf (ledger-record readback 'lk5) :status)))
      (assert (eq t (ledger-provenance-value
                     (getf (ledger-record readback id) :provenance) :cutoff-truncated)))
      (report-ledger-bound-strength (ledger-record readback id))
      (format t "~&LK5 filed as BD3; OPEN; truncation T; BD2 unchanged. Checks/readback passed.~%"))))
