;;; Data-only ingestion; no Wouldwork load, staging, replay, or search.
;;; Run after ingest-lk4-found-2026-09-20.lisp when rebuilding the ledger.
(unless (find-package :ww) (defpackage :ww (:use :cl)))
(load "tech/constraint-ledger.lisp")
(in-package :ww)

(let* ((path "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt")
       (ledger (read-realization-ledger path))
       (link (ledger-record ledger 'lk5))
       (recommendation (copy-tree (getf link :recommendation))))
  ;; Transcribe the already committed 2026-09-21 recommendation, not a new
  ;; post-result recommendation. The original prose remains in its evidence file.
  (assert (null (getf link :attempts)))
  (setf (getf recommendation :date) "2026-09-21"
        (getf recommendation :threads) 16
        (getf recommendation :start) "*t10-checkpoint*"
        (getf recommendation :source)
          "constraint-evidence/lk5-checkpoint-recommendation-2026-09-21.txt"
        (getf recommendation :source-archive)
          "constraint-evidence/t10-location15-checkpoint.txt"
        (getf recommendation :source-sha256)
          "76B893835F609325098B4E6980179C0149DE9213EBCEF1C2FB3961BCF4259635"
        (getf recommendation :success)
          "Success establishes a concrete LK5 realization from this exact start; inspect the result and save the returned checkpoint. File REALIZED with validated NIL unless separate replay evidence exists. It establishes no universal resource allocation and does not establish LK6 or the original final goal."
        (getf recommendation :exhaustion)
          "Exhaustion is a grade-3 bound for this exact start/settings/pruning, not a refutation. Record cutoff truncation and pruning with the result. The checkpoint object and the retained prefix remain unchanged; no automatic deepening or predecessor retry is authorized. Report the result before another search.")
  (ledger-set-value link :recommendation recommendation)
  (ledger-set-value link :search-start "*t10-checkpoint*")
  (ledger-set-value link :search-threads 16)
  (ledger-add-event link "2026-09-21" :amended
    "Transcribed the prior standalone cutoff-8 recommendation from its dated evidence file; exact source is the location15 archive, threads 16.")
  (let* ((id (ingest-ledger-result ledger 'lk5 :exhausted
               :run "constraint-evidence/lk5-cutoff8-result-2026-09-21.txt"
               :truncated :unknown
               :pruning "GRAPH repeated-state pruning; symmetry NIL; minimum-steps pruning enabled T (runtime flag only); parallel cutoff coverage instrumentation was incomplete"
               :seconds 1.839 :date "2026-09-21"))
         (bound (ledger-record ledger id)))
    (ledger-set-value bound :measured
      '(:nodes nil :seconds 1.839 :cpu-seconds 24.343750
        :bytes-consed 4234773504 :cutoff-hits 32356))
    (ledger-set-value bound :provenance
      (append (getf bound :provenance)
        '(:reported-cutoff-truncated nil :reported-outcome :exhausted-no-solution
          :reported-reason :complete
          :coverage-audit "Parallel workers counted cutoff hits but did not measure truncation. Actual coverage unknown; raw COMPLETE/NIL is not a completeness certificate.")))
    (check-ledger-well-formed ledger)
    (write-realization-ledger ledger path "2026-09-21")
    (let ((readback (read-realization-ledger path)))
      (assert (eq :open (getf (ledger-record readback 'lk5) :status)))
      (assert (eq :unknown
                (ledger-provenance-value
                  (getf (ledger-record readback id) :provenance) :cutoff-truncated)))
      (check-ledger-well-formed readback)
      (report-ledger-bound-strength (ledger-record readback id))
      (format t "~&LK5 filed as ~A; OPEN; UNKNOWN coverage. Checks/readback passed.~%" id))))
