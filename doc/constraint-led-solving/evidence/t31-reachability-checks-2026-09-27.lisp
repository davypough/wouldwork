;;; T31 acceptance: synthetic checks before staging; no solve or replay.
(in-package :ww)
(defparameter *t31-checks* 0)
(defun t31-check (label value)
  (incf *t31-checks*)
  (unless value (error "T31 failed: ~A" label)))

(defun t31-oracle (start rows regions excluded)
  "Independent transitive-closure oracle; does not call the production walk."
  (let ((paths (make-hash-table :test #'equal)))
    (dolist (region regions) (setf (gethash (list region region) paths) t))
    (dolist (row rows)
      (when (or (null excluded) (null (fourth row))
                (some (lambda (clause) (not (member excluded clause))) (fourth row)))
        (setf (gethash (subseq row 0 2) paths) t)
        (when (eq (fifth row) :both)
          (setf (gethash (list (second row) (first row)) paths) t))))
    (dolist (via regions)
      (dolist (from regions)
        (dolist (to regions)
          (when (and (gethash (list from via) paths) (gethash (list via to) paths))
            (setf (gethash (list from to) paths) t)))))
    (sort (remove-if-not (lambda (to) (gethash (list start to) paths))
                         (copy-list regions)) #'string<)))

(defun t31-graph-check (rows regions devices)
  (multiple-value-bind (reduced reason) (keeper-spine rows regions devices)
    (t31-check "supported graph" (null reason))
    (t31-check "deterministic reversed input"
               (equal reduced (quotient-reduced-rows (reverse rows))))
    (t31-check "S3/S4 retained membership"
               (null (set-exclusive-or reduced
                       (mapcar #'first (remove-if-not
                         (lambda (entry) (eq (second entry) :spine))
                         (quotient-classified-rows rows))) :test #'equal)))
    (dolist (device (cons nil devices))
      (dolist (region regions)
        (let ((expected (t31-oracle region rows regions device)))
          (t31-check "oracle full/reduced equality"
                     (equal expected (t31-oracle region reduced regions device)))
          (t31-check "production walk agrees with oracle"
                     (equal expected (keeper-reachable region reduced device))))))
    reduced))

(defun t31-synthetic-checks ()
  (let* ((directed '(("A" "B" walk ((d1)) :forward 1)
                     ("A" "C" walk ((d1)) :forward 1)
                     ("B" "C" walk nil :both 1)))
         (both '(("A" "B" walk ((d1)) :both 1)
                 ("A" "C" walk ((d1)) :both 1)
                 ("B" "C" walk nil :both 1)))
         (regions '("A" "B" "C")))
    (dolist (rows (list directed both))
      (t31-check "fixture reproduces independent deletion defect"
        (not (equal (t31-oracle "A" rows regions nil)
                    (t31-oracle "A" (remove-if-not
                       (lambda (row) (eq :spine (first (quotient-row-composition row rows))))
                       rows) regions nil))))
      (t31-check "mutual redundancy reduced safely"
                 (< (length (t31-graph-check rows regions '(d1 d2))) (length rows))))
    (let ((rows '(("A" "B" walk ((d1)) :both 1)
                  ("A" "C" walk nil :forward 1)
                  ("C" "B" walk nil :forward 1))))
      (t31-check "reverse direction cannot be discarded"
                 (member (first rows) (t31-graph-check rows regions '(d1)) :test #'equal)))
    (let ((rows '(("A" "B" walk ((d1)) :forward 1)
                  ("A" "C" walk nil :forward 1)
                  ("C" "B" walk nil :forward 1))))
      (t31-check "door-free replacement recognized"
                 (equal '(:non-minimal nil) (quotient-row-composition (first rows) rows)))
      (t31-check "door-free replacement retained"
                 (= 2 (length (t31-graph-check rows regions '(d1))))))
    ;; All 64 directed topologies, with three independent door assignments.
    (dolist (style '(0 1 2))
      (dotimes (mask 64)
        (let ((rows nil) (bit 0))
          (dolist (from regions)
            (dolist (to regions)
              (unless (equal from to)
                (when (logbitp bit mask)
                  (push (list from to 'walk
                          (case (mod (+ bit style) 3) (0 nil) (1 '((d1))) (2 '((d2))))
                          :forward 1) rows))
                (incf bit))))
          (t31-graph-check rows regions '(d1 d2)))))
    (let ((rows '(("A" "B" walk ((d1) (d2)) :both 1))))
      (multiple-value-bind (spine reason) (keeper-spine rows regions '(d1 d2))
        (t31-check "unsupported family explicit" (and (null spine) (eq reason :alternative-families))))
      (t31-check "unsupported rows preserved" (equal rows (quotient-reduced-rows rows))))
    (dolist (reason '(:alternative-families (:reachability-mismatch "A" d1)))
      (let ((output (with-output-to-string (*standard-output*)
                      (report-necessity-hint-families (make-list 7)
                                                     (list :spine-reason reason)))))
        (t31-check "H2 unavailable" (search "UNAVAILABLE: S4" output :start2 (search "H2 " output) :end2 (search "H3 " output)))
        (t31-check "H4 unavailable" (search "UNAVAILABLE: S4" output :start2 (search "H4 " output) :end2 (search "H5 " output)))
        (t31-check "reason preserved" (search (format nil "~S" reason) output))))
    (let ((output (with-output-to-string (*standard-output*)
                    (report-necessity-hint-families (make-list 7) '(:spine nil :spine-reason nil)))))
      (t31-check "valid empty is not unavailable" (not (search "UNAVAILABLE" output))))
    (format t "~&T31 synthetic checks passed: ~D~%" *t31-checks*)))

(defun t31-forced-spine (rows regions devices)
  (declare (ignore rows regions devices))
  (values nil :alternative-families))

(defun t31-staged-checks ()
  (let* ((controls (control-facts))
         (context (hint-route-context controls))
         (rows (quotient-arc-rows (getf context :arcs) (getf context :names)))
         (regions (mapcar #'first (getf context :blocks))))
    (t31-check "windtunnel S4 available" (null (getf context :spine-reason)))
    (t31-graph-check rows regions (remove-duplicates
                                  (append (mapcar #'third controls) (mapcan #'quotient-row-doors rows))))
    (t31-check "R1 outgoing reachability restored"
               (> (length (keeper-reachable "R1" (getf context :spine) nil)) 1))
    (let ((original (symbol-function 'keeper-spine)))
      (unwind-protect
          (progn
            (setf (symbol-function 'keeper-spine) #'t31-forced-spine)
            (t31-check "context retains upstream failure"
                       (eq :alternative-families (getf (hint-route-context controls) :spine-reason)))
            (let ((output (with-output-to-string (*standard-output*) (report-necessity-hints))))
              (t31-check "end-to-end NH labels failure" (search "UNAVAILABLE: S4 :ALTERNATIVE-FAMILIES" output))))
        (setf (symbol-function 'keeper-spine) original)))
    (format t "~&T31 staged checks passed; total checks: ~D; full rows: ~D; retained: ~D~%"
            *t31-checks* (length rows) (length (getf context :spine)))
    (report-cut-keeper-table)))

(t31-synthetic-checks)
