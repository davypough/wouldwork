;;; CP cycle-plan check -- acceptance checks, T24, 2026-09-26.
;;; Expected readings: t24-cycle-plan-check-2026-09-26.txt, part 1 (written before the run).
;;; Run on a fresh staging, with tech/constraint-profile.lisp loaded:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t24-cycle-plan-check-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; The plan is read from part 1.3 of the evidence file, so the checks run on the plan as
;;; recorded.  No search runs.  A failed check is reported and counted, and the run
;;; continues; the last line gives the totals.

(in-package :ww)


(defparameter *cp-passed* 0)


(defparameter *cp-failed* nil)


(defparameter *cp-evidence* "doc/constraint-method/evidence/t24-cycle-plan-check-2026-09-26.txt")


(defparameter *cp-header* "CP  CYCLE-PLAN CHECK  [grade per row]")


(defun cp-check (label passed)
  (if passed
    (progn (incf *cp-passed*)
           (format t "~&  pass  ~A~%" label))
    (progn (push label *cp-failed*)
           (format t "~&  FAIL  ~A~%" label))))


(defun cp-file-lines (relative)
  (with-open-file (stream (merge-pathnames relative (asdf:system-source-directory :wouldwork)))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim '(#\Return) line))))


(defun cp-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun cp-output-lines (function)
  (with-input-from-string (stream (with-output-to-string (*standard-output*) (funcall function)))
    (loop for line = (read-line stream nil) while line collect line)))


(defun cp-plan ()
  "Part 1.3's plan, read as data in :WW."
  (let* ((text (format nil "~{~A~%~}" (cp-file-lines *cp-evidence*)))
         (start (search "(:name \"crelay-topo" text))
         (*package* (find-package :ww)))
    (read-from-string text t nil :start start)))


(defun cp-segment (id)
  "A fresh copy of the plan's segment ID."
  (dolist (stage (getf (cp-plan) :stages))
    (dolist (segment (getf stage :segments))
      (when (string= id (getf segment :id))
        (return-from cp-segment (copy-list segment))))))


(defun cp-one-stage-plan (segments)
  (list :name "check" :provenance "T24 checks" :stages
        (list (list :id "s" :intent "check" :segments segments))))


(defun cp-report-lines (plan)
  (member *cp-header* (cp-output-lines (lambda () (report-cycle-plan-check plan))) :test #'string=))


(defun cp-expected-block ()
  "Part 1.5's expected block: from the header to the plan line."
  (loop for line in (member *cp-header* (cp-file-lines *cp-evidence*) :test #'string=)
        collect line
        until (member line '("  plan PASS" "  plan CONDITIONAL" "  plan CONFLICT") :test #'string=)))


(defun cp-check-match ()
  "A3: the generated CP block equals part 1.5 line for line."
  (let ((expected (cp-expected-block))
        (generated (cp-trim-blank-tail (cp-report-lines (cp-plan)))))
    (cp-check (format nil "A3: expected block read (~D lines)" (length expected)) (> (length expected) 90))
    (loop for n from 1 to (max (length expected) (length generated))
          for e = (nth (1- n) expected)
          for g = (nth (1- n) generated)
          unless (equal e g)
            do (format t "~&  line ~D~%    expected:  ~A~%    generated: ~A~%" n e g))
    (cp-check (format nil "A3: generated block (~D lines) equals expected (~D lines)"
                      (length generated) (length expected))
              (equal expected generated))))


(defun cp-beam-plates (device facts)
  "The plates of the gates every usable RC chain to DEVICE's receiver needs, less the
 devices that receiver drives, derived here from RC's chains and S4's plate helper."
  (let* ((fact (find device facts :key #'third))
         (receiver (find-if (lambda (primitive) (member primitive (census-type-instances 'receiver)))
                            (first (second fact))))
         (chains (when receiver (rest (first (hint-relay-chains (list receiver) facts)))))
         (usable (remove-if-not (lambda (chain) (member (getf chain :class) '(:bootstrap :latch))) chains))
         (gates (when usable
                  (set-difference (reduce #'intersection (mapcar (lambda (chain) (getf chain :gates)) usable))
                                  (relay-chain-receiver-devices receiver facts)))))
    (loop for gate in gates
          append (copy-list (keeper-mandatory-plates
                              (keeper-pressure-clauses (find gate facts :key #'third)
                                                       (census-type-instances 'pressure-plate)))))))


(defun cp-required-plates (segment facts)
  "SEGMENT's required plates, derived independently of CP's literal reader."
  (keeper-sorted-set
    (loop for device in (getf segment :require)
          for fact = (find device facts :key #'third)
          append (copy-list (keeper-mandatory-plates
                              (keeper-pressure-clauses fact (census-type-instances 'pressure-plate))))
          when (eq (getf segment :view) :physical)
            append (cp-beam-plates device facts))))


(defun cp-segment-lines (lines id)
  "The row lines of segment ID in a CP report."
  (let ((tail (rest (member-if (lambda (line) (search (format nil "    segment ~A  " id) line)) lines))))
    (loop for line in tail
          until (or (search "    segment " line) (search "  stage " line) (search "  plan " line))
          collect line)))


(defun cp-check-plate-rows ()
  "A4: every segment yields exactly one B1 row per required plate, and none for any other plate."
  (let* ((plan (cp-plan))
         (lines (cp-report-lines plan))
         (facts (control-facts))
         (plates (census-type-instances 'pressure-plate))
         (segments 0)
         (bad nil))
    (dolist (stage (getf plan :stages))
      (dolist (segment (getf stage :segments))
        (incf segments)
        (let* ((rows (remove-if-not (lambda (line) (search "      B1  " line)) (cp-segment-lines lines (getf segment :id))))
               (required (cp-required-plates segment facts)))
          (unless (and (every (lambda (plate)
                                (= 1 (count-if (lambda (row) (search (format nil "]  ~(~A~) for " plate) row)) rows)))
                              required)
                       (notany (lambda (plate)
                                 (and (not (member plate required))
                                      (some (lambda (row) (search (format nil "]  ~(~A~) for " plate) row)) rows)))
                               plates))
            (push (getf segment :id) bad)))))
    (format t "~&  segments with a B1 mismatch: ~{~A~^ ~}~%" (reverse bad))
    (cp-check (format nil "A4: each of ~D segments has one B1 row per required plate" segments)
              (and (= segments 18) (null bad)))))


(defun cp-check-ro-scenario ()
  "A4: RO's 2026-09-20 scenario as a one-segment plan: three B1 rows matched, PASS; RO's
 helpers give PERFECT, forced members box1 connector1 tray1, no forced pairing."
  (let* ((segment (list :id "ro" :view :physical :cycle :none :ghosts :absent
                        :provenance "RO 2026-09-20" :available-witnesses '(box1 connector1 tray1)
                        :require '(gate9)))
         (lines (cp-report-lines (cp-one-stage-plan (list segment))))
         (plates '(plate6 plate7 plate8))
         (eligibility (mapcar (lambda (plate) (list plate 'box1 'connector1 'tray1)) plates)))
    (cp-check "A4 RO: three B1 rows matched PASS"
              (every (lambda (plate)
                       (member (format nil "      B1  PASS  [grade 1 -> 2; S1 S2 RO]  ~(~A~) for gate9: matched" plate)
                               lines :test #'string=))
                     plates))
    (cp-check "A4 RO: segment PASS" (member "  plan PASS" lines :test #'string=))
    (cp-check "A4 RO: matching PERFECT" (role-perfect-p plates eligibility))
    (cp-check "A4 RO: forced members box1 connector1 tray1"
              (equal (keeper-sorted-set (role-forced-witnesses plates eligibility)) '(box1 connector1 tray1)))
    (cp-check "A4 RO: no forced pairing" (null (role-forced-pairings plates eligibility)))))


(defun cp-has-line (lines text)
  (some (lambda (line) (search text line)) lines))


(defun cp-check-negatives ()
  "A5: N1-N4 of part 1.6, on plan copies."
  (let* ((lit (cp-segment "c3.lit")))
    (setf (getf lit :held) nil
          (getf lit :off-plate) (append (getf lit :off-plate) '(agent1*)))
    (let ((lines (cp-report-lines (cp-one-stage-plan (list lit)))))
      (cp-check "A5 N1: plate3 (beam, gate8) shortage CONFLICT"
                (cp-has-line lines "B1  CONFLICT  [grade 1 -> 2; S1 S2 RO]  plate3 for gate4 (beam, gate8): matched; shortage, violator plate3 against 0 witnesses"))
      (cp-check "A5 N1: plan CONFLICT" (member "  plan CONFLICT" lines :test #'string=))))
  (let ((lines (cp-report-lines
                 (cp-one-stage-plan (list (list :id "n2" :view :physical :cycle :open :ghosts :present
                                                :available-witnesses () :require '(gate5 gate7)))))))
    (cp-check "A5 N2: B2 CONFLICT on switch2 with S1's exclusion pair"
              (cp-has-line lines "B2  CONFLICT  [grade 1; S1]  switch2 demanded on by gate7 and off by gate5; S1 EXCLUSION {gate5, gate7}")))
  (let ((lift (cp-segment "c2.lift"))
        (alcove (cp-segment "c2.alcove")))
    (remf alcove :landing)
    (let ((lines (cp-report-lines (cp-one-stage-plan (list lift alcove)))))
      (cp-check "A5 N3: B4 CONDITIONAL with checklist 2.2's question"
                (and (cp-has-line lines "B4  CONDITIONAL  [grade 1; CC]  after c2.lift: switch1 stops blower1 (lift to location20) and opens gate2 (G15 FLAG); no landing stated")
                     (cp-has-line lines "        question  in the successor after the toggle, is the launch support at location20 still present?")))
      (cp-check "A5 N3: plan CONDITIONAL" (member "  plan CONDITIONAL" lines :test #'string=))))
  (let ((gate9 (cp-segment "f.gate9")))
    (setf (getf gate9 :held) nil
          (getf gate9 :available-witnesses) '(connector1 box1))
    (let ((lines (cp-report-lines (cp-one-stage-plan (list gate9)))))
      (cp-check "A5 N4: three B1 shortage rows, violator plate6-8 against 2 witnesses"
                (every (lambda (plate)
                         (cp-has-line lines (format nil "B1  CONFLICT  [grade 1 -> 2; S1 S2 RO]  ~(~A~) for gate9: matched; shortage, violator plate6, plate7, plate8 against 2 witnesses (box1, connector1)" plate)))
                       '(plate6 plate7 plate8))))))


(defun cp-block-text ()
  (let ((text (with-open-file (stream (merge-pathnames "tech/constraint-profile.lisp"
                                                       (asdf:system-source-directory :wouldwork)))
                (let ((string (make-string (file-length stream))))
                  (subseq string 0 (read-sequence string stream))))))
    (string-downcase (subseq text (search ";;;; CP -- CYCLE-PLAN CHECK" text)
                             (search "(defun report-static-constraint-profile" text)))))


(defun cp-check-source ()
  "A6: the CP block names no object of the staged problem except the declared constants
 normal and inverted, uses neither LABELS nor FLET, and defines each function before its
 first use in the block."
  (let* ((block (cp-block-text))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants)))))
         (found (remove-if-not
                  (lambda (object)
                    (let ((name (string-downcase (symbol-name object))))
                      (loop for start = (search name block) then (search name block :start2 (1+ start))
                            while start
                            thereis (and (or (zerop start) (not (alphanumericp (char block (1- start)))))
                                         (let ((end (+ start (length name))))
                                           (or (= end (length block))
                                               (not (or (alphanumericp (char block end))
                                                        (char= (char block end) #\-)
                                                        (char= (char block end) #\*)))))))))
                  objects))
         (code (search "(defparameter " block))
         (names (loop for start = (search "(defun " block) then (search "(defun " block :start2 (1+ start))
                      while start
                      collect (cons (subseq block (+ start 7) (position #\Space block :start (+ start 7))) start)))
         (early (remove-if (lambda (entry)
                             (>= (search (car entry) block :start2 code) (cdr entry)))
                           names)))
    (cp-check "A6: CP block located" (> (length block) 5000))
    (cp-check "A6: no LABELS or FLET in the CP block"
              (not (or (search "(labels " block) (search "(flet " block))))
    (format t "~&  staged names found in the CP block: ~(~{~A~^ ~}~)~%" found)
    (cp-check (format nil "A6: no staged object name (~D checked) in the CP block, except declared normal, inverted"
                      (length objects))
              (subsetp found '(normal inverted)))
    (format t "~&  used before definition: ~{~A~^ ~}~%" (mapcar #'car early))
    (cp-check (format nil "A6: callees-first, ~D functions" (length names)) (null early))))


(defun cp-check-profile ()
  "A7: the regenerated profile equals the committed profile file, line for line."
  (let ((file (cp-file-lines "doc/problems/crelay-topo/Constraint-Static-Profile.txt"))
        (generated (cp-output-lines #'report-static-constraint-profile)))
    (cp-check (format nil "A7: regenerated profile (~D lines) equals the file (~D lines)"
                      (length generated) (length file))
              (equal (cp-trim-blank-tail generated) (cp-trim-blank-tail file)))))


(format t "~&CP checks, T24~%")
(cp-check-profile)
(cp-check-match)
(cp-check-plate-rows)
(cp-check-ro-scenario)
(cp-check-negatives)
(cp-check-source)
(format t "~&CP checks: ~D passed, ~D failed~%~{  failed: ~A~%~}"
        *cp-passed* (length *cp-failed*) (reverse *cp-failed*))
