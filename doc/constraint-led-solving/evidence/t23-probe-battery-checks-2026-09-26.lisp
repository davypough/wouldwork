;;; PB probe battery -- acceptance checks, T23, 2026-09-26.
;;; Expected readings: t23-probe-battery-2026-09-26.txt, part 1 (written before the run).
;;; Run after the full battery, in the same image:
;;;   (stage crelay-topo)
;;;   (ww-set *threads* 16)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "tech/constraint-probe-battery.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (run-probe-battery 11 5000000
;;;                      (merge-pathnames "doc/constraint-method/evidence/t23-probe-battery-results-2026-09-26.lisp"
;;;                                       (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t23-probe-battery-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; A3, A4 and A7 read the results file and the staged problem; A5 runs three tiny searches of
;;; its own into t23-pb-negative-2026-09-26.lisp; A6 reads the source.  A failed check is
;;; reported and counted, and the run continues; the last line gives the totals.

(in-package :ww)


(defparameter *pb-passed* 0)


(defparameter *pb-failed* nil)


(defparameter *pb-results-path*
  (merge-pathnames "doc/constraint-method/evidence/t23-probe-battery-results-2026-09-26.lisp"
                   (asdf:system-source-directory :wouldwork)))


(defparameter *pb-negative-path*
  (merge-pathnames "doc/constraint-method/evidence/t23-pb-negative-2026-09-26.lisp"
                   (asdf:system-source-directory :wouldwork)))


(defparameter *pb-expected-list*
  '(("P1.1" agent1 (has-location agent1 location14))
    ("P1.2" agent1 (has-location agent1 location19))
    ("P1.3" agent1 (has-location agent1 location20))
    ("P1.4" agent1 (has-location agent1 location4))
    ("P1.5" agent1 (has-location agent1 location5))
    ("P1.6" agent1 (has-location agent1 location6))
    ("P2.1" box1 (location6))
    ("P2.2" connector1 (location10 location8 location9))
    ("P2.3" tray1 (location20 location3 location4 location5 location7))
    ("P3.1" plate1 (depressed plate1))
    ("P3.2" plate2 (not (depressed plate2)))
    ("P3.3" plate3 (depressed plate3))
    ("P3.4" plate4 (depressed plate4))
    ("P3.5" plate5 (depressed plate5))
    ("P3.6" plate6 (depressed plate6))
    ("P3.7" plate7 (depressed plate7))
    ("P3.8" plate8 (depressed plate8))
    ("P3.9" receiver1 (active receiver1))
    ("P3.10" switch1 (switched-on switch1))
    ("P3.11" switch2 (switched-on switch2))
    ("P4.1" repeater1 (exists (?c connector) (paired ?c repeater1))))
  "A2 part 1, section 1.2: (id subject goal); for P2 the third element is the start region,
 whose complement the goal lists.")


(defparameter *pb-settled*
  '(("P1.4" 4 4) ("P1.5" 4 4) ("P3.1" 2 2) ("P3.10" 5 5))
  "A2 part 1, section 1.4: (id plan-length cutoff) of every settled CHEAP row.")


(defun pb-check (label passed)
  (if passed
    (progn (incf *pb-passed*)
           (format t "~&  pass  ~A~%" label))
    (progn (push label *pb-failed*)
           (format t "~&  FAIL  ~A~%" label))))


(defun pb-file-lines (relative)
  (with-open-file (stream (merge-pathnames relative (asdf:system-source-directory :wouldwork)))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim '(#\Return) line))))


(defun pb-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun pb-find (id records)
  (find id records :key (lambda (record) (getf record :id)) :test #'string=))


(defun pb-check-list ()
  "A3: the generated probe list equals A2's, row for row."
  (let ((probes (probe-battery-probes))
        (locations (sort (copy-list (census-type-instances 'location)) #'string< :key #'symbol-name)))
    (pb-check (format nil "A3: ~D probes generated, ~D expected" (length probes) (length *pb-expected-list*))
              (= (length probes) (length *pb-expected-list*)))
    (loop for (id subject goal) in *pb-expected-list*
          for probe = (pb-find id probes)
          do (pb-check (format nil "A3: ~A ~(~A~) as in A2" id subject)
                       (and probe
                            (eq subject (getf probe :subject))
                            (equal (getf probe :goal)
                                   (if (= 2 (getf probe :family))
                                     (cons 'or (loop for location in locations
                                                     unless (member location goal)
                                                       collect (list 'has-location subject location)))
                                     goal)))))))


(defun pb-check-settled (records)
  "A3: every settled row of A2 is CHEAP with the settled plan length and cutoff."
  (loop for (id length cutoff) in *pb-settled*
        for record = (pb-find id records)
        for last = (car (last (getf record :runs)))
        do (pb-check (format nil "A3: ~A settled CHEAP ~D @ ~D; generated ~A ~D @ ~D"
                             id length cutoff (getf record :label)
                             (length (getf last :plan)) (getf last :cutoff))
                     (and (eq (getf record :label) :cheap)
                          (= length (length (getf last :plan)))
                          (= cutoff (getf last :cutoff))))))


(defun pb-check-sources (probes)
  "A4: each landmark per located agent, each located cargo object, each S1 primitive and
 each fixed relay yields exactly one probe."
  (let* ((facts (from-here-facts *start-state*))
         (controls (control-facts))
         (context (hint-route-context controls))
         (landmarks (probe-landmark-locations controls (getf context :names) (list-static-db)))
         (agents (remove-if-not (lambda (agent) (keeper-fact-value 'has-location agent facts))
                                (census-type-instances 'agent)))
         (cargo (remove-if-not (lambda (object) (keeper-fact-value 'has-location object facts))
                               (census-type-instances 'cargo)))
         (relays (append (census-type-instances 'floor-repeater) (census-type-instances 'wall-repeater))))
    (pb-check (format nil "A4: each of ~D landmark(s) per located agent (~D) once in P1"
                      (length landmarks) (length agents))
              (every (lambda (agent)
                       (every (lambda (location)
                                (= (if (eq location (keeper-fact-value 'has-location agent facts)) 0 1)
                                   (count (list 'has-location agent location) probes
                                          :key (lambda (probe) (getf probe :goal)) :test #'equal)))
                              landmarks))
                     agents))
    (loop for (family objects title) in (list (list 2 cargo "located cargo object")
                                              (list 3 (control-primitives controls) "S1 primitive")
                                              (list 4 relays "fixed relay"))
          do (pb-check (format nil "A4: each of ~D ~A(s) once in P~D" (length objects) title family)
                       (and (= (length objects)
                               (count family probes :key (lambda (probe) (getf probe :family))))
                            (every (lambda (object)
                                     (= 1 (count-if (lambda (probe)
                                                      (and (= family (getf probe :family))
                                                           (eq object (getf probe :subject))))
                                                    probes)))
                                   objects))))))


(defun pb-check-runs (records)
  "A4: every searched probe records, for each run, its cutoff (1, 2, ... in order), its
 truncation flag and its state count."
  (dolist (record records)
    (unless (eq (getf record :label) :start)
      (let ((runs (getf record :runs)))
        (pb-check (format nil "A4: ~A records ~D run(s) with cutoff, truncation and states"
                          (getf record :id) (length runs))
                  (and runs
                       (loop for run in runs
                             for cutoff from 1
                             always (and (eql cutoff (getf run :cutoff))
                                         (member (getf run :truncated) '(t nil))
                                         (integerp (getf run :states))))))))))


(defun pb-check-replays (records)
  "A4: every CHEAP plan replays from the start under VALIDATE-ACTION-SEQUENCE with its
 probe's goal as the goal test, and satisfies it.  The staged goal is restored."
  (let ((goal (copy-tree *goal*)))
    (unwind-protect
        (dolist (record records)
          (when (eq (getf record :label) :cheap)
            (install-compiled-goal (getf record :goal))
            (let ((validation (validate-action-sequence
                                (copy-problem-state *start-state*)
                                (getf (car (last (getf record :runs))) :plan)
                                :goal-test (symbol-function 'goal-fn))))
              (pb-check (format nil "A4: ~A's CHEAP plan (~D actions) replays and meets its goal"
                                (getf record :id) (action-sequence-validation-action-count validation))
                        (and (action-sequence-validation-success-p validation)
                             (action-sequence-validation-goal-satisfied-p validation))))))
      (install-compiled-goal goal))))


(defun pb-negative-label (max-depth states-limit probe)
  "PROBE run alone into the negative-test file; its label and runs."
  (run-probe-battery max-depth states-limit *pb-negative-path* (list probe))
  (let ((record (first (getf (read-probe-battery-results *pb-negative-path*) :probes))))
    (values (getf record :label) (getf record :runs))))


(defun pb-check-negative ()
  "A5: a goal true at the start is START with no search; P3.1 below its CHEAP cutoff is
 NOT FOUND or EXHAUSTED; a states limit of 1 on a probe truncated at cutoff 1 is STOPPED."
  (let* ((facts (from-here-facts *start-state*))
         (agent (find-if (lambda (agent) (keeper-fact-value 'has-location agent facts))
                         (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name)))
         (plate (pb-find "P3.1" (probe-battery-probes))))
    (multiple-value-bind (label runs)
        (pb-negative-label 11 5000000
                           (list :id "A5.1" :family 1 :subject agent
                                 :goal (list 'has-location agent (keeper-fact-value 'has-location agent facts))))
      (pb-check (format nil "A5: a goal true at the start is START (~A) with ~D run(s)" label (length runs))
                (and (eq label :start) (null runs))))
    (multiple-value-bind (label runs) (pb-negative-label 1 5000000 plate)
      (pb-check (format nil "A5: P3.1 at maximum depth 1 is NOT FOUND or EXHAUSTED (~A, ~D run)" label (length runs))
                (member label '(:not-found :exhausted))))
    (multiple-value-bind (label runs) (pb-negative-label 3 1 plate)
      (pb-check (format nil "A5: P3.1 with states limit 1 is STOPPED (~A, ~D run)" label (length runs))
                (and (eq label :stopped) (= 1 (length runs)))))))


(defun pb-source-forms ()
  "The top-level forms of the battery source, read in :WW."
  (with-open-file (stream (merge-pathnames "tech/constraint-probe-battery.lisp"
                                           (asdf:system-source-directory :wouldwork)))
    (let ((*package* (find-package :ww)))
      (loop for form = (read stream nil stream)
            until (eq form stream)
            collect form))))


(defun pb-check-source ()
  "A6: no staged object name, no LABELS or FLET, definitions callees-first, no blank line
 inside a definition; the source reads, so its parentheses balance."
  (let* ((lines (pb-file-lines "tech/constraint-probe-battery.lisp"))
         (text (string-downcase (format nil "~{~A~%~}" lines)))
         (forms (pb-source-forms))
         (names (loop for form in forms
                      when (member (first form) '(defun defparameter))
                        collect (second form)))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants)))))
         (found (remove-if-not
                  (lambda (object)
                    (let ((name (string-downcase (symbol-name object))))
                      (loop for start = (search name text) then (search name text :start2 (1+ start))
                            while start
                            thereis (and (or (zerop start) (not (alphanumericp (char text (1- start)))))
                                         (let ((end (+ start (length name))))
                                           (or (= end (length text))
                                               (not (or (alphanumericp (char text end))
                                                        (char= (char text end) #\-)
                                                        (char= (char text end) #\*)))))))))
                  objects))
         (forward (loop for form in forms
                        for position = (position (second form) names)
                        when (eq (first form) 'defun)
                          append (loop for symbol in (remove-duplicates (alexandria:flatten (cddr form)))
                                       for callee = (position symbol names)
                                       when (and callee (> callee position))
                                         collect (list (second form) symbol))))
         (inside (loop for (line next) on lines
                       for number from 1
                       when (and (zerop (length (string-trim " " line)))
                                 next
                                 (plusp (length next))
                                 (not (member (char next 0) '(#\( #\;)))
                                 (plusp (length (string-trim " " next))))
                         collect number)))
    (pb-check (format nil "A6: source reads as ~D forms, ~D definitions" (length forms) (length names))
              (> (length names) 10))
    (pb-check "A6: no LABELS or FLET" (not (or (search "(labels " text) (search "(flet " text))))
    (format t "~&  staged names found: ~(~{~A~^ ~}~)~%" found)
    (pb-check (format nil "A6: no staged object name (~D checked)" (length objects)) (null found))
    (format t "~&  forward references: ~(~S~)~%" forward)
    (pb-check "A6: definitions callees-first" (null forward))
    (format t "~&  blank lines followed by indented text: ~S~%" inside)
    (pb-check "A6: no blank line inside a definition" (null inside))))


(defun pb-check-profile ()
  "A7: the regenerated profile equals the committed profile file, line for line."
  (let ((file (pb-file-lines "doc/problems/crelay-topo/Constraint-Static-Profile.txt"))
        (generated (with-input-from-string
                       (stream (with-output-to-string (*standard-output*)
                                 (report-static-constraint-profile)))
                     (loop for line = (read-line stream nil) while line collect line))))
    (pb-check (format nil "A7: regenerated profile (~D lines) equals the file (~D lines)"
                      (length generated) (length file))
              (equal (pb-trim-blank-tail generated) (pb-trim-blank-tail file)))))


(format t "~&PB checks, T23~%")
(pb-check-profile)
(pb-check-list)
(let ((records (getf (read-probe-battery-results *pb-results-path*) :probes)))
  (pb-check-settled records)
  (pb-check-sources records)
  (pb-check-runs records)
  (pb-check-replays records))
(pb-check-negative)
(pb-check-source)
(format t "~&PB checks: ~D passed, ~D failed~%~{  failed: ~A~%~}"
        *pb-passed* (length *pb-failed*) (reverse *pb-failed*))
