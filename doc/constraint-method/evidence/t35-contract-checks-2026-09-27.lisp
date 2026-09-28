;;; T35 focused checks; stage claustro-topo and load the diagnostic first. No search/replay.
(in-package :ww)
(defvar *t35-count* 0)
(defun t35-check (label value)
  (assert value () "T35 failed: ~A" label)
  (incf *t35-count*)
  (format t "~&PASS ~A~%" label))
(defun t35-report (function)
  (with-output-to-string (*standard-output*) (funcall function)))
(defun t35-corridor (state)
  (funcall (symbol-function 'fixed-beam-corridor-clear) state 'transmitter1 'receiver1))
(defun t35-fixed-checks ()
  (let* ((records (fixed-beam-records)) (row (first records))
         (facts (list-static-db)) (coupling '(coupled transmitter1 receiver1))
         (open (sightline-state-with-open-gates (census-type-instances 'gate)
                                               (census-type-instances 'gate)))
         (closed (sightline-state-with-open-gates (census-type-instances 'gate) nil))
         (crossing (find 'gate1 (getf row :crossings) :key #'second)))
    (t35-check "one connectorless fixed link" (and (= 1 (length records)) (null (census-type-instances 'connector))))
    (t35-check "fixed row source/sink" (and (eq 'transmitter1 (getf row :source)) (eq 'receiver1 (getf row :sink))))
    (t35-check "fixed row names gate1/location2"
               (and (equal '(gate1) (getf row :gates)) (equal '(location2) (getf row :locations))))
    (t35-check "recorded gate and authored obstacles retained"
               (and crossing (equal '(gate1 location2) (getf row :obstacles))))
    (t35-check "corridor matches engine at start"
               (eql (not (null (getf row :corridor-clear))) (not (null (t35-corridor *start-state*)))))
    (t35-check "open gate clears fixed corridor" (t35-corridor open))
    (t35-check "closed gate blocks fixed corridor" (not (t35-corridor closed)))
    (t35-check "finite gate can be cleared above its top"
               (funcall (symbol-function 'barrier-crossing-clear-for-object) closed nil crossing 100 100))
    (let ((top (funcall (symbol-function 'top) closed 'gate1)))
      (t35-check "gate-top equality still blocks"
                 (not (funcall (symbol-function 'barrier-crossing-clear-for-object) closed nil crossing top top))))
    (let ((blocked (copy-problem-state open)))
      (delete-proposition '(has-location box1 location4) (problem-state.idb blocked))
      (delete-proposition '(on box1 plate1) (problem-state.idb blocked))
      (add-proposition '(has-location box1 location2) (problem-state.idb blocked))
      (invalidate-problem-state-hash blocked)
      (t35-check "body spanning authored location blocks" (not (t35-corridor blocked))))
    (let* ((without (remove-if (lambda (fact) (eq (first fact) 'beam-via)) facts))
           (missing (fixed-beam-record coupling without open))
           (empty (fixed-beam-record coupling (cons '(beam-via transmitter1 nil receiver1) without) open)))
      (t35-check "missing differs from empty authored corridor"
                 (and (not (getf missing :authored-p)) (getf empty :authored-p)))
      (t35-check "absent crossing record is UNRECORDED"
                 (eq :unrecorded (getf (fixed-beam-record coupling
                                        (remove-if (lambda (fact) (eq (first fact) 'los-barrier-crossings>)) facts)
                                        open) :crossings))))
    (dolist (function '(report-fixed-beam-corridors report-relay-chain-table))
      (let ((text (t35-report function)))
        (t35-check (list "fixed dependency report" function)
                   (and (search "transmitter1 -> receiver1" text)
                        (search "gate candidates (gate1)" text)
                        (search "occupancy locations (location2)" text)))))
    (let ((hints (hint-fixed-beam-rows)))
      (t35-check "H3 has qualified fixed candidate"
                 (and (= 1 (length hints)) (string= "CANDIDATE" (getf (first hints) :label))
                      (search "gate1" (getf (first hints) :limit))
                      (search "location2" (getf (first hints) :limit)))))))
(defun t35-jammer-checks ()
  (let* ((rows (jammer-sightline-rows))
         (row (find-if (lambda (row) (and (eq 'jammer1 (getf row :jammer))
                                         (eq 'gate1 (getf row :target)))) rows))
         (site (find '(location5 plate2) (getf row :sites) :key (lambda (site) (subseq site 0 2)) :test #'equal)))
    (t35-check "one row per jammer and target"
               (= (length rows) (* (length (census-type-instances 'jammer)) (length (census-type-instances 'target)))))
    (t35-check "plate2 station sees gate1 with required beam gates"
               (and site (member 'gate2 (third site)) (member 'gate3 (third site))))
    (multiple-value-bind (open closed) (relay-chain-gate-states (census-type-instances 'gate))
      (dolist (row rows)
        (dolist (site (getf row :sites))
          (t35-check "listed sight agrees with engine"
                     (jammer-site-visible-p open site (getf row :jammer) (getf row :target)))
          (dolist (entry closed)
            (t35-check "single gate dependencies agree with engine"
                       (eql (not (null (member (car entry) (third site))))
                            (not (jammer-site-visible-p (cdr entry) site (getf row :jammer) (getf row :target)))))))))
    (let ((text (t35-report #'report-jammer-instances)))
      (t35-check "directional exclusions retained"
                 (and (search "(location1 location7 gate1)" text) (search "(location7 location1 gate4)" text))))))
(defun t35-checks ()
  (let ((state (make-subgoal-progress-state-signature *start-state*))
        (facts (copy-tree (list-static-db))))
    (t35-fixed-checks)
    (t35-jammer-checks)
    (t35-check "zero uncovered mechanics" (search "UNCOVERED (0):" (t35-report #'report-mechanic-coverage)))
    (t35-check "stairs arc retains directions"
               (search "location11 <-> location13" (t35-report #'report-stairs-instances)))
    (t35-check "no diagnostic state mutation"
               (equalp state (make-subgoal-progress-state-signature *start-state*)))
    (t35-check "no diagnostic static mutation" (equal facts (list-static-db)))))
(t35-checks)
(format t "~&T35 CHECKS PASSED: ~D~%" *t35-count*)

