;;; T37 focused checks. Stage claustro-topo, then load the profile and this file.
(in-package :ww)
(defvar *t37-count* 0)
(defun t37-check (name value)
  (assert value () "T37 failure: ~A" name)
  (incf *t37-count*)
  (format t "~&PASS ~A~%" name))
(defun t37-budget-report ()
  (with-output-to-string (*standard-output*) (report-budget-arithmetic)))
(defun t37-check-facts (facts expected-count disjoint)
  (let* ((before (copy-tree facts))
         (costs (budget-arithmetic-body-cost-devices facts)))
    (t37-check "class count" (= expected-count (length costs)))
    (t37-check "disjoint verdict" (eql disjoint (budget-arithmetic-disjoint-supports-p costs)))
    (t37-check "input unchanged" (equal before facts))
    costs))
(defun t37-fixtures ()
  (let* ((facts '((controls ((plate1 plate2 plate3)) device-c normal)
                  (controls ((plate3 plate1 plate2 plate1) (plate2 plate1 plate3)) device-a normal)
                  (controls ((plate2 plate3 plate1)) device-b normal)))
         (costs (t37-check-facts facts 1 t)))
    (t37-check "three members retained and sorted"
               (equal '(device-a device-b device-c) (caar costs)))
    (t37-check "three equivalent devices cost three, not nine"
               (= 3 (budget-arithmetic-total-cost (budget-arithmetic-gate-costs costs))))
    (t37-check "reversed input has the same demand"
               (equal costs (budget-arithmetic-body-cost-devices (reverse facts)))))
  (t37-check-facts '((controls ((plate1 plate2) (plate3)) device-a normal)
                     (controls ((plate3) (plate2 plate1)) device-b normal)) 1 t)
  (t37-check-facts '((controls ((plate1)) device-a normal)
                     (controls ((plate1)) device-b inverted)) 2 nil)
  (t37-check-facts '((controls ((plate1 sensor-a)) device-a normal)
                     (controls ((plate1 sensor-b)) device-b normal)) 2 nil)
  (t37-check-facts '((controls ((plate1 plate2)) device-a normal)
                     (controls ((plate1) (plate2)) device-b normal)) 2 nil)
  (t37-check-facts '((controls ((plate1)) device-a normal)
                     (controls ((plate2)) device-b normal)) 2 t)
  (t37-check-facts '((controls ((sensor-a)) device-a normal)) 0 t)
  (let* ((facts '((controls ((plate1)) device-a normal)
                  (controls ((plate1)) device-b normal)
                  (controls ((plate2)) device-c normal)))
         (costs (t37-check-facts facts 2 t)))
    (t37-check "shared demand plus separate demand sums once"
               (= 2 (budget-arithmetic-total-cost (budget-arithmetic-gate-costs costs))))
    (t37-check "AM1 counts demands"
               (search "2 independent body-cost demands demand 2"
                       (first (budget-arithmetic-constraints
                                (budget-arithmetic-gate-costs costs) '(2 2) nil t)))))
  (let ((saved (symbol-function 'control-facts)))
    (unwind-protect
        (progn
          (setf (symbol-function 'control-facts)
                (lambda () '((controls ((plate1)) device-a normal)
                              (controls ((plate1 plate2)) device-b normal))))
          (t37-check "non-equivalent overlapping report still declines"
                     (search "body-cost support sets overlap" (t37-budget-report))))
      (setf (symbol-function 'control-facts) saved))))
(defun t37-h1-evaluation (controls occupancy actor)
  (let ((saved (symbol-function 'budget-arithmetic-total-cost)) (calls 0) (hints nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'budget-arithmetic-total-cost)
                (lambda (costs) (incf calls) (funcall saved costs)))
          (setf hints (hint-budget-rows controls occupancy actor)))
      (setf (symbol-function 'budget-arithmetic-total-cost) saved))
    (values calls hints)))
(defun t37-staged ()
  (let* ((facts (control-facts))
         (costs (budget-arithmetic-body-cost-devices facts))
         (occupancy (budget-arithmetic-segment-occupancy))
         (report (t37-budget-report)))
    (t37-check "claustro has one class for gate8/gate9"
               (equal '(((gate8 gate9) (plate1 plate2 plate3))) costs))
    (t37-check "claustro one demand of three"
               (= 3 (budget-arithmetic-total-cost (budget-arithmetic-gate-costs costs))))
    (t37-check "claustro classes pass disjointness" (budget-arithmetic-disjoint-supports-p costs))
    (t37-check "claustro report names both members and cost"
               (search "{gate8, gate9}: one shared control demand of 3" report))
    (t37-check "claustro no overlap refusal" (not (search "support sets overlap" report)))
    (t37-check "non-tight budget claims no shortage"
               (and (search "No shortage follows" report)
                    (not (search "refute the fully-open" report))))
    (t37-check "claustro AM2/AM3 evaluated"
               (and (search "AM2" report) (search "demand is 3" report)))
    (multiple-value-bind (calls hints) (t37-h1-evaluation facts occupancy 'agent1)
      (t37-check "claustro H1 reaches budget evaluation" (= calls 1))
      (format t "~&H1 evaluated: demand 3; pool ~S; ~D hints (non-tight is valid empty).~%"
              occupancy (length hints)))
    (multiple-value-bind (calls hints)
        (t37-h1-evaluation '((controls ((plate1)) device-a normal)
                             (controls ((plate1 plate2)) device-b normal)) '(2 2) nil)
      (t37-check "overlap does not evaluate H1 sum" (and (zerop calls) (null hints))))
    (let ((hints (hint-budget-rows facts '(2 2) nil)))
      (t37-check "tight artificial H1 pool counts one demand"
                 (search "plate demand 3 (1 demand)" (getf (first hints) :limit))))))
(t37-fixtures)
(t37-staged)
(format t "~&T37 FOCUSED CHECKS PASSED: ~D~%" *t37-count*)

