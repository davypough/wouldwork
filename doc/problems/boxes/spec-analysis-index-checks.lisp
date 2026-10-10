;;; Focused checks for the supplied indexed BOXES-ADVISOR instance.
;;; Load after staging. Loading only defines functions; it does not run checks.
;;; (check-boxes-indexed-model) exercises copied states, without solving/replaying.

(in-package :ww)

(defun boxes-check-require (condition control &rest arguments)
  (unless condition
    (error "Boxes index check: ~A" (apply #'format nil control arguments))))

(defun boxes-check-source-facts ()
  (with-open-file
      (input (merge-pathnames "probs/problem-boxes-advisor.lisp"
                              (asdf:system-source-directory :wouldwork)))
    (loop for form = (read input nil :eof)
          until (eq form :eof)
          when (and (consp form) (eq (first form) 'define-init))
            return (cdr form)
          finally (error "Boxes index check: specification has no DEFINE-INIT."))))

(defun boxes-check-relation-facts (relation facts)
  (remove-if-not (lambda (fact) (eq (first fact) relation)) facts))

(defun boxes-check-source-plates (area facts)
  (loop for plate in (gethash 'plate *types*)
        for fact = (find plate facts :key #'second :test #'eq)
        when (eq (third fact) area)
          collect plate))

(defun boxes-check-source-gates (area facts)
  (loop for gate in (gethash 'gate *types*)
        for fact = (find gate facts :key #'second :test #'eq)
        when (member area (cddr fact) :test #'eq)
          collect gate))

(defun boxes-check-present-p (state literal)
  (eql t (gethash (convert-to-integer-memoized literal)
                  (problem-state.idb state))))

(defun boxes-check-signature (state)
  (list (current-area state 'agent1)
        (loop for area in (gethash 'area *types*)
              collect (ground-count state area))
        (loop for plate in (gethash 'plate *types*)
              when (boxes-check-present-p state (list 'occupied plate))
                collect plate)
        (boxes-check-present-p state '(carrying agent1))))

(defun boxes-check-static-snapshot ()
  (list (loop for area in (gethash 'area *types*)
              collect (list area (copy-list (local-plates *start-state* area))
                            (copy-list (incident-gates *start-state* area))))
        (loop for gate in (gethash 'gate *types*)
              collect (list gate (controlling-plate *start-state* gate)))))

(defun boxes-check-topology (source-facts)
  (let ((plates (boxes-check-relation-facts 'plate-area source-facts))
        (gates (boxes-check-relation-facts 'gate-separates source-facts))
        (controls (boxes-check-relation-facts 'controls source-facts)))
    (dolist (area (gethash 'area *types*))
      (dolist (relation '(area-plates area-gates))
        (boxes-check-require
         (nth-value 1 (gethash (convert-to-integer-memoized (list relation area))
                              *static-idb*))
         "Missing ~S entry for area ~S, including its empty list." relation area))
      (boxes-check-require
       (equal (local-plates *start-state* area) (boxes-check-source-plates area plates))
       "Local plates for area ~S disagree with source PLATE-AREA facts/order." area)
      (boxes-check-require
       (equal (incident-gates *start-state* area) (boxes-check-source-gates area gates))
       "Incident gates for area ~S disagree with source GATE-SEPARATES facts/order." area))
    (dolist (gate (gethash 'gate *types*))
      (boxes-check-require
       (eq (controlling-plate *start-state* gate)
           (second (find gate controls :key #'third :test #'eq)))
       "Controller for gate ~S disagrees with source CONTROLS facts." gate))))

(defun boxes-check-openness (state source-controls context)
  (let ((occupied (third (boxes-check-signature state))))
    (dolist (gate (gethash 'gate *types*))
      (let* ((plate (second (find gate source-controls :key #'third :test #'eq)))
             (expected (not (null (member plate occupied :test #'eq)))))
        (boxes-check-require (eql (gate-open? state gate) expected)
                             "~S: gate ~S openness disagrees with plate ~S."
                             context gate plate)))))

(defun boxes-check-fixture-state (signature)
  (destructuring-bind (area counts occupied carrying) signature
    (let* ((state (copy-problem-state *start-state*))
           (idb (problem-state.idb state)))
      (update idb (list 'agent-area 'agent1 area))
      (loop for ground-area in (gethash 'area *types*)
            for count in counts
            do (update idb (list 'loose-boxes ground-area count)))
      (dolist (plate (gethash 'plate *types*))
        (update idb (if (member plate occupied :test #'eq)
                        (list 'occupied plate)
                        (list 'not (list 'occupied plate)))))
      (update idb (if carrying '(carrying agent1) '(not (carrying agent1))))
      (invalidate-problem-state-hash state)
      (box-arrangement-valid? state)
      state)))

(defun boxes-check-child (parent child expected source-controls context)
  (let ((trace (cons (problem-state.name child) (problem-state.instantiations child))))
    (boxes-check-require expected "~S: unexpected successor ~S." context trace)
    (boxes-check-require (equal (boxes-check-signature child) (second expected))
                         "~S: wrong arrangement after ~S: ~S."
                         context trace (boxes-check-signature child))
    (boxes-check-require (= (problem-state.time child) (1+ (problem-state.time parent)))
                         "~S: successor ~S does not cost one." context trace)
    (boxes-check-require (not (eq (problem-state.idb child) (problem-state.idb parent)))
                         "~S: successor ~S shares the parent database." context trace)
    (box-arrangement-valid? child)
    (boxes-check-openness child source-controls trace)))

(defun boxes-check-fixture (fixture source-controls)
  (destructuring-bind (context signature expected) fixture
    (let* ((parent (boxes-check-fixture-state signature))
           (before (boxes-check-signature parent))
           (static-before (boxes-check-static-snapshot))
           (children (generate-children (make-node :state parent :depth 0)))
           (traces (mapcar (lambda (child)
                             (cons (problem-state.name child)
                                   (problem-state.instantiations child))) children)))
      (boxes-check-require
       (and (= (length traces) (length expected))
            (null (set-difference traces (mapcar #'first expected) :test #'equal))
            (null (set-difference (mapcar #'first expected) traces :test #'equal)))
       "~S: expected choices ~S, got ~S." context (mapcar #'first expected) traces)
      (boxes-check-require (equal before (boxes-check-signature parent))
                           "~S: generating choices changed the parent." context)
      (boxes-check-require (equal static-before (boxes-check-static-snapshot))
                           "~S: generating choices changed the static indexes." context)
      (boxes-check-openness parent source-controls context)
      (dolist (child children)
        (boxes-check-child parent child
                           (assoc (cons (problem-state.name child)
                                        (problem-state.instantiations child)) expected
                                  :test #'equal)
                           source-controls context))
      (boxes-check-require
       (= (length children)
          (length (remove-duplicates children :key #'problem-state.idb :test #'eq)))
       "~S: independent successors share a database." context)
      (length children))))

(defun boxes-check-fixtures ()
  '((carrying-with-empty-plates
     (area1 (0 1 0 0) nil t)
     (((place agent1 ground area1) (area1 (1 1 0 0) nil nil))
      ((place agent1 plate1 area1) (area1 (0 1 0 0) (plate1) nil))
      ((place agent1 plate2 area1) (area1 (0 1 0 0) (plate2) nil))))
    (carrying-with-occupied-plate
     (area1 (0 0 0 0) (plate1) t)
     (((place agent1 ground area1) (area1 (1 0 0 0) (plate1) nil))
      ((place agent1 plate2 area1) (area1 (0 0 0 0) (plate1 plate2) nil))
      ((cross-gate agent1 gate1 area1 area2) (area2 (0 0 0 0) (plate1) t))))
    (remote-plate-and-nonincident-open-gate
     (area1 (0 0 0 0) (plate2 plate3) nil)
     (((pickup agent1 plate2 area1) (area1 (0 0 0 0) (plate3) t))
      ((cross-gate agent1 gate2 area1 area3) (area3 (0 0 0 0) (plate2 plate3) nil))))
    (two-box-ground-pile
     (area1 (2 0 0 0) nil nil)
     (((pickup agent1 ground area1) (area1 (1 0 0 0) nil t))))))

(defun check-boxes-indexed-model ()
  (boxes-check-require
   (and (eq *problem-name* 'boxes-advisor) (eq *algorithm* 'depth-first))
   "Stage BOXES-ADVISOR with DEPTH-FIRST before loading/running these checks.")
  (let* ((facts (boxes-check-source-facts))
         (controls (boxes-check-relation-facts 'controls facts))
         (initial-before (boxes-check-signature *start-state*))
         (successors 0))
    (boxes-check-require (equal initial-before '(area1 (1 1 0 0) nil nil))
                         "Unexpected initialized arrangement: ~S." initial-before)
    (boxes-check-require (= (hash-table-count (problem-state.idb *start-state*)) 5)
                         "Static indexes added entries to the copied dynamic state.")
    (boxes-check-require (not (goal-fn *start-state*)) "Start state satisfies the goal.")
    (box-arrangement-valid? *start-state*)
    (boxes-check-topology facts)
    (boxes-check-openness *start-state* controls 'start-state)
    (dolist (fixture (boxes-check-fixtures))
      (incf successors (boxes-check-fixture fixture controls)))
    (boxes-check-require (equal initial-before (boxes-check-signature *start-state*))
                         "Checks changed the staged start state.")
    (format t "~&BOXES-INDEX-CHECKS-PASSED: ~D areas, ~D controllers, 4 fixtures, ~D successors.~%"
            (length (gethash 'area *types*)) (length (gethash 'gate *types*)) successors)
    t))
