;;; Load after staging queensN, then (test-queens-symmetry).
;;; Independent queen enumeration and coordinate rotations check the spec helpers.
(in-package :ww)

(defvar *queens-count-classes*)
(defvar *N*)

(defun queens-test-legal-columns (placed size)
  "Independent attack test on a forward list of one-based column positions."
  (loop for col from 1 to size
        unless (loop for previous in (reverse placed)
                     for distance from 1
                     thereis (or (= col previous)
                                 (= (abs (- col previous)) distance)))
          collect col))

(defun queens-test-encoding-node (node placed levels)
  "Compare generated successors with the independent attack test at each prefix."
  (when (zerop levels) (return-from queens-test-encoding-node 0))
  (let* ((children (generate-children node))
         (actual (mapcar (lambda (state) (first (problem-state.instantiations state)))
                         children))
         (expected (queens-test-legal-columns placed *N*)))
    (assert (equal expected (sort actual #'<)))
    (1+ (loop for state in children
              for col = (first (problem-state.instantiations state))
              for prefix = (append placed (list col))
              sum (queens-test-encoding-node
                    (make-node :state state :parent node :depth (length prefix))
                    prefix (1- levels))))))

(defun test-queens-encoding ()
  "Check all prefixes through three placements, including their fourth-row choices."
  (let* ((*queens-count-classes* nil)
         (checked (queens-test-encoding-node (make-node :state *start-state* :depth 0)
                                           nil 4)))
    (format t "~&QUEENS ENCODING PASS: ~D prefixes checked at N=~D~%" checked *N*)
    checked))

(defun test-queens-reflection ()
  "Check the actual pruning hook and expansion at every first placement."
  (let* ((root (make-node :state *start-state* :depth 0))
         (children (expand root)))
    (assert (= *N* (length children)))
    (dolist (state children)
      (let* ((col (first (problem-state.instantiations state)))
             (*queens-count-classes* t)
             (rejected (> col (ceiling *N* 2))))
        (assert (eq rejected (not (null (funcall 'prune-state? state)))))
        (when rejected
          (assert (null (expand (make-node :state state :depth 1 :parent root)))))
        (let ((*queens-count-classes* nil))
          (assert (not (funcall 'prune-state? state))))))
    (format t "~&QUEENS REFLECTION PASS: N=~D~%" *N*)
    t))

(defun check-queens-example ()
  (let ((placed nil))
    (assert (= *N* (solution.depth *count-example*)))
    (dolist (step (solution.path *count-example*))
      (let ((col (second (second step))))
        (assert (member col (queens-test-legal-columns placed *N*)))
        (setf placed (append placed (list col)))))
    (when *queens-count-classes*
      (assert (funcall 'queens-canonical-board-p
                       (map 'vector #'1- placed))))))

(defun queens-test-rotate (board)
  (let* ((size (length board))
         (rotated (make-array size)))
    (dotimes (row size rotated)
      (setf (aref rotated (aref board row)) (- size 1 row)))))

(defun queens-test-images (board)
  (let ((rotated (copy-seq board))
        (images nil))
    (dotimes (turn 4)
      (push rotated images)
      (push (map 'vector (lambda (col) (- (length board) 1 col)) rotated) images)
      (setf rotated (queens-test-rotate rotated)))
    (remove-duplicates images :test #'equalp)))

(defun queens-test-board (board)
  (let* ((before (copy-seq board))
         (images (queens-test-images board))
         (representatives (count-if 'queens-canonical-board-p images)))
    (assert (= 1 representatives))
    (assert (equalp before board))
    (if (funcall 'queens-canonical-board-p board) 1 0)))

(defun queens-test-enumerate (size &optional (placed nil))
  "Independent list-based solver; returns total boards and canonical boards."
  (if (= size (length placed))
      (values 1 (queens-test-board (coerce (reverse placed) 'vector)))
      (let ((total 0) (classes 0))
        (dotimes (col size)
          (unless (loop for previous in placed
                        for distance from 1
                        thereis (or (= col previous)
                                    (= (abs (- col previous)) distance)))
            (multiple-value-bind (child-total child-classes)
                (queens-test-enumerate size (cons col placed))
              (incf total child-total)
              (incf classes child-classes))))
        (values total classes))))

(defun test-queens-symmetry ()
  ;; OEIS A000170 and A002562, checked 2026-10-06.
  ;; https://oeis.org/A000170 and https://oeis.org/A002562
  (loop for size from 1
        for total in '(1 0 0 2 10 4 40 92 352 724)
        for classes in '(1 0 0 1 2 1 6 12 46 92)
        do (multiple-value-bind (actual-total actual-classes)
               (queens-test-enumerate size)
             (assert (= total actual-total))
             (assert (= classes actual-classes))
             (format t "~&QUEENS SYMMETRY PASS: N=~D, boards=~D, classes=~D~%"
                     size total classes)))
  (format t "~&QUEENS SYMMETRY REGRESSIONS PASSED~%")
  t)

(defun measure-queens-symmetry (classes-p expected)
  "One bounded search of the staged board; does not change size or thread count."
  (setf *queens-count-classes* classes-p)
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time))
        (bytes (sb-ext:get-bytes-consed))
        (gc-time sb-ext:*gc-run-time*))
    (sb-ext:with-timeout 300 (solve))
    (assert (= expected *solution-count*))
    (assert (null *solution-paths*))
    (assert (null *unique-solution-states*))
    (check-queens-example)
    (format t "~&QUEENS MEASURE: classes=~S count=~D seconds=~,3F bytes=~D gc-seconds=~,3F cycles=~D~%"
            classes-p *solution-count*
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)
            (- (sb-ext:get-bytes-consed) bytes)
            (/ (- sb-ext:*gc-run-time* gc-time) internal-time-units-per-second)
            *program-cycles*)))
