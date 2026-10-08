;;; Load after test/search/queens-symmetry.lisp and staging queensN.
(in-package :ww)

(defun queens-test-empty-future-row-p (placed size)
  "Independent coordinate test; no masks or spec helper calls."
  (loop for row from (1+ (length placed)) to size
        thereis (not (loop for col from 1 to size
                          thereis (loop for previous in placed
                                        for previous-row from 1
                                        always (and (/= col previous)
                                                    (/= (abs (- col previous))
                                                        (- row previous-row))))))))

(defun queens-test-forward-node (node placed levels)
  "Check the actual hook, including prefixes a pruned search would never visit."
  (let* ((expected (queens-test-empty-future-row-p placed *N*))
         (actual (not (null (funcall 'prune-state? (node.state node))))))
    (assert (eq expected actual))
    (when expected (assert (null (expand node))))
    (1+ (if (zerop levels) 0
            (loop for state in (generate-children node)
                  for col = (first (problem-state.instantiations state))
                  for prefix = (append placed (list col))
                  sum (queens-test-forward-node
                       (make-node :state state :parent node :depth (length prefix))
                       prefix (1- levels)))))))

(defun test-queens-forward (&optional (levels 5))
  (let* ((*queens-count-classes* nil)
         (checked (queens-test-forward-node
                   (make-node :state *start-state* :depth 0) nil levels)))
    (format t "~&QUEENS FORWARD PASS: ~D prefixes at N=~D~%" checked *N*)
    checked))
