;;; Filename: ww-enum-verify.lisp
;;;
;;; Temporary verification utilities for residual symmetry pruning.
;;; Load after problem is initialized and enumerations have been run.
;;;
;;; Usage:
;;;   1) Run pruned enumeration, save *pruned-props*
;;;   2) Disable residual symmetry (redefine enum-residual-symmetry-ok-p to return T)
;;;   3) Run unpruned enumeration, save *unpruned-props*
;;;   4) (verify-residual-symmetry *pruned-props* *unpruned-props*)


(in-package :ww)


(defun all-permutations (list)
  "Return all permutations of LIST."
  (if (null (cdr list))
      (list list)
      (mapcan (lambda (x)
                (mapcar (lambda (rest) (cons x rest))
                        (all-permutations (remove x list :count 1))))
              list)))


(defun connectors-by-location (props group)
  "Group members of GROUP by their loc value found in PROPS.
   Returns an alist of (location . (connector...)) entries."
  (let ((loc-map nil))
    (dolist (p props loc-map)
      (when (and (eq (first p) 'loc)
                 (member (second p) group :test #'eq))
        (let ((obj (second p))
              (area (third p)))
          (let ((entry (assoc area loc-map :test #'eq)))
            (if entry
                (push obj (cdr entry))
                (push (cons area (list obj)) loc-map))))))))


(defun loc-restricted-permutations (group loc-groups)
  "Generate all permutations of GROUP that only swap within co-located subgroups.
   Members not appearing in any loc-group map to themselves."
  (let ((subgroup-perms
          (mapcar (lambda (entry)
                    (let ((members (cdr entry)))
                      (if (cdr members)
                          (all-permutations members)
                          (list members))))
                  loc-groups))
        (singletons (set-difference group
                                    (reduce #'append loc-groups :key #'cdr)
                                    :test #'eq)))
    (let ((base-perms (cartesian-product subgroup-perms)))
      (mapcar (lambda (combo)
                (let ((mapping nil))
                  (loop for entry in loc-groups
                        for perm in combo
                        for originals = (cdr entry)
                        do (loop for orig in originals
                                 for renamed in perm
                                 do (push (cons orig renamed) mapping)))
                  (dolist (s singletons)
                    (push (cons s s) mapping))
                  (mapcar (lambda (obj) (cdr (assoc obj mapping))) group)))
              base-perms))))


(defun make-canonical-permutation-restricted (props group)
  "Canonicalize PROPS by permuting GROUP members only within co-located subgroups.
   Returns the lexicographically smallest renamed proposition list."
  (let* ((loc-groups (connectors-by-location props group))
         (perms (loc-restricted-permutations group loc-groups))
         (best-props nil))
    (dolist (perm perms best-props)
      (let* ((mapping (mapcar #'cons group perm))
             (renamed (sort (mapcar (lambda (p)
                                      (mapcar (lambda (sym)
                                                (or (cdr (assoc sym mapping)) sym))
                                              p))
                                    props)
                            #'string< :key #'prin1-to-string)))
        (when (or (null best-props)
                  (string< (prin1-to-string renamed)
                           (prin1-to-string best-props)))
          (setf best-props renamed))))))


(defun verify-residual-symmetry (pruned-props unpruned-props
                                 &optional (groups *enumerator-detected-groups*))
  "Verify residual symmetry pruning soundness and completeness.
   PRUNED-PROPS: list of sorted prop-lists from pruned enumeration.
   UNPRUNED-PROPS: list of sorted prop-lists from unpruned enumeration.
   Reports:
     - pruned canonical count and duplicates
     - unpruned canonical count
     - missing from pruned (false rejections)
     - extra in pruned (impossible if pruned is subset)"
  (let* ((group (first groups))
         (canon-pruned (make-hash-table :test 'equal))
         (canon-unpruned (make-hash-table :test 'equal))
         (dup-count 0))
    (dolist (p pruned-props)
      (let ((c (if group (make-canonical-permutation-restricted p group) p)))
        (if (gethash c canon-pruned)
            (incf dup-count)
            (setf (gethash c canon-pruned) t))))
    (format t "Pruned: ~D states, ~D canonical, ~D duplicates~%"
            (length pruned-props) (hash-table-count canon-pruned) dup-count)
    (dolist (p unpruned-props)
      (let ((c (if group (make-canonical-permutation-restricted p group) p)))
        (setf (gethash c canon-unpruned) t)))
    (format t "Unpruned: ~D states, ~D canonical~%"
            (length unpruned-props) (hash-table-count canon-unpruned))
    (let ((missing-from-pruned 0)
          (extra-in-pruned 0))
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k canon-pruned) (incf missing-from-pruned)))
               canon-unpruned)
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k canon-unpruned) (incf extra-in-pruned)))
               canon-pruned)
      (format t "Missing from pruned: ~D~%" missing-from-pruned)
      (format t "Extra in pruned: ~D~%" extra-in-pruned)
      (values (zerop missing-from-pruned) (zerop extra-in-pruned) (zerop dup-count)))))
