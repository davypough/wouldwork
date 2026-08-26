;;; Filename: ww-novelty.lisp

;;; Retired width-based novelty pruning experiment.  This file is intentionally absent from
;;; WOULDWORK.ASD and is retained only as source material for a possible future
;;; completeness-preserving novelty ordering strategy.


(in-package :ww)


(sb-ext:defglobal *novelty-atom-ids* (make-hash-table :test #'equal)
  "Maps each proposition seen in this search to a small integer id.")


(sb-ext:defglobal *novelty-partitions* (make-hash-table :test #'equal)
  "Maps a partition value to the pair of tables recording what that partition has seen:
   the atom ids asserted by some state in it, and the encoded atom-id pairs asserted
   together by some state in it.")


(sb-ext:defglobal *novelty-atom-count* 0
  "Number of distinct propositions given an id in this search.")


(defparameter *novelty-pair-radix* 1000000
  "Multiplier encoding an ordered atom-id pair as one integer.")


(defun novelty-pruned-p (state depth)
  "Whether STATE fails the width-*NOVELTY-PRUNING* novelty test and may be discarded.
   A state is novel when it asserts some conjunction of at most *NOVELTY-PRUNING* atoms
   that no earlier state in its partition asserted.  This is deliberately incomplete: it
   can discard the only path to a solution, so a plan found under it is still valid, but a
   failure to find one proves nothing about the problem."
  (when *novelty-pruning*
    (let ((ids (state-novelty-atom-ids state))
          (tables (novelty-partition-tables (state-novelty-partition state depth))))
      (cond ((or (novel-atoms-p ids (car tables))
                 (and (eql *novelty-pruning* 2) (novel-atom-pairs-p ids (cdr tables))))
             (record-state-novelty ids tables)
             nil)
            (t
             (incf *novelty-pruned*)
             t)))))


(defun reset-novelty-pruning ()
  "Clears the novelty tables and seeds them from the start state before a new search."
  (clrhash *novelty-atom-ids*)
  (clrhash *novelty-partitions*)
  (setf *novelty-atom-count* 0)
  (setf *novelty-pruned* 0)
  (when *novelty-pruning*
    (record-state-novelty
      (state-novelty-atom-ids *start-state*)
      (novelty-partition-tables (state-novelty-partition *start-state* 0)))))


(defun state-novelty-partition (state depth)
  "The novelty partition STATE at DEPTH belongs to.  A state is compared for novelty only
   against earlier states in its own partition, so a partition that advances with real
   progress keeps a must-undo plan alive where one global partition would discard it."
  (case *novelty-partition*
    ((nil) 0)
    (depth depth)
    (query (funcall (symbol-function 'novelty-partition?) state))))


(defun novelty-partition-tables (partition)
  "The seen-atom and seen-pair tables for PARTITION, created on first use."
  (or (gethash partition *novelty-partitions*)
      (setf (gethash partition *novelty-partitions*)
            (cons (make-hash-table :test #'eql)
                  (make-hash-table :test #'eql)))))


(defun state-novelty-atom-ids (state)
  "Returns the atom ids of every proposition true in STATE, assigning an id to any
   proposition this search has not seen before.  A fluent proposition is identified by its
   storage key together with its values, so a changed fluent is a different atom."
  (iter (for (key val) in-hashtable (problem-state.idb state))
        (for identity = (if (eql val t) key (cons key val)))
        (collecting (or (gethash identity *novelty-atom-ids*)
                        (setf (gethash identity *novelty-atom-ids*)
                              (incf *novelty-atom-count*))))))


(defun novel-atoms-p (ids seen-atoms)
  "Whether any atom id in IDS is new to SEEN-ATOMS."
  (iter (for id in ids)
        (thereis (not (gethash id seen-atoms)))))


(defun novel-atom-pairs-p (ids seen-pairs)
  "Whether any unordered pair of atom ids in IDS is new to SEEN-PAIRS."
  (iter (for tail on ids)
        (thereis (iter (for other in (rest tail))
                       (thereis (not (gethash (novelty-pair-code (first tail) other)
                                              seen-pairs)))))))


(defun record-state-novelty (ids tables)
  "Marks every atom in IDS, and under width 2 every pair drawn from IDS, as seen in the
   partition whose TABLES are supplied."
  (dolist (id ids)
    (setf (gethash id (car tables)) t))
  (when (eql *novelty-pruning* 2)
    (iter (for tail on ids)
          (dolist (other (rest tail))
            (setf (gethash (novelty-pair-code (first tail) other) (cdr tables)) t)))))


(defun novelty-pair-code (id1 id2)
  "Canonical integer key for the unordered atom-id pair ID1, ID2."
  (if (< id1 id2)
    (+ (* id1 *novelty-pair-radix*) id2)
    (+ (* id2 *novelty-pair-radix*) id1)))
