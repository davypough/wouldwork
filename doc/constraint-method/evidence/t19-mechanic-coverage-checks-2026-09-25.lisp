;;; MC mechanic coverage -- acceptance checks, T19, 2026-09-25.
;;; Expected readings: t19-mechanic-coverage-2026-09-25.txt, part 1 (written before the run).
;;; Run after staging crelay-topo and loading tech/constraint-profile.lisp:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t19-mechanic-coverage-checks-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; No search, propagation or replay.  Errors on the first failed check.

(in-package :ww)


(defparameter *mc-check-count* 0)


(defun mc-check (label passed)
  (incf *mc-check-count*)
  (unless passed
    (error "MC check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun mc-output ()
  (with-output-to-string (*standard-output*)
    (report-mechanic-coverage)))


(defun mc-output-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil) while line collect line)))


(defun mc-check-domain ()
  "A4: the public spliced technologies are exactly crelay-topo's 16 includes, and each has
 exactly one verdict row."
  (let* ((expected '("beam-relay" "box" "elevation" "floor-blower" "gate" "jump" "ladder"
                     "plate" "reachability" "recorder" "step" "switch" "topo-lower-bound"
                     "tray" "visibility" "walkability"))
         (lines (mc-output-lines (mc-output))))
    (mc-check "public spliced technologies = the 16 expected names"
              (equal (mechanic-public-techs) expected))
    (mc-check "each has exactly one COVERED or UNCOVERED row"
              (every (lambda (tech)
                       (= 1 (count-if (lambda (line)
                                        (and (> (length line) 22)
                                             (string= (subseq line 0 22)
                                                      (format nil "    ~18A" tech))
                                             (search "COVERED" line)))
                                      lines)))
                     expected))
    (mc-check "UNCOVERED list is jump recorder step"
              (member "  UNCOVERED (3): jump recorder step" lines :test #'string=))))


(defun mc-check-rows ()
  "A3: the blower1 and ladder1-3 rows equal the expected readings; the walking exit line
 equals an independent count of walking arcs touching location20."
  (let* ((lines (mc-output-lines (mc-output)))
         (walking (sort (loop for arc in (traversal-arc-facts)
                              when (and (eq (second arc) 'walking)
                                        (or (eq (third arc) 'location20)
                                            (and (eq (first arc) 'traverse-via)
                                                 (eq (fifth arc) 'location20))))
                                collect (mechanic-exit-text 'location20 arc))
                        #'string<))
         (expected (append
                     (list "    blower1"
                           "      source       location4"
                           "      destination  location20"
                           "      control      ((switch1)) normal"
                           (format nil "      exits from location20 (~D), by mode; each mode's own predicate is not evaluated"
                                   (+ 2 (length walking)))
                           "        jumping (2): location5, location6 ((gate2))")
                     (when walking
                       (list (format nil "        walking (~D): ~{~A~^, ~}" (length walking) walking)))
                     (list "    ladder1  at location3"
                           "      climbing  location3 --> location1  family ((ladder1))  ladder at source: yes"
                           "    ladder2  at location8"
                           "      climbing  location8 --> location5  family ((ladder2))  ladder at source: yes"
                           "    ladder3  at location11"
                           "      climbing  location11 --> location10  family ((ladder3))  ladder at source: yes"))))
    (format t "~&  walking exits at location20: ~D~%" (length walking))
    (dolist (line expected)
      (mc-check (format nil "row present: ~A" (string-trim " " line))
                (member line lines :test #'string=)))))


(defun mc-check-negative ()
  "A5: without its registry entry, floor-blower is UNCOVERED and its contract is not printed."
  (let ((lines (let ((*mechanic-contracts* (remove "floor-blower" *mechanic-contracts*
                                                   :key #'first :test #'string=)))
                 (mc-output-lines (mc-output)))))
    (mc-check "negative: floor-blower row reads UNCOVERED"
              (member (format nil "    ~18AUNCOVERED" "floor-blower") lines :test #'string=))
    (mc-check "negative: UNCOVERED list gains floor-blower"
              (member "  UNCOVERED (4): floor-blower jump recorder step" lines :test #'string=))
    (mc-check "negative: no floor-blower contract printed"
              (not (member "  contract floor-blower" lines :test #'string=)))))


(defun mc-problem-object-names ()
  "Every instance name in the problem file's own DEFINE-TYPES, lowercased.  Types declared
 by tech/ files (traversal modes, control modes) are substrate vocabulary, not problem
 objects, so they are excluded by reading the problem source rather than *TYPES*."
  (let ((path (merge-pathnames "probs/problem-crelay-topo.lisp"
                               (asdf:system-source-directory :wouldwork)))
        (names nil))
    (with-open-file (stream path)
      (let ((*package* (find-package :ww)))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              when (and (consp form) (eq (first form) 'define-types))
                do (dolist (item (rest form))
                     (when (consp item)
                       (dolist (object item)
                         (pushnew (string-downcase (symbol-name object)) names
                                  :test #'string=)))))))
    names))


(defun mc-check-domain-generality ()
  "A6 (C3): no problem object name appears in the MC block's source text, as a whole word."
  (let* ((path (merge-pathnames "tech/constraint-profile.lisp"
                                (asdf:system-source-directory :wouldwork)))
         (text (with-open-file (stream path)
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (start (search ";;;; MC -- MECHANIC COVERAGE" text))
         (end (search "(defun report-static-constraint-profile" text :start2 start))
         (block (string-downcase (subseq text start end)))
         (names (mc-problem-object-names))
         (hits nil))
    (dolist (name names)
      (loop for position = (search name block) then (search name block :start2 (1+ position))
            while position
            do (let ((before (if (plusp position) (char block (1- position)) #\Space))
                     (after (if (< (+ position (length name)) (length block))
                              (char block (+ position (length name)))
                              #\Space)))
                 (unless (or (alphanumericp before) (find before "-*")
                             (alphanumericp after) (find after "-*"))
                   (pushnew name hits :test #'string=)))))
    (mc-check (format nil "C3: none of ~D problem object names in the MC block~@[ (found ~{~A~^, ~})~]"
                      (length names) hits)
              (null hits))))


(format t "~&MC acceptance checks (T19)~%")
(mc-check-domain)
(mc-check-rows)
(mc-check-negative)
(mc-check-domain-generality)
(format t "~&  ~D checks passed~%" *mc-check-count*)
