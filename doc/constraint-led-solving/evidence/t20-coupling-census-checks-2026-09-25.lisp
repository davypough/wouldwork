;;; CC coupling census -- acceptance checks, T20, 2026-09-25.
;;; Expected readings: t20-coupling-census-2026-09-25.txt, part 1 (written before the run).
;;; Run after staging crelay-topo and loading tech/constraint-profile.lisp:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t20-coupling-census-checks-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; No search, propagation or replay.  Errors on the first failed check.

(in-package :ww)


(defparameter *cc-check-count* 0)


(defparameter *cc-rows* nil
  "RC hop rows, computed once.")


(defun cc-check (label passed)
  (incf *cc-check-count*)
  (unless passed
    (error "CC check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun cc-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil) while line collect line)))


(defun cc-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun cc-evidence-path (name)
  (merge-pathnames (concatenate 'string "doc/constraint-method/evidence/" name)
                   (asdf:system-source-directory :wouldwork)))


(defun cc-expected-lines ()
  "The expected CC block from the evidence file: from its CC header line up to the line
 that begins the K3 basis note, blank tail removed."
  (let* ((lines (with-open-file (stream (cc-evidence-path "t20-coupling-census-2026-09-25.txt"))
                  (loop for line = (read-line stream nil) while line collect line)))
         (start (member "CC  COUPLING CENSUS  [grade 1; occluder role grade 2]" lines
                        :test #'string=)))
    (cc-trim-blank-tail
      (loop for line in start
            until (and (>= (length line) 12) (string= (subseq line 0 12) "Basis of K3:"))
            collect line))))


(defun cc-generated-lines ()
  "The generated CC block, from its header line, blank tail removed."
  (let ((lines (cc-lines (with-output-to-string (*standard-output*)
                           (report-coupling-census)))))
    (cc-trim-blank-tail
      (member "CC  COUPLING CENSUS  [grade 1; occluder role grade 2]" lines :test #'string=))))


(defun cc-rows-output (facts)
  (with-output-to-string (*standard-output*)
    (report-coupling-rows facts (traversal-arc-facts) *cc-rows*)))


(defun cc-check-match ()
  "A3: the generated block equals the expected block line for line."
  (let ((expected (cc-expected-lines))
        (generated (cc-generated-lines)))
    (cc-check "expected block read from the evidence file" (> (length expected) 40))
    (loop for e in expected
          for g in generated
          for n from 1
          unless (string= e g)
            do (format t "~&  line ~D~%    expected: ~A~%    generated: ~A~%" n e g))
    (cc-check (format nil "generated block (~D lines) equals expected (~D lines)"
                      (length generated) (length expected))
              (equal expected generated))))


(defun cc-check-domain ()
  "A4: every CONTROLS primitive and controlled device has exactly one role-table row."
  (let* ((facts (control-facts))
         (objects (remove-duplicates
                    (append (mapcar #'third facts)
                            (loop for fact in facts
                                  append (loop for clause in (second fact)
                                               append (copy-list clause))))))
         (lines (cc-lines (cc-rows-output facts)))
         (table (loop for line in (rest (member-if (lambda (line) (search "role table" line)) lines))
                      while (plusp (length line))
                      collect line)))
    (cc-check "21 domain objects" (= 21 (length objects)))
    (cc-check "role table has 21 rows" (= 21 (length table)))
    (cc-check "each domain object has exactly one row"
              (every (lambda (object)
                       (= 1 (count-if (lambda (line)
                                        (let ((prefix (format nil "    ~(~A~)  " object)))
                                          (and (>= (length line) (length prefix))
                                               (string= prefix (subseq line 0 (length prefix))))))
                                      table)))
                     objects))))


(defun cc-check-negative ()
  "A5: the G15 FLAG is present with the staged facts, and absent without the lift or
 without the barrier."
  (let ((facts (control-facts)))
    (cc-check "staged facts: exactly one G15 FLAG"
              (= 1 (count-if (lambda (line) (search "G15 FLAG" line))
                             (cc-lines (cc-rows-output facts)))))
    (cc-check "without blower1: no G15 FLAG"
              (not (search "G15 FLAG" (cc-rows-output (remove 'blower1 facts :key #'third)))))
    (cc-check "without gate2: no G15 FLAG"
              (not (search "G15 FLAG" (cc-rows-output (remove 'gate2 facts :key #'third)))))
    (cc-check "without gate2: no switch1 fan-out row"
              (not (search "    switch1  subsystems"
                           (cc-rows-output (remove 'gate2 facts :key #'third)))))))


(defun cc-check-source ()
  "A6 (C3 and LABELS/FLET): the CC block of the source names no object of the staged
 problem and uses neither LABELS nor FLET."
  (let* ((text (with-open-file (stream (merge-pathnames "tech/constraint-profile.lisp"
                                                        (asdf:system-source-directory :wouldwork)))
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (block (string-downcase
                  (subseq text (search ";;;; CC -- COUPLING CENSUS" text)
                          (search "(defun report-static-constraint-profile" text))))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants))))))
    (cc-check "CC block located" (> (length block) 1000))
    (cc-check "no LABELS or FLET in the CC block"
              (not (or (search "(labels " block) (search "(flet " block))))
    (cc-check (format nil "no staged object name (~D checked) in the CC block" (length objects))
              (notany (lambda (object)
                        (let ((name (string-downcase (symbol-name object))))
                          (loop for start = (search name block) then (search name block :start2 (1+ start))
                                while start
                                thereis (and (or (zerop start)
                                                 (not (alphanumericp (char block (1- start)))))
                                             (let ((end (+ start (length name))))
                                               (or (= end (length block))
                                                   (not (or (alphanumericp (char block end))
                                                            (char= (char block end) #\-)))))))))
                      objects))))


(format t "~&CC checks, T20~%")
(setf *cc-rows* (coupling-hop-rows))
(cc-check-match)
(cc-check-domain)
(cc-check-negative)
(cc-check-source)
(format t "~&CC checks: ~D passed~%" *cc-check-count*)
