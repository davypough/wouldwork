;;; NH necessity hints -- acceptance checks, T21, 2026-09-25.
;;; Expected readings: t21-necessity-hints-2026-09-25.txt, part 1 (written before the run).
;;; Run after staging crelay-topo and loading tech/constraint-profile.lisp:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t21-necessity-hints-checks-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; No search, propagation or replay.  Errors on the first failed check.
;;; A4 reads each source section's PRINTED output, not NH's helpers, so the two readings are
;;; independent; H6's source is the start state's device-state propositions, not S1's aggregate.

(in-package :ww)


(defparameter *nh-check-count* 0)


(defparameter *nh-families* nil
  "NH's seven hint lists, computed once.")


(defun nh-check (label passed)
  (incf *nh-check-count*)
  (unless passed
    (error "NH check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun nh-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil) while line collect line)))


(defun nh-words (line)
  (let ((words nil) (start nil))
    (loop for index from 0 to (length line)
          for char = (when (< index (length line)) (char line index))
          do (cond ((and char (not (member char '(#\Space #\, #\( #\) #\:))))
                    (unless start (setf start index)))
                   (start (push (subseq line start index) words)
                          (setf start nil))))
    (nreverse words)))


(defun nh-output (function)
  (nh-lines (with-output-to-string (*standard-output*) (funcall function))))


(defun nh-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun nh-source-texts (family)
  "FAMILY's hint sources as lower-case strings, one per hint."
  (mapcar (lambda (hint) (string-downcase (format nil "~{~A~^ ~}" (getf hint :source))))
          (nth (1- family) *nh-families*)))


(defun nh-same-set-p (expected actual)
  (and (= (length actual) (length (remove-duplicates actual :test #'string=)))
       (null (set-exclusive-or expected actual :test #'string=))))


(defun nh-expected-lines ()
  "The expected NH block from the evidence file: its header line up to the Bases line."
  (let* ((lines (with-open-file (stream (merge-pathnames
                                          "doc/constraint-method/evidence/t21-necessity-hints-2026-09-25.txt"
                                          (asdf:system-source-directory :wouldwork)))
                  (loop for line = (read-line stream nil) while line collect line)))
         (start (member "NH  NECESSITY HINTS  [grade per hint]" lines :test #'string=)))
    (nh-trim-blank-tail (loop for line in start
                              until (and (>= (length line) 6) (string= (subseq line 0 6) "Bases."))
                              collect line))))


(defun nh-check-match ()
  "A3: the generated block equals the expected block line for line."
  (let ((expected (nh-expected-lines))
        (generated (nh-trim-blank-tail
                     (member "NH  NECESSITY HINTS  [grade per hint]"
                             (nh-output #'report-necessity-hints) :test #'string=))))
    (nh-check "expected block read from the evidence file" (> (length expected) 60))
    (loop for e in expected
          for g in generated
          for n from 1
          unless (string= e g)
            do (format t "~&  line ~D~%    expected:  ~A~%    generated: ~A~%" n e g))
    (nh-check (format nil "generated block (~D lines) equals expected (~D lines)"
                      (length generated) (length expected))
              (equal expected generated))))


(defun nh-check-h1 ()
  "A4 H1: T6's printed AM3a and AM3b shortages each yield one H1 row."
  (let* ((lines (nh-output #'report-budget-arithmetic))
         (shortage (lambda (tag)
                     (let ((line (find-if (lambda (line) (search tag line)) lines)))
                       (when line
                         (parse-integer line :start (+ (search "so at least " line) 12)
                                             :junk-allowed t)))))
         (sources (mapcar (lambda (hint) (first (getf hint :source))) (first *nh-families*))))
    (nh-check "H1: AM3a shortage > 0 and exactly one :outside row"
              (and (plusp (funcall shortage "AM3a")) (= 1 (count :outside sources))))
    (nh-check "H1: AM3b shortage > 0 and exactly one :inside row"
              (and (plusp (funcall shortage "AM3b")) (= 1 (count :inside sources))))))


(defun nh-check-h2 ()
  "A4 H2: every S4 direction with an APPROACH-ONLY plate yields exactly one hint."
  (let ((device nil) (from nil) (to nil) (expected nil))
    (dolist (line (nh-output #'report-cut-keeper-table))
      (let ((words (nh-words line)))
        (cond ((and (> (length line) 5) (string= (subseq line 0 4) "    ")
                    (char/= (char line 4) #\Space) (search " == " line))
               (setf device (first words)))
              ((search ", device absent:" line)
               (setf from (first words) to (third words)))
              ((search ": APPROACH-ONLY" line)
               (pushnew (string-downcase (format nil "~A ~A ~A" device from to)) expected
                        :test #'string=)))))
    (nh-check (format nil "H2: ~D approach-only directions = H2 sources" (length expected))
              (and expected (nh-same-set-p expected (nh-source-texts 2))))))


(defun nh-check-h3 ()
  "A4 H3: every device S1's control table drives by a device-mediated primitive yields
 exactly one NECESSARY hint."
  (let* ((lines (nh-output #'report-control-algebra))
         (mediated (loop for line in lines
                         when (search "  device-mediated  status relation" line)
                           collect (first (nh-words line))))
         (expected (loop for line in lines
                         for words = (nh-words line)
                         when (and (search " == " line)
                                   (intersection mediated (cddr words) :test #'string=))
                           collect (first words)))
         (actual (loop for hint in (third *nh-families*)
                       when (string= "NECESSARY" (getf hint :label))
                         collect (string-downcase (format nil "~A" (first (getf hint :source)))))))
    (nh-check (format nil "H3: ~D mediated device(s) = H3 NECESSARY sources" (length expected))
              (and expected (nh-same-set-p expected actual)))))


(defun nh-check-h4 ()
  "A4 H4: every SWITCH controller of a GRAPH-REQUIRED device, as S4 prints them, yields
 exactly one hint (crelay-topo has no on-route switch site, so all appear)."
  (let* ((lines (nh-output #'report-cut-keeper-table))
         (required (let ((line (find-if (lambda (line) (search "GRAPH-REQUIRED candidates" line)) lines)))
                     (subseq (nh-words line) 2 (position "concrete" (nh-words line) :test #'string=))))
         (device nil)
         (expected nil))
    (dolist (line lines)
      (let ((words (nh-words line)))
        (cond ((and (> (length line) 5) (string= (subseq line 0 4) "    ")
                    (char/= (char line 4) #\Space) (search " == " line))
               (setf device (first words)))
              ((and (search "      controller " line) (search ": SWITCH;" line)
                    (member device required :test #'string=))
               (pushnew (second words) expected :test #'string=)))))
    (nh-check (format nil "H4: ~D goal-route switch controller(s) = H4 sources" (length expected))
              (and expected (nh-same-set-p expected (nh-source-texts 4))))))


(defun nh-check-h5 ()
  "A4 H5: every CC G15 FLAG or CHECK row yields exactly one hint."
  (let* ((lines (nh-output #'report-coupling-census))
         (rows (loop for line in lines
                     when (and (search "  lift " line) (search "  barrier " line))
                       collect (list (first (nh-words line)) line)))
         (flagged (count-if (lambda (line) (or (search "G15 FLAG" line) (search "G15 CHECK" line)))
                            lines))
         (actual (mapcar (lambda (text) (subseq text 0 (position #\Space text))) (nh-source-texts 5))))
    (nh-check (format nil "H5: ~D G15 FLAG/CHECK row(s) = H5 hints" flagged)
              (and (plusp flagged) (= flagged (length actual))
                   (nh-same-set-p (remove-duplicates (mapcar #'first rows) :test #'string=)
                                  (remove-duplicates actual :test #'string=))))))


(defun nh-check-h6 ()
  "A4 H6: every controlled device whose state proposition (open or turning) is in the start
 database yields exactly one hint -- S1's device state axiom read from the other side."
  (let* ((devices (mapcar #'third (control-facts)))
         (expected (loop for proposition in (database *start-state*)
                         when (and (member (first proposition) '(open turning))
                                   (member (second proposition) devices))
                           collect (string-downcase (symbol-name (second proposition))))))
    (nh-check (format nil "H6: ~D device(s) active in the start database = H6 sources" (length expected))
              (and expected (nh-same-set-p expected (nh-source-texts 6))))))


(defun nh-check-h7 ()
  "A4 H7: every top S5 prints as unreachable from ground yields exactly one hint."
  (let* ((lines (nh-output #'report-height-and-reach-lattice))
         (expected (remove-duplicates
                     (loop for line in (rest (member-if (lambda (line) (search "unreachable from ground" line))
                                                        lines))
                           while (plusp (length (string-trim " " line)))
                           collect (third (nh-words line)))
                     :test #'string=)))
    (nh-check (format nil "H7: ~D unreachable top(s) = H7 sources" (length expected))
              (and expected (nh-same-set-p expected (nh-source-texts 7))))))


(defun nh-check-negative ()
  "A5: with no ghost layer H1 sequences the demands and has no :inside row; without the
 lift H5 is empty."
  (let* ((controls (control-facts))
         (live (first (budget-arithmetic-segment-occupancy)))
         (actor (second (budget-arithmetic-find-relation-call (get 'goal-fn :form) 'has-location)))
         (no-ghosts (hint-budget-rows controls (list live live) actor))
         (lifts (loop for fact in (list-static-db)
                      when (eq (first fact) 'aimed-at) collect (second fact))))
    (nh-check "staged: H1 has an :inside row" (find :inside (first *nh-families*)
                                                    :key (lambda (hint) (first (getf hint :source)))))
    (nh-check "no ghost layer: H1 says the demands are met in sequence"
              (some (lambda (hint) (search "met in sequence" (first (getf hint :hints)))) no-ghosts))
    (nh-check "no ghost layer: no :inside row"
              (notany (lambda (hint) (eq :inside (first (getf hint :source)))) no-ghosts))
    (nh-check "staged: H5 has one hint" (= 1 (length (fifth *nh-families*))))
    (nh-check "without the lift: H5 is empty"
              (null (hint-lift-rows (remove-if (lambda (fact) (member (third fact) lifts)) controls)
                                    (traversal-arc-facts))))))


(defun nh-check-source ()
  "A6 (C3 and LABELS/FLET): the NH block of the source names no object of the staged
 problem and uses neither LABELS nor FLET."
  (let* ((text (with-open-file (stream (merge-pathnames "tech/constraint-profile.lisp"
                                                        (asdf:system-source-directory :wouldwork)))
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (block (string-downcase
                  (subseq text (search ";;;; NH -- NECESSITY HINTS" text)
                          (search "(defun report-static-constraint-profile" text))))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants))))))
    (nh-check "NH block located" (> (length block) 1000))
    (nh-check "no LABELS or FLET in the NH block"
              (not (or (search "(labels " block) (search "(flet " block))))
    (let ((found (remove-if-not
                   (lambda (object)
                     (let ((name (string-downcase (symbol-name object))))
                       (loop for start = (search name block) then (search name block :start2 (1+ start))
                             while start
                             thereis (and (or (zerop start)
                                              (not (alphanumericp (char block (1- start)))))
                                          (let ((end (+ start (length name))))
                                            (or (= end (length block))
                                                (not (or (alphanumericp (char block end))
                                                         (char= (char block end) #\-)))))))))
                   objects)))
      (format t "~&  staged names found in the NH block: ~(~{~A~^ ~}~)~%" found)
      (nh-check (format nil "no staged object name (~D checked) in the NH block, except the declared substrate constants normal, inverted, ground"
                        (length objects))
                (subsetp found '(normal inverted ground))))))


(format t "~&NH checks, T21~%")
(setf *nh-families* (let ((controls (control-facts)))
                      (necessity-hint-families controls (hint-route-context controls))))
(nh-check-match)
(nh-check-h1)
(nh-check-h2)
(nh-check-h3)
(nh-check-h4)
(nh-check-h5)
(nh-check-h6)
(nh-check-h7)
(nh-check-negative)
(nh-check-source)
(format t "~&NH checks: ~D passed~%" *nh-check-count*)
