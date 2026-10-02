;;; MC mechanic contracts for jump and recorder, entry for step -- acceptance checks, T28, 2026-09-26.
;;; Expected readings: t28-mechanic-contracts-2026-09-26.txt, part 1 (written before the run).
;;; Run after staging crelay-topo and loading tech/constraint-profile.lisp, BEFORE writing
;;; the regenerated profile over the baseline (A7 compares against the file on disk):
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t28-mechanic-contracts-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; No search, propagation or replay.  Errors on the first failed check.

(in-package :ww)


(defparameter *t28-check-count* 0)


(defparameter *t28-baseline-sha256*
  "1b7d22e27c70df552dbf6e33478102795898a92e9b7d0cea0596d0724418cf17")


(defparameter *t28-expected-jump-block*
  '("    reach limit 1 (*vertical-reach-limit*); a raise is the least launch elevation above the source floor, reached by standing on a support (S5 tops)"
    "    location20 -> location5  clause ()  level 3/2 -> 0"
    "      no feature  from the floor"
    "    location5 -> location20  clause ()  level 0 -> 3/2"
    "      no feature  raise 1/2 above the floor"
    "    location20 -> location6  clause (gate2)  level 3/2 -> 3/2"
    "      gate2 open  from the floor"
    "      gate2 closed  raise 3 above the floor"
    "    location6 -> location20  clause (gate2)  level 3/2 -> 3/2"
    "      gate2 open  from the floor"
    "      gate2 closed  raise 3 above the floor"
    "    location4 -> location6  clause (gate2)  level 0 -> 3/2"
    "      gate2 open  raise 1/2 above the floor"
    "      gate2 closed  raise 9/2 above the floor"
    "    location6 -> location4  clause (gate2)  level 3/2 -> 0"
    "      gate2 open  from the floor"
    "      gate2 closed  raise 3 above the floor"
    "    location5 -> location6  clause (gate2)  level 0 -> 3/2"
    "      gate2 open  raise 1/2 above the floor"
    "      gate2 closed  raise 9/2 above the floor"
    "    location6 -> location5  clause (gate2)  level 3/2 -> 0"
    "      gate2 open  from the floor"
    "      gate2 closed  raise 3 above the floor"))


(defparameter *t28-expected-recorder-block*
  '("    cycles allowed  unlimited (*max-recorder-cycles*)"
    "    live -> ghost (4): agent1 -> agent1*, box1 -> box1*, connector1 -> connector1*, tray1 -> tray1*"
    "    recorder1  at location1"))


(defparameter *t28-new-functions*
  '("jump-reading-raise" "report-jump-reading" "report-jump-clause" "report-jump-direction"
    "report-jump-instances" "report-recorder-instances"))


(defun t28-check (label passed)
  (incf *t28-check-count*)
  (unless passed
    (error "T28 check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun t28-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil) while line collect line)))


(defun t28-mc-lines ()
  (t28-lines (with-output-to-string (*standard-output*)
               (report-mechanic-coverage))))


(defun t28-file-text (relative)
  (with-open-file (stream (merge-pathnames relative (asdf:system-source-directory :wouldwork))
                          :external-format :utf-8)
    (let ((string (make-string (file-length stream))))
      (subseq string 0 (read-sequence string stream)))))


(defun t28-contract-block (lines tech)
  "The lines of TECH's contract block after its three text lines, up to the next blank line."
  (let ((start (position (format nil "  contract ~A" tech) lines :test #'string=)))
    (when start
      (loop for line in (nthcdr (+ start 4) lines)
            until (string= line "")
            collect line))))


(defun t28-check-domain ()
  "A4: the 16 public spliced technologies each have exactly one verdict row, none UNCOVERED."
  (let ((expected '("beam-relay" "box" "elevation" "floor-blower" "gate" "jump" "ladder"
                    "plate" "reachability" "recorder" "step" "switch" "topo-lower-bound"
                    "tray" "visibility" "walkability"))
        (lines (t28-mc-lines)))
    (t28-check "public spliced technologies = the 16 expected names"
               (equal (mechanic-public-techs) expected))
    (t28-check "each has exactly one COVERED row"
               (every (lambda (tech)
                        (= 1 (count-if (lambda (line)
                                         (and (> (length line) 22)
                                              (string= (subseq line 0 22)
                                                       (format nil "    ~18A" tech))
                                              (search "COVERED" line)
                                              (not (search "UNCOVERED" line))))
                                       lines)))
                      expected))
    (t28-check "count line: 16 covered, 0 UNCOVERED"
               (member "  public technologies spliced (16): 16 covered, 0 UNCOVERED" lines
                       :test #'string=))
    (t28-check "UNCOVERED line is empty"
               (member "  UNCOVERED (0):" lines :test #'string=))))


(defun t28-check-rows ()
  "A3: the verdict lines, contract order, and the jump and recorder blocks equal part 1."
  (let ((lines (t28-mc-lines)))
    (dolist (line '("    jump              COVERED    contract (also S3 S5)"
                    "    recorder          COVERED    contract (also S2 RO CP)"
                    "    step              COVERED    extractors S2 T6; blower mounting in the floor-blower contract; gears-mounted fans have no component"))
      (t28-check (format nil "verdict: ~A" (string-trim " " line))
                 (member line lines :test #'string=)))
    (t28-check "contracts in order floor-blower, jump, ladder, recorder"
               (equal (loop for line in lines
                            when (and (> (length line) 11) (string= (subseq line 0 11) "  contract "))
                              collect (subseq line 11))
                      '("floor-blower" "jump" "ladder" "recorder")))
    (t28-check "jump block equals part 1.3"
               (equal (t28-contract-block lines "jump") *t28-expected-jump-block*))
    (t28-check "recorder block equals part 1.4"
               (equal (t28-contract-block lines "recorder") *t28-expected-recorder-block*))))


(defun t28-check-negative ()
  "A5: without its registry entry, recorder is UNCOVERED; with the reach limit at 5, the
 location4 -> location6 open reading needs no raise."
  (let ((lines (let ((*mechanic-contracts* (remove "recorder" *mechanic-contracts*
                                                   :key #'first :test #'string=)))
                 (t28-mc-lines))))
    (t28-check "negative: recorder row reads UNCOVERED"
               (member (format nil "    ~18AUNCOVERED" "recorder") lines :test #'string=))
    (t28-check "negative: UNCOVERED (1): recorder"
               (member "  UNCOVERED (1): recorder" lines :test #'string=))
    (t28-check "negative: no recorder contract printed"
               (not (member "  contract recorder" lines :test #'string=))))
  (let* ((lines (let ((*vertical-reach-limit* 5))
                  (t28-mc-lines)))
         (block (t28-contract-block lines "jump"))
         (row (position "    location4 -> location6  clause (gate2)  level 0 -> 3/2" block
                        :test #'string=)))
    (t28-check "negative: reach limit 5, location4 -> location6 open from the floor"
               (and row (string= (nth (1+ row) block) "      gate2 open  from the floor")))))


(defun t28-problem-object-names ()
  "Every instance name in the problem file's own DEFINE-TYPES, lowercased (as in T19)."
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


(defun t28-whole-word-hits (names text)
  (let ((hits nil))
    (dolist (name names hits)
      (loop for position = (search name text) then (search name text :start2 (1+ position))
            while position
            do (let ((before (if (plusp position) (char text (1- position)) #\Space))
                     (after (if (< (+ position (length name)) (length text))
                              (char text (+ position (length name)))
                              #\Space)))
                 (unless (or (alphanumericp before) (find before "-*")
                             (alphanumericp after) (find after "-*"))
                   (pushnew name hits :test #'string=)))))))


(defun t28-check-code ()
  "A6: C3 over the registry and the new functions; callees-first; no LABELS or FLET; no
 blank line inside a new definition."
  (let* ((text (t28-file-text "tech/constraint-profile.lisp"))
         (start (search "(defparameter *mechanic-contracts*" text))
         (end (search "(defun report-mechanic-contract" text :start2 start))
         (region (string-downcase (subseq text start end)))
         (code-start (search "(defun jump-reading-raise" text))
         (code (subseq text code-start end))
         (names (t28-problem-object-names))
         (hits (t28-whole-word-hits names region)))
    (t28-check (format nil "C3: none of ~D problem object names in the registry or new code~@[ (found ~{~A~^, ~})~]"
                       (length names) hits)
               (null hits))
    (t28-check "no LABELS or FLET in the new code"
               (not (or (search "(labels" code) (search "(flet" code))))
    (t28-check "no blank line inside a new definition"
               (every (lambda (definition)
                        (not (search (format nil "~%~%") (string-trim '(#\Newline) definition))))
                      (loop for position = 0 then next
                            for next = (search (format nil "~%~%~%(defun") code :start2 (1+ position))
                            collect (subseq code position (or next (length code)))
                            while next)))
    (t28-check "callees-first: each new function is defined before any call to it"
               (every (lambda (name)
                        (let ((definition (search (format nil "(defun ~A " name) text))
                              (call (search (format nil "(~A " name) text)))
                          (and definition call (< definition call))))
                      (butlast *t28-new-functions* 2)))
    (t28-check "the two reporters are defined before REPORT-MECHANIC-CONTRACT funcalls them"
               (every (lambda (name) (< (search (format nil "(defun ~A " name) text) end))
                      (last *t28-new-functions* 2)))))


(defun t28-strip-mc (lines)
  (let ((start (position "MC  MECHANIC COVERAGE  [grade 1]" lines :test #'string=))
        (end (position "S0  TYPE EXTENT CENSUS  [grade 1]" lines :test #'string=)))
    (append (subseq lines 0 start) (subseq lines end))))


(defun t28-check-profile ()
  "A7: the regenerated profile minus MC equals the baseline file minus MC.  The baseline is
 the file on disk, which must still be the pre-T28 profile."
  (let* ((relative "doc/problems/crelay-topo/Constraint-Static-Profile.txt")
         (temporary (merge-pathnames "doc/problems/crelay-topo/t28-profile-check.txt"
                                     (asdf:system-source-directory :wouldwork)))
         (baseline (t28-lines (t28-file-text relative))))
    (write-static-constraint-profile temporary)
    (let ((fresh (t28-lines (t28-file-text "doc/problems/crelay-topo/t28-profile-check.txt"))))
      (delete-file temporary)
      (format t "~&  baseline ~D lines, regenerated ~D lines~%" (length baseline) (length fresh))
      (t28-check "baseline is the pre-T28 profile (1526 lines; SHA-256 recorded in part 1)"
                 (= (length baseline) 1526))
      (t28-check "every section but MC is byte-identical"
                 (equal (t28-strip-mc baseline) (t28-strip-mc fresh))))))


(format t "~&T28 acceptance checks~%")
(t28-check-domain)
(t28-check-rows)
(t28-check-negative)
(t28-check-code)
(t28-check-profile)
(format t "~&  ~D checks passed~%" *t28-check-count*)
