;;; FH "from here" report -- acceptance checks, T22, 2026-09-26.
;;; Expected readings: t22-from-here-2026-09-26.txt, part 1 (written before the run).
;;; Run on a fresh staging, with tech/constraint-profile.lisp loaded:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t22-from-here-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; Part one runs on the fresh staging; the file then sets *THREADS* 16 (a restage) and imports
;;; the t10-c3-location15 checkpoint by replay for part two.  No search runs.  A failed check
;;; is reported and counted, and the run continues; the last line gives the totals.

(in-package :ww)


(defparameter *fh-passed* 0)


(defparameter *fh-failed* nil)


(defun fh-check (label passed)
  (if passed
    (progn (incf *fh-passed*)
           (format t "~&  pass  ~A~%" label))
    (progn (push label *fh-failed*)
           (format t "~&  FAIL  ~A~%" label))))


(defun fh-file-lines (relative)
  (with-open-file (stream (merge-pathnames relative (asdf:system-source-directory :wouldwork)))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim '(#\Return) line))))


(defun fh-trim-blank-tail (lines)
  (reverse (member-if (lambda (line) (plusp (length (string-trim " " line)))) (reverse lines))))


(defun fh-output-lines (function)
  (with-input-from-string (stream (with-output-to-string (*standard-output*) (funcall function)))
    (loop for line = (read-line stream nil) while line collect line)))


(defun fh-expected-block (which)
  "Expected block WHICH (0 for the fresh staging, 1 for the checkpoint) from part 1 of the
 evidence file: from its header line to the next block or the Bases line."
  (let* ((lines (fh-file-lines "doc/constraint-method/evidence/t22-from-here-2026-09-26.txt"))
         (starts (loop for tail on lines
                       when (string= (first tail) "FH  FROM HERE  [grade 1; grade 2 where marked]")
                         collect tail)))
    (fh-trim-blank-tail
      (loop for line in (nth which starts)
            for index from 0
            until (and (plusp index)
                       (or (string= line "FH  FROM HERE  [grade 1; grade 2 where marked]")
                           (and (>= (length line) 3) (string= (subseq line 0 3) "(b)"))
                           (and (>= (length line) 6) (string= (subseq line 0 6) "Bases."))))
            collect line))))


(defun fh-check-match (which label source)
  "A3: the generated FH block equals expected block WHICH line for line."
  (let ((expected (fh-expected-block which))
        (generated (fh-trim-blank-tail
                     (member "FH  FROM HERE  [grade 1; grade 2 where marked]"
                             (fh-output-lines (lambda () (report-from-here source)))
                             :test #'string=))))
    (fh-check (format nil "A3 ~A: expected block read (~D lines)" label (length expected))
              (> (length expected) 30))
    (loop for n from 1 to (max (length expected) (length generated))
          for e = (nth (1- n) expected)
          for g = (nth (1- n) generated)
          unless (equal e g)
            do (format t "~&  line ~D~%    expected:  ~A~%    generated: ~A~%" n e g))
    (fh-check (format nil "A3 ~A: generated block (~D lines) equals expected (~D lines)"
                      label (length generated) (length expected))
              (equal expected generated))))


(defun fh-prefix-count (prefix lines)
  (count-if (lambda (line) (and (>= (length line) (length prefix))
                                (string= prefix line :end2 (length prefix))))
            lines))


(defun fh-check-once (label state)
  "A4: each agent in F0, each primitive controller in F1's controllers, each relay in F3's
 relays appears exactly once."
  (let* ((context (from-here-context))
         (sections (from-here-sections state context))
         (controllers (rest (member-if (lambda (line) (search "    controllers (" line)) (second sections))))
         (relays (rest (member-if (lambda (line) (search "    relays (" line)) (fourth sections))))
         (relay-names (append (census-type-instances 'connector) (census-type-instances 'floor-repeater)
                              (census-type-instances 'wall-repeater))))
    (fh-check (format nil "A4 ~A: each of ~D agents once in F0" label (length (getf context :agents)))
              (every (lambda (agent) (= 1 (fh-prefix-count (format nil "      ~(~A~)  " agent) (first sections))))
                     (getf context :agents)))
    (fh-check (format nil "A4 ~A: each of ~D primitive controllers once in F1" label
                      (length (getf context :primitives)))
              (every (lambda (primitive) (= 1 (fh-prefix-count (format nil "      ~(~A~)  " primitive) controllers)))
                     (getf context :primitives)))
    (fh-check (format nil "A4 ~A: each of ~D relays once in F3" label (length relay-names))
              (every (lambda (relay) (= 1 (fh-prefix-count (format nil "      ~(~A~)  " relay) relays)))
                     relay-names))))


(defun fh-check-mobility (label state)
  "A4: for each agent standing on the ground, F1's grounded destinations equal
 MOBILITY-LOCATIONS less its own location."
  (let* ((agents (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name))
         (expansion (from-here-expansion state agents))
         (facts (from-here-facts state))
         (checked 0))
    (dolist (agent agents)
      (let ((location (keeper-fact-value 'has-location agent facts)))
        (when (and location (null (keeper-fact-value 'on agent facts)))
          (incf checked)
          (let ((grounded (loop for move in (getf expansion :moves)
                                when (and (eq (first move) agent) (eq (second (second move)) 'ground)
                                          (not (eq (first (second move)) location)))
                                  collect (first (second move))))
                (queried (remove location (funcall (symbol-function 'mobility-locations) state agent location))))
            (format t "~&  ~(~A~): F1 grounded ~(~{~A~^ ~}~); MOBILITY-LOCATIONS ~(~{~A~^ ~}~)~%"
                    agent (sort (copy-list grounded) #'string< :key #'symbol-name)
                    (sort (copy-list queried) #'string< :key #'symbol-name))
            (fh-check (format nil "A4 ~A: ~(~A~)'s grounded destinations = MOBILITY-LOCATIONS" label agent)
                      (and (= (length grounded) (length (remove-duplicates grounded)))
                           (null (set-exclusive-or grounded queried))))))))
    (fh-check (format nil "A4 ~A: ~D grounded agent(s) checked" label checked) (plusp checked))))


(defun fh-placement-pairs (text)
  "The (location place) pairs of an F2 placement text, as lower-case strings."
  (let ((pairs nil))
    (loop for start = 0 then (+ end 2)
          for end = (or (search "; " text :start2 start) (length text))
          for target = (subseq text start end)
          for open = (position #\( target)
          do (let ((location (string-trim " " (subseq target 0 open))))
               (loop for place-start = (1+ open) then (+ place-end 2)
                     for place-end = (or (search ", " target :start2 place-start) (position #\) target))
                     do (push (format nil "~A ~A" location (subseq target place-start place-end)) pairs)
                     until (= place-end (position #\) target))))
          until (= end (length text)))
    pairs))


(defun fh-check-placements (label state)
  "A4: each held object's F2 row from the current configuration equals the (location place)
 pairs of the one-step successors in which that object leaves the agent's hands."
  (let* ((agents (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name))
         (facts (from-here-facts state))
         (checked 0))
    (dolist (agent agents)
      (let ((held (third (find-if (lambda (fact) (and (eq (first fact) 'holding) (eq (second fact) agent)))
                                  facts))))
        (when (and held (keeper-fact-value 'has-location agent facts))
          (incf checked)
          (let ((row (fh-placement-pairs (from-here-placement-text state agent held
                                                                   (keeper-fact-value 'has-location agent facts))))
                (released (remove-duplicates
                            (loop for child in (from-here-children state)
                                  for child-facts = (from-here-facts child)
                                  when (and (eq agent (find-if (lambda (item) (member item agents))
                                                               (problem-state.instantiations child)))
                                            (not (member (list 'holding agent held) child-facts :test #'equal))
                                            (keeper-fact-value 'has-location held child-facts))
                                    collect (string-downcase
                                              (format nil "~A ~A" (keeper-fact-value 'has-location held child-facts)
                                                      (or (keeper-fact-value 'on held child-facts) 'ground))))
                            :test #'string=)))
            (format t "~&  ~(~A~) holding ~(~A~): F2 ~{~A~^; ~}; released ~{~A~^; ~}~%" agent held row released)
            (fh-check (format nil "A4 ~A: ~(~A~)'s F2 row = placements of the successors releasing ~(~A~)"
                              label agent held)
                      (null (set-exclusive-or row released :test #'string=)))))))
    (when (zerop checked)
      (format t "~&  ~A: no agent holds cargo; the F2 check is vacuous here~%" label))))


(defun fh-check-chain-cover ()
  "A4 (fresh staging): F3's groups cover RC's usable chains exactly."
  (let* ((context (from-here-context))
         (lines (fourth (from-here-sections (copy-problem-state *start-state*) context)))
         (usable (loop for (receiver . chains) in (getf context :chains)
                       sum (count-if (lambda (chain) (member (getf chain :class) '(:bootstrap :latch))) chains)))
         (grouped (loop for line in lines
                        sum (cond ((search "open now (" line)
                                   (parse-integer line :start (+ (search "open now (" line) 10) :junk-allowed t))
                                  ((search "        needs " line)
                                   (parse-integer line :start (+ (search "): " line) 3) :junk-allowed t))
                                  (t 0)))))
    (fh-check (format nil "A4 fresh: F3 groups hold ~D chains = RC's ~D usable" grouped usable)
              (and (plusp usable) (= grouped usable)))))


(defun fh-check-open-gate ()
  "A5 (fresh staging): forcing open the one closed gate of a single-gate group, on a state
 copy with no propagation (as RC and S6 do), moves that group into OPEN NOW."
  (let* ((context (from-here-context))
         (state (copy-problem-state *start-state*))
         (receiver (first (census-type-instances 'receiver)))
         (chains (rest (assoc receiver (getf context :chains))))
         (before (from-here-candidate-lines receiver chains (from-here-facts state) (getf context :controls)))
         (single (find-if (lambda (line)
                            (and (search "        needs " line)
                                 (not (search ", " line :end2 (search " (" line :start2 14)))))
                          before))
         (name (when single (subseq single 14 (search " (" single :start2 14))))
         (count (when single (parse-integer single :start (+ (search "): " single) 3) :junk-allowed t)))
         (gate (find name (census-type-instances 'gate)
                     :key (lambda (object) (string-downcase (symbol-name object))) :test #'equal)))
    (fh-check "A5 fresh: a single-gate candidate group exists" (and gate count))
    (when gate
      (add-proposition (list 'open gate) (problem-state.idb state))
      (invalidate-problem-state-hash state)
      (let ((after (from-here-candidate-lines receiver chains (from-here-facts state) (getf context :controls))))
        (format t "~&  forced open ~(~A~); group of ~D~%~{    ~A~%~}" gate count after)
        (fh-check (format nil "A5 fresh: with ~(~A~) open, OPEN NOW holds its ~D chains" gate count)
                  (find (format nil "        open now (~D):" count) after :test #'string=))
        (fh-check (format nil "A5 fresh: with ~(~A~) open, no group needs ~(~A~) alone" gate gate)
                  (zerop (fh-prefix-count (format nil "        needs ~(~A~) (" gate) after)))))))


(defun fh-move-routes (state agent)
  "AGENT's MOVE successors in STATE as (destination-configuration . gates-on-route)."
  (loop for child in (from-here-children state)
        when (and (eq (problem-state.name child) 'move)
                  (eq (first (problem-state.instantiations child)) agent))
          collect (cons (funcall (symbol-function 'agent-configuration) child agent)
                        (remove-if-not (lambda (item) (member item (census-type-instances 'gate)))
                                       (alexandria:flatten (second (problem-state.instantiations child)))))))


(defun fh-check-close-gate (label state)
  "A5: on a copy of STATE with one gate bit cleared, no propagation, a grounded agent loses
 exactly the destinations whose route crossed that gate."
  (let* ((facts (from-here-facts state))
         (agent (find-if (lambda (agent)
                           (and (keeper-fact-value 'has-location agent facts)
                                (null (keeper-fact-value 'on agent facts))
                                (some #'rest (fh-move-routes state agent))))
                         (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name)))
         (routes (when agent (fh-move-routes state agent)))
         (gate (first (sort (remove-duplicates (loop for route in routes append (copy-list (rest route))))
                            #'string< :key #'symbol-name)))
         (copy (copy-problem-state state)))
    (fh-check (format nil "A5 ~A: a grounded agent with a gated route exists" label) gate)
    (when gate
      (delete-proposition (list 'open gate) (problem-state.idb copy))
      (invalidate-problem-state-hash copy)
      (let* ((removed (loop for route in routes when (member gate (rest route)) collect (first route)))
             (kept (loop for route in routes unless (member gate (rest route)) collect (first route)))
             (after (mapcar #'first (fh-move-routes copy agent))))
        (format t "~&  ~(~A~), ~(~A~) closed: removed ~(~{~A~^, ~}~); now ~(~{~A~^, ~}~)~%"
                agent gate removed after)
        (fh-check (format nil "A5 ~A: closing ~(~A~) removes ~D destination(s) of ~(~A~)"
                          label gate (length removed) agent)
                  (and removed (null (set-exclusive-or after kept :test #'equal))))))))


(defun fh-check-source ()
  "A6 (C3 and LABELS/FLET): the FH block of the source names no object of the staged
 problem and uses neither LABELS nor FLET."
  (let* ((text (with-open-file (stream (merge-pathnames "tech/constraint-profile.lisp"
                                                        (asdf:system-source-directory :wouldwork)))
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (block (string-downcase
                  (subseq text (search ";;;; FH -- FROM HERE" text)
                          (search "(defun report-static-constraint-profile" text))))
         (objects (remove-duplicates
                    (loop for constants being the hash-values of *types*
                          append (remove nil (copy-list constants))))))
    (fh-check "A6: FH block located" (> (length block) 1000))
    (fh-check "A6: no LABELS or FLET in the FH block"
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
                                                         (char= (char block end) #\-)
                                                         (char= (char block end) #\*)))))))))
                   objects)))
      (format t "~&  staged names found in the FH block: ~(~{~A~^ ~}~)~%" found)
      (fh-check (format nil "A6: no staged object name (~D checked) in the FH block, except the declared substrate constant ground"
                        (length objects))
                (subsetp found '(ground))))))


(defun fh-check-profile ()
  "A7: the regenerated profile equals the committed profile file, line for line."
  (let ((file (fh-file-lines "doc/problems/crelay-topo/Constraint-Static-Profile.txt"))
        (generated (fh-output-lines #'report-static-constraint-profile)))
    (fh-check (format nil "A7: regenerated profile (~D lines) equals the file (~D lines)"
                      (length generated) (length file))
              (equal (fh-trim-blank-tail generated) (fh-trim-blank-tail file)))))


(format t "~&FH checks, T22 -- part one, fresh staging~%")
(fh-check-profile)
(fh-check-match 0 "fresh" nil)
(fh-check-once "fresh" (copy-problem-state *start-state*))
(fh-check-mobility "fresh" (copy-problem-state *start-state*))
(fh-check-placements "fresh" (copy-problem-state *start-state*))
(fh-check-chain-cover)
(fh-check-open-gate)
(fh-check-source)
(format t "~&FH checks, T22 -- part two, t10-c3-location15 checkpoint~%")
(ww-set *threads* 16)
(defparameter *fh-checkpoint*
  (import-search-checkpoint
    (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-c3-location15-checkpoint.txt"
                     (asdf:system-source-directory :wouldwork))))
(fh-check-match 1 "checkpoint" *fh-checkpoint*)
(fh-check-once "checkpoint" (search-checkpoint-state *fh-checkpoint*))
(fh-check-mobility "checkpoint" (search-checkpoint-state *fh-checkpoint*))
(fh-check-placements "checkpoint" (search-checkpoint-state *fh-checkpoint*))
(fh-check-close-gate "checkpoint" (search-checkpoint-state *fh-checkpoint*))
(format t "~&FH checks: ~D passed, ~D failed~%~{  failed: ~A~%~}"
        *fh-passed* (length *fh-failed*) (reverse *fh-failed*))
