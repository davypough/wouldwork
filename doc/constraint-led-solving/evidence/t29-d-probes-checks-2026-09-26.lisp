;;; P5, D's own probes -- acceptance checks, T29, 2026-09-26.
;;; Expected readings: t29-d-probes-2026-09-26.txt, part 1 (written before the run).
;;; Run after staging crelay-topo at threads 16 and loading both diagnostics:
;;;   (stage crelay-topo)
;;;   (ww-set *threads* 16)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "tech/constraint-probe-battery.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/t29-d-probes-checks-2026-09-26.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; Runs two small searches (maximum depth 4) and writes their results beside this file.
;;; Errors on the first failed check.

(in-package :ww)


(defparameter *t29-check-count* 0)


(defparameter *t29-entries*
  '((agent1 (has-location agent1 location1) "start check")
    (agent1 (and (has-location agent1 location2) (depressed plate1)) "T23 P3.1, with agent1's position")))


(defparameter *t29-expected-probes*
  '((:id "P5.1" :family 5 :subject agent1 :goal (has-location agent1 location1)
     :provenance "start check")
    (:id "P5.2" :family 5 :subject agent1
     :goal (and (has-location agent1 location2) (depressed plate1))
     :provenance "T23 P3.1, with agent1's position")))


(defparameter *t29-expected-plan*
  '((1.0 (move agent1 ((walk location1 nil location2))))
    (2.0 (move agent1 ((step (location2 ground) nil (location2 plate1)))))))


(defparameter *t29-new-functions*
  '("d-probe-goal-heads" "d-probe-check-entry" "d-probes"))


(defun t29-path (relative)
  (merge-pathnames relative (asdf:system-source-directory :wouldwork)))


(defun t29-check (label passed)
  (incf *t29-check-count*)
  (unless passed
    (error "T29 check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun t29-lines (thunk)
  (with-input-from-string (stream (with-output-to-string (*standard-output*) (funcall thunk)))
    (loop for line = (read-line stream nil) while line collect line)))


(defun t29-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))


(defun t29-check-construction ()
  "A3: D-PROBES returns part 1.2; P1-P4 equal T23's recorded probes."
  (t29-check "D-PROBES returns the probes of part 1.2"
             (equal (d-probes *t29-entries*) *t29-expected-probes*))
  (let ((recorded (getf (read-probe-battery-results
                          (t29-path "doc/constraint-method/evidence/t23-probe-battery-results-2026-09-26.lisp"))
                        :probes))
        (key (lambda (probe) (list (getf probe :id) (getf probe :family)
                                   (getf probe :subject) (getf probe :goal)))))
    (t29-check (format nil "P1-P4 equal T23's ~D recorded probes" (length recorded))
               (equal (mapcar key (probe-battery-probes)) (mapcar key recorded)))))


(defun t29-check-run ()
  "A3: the two P5 probes run alone at maximum depth 4 give part 1.3's labels and plan, and
 the report prints part 1.4's rows."
  (let ((path (t29-path "doc/constraint-method/evidence/t29-d-probes-results-2026-09-26.lisp")))
    (run-probe-battery 4 1000000 path (d-probes *t29-entries*))
    (let* ((records (getf (read-probe-battery-results path) :probes))
           (start (first records))
           (cheap (second records))
           (runs (getf cheap :runs))
           (last-run (car (last runs)))
           (lines (t29-lines (lambda () (report-probe-battery path)))))
      (t29-check "P5.1 is START with no runs"
                 (and (eq (getf start :label) :start) (null (getf start :runs))))
      (t29-check "P5.2 is CHEAP, cutoff 1 without a plan, found at cutoff 2"
                 (and (eq (getf cheap :label) :cheap)
                      (= (length runs) 2)
                      (not (getf (first runs) :found))
                      (= (getf last-run :cutoff) 2)))
      (t29-check "P5.2's plan equals part 1.3"
                 (equal (getf last-run :plan) *t29-expected-plan*))
      (t29-check "report: P5 block header"
                 (member "  P5 D's own (2)" lines :test #'string=))
      (t29-check "report: P5.1 row"
                 (member "    P5.1  agent1  (has-location agent1 location1)  <start check>  START  [grade 1]"
                         lines :test #'string=))
      (t29-check "report: P5.2 row"
                 (member (format nil "    P5.2  agent1  (and (has-location agent1 location2) (depressed plate1))  <T23 P3.1, with agent1's position>  CHEAP 2 @ 2  [grade 1 on replay]; states ~:D at the last cutoff"
                                 (getf last-run :states))
                         lines :test #'string=))
      (t29-check "report: P5.2 plan lines"
                 (and (member "      (1.0 (move agent1 ((walk location1 nil location2))))" lines :test #'string=)
                      (member "      (2.0 (move agent1 ((step (location2 ground) nil (location2 plate1)))))" lines :test #'string=))))))


(defun t29-check-negative ()
  "A4: a malformed entry and an undeclared relation signal before any search; an empty
 list gives no probes and an empty P5 block."
  (t29-check "negative: entry without a goal signals"
             (t29-signals-p (lambda () (d-probes '((agent1))))))
  (t29-check "negative: undeclared relation signals from D-PROBES"
             (t29-signals-p (lambda () (d-probes '((agent1 (no-such-relation-t29 agent1) "negative"))))))
  (t29-check "negative: empty entry list gives no probes"
             (null (d-probes nil)))
  (let ((lines (t29-lines (lambda () (report-probe-battery-list)))))
    (t29-check "negative: default list prints P5 D's own (0), then none"
               (let ((tail (member "  P5 D's own (0)" lines :test #'string=)))
                 (and tail (string= (second tail) "    none"))))))


(defun t29-problem-object-names ()
  "Every instance name in the problem file's own DEFINE-TYPES, lowercased (as in T19)."
  (let ((names nil))
    (with-open-file (stream (t29-path "probs/problem-crelay-topo.lisp"))
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


(defun t29-check-code ()
  "A5: C3 over the P5 block; no LABELS or FLET; no blank line inside a definition;
 callees-first."
  (let* ((text (with-open-file (stream (t29-path "tech/constraint-probe-battery.lisp")
                                       :external-format :utf-8)
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (start (search ";;;; P5 -- D's own probes" text))
         (end (search ";;;; Runner (spec 12.3, 12.4)" text))
         (region (string-downcase (subseq text start end)))
         (hits (loop for name in (t29-problem-object-names)
                     when (loop for position = (search name region)
                                  then (search name region :start2 (1+ position))
                                while position
                                thereis (let ((before (if (plusp position) (char region (1- position)) #\Space))
                                              (after (if (< (+ position (length name)) (length region))
                                                       (char region (+ position (length name)))
                                                       #\Space)))
                                          (not (or (alphanumericp before) (find before "-*")
                                                   (alphanumericp after) (find after "-*")))))
                       collect name)))
    (t29-check (format nil "C3: no problem object name in the P5 block~@[ (found ~{~A~^, ~})~]" hits)
               (null hits))
    (t29-check "no LABELS or FLET in the P5 block"
               (not (or (search "(labels" region) (search "(flet" region))))
    (t29-check "no blank line inside a P5 definition"
               (every (lambda (chunk)
                        (not (search (format nil "~%~%") (string-trim '(#\Newline) chunk))))
                      (loop for position = 0 then next
                            for next = (search (format nil "~%~%~%(") region :start2 (1+ position))
                            collect (subseq region position (or next (length region)))
                            while next)))
    (t29-check "callees-first: each new function is defined before any call to it"
               (every (lambda (name)
                        (let ((definition (search (format nil "(defun ~A " name) text))
                              (call (search (format nil "(~A " name) text :start2 start)))
                          (and definition call (< definition call))))
                      *t29-new-functions*))))


(format t "~&T29 acceptance checks~%")
(t29-check-construction)
(t29-check-negative)
(t29-check-code)
(t29-check-run)
(format t "~&  ~D checks passed~%" *t29-check-count*)
