;;; T44 focused checks; stage the problem and load tech/constraint-profile.lisp first.
;;; claustro-topo: (t44-run-claustro-checks).  phobia-topo: (t44-run-phobia-checks).
;;; Any problem: (t44-run-general-checks).  No search, no replay, no state evaluation.
;;; T24's crelay-topo checks are the no-reservation regression (evidence file t24-*).
(in-package :ww)


(defvar *t44-count* 0)


(defun t44-check (label value)
  (assert value () "T44 failed: ~A" label)
  (incf *t44-count*)
  (format t "~&PASS ~A~%" label))


(defun t44-report (plan)
  (let ((before (copy-tree plan)))
    (prog1 (with-output-to-string (*standard-output*) (report-cycle-plan-check plan))
      (t44-check (list "caller plan unchanged" (getf plan :name)) (equal before plan)))))


(defun t44-lines (text prefix)
  "TEXT's lines containing PREFIX."
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil)
          while line
          when (search prefix line) collect line)))


(defun t44-has (text &rest fragments)
  (every (lambda (fragment) (search fragment text)) fragments))


(defun t44-one-segment (name reservations segment)
  (list :name name :provenance "T44 check"
        :stages (list (list :id "s" :intent "check" :reservations reservations
                            :segments (list segment)))))


(defun t44-final-segment (available)
  (list :id "final" :view :physical :cycle :none :ghosts :absent
        :available-witnesses available :require '(gate8 gate9)))


(defun t44-claustro-a1 ()
  (let ((text (t44-report
                (t44-one-segment "A1 final phase"
                                 '((:body box2 :role (:place location10) :purpose "jump support")
                                   (:body jammer1 :role (:jam gate5))
                                   (:body jammer2 :role (:jam gate1)))
                                 (t44-final-segment '(box1))))))
    (t44-check "A1 plan PASS" (search "  plan PASS" text))
    (t44-check "A1 shared plates counted once: three B1 rows" (= 3 (length (t44-lines text "      B1  "))))
    (t44-check "A1 box2 reserved off plates" (t44-has text "pool: box2 reserved off plates"))
    (t44-check "A1 jammer1 eligible for all three plates"
               (t44-has text "pool: jammer1 eligible for plate1" "plate2 (jam gate5" "plate3 (jam gate5"))
    (t44-check "A1 jammer2 eligibility carries the gate2/gate3 premise"
               (t44-has text "pool: jammer2 eligible for plate1 (jam gate1 sighted from location4 on plate1 if gate2, gate3 open)"))
    (t44-check "A1 free witnesses box1" (t44-has text "pool: free witnesses box1")))
  (let ((text (t44-report
                (t44-one-segment "A1 pinned end arrangement"
                                 '((:body box1 :role (:weight plate1))
                                   (:body jammer1 :role (:weight plate2))
                                   (:body jammer1 :role (:jam gate5))
                                   (:body jammer2 :role (:weight plate3))
                                   (:body jammer2 :role (:jam gate1))
                                   (:body box2 :role (:place location10) :purpose "jump support"))
                                 (t44-final-segment nil)))))
    (t44-check "A1 pinned plan PASS" (search "  plan PASS" text))
    (t44-check "A1 pinned holders" (t44-has text "plate1 for gate8: held by box1" "plate2 for gate8: held by jammer1"
                                            "plate3 for gate8: held by jammer2"))
    (t44-check "A1 jammer1 SHARED, sighted from its plate"
               (t44-has text "jammer1: weight plate2 [final..final]; jam gate5 [final..final] -- SHARED; jam gate5 sighted from location5 on plate2"))
    (t44-check "A1 jammer2 SHARED, premise gate2/gate3"
               (t44-has text "jammer2: weight plate3 [final..final]; jam gate1 [final..final] -- SHARED; jam gate1 sighted from location6 on plate3 if gate2, gate3 open"))
    (t44-check "A1 pinned: no unpinned plate" (t44-has text "pool: no unpinned plate required"))))


(defun t44-conflict (label reservations available expected)
  (let ((text (t44-report (t44-one-segment label reservations (t44-final-segment available)))))
    (t44-check (list label "CONFLICT" expected) (and (search "  plan CONFLICT" text) (search expected text)))
    text))


(defun t44-context-without-site (jammer target location plate)
  "A CP context whose cached MC survey lacks JAMMER's site at LOCATION on PLATE for TARGET:
   claustro-topo has no unsighted plate site, so the refuted branch is exercised this way."
  (let* ((context (cycle-plan-context))
         (rows (jammer-sightline-rows)))
    (setf (gethash :jam-rows (getf context :cache))
          (mapcar (lambda (row)
                    (if (and (eq (getf row :jammer) jammer) (eq (getf row :target) target))
                      (list :jammer jammer :target target
                            :sites (remove-if (lambda (site) (and (eq (first site) location) (eq (second site) plate)))
                                              (getf row :sites)))
                      row))
                  rows))
    context))


(defun t44-claustro-a2 ()
  (t44-conflict "A2 held and jamming" '((:body jammer1 :role (:hold agent1)) (:body jammer1 :role (:jam gate5)))
                '(box1 box2 jammer2) "C1 held and stationary at once")
  (t44-conflict "A2 two jam targets" '((:body jammer1 :role (:jam gate5)) (:body jammer1 :role (:jam gate1)))
                '(box1 box2 jammer2) "C3 jam names gate5, gate1; its relation holds one")
  (t44-conflict "A2 step weighting a plate" '((:body box2 :role (:place location10)) (:body box2 :role (:weight plate1)))
                '(box1 jammer1 jammer2) "C2 locations differ: location10, location4")
  (t44-conflict "A2 two bodies on one plate" '((:body box1 :role (:weight plate1)) (:body jammer1 :role (:weight plate1)))
                '(box2 jammer2) "K1 box1 and jammer1 contend for plate1")
  (t44-conflict "A2 a box asked to jam" '((:body box1 :role (:jam gate1)))
                '(box2 jammer1 jammer2) "C7 jam gate1: not a jammer")
  (let* ((context (t44-context-without-site 'jammer1 'gate1 'location4 'plate1))
         (segment (t44-final-segment nil))
         (results (cycle-plan-jam-results 'jammer1 '((:weight plate1) (:jam gate1)) segment context)))
    (t44-check "A2 unsighted plate site: CONFLICT"
               (equal results '(("CONFLICT" "jam gate1: no sightline from location4 on plate1 in MC's survey"))))
    (t44-check "A2 unsighted plate site: jammer not eligible for that plate"
               (not (cycle-plan-plate-eligibility 'jammer1 '((:jam gate1)) 'plate1 segment context)))
    (t44-check "A2 still eligible for a sighted plate"
               (cycle-plan-plate-eligibility 'jammer1 '((:jam gate1)) 'plate2 segment context)))
  (t44-check "A2 no surveyed site at a location: CONDITIONAL"
             (equal (cycle-plan-jam-results 'jammer1 '((:jam gate1 location11)) (t44-final-segment nil)
                                            (cycle-plan-context))
                    '(("CONDITIONAL" "jam gate1: no surveyed site at location11 sees it; moved supports are not surveyed"))))
  (let ((text (t44-conflict "A2 box1 reserved elsewhere: shortage"
                            '((:body box1 :role (:place location3)) (:body box2 :role (:place location10))
                              (:body jammer1 :role (:jam gate5)) (:body jammer2 :role (:jam gate1)))
                            nil "for the supplied reservations, refutes this allocation only")))
    (t44-check "A2 shortage names both jammers as the only witnesses"
               (search "against 2 witnesses (jammer1, jammer2)" text))
    (t44-check "A2 box1 reserved off plates" (search "pool: box1 reserved off plates" text))))


(defun t44-phase-plan (s2-available)
  (list :name "A3 phases" :provenance "T44 check"
        :stages (list (list :id "p" :intent "release"
                            :reservations '((:body jammer1 :role (:jam gate1 location1) :through "s1")
                                            (:body jammer2 :role (:jam gate5) :from "s2"))
                            :segments (list (list :id "s1" :view :physical :cycle :none :ghosts :absent
                                                  :available-witnesses '(box1 box2) :require nil)
                                            (list :id "s2" :view :physical :cycle :none :ghosts :absent
                                                  :available-witnesses s2-available :require '(gate8 gate9)))))))


(defun t44-claustro-a3 ()
  (let ((text (t44-report (t44-phase-plan '(box1 box2)))))
    (t44-check "A3 s1 jam from location1 ground" (t44-has text "jammer1: jam gate1 at location1 [s1..s1]; jam gate1 sighted from location1 on ground"))
    (t44-check "A3 release row, not stated available"
               (t44-has text "released after s1: jammer1 jam gate1 at location1 [s1..s1]; not stated available here"))
    (t44-check "A3 released body is not a free witness" (t44-has text "pool: free witnesses box1, box2"))
    (t44-check "A3 s2 matched with jammer2" (t44-has text "pool: jammer2 eligible for plate1"))
    (t44-check "A3 s1 has no required plate" (t44-has text "pool: no unpinned plate required")))
  (let ((text (t44-report (t44-phase-plan '(box1 box2 jammer1)))))
    (t44-check "A3 release row, listed available" (t44-has text "; listed available here"))
    (t44-check "A3 released body free only when listed" (t44-has text "pool: free witnesses box1, box2, jammer1")))
  (t44-check "A3 unknown phase signals"
             (handler-case (progn (report-cycle-plan-check
                                    (t44-one-segment "bad" '((:body box1 :role (:weight plate1) :from "nope"))
                                                     (t44-final-segment nil)))
                                  nil)
               (error () t)))
  (t44-check "A3 unknown role signals"
             (handler-case (progn (report-cycle-plan-check
                                    (t44-one-segment "bad" '((:body box1 :role (:sit plate1)))
                                                     (t44-final-segment nil)))
                                  nil)
               (error () t))))


(defun t44-claustro-recording ()
  (let ((results (cycle-plan-jam-results 'jammer1 '((:weight plate2) (:jam gate5)) '(:view :recording)
                                         (cycle-plan-context))))
    (t44-check "recording view jam sightline CONDITIONAL"
               (and (= 1 (length results)) (string= "CONDITIONAL" (first (first results)))))))


(defun t44-source-checks ()
  (let* ((text (with-open-file (in (merge-pathnames "tech/constraint-profile.lisp"
                                                    (asdf:system-source-directory :wouldwork)))
                 (let ((string (make-string (file-length in))))
                   (subseq string 0 (read-sequence string in)))))
         (start (search "(defun cycle-plan-plate-holders" text))
         (end (search "(defun report-cycle-plan-row" text))
         (code (string-downcase (subseq text start end))))
    (dolist (name '("claustro" "phobia" "crelay" "gate1" "plate1" "jammer1" "box1" "location1"
                    "agent1" "fan1" "gears1"))
      (t44-check (list "no problem name in CP code" name) (not (search name code))))
    (t44-check "no LABELS or FLET in CP code" (not (or (search "(labels " code) (search "(flet " code))))))


(defun t44-capacity (commitments reserved ghosts)
  (mapcar #'third (cycle-plan-capacity-rows commitments reserved (list :ghosts ghosts)
                                            (list :pairs '((livebox . ghostbox))))))


(defun t44-capacity-checks ()
  (t44-check "K1 occupant on two supports"
             (member "K1 x is on s1, s2; ON holds one support"
                     (t44-capacity '((x ((:weight s1) "t") ((:weight s2) "t"))) nil :absent) :test #'string=))
  (t44-check "K1 support occupant also on a plate"
             (member "K1 a is on b, p; ON holds one support"
                     (t44-capacity '((a ((:weight p) "t")) (b ((:support a) "t"))) nil :absent) :test #'string=))
  (t44-check "K1 live and ghost share a plate"
             (equal '("supports, holders and gears not overcommitted")
                    (t44-capacity '((ghostbox ((:weight p) "t")) (livebox ((:weight p) "t"))) nil :present)))
  (t44-check "K1 two live contend"
             (member "K1 livebox and y contend for p"
                     (t44-capacity '((livebox ((:weight p) "t")) (y ((:weight p) "t"))) nil :absent) :test #'string=))
  (t44-check "K2 agent holds two"
             (member "K2 ag holds a, b; HOLDING is bijective"
                     (t44-capacity '((a ((:hold ag) "t")) (b ((:hold ag) "t"))) nil :absent) :test #'string=))
  (t44-check "K3 gears carry two fans"
             (member "K3 g carries f1, f2; MOUNT-FAN needs vacant gears"
                     (t44-capacity '((f1 ((:mount g) "t")) (f2 ((:mount g) "t"))) nil :absent) :test #'string=))
  (t44-check "K4 reserved ghost while absent"
             (member "K4 ghostbox is reserved but ghosts are absent"
                     (t44-capacity '((ghostbox ((:place l) "t"))) '(ghostbox) :absent) :test #'string=)))


(defun t44-run-general-checks ()
  "K1-K4 on constructed commitments (capacity reads no types), and a no-reservation plan
   printing no B5 row."
  (t44-capacity-checks)
  (let ((text (with-output-to-string (*standard-output*)
                (report-cycle-plan-check
                  (list :name "plain" :provenance "T44 check"
                        :stages (list (list :id "s" :intent "none"
                                            :segments (list (list :id "a" :view :physical :cycle :none
                                                                  :ghosts :absent :available-witnesses nil
                                                                  :require nil)))))))))
    (t44-check (list "no reservation, no B5 row" *problem-name*) (not (search "B5" text)))))


(defun t44-run-claustro-checks ()
  (setf *t44-count* 0)
  (t44-claustro-a1)
  (t44-claustro-a2)
  (t44-claustro-a3)
  (t44-claustro-recording)
  (t44-source-checks)
  (t44-run-general-checks)
  (format t "~&T44 CLAUSTRO CHECKS PASSED: ~D~%" *t44-count*))


(defun t44-phobia-segment ()
  (list :id "lift" :view :physical :cycle :none :ghosts :absent :available-witnesses nil :require nil))


(defun t44-run-phobia-checks ()
  (setf *t44-count* 0)
  (let ((text (t44-report (t44-one-segment "A4 floor mount supports the agent"
                                           '((:body fan1 :role (:mount fgears1))
                                             (:body fan1 :role (:support agent1) :purpose "boarding"))
                                           (t44-phobia-segment)))))
    (t44-check "A4 floor mount and support SHARED, PASS"
               (and (search "  plan PASS" text)
                    (search "fan1: mounted on fgears1 [lift..lift]; supports agent1 [boarding; lift..lift] -- SHARED" text))))
  (let ((text (t44-report (t44-one-segment "A4 wall mount cannot support"
                                           '((:body fan1 :role (:mount wgears1))
                                             (:body fan1 :role (:support agent1)))
                                           (t44-phobia-segment)))))
    (t44-check "A4 wall mount with support: C5" (and (search "  plan CONFLICT" text)
                                                     (search "C5 a wall-mounted fan supports nothing" text))))
  (let ((text (t44-report (t44-one-segment "A4 held fan mounted"
                                           '((:body fan1 :role (:mount fgears1)) (:body fan1 :role (:hold agent1)))
                                           (t44-phobia-segment)))))
    (t44-check "A4 held and mounted: C1" (search "C1 held and stationary at once" text)))
  (let ((text (t44-report (t44-one-segment "A4 wall mount placed"
                                           '((:body fan1 :role (:mount wgears1)) (:body fan1 :role (:place location5)))
                                           (t44-phobia-segment)))))
    (t44-check "A4 wall mount has no location: C2" (search "C2 locations differ: none, location5" text)))
  (t44-source-checks)
  (t44-run-general-checks)
  (format t "~&T44 PHOBIA CHECKS PASSED: ~D~%" *t44-count*))
