;;; T42 focused checks; stage the problem and load tech/constraint-profile.lisp first.
;;; Phobia: (t42-run-phobia-checks).  Other problems: (t42-run-absent-equipment-checks).
;;; No search.  Reference states replay prefixes of phobia's validated 54-action path;
;;; fixtures are hand-edited states settled by the engine's own propagation first.
(in-package :ww)
(defvar *t42-count* 0)
(defun t42-check (label value)
  (assert value () "T42 failed: ~A" label)
  (incf *t42-count*)
  (format t "~&PASS ~A~%" label))
(defun t42-text (function &rest arguments)
  (with-output-to-string (*standard-output*) (apply function arguments)))
(defun t42-facts (state)
  (sort (mapcar #'prin1-to-string (list-database (problem-state.idb state))) #'string<))
(defun t42-path-actions ()
  (with-open-file (in (merge-pathnames "doc/problems/phobia-topo/constraint-evidence/complete-validated-path.txt"
                                       (asdf:system-source-directory :wouldwork)))
    (dotimes (i 3) (read-line in))
    (loop for form = (read in nil :eof) until (eq form :eof) collect form)))
(defun t42-prefix (count)
  (let ((validation (validate-action-sequence *start-state* (subseq (t42-path-actions) 0 count))))
    (t42-check (list "phobia prefix replays" count) (action-sequence-validation-success-p validation))
    (action-sequence-validation-final-state validation)))
(defun t42-settled-fixture (state delete add)
  (let ((copy (copy-problem-state state)))
    (dolist (fact delete) (delete-proposition fact (problem-state.idb copy)))
    (dolist (fact add) (add-proposition fact (problem-state.idb copy)))
    (invalidate-problem-state-hash copy)
    (let ((*applying-init-action* nil))
      (funcall (symbol-function 'propagate-changes!) copy))
    (t42-check (list "fixture settles consistently" delete add) (not (state-is-inconsistent copy)))
    copy))
(defun t42-record (result key field value)
  (find value (getf result key) :key (lambda (record) (getf record field))))
(defun t42-status (result gears)
  (getf (t42-record result :drives :gears gears) :status))
(defun t42-agreement (result)
  (let ((agreement (getf result :agreement)))
    (t42-check (list "engine agreement: mounting" (getf result :provenance)) (getf agreement :mount))
    (t42-check (list "engine agreement: removal" (getf result :provenance)) (getf agreement :pickup))
    (t42-check (list "engine agreement: boarding" (getf result :provenance)) (getf agreement :boarding))))
(defun t42-result (state provenance &rest options)
  (let* ((before (t42-facts state))
         (prior (getf options :before))
         (prior-facts (when prior (t42-facts prior)))
         (result (equipment-scenario-result (append (list :state state :provenance provenance) options))))
    (t42-check (list "caller state unchanged" provenance) (equal before (t42-facts state)))
    (when prior
      (t42-check (list "before state unchanged" provenance) (equal prior-facts (t42-facts prior))))
    (t42-check (list "evaluated" provenance) (eq :evaluated (getf result :status)))
    (t42-agreement result)
    (t42-text #'report-equipment-scenario (append (list :state state :provenance provenance) options))
    result))
(defun t42-mc-checks ()
  (let ((text (t42-text #'report-mechanic-coverage)))
    (dolist (piece '("floor-gears       COVERED    contract (also S1 CC; EQ scenario)"
                     "UNCOVERED (0):"
                     "a gears-mounted fan in the floor-gears contract"
                     "fan1  start MOUNTED on wgears1, WALL-HUNG (no location); compatible mounts fgears1, wgears1"
                     "1 fan for 2 mounts: at most 1 mount can have a stream at once"
                     "fgears1  floor gears at location10 -> location11 (level 10); working height 0"
                     "start        VACANT, TURNING, NO FAN: no stream"
                     "wgears1  wall gears at location2 -> location1 (level 0); working height 1"
                     "start        fan1 mounted, turning: EFFECTIVE STREAM"
                     "a jam of wgears1 is redundant while no fan is mounted"
                     "arcs gated by its stream (0; clauses in S3)"
                     "boarding: STEP from the ground at location10 onto the fan"
                     "launched with its stack to location11 (level 10)"
                     "otherwise an occupant not ON a support drops back to location10"
                     "exits from location11"
                     "mount from   location10 (floor 0: within vertical reach)"
                     "mount from   location2 (floor 0: within vertical reach)"
                     "stream physics: the wall-blower contract"
                     "These rows state compatibility and prerequisites only"))
      (t42-check (list "MC contains" piece) (search piece text)))
    (let ((gated (equipment-gated-arcs 'wgears1 (traversal-arc-facts))))
      (t42-check "wgears1 gated arcs are every arc naming it"
                 (= (length gated)
                    (count-if (lambda (arc) (some (lambda (clause) (member 'wgears1 clause)) (fourth arc)))
                              (traversal-arc-facts))))
      (t42-check "MC prints the wgears1 gated-arc count"
                 (search (format nil "arcs gated by its stream (~D; clauses in S3)" (length gated)) text)))
    (let ((floor-row (search "contract floor-blower" text)))
      (t42-check "no floor-blower contract printed in a problem without floor blowers" (null floor-row)))))
(defun t42-source-checks ()
  "C3: the T42 code names no problem object."
  (let* ((source (with-open-file (in (merge-pathnames "tech/constraint-profile.lisp"
                                                      (asdf:system-source-directory :wouldwork)))
                   (let ((text (make-string (file-length in))))
                     (subseq text 0 (read-sequence text in)))))
         (mc (subseq source (search "(defun equipment-state-facts" source)
                     (search "(defun report-mechanic-contract" source)))
         (eq-block (subseq source (search ";;;; EQ -- REMOVABLE EQUIPMENT" source)
                           (search ";;;; NH -- NECESSITY HINTS" source))))
    (dolist (block (list mc eq-block))
      (dolist (name '("phobia" "fan1" "fgears" "wgears" "agent1" "jammer1" "location1"))
        (t42-check (list "no problem name in T42 code" name) (not (search name block :test #'char-equal)))))
    (t42-check "no LABELS or FLET in T42 code"
               (notany (lambda (word) (or (search word mc :test #'char-equal)
                                          (search word eq-block :test #'char-equal)))
                       '("(labels " "(flet ")))))
(defun t42-removal-checks ()
  ;; A2: prefix 11 -> 12, 18 -> 19, and the start-state removal fixture.
  (let* ((p11 (t42-prefix 11)) (p12 (t42-prefix 12)) (p18 (t42-prefix 18)) (p19 (t42-prefix 19))
         (r12 (t42-result p12 "phobia prefix 12" :before p11 :before-provenance "phobia prefix 11"))
         (r19 (t42-result p19 "phobia prefix 19" :before p18 :before-provenance "phobia prefix 18"))
         (moved (t42-settled-fixture *start-state* '((mounted-on fan1 wgears1)) '((has-location fan1 location4))))
         (rmoved (t42-result moved "start with fan1 on the ground at location4" :before *start-state*
                             :before-provenance "phobia start")))
    (t42-check "A2 prefix 12 fan1 held" (equal '(:held agent1) (getf (t42-record r12 :fans :fan 'fan1) :place)))
    (t42-check "A2 11->12 fan place change"
               (equal '((fan1 (:mounted wgears1 nil) (:held agent1))) (getf (getf r12 :transition) :fans)))
    (t42-check "A2 11->12 wgears1 jammed with fan, then vacant"
               (equal '((wgears1 :fan-mounted-stopped :vacant-stopped "NO STREAM EITHER WAY"))
                      (getf (getf r12 :transition) :drives)))
    (t42-check "A2 prefix 12 jammer still named on wgears1"
               (equal '(jammer1) (getf (t42-record r12 :drives :gears 'wgears1) :jammers)))
    (t42-check "A2 18->19 jam released: vacant gears turn with no stream"
               (equal '((wgears1 :vacant-stopped :turning-no-fan "NO STREAM EITHER WAY"))
                      (getf (getf r19 :transition) :drives)))
    (t42-check "A2 fixture: wgears1 stream lost while turning"
               (equal '((wgears1 :effective-stream :turning-no-fan "STREAM LOST"))
                      (getf (getf rmoved :transition) :drives)))
    (t42-check "A2 engine: wgears1 obstructs at start"
               (not (funcall (symbol-function 'stream-obstacle-clear) *start-state* 'agent1 'wgears1)))
    (t42-check "A2 engine: wgears1 clear without its fan"
               (funcall (symbol-function 'stream-obstacle-clear) moved 'agent1 'wgears1))
    (t42-check "A2 engine: wgears1 still turning without its fan"
               (member '(turning wgears1) (crossing-state-facts moved) :test #'equal))
    (t42-check "A2 report prints STREAM LOST"
               (search "mount wgears1: EFFECTIVE STREAM -> TURNING NO FAN: STREAM LOST"
                       (t42-text #'report-equipment-scenario
                                 (list :state moved :provenance "fixture" :before *start-state*
                                       :before-provenance "start"))))))
(defun t42-inert-and-mount-checks ()
  ;; A3 prefix 14; A4 prefix 52; A6 start.
  (let* ((p14 (t42-prefix 14)) (p52 (t42-prefix 52))
         (r14 (t42-result p14 "phobia prefix 14"))
         (r52 (t42-result p52 "phobia prefix 52"))
         (rstart (t42-result *start-state* "phobia start")))
    (t42-check "A3 fan1 resting on the ground at location4"
               (equal '(:resting ground location4) (getf (t42-record r14 :fans :fan 'fan1) :place)))
    (t42-check "A3 fan1 not steppable" (not (getf (t42-record r14 :fans :fan 'fan1) :steppable)))
    (t42-check "A3 inert verdict, engine offers no step"
               (let ((record (t42-record r14 :boarding :fan 'fan1)))
                 (and (string= "NOT STEPPABLE (inert)" (getf record :verdict)) (not (getf record :engine)))))
    (t42-check "A3 pickup possible from location4"
               (null (getf (t42-record r14 :removal :fan 'fan1) :failures)))
    (t42-check "A4 prefix 52 fgears1 mountable"
               (null (getf (t42-record r52 :mounting :gears 'fgears1) :failures)))
    (t42-check "A4 prefix 52 wgears1 out of reach"
               (equal '("OUT OF REACH") (getf (t42-record r52 :mounting :gears 'wgears1) :failures)))
    (let ((*vertical-reach-limit* -1))
      (let ((limited (t42-result p52 "phobia prefix 52, reach limit -1")))
        (t42-check "A4 reach limit -1: fgears1 beyond vertical reach"
                   (equal '("BEYOND VERTICAL REACH")
                          (getf (t42-record limited :mounting :gears 'fgears1) :failures)))
        (t42-check "A4 reach limit -1: wgears1 out of and beyond reach"
                   (equal '("OUT OF REACH" "BEYOND VERTICAL REACH")
                          (getf (t42-record limited :mounting :gears 'wgears1) :failures)))))
    (t42-check "A4 occupied mount (reason function, second fan in the fact list)"
               (equal '("occupied by other-fan")
                      (mapcar (lambda (text) (string-downcase text))
                              (equipment-mount-failures p52 (cons '(mounted-on other-fan fgears1)
                                                                  (equipment-state-facts p52))
                                                        'agent1 'fan1 'fgears1))))
    (t42-check "A6 start: fgears1 turning with no fan"
               (eq :turning-no-fan (t42-status rstart 'fgears1)))
    (t42-check "A6 start: wgears1 effective stream" (eq :effective-stream (t42-status rstart 'wgears1)))
    (t42-check "A6 start: no occupant aloft"
               (null (getf (t42-record rstart :lifts :gears 'fgears1) :occupants)))
    (t42-check "A6 start: wall-hung fan cannot be picked up from location4"
               (equal '("OUT OF REACH") (getf (t42-record rstart :removal :fan 'fan1) :failures)))))
(defun t42-install-and-lift-checks ()
  ;; A5 prefixes 52 -> 53 -> 54; A7 fixtures from the final state.
  (let* ((p52 (t42-prefix 52)) (p53 (t42-prefix 53)) (p54 (t42-prefix 54))
         (r53 (t42-result p53 "phobia prefix 53" :before p52 :before-provenance "phobia prefix 52"))
         (r54 (t42-result p54 "phobia prefix 54" :before p53 :before-provenance "phobia prefix 53")))
    (t42-check "A5 fgears1 effective stream" (eq :effective-stream (t42-status r53 'fgears1)))
    (t42-check "A5 fan1 steppable" (getf (t42-record r53 :fans :fan 'fan1) :steppable))
    (t42-check "A5 agent1 boardable, engine step offered"
               (let ((record (t42-record r53 :boarding :agent 'agent1)))
                 (and (string= "BOARDABLE" (getf record :verdict)) (getf record :engine))))
    (t42-check "A5 52->53 stream gained"
               (equal '((fgears1 :turning-no-fan :effective-stream "STREAM GAINED"))
                      (getf (getf r53 :transition) :drives)))
    (t42-check "A5 54 agent1 sustained by fgears1"
               (equal '((agent1 (fgears1))) (getf (t42-record r54 :lifts :gears 'fgears1) :occupants)))
    (t42-check "A5 53->54 agent1 lifted to location11"
               (and (equal '((agent1 location10 location11)) (getf (getf r54 :transition) :moved))
                    (equal '((fgears1 location11 (agent1) nil)) (getf (getf r54 :transition) :lifts))))
    (t42-check "A5 54 goal state: agent1 at location11"
               (member '(has-location agent1 location11) (crossing-state-facts p54) :test #'equal))
    (let* ((jammed (t42-settled-fixture p54 '((has-location jammer1 location8) (jamming jammer1 wblower4))
                                        '((has-location jammer1 location9) (jamming jammer1 fgears1))))
           (unmounted (t42-settled-fixture p54 '((mounted-on fan1 fgears1)) nil))
           (rj (t42-result jammed "final state, fgears1 jammed" :before p54 :before-provenance "phobia prefix 54"))
           (ru (t42-result unmounted "final state, fan1 unmounted on the ground" :before p54
                           :before-provenance "phobia prefix 54")))
      (dolist (entry (list (list "jammed" rj :fan-mounted-stopped) (list "unmounted" ru :turning-no-fan)))
        (destructuring-bind (label result status) entry
          (t42-check (list "A7 stream lost" label)
                     (equal (list (list 'fgears1 :effective-stream status "STREAM LOST"))
                            (getf (getf result :transition) :drives)))
          (t42-check (list "A7 agent1 drops to location10" label)
                     (member '(agent1 location11 location10) (getf (getf result :transition) :moved) :test #'equal))
          (t42-check (list "A7 aloft lost" label)
                     (equal '((fgears1 location11 nil (agent1))) (getf (getf result :transition) :lifts)))))
      (t42-check "A7 jammed fixture names jammer1" (equal '(jammer1) (getf (t42-record rj :drives :gears 'fgears1) :jammers)))
      (t42-check "A3 unmounted fan resting at location10, not steppable"
                 (let ((record (t42-record ru :fans :fan 'fan1)))
                   (and (equal '(:resting ground location10) (getf record :place)) (not (getf record :steppable)))))
      (t42-check "A3 unmounted: agent1 at location10 is refused a step"
                 (let ((record (t42-record ru :boarding :agent 'agent1)))
                   (and (string= "NOT STEPPABLE (inert)" (getf record :verdict)) (not (getf record :engine))))))))
(defun t42-unresolved-checks ()
  (let* ((unsettled (copy-problem-state *start-state*))
         (inconsistent (copy-problem-state *start-state*))
         (before (t42-facts *start-state*)))
    (delete-proposition '(turning fgears1) (problem-state.idb unsettled))
    (setf (gethash 'inconsistent-state (problem-state.idb inconsistent)) t)
    (dolist (entry (list (list (list :provenance "x") "state: missing problem-state")
                         (list (list :state *start-state*) "state: state provenance missing")
                         (list (list :state *start-state* :provenance "") "state: state provenance missing")
                         (list (list :state inconsistent :provenance "x") "state: state marked inconsistent")
                         (list (list :state unsettled :provenance "x") "state: state is not a propagation fixed point")
                         (list (list :state *start-state* :provenance "x" :before unsettled :before-provenance "y")
                               "before state: state is not a propagation fixed point")
                         (list (list :state *start-state* :provenance "x" :before *start-state*)
                               "before state: state provenance missing")))
      (destructuring-bind (scenario reason) entry
        (let ((result (equipment-scenario-result scenario))
              (text (t42-text #'report-equipment-scenario scenario)))
          (t42-check (list "UNRESOLVED" reason)
                     (and (eq :unresolved (getf result :status)) (search reason (getf result :reason))))
          (t42-check (list "report prints only the reason" reason)
                     (and (search "UNRESOLVED: " text) (not (search "fans:" text)))))))
    (let ((*spliced-tech-names* (cons "recorder" *spliced-tech-names*)))
      (t42-check "UNRESOLVED when the recorder is spliced"
                 (string= "recorder technology spliced: live and recording views are not evaluated"
                          (getf (equipment-scenario-result (list :state *start-state* :provenance "x")) :reason))))
    (t42-check "start state unchanged by unresolved checks" (equal before (t42-facts *start-state*)))))
(defun t42-run-phobia-checks ()
  (setf *t42-count* 0)
  (t42-mc-checks)
  (t42-source-checks)
  (t42-removal-checks)
  (t42-inert-and-mount-checks)
  (t42-install-and-lift-checks)
  (t42-unresolved-checks)
  (format t "~&T42 PHOBIA CHECKS PASSED: ~D~%" *t42-count*))
(defun t42-run-absent-equipment-checks ()
  "A problem without removable equipment: EQ is UNRESOLVED and MC prints no floor-gears block."
  (setf *t42-count* 0)
  (let ((result (equipment-scenario-result (list :state *start-state* :provenance "start"))))
    (t42-check (list "UNRESOLVED without equipment" *problem-name* (getf result :reason))
               (and (eq :unresolved (getf result :status))
                    (member (getf result :reason)
                            '("no fan in the problem" "no gears in the problem"
                              "recorder technology spliced: live and recording views are not evaluated")
                            :test #'string=))))
  (t42-check (list "no floor-gears contract printed" *problem-name*)
             (not (search "contract floor-gears" (t42-text #'report-mechanic-coverage))))
  (format t "~&T42 ABSENT-EQUIPMENT CHECKS PASSED (~A): ~D~%" *problem-name* *t42-count*))
