;;; T43 focused checks; stage the problem and load tech/constraint-profile.lisp first.
;;; claustro-topo: (t43-run-claustro-checks).  corner-topo: (t43-run-corner-checks).
;;; phobia-topo: (t43-run-phobia-checks).  Any problem: (t43-run-general-checks).
;;; No search.  Reference states replay prefixes of each problem's validated path.
(in-package :ww)
(defvar *t43-count* 0)
(defun t43-check (label value)
  (assert value () "T43 failed: ~A" label)
  (incf *t43-count*)
  (format t "~&PASS ~A~%" label))
(defun t43-text (function &rest arguments)
  (with-output-to-string (*standard-output*) (apply function arguments)))
(defun t43-facts (state)
  (sort (mapcar #'prin1-to-string (list-database (problem-state.idb state))) #'string<))
(defun t43-path (relative)
  (merge-pathnames relative (asdf:system-source-directory :wouldwork)))
(defun t43-claustro-actions ()
  (with-open-file (in (t43-path "doc/problems/claustro-topo/constraint-evidence/full-solution-candidate.lisp"))
    (read in)))
(defun t43-corner-actions ()
  (with-open-file (in (t43-path "doc/problems/corner-topo/constraint-evidence/full-path/candidate-actions.lisp"))
    (second (third (read in)))))
(defun t43-phobia-actions ()
  (with-open-file (in (t43-path "doc/problems/phobia-topo/constraint-evidence/complete-validated-path.txt"))
    (dotimes (i 3) (read-line in))
    (loop for form = (read in nil :eof) until (eq form :eof) collect form)))
(defun t43-prefix (actions count)
  (let ((validation (validate-action-sequence *start-state* (subseq actions 0 count))))
    (t43-check (list "prefix replays" *problem-name* count) (action-sequence-validation-success-p validation))
    (action-sequence-validation-final-state validation)))
(defun t43-classified ()
  "SD's classified service entries, computed as REPORT-SERVICE-DEPENDENCIES computes them."
  (let* ((controls (control-facts))
         (arcs (traversal-arc-facts))
         (finals (service-goal-finals (get 'goal-fn :form))))
    (multiple-value-bind (table unused) (service-table arcs controls finals)
      (values (service-classify table) table unused))))
(defun t43-phases ()
  (let* ((arcs (traversal-arc-facts))
         (names (region-name-table (region-blocks (traversal-endpoints) (contract-free-arcs arcs)))))
    (service-phases arcs names (service-goal-finals (get 'goal-fn :form)))))
(defun t43-entry (classified node)
  (find node classified :key #'first :test #'equal))
(defun t43-options (entry kind)
  (remove-if-not (lambda (option) (eq (getf option :kind) kind)) (third entry)))
(defun t43-site-option (entry location support)
  (find (list location support) (t43-options entry :override) :key (lambda (option) (getf option :site))
        :test #'equal))
(defun t43-transition (before after before-label after-label &rest options)
  "SW on a replayed pair, checking caller preservation, evaluation and engine agreement."
  (let* ((old-before (t43-facts before))
         (old-after (t43-facts after))
         (scenario (append (list :before before :before-provenance before-label
                                 :state after :provenance after-label)
                           options))
         (result (service-transition-result scenario)))
    (t43-check (list "before state unchanged" before-label) (equal old-before (t43-facts before)))
    (t43-check (list "after state unchanged" after-label) (equal old-after (t43-facts after)))
    (t43-check (list "evaluated" after-label) (eq :evaluated (getf result :status)))
    (t43-check (list "engine agreement both states" after-label) (every #'identity (getf result :agreement)))
    (t43-check (list "report prints" after-label)
               (search "SW  SERVICE TRANSITION" (t43-text #'report-service-transition scenario)))
    result))
(defun t43-change (result object)
  (find object (getf result :services) :key (lambda (change) (getf change :object))))
(defun t43-source-checks ()
  "C3: the T43 code names no problem object and uses neither LABELS nor FLET."
  (let* ((source (with-open-file (in (t43-path "tech/constraint-profile.lisp"))
                   (let ((text (make-string (file-length in))))
                     (subseq text 0 (read-sequence text in)))))
         (block (subseq source (search ";;;; SD -- SERVICES AND SETUP DEPENDENCIES" source)
                        (search ";;;; FH -- FROM HERE" source))))
    (dolist (name '("claustro" "corner" "phobia" "gate1" "location1" "agent1" "receiver1" "jammer1"
                    "wblower" "fan1" "plate1" "connector1"))
      (t43-check (list "no problem name in T43 code" name) (not (search name block :test #'char-equal))))
    (t43-check "no LABELS or FLET in T43 code"
               (notany (lambda (word) (search word block :test #'char-equal)) '("(labels " "(flet ")))))
(defun t43-closure-checks ()
  "A7 on synthetic tables, and the clause-aware door-set walk."
  (let* ((self (list (list :kind :override :premises (list (list :pass 'a)))))
         (lone (service-classify (list (cons (list :pass 'a) self))))
         (both (service-classify (list (cons (list :pass 'a)
                                             (append self (list (list :kind :override :premises nil)))))))
         (dangling (service-classify (list (cons (list :pass 'b) (list (list :kind :control :premises (list (list :pass 'c)))))
                                           (cons (list :pass 'c) nil))))
         (hidden (service-classify
                  (list (cons (list :pass 'a) (list (list :kind :override :premises (list (list :pass 'b)))
                                                    (list :kind :override :premises nil)))
                        (cons (list :pass 'b) (list (list :kind :control :premises (list (list :active 'r)))
                                                    (list :kind :override :premises nil)))
                        (cons (list :active 'r) (list (list :kind :corridor :premises (list (list :pass 'a)))))))))
    (t43-check "A7 only self-dependent option: SETUP QUESTION" (eq :setup-question (second (first lone))))
    (t43-check "A7 self-dependent option class NEEDS FIRST"
               (eq :needs-first (getf (first (third (first lone))) :class)))
    (t43-check "A7 self-dependent path ends at itself"
               (equal '((:pass a)) (getf (first (third (first lone))) :path)))
    (t43-check "A7 premise-free option added: SUPPORTED" (eq :supported (second (first both))))
    (t43-check "A7 with it, the other option still NEEDS FIRST"
               (eq :needs-first (getf (first (third (first both))) :class)))
    (t43-check "A7 unmet premise: NO PROVIDER" (eq :no-provider (second (first dangling))))
    (t43-check "A7 unmet premise: option UNSUPPORTED"
               (eq :unsupported (getf (first (third (first dangling))) :class)))
    (let ((option (first (third (first hidden)))))
      (t43-check "A7 another jam hides the dependency: SUPPORTED with any providers"
                 (eq :supported (getf option :class)))
      (t43-check "A7 standing providers expose it: NEEDS FIRST"
                 (eq :needs-first (getf option :standing-class)))
      (t43-check "A7 standing path b -> r -> a"
                 (equal '((:pass b) (:active r) (:pass a)) (getf option :standing-path))))
    (let* ((rows '(("R1" "R2" walking ((g1) (g2)) :both 1)
                   ("R2" "R3" walking ((g3)) :forward 1)
                   ("R1" "R3" walking ((g1 g3 g4)) :both 1)))
           (forward (service-door-sets "R1" rows))
           (backward (service-door-sets "R3" rows)))
      (t43-check "door sets: one clause per row, minimal"
                 (equal '((g1 g3) (g2 g3)) (service-sorted-sets (gethash "R3" forward))))
      (t43-check "door sets: directed row not walked backward"
                 (equal '((g1 g3 g4)) (service-sorted-sets (gethash "R1" backward))))
      (t43-check "door sets: start region free" (equal '(nil) (gethash "R1" forward))))))
(defun t43-unresolved-checks (state)
  "A8: every reason prints only itself; the caller's states are unchanged."
  (let* ((unsettled (copy-problem-state state))
         (inconsistent (copy-problem-state state))
         (before (t43-facts state))
         (agent (first (census-type-instances 'agent)))
         (fact (first (remove-if-not (lambda (fact) (member (first fact) '(open turning active)))
                                     (list-database (problem-state.idb state))))))
    (if fact
      (delete-proposition fact (problem-state.idb unsettled))
      (add-proposition (list 'open (first (census-type-instances 'gate))) (problem-state.idb unsettled)))
    (setf (gethash 'inconsistent-state (problem-state.idb inconsistent)) t)
    (dolist (entry (list (list (list :state state :provenance "x") "before state: missing problem-state")
                         (list (list :before state :state state :provenance "x") "before state: state provenance missing")
                         (list (list :before state :before-provenance "" :state state :provenance "x")
                               "before state: state provenance missing")
                         (list (list :before state :before-provenance "b" :provenance "x") "state: missing problem-state")
                         (list (list :before state :before-provenance "b" :state state) "state: state provenance missing")
                         (list (list :before inconsistent :before-provenance "b" :state state :provenance "x")
                               "before state: state marked inconsistent")
                         (list (list :before state :before-provenance "b" :state unsettled :provenance "x")
                               "state: state is not a propagation fixed point")
                         (list (list :before state :before-provenance "b" :state state :provenance "x"
                                     :agent (first (census-type-instances 'location)))
                               "is not an agent")
                         (list (list :before state :before-provenance "b" :state state :provenance "x"
                                     :agent agent :transit (list agent))
                               "a :TRANSIT or :RETURN entry is not a location")
                         (list (list :before state :before-provenance "b" :state state :provenance "x"
                                     :agent agent :final (list 'open))
                               "a :FINAL entry is not a proposition")))
      (let* ((result (service-transition-result (first entry)))
             (text (t43-text #'report-service-transition (first entry))))
        (t43-check (list "A8 unresolved" (second entry)) (eq :unresolved (getf result :status)))
        (t43-check (list "A8 reason" (second entry)) (search (second entry) (getf result :reason)))
        (t43-check (list "A8 prints only its reason" (second entry))
                   (and (search "UNRESOLVED" text) (not (search "passage services" text))))))
    (t43-check "A8 caller state unchanged by unresolved calls" (equal before (t43-facts state)))))
(defun t43-sd-text-checks (pieces)
  (let ((text (t43-text #'report-service-dependencies)))
    (dolist (piece pieces)
      (t43-check (list "SD contains" piece) (search piece text)))
    text))
(defun t43-run-claustro-checks ()
  (setf *t43-count* 0)
  (t43-source-checks)
  (t43-closure-checks)
  (let* ((classified (t43-classified))
         (gate1 (t43-entry classified '(:pass gate1)))
         (plate-room (remove-if-not (lambda (option)
                                      (member (first (getf option :site)) '(location3 location4 location5 location6)))
                                    (t43-options gate1 :override))))
    ;; A1
    (t43-check "A1 gate1 has override options only"
               (every (lambda (option) (eq :override (getf option :kind))) (third gate1)))
    (dolist (site '((location1 ground) (location2 ground) (location7 ground)))
      (t43-check (list "A1 gate1 DIRECT site" site)
                 (eq :direct (getf (t43-site-option gate1 (first site) (second site)) :class))))
    (t43-check "A1 eight plate-room sites" (= 8 (length plate-room)))
    (dolist (option plate-room)
      (let ((path (getf option :standing-path)))
        (t43-check (list "A1 plate-room site needs gate1 first through standing providers" (getf option :site))
                   (and (eq :needs-first (getf option :standing-class))
                        (member (first path) '((:pass gate2) (:pass gate3)) :test #'equal)
                        (equal (second path) '(:active receiver1))
                        (equal (car (last path)) '(:pass gate1))))
        (t43-check (list "A1 plate-room site supported only with a further jam" (getf option :site))
                   (eq :supported (getf option :class)))))
    (t43-check "A1 location2 site flagged on receiver1's corridor"
               (service-corridor-flag (t43-site-option gate1 'location2 'ground) 'gate1 (nth-value 1 (t43-classified))))
    (t43-check "A1 location1 site not flagged"
               (null (service-corridor-flag (t43-site-option gate1 'location1 'ground) 'gate1 (nth-value 1 (t43-classified)))))
    (t43-check "A1 gate1 SUPPORTED" (eq :supported (second gate1)))
    (dolist (gate '(gate2 gate3 gate6 gate7))
      (t43-check (list "A1 CONTROL via receiver1 active" gate)
                 (equal '((:active receiver1))
                        (getf (first (t43-options (t43-entry classified (list :pass gate)) :control)) :premises))))
    (let ((control (first (t43-options (t43-entry classified '(:pass gate4)) :control))))
      (t43-check "A1 gate4 CONTROL via receiver1 inactive (leaf)"
                 (and (null (getf control :premises)) (equal '("receiver1 inactive") (getf control :leaves))
                      (eq :direct (getf control :class)))))
    (t43-check "A1 receiver1 corridor option needs gate1 and location2 clear"
               (let ((corridor (first (t43-options (t43-entry classified '(:active receiver1)) :corridor))))
                 (and (equal '((:pass gate1)) (getf corridor :premises))
                      (equal '("location2 clear") (getf corridor :leaves)))))
    (t43-check "A1 opposed controls name receiver1"
               (equal '((receiver1 (gate2 gate3 gate6 gate7) (gate4)))
                      (service-opposed-controls (nth-value 1 (t43-classified)))))
    (let ((phases (t43-phases)))
      (t43-check "A1 jammer2's region access sets all name gate5"
                 (every (lambda (set) (member 'gate5 set))
                        (gethash "R6" (getf phases :access))))
      (t43-check "A1 transit necessary gate5-gate9"
                 (equal '(gate5 gate6 gate7 gate8 gate9) (service-necessary-doors (getf phases :transit)))))
    (t43-sd-text-checks
      '("SD  SERVICES AND SETUP DEPENDENCIES  [grade 2]"
        "gate1 open  start BLOCKED  verdict SUPPORTED  route TRANSIT alternative, RETURN alternative, TEMPORARY"
        "OVERRIDE SUPPORTED; through standing providers NEEDS gate1 open FIRST (path gate2 open -> receiver1 active -> gate1 open): gate2 open; gate3 open"
        "jam at location2 on ground (jammer1, jammer2); OCCUPIES location2 on transmitter1 -> receiver1's fixed corridor, which needs gate1"
        "jam at location7 on ground (jammer1, jammer2); JAM-DISALLOWED> from location1"
        "gate4 open  == (not receiver1)"
        "CONTROL DIRECT: receiver1 inactive"
        "receiver1: on for gate2, gate3, gate6, gate7; off for gate4"
        "gate1 open: 8 options, 0 even with further jams"
        "jammer2  location9 (R6)"
        "R6  (gate1 gate5) (gate2 gate5)"
        "NOT CLAIMED")))
  ;; A2
  (let* ((actions (t43-claustro-actions))
         (p5 (t43-prefix actions 5)) (p6 (t43-prefix actions 6))
         (p28 (t43-prefix actions 28)) (p29 (t43-prefix actions 29))
         (cut (t43-transition p5 p6 "claustro prefix 5" "claustro prefix 6 (box1 put at location2)"))
         (handover (t43-transition p28 p29 "claustro prefix 28" "claustro prefix 29 (jammer1 picked up)")))
    (t43-check "A2 receiver1 withdrawn, driving gate2-4, gate6, gate7"
               (equal '((receiver1 t nil (gate2 gate3 gate4 gate6 gate7))) (getf cut :primitives)))
    (dolist (gate '(gate2 gate3 gate6 gate7))
      (t43-check (list "A2 LOST by the beam interruption" gate)
                 (let ((change (t43-change cut gate)))
                   (and (eq :lost (getf change :change)) (equal '(:control) (getf change :lost))))))
    (t43-check "A2 gate4 GAINED by control" (equal '(:control) (getf (t43-change cut 'gate4) :providers)))
    (t43-check "A2 gate1 KEPT by its jam" (eq :kept (getf (t43-change cut 'gate1) :change)))
    (t43-check "A2 handover: gate1 KEPT BY ALTERNATIVE (OVERRIDE)"
               (let ((change (t43-change handover 'gate1)))
                 (and (eq :kept-by-alternative (getf change :change)) (getf change :override)
                      (equal '((:jam jammer1)) (getf change :lost))
                      (equal '((:jam jammer2)) (getf change :providers)))))
    (t43-check "A2 handover: no service LOST"
               (notany (lambda (change) (eq :lost (getf change :change))) (getf handover :services)))
    (t43-check "A2 handover: the jam removed is jammer1's"
               (equal '(((jamming jammer1 gate1)) nil) (getf handover :jams)))
    (t43-unresolved-checks p6))
  (format t "~&T43 CLAUSTRO CHECKS PASSED: ~D~%" *t43-count*))
(defun t43-run-corner-checks ()
  (setf *t43-count* 0)
  (let* ((classified (t43-classified))
         (phases (t43-phases))
         (gate1 (t43-entry classified '(:pass gate1))))
    ;; A3
    (t43-check "A3 gate1 route: transit and return necessary, temporary, not final"
               (equal '("TRANSIT necessary" "RETURN necessary" "TEMPORARY") (service-route-role 'gate1 phases)))
    (t43-check "A3 finals" (equal '((active receiver2) (active receiver3)) (getf phases :finals)))
    (t43-check "A3 gate1 CONTROL via receiver1 active"
               (equal '((:active receiver1)) (getf (first (t43-options gate1 :control)) :premises)))
    (t43-check "A3 receiver1 has DIRECT bootstrap chains"
               (find-if (lambda (option) (and (eq :bootstrap (getf option :rc-class)) (eq :direct (getf option :class))))
                        (t43-options (t43-entry classified '(:active receiver1)) :chain)))
    (t43-check "A3 receiver1 latch chains need receiver1 first"
               (every (lambda (option) (eq :needs-first (getf option :class)))
                      (remove :bootstrap (t43-options (t43-entry classified '(:active receiver1)) :chain)
                              :key (lambda (option) (getf option :rc-class)))))
    (dolist (receiver '(receiver2 receiver3))
      (t43-check (list "A3 FINAL receiver has chain options" receiver)
                 (t43-options (t43-entry classified (list :active receiver)) :chain)))
    (t43-check "A3 no OVERRIDE without a jammer"
               (notany (lambda (entry) (t43-options entry :override)) classified))
    (t43-sd-text-checks
      '("final services: (active receiver2) (active receiver3)"
        "temporary services (in some transit set, not final): gate1 via receiver1"
        "gate1 open  == receiver1  start BLOCKED  verdict SUPPORTED  route TRANSIT necessary, RETURN necessary, TEMPORARY"
        "receiver2 active  start INACTIVE  verdict SUPPORTED  route FINAL")))
  ;; A4
  (let* ((actions (t43-corner-actions))
         (result (t43-transition (t43-prefix actions 14) (t43-prefix actions 15)
                                 "corner prefix 14" "corner prefix 15 (connector2 picked up at location4)"
                                 :return '(location1) :final '((active receiver2) (active receiver3)))))
    (t43-check "A4 gate1 LOST, control withdrawn"
               (let ((change (t43-change result 'gate1)))
                 (and (eq :lost (getf change :change)) (equal '(:control) (getf change :lost)))))
    (t43-check "A4 receiver1 withdrawn, affecting gate1"
               (member '(receiver1 t nil (gate1)) (getf result :primitives) :test #'equal))
    (t43-check "A4 return to location1 NOT MET" (member '(:return location1 nil) (getf result :requirements) :test #'equal))
    (t43-check "A4 finals MET"
               (and (member '(:final (active receiver2) t) (getf result :requirements) :test #'equal)
                    (member '(:final (active receiver3) t) (getf result :requirements) :test #'equal)))
    (t43-check "A4 retrieval lost for the east-side connectors"
               (equal '(connector1 connector3) (mapcar #'first (getf result :retrieval-lost))))
    (t43-unresolved-checks (t43-prefix actions 15)))
  (format t "~&T43 CORNER CHECKS PASSED: ~D~%" *t43-count*))
(defun t43-run-phobia-checks ()
  (setf *t43-count* 0)
  (let ((classified (t43-classified)))
    ;; A5
    (t43-check "A5 wblower2 CONTROL needs receiver2 active"
               (equal '((:active receiver2)) (getf (first (t43-options (t43-entry classified '(:pass wblower2)) :control)) :premises)))
    (t43-check "A5 wblower3 CONTROL is receiver2 inactive"
               (equal '("receiver2 inactive") (getf (first (t43-options (t43-entry classified '(:pass wblower3)) :control)) :leaves)))
    (t43-check "A5 opposed controls name receiver2"
               (member '(receiver2 (wblower2) (wblower3)) (service-opposed-controls (nth-value 1 (t43-classified)))
                       :test #'equal))
    (dolist (drive '(wblower2 wblower3))
      (t43-check (list "A5 OVERRIDE options" drive) (t43-options (t43-entry classified (list :pass drive)) :override)))
    (t43-check "A5 wgears1 EQUIPMENT option"
               (t43-options (t43-entry classified '(:pass wgears1)) :equipment))
    (t43-check "A5 receiver2 no provider in RC's start-state scope"
               (eq :no-provider (second (t43-entry classified '(:active receiver2)))))
    (t43-sd-text-checks
      '("wblower2 clear  == (not receiver2)"
        "wblower3 clear  == receiver2"
        "receiver2: on for wblower2; off for wblower3"
        "EQUIPMENT DIRECT: no fan mounted (T42 removal)"
        "fan1  mounted on wgears1, wall-hung (no location)")))
  ;; A6
  (let* ((actions (t43-phobia-actions))
         (result (t43-transition (t43-prefix actions 43) (t43-prefix actions 44)
                                 "phobia prefix 43" "phobia prefix 44 (connector1 picked up at location5)"
                                 :transit '(location8) :return '(location13))))
    (t43-check "A6 wblower2 KEPT BY ALTERNATIVE (OVERRIDE, jammer1)"
               (let ((change (t43-change result 'wblower2)))
                 (and (eq :kept-by-alternative (getf change :change)) (getf change :override)
                      (equal '(:control) (getf change :lost)) (equal '((:jam jammer1)) (getf change :providers)))))
    (t43-check "A6 wblower3 GAINED by control"
               (let ((change (t43-change result 'wblower3)))
                 (and (eq :gained (getf change :change)) (equal '(:control) (getf change :providers)))))
    (t43-check "A6 receiver2 withdrawn, affecting wblower2 and wblower3"
               (equal '((receiver2 t nil (wblower2 wblower3))) (getf result :primitives)))
    (t43-check "A6 connector1's pairings removed"
               (equal '(((paired connector1 connector2) (paired connector1 receiver2)) nil) (getf result :pairings)))
    (t43-check "A6 no service LOST"
               (notany (lambda (change) (eq :lost (getf change :change))) (getf result :services)))
    (t43-check "A6 transit to location8 MET" (member '(:transit location8 t) (getf result :requirements) :test #'equal))
    (t43-check "A6 return from location13 MET" (member '(:return location13 t) (getf result :requirements) :test #'equal))
    (t43-unresolved-checks (t43-prefix actions 44)))
  (format t "~&T43 PHOBIA CHECKS PASSED: ~D~%" *t43-count*))
(defun t43-run-general-checks ()
  "Any problem: SD prints; SW is UNRESOLVED with a spliced recorder, else evaluates start->start."
  (setf *t43-count* 0)
  (let ((text (t43-text #'report-service-dependencies))
        (result (service-transition-result (list :before *start-state* :before-provenance "start"
                                                 :state *start-state* :provenance "start"
                                                 :agent (first (census-type-instances 'agent))))))
    (t43-check (list "SD prints" *problem-name*) (search "NOT CLAIMED" text))
    (if (member "recorder" *spliced-tech-names* :test #'string=)
      (progn
        (t43-check (list "SD physical-view note" *problem-name*) (search "RECORDER SPLICED" text))
        (t43-check (list "SW UNRESOLVED with a recorder" *problem-name*)
                   (and (eq :unresolved (getf result :status)) (search "recorder" (getf result :reason)))))
      (t43-check (list "SW start->start: nothing lost or gained" *problem-name*)
                 (and (eq :evaluated (getf result :status))
                      (every (lambda (change) (member (getf change :change) '(:kept :blocked)))
                             (getf result :services))))))
  (format t "~&T43 GENERAL CHECKS PASSED (~A): ~D~%" *problem-name* *t43-count*))
