;;; Filename: constraint-boundary.lisp

;;; Optional supplied-transition diagnostic BT (T45, Extractor-Specifications.md section 6.3):
;;; what one recorder boundary (STOP-RECORDER, CANCEL-PLAYBACK) or one support-changing
;;; engine action does to a supplied settled state.  Prerequisites are read separately from
;;; effects.  A closure whose prerequisites are unmet is evaluated only as a HYPOTHETICAL,
;;; through the engine's own CLOSE-RECORDER-CYCLE-STATE!.  Consequences are read from the
;;; facts and engine queries of each state: supports and their chains, plates, beams,
;;; devices, each agent's route conditions in its own view, and caller-supplied obligations.
;;;
;;; THIS FILE IS A LOADABLE DIAGNOSTIC, like tech/constraint-profile.lisp: never named in an
;;; (include-tech ...) directive, not an ASDF component, plain Common Lisp in :WW, no problem
;;; object names.  Load it after constraint-profile.lisp and constraint-arrangement.lisp,
;;; after staging.  Callees precede callers because it is reloaded by hand.  Every state is
;;; evaluated on a private copy; the caller's state and scenario are never changed.
;;;
;;;   (report-boundary-transition
;;;     (list :state <settled state> :provenance "<source>"
;;;           :event '(:stop <ghost agent>)          ; or (:cancel <live agent>)
;;;                                                  ; or (:action <action form>)
;;;           :agents '(<agent> ...)                 ; optional
;;;           :obligations '((:fact (open <gate>) :phase :across :purpose "...")
;;;                          (:body <body> :role (:weight <plate>) :phase :across)
;;;                          (:reach (<agent> <location>) :phase :until-event))))

(in-package :ww)


;;;; COMMON TERMS ;;;;


(defun boundary-recorder-p ()
  "Whether the recorder technology is spliced into the staged problem."
  (and (member "recorder" *spliced-tech-names* :test #'string=) t))


(defun boundary-side (object)
  "OBJECT's recorder layer from the static mapping: :LIVE, :GHOST, or NIL when unmapped."
  (let ((mappings (relay-view-mappings)))
    (cond ((find object mappings :key #'second) :live)
          ((find object mappings :key #'third) :ghost))))


(defun boundary-state-phase (facts)
  "T34's phase for a state with FACTS: :OPEN while recording, :CLOSED with the recorder
   spliced, else :ORDINARY."
  (cond ((member '(recording-in-progress) facts :test #'equal) :open)
        ((boundary-recorder-p) :closed)
        (t :ordinary)))


(defun boundary-state-reason (state provenance label)
  "Why STATE, labelled LABEL, cannot be evaluated, or NIL: T43's settled-state rules, then
   T34's structural validation for the state's own phase."
  (or (equipment-state-reason state provenance label)
      (when (problem-state.happenings state)
        (format nil "~A: scheduled happenings are not modelled" label))
      (let ((invalid (arrangement-validation-reason
                       state (boundary-state-phase (arrangement-facts state)))))
        (when invalid
          (format nil "~A: structurally invalid (section 6.2): ~A" label invalid)))))


(defun boundary-event-form (event)
  "The engine action form EVENT names."
  (ecase (first event)
    (:stop (list 'stop-recorder (second event)))
    (:cancel (list 'cancel-playback (second event)))
    (:action (second event))))


(defun boundary-event-kind (event)
  "EVENT's kind: :STOP, :CANCEL, :START, or :ACTION for any other engine action.  A recorder
   session action given as (:ACTION form) takes its session kind."
  (let ((name (first (boundary-event-form event))))
    (case name
      (stop-recorder :stop)
      (cancel-playback :cancel)
      (start-recorder :start)
      (t :action))))


(defun boundary-event-agent (event)
  "The agent a STOP or CANCEL event names; a replay phrase's connectives are skipped."
  (find-if (lambda (argument) (member argument (census-type-instances 'agent)))
           (rest (boundary-event-form event))))


(defun boundary-event-reason (event)
  "Why EVENT is malformed or unsupported, or NIL."
  (cond ((not (and (consp event) (member (first event) '(:stop :cancel :action))
                   (consp (rest event)) (null (cddr event))))
         "event missing or malformed: use (:stop agent), (:cancel agent) or (:action form)")
        ((and (eq (first event) :action)
              (not (and (consp (second event)) (symbolp (first (second event))))))
         "event :ACTION needs one action form")
        ((and (eq (first event) :action)
              (not (find (first (second event)) *actions* :key #'action.name)))
         (format nil "no action named ~(~A~) in the staged problem" (first (second event))))
        ((and (member (boundary-event-kind event) '(:stop :cancel :start))
              (not (boundary-recorder-p)))
         "recorder technology not spliced: no recorder boundary exists")
        ((and (member (boundary-event-kind event) '(:stop :cancel))
              (/= 1 (count-if (lambda (argument) (member argument (census-type-instances 'agent)))
                              (rest (boundary-event-form event)))))
         "a :STOP or :CANCEL event must name one agent")))


(defun boundary-role-fact (body role)
  "The fact T44's ROLE for BODY asserts (section 13.8)."
  (ecase (first role)
    (:weight (list 'on body (second role)))
    (:jam (list 'jamming body (second role)))
    (:place (list 'has-location body (second role)))
    (:hold (list 'holding (second role) body))
    (:mount (list 'mounted-on body (second role)))
    (:support (list 'on (second role) body))))


(defun boundary-obligation-reason (obligation)
  "Why OBLIGATION is malformed, or NIL."
  (let ((subjects (count-if (lambda (key) (getf obligation key)) '(:fact :role :reach))))
    (cond ((not (member (getf obligation :phase) '(:until-event :across :after)))
           (format nil "obligation phase must be :UNTIL-EVENT, :ACROSS or :AFTER: ~S" obligation))
          ((/= subjects 1)
           (format nil "obligation needs exactly one of :FACT, :BODY with :ROLE, or :REACH: ~S" obligation))
          ((and (getf obligation :fact) (not (consp (getf obligation :fact))))
           (format nil "obligation :FACT is not a proposition: ~S" obligation))
          ((and (getf obligation :role)
                (not (and (getf obligation :body) (consp (getf obligation :role))
                          (member (first (getf obligation :role)) '(:weight :jam :place :hold :mount :support)))))
           (format nil "obligation role needs :BODY and a T44 role: ~S" obligation))
          ((and (getf obligation :reach)
                (not (and (consp (getf obligation :reach))
                          (member (first (getf obligation :reach)) (census-type-instances 'agent))
                          (member (second (getf obligation :reach)) (census-type-instances 'location)))))
           (format nil "obligation :REACH needs (agent location): ~S" obligation)))))


(defun boundary-scenario-reason (scenario)
  "Why SCENARIO cannot be evaluated, or NIL.  Missing data is never a transition verdict."
  (cond ((not (member 'propagate-changes! *update-names*)) "propagation unavailable")
        ((boundary-state-reason (getf scenario :state) (getf scenario :provenance) "state"))
        ((boundary-event-reason (getf scenario :event)))
        ((notevery (lambda (agent) (member agent (census-type-instances 'agent)))
                   (getf scenario :agents))
         "an :AGENTS entry is not an agent")
        ((some #'boundary-obligation-reason (getf scenario :obligations))
         (some #'boundary-obligation-reason (getf scenario :obligations)))
        ((and (member (boundary-event-kind (getf scenario :event)) '(:stop :cancel))
              (not (member '(recording-in-progress) (arrangement-facts (getf scenario :state))
                           :test #'equal)))
         "no recording cycle is open in the state: nothing to close")))


;;;; PREREQUISITES ;;;;


(defun boundary-cross-layer-facts (facts)
  "The HOLDING and ON facts in FACTS joining a live and a ghost object."
  (remove-if-not (lambda (fact)
                   (and (member (first fact) '(holding on))
                        (member :live (mapcar #'boundary-side (rest fact)))
                        (member :ghost (mapcar #'boundary-side (rest fact)))))
                 facts))


(defun boundary-located-agents (facts)
  "The agents with a location in FACTS, by name."
  (keeper-sorted-set (loop for agent in (census-type-instances 'agent)
                           when (keeper-fact-value 'has-location agent facts)
                             collect agent)))


(defun boundary-agent-readiness (state agent)
  "(agent at-recorder empty-handed can-close) by the engine's own queries in STATE."
  (list agent
        (and (relay-view-call 'recording-agent-at-recorder state agent) t)
        (and (relay-view-call 'recording-agent-empty-handed state agent) t)
        (and (relay-view-call 'recording-agent-can-close state agent) t)))


(defun boundary-session-rows (state facts kind agent)
  "STOP or CANCEL prerequisites as (label met detail) rows."
  (let ((ghosts (remove-if-not (lambda (other) (eq (boundary-side other) :ghost))
                               (boundary-located-agents facts)))
        (cross (boundary-cross-layer-facts facts))
        (own (boundary-agent-readiness state agent)))
    (append
      (list (list (if (eq kind :stop) "agent is a mapped ghost" "agent is a mapped live agent")
                  (eq (boundary-side agent) (if (eq kind :stop) :ghost :live))
                  (format nil "~(~A~) is ~(~A~)" agent (or (boundary-side agent) "unmapped")))
            (list "recording in progress" (and (member '(recording-in-progress) facts :test #'equal) t) nil))
      (if (eq kind :stop)
        (append
          (loop for ghost in ghosts
                for readiness = (boundary-agent-readiness state ghost)
                collect (list (format nil "ghost ~(~A~) at a recorder" ghost) (second readiness) nil)
                collect (list (format nil "ghost ~(~A~) empty-handed" ghost) (third readiness) nil))
          (list (list "no live/ghost HOLDING or ON dependency" (null cross)
                      (when cross (format nil "~(~{~S~^ ~}~)" cross)))))
        (list (list (format nil "~(~A~) at a recorder" agent) (second own) nil)
              (list (format nil "~(~A~) empty-handed" agent) (third own) nil))))))


(defun boundary-engine-successor (state form)
  "The engine's successor of FORM on a private copy of STATE: (values successor applies reason)."
  (let ((*applying-init-action* nil))
    (multiple-value-bind (successor ok reason)
        (apply-action-to-state form (%copy-problem-state state t) nil)
      (values successor (and ok t) reason))))


(defun boundary-prerequisites (state facts event)
  "Prerequisite rows, the itemized verdict, the engine's applicability and its successor."
  (let ((kind (boundary-event-kind event))
        (form (boundary-event-form event)))
    (multiple-value-bind (successor applies reason) (boundary-engine-successor state form)
      (let ((rows (if (member kind '(:stop :cancel))
                    (boundary-session-rows state facts kind (boundary-event-agent event))
                    (list (list "engine precondition" applies
                                (unless applies (format nil "~(~S~)" reason)))))))
        (list :rows rows
              :met (every #'second rows)
              :engine applies
              :engine-reason reason
              :successor (when applies successor)
              :agrees (eq (every #'second rows) applies)
              :readiness (when (member kind '(:stop :cancel))
                           (mapcar (lambda (agent) (boundary-agent-readiness state agent))
                                   (boundary-located-agents facts))))))))


;;;; EFFECTS ;;;;


(defun boundary-hypothetical-closure (state kind)
  "A private copy of STATE closed as KIND (:STOP or :CANCEL) asserts, then normalized by the
   engine's own CLOSE-RECORDER-CYCLE-STATE!, whatever the prerequisites."
  (let ((copy (%copy-problem-state state t))
        (*applying-init-action* nil))
    (revise (problem-state.idb copy)
            (list '(not (recording-in-progress))
                  (list 'recorder-cycles-used (relay-view-call 'recorder-cycle-count state))
                  '(recorder-cycle-closed)
                  (if (eq kind :stop)
                    '(recorder-cycle-stopped-by-ghost)
                    '(not (recorder-cycle-stopped-by-ghost)))))
    (invalidate-problem-state-hash copy)
    (relay-view-call 'close-recorder-cycle-state! copy)
    copy))


(defun boundary-support-facts (facts)
  "FACTS' support relations: ON, HOLDING of a tray, MOUNTED-ON."
  (remove-if-not (lambda (fact)
                   (or (member (first fact) '(on mounted-on))
                       (and (eq (first fact) 'holding)
                            (member (third fact) (census-type-instances 'tray)))))
                 facts))


(defun boundary-changes-support-p (before after)
  "Whether a support relation differs between the fact lists BEFORE and AFTER."
  (let ((old (boundary-support-facts before))
        (new (boundary-support-facts after)))
    (or (set-difference old new :test #'equal)
        (set-difference new old :test #'equal))))


(defun boundary-successor-reason (successor)
  "Why the successor cannot be evaluated, or NIL: engine rejection first, then T34 validation
   and a second propagation pass."
  (cond ((state-is-inconsistent successor)
         "engine marked the successor inconsistent (rejection or propagation bound); no impossibility claim")
        ((not (crossing-settled-p successor))
         "successor is not a fixed point under a second propagation pass")
        (t (let ((invalid (arrangement-validation-reason
                            successor (boundary-state-phase (arrangement-facts successor)))))
             (when invalid (format nil "successor structurally invalid (section 6.2): ~A" invalid))))))


(defun boundary-effects (state event prerequisites)
  "The successor, its label (:ENGINE or :HYPOTHETICAL), the hypothetical closure's agreement
   with the engine where both exist, or a reason when there is no effect to evaluate."
  (let* ((kind (boundary-event-kind event))
         (engine (getf prerequisites :successor))
         (closure (when (member kind '(:stop :cancel)) (boundary-hypothetical-closure state kind))))
    (cond ((and (null engine) (null closure))
           (list :reason "action not applicable: no hypothetical effect is synthesized for it"))
          ((and engine closure)
           (list :successor engine :label :engine
                 :closure-agrees (equal (arrangement-facts engine) (arrangement-facts closure))))
          (engine
           (if (or (member kind '(:start))
                   (boundary-changes-support-p (arrangement-facts state) (arrangement-facts engine)))
             (list :successor engine :label :engine)
             (list :reason "the action changes neither the recorder session nor a support relation: outside T45 (use SW or T34)")))
          (t (list :successor closure :label :hypothetical)))))


;;;; CONSEQUENCES ;;;;


(defun boundary-present-objects (facts)
  "Mobile objects located, held or mounted in FACTS."
  (keeper-sorted-set
    (loop for object in (census-type-instances 'mobile-object)
          when (or (keeper-fact-value 'has-location object facts)
                   (keeper-fact-value 'mounted-on object facts)
                   (find-if (lambda (fact) (and (eq (first fact) 'holding) (eq (third fact) object))) facts))
            collect object)))


(defun boundary-holder (object facts)
  "The agent holding OBJECT in FACTS, or NIL."
  (second (find-if (lambda (fact) (and (eq (first fact) 'holding) (eq (third fact) object))) facts)))


(defun boundary-support-chain (object facts)
  "OBJECT's supports and holders down to the ground or a wall mount, as a list of links:
   (ON support), (HELD agent), (MOUNTED gears), ending in (GROUND location); a fixed support
   such as a plate gives its HAS-POSITION location."
  (let ((chain nil)
        (current object))
    (loop while current
          do (let ((support (keeper-fact-value 'on current facts))
                   (holder (boundary-holder current facts))
                   (mount (keeper-fact-value 'mounted-on current facts)))
               (cond (support (push (list 'on support) chain) (setf current support))
                     (holder (push (list 'held holder) chain) (setf current holder))
                     (mount (push (list 'mounted mount) chain) (setf current nil))
                     (t (push (list 'ground (or (keeper-fact-value 'has-location current facts)
                                                (keeper-fact-value 'has-position current (list-static-db))))
                              chain)
                        (setf current nil)))))
    (nreverse chain)))


(defun boundary-base (state object facts)
  "OBJECT's engine BASE in STATE when it is a located vertical object, else NIL."
  (when (and (keeper-fact-value 'has-location object facts)
             (member object (census-type-instances 'vertical-object)))
    (relay-view-call 'base state object)))


(defun boundary-support-rows (before-state after-state before after)
  "Per mobile object in either state: (object label chain-before chain-after base-before
   base-after), label RETAINED, CHANGED, REMOVED or ADDED."
  (let ((old (boundary-present-objects before))
        (new (boundary-present-objects after)))
    (loop for object in (keeper-sorted-set (append old new))
          for chain-before = (when (member object old) (boundary-support-chain object before))
          for chain-after = (when (member object new) (boundary-support-chain object after))
          collect (list object
                        (cond ((not (member object new)) :removed)
                              ((not (member object old)) :added)
                              ((equal chain-before chain-after) :retained)
                              (t :changed))
                        chain-before chain-after
                        (when (member object old) (boundary-base before-state object before))
                        (when (member object new) (boundary-base after-state object after))))))


(defun boundary-occupants (support facts)
  "(occupant side) for every ON occupant of SUPPORT in FACTS."
  (loop for fact in facts
        when (and (eq (first fact) 'on) (eq (third fact) support))
          collect (list (second fact) (or (boundary-side (second fact)) :unmapped))))


(defun boundary-plate-rows (before after)
  "Per plate: (plate occupants-before occupants-after (depressed recording-depressed) before
   and after)."
  (loop for plate in (keeper-sorted-set (census-type-instances 'plate))
        collect (list plate (boundary-occupants plate before) (boundary-occupants plate after)
                      (list (and (member (list 'depressed plate) before :test #'equal) t)
                            (and (member (list 'recording-depressed plate) before :test #'equal) t))
                      (list (and (member (list 'depressed plate) after :test #'equal) t)
                            (and (member (list 'recording-depressed plate) after :test #'equal) t)))))


(defun boundary-receiver-rows (before after)
  "Per receiver: (receiver (active recording-active) before and after)."
  (loop for receiver in (keeper-sorted-set (census-type-instances 'receiver))
        collect (list receiver
                      (list (and (member (list 'active receiver) before :test #'equal) t)
                            (and (member (list 'recording-active receiver) before :test #'equal) t))
                      (list (and (member (list 'active receiver) after :test #'equal) t)
                            (and (member (list 'recording-active receiver) after :test #'equal) t)))))


(defun boundary-relation-changes (before after excluded)
  "(relation removed added) for every relation outside EXCLUDED whose facts differ."
  (let ((removed (set-difference before after :test #'equal))
        (added (set-difference after before :test #'equal)))
    (loop for relation in (sort (remove-duplicates (mapcar #'first (append removed added)))
                                #'string< :key #'symbol-name)
          unless (member relation excluded)
            collect (list relation
                          (remove-if-not (lambda (fact) (eq (first fact) relation)) removed)
                          (remove-if-not (lambda (fact) (eq (first fact) relation)) added)))))


(defun boundary-ghost-attribution (before after)
  "Ghost objects named by the ON, HOLDING and PAIRED facts removed between BEFORE and AFTER."
  (keeper-sorted-set
    (loop for fact in (set-difference before after :test #'equal)
          when (member (first fact) '(on holding paired))
            append (remove-if-not (lambda (object) (eq (boundary-side object) :ghost)) (rest fact)))))


(defun boundary-agent-route (state agent facts arcs)
  "AGENT's route conditions in STATE, in its own view: NIL when absent, else a plist."
  (when (keeper-fact-value 'has-location agent facts)
    (list :location (keeper-fact-value 'has-location agent facts)
          :passable (loop for object in (service-passage-objects arcs)
                          collect (cons object (and (relay-view-call 'obstacle-clear state agent object) t)))
          :arcs (loop for arc in arcs
                      when (fourth arc)
                        collect (cons arc (service-arc-passable-p state agent arc)))
          :mobility (service-state-mobility state agent facts))))


(defun boundary-route-row (agent before-state after-state before after arcs)
  "AGENT's route conditions before and after the event, and what changed."
  (let ((old (boundary-agent-route before-state agent before arcs))
        (new (boundary-agent-route after-state agent after arcs)))
    (list :agent agent :side (boundary-side agent)
          :status (cond ((and old (null new)) :removed)
                        ((and new (null old)) :added)
                        (t :present))
          :locations (list (getf old :location) (getf new :location))
          :passage (when (and old new)
                     (loop for (object . passable) in (getf old :passable)
                           for after-passable = (cdr (assoc object (getf new :passable)))
                           unless (eq passable after-passable)
                             collect (list object passable after-passable)))
          :arcs-lost (when (and old new)
                       (loop for (arc . passable) in (getf old :arcs)
                             when (and passable (not (cdr (assoc arc (getf new :arcs) :test #'equal))))
                               collect arc))
          :arcs-gained (when (and old new)
                         (loop for (arc . passable) in (getf new :arcs)
                               when (and passable (not (cdr (assoc arc (getf old :arcs) :test #'equal))))
                                 collect arc))
          :mobility-lost (keeper-sorted-set (set-difference (getf old :mobility) (getf new :mobility)))
          :mobility-gained (when (and old new)
                             (keeper-sorted-set (set-difference (getf new :mobility) (getf old :mobility)))))))


(defun boundary-obligation-holds-p (obligation state facts)
  "Whether OBLIGATION holds in STATE with FACTS."
  (cond ((getf obligation :reach)
         (let ((agent (first (getf obligation :reach))))
           (and (keeper-fact-value 'has-location agent facts)
                (member (second (getf obligation :reach)) (service-state-mobility state agent facts))
                t)))
        (t (let ((fact (or (getf obligation :fact)
                           (boundary-role-fact (getf obligation :body) (getf obligation :role)))))
             (and (or (member fact facts :test #'equal)
                      (member fact (list-static-db) :test #'equal))
                  t)))))


(defun boundary-obligation-verdict (phase held-before held-after)
  "An obligation's verdict for its PHASE (section 6.3)."
  (cond ((and (member phase '(:until-event :across)) (not held-before)) :not-held-before)
        ((eq phase :until-event) (if held-after :kept :expended))
        ((eq phase :across) (if held-after :survives :lost))
        (t (if held-after :met :not-met))))


(defun boundary-obligation-rows (obligations before-state after-state before after)
  "Per obligation: (obligation held-before held-after verdict)."
  (loop for obligation in obligations
        for old = (boundary-obligation-holds-p obligation before-state before)
        for new = (boundary-obligation-holds-p obligation after-state after)
        collect (list obligation old new (boundary-obligation-verdict (getf obligation :phase) old new))))


(defun boundary-consequences (scenario before-state after-state)
  "Every consequence family of section 6.3 for the two states."
  (let* ((before (arrangement-facts before-state))
         (after (arrangement-facts after-state))
         (arcs (traversal-arc-facts))
         (agents (or (getf scenario :agents) (boundary-located-agents before))))
    (list :supports (boundary-relation-changes before after
                                               (remove-if (lambda (relation) (member relation '(on holding mounted-on)))
                                                          (mapcar #'first (append before after))))
          :chains (boundary-support-rows before-state after-state before after)
          :plates (boundary-plate-rows before after)
          :pairings (boundary-relation-changes before after
                                               (remove 'paired (mapcar #'first (append before after))))
          :receivers (boundary-receiver-rows before after)
          :devices (boundary-relation-changes before after '(on holding mounted-on paired has-location))
          :primitives (service-primitive-changes (control-facts) before after)
          :ghosts (boundary-ghost-attribution before after)
          :routes (mapcar (lambda (agent) (boundary-route-row agent before-state after-state before after arcs))
                          agents)
          :obligations (boundary-obligation-rows (getf scenario :obligations)
                                                 before-state after-state before after))))


;;;; ENTRY POINTS ;;;;


(defun boundary-evaluate (scenario)
  "The evaluated result, or an UNRESOLVED or INCONSISTENT one."
  (let* ((state (getf scenario :state))
         (event (getf scenario :event))
         (prerequisites (boundary-prerequisites state (arrangement-facts state) event))
         (effects (boundary-effects state event prerequisites))
         (successor (getf effects :successor))
         (common (list :event event :kind (boundary-event-kind event)
                       :provenance (getf scenario :provenance) :prerequisites prerequisites)))
    (cond ((null successor)
           (append (list :status :unresolved :reason (getf effects :reason)) common))
          ((state-is-inconsistent successor)
           (append (list :status :inconsistent :reason (boundary-successor-reason successor)
                         :effects (getf effects :label))
                   common))
          ((boundary-successor-reason successor)
           (append (list :status :unresolved :reason (boundary-successor-reason successor)
                         :effects (getf effects :label))
                   common))
          (t (append (list :status :evaluated :effects (getf effects :label)
                           :closure-agrees (getf effects :closure-agrees)
                           :successor successor)
                     common
                     (boundary-consequences scenario state successor))))))


(defun boundary-transition-result (scenario)
  "BT: what one recorder boundary or support change does to a supplied settled state.
   Returns a plist; the caller's state and scenario are never changed (section 6.3)."
  (let ((reason (handler-case (boundary-scenario-reason scenario)
                  (error (condition) (format nil "input check exception: ~A" condition)))))
    (if reason
      (list :status :unresolved :reason reason)
      (handler-case (boundary-evaluate scenario)
        (error (condition)
          (list :status :unresolved :stage :transition-evaluation
                :reason (format nil "engine/evaluation exception: ~A" condition)))))))


;;;; REPORT ;;;;


(defun boundary-chain-text (chain)
  "A support chain in words."
  (format nil "~:[absent~;~:*~(~{~{~A ~A~}~^ / ~}~)~]" chain))


(defun report-boundary-prerequisites (result)
  "The prerequisite part of a BT report."
  (let ((prerequisites (getf result :prerequisites)))
    (format t "  prerequisites: ~:[NOT MET~;MET~]; engine applicability ~:[refused~;applies~]~:[ -- DISAGREES with the itemized rows~;~]~%"
            (getf prerequisites :met) (getf prerequisites :engine) (getf prerequisites :agrees))
    (dolist (row (getf prerequisites :rows))
      (format t "    ~:[not met~;met    ~]  ~A~@[: ~A~]~%" (second row) (first row) (third row)))
    (dolist (readiness (getf prerequisites :readiness))
      (format t "    info: ~(~A~) at a recorder ~:[no~;yes~], empty-handed ~:[no~;yes~], can close (recorder in its mobility) ~:[no~;yes~]~%"
              (first readiness) (second readiness) (third readiness) (fourth readiness)))))


(defun report-boundary-relation-changes (label changes)
  "One line per relation whose facts changed."
  (if changes
    (dolist (change changes)
      (format t "    ~A ~(~A~): removed ~:[none~;~:*~(~{~S~^ ~}~)~]; added ~:[none~;~:*~(~{~S~^ ~}~)~]~%"
              label (first change) (second change) (third change)))
    (format t "    ~A: no change~%" label)))


(defun report-boundary-supports (result)
  "Supports, chains and plates."
  (format t "  supports~%")
  (report-boundary-relation-changes "support" (getf result :supports))
  (dolist (row (getf result :chains))
    (unless (eq (second row) :retained)
      (format t "    ~(~A~) ~A: ~A (base ~S) -> ~A (base ~S)~%" (first row) (second row)
              (boundary-chain-text (third row)) (fifth row) (boundary-chain-text (fourth row)) (sixth row))))
  (format t "    retained chains: ~:[none~;~:*~(~{~A~^, ~}~)~]~%"
          (loop for row in (getf result :chains)
                when (eq (second row) :retained)
                  collect (format nil "~A ~A" (first row) (boundary-chain-text (third row)))))
  (format t "  plates (occupant layer; depressed physical/recording)~%")
  (dolist (row (getf result :plates))
    (format t "    ~(~A~): ~:[empty~;~:*~(~{~{~A ~A~}~^, ~}~)~] ~S -> ~:[empty~;~:*~(~{~{~A ~A~}~^, ~}~)~] ~S~%"
            (first row) (second row) (fourth row) (third row) (fifth row))))


(defun report-boundary-beams-and-devices (result)
  "Beams, receivers, devices and primitives."
  (format t "  beams~%")
  (report-boundary-relation-changes "pairing" (getf result :pairings))
  (dolist (row (getf result :receivers))
    (format t "    ~(~A~) active physical/recording ~S -> ~S~%" (first row) (second row) (third row)))
  (format t "  devices and other derived facts~%")
  (report-boundary-relation-changes "fact" (getf result :devices))
  (if (getf result :primitives)
    (dolist (entry (getf result :primitives))
      (format t "    primitive ~(~A~) ~:[off~;on~] -> ~:[off~;on~]~:[; drives no device~;; affected devices ~:*~(~{~A~^, ~}~)~]~%"
              (first entry) (second entry) (third entry) (fourth entry)))
    (format t "    no S1 primitive changed in the physical view~%"))
  (format t "  ghost objects in removed ON/HOLDING/PAIRED facts: ~:[none~;~:*~(~{~A~^, ~}~)~]~%"
          (getf result :ghosts)))


(defun report-boundary-routes (result)
  "Route conditions per agent."
  (format t "  route conditions (each agent in its own view)~%")
  (dolist (row (getf result :routes))
    (format t "    ~(~A~) (~(~A~)): ~A~@[ at ~(~A~)~]~@[ -> ~(~A~)~]~%"
            (getf row :agent) (or (getf row :side) "unmapped") (getf row :status)
            (first (getf row :locations)) (second (getf row :locations)))
    (dolist (entry (getf row :passage))
      (format t "      ~(~A~) passable ~:[no~;yes~] -> ~:[no~;yes~]~%" (first entry) (second entry) (third entry)))
    (dolist (arc (getf row :arcs-lost))
      (format t "      arc LOST   ~A~%" (service-arc-text arc)))
    (dolist (arc (getf row :arcs-gained))
      (format t "      arc GAINED ~A~%" (service-arc-text arc)))
    (format t "      mobility lost ~:[none~;~:*~(~{~A~^ ~}~)~]; gained ~:[none~;~:*~(~{~A~^ ~}~)~]~%"
            (getf row :mobility-lost) (getf row :mobility-gained))))


(defun report-boundary-obligations (result)
  "Obligation rows."
  (when (getf result :obligations)
    (format t "  obligations~%")
    (dolist (row (getf result :obligations))
      (let ((obligation (first row)))
        (format t "    ~A ~(~A~)~@[ \"~A\"~]: held ~:[no~;yes~] -> ~:[no~;yes~]  ~A~%"
                (fourth row) (getf obligation :phase) (getf obligation :purpose) (second row) (third row)
                (cond ((getf obligation :fact) (format nil "~(~S~)" (getf obligation :fact)))
                      ((getf obligation :reach) (format nil "reach ~(~{~A~^ ~}~)" (getf obligation :reach)))
                      (t (format nil "~(~A~) ~A" (getf obligation :body) (cycle-plan-role-text (getf obligation :role))))))))))


(defun report-boundary-transition (scenario)
  "Print BT for SCENARIO and return its result."
  (let ((*print-pretty* nil)
        (result (boundary-transition-result scenario)))
    (format t "~%BT  BOUNDARY AND SUPPORT TRANSITION  [supplied settled state and event]~%")
    (when (getf result :event)
      (format t "  state: ~A~%  event: ~(~S~) (~A)~%" (getf result :provenance) (getf result :event) (getf result :kind))
      (report-boundary-prerequisites result))
    (case (getf result :status)
      (:unresolved (format t "  UNRESOLVED: ~A~%" (getf result :reason)))
      (:inconsistent (format t "  INCONSISTENT: ~A~%" (getf result :reason)))
      (t
       (format t "  effects: ~A~@[~A~]~%" (getf result :effects)
               (case (getf result :effects)
                 (:hypothetical " -- the engine's closure applied to this state although its prerequisites are not met; not an available transition")
                 (t (when (member (getf result :kind) '(:stop :cancel))
                      (format nil " -- hypothetical closure ~:[DISAGREES with~;agrees with~] the engine successor"
                              (getf result :closure-agrees))))))
       (report-boundary-supports result)
       (report-boundary-beams-and-devices result)
       (report-boundary-routes result)
       (report-boundary-obligations result)
       (format t "  NOT CLAIMED: reachability of the state or of the event, a complete plan, global necessity, or availability of a HYPOTHETICAL closure.  A stable arrangement carries no guarantee through this event.~%")))
    result))
