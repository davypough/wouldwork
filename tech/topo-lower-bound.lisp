;;; Filename: topo-lower-bound.lisp

;;; Optional, domain-general delete relaxation for topology planning.  The staged problem
;;; supplies only its ordinary types, traversal facts, state, and active goal; no problem
;;; object names or coordinate regions are authored here.
;;;
;;; The relaxation is deliberately permissive:
;;;   - traversal keeps only gate prerequisites and ignores every other obstacle;
;;;   - placement keeps direct reach and support location, but ignores clearance and height;
;;;   - one beam-affecting action may activate every receiver;
;;;   - without blower propagation, normally controlled gates retain their positive DNF
;;;     plate/receiver dependencies and one action may newly depress only one plate;
;;;   - inverted gates, jammer bypasses, and blower cascades keep one-action fallbacks;
;;;   - acquiring cargo costs one action without modeling the trip to it;
;;;   - tray placement propagates location only through the retained ON support chain;
;;;   - a domain with blower drives gets one-action relocation fallbacks;
;;;   - delete effects are absent, as the relaxed heuristics require.
;;; A companion finite-domain bound keeps one current location per agent while routing the
;;; supported cargo-location goals.  Its movement, manipulation, and recorder-session costs
;;; are disjoint; tray riders, propagating relocators, unsupported goals, and happenings are
;;; omitted rather than overestimated.
;;; Serial depth-first search registers that inexpensive finite-domain term as a cheap
;;; precheck.  If it independently proves the node cannot fit under the active cutoff or
;;; incumbent, the engine prunes without evaluating the aggregate LM-cut fallback.  After
;;; an unproductive warmup, the serial search samples admitted-node fallbacks and immediately
;;; resumes eager evaluation if one uniquely prunes; parallel search remains eager.
;;; The complete static model is built once, then backward-sliced from the active goal.
;;; Per-state evaluation reads only facts used by that goal-relevant operator slice.
;;; Each omission can only shorten the abstract plan.  Unsupported goal forms are omitted
;;; from the partial model and therefore contribute zero rather than risking over-pruning.
;;;
;;; PROVIDES:
;;;   query : topo-relaxed-hmax-bound, topo-relaxed-lm-cut-bound,
;;;           topo-finite-resource-bound, topo-finite-beam-resource-bound,
;;;           topo-lm-cut-resource-bound,
;;;           min-steps-remaining? (max of LM-cut and finite-resource)
;;;   diagnostic : analyze-topo-finite-resource-bound,
;;;                report-topo-finite-resource-bound-analysis,
;;;                analyze-topo-control-setup,
;;;                report-topo-control-setup-analysis
;;;   model : registered with the engine's relaxed heuristic evaluators


(in-package :ww)


(defparameter *topo-relaxed-static-operators* nil
  "Goal-relevant abstract operators for the currently staged topology problem.")


(defparameter *topo-relaxed-all-operators* nil
  "Complete abstract operator model, compiled once for the staged topology problem.")


(defparameter *topo-relaxed-all-operators-built-p* nil
  "Whether the complete model has been built, including a legitimately empty model.")


(defparameter *topo-relaxed-static-relevance* nil
  "Backward relevance compilation for *TOPO-RELAXED-STATIC-OPERATORS*.")


(defparameter *topo-relaxed-static-indexed-model* nil
  "Integer-indexed evaluator model for the cached topology relevance slice.")


(defparameter *topo-relaxed-static-goals* nil
  "Goal facts used to compile the cached topology relevance slice.")


(defparameter *topo-beam-link-static-goals* nil
  "Goal facts used to compile the cached binary beam-link model.")


(defparameter *topo-beam-link-static-relevance* nil)


(defparameter *topo-beam-link-static-indexed-model* nil)


(defparameter *topo-resource-static-context-built-p* nil
  "Whether the finite-resource bound's static location indexes have been compiled.")


(defparameter *topo-resource-locations* nil)


(defparameter *topo-resource-reaches* nil)


(defparameter *topo-resource-routes* nil)


(defparameter *topo-relaxed-static-propositions* nil
  "Cached decoding of *STATIC-IDB* for the currently staged problem.")


(defparameter *topo-relaxed-static-propositions-built-p* nil
  "Whether the static proposition cache is compiled, including a legitimately empty one.")


(defparameter *topo-relaxed-static-positions* nil
  "Cached immutable HAS-POSITION facts of the currently staged problem.")


(defparameter *topo-relaxed-static-positions-built-p* nil
  "Whether the static HAS-POSITION cache is compiled, including a legitimately empty one.")


(defparameter *topo-relaxed-state-facts-override* nil
  "Dynamically shared state facts while evaluating the combined Topo lower bound.")


(defstruct (topo-resource-task (:conc-name topo-resource-task.))
  "One cargo-location obligation for the finite-domain routing lower bound."
  object
  required-agent
  (pickup-locations nil :type list)
  (finish-locations nil :type list)
  (manipulation-cost 0 :type (integer 0 *)))


(defstruct
    (topo-resource-bound-analysis
      (:conc-name topo-resource-bound-analysis.))
  "Diagnostic decomposition of one finite-domain Topo resource bound."
  (goals nil :type list)
  (tasks nil :type list)
  (manipulation-cost 0 :type (integer 0 *))
  (routing-cost 0 :type (integer 0 *))
  (session-cost 0 :type (integer 0 *))
  (total 0 :type (integer 0 *)))


(defstruct
    (topo-control-setup-analysis
      (:conc-name topo-control-setup-analysis.))
  "Diagnostic residual control cost after finite-resource action classes cost zero."
  resource-analysis
  (shared-effect-cost 0 :type (integer 0 *))
  (dedicated-effect-cost 0 :type (integer 0 *))
  (typed-shared-effect-cost 0 :type (integer 0 *))
  (typed-dedicated-effect-cost 0 :type (integer 0 *))
  (beam-link-cost 0 :type (integer 0 1))
  (shared-effect-steps nil :type list)
  (dedicated-effect-steps nil :type list)
  (typed-shared-effect-steps nil :type list)
  (typed-dedicated-effect-steps nil :type list)
  (typed-covered-trigger-counts nil :type list)
  (beam-provenance nil :type list)
  (beam-link-steps nil :type list))


(defparameter *topo-control-typed-trigger-facts*
  '((:topo-plate-change)
    (:topo-receiver-change)
    (:topo-relocation-change))
  "Capability-specific propagation effects used only by control/setup diagnostics.")


(define-problem-helper topo-relaxed-type-instances (type)
  (copy-list (init-type-instances type)))


(define-problem-helper topo-relaxed-object-of-type-p (object type)
  (init-type-member-p object type))


(define-problem-helper topo-relaxed-ground-form-p (form)
  (not (some (lambda (item)
               (and (symbolp item)
                    (plusp (length (symbol-name item)))
                    (member (char (symbol-name item) 0) '(#\? #\$))))
             (alexandria:flatten form))))


(define-problem-helper topo-relaxed-positive-goal-literals (form)
  "Collect supported positive ground conjuncts without interpreting quantifiers or NOT."
  (cond ((not (consp form)) nil)
        ((eq (first form) 'and)
         (mapcan #'topo-relaxed-positive-goal-literals (rest form)))
        ((member (first form) '(not or exists exist forsome forall forevery doall if)) nil)
        ((topo-relaxed-ground-form-p form) (list form))
        (t nil)))


(define-problem-helper topo-relaxed-normalize-state-proposition (proposition)
  "Map indexed HOLDING/ON storage relations back to their public abstract facts."
  (case (first proposition)
    ((holding holding1 holding2) (cons 'holding (rest proposition)))
    ((on on1 on2) (cons 'on (rest proposition)))
    (otherwise proposition)))


(define-problem-helper topo-relaxed-state-facts (state)
  (let ((facts
          (append (topo-relaxed-current-facts state)
                  (topo-relaxed-static-position-facts))))
    (when (member '(recording-in-progress) facts :test #'equal)
      (pushnew '(:recording-in-progress) facts :test #'equal))
    (when (and (fboundp 'ghost-stops-recorder)
               (funcall (symbol-function 'ghost-stops-recorder) state))
      (pushnew '(:ghost-stops-recorder) facts :test #'equal))
    facts))


(define-problem-helper topo-relaxed-current-facts (state)
  "Decode STATE's own propositions into distinct abstract facts.

LIST-DATABASE is a diagnostic printer: it conses one format string per proposition in order
to sort the whole database by relation name, and no consumer of these facts reads them in
order.  A bijective relation stores both of its indexes, so normalizing HOLDING1/HOLDING2
and ON1/ON2 back to their public form is the only source of duplicates here -- a static
relation such as HAS-POSITION cannot also appear in a state's own database -- and a hash
table removes them in linear time."
  (let ((seen (make-hash-table :test #'equal))
        (facts nil))
    (maphash
      (lambda (key value)
        (let ((fact
                (topo-relaxed-normalize-state-proposition
                  (if (eql value t)
                    (convert-to-proposition key)
                    (convert-to-fluent-proposition key value)))))
          (unless (gethash fact seen)
            (setf (gethash fact seen) t)
            (push fact facts))))
      (problem-state.idb state))
    facts))


(define-problem-helper topo-relaxed-static-position-facts ()
  "Cache the HAS-POSITION facts that staging fixes for the whole search.

Placements are the only static propositions the relaxed fact set carries, and only the
LM-cut placement operators read them; the finite-resource term reads HAS-LOCATION, HOLDING
and ON alone.  STAGE recreates *STATIC-IDB* and reloads this file, so the DEFPARAMETER above
invalidates the cache exactly as *TOPO-RESOURCE-STATIC-CONTEXT-BUILT-P* does."
  (unless *topo-relaxed-static-positions-built-p*
    (setf *topo-relaxed-static-positions*
            (remove-if-not
              (lambda (proposition)
                (eq (first proposition) 'has-position))
              (topo-relaxed-static-propositions))
          *topo-relaxed-static-positions-built-p* t))
  *topo-relaxed-static-positions*)


(define-problem-helper topo-relaxed-ghost-object-p (state object)
  (and (fboundp 'ghost-recording-object)
       (funcall (symbol-function 'ghost-recording-object) state object)))


(define-problem-helper topo-relaxed-goal-fact (state literal)
  "Translate one supported concrete goal literal into the shared abstract vocabulary."
  (case (first literal)
    (has-location
      (let ((object (second literal))
            (location (third literal)))
        (when (and (topo-relaxed-object-of-type-p location 'location)
                   (not (topo-relaxed-ghost-object-p state object))
                   (or (topo-relaxed-object-of-type-p object 'agent)
                       (topo-relaxed-object-of-type-p object 'cargo)))
          literal)))
    (holding
      (when (and (topo-relaxed-object-of-type-p (second literal) 'agent)
                 (topo-relaxed-object-of-type-p (third literal) 'cargo)
                 (not (topo-relaxed-ghost-object-p state (second literal)))
                 (not (topo-relaxed-ghost-object-p state (third literal))))
        literal))
    (on
      (when (and (topo-relaxed-object-of-type-p (second literal) 'support-occupant)
                 (topo-relaxed-object-of-type-p (third literal) 'support)
                 (not (topo-relaxed-ghost-object-p state (second literal)))
                 (not (topo-relaxed-ghost-object-p state (third literal))))
        literal))
    ((open active depressed latched) literal)
    (ghost-stops-recorder '(:ghost-stops-recorder))
    (otherwise nil)))


(define-problem-helper topo-relaxed-goal-facts (state goal)
  (remove nil
          (mapcar (lambda (literal)
                    (topo-relaxed-goal-fact state literal))
                  (topo-relaxed-positive-goal-literals goal))))


(define-problem-helper topo-relaxed-static-propositions ()
  "Cache the staged problem's static propositions; *STATIC-IDB* is fixed once staging ends.

Callers scan this list for TRAVERSE-VIA, REACH-VIA, CONTROLS and HAS-POSITION records.  The
static model builders read it once, but TOPO-BEAM-STRUCTURALLY-LINKED-P reads it on every
beam-term evaluation, and LIST-DATABASE decodes every key and conses a FORMAT string per
proposition to sort them.  The sorted list is cached exactly as LIST-DATABASE returns it, so
every caller sees the order it saw before and only the rebuilding is gone.  STAGE recreates
*STATIC-IDB* and reloads this file, so the DEFPARAMETER above invalidates the cache the same
way *TOPO-RESOURCE-STATIC-CONTEXT-BUILT-P* does."
  (unless *topo-relaxed-static-propositions-built-p*
    (setf *topo-relaxed-static-propositions*
            (list-database *static-idb*)
          *topo-relaxed-static-propositions-built-p* t))
  *topo-relaxed-static-propositions*)


(define-problem-helper topo-relaxed-propagating-relocator-p ()
  "Whether installed apparatus may move physical occupants as an action consequence."
  (some (lambda (type)
          (topo-relaxed-type-instances type))
        '(floor-gears wall-gears angled-gears
          floor-blower wall-blower angled-blower)))


(define-problem-helper topo-relaxed-gate-clause (clause)
  "Keep only named gates from a traversal obstacle clause."
  (sort (remove-if-not
          (lambda (item) (topo-relaxed-object-of-type-p item 'gate))
          (copy-list clause))
        #'string< :key #'symbol-name))


(define-problem-helper topo-relaxed-canonical-family (family)
  "Canonical antichain of gate clauses; NIL traversal payload means one empty clause."
  (let* ((raw (if family family (list nil)))
         (clauses
           (remove-duplicates
             (mapcar #'topo-relaxed-gate-clause raw)
             :test #'equal)))
    (sort
      (remove-if
        (lambda (clause)
          (some (lambda (other)
                  (and (< (length other) (length clause))
                       (subsetp other clause :test #'eq)))
                clauses))
        clauses)
      (lambda (left right)
        (or (< (length left) (length right))
            (and (= (length left) (length right))
                 (string< (format nil "~{~A~^/~}" left)
                          (format nil "~{~A~^/~}" right))))))))


(define-problem-helper topo-relaxed-family-union (left right)
  (topo-relaxed-canonical-family (append (copy-tree left) (copy-tree right))))


(define-problem-helper topo-relaxed-family-product (left right)
  "Conjoin each route alternative in LEFT with each in RIGHT."
  (when (and left right)
    (topo-relaxed-canonical-family
      (loop for left-clause in left
            append (loop for right-clause in right
                         collect (union left-clause right-clause :test #'eq))))))


(define-problem-helper topo-relaxed-traversal-record-p (proposition)
  (member (first proposition) '(traverse-via traverse-via>) :test #'eq))


(define-problem-helper topo-relaxed-add-route-family (routes from to family)
  (let* ((key (list from to))
         (old (gethash key routes))
         (new (if old
                (topo-relaxed-family-union old family)
                (topo-relaxed-canonical-family family))))
    (unless (equal old new)
      (setf (gethash key routes) new)
      t)))


(define-problem-helper topo-relaxed-route-families (locations)
  "All-pairs relaxed traversal, retaining subset-minimal gate prerequisites per route."
  (let ((routes (make-hash-table :test #'equal))
        (records
          (remove-if-not #'topo-relaxed-traversal-record-p
                         (topo-relaxed-static-propositions))))
    (dolist (location locations)
      (setf (gethash (list location location) routes) (list nil)))
    (dolist (record records)
      (destructuring-bind (relation mode from family to) record
        (declare (ignore mode))
        (topo-relaxed-add-route-family routes from to family)
        (when (eq relation 'traverse-via)
          (topo-relaxed-add-route-family routes to from family))))
    ;; A non-topological model may still include this technology.  Fully connecting it is
    ;; the safe relaxation: an omitted concrete movement schema can never be overestimated.
    (when (null records)
      (dolist (from locations)
        (dolist (to locations)
          (setf (gethash (list from to) routes) (list nil)))))
    (dolist (via locations)
      (dolist (from locations)
        (let ((left (gethash (list from via) routes)))
          (when left
            (dolist (to locations)
              (let ((right (gethash (list via to) routes)))
                (when right
                  (topo-relaxed-add-route-family
                    routes from to
                    (topo-relaxed-family-product left right)))))))))
    routes))


(define-problem-helper topo-relaxed-movement-operators
    (agents cargo locations)
  (let ((routes (topo-relaxed-route-families locations))
        (operators nil))
    (dolist (agent agents)
      (dolist (from locations)
        (dolist (to locations)
          (unless (eq from to)
            (dolist (gates (gethash (list from to) routes))
              (push
                (make-relaxed-hmax-operator
                  :name (list 'move agent from to gates)
                  :preconditions
                    (cons (list 'has-location agent from)
                          (mapcar (lambda (gate) (list 'open gate)) gates))
                  :effects (list (list 'has-location agent to)))
                operators)))))
      ;; A held tray keeps its location synchronized with its holder.  The zero-cost
      ;; consequence is attached to the already-counted MOVE fact rather than charging a
      ;; second action.  Other cargo requires an explicit placement in the relaxation.
      (dolist (tray (intersection cargo (topo-relaxed-type-instances 'tray) :test #'eq))
        (dolist (location locations)
          (push
            (make-relaxed-hmax-operator
              :name (list 'carried-tray-location agent tray location)
              :cost 0
              :preconditions (list (list 'holding agent tray)
                                   (list 'has-location agent location))
              :effects (list (list 'has-location tray location)))
            operators))))
    operators))


(define-problem-helper topo-relaxed-add-reach-clause
    (reaches from target barriers)
  (pushnew (topo-relaxed-gate-clause barriers)
           (gethash (list from target) reaches)
           :test #'equal))


(define-problem-helper topo-relaxed-reach-clauses (locations)
  "Direct manipulation reach from an agent location to a target location."
  (let ((reaches (make-hash-table :test #'equal)))
    (dolist (location locations)
      (setf (gethash (list location location) reaches) (list nil)))
    (dolist (record (topo-relaxed-static-propositions))
      (when (member (first record) '(reach-via reach-via>) :test #'eq)
        (destructuring-bind (relation left barriers right) record
          (if (eq relation 'reach-via)
            (progn
              (topo-relaxed-add-reach-clause reaches left right barriers)
              (topo-relaxed-add-reach-clause reaches right left barriers))
            (topo-relaxed-add-reach-clause reaches left right barriers)))))
    reaches))


(define-problem-helper topo-resource-object-location (object facts)
  (third
    (find object facts
          :key (lambda (fact)
                 (when (eq (first fact) 'has-location)
                   (second fact)))
          :test #'eq)))


(define-problem-helper topo-resource-object-holder (object facts)
  (second
    (find object facts
          :key (lambda (fact)
                 (when (eq (first fact) 'holding)
                   (third fact)))
          :test #'eq)))


(define-problem-helper topo-resource-object-support (object facts)
  (third
    (find object facts
          :key (lambda (fact)
                 (when (eq (first fact) 'on)
                   (second fact)))
          :test #'eq)))


(define-problem-helper topo-resource-tray-supported-p (object facts)
  "Whether OBJECT can inherit a tray's movement without its own manipulation actions."
  (let ((support (topo-resource-object-support object facts))
        (visited nil))
    (loop while support
          do (when (topo-relaxed-object-of-type-p support 'tray)
               (return t))
             (when (member support visited :test #'eq)
               (return nil))
             (push support visited)
             (setf support (topo-resource-object-support support facts)))))


(define-problem-helper topo-resource-reach-vantages
    (target locations reaches)
  (loop for location in locations
        when (gethash (list location target) reaches)
          collect location))


(define-problem-helper topo-resource-location-task
    (goal facts locations reaches)
  (let ((object (second goal))
        (target (third goal)))
    (unless (or (member goal facts :test #'equal)
                (not (topo-relaxed-object-of-type-p object 'cargo))
                (topo-resource-tray-supported-p object facts))
      (let* ((holder (topo-resource-object-holder object facts))
             (object-location (topo-resource-object-location object facts))
             (holder-location
               (when holder
                 (topo-resource-object-location holder facts)))
             (tray-p (topo-relaxed-object-of-type-p object 'tray))
             (pickup-locations
               (if holder
                 (and holder-location (list holder-location))
                 (and object-location
                      (topo-resource-reach-vantages
                        object-location locations reaches))))
             (finish-locations
               (if tray-p
                 (list target)
                 (topo-resource-reach-vantages target locations reaches))))
        (when (and pickup-locations finish-locations)
          (make-topo-resource-task
            :object object
            :required-agent holder
            :pickup-locations pickup-locations
            :finish-locations finish-locations
            :manipulation-cost
              (if tray-p
                (if holder 0 1)
                (if holder 1 2))))))))


(define-problem-helper topo-resource-location-tasks
    (goals facts locations reaches)
  (unless (topo-relaxed-propagating-relocator-p)
    (remove-duplicates
      (remove nil
        (loop for goal in goals
              when (and (consp goal) (eq (first goal) 'has-location))
                collect
                  (topo-resource-location-task
                    goal facts locations reaches)))
      :key #'topo-resource-task.object
      :test #'eq)))


(define-problem-helper topo-resource-ensure-static-context ()
  (unless *topo-resource-static-context-built-p*
    (setf *topo-resource-locations*
            (topo-relaxed-type-instances 'location)
          *topo-resource-reaches*
            (topo-relaxed-reach-clauses *topo-resource-locations*)
          *topo-resource-routes*
            (topo-relaxed-route-families *topo-resource-locations*)
          *topo-resource-static-context-built-p* t))
  (values *topo-resource-locations*
          *topo-resource-reaches*
          *topo-resource-routes*))


(define-problem-helper topo-resource-located-agents (facts)
  (loop for agent in (topo-relaxed-type-instances 'agent)
        for location = (topo-resource-object-location agent facts)
        when location
          collect (cons agent location)))


(define-problem-helper topo-resource-move-cost (from to routes)
  ;; A missing static route may become available through a capability this partial model
  ;; omits.  Charge zero rather than treating that uncertainty as an impossible task.
  (if (or (eq from to)
          (null (gethash (list from to) routes)))
    0
    1))


(define-problem-helper topo-resource-replace-position
    (positions index location)
  (loop for position in positions
        for position-index from 0
        collect (if (= position-index index) location position)))


(define-problem-helper topo-resource-task-transition-cost
    (from pickup finish routes)
  (+ (topo-resource-move-cost from pickup routes)
     (topo-resource-move-cost pickup finish routes)))


(define-problem-helper topo-resource-routing-search
    (tasks agents positions routes memo)
  (when (null tasks)
    (return-from topo-resource-routing-search 0))
  (let ((key
          (list (mapcar #'topo-resource-task.object tasks)
                positions)))
    (multiple-value-bind (cached present-p) (gethash key memo)
      (when present-p
        (return-from topo-resource-routing-search cached)))
    (let ((best nil))
      (dolist (task tasks)
        (let ((remaining (remove task tasks :count 1 :test #'eq)))
          (loop for agent in agents
                for from in positions
                for index from 0
                do (when (or (null (topo-resource-task.required-agent task))
                             (eq agent
                                 (topo-resource-task.required-agent task)))
                     (dolist (pickup
                              (topo-resource-task.pickup-locations task))
                       (dolist (finish
                                (topo-resource-task.finish-locations task))
                         (let* ((next-positions
                                  (topo-resource-replace-position
                                    positions index finish))
                                (cost
                                  (+
                                    (topo-resource-task-transition-cost
                                      from pickup finish routes)
                                    (topo-resource-routing-search
                                      remaining agents next-positions
                                      routes memo))))
                           (setf best (if best (min best cost) cost)))))))))
      ;; No located eligible agent means this partial abstraction abstains from routing.
      (setf (gethash key memo) (or best 0)))))


(define-problem-helper topo-resource-routing-cost (tasks facts routes)
  (let ((located-agents (topo-resource-located-agents facts)))
    (if (or (null tasks) (null located-agents))
      0
      (topo-resource-routing-search
        tasks
        (mapcar #'car located-agents)
        (mapcar #'cdr located-agents)
        routes
        (make-hash-table :test #'equal)))))


(define-problem-helper topo-resource-session-cost (goals facts)
  (if (and (member '(:ghost-stops-recorder) goals :test #'equal)
           (not (member '(:ghost-stops-recorder) facts :test #'equal)))
    (if (member '(:recording-in-progress) facts :test #'equal) 1 2)
    0))


(define-problem-helper topo-finite-resource-bound-components-from-facts
    (state goal facts)
  "Return manipulation, routing, and session costs plus their goals and tasks."
  (multiple-value-bind (locations reaches routes)
      (topo-resource-ensure-static-context)
    (let* ((goals (topo-relaxed-goal-facts state goal))
           (tasks
             (topo-resource-location-tasks
               goals facts locations reaches))
           (manipulation-cost
             (reduce #'+ tasks
                     :key #'topo-resource-task.manipulation-cost
                     :initial-value 0))
           (routing-cost
             (topo-resource-routing-cost tasks facts routes))
           (session-cost
             (topo-resource-session-cost goals facts)))
      (values manipulation-cost routing-cost session-cost goals tasks))))


(define-problem-helper topo-finite-resource-bound-from-facts
    (state goal facts)
  (multiple-value-bind
      (manipulation-cost routing-cost session-cost goals tasks)
      (topo-finite-resource-bound-components-from-facts state goal facts)
    (declare (ignore goals tasks))
    (+ manipulation-cost routing-cost session-cost)))


(define-problem-helper topo-finite-resource-bound-analysis-from-facts
    (state goal facts)
  "Return the component record corresponding exactly to the numeric resource bound."
  (multiple-value-bind
      (manipulation-cost routing-cost session-cost goals tasks)
      (topo-finite-resource-bound-components-from-facts state goal facts)
    (make-topo-resource-bound-analysis
      :goals goals
      :tasks tasks
      :manipulation-cost manipulation-cost
      :routing-cost routing-cost
      :session-cost session-cost
      :total (+ manipulation-cost routing-cost session-cost))))


(define-problem-helper analyze-topo-finite-resource-bound (state goal)
  "Return a read-only decomposition of the finite-domain Topo resource bound."
  (if *happening-names*
    (make-topo-resource-bound-analysis)
    (topo-finite-resource-bound-analysis-from-facts
      state goal (topo-relaxed-state-facts state))))


(define-problem-helper report-topo-finite-resource-bound-analysis
    (state goal &optional (stream t))
  "Print the finite-resource components and retained cargo obligations."
  (let ((analysis (analyze-topo-finite-resource-bound state goal)))
    (format stream
            "~&Finite-resource total = ~:D: manipulation ~:D, routing ~:D, session ~:D.~%"
            (topo-resource-bound-analysis.total analysis)
            (topo-resource-bound-analysis.manipulation-cost analysis)
            (topo-resource-bound-analysis.routing-cost analysis)
            (topo-resource-bound-analysis.session-cost analysis))
    (dolist (task (topo-resource-bound-analysis.tasks analysis))
      (format stream
              "  ~S: agent ~S, pickup ~S, finish ~S, manipulation ~:D.~%"
              (topo-resource-task.object task)
              (topo-resource-task.required-agent task)
              (topo-resource-task.pickup-locations task)
              (topo-resource-task.finish-locations task)
              (topo-resource-task.manipulation-cost task)))
    analysis))


(define-problem-helper topo-control-setup-task-object-p
    (object resource-analysis)
  (some (lambda (task)
          (and (plusp (topo-resource-task.manipulation-cost task))
               (eq object (topo-resource-task.object task))))
        (topo-resource-bound-analysis.tasks resource-analysis)))


(define-problem-helper topo-control-setup-covered-operator-p
    (operator resource-analysis)
  "Whether the finite bound already charges the abstract operator's action class."
  (let* ((name (relaxed-hmax-operator.name operator))
         (family (if (consp name) (first name) name)))
    (case family
      (move
        (plusp (topo-resource-bound-analysis.routing-cost resource-analysis)))
      ((pickup put-ground put-on-at)
        (topo-control-setup-task-object-p (third name) resource-analysis))
      ((relaxed-start-recorder relaxed-stop-recorder)
        (plusp (topo-resource-bound-analysis.session-cost resource-analysis)))
      (otherwise nil))))


(define-problem-helper topo-control-setup-partitioned-operator
    (operator resource-analysis share-covered-effects-p)
  (let ((copy (copy-relaxed-hmax-operator operator)))
    (when (topo-control-setup-covered-operator-p operator resource-analysis)
      (setf (relaxed-hmax-operator.cost copy) 0)
      (unless share-covered-effects-p
        (setf (relaxed-hmax-operator.effects copy)
              (remove '(:topo-action-taken)
                      (relaxed-hmax-operator.effects copy)
                      :test #'equal))))
    copy))


(define-problem-helper topo-control-setup-partitioned-operators
    (operators resource-analysis share-covered-effects-p)
  (mapcar
    (lambda (operator)
      (topo-control-setup-partitioned-operator
        operator resource-analysis share-covered-effects-p))
    operators))


(define-problem-helper topo-control-setup-lm-cut-analysis
    (facts operators goals resource-analysis share-covered-effects-p)
  (let* ((partitioned
           (topo-control-setup-partitioned-operators
             operators resource-analysis share-covered-effects-p))
         (indexed
           (compile-relaxed-indexed-model partitioned goals :validate nil)))
    (relaxed-indexed-lm-cut-analysis
      facts indexed :ignore-unreachable t)))


(define-problem-helper topo-control-typed-trigger-precondition
    (operator)
  (let* ((name (relaxed-hmax-operator.name operator))
         (family (if (consp name) (first name) name)))
    (case family
      (relaxed-activate '(:topo-receiver-change))
      (propagated-plate-change '(:topo-plate-change))
      ((propagated-relocation propagated-landing)
        '(:topo-relocation-change))
      (otherwise nil))))


(define-problem-helper topo-control-typed-beam-blocker-p (object)
  (and object
       (topo-relaxed-object-of-type-p object 'beam-blocker)))


(define-problem-helper topo-control-typed-trigger-effects
    (operator)
  "Conservative controller effects supplied by the installed abstract capability."
  (let* ((name (relaxed-hmax-operator.name operator))
         (family (if (consp name) (first name) name)))
    (case family
      (move
        (when (topo-control-typed-beam-blocker-p (second name))
          (list '(:topo-receiver-change))))
      ((pickup put-ground put-on-at)
        (when (topo-control-typed-beam-blocker-p (third name))
          (list '(:topo-receiver-change))))
      (relaxed-change-plate
        (list '(:topo-plate-change) '(:topo-receiver-change)))
      ((normal-controls-open relaxed-open-fallback plate-consequence)
        (list '(:topo-receiver-change)))
      ((relaxed-start-recorder relaxed-stop-recorder)
        (list '(:topo-receiver-change)))
      (relaxed-any-action
        (copy-list *topo-control-typed-trigger-facts*))
      (otherwise nil))))


(define-problem-helper topo-control-typed-effects-without-triggers
    (effects)
  (remove-if
    (lambda (effect)
      (or (equal effect '(:topo-action-taken))
          (member effect *topo-control-typed-trigger-facts* :test #'equal)))
    effects))


(define-problem-helper topo-control-typed-preconditions
    (operator)
  (let ((replacement
          (topo-control-typed-trigger-precondition operator)))
    (mapcar
      (lambda (precondition)
        (if (equal precondition '(:topo-action-taken))
          (or replacement precondition)
          precondition))
      (relaxed-hmax-operator.preconditions operator))))


(define-problem-helper topo-control-setup-typed-operator
    (operator resource-analysis share-covered-effects-p)
  (let* ((copy (copy-relaxed-hmax-operator operator))
         (explicit-effects
           (topo-control-typed-effects-without-triggers
             (relaxed-hmax-operator.effects operator)))
         (trigger-effects
           (topo-control-typed-trigger-effects operator))
         (covered-p
           (topo-control-setup-covered-operator-p
             operator resource-analysis)))
    (setf (relaxed-hmax-operator.preconditions copy)
          (topo-control-typed-preconditions operator)
          (relaxed-hmax-operator.effects copy)
          (append explicit-effects
                  (unless (and covered-p
                               (not share-covered-effects-p))
                    trigger-effects)))
    (when covered-p
      (setf (relaxed-hmax-operator.cost copy) 0))
    copy))


(define-problem-helper topo-control-setup-typed-operators
    (operators resource-analysis share-covered-effects-p)
  (mapcar
    (lambda (operator)
      (topo-control-setup-typed-operator
        operator resource-analysis share-covered-effects-p))
    operators))


(define-problem-helper topo-control-setup-typed-lm-cut-analysis
    (facts operators goals resource-analysis share-covered-effects-p)
  (let* ((partitioned
           (topo-control-setup-typed-operators
             operators resource-analysis share-covered-effects-p))
         (indexed
           (compile-relaxed-indexed-model partitioned goals :validate nil)))
    (relaxed-indexed-lm-cut-analysis
      facts indexed :ignore-unreachable t)))


(define-problem-helper topo-control-typed-trigger-family
    (operator effect)
  (let ((name (relaxed-hmax-operator.name operator)))
    (list (if (consp name) (first name) name) effect)))


(define-problem-helper topo-control-typed-covered-trigger-counts
    (operators resource-analysis)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (operator operators)
      (when (topo-control-setup-covered-operator-p
              operator resource-analysis)
        (dolist (effect (topo-control-typed-trigger-effects operator))
          (incf
            (gethash
              (topo-control-typed-trigger-family operator effect)
              counts
              0)))))
    (sort
      (loop for key being the hash-keys of counts using (hash-value count)
            collect (cons key count))
      #'> :key #'cdr)))


(define-problem-helper topo-beam-relation-facts
    (relation facts)
  (remove-if-not
    (lambda (fact)
      (and (consp fact) (eq (first fact) relation)))
    facts))


(define-problem-helper topo-beam-neighbors
    (node paired coupled)
  "Return a permissive structural beam graph, ignoring hue and corridor clearance."
  (let ((neighbors nil))
    (dolist (fact paired)
      (when (eq node (second fact))
        (pushnew (third fact) neighbors :test #'eq))
      (when (eq node (third fact))
        (pushnew (second fact) neighbors :test #'eq)))
    (dolist (fact coupled)
      (when (eq node (second fact))
        (pushnew (third fact) neighbors :test #'eq)))
    neighbors))


(define-problem-helper topo-beam-structurally-linked-p
    (receiver facts &optional
                    (transmitters (topo-relaxed-type-instances 'transmitter)))
  "Whether authored and current links contain any transmitter-to-receiver path.

PAIRED is structurally undirected and COUPLED is directional, matching beam-relay's
link semantics.  Ignoring hue, visibility, and blockers makes a positive answer deliberately
permissive: geometry can then change receiver status without another link action."
  (let* ((all-facts
           (union facts (topo-relaxed-static-propositions) :test #'equal))
         (paired (topo-beam-relation-facts 'paired all-facts))
         (coupled (topo-beam-relation-facts 'coupled all-facts))
         (frontier (copy-list transmitters))
         (visited nil))
    (loop while frontier
          for node = (pop frontier)
          do (when (eq node receiver)
               (return-from topo-beam-structurally-linked-p t))
             (unless (member node visited :test #'eq)
               (push node visited)
               (dolist (neighbor
                         (topo-beam-neighbors node paired coupled))
                 (unless (member neighbor visited :test #'eq)
                   (push neighbor frontier)))))
    nil))


(define-problem-helper topo-beam-charged-connector-p
    (resource-analysis)
  "Whether CONNECT could overlap a connector placement already charged by the resource term."
  (some
    (lambda (task)
      (and (plusp (topo-resource-task.manipulation-cost task))
           (topo-relaxed-object-of-type-p
             (topo-resource-task.object task) 'connector)))
    (topo-resource-bound-analysis.tasks resource-analysis)))


(define-problem-helper topo-control-beam-link-status
    (receiver facts resource-analysis)
  (cond
    ((member (list 'active receiver) facts :test #'equal)
     :active)
    ((topo-beam-structurally-linked-p receiver facts)
     :linked)
    ((or *happening-names*
         (not (member "beam-relay" *spliced-tech-names* :test #'string=))
         (not (topo-relaxed-update-installed-p 'update-relay-status!))
         (not (topo-relaxed-update-installed-p 'update-receiver-status!)))
     :unsupported-provider)
    ((null (topo-relaxed-type-instances 'connector))
     :no-connector)
    ((topo-beam-charged-connector-p resource-analysis)
     :resource-overlap)
    (t
     :link-required)))


(define-problem-helper topo-control-beam-provenance
    (facts resource-analysis)
  (mapcar
    (lambda (receiver)
      (list receiver
            (topo-control-beam-link-status
              receiver facts resource-analysis)))
    (topo-relaxed-type-instances 'receiver)))


(define-problem-helper topo-control-beam-link-operator
    (operator link-required-receivers)
  (let* ((copy (copy-relaxed-hmax-operator operator))
         (name (relaxed-hmax-operator.name operator))
         (family (if (consp name) (first name) name)))
    ;; This diagnostic asks only whether link construction is unavoidable.  Every other
    ;; abstract action is free, and already-linked or unsupported receivers are activated
    ;; for free.  Consequently the result is binary and cannot absorb unrelated setup cost.
    (setf (relaxed-hmax-operator.cost copy) 0)
    (when (eq family 'relaxed-activate)
      (setf (relaxed-hmax-operator.preconditions copy)
            (when (member (second name) link-required-receivers :test #'eq)
              (list '(:topo-beam-link-change)))))
    copy))


(define-problem-helper topo-control-beam-link-operators
    (operators link-required-receivers)
  (let ((copies
          (mapcar
            (lambda (operator)
              (topo-control-beam-link-operator
                operator link-required-receivers))
            operators)))
    (when link-required-receivers
      (push
        (make-relaxed-hmax-operator
          :name 'relaxed-connect-beam-link
          :effects (list '(:topo-beam-link-change)))
        copies))
    copies))


(define-problem-helper topo-control-beam-link-lm-cut-analysis
    (facts operators goals beam-provenance)
  "Return one only when every relaxed route needs a receiver with no structural link.

One shared link effect lets one CONNECT stand in for every missing receiver, which is safe
even when the concrete pairing limit would require more.  Unknown providers and possible
overlap with a resource-charged connector are classified free before this model is built."
  (let* ((required
           (loop for (receiver status) in beam-provenance
                 when (eq status :link-required)
                   collect receiver))
         (provenance-operators
           (topo-control-beam-link-operators operators required))
         (indexed
           (compile-relaxed-indexed-model
             provenance-operators goals :validate nil)))
    (relaxed-indexed-lm-cut-analysis
      facts indexed :ignore-unreachable t)))


(define-problem-helper topo-beam-link-activation-operator
    (operator precondition suffix)
  (let* ((copy (copy-relaxed-hmax-operator operator))
         (name (relaxed-hmax-operator.name operator)))
    (setf (relaxed-hmax-operator.name copy)
            (list (first name) suffix (second name))
          (relaxed-hmax-operator.cost copy) 0
          (relaxed-hmax-operator.preconditions copy)
            (list precondition))
    copy))


(define-problem-helper topo-beam-link-static-operator-copies
    (operator)
  "Make every non-link action free and split receiver activation by link provenance."
  (let* ((name (relaxed-hmax-operator.name operator))
         (family (if (consp name) (first name) name)))
    (if (eq family 'relaxed-activate)
      (list
        (topo-beam-link-activation-operator
          operator
          (list :topo-beam-linked (second name))
          'existing-link)
        (topo-beam-link-activation-operator
          operator
          '(:topo-beam-link-change)
          'new-link))
      (let ((copy (copy-relaxed-hmax-operator operator)))
        (setf (relaxed-hmax-operator.cost copy) 0)
        (list copy)))))


(define-problem-helper topo-beam-link-static-operators
    (operators)
  (let ((copies nil))
    (dolist (operator operators)
      (setf copies
            (nconc
              (topo-beam-link-static-operator-copies operator)
              copies)))
    (push
      (make-relaxed-hmax-operator
        :name 'relaxed-connect-beam-link
        :effects (list '(:topo-beam-link-change)))
      copies)
    copies))


(define-problem-helper topo-beam-link-bound-supported-p
    (resource-analysis)
  "Whether one relay link action is known to be separate from every resource charge."
  (and (null *happening-names*)
       (member "beam-relay" *spliced-tech-names* :test #'string=)
       (topo-relaxed-update-installed-p 'update-relay-status!)
       (topo-relaxed-update-installed-p 'update-receiver-status!)
       (topo-relaxed-type-instances 'connector)
       (not (topo-beam-charged-connector-p resource-analysis))))


(define-problem-helper topo-beam-link-current-facts
    (facts)
  (append
    facts
    (loop for receiver in (topo-relaxed-type-instances 'receiver)
          when (topo-beam-structurally-linked-p receiver facts)
            collect (list :topo-beam-linked receiver))))


(define-problem-helper analyze-topo-control-setup
    (state goal)
  "Measure residual setup cost without making it a production pruning term.

SHARED-EFFECT-COST lets resource actions retain generic propagation side effects.
DEDICATED-EFFECT-COST removes only their :TOPO-ACTION-TAKEN effect, exposing the cost
which depends on assuming a separate controller-changing action.  The latter is a candidate,
not an admissible bound, until the concrete capability proves that separation."
  (let* ((resource-analysis
           (analyze-topo-finite-resource-bound state goal))
         (model (build-topo-relaxed-hmax-model state goal)))
    (if (null model)
      (make-topo-control-setup-analysis
        :resource-analysis resource-analysis)
      (let* ((facts (relaxed-hmax-model.facts model))
             (operators (relaxed-hmax-model.operators model))
             (goals (relaxed-hmax-model.goals model))
             (shared
               (multiple-value-list
                 (topo-control-setup-lm-cut-analysis
                   facts operators goals resource-analysis t)))
             (dedicated
               (multiple-value-list
                 (topo-control-setup-lm-cut-analysis
                   facts operators goals resource-analysis nil)))
             (typed-shared
               (multiple-value-list
                 (topo-control-setup-typed-lm-cut-analysis
                   facts operators goals resource-analysis t)))
             (typed-dedicated
               (multiple-value-list
                 (topo-control-setup-typed-lm-cut-analysis
                   facts operators goals resource-analysis nil)))
             (beam-provenance
               (topo-control-beam-provenance facts resource-analysis))
             (beam-link
               (multiple-value-list
                 (topo-control-beam-link-lm-cut-analysis
                   facts operators goals beam-provenance))))
        (make-topo-control-setup-analysis
          :resource-analysis resource-analysis
          :shared-effect-cost (first shared)
          :dedicated-effect-cost (first dedicated)
          :typed-shared-effect-cost (first typed-shared)
          :typed-dedicated-effect-cost (first typed-dedicated)
          :beam-link-cost (first beam-link)
          :shared-effect-steps (second shared)
          :dedicated-effect-steps (second dedicated)
          :typed-shared-effect-steps (second typed-shared)
          :typed-dedicated-effect-steps (second typed-dedicated)
          :typed-covered-trigger-counts
            (topo-control-typed-covered-trigger-counts
              operators resource-analysis)
          :beam-provenance beam-provenance
          :beam-link-steps (second beam-link))))))


(define-problem-helper report-topo-control-setup-analysis
    (state goal &optional (stream t))
  "Print the residual setup costs and beam-link provenance decision."
  (let* ((analysis (analyze-topo-control-setup state goal))
         (resource
           (topo-control-setup-analysis.resource-analysis analysis)))
    (format stream
            "~&Finite resource = ~D; generic shared/dedicated = ~D/~D; typed shared/dedicated = ~D/~D; required beam link = ~D; provenance candidate total = ~D.~%"
            (topo-resource-bound-analysis.total resource)
            (topo-control-setup-analysis.shared-effect-cost analysis)
            (topo-control-setup-analysis.dedicated-effect-cost analysis)
            (topo-control-setup-analysis.typed-shared-effect-cost analysis)
            (topo-control-setup-analysis.typed-dedicated-effect-cost analysis)
            (topo-control-setup-analysis.beam-link-cost analysis)
            (+ (topo-resource-bound-analysis.total resource)
               (topo-control-setup-analysis.beam-link-cost analysis)))
    (dolist (entry
              (topo-control-setup-analysis.typed-covered-trigger-counts analysis))
      (format stream "  Covered ~S can supply ~S: ~D operator~:P.~%"
              (first (car entry)) (second (car entry)) (cdr entry)))
    (dolist (entry (topo-control-setup-analysis.beam-provenance analysis))
      (format stream "  Receiver ~S beam provenance: ~S.~%"
              (first entry) (second entry)))
    (report-relaxed-lm-cut-analysis
      (topo-control-setup-analysis.beam-link-cost analysis)
      (topo-control-setup-analysis.beam-link-steps analysis)
      stream)
    analysis))


(define-problem-helper topo-finite-resource-bound-for (state goal)
  "Add disjoint manipulation, movement, and recorder-session lower-bound costs."
  (if *happening-names*
    0
    (topo-finite-resource-bound-from-facts
      state goal (topo-relaxed-state-facts state))))


(define-problem-helper topo-finite-beam-resource-bound-from-facts
    (state goal facts)
  (let ((resource
          (topo-finite-resource-bound-analysis-from-facts
            state goal facts)))
    (+ (topo-resource-bound-analysis.total resource)
       (topo-beam-link-bound-from-facts
         state goal facts resource))))


(define-problem-helper topo-finite-beam-resource-bound-for (state goal)
  "Add a disjoint required relay-link step to the finite-resource lower bound."
  (if *happening-names*
    0
    (topo-finite-beam-resource-bound-from-facts
      state goal (topo-relaxed-state-facts state))))


(define-problem-helper topo-lm-cut-resource-bound-for (state goal)
  "Take the stronger Topo estimate while sharing one extraction of concrete state facts."
  (if *happening-names*
    0
    (let* ((facts (topo-relaxed-state-facts state))
           (*topo-relaxed-state-facts-override* facts))
      (max (registered-relaxed-lm-cut-bound state goal)
           (topo-finite-resource-bound-from-facts
             state goal facts)))))


(define-problem-helper topo-relaxed-placement-operators
    (agents cargo supports locations)
  "Ground and support placements reachable in one concrete manipulation action."
  (let ((reaches (topo-relaxed-reach-clauses locations))
        (operators nil))
    (dolist (agent agents)
      (dolist (object cargo)
        (dolist (from locations)
          (dolist (target locations)
            (dolist (gates (gethash (list from target) reaches))
              (let ((preconditions
                      (append
                        (list (list 'holding agent object)
                              (list 'has-location agent from))
                        (mapcar (lambda (gate) (list 'open gate)) gates))))
                (push
                  (make-relaxed-hmax-operator
                    :name (list 'put-ground agent object from target gates)
                    :preconditions preconditions
                    :effects (list (list 'has-location object target)))
                  operators)
                (dolist (support supports)
                  ;; Movable supports publish HAS-LOCATION; fixed supports publish
                  ;; HAS-POSITION.  Both alternatives retain the shared HAS-LOCATION/ON
                  ;; effects of one concrete placement for LM-cut.
                  (dolist (support-relation '(has-location has-position))
                    (push
                      (make-relaxed-hmax-operator
                        :name
                          (list 'put-on-at agent object support from target
                                gates support-relation)
                        :preconditions
                          (append preconditions
                                  (list
                                    (list support-relation support target)))
                        :effects (list (list 'has-location object target)
                                       (list 'on object support)))
                      operators)))))))))
    operators))


(define-problem-helper topo-relaxed-plate-facts (plate)
  (cond ((topo-relaxed-object-of-type-p plate 'toggle-plate)
         (list (list 'depressed plate) (list 'latched plate)))
        ((topo-relaxed-object-of-type-p plate 'pressure-plate)
         (list (list 'depressed plate)))
        (t
         (error "Plate has neither concrete plate type: ~S" plate))))


(define-problem-helper topo-relaxed-update-installed-p
    (name &optional (update-names *update-names*))
  (member name update-names :test #'eq))


(define-problem-helper topo-relaxed-independent-plate-changes-p
    (&optional (update-names *update-names*))
  "Whether one ordinary action can newly depress at most one plate."
  (and (topo-relaxed-update-installed-p
         'update-plate-status! update-names)
       (not (topo-relaxed-propagating-relocator-p))))


(define-problem-helper topo-relaxed-controls-record (device)
  (find device
        (topo-relaxed-static-propositions)
        :key (lambda (proposition)
               (when (eq (first proposition) 'controls)
                 (third proposition)))
        :test #'eq))


(define-problem-helper topo-relaxed-controller-fact (controller)
  (cond ((topo-relaxed-object-of-type-p controller 'receiver)
         (list 'active controller))
        ((topo-relaxed-object-of-type-p controller 'pressure-plate)
         (list 'depressed controller))
        ((topo-relaxed-object-of-type-p controller 'toggle-plate)
         (list 'latched controller))
        (t
         (error "Unsupported relaxed gate controller: ~S" controller))))


(define-problem-helper topo-relaxed-gate-control-operators
    (gates &optional (update-names *update-names*))
  "Positive normal-control derivations plus safe inverted/jammer fallbacks."
  (let ((operators nil)
        (jammers (topo-relaxed-type-instances 'jammer)))
    (dolist (gate gates)
      (let ((record (topo-relaxed-controls-record gate)))
        (when (and record
                   (eq (fourth record) 'normal)
                   (topo-relaxed-update-installed-p
                     'update-gate-status! update-names))
          (dolist (clause (second record))
            (push
              (make-relaxed-hmax-operator
                :name (list 'normal-controls-open gate clause)
                :cost 0
                :preconditions (mapcar #'topo-relaxed-controller-fact clause)
                :effects (list (list 'open gate)))
              operators)))
        ;; An inverted gate needs a delete-aware controller model.  A jammer can bypass
        ;; either polarity.  Outside the public gate technology, custom OPEN writers are
        ;; unknown.  In each case an unconstrained one-action achiever is the safe bound.
        (when (or (not (topo-relaxed-update-installed-p
                         'update-gate-status! update-names))
                  jammers
                  (and record (eq (fourth record) 'inverted)))
          (push
            (make-relaxed-hmax-operator
              :name (list 'relaxed-open-fallback gate)
              :effects (list (list 'open gate)))
            operators))))
    operators))


(define-problem-helper topo-relaxed-device-operators
    (plates receivers gates)
  (let ((operators (topo-relaxed-gate-control-operators gates)))
    (if (topo-relaxed-independent-plate-changes-p)
      (dolist (plate plates)
        (push
          (make-relaxed-hmax-operator
            :name (list 'relaxed-change-plate plate)
            :effects (topo-relaxed-plate-facts plate))
          operators))
      ;; Custom plate writers and blower cascades may change several plates together.
      (dolist (plate plates)
        (dolist (fact (topo-relaxed-plate-facts plate))
          (push
            (make-relaxed-hmax-operator
              :name (list 'propagated-plate-change fact)
              :cost 0
              :preconditions (list '(:topo-action-taken))
              :effects (list fact))
            operators))))
    ;; Moving one beam-affecting object can change several receivers together.  The shared
    ;; trigger also lets that receiver change share the cost of its concrete action.
    (dolist (receiver receivers)
      (push
        (make-relaxed-hmax-operator
          :name (list 'relaxed-activate receiver)
          :cost 0
          :preconditions (list '(:topo-action-taken))
          :effects (list (list 'active receiver)))
        operators))
    (push
      (make-relaxed-hmax-operator
        :name 'relaxed-any-action
        :effects (list '(:topo-action-taken)))
      operators)
    operators))


(define-problem-helper topo-relaxed-manipulation-operators
    (agents cargo supports plates receivers gates locations)
  (let ((operators
          (append
            (topo-relaxed-placement-operators
              agents cargo supports locations)
            (topo-relaxed-device-operators plates receivers gates))))
    (dolist (agent agents)
      (dolist (object cargo)
        ;; The relaxation deliberately omits the acquisition trip and reach test.
        (push
          (make-relaxed-hmax-operator
            :name (list 'pickup agent object)
            :effects (list (list 'holding agent object)))
          operators)))
    ;; Location inherited through a support chain is a consequence, not another action.
    ;; This also makes the relaxation safe for occupants riding on a carried tray.
    (dolist (occupant (topo-relaxed-type-instances 'support-occupant))
      (dolist (support supports)
        (dolist (location locations)
          (push
            (make-relaxed-hmax-operator
              :name (list 'supported-location occupant support location)
              :cost 0
              :preconditions (list (list 'on occupant support)
                                   (list 'has-location support location))
              :effects (list (list 'has-location occupant location)))
            operators))))
    ;; A tray placement's ordinary HAS-LOCATION effect feeds the zero-cost
    ;; SUPPORTED-LOCATION consequences above.  Under delete relaxation the ON chain remains,
    ;; so one placement relocates every actual rider jointly without an unconstrained
    ;; tray-release fallback for occupants which are not on the tray.
    ;; Floor, wall, and angled streams can relocate a stack when an otherwise unrelated
    ;; action changes their controller.  Their potentially joint cascade remains tied to
    ;; one shared action trigger even though ordinary plate/gate control is modeled below.
    (when (topo-relaxed-propagating-relocator-p)
      (dolist (object (union agents cargo :test #'eq))
        (dolist (location locations)
          (push
            (make-relaxed-hmax-operator
              :name (list 'propagated-relocation object location)
              :cost 0
              :preconditions (list '(:topo-action-taken))
              :effects (list (list 'has-location object location)))
            operators)))
      (dolist (occupant (topo-relaxed-type-instances 'support-occupant))
        (dolist (support supports)
          (push
            (make-relaxed-hmax-operator
              :name (list 'propagated-landing occupant support)
              :cost 0
              :preconditions (list '(:topo-action-taken))
              :effects (list (list 'on occupant support)))
            operators))))
    (dolist (plate plates)
      (dolist (occupant (topo-relaxed-type-instances 'support-occupant))
        (push
          (make-relaxed-hmax-operator
            :name (list 'plate-consequence occupant plate)
            :cost 0
            :preconditions (list (list 'on occupant plate))
            :effects (list (list 'depressed plate)))
          operators)))
    operators))


(define-problem-helper topo-relaxed-recorder-operators ()
  (when (topo-relaxed-type-instances 'recorder)
    (list
      (make-relaxed-hmax-operator
        :name 'relaxed-start-recorder
        :effects (list '(:recording-in-progress)))
      (make-relaxed-hmax-operator
        :name 'relaxed-stop-recorder
        :preconditions (list '(:recording-in-progress))
        :effects (list '(:ghost-stops-recorder))))))


(define-problem-helper topo-relaxed-add-action-triggers! (operators)
  "Let every counted abstract action share its propagation side effects."
  (dolist (operator operators operators)
    (when (plusp (relaxed-hmax-operator.cost operator))
      (pushnew '(:topo-action-taken)
               (relaxed-hmax-operator.effects operator)
               :test #'equal))))


(define-problem-helper topo-relaxed-build-static-operators ()
  (let ((agents (topo-relaxed-type-instances 'agent))
        (cargo (topo-relaxed-type-instances 'cargo))
        (locations (topo-relaxed-type-instances 'location))
        (supports (topo-relaxed-type-instances 'support))
        (plates (topo-relaxed-type-instances 'plate))
        (receivers (topo-relaxed-type-instances 'receiver))
        (gates (topo-relaxed-type-instances 'gate)))
    (topo-relaxed-add-action-triggers!
      (append
        (topo-relaxed-movement-operators agents cargo locations)
        (topo-relaxed-manipulation-operators
          agents cargo supports plates receivers gates locations)
        (topo-relaxed-recorder-operators)))))


(define-problem-helper topo-relaxed-ensure-static-relevance (goals)
  (unless *topo-relaxed-all-operators-built-p*
    (setf *topo-relaxed-all-operators*
            (topo-relaxed-build-static-operators)
          *topo-relaxed-all-operators-built-p* t))
  (unless (equal goals *topo-relaxed-static-goals*)
    (setf *topo-relaxed-static-goals* (copy-list goals)
          *topo-relaxed-static-relevance*
            (compile-relaxed-hmax-relevance
              *topo-relaxed-all-operators* goals)
          *topo-relaxed-static-operators*
            (relaxed-hmax-relevance.operators
              *topo-relaxed-static-relevance*)
          *topo-relaxed-static-indexed-model*
            (compile-relaxed-indexed-model
              *topo-relaxed-static-operators* goals :validate nil))))


(define-problem-helper topo-beam-link-ensure-static-model (goals)
  "Compile the binary link-necessity model once for the active goal facts."
  (topo-relaxed-ensure-static-relevance goals)
  (unless (equal goals *topo-beam-link-static-goals*)
    (let* ((operators
             (topo-beam-link-static-operators
               *topo-relaxed-all-operators*))
           (relevance
             (compile-relaxed-hmax-relevance operators goals)))
      (setf *topo-beam-link-static-goals* (copy-list goals)
            *topo-beam-link-static-relevance* relevance
            *topo-beam-link-static-indexed-model*
              (compile-relaxed-indexed-model
                (relaxed-hmax-relevance.operators relevance)
                goals
                :validate nil)))))


(define-problem-helper topo-beam-link-bound-from-facts
    (state goal facts resource-analysis)
  "Return the admissible binary receiver-link cost for the standard relay provider."
  (if (not (topo-beam-link-bound-supported-p resource-analysis))
    0
    (let ((goals (topo-relaxed-goal-facts state goal)))
      (if (null goals)
        0
        (progn
          (topo-beam-link-ensure-static-model goals)
          (relaxed-indexed-hmax-cost
            (relaxed-hmax-relevant-facts
              (topo-beam-link-current-facts facts)
              *topo-beam-link-static-relevance*)
            *topo-beam-link-static-indexed-model*
            :ignore-unreachable t))))))


(define-problem-helper build-topo-relaxed-hmax-model (state goal)
  (let ((goals (topo-relaxed-goal-facts state goal)))
    ;; A generic happening can change arbitrary state after one WAIT.  Without a declarative
    ;; happening model, abstaining is safer than assuming ordinary action dependencies.
    (when (and goals (null *happening-names*))
      (topo-relaxed-ensure-static-relevance goals)
      (make-relaxed-hmax-model
        :facts
          (relaxed-hmax-relevant-facts
            (or *topo-relaxed-state-facts-override*
                (topo-relaxed-state-facts state))
            *topo-relaxed-static-relevance*)
        :operators *topo-relaxed-static-operators*
        :goals goals
        :validated-p t
        :indexed-model *topo-relaxed-static-indexed-model*))))


(define-problem-helper analyze-topo-relaxed-lm-cut (state goal)
  "Return the Topo LM-cut value and its domain-independent cut records."
  (let ((model (build-topo-relaxed-hmax-model state goal)))
    (if model
      (relaxed-indexed-lm-cut-analysis
        (relaxed-hmax-model.facts model)
        (relaxed-hmax-model.indexed-model model)
        :ignore-unreachable t)
      (values 0 nil))))


(define-problem-helper report-topo-relaxed-lm-cut-analysis
    (state goal &optional (stream t))
  "Print the generic LM-cut summary for a Topo STATE and GOAL."
  (multiple-value-bind (cost steps)
      (analyze-topo-relaxed-lm-cut state goal)
    (report-relaxed-lm-cut-analysis cost steps stream)))


(register-relaxed-hmax-model-builder 'build-topo-relaxed-hmax-model)


(define-query topo-relaxed-hmax-bound ()
  (registered-relaxed-hmax-bound state *goal*))


(define-query topo-relaxed-lm-cut-bound ()
  (registered-relaxed-lm-cut-bound state *goal*))


(define-query topo-finite-resource-bound ()
  (topo-finite-resource-bound-for state *goal*))


(define-query topo-finite-beam-resource-bound ()
  (topo-finite-beam-resource-bound-for state *goal*))


(define-query topo-lm-cut-resource-bound ()
  (topo-lm-cut-resource-bound-for state *goal*))


(define-query min-steps-remaining? ()
  (topo-lm-cut-resource-bound))


;; Search evaluates this inexpensive admissible sum before the aggregate query.  When it
;; already proves pruning, the general contributor interface avoids LM-cut entirely.
(register-min-steps-remaining-contributor
  'topo-finite-resource-bound
  :priority 10)
