;;; Filename: ww-relaxed-heuristics.lisp

;;; Domain-independent h-max and landmark-cut propagation over a permissive abstract model.
;;; Technologies opt in by registering model builders; ordinary problems pay nothing.
;;; Static relaxed models can be compiled backward from their goals once, then reuse the
;;; resulting operator and current-fact slice for both evaluators at every search state.
;;; Repeated evaluation can also use integer fact/operator indexes, avoiding per-state hash
;;; indexes and allocated LM-cut justification arcs.
;;; Optional analysis records expose the same cuts without changing their cost partitioning.
;;; LM-cut follows Helmert and Domshlak's justification-graph cost partitioning: each round
;;; subtracts a positive disjunctive-landmark cut from residual operator costs.


(in-package :ww)


(defstruct (relaxed-hmax-operator (:conc-name relaxed-hmax-operator.))
  "One abstract operator.  Preconditions and effects are arbitrary EQUAL facts."
  name
  (preconditions nil :type list)
  (effects nil :type list)
  (cost 1 :type (integer 0 *)))


(defstruct (relaxed-hmax-model (:conc-name relaxed-hmax-model.))
  "A partial relaxed model contributed for one state and active goal."
  (facts nil :type list)
  (operators nil :type list)
  (goals nil :type list)
  (validated-p nil)
  indexed-model)


(defstruct (relaxed-hmax-relevance (:conc-name relaxed-hmax-relevance.))
  "A goal-compiled operator slice and the facts it can read on paths to those goals."
  fact-table
  (operators nil :type list))


(defstruct (relaxed-indexed-operator (:conc-name relaxed-indexed-operator.))
  "One relaxed operator compiled to integer fact identifiers."
  name
  (preconditions #() :type simple-vector)
  (effects #() :type simple-vector)
  (cost 1 :type (integer 0 *)))


(defstruct (relaxed-indexed-model (:conc-name relaxed-indexed-model.))
  "Immutable fact/operator indexes shared by h-max and LM-cut evaluations."
  fact-table
  (facts #() :type simple-vector)
  (operators #() :type simple-vector)
  (dependents #() :type simple-vector)
  (achievers #() :type simple-vector)
  (no-precondition-operators #() :type simple-vector)
  (goals #() :type simple-vector))


(defstruct (relaxed-lm-cut-arc (:conc-name relaxed-lm-cut-arc.))
  "One justification-graph arc.  NIL OPERATOR marks an initial-fact arc."
  from
  to
  operator)


(defstruct
    (relaxed-lm-cut-analysis-operator
      (:conc-name relaxed-lm-cut-analysis-operator.))
  "One operator crossing a diagnostic LM cut."
  name
  cost
  residual-cost
  supporter
  (preconditions nil :type list)
  (effects nil :type list))


(defstruct
    (relaxed-lm-cut-analysis-step
      (:conc-name relaxed-lm-cut-analysis-step.))
  "One cost-partitioning round from a diagnostic LM-cut evaluation."
  goal
  goal-cost
  cut-cost
  (operators nil :type list))


(sb-ext:defglobal *relaxed-hmax-model-builders* nil
  "Problem-local functions of (STATE GOAL) returning RELAXED-HMAX-MODEL or NIL.")


(defun register-relaxed-hmax-model-builder (builder)
  "Register a staged problem's relaxed-model BUILDER once."
  (unless (and (symbolp builder) (fboundp builder))
    (error "Relaxed h-max model builder requires a defined function: ~S" builder))
  (pushnew builder *relaxed-hmax-model-builders* :test #'eq)
  builder)


(defun relaxed-hmax-operator-cost-valid-p (operator)
  (and (relaxed-hmax-operator-p operator)
       (integerp (relaxed-hmax-operator.cost operator))
       (not (minusp (relaxed-hmax-operator.cost operator)))
       (relaxed-hmax-operator.effects operator)))


(defun validate-relaxed-hmax-operators (operators)
  "Reject malformed abstract operators before cost propagation."
  (dolist (operator operators operators)
    (unless (relaxed-hmax-operator-cost-valid-p operator)
      (error "Malformed relaxed h-max operator: ~S" operator))))


(defun compile-relaxed-hmax-relevance (operators goals)
  "Return the exact backward-relevant slice of OPERATORS for GOALS.

The selected operators are copied with effects restricted to relevant facts.  Preconditions
stay complete, and every jointly relevant effect stays on its original operator, preserving
the shared action cost required by LM-cut."
  (validate-relaxed-hmax-operators operators)
  (let ((achievers (make-hash-table :test #'equal))
        (relevant (make-hash-table :test #'equal))
        (selected (make-hash-table :test #'eq))
        (agenda nil))
    (dolist (operator operators)
      (dolist (effect (relaxed-hmax-operator.effects operator))
        (push operator (gethash effect achievers))))
    (dolist (goal goals)
      (unless (gethash goal relevant)
        (setf (gethash goal relevant) t)
        (push goal agenda)))
    (loop while agenda
          for fact = (pop agenda)
          do (dolist (operator (gethash fact achievers))
               (unless (gethash operator selected)
                 (setf (gethash operator selected) t)
                 (dolist (precondition
                          (relaxed-hmax-operator.preconditions operator))
                   (unless (gethash precondition relevant)
                     (setf (gethash precondition relevant) t)
                     (push precondition agenda))))))
    (make-relaxed-hmax-relevance
      :fact-table relevant
      :operators
        (loop for operator in operators
              when (gethash operator selected)
                collect
                  (make-relaxed-hmax-operator
                    :name (relaxed-hmax-operator.name operator)
                    :cost (relaxed-hmax-operator.cost operator)
                    :preconditions
                      (copy-list
                        (relaxed-hmax-operator.preconditions operator))
                    :effects
                      (remove-if-not
                        (lambda (effect) (gethash effect relevant))
                        (relaxed-hmax-operator.effects operator)))))))


(defun relaxed-hmax-relevant-facts (facts relevance)
  "Discard current FACTS which no goal-supporting operator can read."
  (remove-if-not
    (lambda (fact)
      (gethash fact (relaxed-hmax-relevance.fact-table relevance)))
    facts))


(defun relaxed-indexed-intern-fact! (fact fact-table fact-vector)
  (multiple-value-bind (identifier present) (gethash fact fact-table)
    (if present
      identifier
      (let ((new-identifier (fill-pointer fact-vector)))
        (vector-push-extend fact fact-vector)
        (setf (gethash fact fact-table) new-identifier)
        new-identifier))))


(defun relaxed-indexed-fact-identifiers (facts fact-table fact-vector)
  (coerce
    (loop for fact in facts
          collect (relaxed-indexed-intern-fact!
                    fact fact-table fact-vector))
    'simple-vector))


(defun relaxed-indexed-freeze-index-lists! (indexes)
  (dotimes (identifier (length indexes) indexes)
    (setf (aref indexes identifier)
          (coerce (nreverse (aref indexes identifier)) 'simple-vector))))


(defun compile-relaxed-indexed-model (operators goals &key (validate t))
  "Compile OPERATORS and GOALS to immutable integer indexes for repeated evaluation."
  (when validate
    (validate-relaxed-hmax-operators operators))
  (let ((fact-table (make-hash-table :test #'equal))
        (fact-vector
          (make-array 32 :adjustable t :fill-pointer 0))
        (indexed-operators nil)
        (no-precondition-operators nil))
    (let ((goal-identifiers
            (relaxed-indexed-fact-identifiers
              goals fact-table fact-vector)))
      (dolist (operator operators)
        (push
          (make-relaxed-indexed-operator
            :name (relaxed-hmax-operator.name operator)
            :preconditions
              (relaxed-indexed-fact-identifiers
                (relaxed-hmax-operator.preconditions operator)
                fact-table fact-vector)
            :effects
              (relaxed-indexed-fact-identifiers
                (relaxed-hmax-operator.effects operator)
                fact-table fact-vector)
            :cost (relaxed-hmax-operator.cost operator))
          indexed-operators))
      (let* ((operator-vector
               (coerce (nreverse indexed-operators) 'simple-vector))
             (dependents
               (make-array (length fact-vector) :initial-element nil))
             (achievers
               (make-array (length fact-vector) :initial-element nil)))
        (dotimes (operator-identifier (length operator-vector))
          (let ((operator (aref operator-vector operator-identifier)))
            (if (zerop (length
                         (relaxed-indexed-operator.preconditions operator)))
              (push operator-identifier no-precondition-operators)
              (loop for fact-identifier across
                      (relaxed-indexed-operator.preconditions operator)
                    do (push operator-identifier
                             (aref dependents fact-identifier))))
            (loop for fact-identifier across
                    (relaxed-indexed-operator.effects operator)
                  do (push operator-identifier
                           (aref achievers fact-identifier)))))
        (make-relaxed-indexed-model
          :fact-table fact-table
          :facts (coerce fact-vector 'simple-vector)
          :operators operator-vector
          :dependents (relaxed-indexed-freeze-index-lists! dependents)
          :achievers (relaxed-indexed-freeze-index-lists! achievers)
          :no-precondition-operators
            (coerce (nreverse no-precondition-operators) 'simple-vector)
          :goals goal-identifiers)))))


(defun relaxed-hmax-precondition-cost (preconditions costs)
  "Return h-max's maximum prerequisite cost, or NIL if one is unreachable."
  (let ((maximum 0))
    (dolist (precondition preconditions maximum)
      (multiple-value-bind (cost present) (gethash precondition costs)
        (unless present
          (return-from relaxed-hmax-precondition-cost nil))
        (setf maximum (max maximum cost))))))


(defun relaxed-hmax-record-effect-costs (effects candidate costs)
  "Lower every EFFECT to CANDIDATE and return the facts whose costs improved."
  (let ((improved nil))
    (dolist (effect effects improved)
      (multiple-value-bind (old present) (gethash effect costs)
        (when (or (not present) (< candidate old))
          (setf (gethash effect costs) candidate
                improved (cons effect improved)))))))


(defun relaxed-hmax-current-operator-cost (operator operator-costs)
  (if operator-costs
    (gethash operator operator-costs)
    (relaxed-hmax-operator.cost operator)))


(defun relaxed-hmax-apply-operator (operator costs &optional operator-costs)
  "Apply one currently reachable OPERATOR and return its improved effect facts."
  (let ((precondition-cost
          (relaxed-hmax-precondition-cost
            (relaxed-hmax-operator.preconditions operator) costs)))
    (when precondition-cost
      (relaxed-hmax-record-effect-costs
        (relaxed-hmax-operator.effects operator)
        (+ precondition-cost
           (relaxed-hmax-current-operator-cost operator operator-costs))
        costs))))


(defun relaxed-hmax-fact-costs
    (facts operators &key operator-costs (validate t))
  "Compute the least h-max cost of every reachable abstract fact."
  (when validate
    (validate-relaxed-hmax-operators operators))
  (let ((costs (make-hash-table :test #'equal))
        (dependents (make-hash-table :test #'equal))
        (agenda nil))
    (dolist (fact facts)
      (unless (nth-value 1 (gethash fact costs))
        (setf (gethash fact costs) 0)
        (push fact agenda)))
    (dolist (operator operators)
      (dolist (precondition (relaxed-hmax-operator.preconditions operator))
        (pushnew operator (gethash precondition dependents) :test #'eq)))
    ;; Operators without prerequisites are not indexed by any agenda fact.
    (dolist (operator operators)
      (when (null (relaxed-hmax-operator.preconditions operator))
        (dolist (effect (relaxed-hmax-apply-operator
                          operator costs operator-costs))
          (push effect agenda))))
    ;; A fact-cost improvement can lower any operator that reads it. Re-evaluating just
    ;; those dependents reaches the same fixed point without rescanning the complete model.
    (loop while agenda
          for fact = (pop agenda)
          do (dolist (operator (gethash fact dependents))
               (dolist (effect (relaxed-hmax-apply-operator
                                 operator costs operator-costs))
                 (push effect agenda))))
    costs))


(defun relaxed-hmax-goal-cost (goals costs &key ignore-unreachable)
  "Return max fact cost for GOALS.

When IGNORE-UNREACHABLE is true, omit goals absent from the partial model.  This is the
safe mode for technology composition: an incomplete relaxation must weaken the bound,
never turn an unknown fact into proof that the concrete problem is impossible."
  (let ((maximum 0)
        (reachable 0))
    (dolist (goal goals)
      (multiple-value-bind (cost present) (gethash goal costs)
        (cond (present
               (incf reachable)
               (setf maximum (max maximum cost)))
              ((not ignore-unreachable)
               (return-from relaxed-hmax-goal-cost nil)))))
    (if (or goals (plusp reachable)) maximum 0)))


(defun relaxed-hmax-cost
    (facts operators goals &key ignore-unreachable (validate t))
  "Return the admissible h-max cost of GOALS in the supplied delete relaxation."
  (relaxed-hmax-goal-cost
    goals
    (relaxed-hmax-fact-costs facts operators :validate validate)
    :ignore-unreachable ignore-unreachable))


(defun relaxed-lm-cut-reachable-goals (goals costs ignore-unreachable)
  "Return modeled reachable GOALS, or NIL when strict reachability fails."
  (if ignore-unreachable
    (remove-if-not (lambda (goal)
                     (nth-value 1 (gethash goal costs)))
                   goals)
    (when (every (lambda (goal)
                   (nth-value 1 (gethash goal costs)))
                 goals)
      goals)))


(defun relaxed-lm-cut-max-cost-fact (facts costs)
  "Choose the first maximal h-max FACT, returning it and its cost."
  (let ((selected nil)
        (maximum nil))
    (dolist (fact facts (values selected maximum))
      (multiple-value-bind (cost present) (gethash fact costs)
        (unless present
          (return-from relaxed-lm-cut-max-cost-fact (values nil nil)))
        (when (or (null maximum) (> cost maximum))
          (setf selected fact
                maximum cost))))))


(defun relaxed-lm-cut-residual-costs (operators)
  (let ((costs (make-hash-table :test #'eq)))
    (dolist (operator operators costs)
      (setf (gethash operator costs)
            (relaxed-hmax-operator.cost operator)))))


(defun relaxed-lm-cut-justification-arcs
    (facts operators fact-costs source)
  "Build the h-max-preserving single-precondition/single-effect graph."
  (let ((arcs nil))
    (dolist (fact facts)
      (push (make-relaxed-lm-cut-arc :from source :to fact) arcs))
    (dolist (operator operators)
      (multiple-value-bind (precondition precondition-cost)
          (relaxed-lm-cut-max-cost-fact
            (relaxed-hmax-operator.preconditions operator)
            fact-costs)
        (declare (ignore precondition-cost))
        (when (or precondition
                  (null (relaxed-hmax-operator.preconditions operator)))
          (dolist (effect (relaxed-hmax-operator.effects operator))
            (push
              (make-relaxed-lm-cut-arc
                :from (or precondition source)
                :to effect
                :operator operator)
              arcs)))))
    arcs))


(defun relaxed-lm-cut-arc-cost (arc residual-costs)
  (let ((operator (relaxed-lm-cut-arc.operator arc)))
    (if operator (gethash operator residual-costs) 0)))


(defun relaxed-lm-cut-goal-zone (goal arcs residual-costs)
  "Facts with a zero-residual-cost path to GOAL in the justification graph."
  (let ((incoming (make-hash-table :test #'equal))
        (zone (make-hash-table :test #'equal))
        (agenda (list goal)))
    (dolist (arc arcs)
      (push arc (gethash (relaxed-lm-cut-arc.to arc) incoming)))
    (setf (gethash goal zone) t)
    (loop while agenda
          for fact = (pop agenda)
          do (dolist (arc (gethash fact incoming))
               (let ((source (relaxed-lm-cut-arc.from arc)))
                 (when (and (zerop (relaxed-lm-cut-arc-cost arc residual-costs))
                            (not (gethash source zone)))
                   (setf (gethash source zone) t)
                   (push source agenda)))))
    zone))


(defun relaxed-lm-cut-before-goal-zone (source arcs goal-zone)
  "Facts reachable from SOURCE without entering GOAL-ZONE."
  (let ((outgoing (make-hash-table :test #'equal))
        (zone (make-hash-table :test #'equal))
        (agenda (list source)))
    (dolist (arc arcs)
      (push arc (gethash (relaxed-lm-cut-arc.from arc) outgoing)))
    (setf (gethash source zone) t)
    (loop while agenda
          for fact = (pop agenda)
          do (dolist (arc (gethash fact outgoing))
               (let ((target (relaxed-lm-cut-arc.to arc)))
                 (when (and (not (gethash target goal-zone))
                            (not (gethash target zone)))
                   (setf (gethash target zone) t)
                   (push target agenda)))))
    zone))


(defun relaxed-lm-cut-operators (arcs before-goal-zone goal-zone)
  "Original operators labeling arcs across the selected justification cut."
  (let ((operators nil))
    (dolist (arc arcs operators)
      (let ((operator (relaxed-lm-cut-arc.operator arc)))
        (when (and operator
                   (gethash (relaxed-lm-cut-arc.from arc) before-goal-zone)
                   (gethash (relaxed-lm-cut-arc.to arc) goal-zone))
          (pushnew operator operators :test #'eq))))))


(defun relaxed-lm-cut-minimum-cost (operators residual-costs)
  (reduce #'min operators
          :key (lambda (operator)
                 (gethash operator residual-costs))))


(defun relaxed-lm-cut-reduce-costs! (operators amount residual-costs)
  (dolist (operator operators)
    (decf (gethash operator residual-costs) amount)))


(defun relaxed-lm-cut-cost
    (facts operators goals &key ignore-unreachable (validate t))
  "Return the admissible landmark-cut estimate for the supplied delete relaxation."
  (when validate
    (validate-relaxed-hmax-operators operators))
  (let* ((residual-costs (relaxed-lm-cut-residual-costs operators))
         (initial-costs
           (relaxed-hmax-fact-costs
             facts operators :operator-costs residual-costs :validate nil))
         (reachable-goals
           (relaxed-lm-cut-reachable-goals
             goals initial-costs ignore-unreachable))
         (total 0)
         (source (gensym "LM-CUT-SOURCE-")))
    (when (and goals (null reachable-goals) (not ignore-unreachable))
      (return-from relaxed-lm-cut-cost nil))
    (loop
      for fact-costs = (relaxed-hmax-fact-costs
                         facts operators :operator-costs residual-costs :validate nil)
      do (multiple-value-bind (goal goal-cost)
             (relaxed-lm-cut-max-cost-fact reachable-goals fact-costs)
           (when (or (null goal) (zerop goal-cost))
             (return total))
           (let* ((arcs
                    (relaxed-lm-cut-justification-arcs
                      facts operators fact-costs source))
                  (goal-zone
                    (relaxed-lm-cut-goal-zone goal arcs residual-costs))
                  (before-goal-zone
                    (relaxed-lm-cut-before-goal-zone source arcs goal-zone))
                  (cut
                    (relaxed-lm-cut-operators
                      arcs before-goal-zone goal-zone))
                  (cut-cost
                    (relaxed-lm-cut-minimum-cost cut residual-costs)))
             (unless (plusp cut-cost)
               (error "LM-cut produced a non-positive justification cut: ~S" cut))
             (incf total cut-cost)
             (relaxed-lm-cut-reduce-costs! cut cut-cost residual-costs))))))


(defun relaxed-indexed-initial-facts (facts model)
  (let* ((fact-count (length (relaxed-indexed-model.facts model)))
         (initial-mask (make-array fact-count :element-type 'bit :initial-element 0))
         (identifiers nil))
    (dolist (fact facts)
      (multiple-value-bind (identifier present)
          (gethash fact (relaxed-indexed-model.fact-table model))
        (when (and present (zerop (sbit initial-mask identifier)))
          (setf (sbit initial-mask identifier) 1)
          (push identifier identifiers))))
    (values initial-mask
            (coerce (nreverse identifiers) 'simple-vector))))


(defun relaxed-indexed-precondition-cost (operator costs)
  (let ((maximum 0))
    (loop for identifier across
            (relaxed-indexed-operator.preconditions operator)
          for cost = (aref costs identifier)
          do (when (minusp cost)
               (return-from relaxed-indexed-precondition-cost -1))
             (setf maximum (max maximum cost)))
    maximum))


(defun relaxed-indexed-record-effects!
    (operator candidate costs agenda)
  (loop for identifier across (relaxed-indexed-operator.effects operator)
        for old-cost = (aref costs identifier)
        when (or (minusp old-cost) (< candidate old-cost))
          do (setf (aref costs identifier) candidate)
             (vector-push-extend identifier agenda)))


(defun relaxed-indexed-apply-operator!
    (operator-identifier model costs agenda residual-costs)
  (let* ((operator
           (aref (relaxed-indexed-model.operators model)
                 operator-identifier))
         (precondition-cost
           (relaxed-indexed-precondition-cost operator costs)))
    (unless (minusp precondition-cost)
      (relaxed-indexed-record-effects!
        operator
        (+ precondition-cost
           (if residual-costs
             (aref residual-costs operator-identifier)
             (relaxed-indexed-operator.cost operator)))
        costs agenda))))


(defun relaxed-indexed-fact-costs
    (initial-identifiers model &optional residual-costs)
  (let* ((fact-count (length (relaxed-indexed-model.facts model)))
         (costs (make-array fact-count :initial-element -1))
         (agenda
           (make-array (max 1 (length initial-identifiers))
                       :element-type 'fixnum
                       :adjustable t
                       :fill-pointer 0))
         (head 0))
    (loop for identifier across initial-identifiers
          do (setf (aref costs identifier) 0)
             (vector-push-extend identifier agenda))
    (loop for operator-identifier across
            (relaxed-indexed-model.no-precondition-operators model)
          do (relaxed-indexed-apply-operator!
               operator-identifier model costs agenda residual-costs))
    (loop while (< head (fill-pointer agenda))
          for fact-identifier = (aref agenda head)
          do (incf head)
             (loop for operator-identifier across
                     (aref (relaxed-indexed-model.dependents model)
                           fact-identifier)
                   do (relaxed-indexed-apply-operator!
                        operator-identifier model costs agenda residual-costs)))
    costs))


(defun relaxed-indexed-goal-cost (model costs ignore-unreachable)
  (let ((maximum 0))
    (loop for identifier across (relaxed-indexed-model.goals model)
          for cost = (aref costs identifier)
          do (cond ((not (minusp cost))
                    (setf maximum (max maximum cost)))
                   ((not ignore-unreachable)
                    (return-from relaxed-indexed-goal-cost nil))))
    maximum))


(defun relaxed-indexed-hmax-cost (facts model &key ignore-unreachable)
  "Evaluate h-max using a COMPILE-RELAXED-INDEXED-MODEL result."
  (multiple-value-bind (initial-mask initial-identifiers)
      (relaxed-indexed-initial-facts facts model)
    (declare (ignore initial-mask))
    (relaxed-indexed-goal-cost
      model
      (relaxed-indexed-fact-costs initial-identifiers model)
      ignore-unreachable)))


(defun relaxed-indexed-residual-costs (model)
  (let* ((operators (relaxed-indexed-model.operators model))
         (costs (make-array (length operators))))
    (dotimes (identifier (length operators) costs)
      (setf (aref costs identifier)
            (relaxed-indexed-operator.cost
              (aref operators identifier))))))


(defun relaxed-indexed-reachable-goals
    (model costs ignore-unreachable)
  (let ((reachable nil))
    (loop for identifier across (relaxed-indexed-model.goals model)
          for cost = (aref costs identifier)
          do (cond ((not (minusp cost))
                    (push identifier reachable))
                   ((not ignore-unreachable)
                    (return-from relaxed-indexed-reachable-goals
                      (values #() nil)))))
    (values (coerce (nreverse reachable) 'simple-vector) t)))


(defun relaxed-indexed-max-cost-goal (goals costs)
  (let ((selected -1)
        (maximum -1))
    (loop for identifier across goals
          for cost = (aref costs identifier)
          when (> cost maximum)
            do (setf selected identifier
                     maximum cost))
    (values selected maximum)))


(defun relaxed-indexed-operator-supporter (operator costs source)
  (let ((selected source)
        (maximum -1))
    (loop for identifier across
            (relaxed-indexed-operator.preconditions operator)
          for cost = (aref costs identifier)
          do (when (minusp cost)
               (return-from relaxed-indexed-operator-supporter -1))
             (when (> cost maximum)
               (setf selected identifier
                     maximum cost)))
    selected))


(defun relaxed-indexed-supporters (model costs source)
  (let* ((operators (relaxed-indexed-model.operators model))
         (supporters (make-array (length operators))))
    (dotimes (identifier (length operators) supporters)
      (setf (aref supporters identifier)
            (relaxed-indexed-operator-supporter
              (aref operators identifier) costs source)))))


(defun relaxed-indexed-add-zone-fact! (identifier zone agenda)
  (when (zerop (sbit zone identifier))
    (setf (sbit zone identifier) 1)
    (vector-push-extend identifier agenda)
    t))


(defun relaxed-indexed-goal-zone
    (goal model initial-mask residual-costs supporters source)
  (let* ((fact-count (length (relaxed-indexed-model.facts model)))
         (zone
           (make-array (1+ fact-count) :element-type 'bit :initial-element 0))
         (agenda
           (make-array 16 :element-type 'fixnum
                          :adjustable t :fill-pointer 0))
         (head 0))
    (relaxed-indexed-add-zone-fact! goal zone agenda)
    (loop while (< head (fill-pointer agenda))
          for target = (aref agenda head)
          do (incf head)
             (when (< target fact-count)
               (when (= (sbit initial-mask target) 1)
                 (relaxed-indexed-add-zone-fact! source zone agenda))
               (loop for operator-identifier across
                       (aref (relaxed-indexed-model.achievers model) target)
                     for supporter = (aref supporters operator-identifier)
                     when (and (not (minusp supporter))
                               (zerop
                                 (aref residual-costs operator-identifier)))
                       do (relaxed-indexed-add-zone-fact!
                            supporter zone agenda))))
    zone))


(defun relaxed-indexed-before-goal-zone
    (model initial-mask supporters goal-zone source)
  (let* ((fact-count (length (relaxed-indexed-model.facts model)))
         (operators (relaxed-indexed-model.operators model))
         (zone
           (make-array (1+ fact-count) :element-type 'bit :initial-element 0))
         (changed t))
    (setf (sbit zone source) 1)
    (dotimes (identifier fact-count)
      (when (and (= (sbit initial-mask identifier) 1)
                 (= (sbit goal-zone identifier) 0))
        (setf (sbit zone identifier) 1)))
    (loop while changed
          do (setf changed nil)
             (dotimes (operator-identifier (length operators))
               (let ((supporter (aref supporters operator-identifier)))
                 (when (and (not (minusp supporter))
                            (= (sbit zone supporter) 1))
                   (loop for effect across
                           (relaxed-indexed-operator.effects
                             (aref operators operator-identifier))
                         when (and (= (sbit goal-zone effect) 0)
                                   (= (sbit zone effect) 0))
                           do (setf (sbit zone effect) 1
                                    changed t))))))
    zone))


(defun relaxed-indexed-operator-enters-zone-p
    (operator goal-zone)
  (loop for effect across (relaxed-indexed-operator.effects operator)
        thereis (= (sbit goal-zone effect) 1)))


(defun relaxed-indexed-cut-cost
    (model residual-costs supporters before-goal-zone goal-zone)
  (let ((operators (relaxed-indexed-model.operators model))
        (minimum nil))
    (dotimes (operator-identifier (length operators) minimum)
      (let ((supporter (aref supporters operator-identifier)))
        (when (and (not (minusp supporter))
                   (= (sbit before-goal-zone supporter) 1)
                   (relaxed-indexed-operator-enters-zone-p
                     (aref operators operator-identifier) goal-zone))
          (let ((cost (aref residual-costs operator-identifier)))
            (setf minimum (if minimum (min minimum cost) cost))))))))


(defun relaxed-indexed-reduce-cut!
    (model amount residual-costs supporters before-goal-zone goal-zone)
  (let ((operators (relaxed-indexed-model.operators model)))
    (dotimes (operator-identifier (length operators))
      (let ((supporter (aref supporters operator-identifier)))
        (when (and (not (minusp supporter))
                   (= (sbit before-goal-zone supporter) 1)
                   (relaxed-indexed-operator-enters-zone-p
                     (aref operators operator-identifier) goal-zone))
          (decf (aref residual-costs operator-identifier) amount))))))


(defun relaxed-indexed-fact-value (identifier model source)
  (if (= identifier source)
    :initial-facts
    (aref (relaxed-indexed-model.facts model) identifier)))


(defun relaxed-indexed-fact-values (identifiers model)
  (loop for identifier across identifiers
        collect (aref (relaxed-indexed-model.facts model) identifier)))


(defun relaxed-indexed-goal-zone-effects (operator model goal-zone)
  (loop for identifier across (relaxed-indexed-operator.effects operator)
        when (= (sbit goal-zone identifier) 1)
          collect (aref (relaxed-indexed-model.facts model) identifier)))


(defun relaxed-indexed-cut-analysis-operators
    (model residual-costs supporters before-goal-zone goal-zone source)
  (let ((operators (relaxed-indexed-model.operators model))
        (analysis nil))
    (dotimes (operator-identifier (length operators) (nreverse analysis))
      (let ((supporter (aref supporters operator-identifier))
            (operator (aref operators operator-identifier)))
        (when (and (not (minusp supporter))
                   (= (sbit before-goal-zone supporter) 1)
                   (relaxed-indexed-operator-enters-zone-p operator goal-zone))
          (push
            (make-relaxed-lm-cut-analysis-operator
              :name (relaxed-indexed-operator.name operator)
              :cost (relaxed-indexed-operator.cost operator)
              :residual-cost (aref residual-costs operator-identifier)
              :supporter
                (relaxed-indexed-fact-value supporter model source)
              :preconditions
                (relaxed-indexed-fact-values
                  (relaxed-indexed-operator.preconditions operator) model)
              :effects
                (relaxed-indexed-goal-zone-effects
                  operator model goal-zone))
            analysis))))))


(defun relaxed-indexed-lm-cut-evaluate
    (facts model ignore-unreachable analyze)
  (multiple-value-bind (initial-mask initial-identifiers)
      (relaxed-indexed-initial-facts facts model)
    (let* ((residual-costs (relaxed-indexed-residual-costs model))
           (initial-costs
             (relaxed-indexed-fact-costs
               initial-identifiers model residual-costs))
           (source (length (relaxed-indexed-model.facts model)))
           (total 0)
           (steps nil))
      (multiple-value-bind (reachable-goals reachable-p)
          (relaxed-indexed-reachable-goals
            model initial-costs ignore-unreachable)
        (unless reachable-p
          (return-from relaxed-indexed-lm-cut-evaluate
            (values nil nil)))
        (loop
          for fact-costs =
            (relaxed-indexed-fact-costs
              initial-identifiers model residual-costs)
          do (multiple-value-bind (goal goal-cost)
                 (relaxed-indexed-max-cost-goal reachable-goals fact-costs)
               (when (or (minusp goal) (zerop goal-cost))
                 (return (values total (nreverse steps))))
               (let* ((supporters
                        (relaxed-indexed-supporters model fact-costs source))
                      (goal-zone
                        (relaxed-indexed-goal-zone
                          goal model initial-mask residual-costs
                          supporters source))
                      (before-goal-zone
                        (relaxed-indexed-before-goal-zone
                          model initial-mask supporters goal-zone source))
                      (cut-cost
                        (relaxed-indexed-cut-cost
                          model residual-costs supporters
                          before-goal-zone goal-zone)))
                 (unless (and cut-cost (plusp cut-cost))
                   (error "Indexed LM-cut produced a non-positive cut."))
                 (when analyze
                   (push
                     (make-relaxed-lm-cut-analysis-step
                       :goal (aref (relaxed-indexed-model.facts model) goal)
                       :goal-cost goal-cost
                       :cut-cost cut-cost
                       :operators
                         (relaxed-indexed-cut-analysis-operators
                           model residual-costs supporters before-goal-zone
                           goal-zone source))
                     steps))
                 (incf total cut-cost)
                 (relaxed-indexed-reduce-cut!
                   model cut-cost residual-costs supporters
                   before-goal-zone goal-zone))))))))


(defun relaxed-indexed-lm-cut-cost (facts model &key ignore-unreachable)
  "Evaluate LM-cut using a COMPILE-RELAXED-INDEXED-MODEL result."
  (relaxed-indexed-lm-cut-evaluate
    facts model ignore-unreachable nil))


(defun relaxed-indexed-lm-cut-analysis (facts model &key ignore-unreachable)
  "Return LM-cut's cost and reusable justification-cut records as two values."
  (relaxed-indexed-lm-cut-evaluate
    facts model ignore-unreachable t))


(defun relaxed-lm-cut-analysis-operator-family (operator)
  (let ((name (relaxed-lm-cut-analysis-operator.name operator)))
    (if (consp name) (first name) name)))


(defun relaxed-lm-cut-analysis-family-counts (operators)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (operator operators)
      (incf
        (gethash
          (relaxed-lm-cut-analysis-operator-family operator)
          counts
          0)))
    (sort
      (loop for family being the hash-keys of counts using (hash-value count)
            collect (cons family count))
      (lambda (left right)
        (or (> (cdr left) (cdr right))
            (and (= (cdr left) (cdr right))
                 (string< (princ-to-string (car left))
                          (princ-to-string (car right)))))))))


(defun report-relaxed-lm-cut-analysis (cost steps &optional (stream t))
  "Print a compact, domain-independent summary of LM-cut analysis records."
  (format stream "~&LM-cut total = ~S in ~D cut~:P.~%" cost (length steps))
  (loop for step in steps
        for index from 1
        do (format stream
             "  Cut ~D: goal ~S, h-max ~D, cost ~D, ~D operator~:P.~%"
             index
             (relaxed-lm-cut-analysis-step.goal step)
             (relaxed-lm-cut-analysis-step.goal-cost step)
             (relaxed-lm-cut-analysis-step.cut-cost step)
             (length (relaxed-lm-cut-analysis-step.operators step)))
           (format stream "    Families: ~{~A=~D~^, ~}~%"
             (loop for (family . count) in
                     (relaxed-lm-cut-analysis-family-counts
                       (relaxed-lm-cut-analysis-step.operators step))
                   append (list family count))))
  (values cost steps))


(defun merge-relaxed-hmax-models (models)
  "Union partial MODELS into one shared fact/operator vocabulary."
  (make-relaxed-hmax-model
    :facts (remove-duplicates
             (mapcan (lambda (model)
                       (copy-list (relaxed-hmax-model.facts model)))
                     models)
             :test #'equal)
    :operators (mapcan (lambda (model)
                         (copy-list (relaxed-hmax-model.operators model)))
                       models)
    :goals (remove-duplicates
             (mapcan (lambda (model)
                       (copy-list (relaxed-hmax-model.goals model)))
                     models)
             :test #'equal)
    :validated-p
      (every #'relaxed-hmax-model.validated-p models)))


(defun registered-relaxed-model (state goal)
  "Build and merge every registered partial relaxation for STATE and GOAL."
  (let ((models
          (remove nil
                  (mapcar (lambda (builder)
                            (funcall (symbol-function builder) state goal))
                          *relaxed-hmax-model-builders*))))
    (cond ((null models) nil)
          ((null (rest models)) (first models))
          (t (merge-relaxed-hmax-models models)))))


(defun registered-relaxed-hmax-bound (state goal)
  "Evaluate h-max over every registered partial relaxation for STATE and GOAL."
  (let ((model (registered-relaxed-model state goal)))
    (if model
      (if (relaxed-hmax-model.indexed-model model)
        (relaxed-indexed-hmax-cost
          (relaxed-hmax-model.facts model)
          (relaxed-hmax-model.indexed-model model)
          :ignore-unreachable t)
        (relaxed-hmax-cost
          (relaxed-hmax-model.facts model)
          (relaxed-hmax-model.operators model)
          (relaxed-hmax-model.goals model)
          :ignore-unreachable t
          :validate (not (relaxed-hmax-model.validated-p model))))
      0)))


(defun registered-relaxed-lm-cut-bound (state goal)
  "Evaluate landmark-cut over every registered partial relaxation for STATE and GOAL."
  (let ((model (registered-relaxed-model state goal)))
    (if model
      (if (relaxed-hmax-model.indexed-model model)
        (relaxed-indexed-lm-cut-cost
          (relaxed-hmax-model.facts model)
          (relaxed-hmax-model.indexed-model model)
          :ignore-unreachable t)
        (relaxed-lm-cut-cost
          (relaxed-hmax-model.facts model)
          (relaxed-hmax-model.operators model)
          (relaxed-hmax-model.goals model)
          :ignore-unreachable t
          :validate (not (relaxed-hmax-model.validated-p model))))
      0)))
