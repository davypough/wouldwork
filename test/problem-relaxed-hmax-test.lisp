;;; Filename: problem-relaxed-hmax-test.lisp
;;;
;;; Domain-independent h-max/LM-cut correctness plus a small topology integration fixture.
;;; The synthetic STRIPS check compares both estimates with exact shortest distances from
;;; every solvable truth assignment and compares the reference and indexed evaluators.
;;; The physical fixture then checks the bounds along a concrete move/pickup/move/put plan.
;;; Expected minimum path length: four.

(in-package :ww)


(ww-set *problem-name* relaxed-hmax-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)
(setf *expected-min-length* 4)


(define-types
  agent (hmax-agent)
  location
    (hmax-origin hmax-box-site hmax-goal-site
     hmax-reach-site hmax-directed-site)
  gate (hmax-gate hmax-controlled-gate hmax-inverted-gate)
  pressure-plate (hmax-plate hmax-plate2)
  receiver (hmax-receiver)
  box (hmax-box hmax-target-box))


(include-tech box)
(include-tech plate)
(include-tech gate)
(include-tech reachability)
(include-tech walkability)
(include-tech topo-lower-bound)


(define-init
  (has-location hmax-agent hmax-origin)
  (has-location hmax-box hmax-box-site)
  (has-location hmax-target-box hmax-goal-site)
  (has-position hmax-plate hmax-reach-site)
  (has-position hmax-plate2 hmax-directed-site)
  (controls (()) hmax-gate normal)
  (controls ((hmax-plate hmax-plate2)) hmax-controlled-gate normal)
  (controls (()) hmax-inverted-gate inverted)
  (reach-via hmax-origin (hmax-gate) hmax-reach-site)
  (reach-via> hmax-origin () hmax-directed-site)
  (traverse-via walking hmax-origin () hmax-box-site)
  (traverse-via walking hmax-box-site ((hmax-gate)) hmax-goal-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(defparameter *relaxed-bound-test-cheap-value* 0)
(defparameter *relaxed-bound-test-expensive-value* 0)
(defparameter *relaxed-bound-test-fallback-value* 0)
(defparameter *relaxed-bound-test-cheap-calls* 0)
(defparameter *relaxed-bound-test-expensive-calls* 0)
(defparameter *relaxed-bound-test-fallback-calls* 0)


(define-test-helper relaxed-bound-test-cheap (state)
  (declare (ignore state))
  (incf *relaxed-bound-test-cheap-calls*)
  *relaxed-bound-test-cheap-value*)


(define-test-helper relaxed-bound-test-expensive (state)
  (declare (ignore state))
  (incf *relaxed-bound-test-expensive-calls*)
  *relaxed-bound-test-expensive-value*)


(define-test-helper relaxed-bound-test-fallback (state)
  (declare (ignore state))
  (incf *relaxed-bound-test-fallback-calls*)
  *relaxed-bound-test-fallback-value*)


(define-test-helper relaxed-bound-short-circuit-test-p ()
  (let ((saved-solution-paths *solution-paths*)
        (saved-contributor-evaluations *min-steps-contributor-evaluations*)
        (saved-contributor-prunes *min-steps-contributor-prunes*))
    (unwind-protect
        (progn
          (setf *solution-paths* nil
                *min-steps-contributor-evaluations* 0
                *min-steps-contributor-prunes* 0)
          (let ((*min-steps-remaining-contributors* nil)
                (*depth-cutoff* 4)
                (*min-steps-fallback-mode* :eager)
                (*relaxed-bound-test-cheap-value* 4)
                (*relaxed-bound-test-expensive-value* 4)
                (*relaxed-bound-test-cheap-calls* 0)
                (*relaxed-bound-test-expensive-calls* 0))
            (register-min-steps-remaining-contributor
              'relaxed-bound-test-expensive :priority 20)
            (register-min-steps-remaining-contributor
              'relaxed-bound-test-cheap :priority 10)
            (and
              (equal (mapcar #'second *min-steps-remaining-contributors*)
                     '(relaxed-bound-test-cheap relaxed-bound-test-expensive))
              (min-steps-remaining-prunes-node-p *start-state* 1)
              (= *relaxed-bound-test-cheap-calls* 1)
              (zerop *relaxed-bound-test-expensive-calls*)
              (progn
                (setf *relaxed-bound-test-cheap-value* 2
                      *relaxed-bound-test-cheap-calls* 0
                      *relaxed-bound-test-expensive-calls* 0)
                (min-steps-remaining-prunes-node-p *start-state* 1))
              (= *relaxed-bound-test-cheap-calls* 1)
              (= *relaxed-bound-test-expensive-calls* 1)
              (= *min-steps-contributor-evaluations* 3)
              (= *min-steps-contributor-prunes* 2))))
      (setf *solution-paths* saved-solution-paths
            *min-steps-contributor-evaluations* saved-contributor-evaluations
            *min-steps-contributor-prunes* saved-contributor-prunes))))


(define-test-helper relaxed-bound-adaptive-fallback-test-p ()
  ;; *MIN-STEPS-FALLBACK-EVALUATIONS* and *MIN-STEPS-FALLBACK-UNIQUE-PRUNES* are DEFGLOBAL
  ;; (INCREMENT-GLOBAL's ATOMIC-INCF expansion requires that), so they have no dynamic
  ;; binding and cannot be LET-bound -- save and restore them instead.
  (let ((saved-solution-paths *solution-paths*)
        (saved-fallback-evaluations *min-steps-fallback-evaluations*)
        (saved-fallback-unique-prunes *min-steps-fallback-unique-prunes*)
        (original-fallback
          (when (fboundp 'min-steps-remaining?)
            (symbol-function 'min-steps-remaining?))))
    (unwind-protect
        (progn
          (setf *solution-paths* nil
                (symbol-function 'min-steps-remaining?)
                  (symbol-function 'relaxed-bound-test-fallback))
          (let ((*min-steps-remaining-contributors* nil)
                (*depth-cutoff* 4)
                (*threads* 0)
                (*min-steps-fallback-warmup* 2)
                (*min-steps-fallback-sample-interval* 3)
                (*min-steps-fallback-mode* :eager)
                (*min-steps-fallback-nonpruning-streak* 0)
                (*min-steps-fallback-sample-countdown* 0)
                (*min-steps-fallback-skipped* 0)
                (*min-steps-fallback-reactivations* 0)
                (*relaxed-bound-test-cheap-value* 2)
                (*relaxed-bound-test-fallback-value* 2)
                (*relaxed-bound-test-cheap-calls* 0)
                (*relaxed-bound-test-fallback-calls* 0))
            (register-min-steps-remaining-contributor
              'relaxed-bound-test-cheap :priority 10)
            (initialize-min-steps-fallback-adaptation)
            (and
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (eq *min-steps-fallback-mode* :sampling)
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (= *relaxed-bound-test-fallback-calls* 3)
              (= *min-steps-fallback-skipped* 2)
              (progn
                (setf *relaxed-bound-test-fallback-value* 4)
                t)
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (not (min-steps-remaining-prunes-node-p *start-state* 1))
              (min-steps-remaining-prunes-node-p *start-state* 1)
              (eq *min-steps-fallback-mode* :active)
              (min-steps-remaining-prunes-node-p *start-state* 1)
              (= *relaxed-bound-test-fallback-calls* 5)
              (= *min-steps-fallback-evaluations* 5)
              (= *min-steps-fallback-skipped* 4)
              (= *min-steps-fallback-unique-prunes* 2)
              (= *min-steps-fallback-reactivations* 1))))
      (if original-fallback
        (setf (symbol-function 'min-steps-remaining?) original-fallback)
        (fmakunbound 'min-steps-remaining?))
      (setf *solution-paths* saved-solution-paths
            *min-steps-fallback-evaluations* saved-fallback-evaluations
            *min-steps-fallback-unique-prunes* saved-fallback-unique-prunes))))


(define-test-helper relaxed-bound-pruning-relevance-test-p ()
  (let ((saved-solution-paths *solution-paths*)
        (saved-contributor-evaluations *min-steps-contributor-evaluations*)
        (saved-contributor-prunes *min-steps-contributor-prunes*))
    (unwind-protect
        (progn
          (setf *solution-paths* nil
                *min-steps-contributor-evaluations* 0
                *min-steps-contributor-prunes* 0)
          (let ((*min-steps-remaining-contributors* nil)
                (*solution-type* 'min-length)
                (*relaxed-bound-test-cheap-value* 4)
                (*relaxed-bound-test-cheap-calls* 0))
            (register-min-steps-remaining-contributor
              'relaxed-bound-test-cheap :priority 10)
            (and
              (let ((*depth-cutoff* 0)
                    (*min-steps-pruning-enabled* t))
                (not (min-steps-remaining-prunes-node-p *start-state* 1)))
              (zerop *relaxed-bound-test-cheap-calls*)
              (let ((*depth-cutoff* 4)
                    (*min-steps-pruning-enabled* nil))
                (not (min-steps-remaining-prunes-node-p *start-state* 1)))
              (zerop *relaxed-bound-test-cheap-calls*)
              (let ((*depth-cutoff* 4)
                    (*min-steps-pruning-enabled* t))
                (min-steps-remaining-prunes-node-p *start-state* 1))
              (= *relaxed-bound-test-cheap-calls* 1)
              (progn
                (setf *solution-paths* (list (make-solution :depth 4)))
                t)
              (let ((*depth-cutoff* 0)
                    (*min-steps-pruning-enabled* t))
                (min-steps-remaining-prunes-node-p *start-state* 1))
              (= *relaxed-bound-test-cheap-calls* 2)
              (= *min-steps-contributor-evaluations* 2)
              (= *min-steps-contributor-prunes* 2))))
      (setf *solution-paths* saved-solution-paths
            *min-steps-contributor-evaluations* saved-contributor-evaluations
            *min-steps-contributor-prunes* saved-contributor-prunes))))


(define-test-helper relaxed-bound-fallback-configuration-test-p ()
  (and
    *min-steps-pruning-enabled*
    (= *min-steps-fallback-warmup* 512)
    (= *min-steps-fallback-sample-interval* 64)
    (not (assoc '*min-steps-fallback-warmup* *problem-parameter-defaults*))
    (not (assoc '*min-steps-fallback-sample-interval*
                *problem-parameter-defaults*))
    (not (assoc '*min-steps-pruning-enabled* *problem-parameter-defaults*))
    (not (member '*min-steps-fallback-warmup*
                 *persisted-problem-parameters*))
    (not (member '*min-steps-fallback-sample-interval*
                 *persisted-problem-parameters*))
    (not (member '*min-steps-pruning-enabled*
                 *persisted-problem-parameters*))
    (let ((display
            (with-output-to-string (*standard-output*)
              (display-current-parameters))))
      (and (null (search "MIN-STEPS-FALLBACK-WARMUP" display))
           (null (search "MIN-STEPS-FALLBACK-SAMPLE-INTERVAL" display))
           (null (search "MIN-STEPS-PRUNING-ENABLED" display))))
    (let ((*min-steps-remaining-contributors* '((10 some-bound)))
          (*min-steps-fallback-sample-interval* 64)
          (*threads* 1))
      (not (min-steps-fallback-adaptive-p)))
    (let ((*min-steps-remaining-contributors* '((10 some-bound)))
          (*min-steps-fallback-sample-interval* 1)
          (*threads* 0))
      (not (min-steps-fallback-adaptive-p)))
    (let ((legacy-params
            (append (subseq *default-parameters* 0 14) '(t nil t))))
      (equal (migrate-retired-recorder-settings legacy-params)
             *default-parameters*))
    (equal
      (normalize-persisted-problem-parameters
        (append (subseq *default-parameters* 0 14) '(t 5 512 64)))
      (append (subseq *default-parameters* 0 14) '(5)))
    (equal
      (normalize-persisted-problem-parameters
        (append (subseq *default-parameters* 0 14) '(nil 3)))
      (append (subseq *default-parameters* 0 14) '(3)))
    (equal
      (normalize-persisted-problem-parameters
        (append (subseq *default-parameters* 0 14) '(t)))
      *default-parameters*)))


(define-test-helper relaxed-bound-search-progress-timing-test-p ()
  (let ((saved-start-time *start-time*)
        (saved-prior-time *prior-time*)
        (saved-parallel-time *prior-parallel-progress-time*)
        (saved-parallel-states *prior-parallel-progress-states*)
        (saved-parallel-cycles *prior-parallel-progress-cycles*))
    (unwind-protect
        (progn
          (setf *start-time* 0
                *prior-time* 0
                *prior-parallel-progress-time* 0
                *prior-parallel-progress-states* 1
                *prior-parallel-progress-cycles* 1)
          (initialize-search-progress-timing)
          (and (= *prior-time* *start-time*)
               (= *prior-parallel-progress-time* *start-time*)
               (zerop *prior-parallel-progress-states*)
               (zerop *prior-parallel-progress-cycles*)))
      (setf *start-time* saved-start-time
            *prior-time* saved-prior-time
            *prior-parallel-progress-time* saved-parallel-time
            *prior-parallel-progress-states* saved-parallel-states
            *prior-parallel-progress-cycles* saved-parallel-cycles))))


(define-test-helper relaxed-hmax-test-abstract-operators ()
  (list
    (make-relaxed-hmax-operator
      :name 'move-a-b :preconditions '(at-a) :effects '(at-b))
    (make-relaxed-hmax-operator
      :name 'move-b-a :preconditions '(at-b) :effects '(at-a))
    (make-relaxed-hmax-operator
      :name 'pickup-key :preconditions '(at-b) :effects '(key))
    (make-relaxed-hmax-operator
      :name 'unlock :preconditions '(at-a key) :effects '(goal))))


(define-test-helper relaxed-hmax-test-cost (mask)
  (relaxed-hmax-cost
    (loop for fact in '(at-a at-b key goal)
          for bit from 0
          when (logbitp bit mask)
            collect fact)
    (relaxed-hmax-test-abstract-operators)
    '(goal)))


(define-test-helper relaxed-lm-cut-test-cost (mask)
  (relaxed-lm-cut-cost
    (loop for fact in '(at-a at-b key goal)
          for bit from 0
          when (logbitp bit mask)
            collect fact)
    (relaxed-hmax-test-abstract-operators)
    '(goal)))


(define-test-helper relaxed-hmax-test-concrete-actions ()
  ;; Each row is (precondition-mask add-mask delete-mask).
  '((1 2 1)
    (2 1 2)
    (2 4 0)
    (5 8 0)))


(define-test-helper relaxed-hmax-test-exact-distance (start)
  (let ((queue (list (cons start 0)))
        (seen (make-hash-table :test #'eql)))
    (setf (gethash start seen) t)
    (loop while queue
          for entry = (pop queue)
          for state = (car entry)
          for distance = (cdr entry)
          do (when (logbitp 3 state)
               (return distance))
             (dolist (action (relaxed-hmax-test-concrete-actions))
               (destructuring-bind (preconditions additions deletions) action
                 (when (= (logand state preconditions) preconditions)
                   (let ((successor
                           (logior additions (logandc2 state deletions))))
                     (unless (gethash successor seen)
                       (setf (gethash successor seen) t
                             queue (nconc queue
                                          (list (cons successor (1+ distance)))))))))))))


(define-test-helper relaxed-hmax-test-exhaustively-admissible-p ()
  (loop for mask below 16
        for exact = (relaxed-hmax-test-exact-distance mask)
        always (or (null exact)
                   (<= (relaxed-hmax-test-cost mask) exact))))


(define-test-helper relaxed-lm-cut-test-exhaustively-admissible-p ()
  (loop for mask below 16
        for exact = (relaxed-hmax-test-exact-distance mask)
        always (or (null exact)
                   (let ((hmax (relaxed-hmax-test-cost mask))
                         (lm-cut (relaxed-lm-cut-test-cost mask)))
                     (<= hmax lm-cut exact)))))


(define-test-helper relaxed-lm-cut-test-weighted-operators ()
  (list
    (make-relaxed-hmax-operator
      :name 'setup-a-and-b :cost 2 :preconditions '(start) :effects '(a b))
    (make-relaxed-hmax-operator
      :name 'setup-c :preconditions '(start) :effects '(c))
    (make-relaxed-hmax-operator
      :name 'zero-bridge :cost 0 :preconditions '(c) :effects '(a))
    (make-relaxed-hmax-operator
      :name 'finish-g1 :preconditions '(a) :effects '(g1))
    (make-relaxed-hmax-operator
      :name 'finish-g2 :cost 2 :preconditions '(b c) :effects '(g2))
    (make-relaxed-hmax-operator
      :name 'alternate-g2 :cost 4 :preconditions '(a) :effects '(g2))))


(define-test-helper relaxed-lm-cut-test-weighted-actions ()
  ;; Each row is (precondition-mask add-mask delete-mask cost).
  '((1 6 0 2)
    (1 8 0 1)
    (8 2 0 0)
    (2 16 0 1)
    (12 32 0 2)
    (2 32 0 4)))


(define-test-helper relaxed-lm-cut-test-weighted-facts (mask)
  (loop for fact in '(start a b c g1 g2)
        for bit from 0
        when (logbitp bit mask)
          collect fact))


(define-test-helper relaxed-lm-cut-test-weighted-exact-distance (start)
  (let ((queue (list (cons start 0)))
        (distances (make-hash-table :test #'eql)))
    (setf (gethash start distances) 0)
    (loop while queue
          for entry = (progn
                        (setf queue (sort queue #'< :key #'cdr))
                        (pop queue))
          for state = (car entry)
          for distance = (cdr entry)
          when (= distance (gethash state distances))
            do (when (= (logand state 48) 48)
                 (return distance))
               (dolist (action (relaxed-lm-cut-test-weighted-actions))
                 (destructuring-bind
                     (preconditions additions deletions action-cost) action
                   (when (= (logand state preconditions) preconditions)
                     (let* ((successor
                              (logior additions (logandc2 state deletions)))
                            (successor-distance (+ distance action-cost))
                            (old-distance (gethash successor distances)))
                       (when (or (null old-distance)
                                 (< successor-distance old-distance))
                         (setf (gethash successor distances) successor-distance)
                         (push (cons successor successor-distance) queue)))))))))


(define-test-helper relaxed-lm-cut-test-weighted-admissible-p ()
  (loop for mask below 64
        for facts = (relaxed-lm-cut-test-weighted-facts mask)
        for exact = (relaxed-lm-cut-test-weighted-exact-distance mask)
        always (or
                 (null exact)
                 (let* ((operators (relaxed-lm-cut-test-weighted-operators))
                        (hmax (relaxed-hmax-cost facts operators '(g1 g2)))
                        (lm-cut (relaxed-lm-cut-cost facts operators '(g1 g2))))
                   (<= hmax lm-cut exact)))))


(define-test-helper relaxed-hmax-test-relevance-equivalent-p ()
  (let* ((operators
           (append
             (relaxed-hmax-test-abstract-operators)
             (list
               (make-relaxed-hmax-operator
                 :name 'decoy-start :effects '(decoy-a))
               (make-relaxed-hmax-operator
                 :name 'decoy-finish
                 :preconditions '(decoy-a) :effects '(decoy-b)))))
         (relevance (compile-relaxed-hmax-relevance operators '(goal)))
         (relevant-operators
           (relaxed-hmax-relevance.operators relevance)))
    (and
      (= (length relevant-operators) 4)
      (loop for mask below 64
            for facts =
              (loop for fact in '(at-a at-b key goal decoy-a decoy-b)
                    for bit from 0
                    when (logbitp bit mask)
                      collect fact)
            for relevant-facts =
              (relaxed-hmax-relevant-facts facts relevance)
            always
              (and
                (equal
                  (relaxed-hmax-cost facts operators '(goal))
                  (relaxed-hmax-cost
                    relevant-facts relevant-operators '(goal) :validate nil))
                (equal
                  (relaxed-lm-cut-cost facts operators '(goal))
                  (relaxed-lm-cut-cost
                    relevant-facts relevant-operators '(goal) :validate nil)))))))


(define-test-helper relaxed-lm-cut-test-weighted-relevance-equivalent-p ()
  (let* ((operators (relaxed-lm-cut-test-weighted-operators))
         (relevance
           (compile-relaxed-hmax-relevance operators '(g1 g2)))
         (relevant-operators
           (relaxed-hmax-relevance.operators relevance)))
    (loop for mask below 64
          for facts = (relaxed-lm-cut-test-weighted-facts mask)
          for relevant-facts =
            (relaxed-hmax-relevant-facts facts relevance)
          always
            (and
              (equal (relaxed-hmax-cost facts operators '(g1 g2))
                     (relaxed-hmax-cost
                       relevant-facts relevant-operators '(g1 g2)
                       :validate nil))
              (equal (relaxed-lm-cut-cost facts operators '(g1 g2))
                     (relaxed-lm-cut-cost
                       relevant-facts relevant-operators '(g1 g2)
                       :validate nil))))))


(define-test-helper relaxed-indexed-test-core-equivalent-p ()
  (let* ((operators (relaxed-hmax-test-abstract-operators))
         (model (compile-relaxed-indexed-model operators '(goal))))
    (loop for mask below 16
          for facts =
            (loop for fact in '(at-a at-b key goal)
                  for bit from 0
                  when (logbitp bit mask)
                    collect fact)
          always
            (and
              (equal (relaxed-hmax-cost facts operators '(goal))
                     (relaxed-indexed-hmax-cost facts model))
              (equal (relaxed-lm-cut-cost facts operators '(goal))
                     (relaxed-indexed-lm-cut-cost facts model))))))


(define-test-helper relaxed-indexed-test-weighted-equivalent-p ()
  (let* ((operators (relaxed-lm-cut-test-weighted-operators))
         (model (compile-relaxed-indexed-model operators '(g1 g2))))
    (loop for mask below 64
          for facts = (relaxed-lm-cut-test-weighted-facts mask)
          always
            (and
              (equal (relaxed-hmax-cost facts operators '(g1 g2))
                     (relaxed-indexed-hmax-cost facts model))
              (equal (relaxed-lm-cut-cost facts operators '(g1 g2))
                     (relaxed-indexed-lm-cut-cost facts model))))))


(define-test-helper relaxed-indexed-test-analysis-p ()
  (let* ((operators (relaxed-lm-cut-test-weighted-operators))
         (model (compile-relaxed-indexed-model operators '(g1 g2))))
    (multiple-value-bind (cost steps)
        (relaxed-indexed-lm-cut-analysis '(start) model)
      (and
        (= cost 5)
        (= (loop for step in steps
                 sum (relaxed-lm-cut-analysis-step.cut-cost step))
           cost)
        (search "LM-cut total = 5"
                (with-output-to-string (stream)
                  (report-relaxed-lm-cut-analysis cost steps stream)))
        (every
          (lambda (step)
            (and
              (relaxed-lm-cut-analysis-step.goal step)
              (plusp (relaxed-lm-cut-analysis-step.goal-cost step))
              (relaxed-lm-cut-analysis-step.operators step)
              (every
                (lambda (operator)
                  (and
                    (relaxed-lm-cut-analysis-operator.name operator)
                    (relaxed-lm-cut-analysis-operator.effects operator)))
                (relaxed-lm-cut-analysis-step.operators step))))
          steps)))))


(define-test-helper relaxed-hmax-test-physical-plan ()
  '((move hmax-agent
      ((walk hmax-origin nil hmax-box-site)))
    (pickup-box hmax-agent hmax-box hmax-box-site hmax-box-site)
    (move hmax-agent
      ((walk hmax-box-site (hmax-gate) hmax-goal-site)))
    (put-box hmax-agent hmax-box hmax-target-box hmax-goal-site)))


(define-test-helper relaxed-hmax-test-plan-bounds (bound-query)
  (let ((state (copy-problem-state *start-state*))
        (actions (relaxed-hmax-test-physical-plan))
        (bounds nil))
    (push (funcall (symbol-function bound-query) state) bounds)
    (loop while actions
          for action = (pop actions)
          do (multiple-value-bind (next-state success-p failure-reason)
                 (apply-action-to-state action state (first actions))
               (unless success-p
                 (error "H-max fixture action failed: ~S (~A)" action failure-reason))
               (setf state next-state)
               (push (funcall (symbol-function bound-query) state)
                     bounds)))
    (nreverse bounds)))


(define-test-helper relaxed-topo-test-model-for-goal (goal)
  (let* ((goals (topo-relaxed-goal-facts *start-state* goal))
         (operators
           (or *topo-relaxed-all-operators*
               (setf *topo-relaxed-all-operators*
                     (topo-relaxed-build-static-operators))))
         (relevance (compile-relaxed-hmax-relevance operators goals)))
    (make-relaxed-hmax-model
      :facts
        (relaxed-hmax-relevant-facts
          (topo-relaxed-state-facts *start-state*) relevance)
      :operators (relaxed-hmax-relevance.operators relevance)
      :goals goals
      :validated-p t)))


(define-test-helper relaxed-topo-test-bound-for-goal (evaluator goal)
  (let ((model (relaxed-topo-test-model-for-goal goal)))
    (funcall
      (symbol-function evaluator)
      (relaxed-hmax-model.facts model)
      (relaxed-hmax-model.operators model)
      (relaxed-hmax-model.goals model)
      :ignore-unreachable t
      :validate nil)))


(define-test-helper relaxed-topo-test-goal-cache-p ()
  (let ((original-goals (copy-list *topo-relaxed-static-goals*)))
    (unwind-protect
      (progn
        (build-topo-relaxed-hmax-model
          *start-state* '(open hmax-inverted-gate))
        (and
          (equal *topo-relaxed-static-goals*
                 '((open hmax-inverted-gate)))
          (= (length *topo-relaxed-static-operators*) 1)
          (= (length
               (relaxed-indexed-model.operators
                 *topo-relaxed-static-indexed-model*))
             1)
          (progn
            (build-topo-relaxed-hmax-model *start-state* *goal*)
            (equal *topo-relaxed-static-goals* original-goals))))
      (topo-relaxed-ensure-static-relevance original-goals))))


(define-test-helper relaxed-resource-test-routes (locations)
  (let ((routes (make-hash-table :test #'equal)))
    (dolist (from locations)
      (dolist (to locations)
        (setf (gethash (list from to) routes) (list nil))))
    routes))


(define-test-helper relaxed-resource-test-tasks ()
  (list
    (make-topo-resource-task
      :object 'resource-box1
      :pickup-locations '(resource-location1)
      :finish-locations '(resource-location2)
      :manipulation-cost 2)
    (make-topo-resource-task
      :object 'resource-box2
      :pickup-locations '(resource-location3)
      :finish-locations '(resource-location4)
      :manipulation-cost 2)))


(define-test-helper relaxed-resource-test-routing-cost (agents positions)
  (topo-resource-routing-search
    (relaxed-resource-test-tasks)
    agents positions
    (relaxed-resource-test-routes
      '(resource-location1 resource-location2
        resource-location3 resource-location4))
    (make-hash-table :test #'equal)))


(define-test-helper relaxed-resource-test-happening-abstention-p ()
  (let ((original-happenings *happening-names*))
    (unwind-protect
      (progn
        (setf *happening-names* '(resource-test-happening))
        (zerop (topo-finite-resource-bound-for *start-state* *goal*)))
      (setf *happening-names* original-happenings))))


(define-test-helper relaxed-resource-test-analysis-p ()
  (let ((analysis
          (analyze-topo-finite-resource-bound *start-state* *goal*)))
    (and
      (= (topo-resource-bound-analysis.manipulation-cost analysis) 2)
      (= (topo-resource-bound-analysis.routing-cost analysis) 2)
      (zerop (topo-resource-bound-analysis.session-cost analysis))
      (= (topo-resource-bound-analysis.total analysis) 4)
      (= (topo-resource-bound-analysis.total analysis)
         (funcall (symbol-function 'topo-finite-resource-bound)
                  *start-state*))
      (= (length (topo-resource-bound-analysis.tasks analysis)) 1))))


(define-test-helper relaxed-control-test-resource-analysis ()
  (make-topo-resource-bound-analysis
    :tasks
      (list
        (make-topo-resource-task
          :object 'control-box
          :manipulation-cost 2))
    :manipulation-cost 2
    :routing-cost 1
    :total 3))


(define-test-helper relaxed-control-test-operator
    (name preconditions effects &optional (cost 1))
  (make-relaxed-hmax-operator
    :name name
    :preconditions preconditions
    :effects effects
    :cost cost))


(define-test-helper relaxed-control-test-base-operators ()
  (list
    (relaxed-control-test-operator
      '(move control-agent control-start control-goal (control-gate))
      '((control-at-start) (open control-gate))
      '((control-at-goal) (:topo-action-taken)))
    (relaxed-control-test-operator
      '(normal-controls-open control-gate (control-plate))
      '((depressed control-plate))
      '((open control-gate))
      0)
    (relaxed-control-test-operator
      '(relaxed-change-plate control-plate)
      nil
      '((depressed control-plate) (:topo-action-taken)))))


(define-test-helper relaxed-control-test-overlap-operators ()
  (append
    (relaxed-control-test-base-operators)
    (list
      (relaxed-control-test-operator
        '(put-on-at control-agent control-box control-plate
                    control-start control-start nil has-position)
        nil
        '((on control-box control-plate) (:topo-action-taken)))
      (relaxed-control-test-operator
        '(plate-consequence control-box control-plate)
        '((on control-box control-plate))
        '((depressed control-plate))
        0))))


(define-test-helper relaxed-control-test-receiver-operators ()
  (list
    (relaxed-control-test-operator
      '(pickup control-agent control-box)
      nil
      '((holding control-agent control-box) (:topo-action-taken)))
    (relaxed-control-test-operator
      '(move control-agent control-start control-goal (control-gate))
      '((control-at-start) (open control-gate))
      '((control-at-goal) (:topo-action-taken)))
    (relaxed-control-test-operator
      '(normal-controls-open control-gate (control-receiver))
      '((active control-receiver))
      '((open control-gate))
      0)
    (relaxed-control-test-operator
      '(relaxed-activate control-receiver)
      '((:topo-action-taken))
      '((active control-receiver))
      0)
    (relaxed-control-test-operator
      'relaxed-any-action
      nil
      '((:topo-action-taken)))))


(define-test-helper relaxed-control-test-cost
    (operators share-covered-effects-p)
  (nth-value
    0
    (topo-control-setup-lm-cut-analysis
      '((control-at-start))
      operators
      '((control-at-goal))
      (relaxed-control-test-resource-analysis)
      share-covered-effects-p)))


(define-test-helper relaxed-control-test-typed-cost
    (operators share-covered-effects-p)
  (nth-value
    0
    (topo-control-setup-typed-lm-cut-analysis
      '((control-at-start))
      operators
      '((control-at-goal))
      (relaxed-control-test-resource-analysis)
      share-covered-effects-p)))


(define-test-helper relaxed-control-test-typed-blocker-costs ()
  (let ((original (copy-list (gethash 'beam-blocker *types*))))
    (unwind-protect
      (progn
        (pushnew 'control-box (gethash 'beam-blocker *types*) :test #'eq)
        (list
          (relaxed-control-test-typed-cost
            (relaxed-control-test-receiver-operators) t)
          (relaxed-control-test-typed-cost
            (relaxed-control-test-receiver-operators) nil)))
      (setf (gethash 'beam-blocker *types*) original))))


(define-test-helper relaxed-control-test-beam-link-cost
    (operators status)
  (nth-value
    0
    (topo-control-beam-link-lm-cut-analysis
      '((control-at-start))
      operators
      '((control-at-goal))
      (list (list 'control-receiver status)))))


(define-test-helper relaxed-control-test-beam-link-bound-cost
    (operators facts)
  (let* ((goals '((control-at-goal)))
         (beam-operators
           (topo-beam-link-static-operators operators))
         (relevance
           (compile-relaxed-hmax-relevance beam-operators goals))
         (indexed
           (compile-relaxed-indexed-model
             (relaxed-hmax-relevance.operators relevance)
             goals
             :validate nil)))
    (relaxed-indexed-hmax-cost facts indexed :ignore-unreachable t)))


(define-test-helper relaxed-control-test-beam-link-bound-support-p ()
  (let ((original-connectors (copy-list (gethash 'connector *types*)))
        (original-updates (copy-list *update-names*))
        (original-happenings (copy-list *happening-names*)))
    (unwind-protect
      (progn
        (pushnew 'control-connector (gethash 'connector *types*) :test #'eq)
        (setf *update-names*
              (append
                '(update-relay-status! update-receiver-status!)
                *update-names*)
              *happening-names* nil)
        (let ((*spliced-tech-names*
                (cons "beam-relay" *spliced-tech-names*)))
          (and
            (topo-beam-link-bound-supported-p
              (make-topo-resource-bound-analysis))
            (not
              (topo-beam-link-bound-supported-p
                (make-topo-resource-bound-analysis
                  :tasks
                    (list
                      (make-topo-resource-task
                        :object 'control-connector
                        :manipulation-cost 1)))))
            (progn
              (setf *happening-names* '(control-happening))
              (not
                (topo-beam-link-bound-supported-p
                  (make-topo-resource-bound-analysis)))))))
      (setf (gethash 'connector *types*) original-connectors
            *update-names* original-updates
            *happening-names* original-happenings))))


(define-test-helper relaxed-control-test-beam-link-analysis-p ()
  (let ((alternate-route
          (cons
            (relaxed-control-test-operator
              '(move control-agent control-start control-goal nil)
              '((control-at-start))
              '((control-at-goal) (:topo-action-taken)))
            (relaxed-control-test-receiver-operators))))
    (and
      (= (relaxed-control-test-beam-link-cost
           (relaxed-control-test-receiver-operators) :link-required)
         1)
      (zerop
        (relaxed-control-test-beam-link-cost
          (relaxed-control-test-receiver-operators) :linked))
      (zerop
        (relaxed-control-test-beam-link-cost
          alternate-route :link-required))
      (= (relaxed-control-test-beam-link-bound-cost
           (relaxed-control-test-receiver-operators)
           '((control-at-start)))
         1)
      (zerop
        (relaxed-control-test-beam-link-bound-cost
          (relaxed-control-test-receiver-operators)
          '((control-at-start) (:topo-beam-linked control-receiver))))
      (zerop
        (relaxed-control-test-beam-link-bound-cost
          alternate-route
          '((control-at-start))))
      (relaxed-control-test-beam-link-bound-support-p)
      (topo-beam-structurally-linked-p
        'graph-receiver
        '((paired graph-connector graph-transmitter)
          (paired graph-connector graph-receiver))
        '(graph-transmitter))
      (topo-beam-structurally-linked-p
        'graph-receiver
        '((coupled graph-transmitter graph-receiver))
        '(graph-transmitter))
      (not
        (topo-beam-structurally-linked-p
          'graph-receiver nil '(graph-transmitter))))))


(define-test-helper relaxed-control-setup-analysis-p ()
  (and
    (= (relaxed-control-test-cost
         (relaxed-control-test-base-operators) t)
       1)
    (= (relaxed-control-test-cost
         (relaxed-control-test-base-operators) nil)
       1)
    (zerop
      (relaxed-control-test-cost
        (relaxed-control-test-overlap-operators) t))
    (zerop
      (relaxed-control-test-cost
        (relaxed-control-test-overlap-operators) nil))
    (zerop
      (relaxed-control-test-cost
        (relaxed-control-test-receiver-operators) t))
    (= (relaxed-control-test-cost
         (relaxed-control-test-receiver-operators) nil)
       1)
    (= (relaxed-control-test-typed-cost
         (relaxed-control-test-base-operators) t)
       1)
    (= (relaxed-control-test-typed-cost
         (relaxed-control-test-base-operators) nil)
       1)
    (zerop
      (relaxed-control-test-typed-cost
        (relaxed-control-test-overlap-operators) t))
    (zerop
      (relaxed-control-test-typed-cost
        (relaxed-control-test-overlap-operators) nil))
    (equal (relaxed-control-test-typed-blocker-costs) '(0 1))
    (relaxed-control-test-beam-link-analysis-p)))


(define-test-claim relaxed-hmax-core-contract
  (relaxed-hmax-test-exhaustively-admissible-p)
  (relaxed-lm-cut-test-exhaustively-admissible-p)
  (relaxed-lm-cut-test-weighted-admissible-p)
  (relaxed-hmax-test-relevance-equivalent-p)
  (relaxed-lm-cut-test-weighted-relevance-equivalent-p)
  (relaxed-indexed-test-core-equivalent-p)
  (relaxed-indexed-test-weighted-equivalent-p)
  (relaxed-indexed-test-analysis-p)
  (= (relaxed-hmax-cost
       '(start)
       (relaxed-lm-cut-test-weighted-operators)
       '(g1 g2))
     4)
  (= (relaxed-lm-cut-cost
       '(start)
       (relaxed-lm-cut-test-weighted-operators)
       '(g1 g2))
     5)
  (= (relaxed-lm-cut-test-weighted-exact-distance 1) 6)
  (= (relaxed-hmax-cost
       nil
       (list
         (make-relaxed-hmax-operator
           :name 'expensive-a :cost 5 :effects '(a))
         (make-relaxed-hmax-operator
           :name 'a-to-goal :preconditions '(a) :effects '(goal))
         (make-relaxed-hmax-operator
           :name 'cheap-a :effects '(a)))
       '(goal))
     2)
  (= (relaxed-lm-cut-cost
       nil
       (list
         (make-relaxed-hmax-operator
           :name 'joint-achiever :effects '(a b)))
       '(a b))
     1)
  (equal
    (relaxed-hmax-operator.effects
      (first
        (relaxed-hmax-relevance.operators
          (compile-relaxed-hmax-relevance
            (list
              (make-relaxed-hmax-operator
                :name 'joint-achiever :effects '(a b decoy)))
            '(a b)))))
    '(a b))
  (let ((model
          (compile-relaxed-indexed-model nil '(unreachable))))
    (and
      (null (relaxed-indexed-hmax-cost nil model))
      (zerop
        (relaxed-indexed-hmax-cost
          nil model :ignore-unreachable t))
      (null (relaxed-indexed-lm-cut-cost nil model))
      (zerop
        (relaxed-indexed-lm-cut-cost
          nil model :ignore-unreachable t))))
  (null (relaxed-hmax-cost nil nil '(unreachable)))
  (zerop (relaxed-hmax-cost nil nil '(unreachable) :ignore-unreachable t))
  (null (relaxed-lm-cut-cost nil nil '(unreachable)))
  (zerop (relaxed-lm-cut-cost nil nil '(unreachable) :ignore-unreachable t)))


(define-test-claim topo-finite-resource-contract
  (equal (mapcar #'second *min-steps-remaining-contributors*)
         '(topo-finite-resource-bound))
  (find 'topo-finite-resource-budget *candidate-state-screeners*
        :key #'candidate-state-screener-name :test #'eq)
  (not (fboundp 'min-steps-remaining?))
  (relaxed-bound-short-circuit-test-p)
  (relaxed-bound-adaptive-fallback-test-p)
  (relaxed-bound-pruning-relevance-test-p)
  (relaxed-bound-fallback-configuration-test-p)
  (relaxed-bound-search-progress-timing-test-p)
  (= (relaxed-resource-test-routing-cost
       '(resource-agent1) '(resource-location1))
     3)
  (= (relaxed-resource-test-routing-cost
       '(resource-agent1 resource-agent2)
       '(resource-location1 resource-location3))
     2)
  (= (funcall (symbol-function 'topo-finite-resource-bound) *start-state*) 4)
  (= (funcall (symbol-function 'topo-finite-beam-resource-bound) *start-state*) 4)
  (= (funcall (symbol-function 'topo-lm-cut-resource-bound) *start-state*) 4)
  (equal (relaxed-hmax-test-plan-bounds 'topo-finite-resource-bound)
         '(4 3 2 1 0))
  (equal (relaxed-hmax-test-plan-bounds 'topo-finite-beam-resource-bound)
         '(4 3 2 1 0))
  (equal (relaxed-hmax-test-plan-bounds 'topo-lm-cut-resource-bound)
         '(4 3 2 1 0))
  (equal (relaxed-hmax-test-plan-bounds 'min-steps-remaining-bound)
         '(4 3 2 1 0))
  (relaxed-resource-test-analysis-p)
  (relaxed-control-setup-analysis-p)
  (relaxed-resource-test-happening-abstention-p))


(define-test-claim topo-candidate-screening-contract
  (let ((tight
          (make-candidate-screening-context
            :final-goal (copy-tree *goal*)
            :final-goal-function (symbol-function 'goal-fn)
            :remaining-depth-budget 3))
        (exact
          (make-candidate-screening-context
            :final-goal (copy-tree *goal*)
            :final-goal-function (symbol-function 'goal-fn)
            :remaining-depth-budget 4))
        (unrestricted
          (make-candidate-screening-context
            :final-goal (copy-tree *goal*)
            :final-goal-function (symbol-function 'goal-fn))))
    (and
      (eq (candidate-screening-result-status
            (screen-candidate-state *start-state* tight))
          :impossible)
      (eq (candidate-screening-result-status
            (screen-candidate-state *start-state* exact))
          :unknown)
      (eq (candidate-screening-result-status
            (screen-candidate-state *start-state* unrestricted))
          :unknown))))


(define-test-claim topo-relaxed-hmax-contract
  (member 'build-topo-relaxed-hmax-model
          *relaxed-hmax-model-builders*
          :test #'eq)
  (= (funcall (symbol-function 'topo-relaxed-hmax-bound) *start-state*) 2)
  (= (funcall (symbol-function 'topo-relaxed-lm-cut-bound) *start-state*) 3)
  (multiple-value-bind (cost steps)
      (analyze-topo-relaxed-lm-cut *start-state* *goal*)
    (and (= cost 3) steps))
  (relaxed-hmax-model.validated-p
    (build-topo-relaxed-hmax-model *start-state* *goal*))
  (not
    (relaxed-hmax-model.unreachability-complete-p
      (build-topo-relaxed-hmax-model *start-state* *goal*)))
  (relaxed-indexed-model-p
    (relaxed-hmax-model.indexed-model
      (build-topo-relaxed-hmax-model *start-state* *goal*)))
  (relaxed-topo-test-goal-cache-p)
  (< (length *topo-relaxed-static-operators*)
     (length *topo-relaxed-all-operators*))
  (equal (relaxed-hmax-test-plan-bounds 'topo-relaxed-hmax-bound)
         '(2 2 2 1 0))
  (equal (relaxed-hmax-test-plan-bounds 'topo-relaxed-lm-cut-bound)
         '(3 3 2 1 0))
  (= (relaxed-topo-test-bound-for-goal
       'relaxed-hmax-cost '(open hmax-controlled-gate))
     1)
  (= (relaxed-topo-test-bound-for-goal
       'relaxed-lm-cut-cost '(open hmax-controlled-gate))
     2)
  (= (relaxed-topo-test-bound-for-goal
       'relaxed-lm-cut-cost '(open hmax-inverted-gate))
     1)
  (= (relaxed-topo-test-bound-for-goal
       'relaxed-lm-cut-cost
       '(and (depressed hmax-plate) (active hmax-receiver)))
     1)
  (not (topo-relaxed-independent-plate-changes-p nil))
  (some
    (lambda (operator)
      (equal (relaxed-hmax-operator.name operator)
             '(relaxed-open-fallback hmax-controlled-gate)))
    (topo-relaxed-gate-control-operators '(hmax-controlled-gate) nil))
  (let ((reaches
          (topo-relaxed-reach-clauses
            (topo-relaxed-type-instances 'location))))
    (and
      (member '(hmax-gate)
              (gethash '(hmax-origin hmax-reach-site) reaches)
              :test #'equal)
      (member '(hmax-gate)
              (gethash '(hmax-reach-site hmax-origin) reaches)
              :test #'equal)
      (member nil
              (gethash '(hmax-origin hmax-directed-site) reaches)
              :test #'equal)
      (null (gethash '(hmax-directed-site hmax-origin) reaches))))
  (member '(has-position hmax-plate hmax-reach-site)
          (topo-relaxed-state-facts *start-state*)
          :test #'equal)
  (some
    (lambda (operator)
      (and (member '(holding hmax-agent hmax-box)
                   (relaxed-hmax-operator.preconditions operator)
                   :test #'equal)
           (member '(has-location hmax-agent hmax-origin)
                   (relaxed-hmax-operator.preconditions operator)
                   :test #'equal)
           (member '(open hmax-gate)
                   (relaxed-hmax-operator.preconditions operator)
                   :test #'equal)
           (member '(has-position hmax-plate hmax-reach-site)
                   (relaxed-hmax-operator.preconditions operator)
                   :test #'equal)
           (member '(has-location hmax-box hmax-reach-site)
                   (relaxed-hmax-operator.effects operator)
                   :test #'equal)
           (member '(on hmax-box hmax-plate)
                   (relaxed-hmax-operator.effects operator)
                   :test #'equal)))
    *topo-relaxed-all-operators*)
  (every
    (lambda (operator)
      (or
        (not (member '(has-location hmax-box hmax-goal-site)
                     (relaxed-hmax-operator.effects operator)
                     :test #'equal))
        (not (member (first (relaxed-hmax-operator.name operator))
                     '(put-ground put-on-at)
                     :test #'eq))
        (member '(has-location hmax-agent hmax-goal-site)
                (relaxed-hmax-operator.preconditions operator)
                :test #'equal)))
    *topo-relaxed-all-operators*)
  (some (lambda (operator)
          (member '(open hmax-gate)
                  (relaxed-hmax-operator.preconditions operator)
                  :test #'equal))
        *topo-relaxed-all-operators*)
  (some
    (lambda (operator)
      (and (equal (relaxed-hmax-operator.effects operator)
                  '((open hmax-controlled-gate)))
           (equal (relaxed-hmax-operator.preconditions operator)
                  '((depressed hmax-plate)
                    (depressed hmax-plate2)))
           (zerop (relaxed-hmax-operator.cost operator))))
    (relaxed-hmax-model.operators
      (relaxed-topo-test-model-for-goal '(open hmax-controlled-gate))))
  (loop for bound in (relaxed-hmax-test-plan-bounds 'min-steps-remaining-bound)
        for exact-remaining from 4 downto 0
        always (<= bound exact-remaining)))


(define-goal
  (and (has-location hmax-box hmax-goal-site)
       (on hmax-box hmax-target-box)))
