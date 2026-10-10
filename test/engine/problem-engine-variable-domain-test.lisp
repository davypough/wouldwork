;;; Filename: problem-engine-variable-domain-test.lisp

;;; Focused tests for $variable quantifier domains: a ?variable in EXISTS, FORALL or
;;; DOALL may range over the list held by a $variable, read when the quantifier runs.
;;;
;;;   - EXISTS and FORALL over a $variable list in an action precondition
;;;   - DOALL over a $variable list in an effect, one successor per qualifying element
;;;   - EXISTS, FORALL and DOALL over a $variable list in query bodies
;;;   - the empty list: EXISTS false, FORALL true, DOALL no iterations
;;;   - a COMBINATION header over a $variable list
;;;   - rejection of a $variable domain in an action parameter list
;;;
;;; Run from the Wouldwork REPL with:
;;;
;;;   (stage engine-variable-domain-test)
;;;   (run-test-claims)
;;;   (solve)


(in-package :ww)


(ww-set *problem-name* engine-variable-domain-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (agent1)
  area  (area1 area2 area3)
  plate (plate1 plate2 plate3 plate4))


(define-dynamic-relations
  (loc agent $area)
  (covered plate)
  (marked plate))


(define-static-relations
  (plate-area plate $area)
  (area-plates area $list))


;;; Area1 has three plates, one covered; area2 has one clear plate; area3 has none.
(define-init
  (loc agent1 area1)
  (covered plate1)
  (plate-area plate1 area1)
  (plate-area plate2 area1)
  (plate-area plate4 area1)
  (plate-area plate3 area2))


(define-init-action initialize-area-plates 0
  ()
  (always-true)
  ()
  (assert
    (doall (?area area)
      (area-plates ?area nil))
    (doall (?plate plate)
      (do
        (bind (plate-area ?plate $area))
        (bind (area-plates $area $plates))
        (area-plates $area (cons ?plate $plates))))))


(define-query any-clear-plate? (?area area)
  (do (bind (area-plates ?area $plates))
      (exists (?plate $plates)
        (not (covered ?plate)))))


(define-query all-plates-covered? (?area area)
  (do (bind (area-plates ?area $plates))
      (forall (?plate $plates)
        (covered ?plate))))


(define-query clear-plate-count? (?area area)
  (do (bind (area-plates ?area $plates))
      (setq $count 0)
      (doall (?plate $plates)
        (if (not (covered ?plate))
          (setq $count (1+ $count))))
      $count))


(define-query clear-plate-pair? (?area area)
  (do (bind (area-plates ?area $plates))
      (exists (combination (?plate1 ?plate2) $plates)
        (and (not (covered ?plate1))
             (not (covered ?plate2))))))


;;; Marks every clear plate in the agent's area, each mark a separate successor.
(define-action mark-clear-plate 1
  (?agent agent)
  (and (bind (loc ?agent $area))
       (bind (area-plates $area $plates))
       (exists (?plate $plates)
         (not (covered ?plate)))
       (forall (?plate $plates)
         (not (marked ?plate))))
  (?agent "marks" ?plate)
  (doall (?plate $plates)
    (if (not (covered ?plate))
      (assert (marked ?plate)))))


(define-test-helper variable-domain-successor-count (action-name)
  "Number of successors ACTION-NAME generates from the start state."
  (let* ((action (find action-name *actions* :key #'action.name))
         (eff-args (apply (action.pre-defun-name action)
                          *start-state*
                          (first (action.precondition-args action)))))
    (length (apply (action.eff-defun-name action) *start-state* eff-args))))


(define-test-claim engine-variable-domain-queries
  (any-clear-plate? *start-state* 'area1)
  (any-clear-plate? *start-state* 'area2)
  (not (any-clear-plate? *start-state* 'area3))
  (not (all-plates-covered? *start-state* 'area1))
  (all-plates-covered? *start-state* 'area3)
  (= (clear-plate-count? *start-state* 'area1) 2)
  (= (clear-plate-count? *start-state* 'area3) 0)
  (clear-plate-pair? *start-state* 'area1)
  (not (clear-plate-pair? *start-state* 'area2))
  (not (clear-plate-pair? *start-state* 'area3)))


(define-test-claim engine-variable-domain-action
  (= (variable-domain-successor-count 'mark-clear-plate) 2)
  (expect-condition
    (lambda ()
      (create-action 'variable-domain-parameter-action 1
                     '(?agent agent ?plate $plates)
                     '(bind (loc ?agent $plates))
                     '(?agent ?plate)
                     '(assert (marked ?plate))
                     nil))
    'error
    :containing "$PLATES"))


(define-goal
  (marked plate2))
