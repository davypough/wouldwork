;;; Filename: problem-engine-route-recording-test.lisp
;;;
;;; Characterizes nested route metadata before the mobility model starts using it.
;;; Routes are action metadata: they are recorded, displayed, replayed, and copied,
;;; but they do not participate in proposition-state identity.
;;;
;;; Expected minimum path length: 1.

(in-package :ww)

(ww-set *problem-name* engine-route-recording-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  route-test-agent (route-agent)
  location (route-origin route-middle route-first route-second))


(define-dynamic-relations
  (route-test-at route-test-agent $location))


(define-init
  (route-test-at route-agent route-origin))


(define-query route-test-route (?source location ?destination location)
  (if (eql ?destination 'route-first)
      (list
        (list 'walk ?source '(gate-a) 'route-middle)
        (list 'stairs 'route-middle nil 'route-first))
      (list
        (list 'walk ?source nil 'route-second))))


(define-action route-test-move
  1
  (?agent route-test-agent)
  (bind (route-test-at ?agent $source))
  (">" ?agent "moves from" $source "to" $destination "via" $route)
  (doall (?to-location location)
    (if (member ?to-location '(route-first route-second))
      (assert
        (route-test-at ?agent ?to-location)
        (assign $destination ?to-location)
        (assign $route (route-test-route $source ?to-location))))))


(define-test-helper route-test-expected-route ()
  (copy-tree
    '((walk route-origin (gate-a) route-middle)
      (stairs route-middle nil route-first))))


(define-test-helper route-test-expected-instantiations ()
  (list 'route-agent
        'route-origin
        'route-first
        (route-test-expected-route)))


(define-test-helper route-test-action-and-updates ()
  "Return the route test action and all updates produced from the start state."
  (let* ((action (find 'route-test-move *actions* :key #'action.name))
         (pre-result
           (funcall (action.pre-defun-name action)
                    *start-state*
                    'route-agent)))
    (unless pre-result
      (error "ROUTE-TEST-MOVE precondition unexpectedly failed."))
    (list action
          (apply (action.eff-defun-name action)
                 *start-state*
                 pre-result))))


(define-test-helper route-test-update (destination)
  "Return ROUTE-TEST-MOVE's update for DESTINATION."
  (destructuring-bind (action updates) (route-test-action-and-updates)
    (declare (ignore action))
    (or (find destination
              updates
              :key (lambda (update)
                     (third (update.instantiations update))))
        (error "No route-test update for ~S." destination))))


(define-test-helper route-test-child (destination)
  "Return the child state produced for DESTINATION."
  (destructuring-bind (action updates) (route-test-action-and-updates)
    (let ((update
            (find destination
                  updates
                  :key (lambda (candidate)
                         (third (update.instantiations candidate))))))
      (unless update
        (error "No route-test update for ~S." destination))
      (create-action-state action *start-state* update))))


(define-test-claim nested-route-capture-contract
  (destructuring-bind (action updates) (route-test-action-and-updates)
    (declare (ignore action))
    (let ((destinations
            (mapcar (lambda (update)
                      (third (update.instantiations update)))
                    updates)))
      (unless (= (length updates) 2)
        (error "Expected two route successors, got ~D." (length updates)))
      (unless (and (= (count 'route-first destinations) 1)
                   (= (count 'route-second destinations) 1))
        (error "Expected one successor per endpoint, got ~S." destinations)))
    (unless (equal (update.instantiations (route-test-update 'route-first))
                   (route-test-expected-instantiations))
      (error "The first endpoint did not retain its complete nested route."))
    (unless (equal (update.instantiations (route-test-update 'route-first))
                   (update.instantiations (route-test-update 'route-first)))
      (error "Repeated route generation was not deterministic."))
    t))


(define-test-claim nested-route-copy-contract
  (let* ((original (route-test-child 'route-first))
         (copied (copy-problem-state original))
         (original-route (fourth (problem-state.instantiations original)))
         (copied-route (fourth (problem-state.instantiations copied))))
    (when (eq original-route copied-route)
      (error "COPY-PROBLEM-STATE retained a shared nested route."))
    (setf (first (first copied-route)) 'changed-mode)
    (unless (eql (first (first original-route)) 'walk)
      (error "Mutating a copied route changed the original state."))
    t)
  (let* ((state (route-test-child 'route-first))
         (record (record-move state))
         (state-route (fourth (problem-state.instantiations state))))
    (setf (first (first state-route)) 'changed-mode)
    (unless (eql (first (first (fifth (second record)))) 'walk)
      (error "RECORD-MOVE retained a shared nested route."))
    t))


(define-test-claim nested-route-display-and-replay-contract
  (let* ((instantiations (route-test-expected-instantiations))
         (action-form (cons 'route-test-move instantiations))
         (expected-display
           (list ">" 'route-agent "moves from" 'route-origin
                 "to" 'route-first "via" (route-test-expected-route))))
    (unless (equal (merge-effect-format 'route-test-move instantiations)
                   expected-display)
      (error "Nested route metadata was not preserved by display formatting."))
    (multiple-value-bind (replayed success-p failure)
        (apply-action-to-state action-form *start-state* nil)
      (unless success-p
        (error "Nested route replay failed: ~S" failure))
      (setf (first (first (fifth action-form))) 'changed-mode)
      (unless (eql (first
                     (first
                       (fourth (problem-state.instantiations replayed))))
                   'walk)
        (error "Replay retained a shared nested route."))
      (let* ((path (list (record-move replayed)))
             (solution (make-solution :depth 1 :time 1
                                      :path path :goal replayed))
             (printed
               (with-output-to-string (*standard-output*)
                 (printout-solution-with-states solution))))
        (unless (search "via" printed :test #'char-equal)
          (error "Printed solution omitted the route connective."))
        (unless (search "GATE-A" printed :test #'char-equal)
          (error "Printed solution omitted nested route contents."))
        (when (search "REPLAY FAILURE" printed :test #'char-equal)
          (error "Printed-solution replay failed."))))
    (multiple-value-bind (state success-p failure)
        (apply-action-to-state
          '(route-test-move route-agent route-origin route-first
            ((walk route-origin nil route-first)))
          *start-state*
          nil)
      (declare (ignore state))
      (when success-p
        (error "Replay accepted the wrong route witness."))
      (unless (and (consp failure) (eql (first failure) :state-mismatch))
        (error "Wrong-route replay reported an unexpected failure: ~S" failure)))
    t))


(define-test-claim route-metadata-is-not-graph-identity
  (let* ((retained (route-test-child 'route-first))
         (duplicate (copy-problem-state retained))
         (alternate-instantiations
           '(route-agent route-origin route-first
             ((walk route-origin nil route-first))))
         (node (make-node :state retained :depth 1))
         (open
           (hs::make-hstack
             :table (make-hash-table :test 'eql)
             :keyfn #'node.state.idb-hash))
         (closed (make-hash-table :test 'eql)))
    (setf (problem-state.instantiations duplicate)
          (copy-tree alternate-instantiations))
    (unless (equalp (problem-state.idb retained)
                    (problem-state.idb duplicate))
      (error "Route-only variation unexpectedly changed the proposition state."))
    (when (equal (problem-state.instantiations retained)
                 (problem-state.instantiations duplicate))
      (error "The graph-identity probe did not use distinct route witnesses."))
    (ensure-idb-hash retained)
    (ensure-idb-hash duplicate)
    (hs::push-hstack node open)
    (unless (eq (idb-in-open duplicate open 1) node)
      (error "OPEN did not recognize proposition-identical route variants."))
    (unless (equal (problem-state.instantiations (node.state node))
                   (route-test-expected-instantiations))
      (error "OPEN did not retain the first route witness."))
    (let ((entry (make-closed-entry retained 1)))
      (closed-bucket-insert entry retained 1 closed)
      (unless (eq (closed-bucket-find duplicate 1 closed) entry)
        (error "CLOSED did not recognize proposition-identical route variants.")))
    t))


(define-goal
  (route-test-at route-agent route-first))
