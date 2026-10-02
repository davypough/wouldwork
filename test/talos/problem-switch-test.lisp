;;; Focused switch characterization.  The only legal action toggles MAIN-SWITCH while the
;;; agent is holding cargo.  A high switch fails vertical reach, an unlinked switch fails
;;; horizontal reach, and a switch behind a closed reach barrier remains inaccessible.
;;; Expected minimum path length: one.
;;;
;;; This problem carries no segment geometry, so -REACHABILITY-COORDINATES' derivation is inert
;;; here and every REACH-VIA fact in the staged database is one authored below.  That inertness
;;; is the property this file guards: a problem that hand-authors its own reach must keep
;;; working exactly as it did before reach became derivable.  Do not add WALL-SEGMENT>,
;;; EDGE-SEGMENT>, or BOUNDARY-WALL to it -- any one of them arms the derivation and the guard
;;; is gone.

(in-package :ww)


(ww-set *problem-name* switch-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (agent1)
  box (held-box)
  switch (main-switch high-switch unlinked-switch blocked-switch)
  gate (normal-gate inverted-gate closed-barrier)
  location (switch-site))


(include-tech switch)
(include-tech gate)


(define-init
  (has-location agent1 switch-site)
  (holding agent1 held-box)

  (apparatus-coords> main-switch 0 0)
  (apparatus-coords> high-switch 0 1 3)
  (apparatus-coords> unlinked-switch 0 2)
  (apparatus-coords> blocked-switch 0 3)

  (reach-via switch-site () main-switch)
  (reach-via switch-site () high-switch)
  (reach-via switch-site (closed-barrier) blocked-switch)

  (controls ((main-switch)) normal-gate normal)
  (controls ((main-switch)) inverted-gate inverted))


(define-init-action initialize-switch-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-update propagate-consequences! ()
  (let ((*propagated-state-changed* nil))
    (update-gate-status!)
    *propagated-state-changed*))


(define-test-claim switch-schema-and-mounting
  (expect-relation-kind 'switched-on :dynamic)
  (expect-registered :action 'toggle-switch)
  (= (funcall (symbol-function 'base) *start-state* 'main-switch) 1)
  (= (funcall (symbol-function 'top) *start-state* 'main-switch) 1)
  (funcall (symbol-function 'reachable)
           *start-state* 'main-switch 'switch-site)
  (not (funcall (symbol-function 'reachable)
                *start-state* 'unlinked-switch 'switch-site))
  (not (funcall (symbol-function 'reachable)
                *start-state* 'blocked-switch 'switch-site))
  (not (funcall (symbol-function 'within-agent-vertical-reach)
                *start-state* 'agent1
                (funcall (symbol-function 'base) *start-state* 'high-switch)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((apparatus-coords> main-switch 0 0 1))
        :checks '(switch-init-check)))
    'init-check-failure
    :containing "HIGH-SWITCH"
    :check 'switch-init-check))


(define-goal
  (and (switched-on main-switch)
       (open normal-gate)
       (not (open inverted-gate))
       (holding agent1 held-box)))
