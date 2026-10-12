;;; Tiny fixture for FINALLY followups under backtracking. The check script
;;; backtracking-followups.lisp selects the algorithm; (test-talos) solves it depth-first.
;;; SWITCH-ON's followup writes (seen-on), which no effect literal records;
;;; SWITCH-OFF then literally inverts SWITCH-ON. JAM's followup asserts
;;; (inconsistent-state). The only solution within depth 2 is SWITCH-ON, SWITCH-OFF.
;;; MARK-LEVELS is never applicable in search (JAM's successors are dropped); its
;;; loop of asserts is called directly to check that each collects its own followups.
(in-package :ww)


(ww-set *problem-name* backtracking-followups)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 2)


(define-types level (1 2 3))


(define-dynamic-relations (ready) (on) (seen-on) (jammed) (level-set level))


(define-update mark-seen! ()
  (seen-on))


(define-update check-jam! ()
  (if (jammed)
    (inconsistent-state)))


(define-action switch-on 1
  () (not (on))
  ()
  (assert (on)
          (finally (mark-seen!))))


(define-action switch-off 1
  () (on)
  ()
  (assert (not (on))))


(define-action jam 1
  () (not (jammed))
  ()
  (assert (jammed)
          (finally (check-jam!))))


(define-action mark-levels 1
  () (jammed)
  ()
  (doall (?level level)
    (assert (level-set ?level)
            (finally (mark-seen!)))))


(define-init
  (ready))


(define-goal
  (and (seen-on)
       (not (on))))
