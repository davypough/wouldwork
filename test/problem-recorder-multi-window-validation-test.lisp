;;; Filename: problem-recorder-multi-window-validation-test.lisp

;;; Focused Stage 2/3/4 characterization of the integrated-path state machine, boundary
;;; identity.  Completed
;;; cycles are separated by real STOP/START transitions.  Between them the live box moves,
;;; so the next isolated recording succeeds only when its snapshot is reconstructed through
;;; the preceding normalized boundary and its fresh ghost fork.  The fixture also proves
;;; that an integrated stop can be physically legal while its recording is invalid, and
;;; that mandatory boundary validation rejects that successor before graph insertion.
;;; Stage 3 additionally proves no-progress rejection and the separate Pareto rule under
;;; which an equal-or-cheaper normalized boundary with fewer cycles dominates one with more
;;; cycles while open states, physical differences, and cost/resource tradeoffs remain.
;;; Stage 4 reconstructs complete one-, two-, and three-cycle reports from accepted paths.
;;; Expected ordinary harness path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-multi-window-validation-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(ww-set *max-recorder-cycles* 3)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  box (live-box ghost-box)
  recorder (recorder1)
  location (recorder-site first-site second-site third-site))


(include-tech recorder)


(define-dynamic-relations
  (first-recorded)
  (second-prepared)
  (second-recorded)
  (third-prepared)
  (third-recorded)
  (invalid-cycle-enabled)
  (invalid-cycle-consumed))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-box ghost-box)
  (has-location live-agent recorder-site)
  (has-location live-box first-site)
  (has-position recorder1 recorder-site))


(define-action record-first-cycle
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (has-location ghost-box first-site))
  (">" ?agent "records the first snapshot")
  (assert (first-recorded)))


(define-action prepare-second-cycle
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recorder-cycle-closed)
       (has-location live-box first-site))
  (">" ?agent "moves the live box before cycle two")
  (assert (has-location live-box second-site)
          (second-prepared)))


(define-action record-second-cycle
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (second-prepared)
       (has-location ghost-box second-site))
  (">" ?agent "records from the second snapshot")
  (assert (second-recorded)))


(define-action prepare-third-cycle
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recorder-cycle-closed)
       (has-location live-box second-site))
  (">" ?agent "moves the live box before cycle three")
  (assert (has-location live-box third-site)
          (third-prepared)))


(define-action record-third-cycle
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (third-prepared)
       (has-location ghost-box third-site))
  (">" ?agent "records from the third snapshot")
  (assert (third-recorded)))


(define-action enable-invalid-cycle
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress))
  (">" ?agent "creates a live-only in-window dependency")
  (assert (invalid-cycle-enabled)))


(define-action consume-invalid-cycle
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (invalid-cycle-enabled))
  (">" ?agent "uses the invalid live-only dependency")
  (assert (invalid-cycle-consumed)))


(define-test-helper recorder-two-window-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (record-first-cycle ghost-agent))
    (3.0 (stop-recorder ghost-agent))
    (4.0 (prepare-second-cycle live-agent))
    (5.0 (start-recorder live-agent))
    (6.0 (record-second-cycle ghost-agent))
    (7.0 (stop-recorder ghost-agent))))


(define-test-helper recorder-three-window-path (&optional final-open-p)
  (append
    (recorder-two-window-path)
    '((8.0 (prepare-third-cycle live-agent))
      (9.0 (start-recorder live-agent))
      (10.0 (record-third-cycle ghost-agent)))
    (unless final-open-p
      '((11.0 (stop-recorder ghost-agent))))))


(define-test-helper recorder-invalid-closed-window-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (enable-invalid-cycle live-agent))
    (3.0 (consume-invalid-cycle ghost-agent))
    (4.0 (stop-recorder ghost-agent))))


(define-test-helper recorder-no-progress-second-window-path ()
  '((1.0 (start-recorder live-agent))
    (2.0 (record-first-cycle ghost-agent))
    (3.0 (stop-recorder ghost-agent))
    (4.0 (start-recorder live-agent))
    (5.0 (stop-recorder ghost-agent))))


(define-test-helper recorder-path-validation (path)
  (validate-action-sequence *start-state* path))


(define-test-helper recorder-solution-for-path (path)
  (let ((validation (recorder-path-validation path)))
    (unless (action-sequence-validation-success-p validation)
      (error "Recorder report fixture path failed at ~S: ~S"
             (action-sequence-validation-failure-action validation)
             (action-sequence-validation-failure-reason validation)))
    (let ((final-state (action-sequence-validation-final-state validation)))
      (make-solution
        :depth (length path)
        :time (problem-state.time final-state)
        :value (problem-state.value final-state)
        :path path
        :goal final-state))))


(define-test-helper recorder-complete-path-valid-p (path)
  (let ((validation (recorder-path-validation path)))
    (and (action-sequence-validation-success-p validation)
         (validate-recorder-solution
           *start-state* path
           (action-sequence-validation-final-state validation)))))


(define-test-helper recorder-progress-with-happenings-p (snapshot final-state)
  (let ((original-happenings *happening-names*))
    (unwind-protect
      (progn
        (setf *happening-names* '(future-event))
        (recorder-completed-cycle-made-progress-p snapshot final-state))
      (setf *happening-names* original-happenings))))


(define-test-helper recorder-state-with-cycle-count (state count)
  (let ((copy (copy-problem-state state)))
    (dolist (proposition (database copy))
      (when (eql (first proposition) 'recorder-cycles-used)
        (delete-proposition proposition (problem-state.idb copy))))
    (add-proposition
      (list 'recorder-cycles-used count)
      (problem-state.idb copy))
    (invalidate-problem-state-hash copy)
    copy))


(define-test-claim recorder-multi-window-parser-contract
  (multiple-value-bind (cycles trailing-setup diagnostic)
      (parse-recorder-path *start-state* (recorder-three-window-path))
    (and (null diagnostic)
         (null trailing-setup)
         (equal (mapcar #'recorder-path-cycle.number cycles) '(1 2 3))
         (equal (recorder-path-cycle.setup (second cycles))
                '((4.0 (prepare-second-cycle live-agent))))
         (equal (recorder-path-cycle.setup (third cycles))
                '((8.0 (prepare-third-cycle live-agent))))
         (every #'recorder-path-cycle.ending cycles)))
  (multiple-value-bind (cycles trailing-setup diagnostic)
      (parse-recorder-path *start-state* (recorder-three-window-path t))
    (and (null diagnostic)
         (null trailing-setup)
         (= (length cycles) 3)
         (null (recorder-path-cycle.ending (third cycles))))))


(define-test-claim recorder-valid-multi-window-paths
  (recorder-complete-path-valid-p (recorder-two-window-path))
  (recorder-complete-path-valid-p (recorder-three-window-path))
  (recorder-complete-path-valid-p (recorder-three-window-path t))
  (let ((validation
          (recorder-path-validation (recorder-three-window-path))))
    (and (action-sequence-validation-success-p validation)
         (= (funcall
              (symbol-function 'recorder-cycle-count)
              (action-sequence-validation-final-state validation))
            3)
         (recorder-cycle-boundary-closed-p
           (action-sequence-validation-final-state validation)))))


(define-test-claim recorder-open-and-explicitly-closed-goals
  (let* ((open-path (recorder-three-window-path t))
         (closed-path (recorder-three-window-path))
         (open-playback (recorder-path-validation open-path))
         (closed-playback (recorder-path-validation closed-path))
         (original-goal (copy-tree *goal*)))
    (and
      (action-sequence-validation-success-p open-playback)
      (action-sequence-validation-success-p closed-playback)
      (unwind-protect
        (progn
          ;; The ordinary goal accepts the final recording as soon as its useful ghost
          ;; action completes, without inventing a STOP-RECORDER move.
          (install-compiled-goal '(third-recorded))
          (multiple-value-bind (open-valid-p open-diagnostic)
              (validate-recorder-solution
                *start-state* open-path
                (action-sequence-validation-final-state open-playback))
            (and open-valid-p
                 (null open-diagnostic)
                 ;; Strengthening the same goal requires the authored return/stop ending.
                 (progn
                   (install-compiled-goal
                     '(and (third-recorded) (ghost-stops-recorder)))
                   t)
                 (multiple-value-bind (closed-valid-p closed-diagnostic)
                     (validate-recorder-solution
                       *start-state* closed-path
                       (action-sequence-validation-final-state closed-playback))
                   (and closed-valid-p
                        (null closed-diagnostic)))
                 (multiple-value-bind (open-valid-p open-diagnostic)
                     (validate-recorder-solution
                       *start-state* open-path
                       (action-sequence-validation-final-state open-playback))
                   (and (not open-valid-p)
                        (equal open-diagnostic
                               '(:phase :playback :reason :goal-not-satisfied)))))))
        (install-compiled-goal original-goal)))))


(define-test-claim recorder-malformed-window-diagnostics
  (let ((diagnostic
          (recorder-boundary-diagnostic
            '((1.0 (start-recorder live-agent))
              (2.0 (start-recorder live-agent)))
            *start-state*)))
    (and (= (getf diagnostic :cycle) 1)
         (eql (getf diagnostic :detail) :multiple-starts)))
  (let ((diagnostic
          (recorder-boundary-diagnostic
            '((1.0 (stop-recorder ghost-agent)))
            *start-state*)))
    (and (= (getf diagnostic :cycle) 1)
         (eql (getf diagnostic :detail) :stop-without-start)))
  (let ((diagnostic
          (recorder-boundary-diagnostic
            (append
              (recorder-three-window-path)
              '((12.0 (start-recorder live-agent))))
            *start-state*)))
    (and (= (getf diagnostic :cycle) 4)
         (eql (getf diagnostic :detail) :maximum-exceeded))))


(define-test-claim recorder-invalid-stop-pruned-before-duplicate
  (let* ((path (recorder-invalid-closed-window-path))
         (validation (recorder-path-validation path)))
    (and
      ;; Integrated playback is physically executable through STOP.
      (action-sequence-validation-success-p validation)
      (multiple-value-bind (valid-p diagnostic)
          (validate-recorder-cycle-boundary-prefix
            *start-state* path
            (action-sequence-validation-final-state validation))
        (and (not valid-p)
             (= (getf diagnostic :cycle) 1)
             (eql (getf diagnostic :phase) :recording)
             (equal (getf diagnostic :action)
                    '(consume-invalid-cycle ghost-agent))))
      ;; The registered mandatory trigger rejects the same stop prefix.
      (not (candidate-search-prefix-valid-p
             path (action-sequence-validation-final-state validation))))))


(define-test-claim recorder-no-progress-cycle-pruned-before-duplicate
  (let* ((path (recorder-no-progress-second-window-path))
         (validation (recorder-path-validation path))
         (final-state (action-sequence-validation-final-state validation)))
    (and
      (action-sequence-validation-success-p validation)
      (multiple-value-bind (cycles trailing-setup diagnostic)
          (parse-recorder-path *start-state* path)
        (declare (ignore trailing-setup))
        (and (null diagnostic)
             (multiple-value-bind (snapshot snapshot-diagnostic)
                 (recorder-path-cycle-snapshot
                   *start-state* path (second cycles))
               (and (null snapshot-diagnostic)
                    (recorder-boundary-equivalent-p snapshot final-state)
                    (not (recorder-completed-cycle-made-progress-p
                           snapshot final-state))
                    (recorder-progress-with-happenings-p
                      snapshot final-state)
                    (let ((improved-state (copy-problem-state final-state))
                          (*solution-type* 'max-value))
                      (setf (problem-state.value improved-state)
                            (1+ (problem-state.value snapshot)))
                      (recorder-completed-cycle-made-progress-p
                        snapshot improved-state))))))
      (multiple-value-bind (valid-p diagnostic)
          (validate-recorder-cycle-boundary-prefix
            *start-state* path final-state)
        (and (not valid-p)
             (= (getf diagnostic :cycle) 2)
             (eql (getf diagnostic :reason)
                  :no-persistent-progress)))
      (not (candidate-search-prefix-valid-p path final-state)))))


(define-test-claim recorder-boundary-resource-dominance
  (let* ((path (recorder-no-progress-second-window-path))
         (validation (recorder-path-validation path))
         (final-state (action-sequence-validation-final-state validation)))
    (multiple-value-bind (cycles trailing-setup diagnostic)
        (parse-recorder-path *start-state* path)
      (declare (ignore trailing-setup))
      (multiple-value-bind (snapshot snapshot-diagnostic)
          (recorder-path-cycle-snapshot *start-state* path (second cycles))
        (and
          (action-sequence-validation-success-p validation)
          (null diagnostic)
          (null snapshot-diagnostic)
          (recorder-boundary-dominance-enabled-p)
          ;; The default one-cycle policy avoids the boundary frontier entirely.
          (let ((*max-recorder-cycles* 1))
            (not (recorder-boundary-dominance-enabled-p)))
          ;; Open states retain their complete identity and never enter this frontier.
          (let* ((start-validation
                   (recorder-path-validation
                     '((1.0 (start-recorder live-agent)))))
                 (open-state
                   (action-sequence-validation-final-state start-validation)))
            (and (action-sequence-validation-success-p start-validation)
                 (not (recorder-normalized-boundary-p open-state))))
          ;; Fewer cycles plus no worse depth dominates the equal physical boundary.
          (progn
            (reset-recorder-boundary-dominance)
            (and (not (prune-recorder-boundary-dominated-successor-p
                        (make-node :depth 2) snapshot))
                 (prune-recorder-boundary-dominated-successor-p
                   (make-node :depth 4) final-state)
                 (= *recorder-boundary-dominance-pruned* 1)
                 ;; Same-cycle duplicates remain the ordinary graph's responsibility.
                 (not (prune-recorder-boundary-dominated-successor-p
                        (make-node :depth 5) snapshot))))
          ;; A physical difference prevents resource dominance.
          (let ((changed-state (copy-problem-state final-state)))
            (add-proposition '(third-recorded)
                             (problem-state.idb changed-state))
            (invalidate-problem-state-hash changed-state)
            (not (prune-recorder-boundary-dominated-successor-p
                   (make-node :depth 6) changed-state)))
          ;; A cheaper state with more cycles remains on the Pareto frontier.
          (progn
            (reset-recorder-boundary-dominance)
            (and (not (prune-recorder-boundary-dominated-successor-p
                        (make-node :depth 9) snapshot))
                 (not (prune-recorder-boundary-dominated-successor-p
                        (make-node :depth 0) final-state))
                 (prune-recorder-boundary-dominated-successor-p
                   (make-node :depth 1)
                   (recorder-state-with-cycle-count final-state 3))))
          (progn
            (reset-search-successor-pruners)
            (and (zerop *recorder-boundary-dominance-pruned*)
                 (zerop (hash-table-count
                          *recorder-boundary-dominance-table*)))))))))


(define-test-claim recorder-multi-window-report-is-complete
  (let* ((path (recorder-two-window-path))
         (solution (recorder-solution-for-path path))
         (report (build-recorder-report solution))
         (cycles (getf report :cycles))
         (first-cycle (first cycles))
         (second-cycle (second cycles))
         (printed
           (with-output-to-string (stream)
             (print-recorder-report solution stream))))
    (and
      (eq (getf report :integrated) path)
      (= (getf report :cycle-count) 2)
      (= (length cycles) 2)
      (not (member :setup report))
      (equal (getf first-cycle :integrated) (subseq path 0 3))
      (null (getf first-cycle :setup))
      (equal (getf first-cycle :recording) (subseq path 0 3))
      (null (getf first-cycle :playback))
      (eql (getf first-cycle :closure) :explicit)
      (= (getf first-cycle :depth) 3)
      (= (getf first-cycle :elapsed-time) 3)
      (equal (getf second-cycle :integrated) (subseq path 3))
      (equal (getf second-cycle :setup) (list (fourth path)))
      (equal (getf second-cycle :recording) (subseq path 4))
      (null (getf second-cycle :playback))
      (eql (getf second-cycle :closure) :explicit)
      (= (getf second-cycle :depth) 4)
      (= (getf second-cycle :elapsed-time) 4)
      (null (getf report :trailing-setup))
      (equal (getf report :trailing-metrics)
             '(:depth 0 :elapsed-time 0.0 :value-change 0.0))
      (equal (getf report :totals)
             '(:depth 7 :elapsed-time 7.0 :value-change 0.0))
      (search "Recorder cycle 1:" printed)
      (search "Recorder cycle 2:" printed)
      (search "Complete solution totals: depth 7" printed)))
  (let* ((path (recorder-three-window-path t))
         (report (build-recorder-report (recorder-solution-for-path path)))
         (third-cycle (third (getf report :cycles))))
    (and
      (= (getf report :cycle-count) 3)
      (equal (getf third-cycle :integrated) (subseq path 7))
      (equal (getf third-cycle :setup) (list (eighth path)))
      (equal (getf third-cycle :recording)
             (append (subseq path 8) (list '(stop-recorder))))
      (null (getf third-cycle :playback))
      (eql (getf third-cycle :closure) :synthesized)
      (= (getf third-cycle :depth) 3)
      (= (getf third-cycle :elapsed-time) 3)
      (= (getf (getf report :totals) :depth) 10)))
  (let* ((path
           (append (recorder-two-window-path)
                   '((8.0 (prepare-third-cycle live-agent)))))
         (report (build-recorder-report (recorder-solution-for-path path))))
    (and
      (= (getf report :cycle-count) 2)
      (equal (getf report :trailing-setup) (last path))
      (equal (getf report :trailing-metrics)
             '(:depth 1 :elapsed-time 1.0 :value-change 0.0))
      (= (getf (getf report :totals) :depth) 8))))


(define-test-claim recorder-two-windows-in-one-search
  (let ((original-goal *goal*)
        (*depth-cutoff* 7))
    (unwind-protect
      (progn
        (install-compiled-goal
          '(and (second-recorded) (recorder-cycle-closed)))
        (solve)
        (let ((solution (select-continuation-solution)))
          (and *solutions-valid*
               solution
               (= (solution.depth solution) 6)
               (= (getf (build-recorder-report solution) :cycle-count)
                  2))))
      (install-compiled-goal original-goal))))


(define-goal
  (always-true))
