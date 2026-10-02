;;; Same-location release consequences, exercised without puzzle search.
(in-package :ww)
(ww-set *problem-name* support-settling-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 0)
(setf *expected-min-length* 0)

(define-types
  agent (live-holder ghost-holder live-other ghost-other live-rider ghost-rider)
  tray (live-tray ghost-tray live-nested-tray ghost-nested-tray)
  connector (live-connector ghost-connector)
  box (live-a ghost-a live-b ghost-b live-c ghost-c live-blocker ghost-blocker)
  recorder (recorder1)
  location (site away remote mid)
  transmitter (source)
  receiver (target)
  gate (gate1)
  pressure-plate (plate1)
  hue (red))

(include-tech recorder)
(include-tech tray)
(include-tech box)
(include-tech beam-relay)
(include-tech visibility)
(include-tech gate)
(include-tech plate)
(include-tech reachability)

(define-init
  (recording-copy> live-holder ghost-holder)
  (recording-copy> live-other ghost-other)
  (recording-copy> live-rider ghost-rider)
  (recording-copy> live-tray ghost-tray)
  (recording-copy> live-nested-tray ghost-nested-tray)
  (recording-copy> live-connector ghost-connector)
  (recording-copy> live-a ghost-a)
  (recording-copy> live-b ghost-b)
  (recording-copy> live-c ghost-c)
  (recording-copy> live-blocker ghost-blocker)
  (recording-in-progress)
  (has-position recorder1 site)
  (has-position plate1 remote)
  (has-location live-holder site)
  (has-location ghost-holder site)
  (has-location live-other site)
  (has-location live-rider away)
  (has-height live-holder 1)
  (has-height ghost-holder 1)
  (holding live-holder live-tray)
  (holding ghost-holder ghost-tray)
  (has-location live-tray site)
  (has-location ghost-tray site)
  (has-location live-nested-tray away)
  (has-location live-connector site)
  (has-location ghost-connector site)
  (on live-connector live-tray)
  (on ghost-connector ghost-tray)
  (has-location live-a away)
  (has-location live-b away)
  (has-location live-c away)
  (has-height live-a 1/2)
  (has-height live-b 1/2)
  (has-height live-c 1)
  (location-coords> mid 5 0)
  (location-coords> site 0 0)
  (location-coords> away 0 10)
  (location-coords> remote 0 20)
  (apparatus-coords> source -10 0 1)
  (apparatus-coords> target 10 0 1)
  (gate-segment> gate1 20 9 20 11)
  (has-location live-blocker mid)
  (has-height live-blocker 5/4)
  (has-chroma source red)
  (has-chroma target red)
  (paired live-connector source)
  (paired live-connector target)
  (los-via site () source)
  (los-via site (mid) target)
  (controls ((target)) gate1 normal)
  (reach-via site () remote))

(define-init-action initialize-derived-state
  0 () (always-true) ()
  ;; Authored initial ON facts require same-layer supports. Establish the runtime
  ;; rule-19 arrangement only after initialization, without weakening that check.
  (assert (not (on live-connector live-tray))
          (on live-connector ghost-tray)
          (propagate-changes!)))
(define-goal (always-true))

(define-test-helper settling-query (state name &rest args)
  (if (gethash name *relations*)
    (member (cons name args) (database state) :test #'equal)
    (apply (symbol-function name) state args)))

(define-test-helper settling-state (removals additions)
  (let ((state (copy-problem-state *start-state*)))
    (dolist (fact removals) (delete-proposition fact (problem-state.idb state)))
    (dolist (fact additions) (add-proposition fact (problem-state.idb state)))
    (invalidate-problem-state-hash state)
    state))

(define-test-helper settling-apply (state action)
  (multiple-value-bind (next valid diagnostic) (apply-action-to-state action state nil)
    (unless valid (error "Settling action failed: ~S: ~S" action diagnostic))
    next))

(define-test-helper settling-release (state)
  (settling-apply state '(put-tray ghost-holder ghost-tray ground site)))

(define-test-claim settling-equal-height-beam-and-layers
  (let ((next (settling-release *start-state*)))
    (and (settling-query *start-state* 'active 'target)
         (settling-query next 'on 'live-connector 'live-tray)
         (not (settling-query next 'on 'ghost-connector 'live-tray))
         (= (settling-query next 'base 'live-connector) 1)
         (= (settling-query next 'base 'ghost-connector) 0)
         (settling-query next 'paired 'live-connector 'source)
         (settling-query next 'paired 'live-connector 'target)
         (settling-query next 'active 'target)
         (settling-query next 'open 'gate1)
         (not (settling-query next 'recording-active 'target))
         (settling-query *start-state* 'on 'live-connector 'ghost-tray)
         (= (- (problem-state.time next) (problem-state.time *start-state*)) 1))))

(define-test-claim settling-ground-changes-beam-not-pairings
  (let ((next (settling-release
                (settling-state '((holding live-holder live-tray)) nil))))
    (and (= (settling-query next 'base 'live-connector) 0)
         (not (settling-query next 'active 'target))
         (not (settling-query next 'open 'gate1))
         (settling-query next 'paired 'live-connector 'target))))

(define-test-claim settling-highest-lower-stack
  (let* ((before (settling-state
                   '((holding live-holder live-tray) (on live-connector ghost-tray))
                   '((on live-c ghost-tray) (has-location live-c site)
                     (on live-connector live-c) (has-location live-a site))))
         (next (settling-release before)))
    (and (settling-query next 'on 'live-c 'live-a)
         (settling-query next 'on 'live-connector 'live-c)
         (= (settling-query next 'base 'live-c) 1/2)
         (= (settling-query next 'base 'live-connector) 3/2)
         (settling-query next 'has-location 'live-connector 'site))))

(define-test-claim settling-no-colocation-or-movement-transfer
  (let ((state (copy-problem-state *start-state*)))
    (settling-query state 'propagate-changes!)
    (settling-query state 'apply-agent-configuration! 'ghost-holder '(away ground))
    (settling-query state 'propagate-changes!)
    (and (settling-query state 'on 'live-connector 'ghost-tray)
         (settling-query state 'has-location 'live-connector 'away)
         (settling-query state 'has-location 'live-tray 'site))))

(define-test-claim settling-remote-release-remains-unloading
  (let* ((state (settling-state nil '((has-location live-a remote))))
         (next (settling-apply state '(put-tray ghost-holder ghost-tray ground remote))))
    (and (= (settling-query next 'base 'live-connector) 0)
         (settling-query next 'has-location 'live-connector 'remote)
         (settling-query next 'has-location 'ghost-holder 'site)
         (not (settling-query next 'on 'live-connector 'live-a)))))

(define-test-claim settling-ambiguous-highest-surfaces
  (expect-condition
    (lambda ()
      (settling-release
        (settling-state '((holding live-holder live-tray))
                        '((has-location live-a site) (has-location live-b site)))))
    'error :containing "Ambiguous settling"))

(define-test-claim settling-occupied-higher-and-incompatible
  (let ((next (settling-release
                (settling-state nil '((on live-c live-tray) (has-location live-c site)
                                      (has-location ghost-a site))))))
    ;; The occupied live tray and its too-high box cannot catch; ghost box is forbidden.
    (and (= (settling-query next 'base 'live-connector) 0)
         (not (settling-query next 'support-use-allowed 'live-connector 'ghost-a))
         (not (settling-query next 'support-use-allowed 'ghost-connector 'live-a)))))

(define-test-claim settling-descendants-and-holder-cycle
  (let ((state (settling-state '((on live-connector ghost-tray))
                 '((on live-c ghost-tray) (has-location live-c site)
                   (on live-rider live-c) (has-location live-rider site)
                   (holding live-rider live-nested-tray)
                   (has-location live-nested-tray site)
                   (on live-connector live-nested-tray)))))
    (and (settling-query state 'support-dependent-p 'live-nested-tray 'live-c nil)
         (null (settling-query state 'settling-support 'live-c 'site 10
                              '(live-tray ghost-tray live-a live-b)))
         (progn
           (settling-query state 'relocate-stack! 'live-c 'away)
           (and (settling-query state 'has-location 'live-nested-tray 'away)
                (settling-query state 'has-location 'live-connector 'away))))))

(define-test-claim settling-support-cycle-errors
  (expect-condition
    (lambda ()
      (settling-query (settling-state nil '((on live-a live-b) (on live-b live-a)))
                      'support-dependent-p 'live-a 'live-c nil))
    'error :containing "cycle"))

(define-test-claim settling-placed-tray-consumes-capacity
  (let* ((state (settling-state '((holding live-holder live-tray))
                              '((has-location ghost-a site))))
         (next (settling-apply state '(put-tray ghost-holder ghost-tray ghost-a site))))
    (and (settling-query next 'on 'ghost-tray 'ghost-a)
         (= (settling-query next 'base 'ghost-connector) 0))))

(define-test-claim settling-boundary-policy-unchanged
  (let ((state (settling-state '((holding live-holder live-tray)) nil)))
    (and (not (settling-query state 'recorder-cycle-boundary-safe))
         (let ((next (settling-apply state '(cancel-playback live-holder))))
           (and (not (recorder-state-contains-ghost-reference-p next))
                (= (settling-query next 'base 'live-connector) 0)
                (not (settling-query next 'on 'live-connector 'live-tray)))))))

(define-test-claim settling-prefers-highest-and-nested-support
  (let ((next (settling-release
                (settling-state '((holding live-holder live-tray))
                  '((has-location live-a site) (has-location live-b site)
                    (on live-b live-a))))))
    (and (settling-query next 'on 'live-connector 'live-b)
         (= (settling-query next 'base 'live-connector) 1))))

(define-test-claim settling-ground-preserves-upper-stack
  (let ((next (settling-release
                (settling-state '((holding live-holder live-tray)
                                  (on live-connector ghost-tray))
                  '((on live-c ghost-tray) (has-location live-c site)
                    (on live-connector live-c))))))
    (and (= (settling-query next 'base 'live-c) 0)
         (= (settling-query next 'base 'live-connector) 1)
         (settling-query next 'on 'live-connector 'live-c))))

(define-test-claim settling-does-not-use-other-location
  ;; The live tray remains a valid equal-height support, but is no longer at SITE.
  (let ((next (settling-release
                (settling-state nil '((has-location live-holder away)
                                      (has-location live-tray away))))))
    (= (settling-query next 'base 'live-connector) 0)))

(define-test-claim settling-floor-support-precedes-ground
  (let* ((state (settling-state '((holding live-holder live-tray)) nil))
         (riders '((live-connector 1) (ghost-connector 1))))
    ;; Direct selection at the shared plate exercises per-layer joint capacity.
    (let ((forward (settling-query state 'released-rider-landings riders 'remote))
          (reverse-order (settling-query state 'released-rider-landings (reverse riders) 'remote)))
      (and (equal (assoc 'live-connector forward) '(live-connector plate1))
           (equal (assoc 'ghost-connector forward) '(ghost-connector plate1))
           (equal (assoc 'live-connector reverse-order) (assoc 'live-connector forward))
           (equal (assoc 'ghost-connector reverse-order) (assoc 'ghost-connector forward))))))

(define-test-claim settling-no-propagated-ground-intermediate
  (let ((original (symbol-function 'propagate-changes!))
        (observations 0))
    (unwind-protect
        (progn
          (setf (symbol-function 'propagate-changes!)
                (lambda (state)
                  (incf observations)
                  (unless (and (settling-query state 'on 'live-connector 'live-tray)
                               (= (settling-query state 'base 'live-connector) 1))
                    (error "Propagation saw a transient grounded connector"))
                  (funcall original state)))
          (settling-release *start-state*)
          (plusp observations))
      (setf (symbol-function 'propagate-changes!) original))))

(define-test-claim settling-copy-hash-and-fixpoint
  (let* ((before (copy-problem-state *start-state*))
         (old-hash (ensure-idb-hash before))
         (next (settling-release before))
         (next-hash (ensure-idb-hash next))
         (facts (copy-tree (database next))))
    (settling-query next 'propagate-changes!)
    (and (= old-hash (ensure-idb-hash before))
         (= next-hash (compute-idb-hash (problem-state.idb next)))
         (equal facts (database next))
         (settling-query before 'on 'live-connector 'ghost-tray)
         (settling-query next 'on 'live-connector 'live-tray))))

(define-test-claim settling-lower-ties-do-not-hide-unique-highest
  (let ((next (settling-release
                (settling-state nil '((has-location live-a site)
                                      (has-location live-b site))))))
    (and (settling-query next 'on 'live-connector 'live-tray)
         (= (settling-query next 'base 'ghost-connector) 0))))

(define-test-claim settling-stop-after-resolved-dependency
  (let* ((before (settling-state nil '((has-location ghost-other site)
                                      (has-location ghost-rider site))))
         (released (settling-release before))
         (closed (settling-apply released '(stop-recorder ghost-holder))))
    (and (not (settling-query before 'recorder-cycle-boundary-safe))
         (settling-query released 'recorder-cycle-boundary-safe)
         (not (recorder-state-contains-ghost-reference-p closed))
         (settling-query closed 'on 'live-connector 'live-tray)
         (settling-query closed 'paired 'live-connector 'target)
         (settling-query closed 'active 'target))))
