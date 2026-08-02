;;; Filename: ww-problem-tests.lisp

;;; Runs through some test problems, checking that they stage properly
;;; and solve correctly.


(in-package :ww)


;Any additions to this list requires rebuilding problem-test-solutions.lisp
;in the run-test-problems function below.
(defvar *test-problem-files*
  '("problem-blocks3.lisp" "problem-blocks3a.lisp" "problem-blocks4.lisp" "problem-boxes.lisp"
    "problem-jugs2.lisp" "problem-jugs4.lisp" "problem-queens4.lisp" "problem-queens8.lisp"
    "problem-captjohn.lisp" "problem-quern.lisp" "problem-graveyard.lisp" "problem-sentry.lisp"
    ;"problem-crossword5-11.lisp"  ;runs out of default memory
    ;"problem-crossword15-18.lisp"  ;runs out of default memory
    "problem-crossword13.lisp" "problem-array-path.lisp"
    "problem-tiles0a-csp.lisp" "problem-tiles1a-heuristic.lisp"
    "problem-tiles1e-heuristic.lisp"
    ;"problem-tiles1b.lisp"  ;takes too long
    ;"problem-tiles1c.lisp"  ;takes too long
    ;"problem-tiles1d.lisp"  ;needs debugging
    ;"problem-tiles2a.lisp"  ;takes too long
    ;"problem-tiles2a-heuristic.lisp"  ;takes too long
    ;"problem-tiles2b.lisp"  ;takes too long
    ;"problem-tiles2c.lisp"  ;takes too long
    ;"problem-tiles3a-heuristic.lisp"  ;takes too long
    ;"problem-tiles5a-heuristic.lisp"  ;takes too long
    ;"problem-tiles5b-heuristic.lisp"  ;needs debugging
    ;"problem-tiles7a-heuristic2.lisp"  ;takes too long
    ;"problem-tiles7a-heuristic3.lisp"  ;takes too long
    ;"problem-tiles0b-csp.lisp"  ;takes too long
    ;"problem-tiles7a-heuristic.lisp"  ;takes too long
    "problem-hanoi.lisp"
    ;"problem-triangle.lisp"  ;needs debugging
    ;"problem-triangle-backward.lisp"  ;takes too long
    "problem-triangle-xy.lisp" "problem-triangle-xyz.lisp" "problem-triangle-heuristic.lisp"
    "problem-triangle-macros.lisp" "problem-triangle-macros-one.lisp" "problem-triangle-xyz-one.lisp"
    "problem-tsp.lisp"
    "problem-u2.lisp" "problem-donald.lisp" "problem-knap4a.lisp" "problem-knap4b.lisp"
    ;"problem-crater.lisp"  ;needs debugging
    "problem-knap19.lisp"
    ;"problem-socrates1.lisp"  ;needs debugging
    ;"problem-socrates2.lisp"  ;needs debugging
    ;"problem-smallspace-macro.lisp"  ;needs debugging
    ;"problem-smallspace2.lisp"  ;takes too long
    "problem-smallspace.lisp"))


;Any additions to this list requires deleting problem-test-bt-solutions.lisp
;and re-running (test-bt) to rebuild it.
;One representative chosen per problem class to avoid redundancy.
;hanoi and donald have no native depth-cutoff; overrides set in run-bt-test-problems.
(defvar *test-bt-problem-files*
  '("problem-blocks3.lisp"              ;tree, every, non-fluent assertions
    "problem-blocks3a.lisp"             ;graph->tree, every, fluent bind
    ;"problem-blocks4.lisp"             ;redundant with blocks3
    "problem-boxes.lisp"                ;graph->tree, min-length, multi-object
    ;"problem-jugs2.lisp"               ;min-time solution type not supported by bt (use quern for fluent arithmetic)
    ;"problem-jugs4.lisp"               ;redundant with jugs2
    "problem-queens4.lisp"              ;tree, every, structured assignment
    ;"problem-queens8.lisp"             ;redundant with queens4
    "problem-captjohn.lisp"             ;csp, variable-per-level assignment
    "problem-quern.lisp"                ;first, depth-cutoff 8, conditional assert
    ;"problem-graveyard.lisp"            ;min-length on tree requires exhausting 12^14 nodes; no solution within native depth-cutoff 10
    ;"problem-sentry.lisp"              ;has define-happening, incompatible with bt
    "problem-crossword13.lisp"          ;tree, first, string state, nested updates
    "problem-array-path.lisp"           ;tree, min-length, no-solution case
    ;"problem-tiles0a-csp.lisp"         ;takes too long
    ;"problem-tiles1a.lisp"              ;graph->tree, min-length, list-coord state--takes too long with bt
    ;"problem-tiles1a-heuristic.lisp"   ;heuristic unused by bt, redundant with tiles1a
    ;"problem-tiles1e-heuristic.lisp"   ;same
    "problem-hanoi.lisp"                ;min-length, depth-cutoff 9 set in run-bt-test-problems
    "problem-triangle-xyz.lisp"         ;first, canonical triangle form
    ;"problem-triangle-xy.lisp"         ;redundant with triangle-xyz
    "problem-triangle-macros.lisp"      ;tree, first, multiple asserts per action
    ;"problem-triangle-macros-one.lisp" ;redundant with triangle-macros
    ;"problem-triangle-xyz-one.lisp"    ;redundant with triangle-xyz
    ;"problem-tsp.lisp"                 ;min-value solution type not supported by bt
    "problem-u2.lisp"                   ;min-length, time-constrained preconditions
    "problem-donald.lisp"               ;tree, first, depth-cutoff 6 set in run-bt-test-problems
    ;"problem-knap4a.lisp"              ;max-value solution type not supported by bt
    ;"problem-knap4b.lisp"              ;max-value solution type not supported by bt
    ;"problem-knap19.lisp"              ;too slow
    ;"problem-crater.lisp"              ;ok
    ;"problem-smallspace2.lisp"         ;takes too long
    ;"problem-smallspace.lisp"          ;takes too long
))


;;; Helper Functions ;;;

(defun parse-problem-name (problem-filename)
  "Extract problem name from filename (e.g., 'problem-blocks3.lisp' -> 'blocks3')"
  (if (string-prefix-p "problem-" problem-filename)
      (subseq problem-filename 8 (- (length problem-filename) 5))
      (subseq problem-filename 0 (- (length problem-filename) 5))))


(defun prompt-user-action (problem-name)
  "Prompt user for Continue/Skip/All/Quit. Returns (values should-process continue-all)"
  (format t "~%=====================================================~%")
  (format t "Process problem: ~A~%" (string-upcase problem-name))
  (format t "=====================================================~%")
  (format t "Continue, Skip, All, Quit: ")
  (force-output)
  (let* ((response (read-line))
         (choice (if (> (length response) 0)
                     (char-upcase (char response 0))
                     #\C)))
    (case choice
      (#\Q (values nil :quit))
      (#\S (values nil nil))
      (#\A (values t t))
      (#\C (values t nil))
      (t (values t nil)))))


(defun cleanup-test-files ()
  "Delete temporary problem.lisp and vals.lisp files"
  (uiop:delete-file-if-exists (in-src "problem.lisp"))
  (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))))


(defun print-test-header (problem-name &optional (algorithm ""))
  "Print test header for a problem"
  (format t "~%=====================================================~%")
  (format t "Process problem~A: ~A~%"
          (if (string= algorithm "") "" (format nil " (~A)" algorithm))
          (string-upcase problem-name))
  (format t "=====================================================~%"))


(defun collect-solution-data ()
  "Collect best solution and state from current problem results"
  (let ((best-solution (ut::if-it (first *solution-paths*) (solution.path ut::it)))
        (best-state (when *best-states*
                      (alexandria:hash-table-alist (problem-state.idb (first *best-states*))))))
    (list best-solution best-state)))


(defun run-test-problems ()
  (cleanup-test-files)
  (reset-parameters)  ; Initial reset for the test suite
  (with-silenced-compilation
    (let* ((problems-to-run *test-problem-files*)
           (test-solutions-file (merge-pathnames "problem-test-solutions.lisp"
                                                 (asdf:system-source-directory :wouldwork)))
           (problem-test-solutions (if (probe-file test-solutions-file)
                                     (read-hash-table-from-file test-solutions-file)
                                     (make-hash-table :test #'equal)))
           (problems-processed 0)
           (continue-all nil)
           failed-problems)
      (loop for problem in problems-to-run
            do (let* ((problem-name (parse-problem-name problem))
                      (should-process t))
                 (print-test-header problem-name)

                 (unless continue-all
                   (format t "Continue, Skip, All, Quit: ")
                   (force-output)
                   (let* ((response (read-line))
                          (choice (if (> (length response) 0)
                                      (char-upcase (char response 0))
                                      #\C)))
                     (case choice
                       (#\Q (return-from run-test-problems nil))
                       (#\S (setf should-process nil))
                       (#\A (setf continue-all t))
                       (#\C nil)
                       (t nil))))

                 (when should-process
                   (reset-parameters)  ; RESET PARAMETERS BEFORE EACH TEST
                   (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
                   (load-problem problem-name)
                   (incf problems-processed)
                   (ww-solve)
                   (let ((solution-data (collect-solution-data)))
                     (unless (equalp solution-data
                                     (gethash problem-name problem-test-solutions))
                       (format t "~%The problem solution above does not match the expected solution:")
                       (format t "~%~A~2%" (gethash problem-name problem-test-solutions))
                       (push problem-name failed-problems))
                     (unless (probe-file test-solutions-file)
                       (setf (gethash problem-name problem-test-solutions)
                             solution-data))
                     t))))
      (cleanup-test-files)
      (stage blocks3)
      (format t "~%~%Final Summary:~%")
      (format t "Total test problems run: ~D~%" (length *test-problem-files*))
      (format t "Test failures: ~D~%" (length failed-problems))
      (format t "Failed problems: ~A~%" (reverse failed-problems))
      (format t "Note: A failed problem solution is not necessarily wrong, but different from the reference solution,")
      (format t "a common occurrence when running in parallel mode.")
      (progn (unless (probe-file test-solutions-file)
               (write-hash-table-to-file problem-test-solutions
                 (merge-pathnames "problem-test-solutions.lisp" (asdf:system-source-directory :wouldwork))))
             t)
      t)))


(defun test ()
  "Run standard test suite using depth-first search."
  (run-test-problems))


(defvar *expected-min-length* nil
  "Test-only.  When a test/problem-*.lisp file sets this (plain SETF, not WW-SET --
   it is test metadata, not a search-control parameter, and must not be persisted to
   vals.lisp), TEST-TALOS requires the solved plan to have exactly this length under
   min-length search.  NIL performs no check.")


;;; MUTATION VALIDATION ;;;

;;; Supports TEST-TALOS :VALIDATE T: a small, hand-maintained table of deliberate
;;; mutations, each confirming that one specific negative check actually has teeth --
;;; the automated form of temporarily re-breaking a fix to confirm its test goes red.
;;; Nothing can auto-derive which line of a tech/ file a given assertion guards, so
;;; this can't be fully automatic; each case names a function, a broken version of it,
;;; and the one test file expected to fail when that version is installed.


(defstruct mutation-case
  target-name    ;symbol whose symbol-function gets temporarily swapped
  install-thunk  ;0-arg function whose call installs the broken version
  test-file      ;the one test/problem-*.lisp file expected to then fail
  note)          ;what real bug this simulates


(defmacro with-broken-function ((name install-form) &body body)
  "Temporarily rebind NAME's symbol-function for the duration of BODY.  INSTALL-FORM
   performs the replacement -- for a DEFINE-QUERY/DEFINE-UPDATE target this is an
   INSTALL-QUERY/INSTALL-UPDATE call followed by COMPILE, exactly mirroring how
   COMPILE-ALL-FUNCTIONS installs the real one, since COMPILE's effect on a named
   function is itself to rebind its symbol-function.  The original definition is
   saved before INSTALL-FORM runs and restored by UNWIND-PROTECT once BODY completes
   or signals, so a crash mid-run never leaves NAME's broken version bound in the
   image."
  `(let* ((target ,name)
          (original (symbol-function target)))
     (unwind-protect
       (progn ,install-form ,@body)
       (setf (symbol-function target) original))))


(defun rebuild-action-precondition (action-name new-precondition)
  "Test-only.  Recompiles ACTION-NAME's precondition function from NEW-PRECONDITION, a
   hand-edited variant of its own :precondition-form, using BUILD-PRECONDITION-LAMBDA --
   the same entry point CREATE-ACTION uses -- so a mutation case can swap in a broken
   inline precondition clause the same way an INSTALL-QUERY-based case swaps in a broken
   query body.  Re-derives PRE-PARAM-?VARS/TYPES fresh from the action's own stored,
   unmutated :PRECONDITION-PARAMS, but reuses its ORIGINAL PRE-$VARS (filtered from
   :PRECONDITION-VARIABLES) and EFF-ARGS unchanged: the precondition-lambda's success
   return value, (list ,@eff-args), must destructure into the already-compiled, untouched
   EFFECT-LAMBDA with the same variable set and order, so EFF-ARGS cannot be recomputed
   from the (possibly narrower) mutated form without desyncing that contract."
  (let* ((action (find action-name *actions* :key #'action.name))
         (pre-params (action.precondition-params action)))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params pre-params)
      (let* ((flat-pre-param-?vars (alexandria:flatten pre-param-?vars))
             (*var-type-env* (append (mapcar #'cons flat-pre-param-?vars (flatten-param-types pre-param-types))
                                      *var-type-env*))
             (pre-$vars (remove-if-not #'$varp (action.precondition-variables action)))
             (pre-special-$vars (get-special-vars (action.precondition-form action)))
             (eff-args (append flat-pre-param-?vars pre-$vars pre-special-$vars)))
        (compile (action.pre-defun-name action)
                 (subst-int-code
                   (build-precondition-lambda action-name new-precondition pre-param-?vars pre-$vars eff-args)))))))


(defparameter *mutation-cases*
  (list
    (make-mutation-case
      :target-name 'obstacle-clear
      :test-file "problem-ladder-test.lisp"
      :note "Simulates dropping the not-holding guard on ladder's own OBSTACLE-CLEAR
             branch -- eg a refactor that copies screen's clause and forgets it for
             ladder.  Carrying-agent's negative probe (ladder use while carrying)
             should then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'obstacle-clear
            '(?agent agent ?obstacle (either gate screen ladder gears))
            '(or (and (gate ?obstacle) (open ?obstacle))
                 (and (screen ?obstacle) (not (bind (holding ?agent $any-held-object))))
                 (ladder ?obstacle)
                 (and (gears ?obstacle) (stream-obstacle-clear ?agent ?obstacle))))
          (compile 'obstacle-clear (subst-int-code (symbol-value 'obstacle-clear)))))
    (make-mutation-case
      :target-name 'safe
      :test-file "problem-jump-test.lisp"
      :note "Simulates the destination-safety check being disabled entirely -- eg a
             stray debugging override left in place.  The goal directly asserts
             (not (safe unsafe-goal)), so this should make the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'safe '(?location location) '(not nil))
          (compile 'safe (subst-int-code (symbol-value 'safe)))))
    (make-mutation-case
      :target-name 'arbitrate-crossings
      :test-file "problem-beam-crossing-cascade-test.lisp"
      :note "Simulates losing the numeric priority branch, degrading to the
             alphabetical-only tie-break the file's own header documents as
             insufficient for this four-way loop."
      :install-thunk
        (lambda ()
          (install-query 'arbitrate-crossings '(?candidate)
            '(do (assign $kept nil)
                 (assign $remaining ?candidate)
                 (ww-loop for $round from 1 to (length ?candidate)
                          do (assign $lighting (compute-relay-lighting $kept))
                             (assign $best nil)
                             (doall (?x (get-current-crossings))
                               (if (and (member ?x $remaining)
                                        (crossing-reaches ?x $kept $lighting))
                                 (if (or (not $best)
                                         (string< (symbol-name ?x) (symbol-name $best)))
                                   (assign $best ?x))))
                             (if (not $best)
                               (return t)
                               (do (assign $kept (cons $best $kept))
                                   (assign $remaining (remove $best $remaining)))))
                 $kept))
          (compile 'arbitrate-crossings
                   (subst-int-code (symbol-value 'arbitrate-crossings)))))
    (make-mutation-case
      :target-name 'obstacle-clear
      :test-file "problem-ladder-test.lisp"
      :note "Simulates dropping the open-gate guard on OBSTACLE-CLEAR's gate branch --
             eg a refactor that treats gate like ladder/screen and forgets gate must
             also be open.  Gate-agent's negative probe (closed gate should block the
             flat conjunction) should then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'obstacle-clear
            '(?agent agent ?obstacle (either gate screen ladder gears))
            '(or (gate ?obstacle)
                 (and (screen ?obstacle) (not (bind (holding ?agent $any-held-object))))
                 (and (ladder ?obstacle) (not (bind (holding ?agent $any-held-object))))
                 (and (gears ?obstacle) (stream-obstacle-clear ?agent ?obstacle))))
          (compile 'obstacle-clear (subst-int-code (symbol-value 'obstacle-clear)))))
    (make-mutation-case
      :target-name 'jump-elevation-reachable
      :test-file "problem-jump-test.lisp"
      :note "Simulates the upward-height restriction being dropped entirely -- eg an
             off-by-something that always passes.  The goal directly asserts
             (not (jump-elevation-reachable boundary-agent 5)), a static fact
             independent of any plan, so this should make the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'jump-elevation-reachable '(?agent agent ?target-elevation) '(not nil))
          (compile 'jump-elevation-reachable
                   (subst-int-code (symbol-value 'jump-elevation-reachable)))))
    (make-mutation-case
      :target-name 'same-crossing-set
      :test-file "problem-beam-crossing-cascade-test.lisp"
      :note "Simulates dropping the length check from the fixpoint/oscillation
             comparison, leaving only the subset check.  On this geometry, round 2's
             empty NEXT set is then wrongly seen as equal to round 1's four-crossing
             ACTIVE set, so UPDATE-CROSSING-STATUS! resolves immediately with zero
             active crossings instead of continuing through oscillation detection to
             ARBITRATE-CROSSINGS.  Reachable via the same recomputation helper as the
             arbitrate-crossings case above."
      :install-thunk
        (lambda ()
          (install-query 'same-crossing-set '(?left ?right)
            '(ww-loop for $crossing in ?left always (member $crossing ?right)))
          (compile 'same-crossing-set
                   (subst-int-code (symbol-value 'same-crossing-set)))))
    (make-mutation-case
      :target-name 'use-ladder-pre-fn
      :test-file "problem-ladder-test.lisp"
      :note "Simulates dropping USE-LADDER's not-already-supported guard -- eg a refactor
             that assumes ground-only movement never needs re-checking support.
             Supported-agent's negative probe (already on a support-box) should then
             wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'use-ladder
            '(and (bind (has-location ?agent $a-location))
                  (bind (has-position ?ladder $ladder-location))
                  (eql $a-location $ladder-location)
                  (bind (climb-via> $a-location $means ?destination))
                  (member ?ladder $means)
                  (one-way-clear ?agent $means)
                  (safe ?destination)))))
    (make-mutation-case
      :target-name 'use-ladder-pre-fn
      :test-file "problem-ladder-test.lisp"
      :note "Simulates dropping USE-LADDER's exact-positioning check -- eg a refactor
             that assumes an edge's climb-via> origin implies the agent is at the ladder
             itself.  Misplaced-agent's negative probe (ladder fixed elsewhere) should
             then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'use-ladder
            '(and (bind (has-location ?agent $a-location))
                  (not (bind (on ?agent $anyplace)))
                  (bind (has-position ?ladder $ladder-location))
                  (bind (climb-via> $a-location $means ?destination))
                  (member ?ladder $means)
                  (one-way-clear ?agent $means)
                  (safe ?destination)))))
    (make-mutation-case
      :target-name 'use-ladder-pre-fn
      :test-file "problem-ladder-test.lisp"
      :note "Simulates dropping USE-LADDER's means-membership check -- eg a refactor
             that assumes any correctly-positioned ladder at the edge's origin must be
             one of its enabling means.  Unlisted-agent's negative probe (ladder6
             positioned correctly but absent from the edge's means list) should then
             wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'use-ladder
            '(and (bind (has-location ?agent $a-location))
                  (not (bind (on ?agent $anyplace)))
                  (bind (has-position ?ladder $ladder-location))
                  (eql $a-location $ladder-location)
                  (bind (climb-via> $a-location $means ?destination))
                  (one-way-clear ?agent $means)
                  (safe ?destination)))))
    (make-mutation-case
      :target-name 'walk-pre-fn
      :test-file "problem-walkability-test.lisp"
      :note "Simulates dropping WALK's not-already-supported guard -- eg a refactor that
             assumes the derived walkable-locations closure alone gates movement.
             Supported-agent's negative probe (already on a support-box) should then
             wrongly produce a successor, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'walk
            '(and (bind (has-location ?agent $a-location))
                  (assign $walkable-locations (walkable-locations ?agent $a-location))))))
    (make-mutation-case
      :target-name 'step-on-pre-fn
      :test-file "problem-step-test.lisp"
      :note "Simulates dropping STEP-ON's not-already-supported guard -- eg a refactor
             that assumes exact colocation and CLEARTOP are the only requirements.
             Supported-agent's negative probe (already on current-plate, alternate-plate
             clear and colocated) should then wrongly pass, making the goal
             unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'step-on
            '(and (bind (has-location ?agent $a-location))
                  (or (and (plate ?fixture)
                           (bind (has-position ?fixture $f-location)))
                      (and (fan ?fixture)
                           (bind (mounted-on ?fixture $gears))
                           (bind (has-location ?fixture $f-location))))
                  (eql $a-location $f-location)
                  (cleartop ?fixture)))))
    (make-mutation-case
      :target-name 'step-on-pre-fn
      :test-file "problem-step-test.lisp"
      :note "Simulates dropping STEP-ON's exact-colocation check -- eg a refactor that
             assumes any clear steppable fixture of the right kind is close enough.
             Loose-agent's negative probe (remote-plate positioned elsewhere) should
             then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'step-on
            '(and (bind (has-location ?agent $a-location))
                  (not (bind (on ?agent $anyplace)))
                  (or (and (plate ?fixture)
                           (bind (has-position ?fixture $f-location)))
                      (and (fan ?fixture)
                           (bind (mounted-on ?fixture $gears))
                           (bind (has-location ?fixture $f-location))))
                  (cleartop ?fixture)))))
    (make-mutation-case
      :target-name 'pickup-fan-pre-fn
      :test-file "problem-gears-fan-test.lisp"
      :note "Simulates dropping PICKUP-FAN's not-welded guard -- eg a refactor that
             treats welding as purely a display/consequence detail rather than a
             pickup blocker.  The welded-fan negative probe (otherwise clear and
             reachable) should then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'pickup-fan
            '(and (bind (has-location ?agent $a-location))
                  (or (and (bind (has-location ?fan $fan-location))
                           (cleartop ?fan)
                           (pickup-clear ?agent $a-location ?fan $fan-location))
                      (and (bind (mounted-on ?fan $w-gears))
                           (wall-gears $w-gears)
                           (not (bind (holding ?agent $any-held)))
                           (bind (has-position $w-gears $fan-location))
                           (reachable $fan-location $a-location)
                           (within-agent-vertical-reach ?agent (gears-elevation $w-gears))))))))
    (make-mutation-case
      :target-name 'mount-fan-pre-fn
      :test-file "problem-gears-fan-test.lisp"
      :note "Simulates dropping MOUNT-FAN's not-already-occupied guard -- eg a refactor
             that assumes reach and vertical clearance alone determine a legal mount.
             The occupied-gears negative probe (already carrying gear-occupant-fan)
             should then wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'mount-fan
            '(and (holding ?agent ?fan)
                  (bind (has-location ?agent $a-location))
                  (bind (has-position ?gears $g-location))
                  (reachable $g-location $a-location)
                  (within-agent-vertical-reach ?agent (gears-elevation ?gears))))))
    (make-mutation-case
      :target-name 'jam-target-pre-fn
      :test-file "problem-jammer-test.lisp"
      :note "Simulates dropping JAM-TARGET's JAM-DISALLOWED> guard -- eg a refactor that
             assumes reach and visibility alone determine a legal jam placement.  The
             disallowed-agent negative probe (geometrically and visually legal, but an
             authored JAM-DISALLOWED> fact names this exact triple) should then wrongly
             pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'jam-target
            '(and (bind (holding ?agent $any-jammer))
                  (jammer $any-jammer)
                  (bind (has-location ?agent $a-location))
                  (reachable ?location $a-location)
                  (or (and (or (gate ?target) (gun ?target))
                           (visible ?location ?target))
                      (and (or (floor-gears ?target) (wall-gears ?target))
                           (bind (has-position ?target $t-location))
                           (or (eql ?location $t-location)
                               (visible ?location $t-location))))
                  (assign $places (placement-options ?agent ?location $any-jammer))))))
    (make-mutation-case
      :target-name 'step-off-pre-fn
      :test-file "problem-step-test.lisp"
      :note "Simulates dropping STEP-OFF's STEPPABLE type guard -- eg a refactor that
             assumes anything an agent can be ON is a valid step-off target, blurring
             the line with jump's box-drop.  The box-agent negative probe (resting on
             a box, not a steppable) should then wrongly pass, making the goal
             unsatisfiable."
      :install-thunk
        (lambda ()
          (rebuild-action-precondition 'step-off
            '(and (bind (on ?agent $fixture))
                  (bind (has-location ?agent $a-location))))))
    (make-mutation-case
      :target-name 'visible-clear
      :test-file "problem-visibility-test.lisp"
      :note "Simulates dropping VISIBLE-CLEAR's open-state check -- eg a refactor that
             conflates the operational gate-transparency test with POTENTIALLY-VISIBLE's
             structural one.  Cascades into VISIBLE and BEAM-VISIBLE, whose own
             closed-gate negative probes (mixed-site, blocked-target-site, blocked-left/
             right) would then also wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'visible-clear '(?occluder gate) '(gate ?occluder))
          (compile 'visible-clear (subst-int-code (symbol-value 'visible-clear)))))
    (make-mutation-case
      :target-name 'reachable-clear
      :test-file "problem-reachability-test.lisp"
      :note "Simulates dropping REACHABLE-CLEAR's open-state check -- eg a refactor that
             treats any gate barrier as passable regardless of its current state.
             Cascades into REACHABLE, whose own closed-gate and mixed-edge negative
             probes would then also wrongly pass, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-query 'reachable-clear '(?barrier gate) '(gate ?barrier))
          (compile 'reachable-clear (subst-int-code (symbol-value 'reachable-clear)))))
    (make-mutation-case
      :target-name 'update-gate-status!
      :test-file "problem-jammer-test.lisp"
      :note "Simulates dropping UPDATE-GATE-STATUS!'s jamming override -- eg a refactor
             that treats jamming as purely a display/consequence detail rather than a
             standing force-open.  Uses the jammer lifecycle test rather than gate.lisp's
             own dedicated zero-action test, since that one's only OPEN derivation runs
             inside its init-action's own PROPAGATE-CHANGES! and is unreachable by a
             post-stage swap; here JAM-TARGET's own PROPAGATE-CHANGES! call fires during
             solving.  With jamming disabled, gate-target -- otherwise uncontrolled --
             never opens, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-update 'update-gate-status! '()
            '(doall (?gate gate)
               (do (assign $control-on nil)
                   (if (bind (controls $clauses ?gate $mode))
                     (do (assign $any-clause-on
                           (ww-loop for $clause in $clauses
                                    thereis (ww-loop for $c in $clause
                                                     always (energized $c))))
                         (if (eql $mode 'normal)
                           (assign $control-on $any-clause-on)
                           (if (eql $mode 'inverted)
                             (assign $control-on (not $any-clause-on))))))
                   (if $control-on
                     (open ?gate)
                     (not (open ?gate))))))
          (compile 'update-gate-status!
                   (subst-int-code (symbol-value 'update-gate-status!)))))
    (make-mutation-case
      :target-name 'update-gun-status!
      :test-file "problem-gun-test.lisp"
      :note "Simulates dropping UPDATE-GUN-STATUS!'s not-jammed override -- eg a refactor
             that treats jamming as purely a display/consequence detail rather than a
             lethality override.  With jamming disabled, gun1 -- uncontrolled and
             therefore armed by default -- stays permanently lethal, so WATCHED never
             becomes safe and WALK can never cross it, making the goal unsatisfiable."
      :install-thunk
        (lambda ()
          (install-update 'update-gun-status! '()
            '(doall (?gun gun)
               (do (assign $control-on t)
                   (if (bind (controls $clauses ?gun $mode))
                     (do (assign $any-clause-on
                           (ww-loop for $clause in $clauses
                                    thereis (ww-loop for $c in $clause
                                                     always (energized $c))))
                         (if (eql $mode 'normal)
                           (assign $control-on $any-clause-on)
                           (if (eql $mode 'inverted)
                             (assign $control-on (not $any-clause-on))))))
                   (if $control-on
                     (lethal ?gun)
                     (not (lethal ?gun))))))
          (compile 'update-gun-status!
                   (subst-int-code (symbol-value 'update-gun-status!)))))))


(defun run-mutation-case (case)
  "Stage CASE's test file, install its broken function, solve, and confirm the
   solve now fails -- via no solution, a wrong solved length, or a Lisp error, any of
   which means a real version of this bug would turn TEST-TALOS's ordinary sweep red.
   Staging itself runs unprotected, same as the ordinary sweep, so a staging error
   still halts the run immediately; only the post-stage solve is wrapped, since that
   is where an ill-formed mutation could plausibly signal instead of cleanly failing.
   Returns T if the mutation was detected (the check has teeth), NIL if it was not
   (a surviving mutant)."
  (let* ((problem-name (parse-problem-name (mutation-case-test-file case)))
         (problem-path (format nil "test/~A" (mutation-case-test-file case))))
    (print-test-header problem-name "VALIDATE")
    (format t "Breaking ~A -- ~A~%" (mutation-case-target-name case) (mutation-case-note case))
    (setf *expected-min-length* nil)
    (%stage problem-path)
    (with-broken-function ((mutation-case-target-name case)
                            (funcall (mutation-case-install-thunk case)))
      (handler-case
        (progn
          (ww-solve)
          (cond
            ((not *solution-paths*)
              (format t "~%Mutation detected: no solution found.~%")
              t)
            ((and *expected-min-length*
                  (eq *solution-type* 'min-length)
                  (/= (solution.depth (first *solution-paths*)) *expected-min-length*))
              (format t "~%Mutation detected: solved at the wrong length.~%")
              t)
            (t
              (format t "~%SURVIVING MUTANT: ~A still solves correctly with ~A broken.~%"
                      problem-name (mutation-case-target-name case))
              nil)))
        (error (e)
          (format t "~%Mutation detected: signaled an error instead of solving: ~A~%" e)
          t)))))


(defun test-talos (&key validate)
  "Stage and solve every problem file in the test directory.
   A file that solves without error but reaches no solution, or whose solved
   length does not match its own *EXPECTED-MIN-LENGTH* (when set, under
   min-length search), is recorded as a failure and the run continues; a
   genuine Lisp error still halts the run immediately, as it does for TEST and
   TEST-BT.  A final summary lists every failed problem.
   With :VALIDATE T, also steps through *MUTATION-CASES* after the normal sweep --
   see RUN-MUTATION-CASE and the MUTATION VALIDATION section above."
  (let ((problem-files
          (sort (directory (merge-pathnames "problem-*.lisp"
                                            (get-test-folder-path)))
                #'string-lessp
                :key #'file-namestring))
        failed-problems
        surviving-mutants)
    (cleanup-test-files)
    (unwind-protect
      (progn
        (dolist (problem-file problem-files)
          (let ((problem-name (parse-problem-name (file-namestring problem-file)))
                (problem-path (format nil "test/~A" (file-namestring problem-file))))
            (print-test-header problem-name "TALOS")
            (setf *expected-min-length* nil)
            (%stage problem-path)
            (ww-solve)
            (cond
              ((not *solution-paths*)
                (format t "~%Talos test ~A completed without a solution.~%"
                        problem-name)
                (push problem-name failed-problems))
              ((and *expected-min-length*
                    (eq *solution-type* 'min-length)
                    (/= (solution.depth (first *solution-paths*))
                        *expected-min-length*))
                (format t "~%Talos test ~A solved at length ~D, expected ~D.~%"
                        problem-name
                        (solution.depth (first *solution-paths*))
                        *expected-min-length*)
                (push problem-name failed-problems)))))
        (when validate
          (format t "~%~%Validating check teeth (~D mutation case~:P)...~%"
                  (length *mutation-cases*))
          (dolist (case *mutation-cases*)
            (unless (run-mutation-case case)
              (push (mutation-case-test-file case) surviving-mutants))))
        (format t "~%~%Final Summary:~%")
        (format t "Total Talos test problems run: ~D~%" (length problem-files))
        (format t "Test failures: ~D~%" (length failed-problems))
        (format t "Failed problems: ~A~%" (reverse failed-problems))
        (when validate
          (format t "Mutation cases run: ~D~%" (length *mutation-cases*))
          (format t "Surviving mutants: ~D~%" (length surviving-mutants))
          (format t "Surviving mutant files: ~A~%" (reverse surviving-mutants)))
        (format t "Overall: ~:[FAILED~;PASSED~]~%"
                (and (null failed-problems) (null surviving-mutants)))
        (and (null failed-problems) (null surviving-mutants)))
      (cleanup-test-files))))


(defun write-hash-table-to-file (hash-table filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print hash-table out))))


(defun read-hash-table-from-file (filename)
  (with-open-file (in filename :direction :input)
    (with-standard-io-syntax
      (read in))))


;;; BACKTRACKING TEST ;;;


(defun run-bt-test-problems ()
  (cleanup-test-files)
  (reset-parameters)
  (with-silenced-compilation
    (let* ((problems-to-run *test-bt-problem-files*)
           (test-solutions-file (merge-pathnames "problem-test-bt-solutions.lisp"
                                                 (asdf:system-source-directory :wouldwork)))
           (problem-test-solutions (if (probe-file test-solutions-file)
                                      (read-hash-table-from-file test-solutions-file)
                                      (make-hash-table :test #'equal)))
           (problems-processed 0)
           (continue-all nil)
           failed-problems)
      (loop for problem in problems-to-run
            do (let* ((problem-name (parse-problem-name problem))
                      (should-process t))
                 (print-test-header problem-name "BACKTRACKING")
                 (unless continue-all
                   (format t "Continue, Skip, All, Quit: ")
                   (force-output)
                   (let* ((response (read-line))
                          (choice (if (> (length response) 0)
                                      (char-upcase (char response 0))
                                      #\C)))
                     (case choice
                       (#\Q (return-from run-bt-test-problems nil))
                       (#\S (setf should-process nil))
                       (#\A (setf continue-all t))
                       (#\C nil)
                       (t nil))))
                 (when should-process
                   (reset-parameters)
                   (uiop:delete-file-if-exists (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork)))
                   ;; Load silently to acquire native settings from define-problem,
                   ;; suppressing the initial depth-first parameter display.
                   ;; Justified here as this is a test routine only.
                   (let ((*standard-output* (make-broadcast-stream)))
                     (%stage problem-name))
                   ;; Override for backtracking; native depth-cutoff already preserved.
                   (setf *algorithm*     'backtracking
                         *tree-or-graph* 'tree)
                   ;; Problems with no native depth-cutoff need one for bt tree search.
                   (when (string-equal problem-name "hanoi")
                     (setf *depth-cutoff* 9))
                   (when (string-equal problem-name "donald")
                     (setf *depth-cutoff* 6))
                   ;; refresh saves bt overrides to vals.lisp and does one visible
                   ;; load, producing a single parameter display showing backtracking.
                   (refresh)
                   (incf problems-processed)
                   (ww-solve)
                   (let ((solution-data (collect-solution-data)))
                     (unless (equalp solution-data
                                     (gethash problem-name problem-test-solutions))
                       (format t "~%The problem solution above does not match the expected solution:")
                       (format t "~%~A~2%" (gethash problem-name problem-test-solutions))
                       (push problem-name failed-problems))
                     (unless (probe-file test-solutions-file)
                       (setf (gethash problem-name problem-test-solutions)
                             solution-data))
                     t))))
      (cleanup-test-files)
      (stage blocks3)
      (format t "~%~%Final Summary:~%")
      (format t "Total test problems run: ~D~%" (length *test-bt-problem-files*))
      (format t "Test failures: ~D~%" (length failed-problems))
      (format t "Failed problems: ~A~%" (reverse failed-problems))
      (format t "Note: A failed BT solution is not necessarily wrong, but different from the reference solution.~%")
      (progn (unless (probe-file test-solutions-file)
               (write-hash-table-to-file problem-test-solutions test-solutions-file))
             t)
      t)))


(defun test-bt ()
  "Run standard test suite using backtracking search."
  (run-bt-test-problems))


;;; START-STATE GOAL TEST ;;;


(defun check-start-is-goal-test (condition control &rest args)
  "Signal a focused start-is-goal test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control args)))
  t)


(defun run-start-is-goal-case (algorithm tree-or-graph threads solution-type
                               expected-count &key terminal expected-value)
  "Run one start-is-goal configuration and validate its solution records."
  (setf *algorithm* algorithm
        *tree-or-graph* tree-or-graph
        *threads* threads
        *solution-type* solution-type
        *depth-cutoff* 1
        *randomize-search* nil
        *branch* -1
        *symmetry-pruning* nil
        *debug* 0
        *probe* nil
        *auto-wait* nil)
  (ww-solve)
  (let ((root-solution (find 0 *solution-paths* :key #'solution.depth)))
    (check-start-is-goal-test
      root-solution
      "No depth-zero solution for ~A/~A with ~D thread~:P and solution type ~A."
      algorithm tree-or-graph threads solution-type)
    (check-start-is-goal-test
      (null (solution.path root-solution))
      "The depth-zero solution path is not empty: ~S"
      (solution.path root-solution))
    (check-start-is-goal-test
      (equalp (problem-state.idb (solution.goal root-solution))
              (problem-state.idb *start-state*))
      "The depth-zero goal state differs from the initialized start state.")
    (check-start-is-goal-test
      (= (solution.time root-solution) (problem-state.time *start-state*))
      "The depth-zero solution time differs from the start time.")
    (check-start-is-goal-test
      (= (solution.value root-solution) (problem-state.value *start-state*))
      "The depth-zero solution value differs from the start value.")
    (check-start-is-goal-test
      (= (length *solution-paths*) expected-count)
      "Expected ~D solution~:P for ~A/~A/~A, but found ~D."
      expected-count algorithm tree-or-graph solution-type
      (length *solution-paths*))
    (check-start-is-goal-test
      (= (length *unique-solution-states*) expected-count)
      "Expected ~D unique goal state~:P for ~A/~A/~A, but found ~D."
      expected-count algorithm tree-or-graph solution-type
      (length *unique-solution-states*))
    (when terminal
      (check-start-is-goal-test
        (and (= *program-cycles* 0)
             (= *total-states-processed* 1)
             (= *max-depth-explored* 0))
        "A terminal depth-zero solution expanded the search: cycles=~D, states=~D, depth=~D."
        *program-cycles* *total-states-processed* *max-depth-explored*))
    (when expected-value
      (check-start-is-goal-test
        (find expected-value *solution-paths* :key #'solution.value :test #'=)
        "No solution has the expected objective value ~A."
        expected-value)))
  (format t "~&Passed start-is-goal case: ~A / ~A / ~D thread~:P / ~A~%"
          algorithm tree-or-graph threads solution-type)
  t)


(defun test-start-is-goal ()
  "Verify depth-zero goal handling across solution types and search engines."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-start-is-goal-test.lisp")
        (run-start-is-goal-case 'depth-first 'graph 0 'first 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-length 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-time 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 1 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 0 2 2)
        (run-start-is-goal-case 'depth-first 'graph 0 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 0 'all-paths 3)
        (run-start-is-goal-case 'depth-first 'graph 0 'min-value 2
                                :expected-value -10)
        (run-start-is-goal-case 'depth-first 'graph 0 'max-value 2
                                :expected-value 10)
        (run-start-is-goal-case 'backtracking 'tree 0 'first 1 :terminal t)
        (run-start-is-goal-case 'backtracking 'tree 0 2 2)
        (run-start-is-goal-case 'backtracking 'tree 0 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 2 'first 1 :terminal t)
        (run-start-is-goal-case 'depth-first 'graph 2 'every 3)
        (run-start-is-goal-case 'depth-first 'graph 2 'min-value 3
                                :expected-value -10)
        (run-start-is-goal-case 'depth-first 'graph 2 'max-value 3
                                :expected-value 10)
        (format t "~2&All start-is-goal cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; CANDIDATE SOLUTION VALIDATOR TEST ;;;


(defun check-solution-validator-test (condition control &rest args)
  "Signal a focused candidate-validator test failure unless CONDITION is true."
  (unless condition
    (error "~A" (apply #'format nil control args)))
  t)


(defun solution-validator-accepted-goal-p (state)
  "Return true when STATE contains the repaired test goal."
  (member '(validator-at validator-accepted)
          (list-database (problem-state.idb state))
          :test #'equal))


(defun run-solution-validator-case
    (algorithm tree-or-graph threads solution-type)
  "Run one search configuration and require the rejected depth-one goal to be repaired."
  (setf *algorithm* algorithm
        *tree-or-graph* tree-or-graph
        *threads* threads
        *solution-type* solution-type
        *depth-cutoff* 2
        *randomize-search* nil
        *branch* -1
        *symmetry-pruning* nil
        *debug* 0
        *probe* nil
        *auto-wait* nil)
  (ww-solve)
  (check-solution-validator-test
    (= *nominal-solution-candidates*
       (+ *accepted-solution-candidates* *rejected-solution-candidates*))
    "Candidate-validation totals do not balance: ~D checked, ~D accepted, ~D rejected."
    *nominal-solution-candidates*
    *accepted-solution-candidates*
    *rejected-solution-candidates*)
  (check-solution-validator-test
    (plusp *accepted-solution-candidates*)
    "The accepted candidate was not counted.")
  (check-solution-validator-test
    (plusp
      (gethash
        '(accept-only-repaired-validator :unspecified :repair-required)
        *solution-validator-rejections*
        0))
    "The rejected candidate diagnostic was not grouped by validator and reason.")
  (let ((report
          (with-output-to-string (stream)
            (print-candidate-solution-validation-statistics stream))))
    (check-solution-validator-test
      (and (search "Candidate solution validation:" report)
           (search "Nominal goal paths checked" report)
           (search "REPAIR-REQUIRED" report))
      "The candidate-validation report is incomplete:~%~A"
      report))
  (check-solution-validator-test
    *solution-paths*
    "No validated solution for ~A/~A with ~D thread~:P and solution type ~A."
    algorithm tree-or-graph threads solution-type)
  (dolist (solution *solution-paths*)
    (check-solution-validator-test
      (= (solution.depth solution) 2)
      "A rejected candidate was recorded at depth ~D: ~S"
      (solution.depth solution) (solution.path solution))
    (check-solution-validator-test
      (solution-validator-accepted-goal-p (solution.goal solution))
      "A recorded solution does not contain the repaired goal: ~S"
      (list-database (problem-state.idb (solution.goal solution)))))
  (format t "~&Passed solution-validator case: ~A / ~A / ~D thread~:P / ~A~%"
          algorithm tree-or-graph threads solution-type)
  t)


(defun test-solution-validator ()
  "Verify candidate validation, rejected-goal expansion, and reusable action replay."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-engine-solution-validator-test.lisp")
        (reset-candidate-solution-validation-statistics)
        (let ((empty-report
                (with-output-to-string (stream)
                  (print-candidate-solution-validation-statistics stream))))
          (check-solution-validator-test
            (search "No path reached the problem goal." empty-report)
            "The zero-candidate validation report is incomplete:~%~A"
            empty-report))
        (let ((result
                (validate-action-sequence
                  *start-state*
                  '((advance-to-rejected-goal)
                    (repair-rejected-goal))
                  :goal-test #'solution-validator-accepted-goal-p)))
          (check-solution-validator-test
            (action-sequence-validation-success-p result)
            "The reusable action-sequence validator rejected the valid repair path: ~S"
            (action-sequence-validation-failure-reason result))
          (check-solution-validator-test
            (action-sequence-validation-goal-satisfied-p result)
            "The reusable action-sequence validator did not recognize the repaired goal."))
        (run-solution-validator-case 'depth-first 'graph 0 'min-length)
        (run-solution-validator-case 'backtracking 'tree 0 'first)
        (run-solution-validator-case 'depth-first 'graph 2 'min-length)
        (run-solution-validator-case 'depth-first 'graph 0 'all-paths)
        (format t "~2&All candidate solution-validator cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; RECORDER SNAPSHOT-RESET VALIDATION TEST ;;;


(defun test-recorder-playback-validation ()
  "Verify exact recording validation and snapshot-reset playback semantics."
  (cleanup-test-files)
  (unwind-protect
      (progn
        (%stage "test/problem-recorder-playback-validation-test.lisp")
        (let* ((valid-path
                 '((1.0 (finish-while-plate-clear live-agent))
                   (2.0 (step-on ghost-agent recorder-site plate1))
                   (3.0 (step-off ghost-agent recorder-site plate1))))
               (invalid-playback-path
                 '((1.0 (step-on ghost-agent recorder-site plate1))
                   (2.0 (finish-while-plate-clear live-agent))
                   (3.0 (step-off ghost-agent recorder-site plate1))))
               (away-stop-path
                 '((1.0 (finish-while-plate-clear live-agent))
                   (2.0 (walk ghost-agent recorder-site away-site))))
               (stranded-stop-path
                 '((1.0 (finish-while-plate-clear live-agent))
                   (2.0 (walk ghost-agent recorder-site stranded-site))))
               (recording-validation
                 (validate-action-sequence
                   *start-state*
                   '((step-on ghost-agent recorder-site plate1)
                     (step-off ghost-agent recorder-site plate1)))))
          (check-solution-validator-test
            (action-sequence-validation-success-p recording-validation)
            "The ghost-only recording sequence failed: ~S"
            (action-sequence-validation-failure-reason recording-validation))
          (check-solution-validator-test
            (member '(recording-latched plate1)
                    (list-database
                      (problem-state.idb
                        (action-sequence-validation-final-state
                          recording-validation)))
                    :test #'equal)
            "The recording-final latch did not differ from the initial snapshot.")
          (multiple-value-bind (valid-p diagnostic)
              (validate-recorder-solution *start-state* valid-path *start-state*)
            (check-solution-validator-test
              valid-p
              "Snapshot-reset playback was rejected: ~S"
              diagnostic))
          (multiple-value-bind (valid-p diagnostic)
              (validate-recorder-solution
                *start-state* invalid-playback-path *start-state*)
            (check-solution-validator-test
              (and (not valid-p)
                   (eql (getf diagnostic :phase) :playback)
                   (eql (getf diagnostic :reason) :action-failed))
              "An invalid exact playback was not diagnosed correctly: ~S"
              diagnostic))
          (multiple-value-bind (valid-p diagnostic)
              (validate-recorder-solution
                *start-state* away-stop-path *start-state*)
            (check-solution-validator-test
              valid-p
              "A recording ending within walking range of its recorder was rejected: ~S"
              diagnostic))
          (multiple-value-bind (valid-p diagnostic)
              (validate-recorder-solution
                *start-state* stranded-stop-path *start-state*)
            (check-solution-validator-test
              (and (not valid-p)
                   (eql (getf diagnostic :phase) :recording)
                   (eql (getf diagnostic :reason) :agents-cannot-close))
              "A stranded ghost agent was not diagnosed correctly: ~S"
              diagnostic)))
        (ww-solve)
        (check-solution-validator-test
          (and *solution-paths*
               (= (solution.depth (first *solution-paths*)) 1))
          "The focused recorder search did not find its validated one-step solution.")
        (format t "~2&All recorder snapshot-reset validation cases passed.~%")
        t)
    (cleanup-test-files)
    (stage blocks3)))


;;; SIMPLE DEPTH-FIRST VS BACKTRACKING BENCHMARK ;;;


(defun bench-depth&back (problem-name depth-cutoff &key (solution-type 'first))
  "Runs depth-first and backtracking once each on PROBLEM-NAME.
   Enforces tree mode and identical depth cutoff/settings for both runs.
   Returns plist of the two result rows."
  (let* ((problem-str (string-downcase (string problem-name)))
         (results nil))
    (format t "~%========================================~%")
    (format t "BENCH DEPTH-FIRST VS BACKTRACKING~%")
    (format t "problem=~A depth-cutoff=~D solution-type=~A~%"
            (string-upcase problem-str) depth-cutoff solution-type)
    (format t "========================================~%")
    (dolist (algorithm '(depth-first backtracking))
      (%stage problem-str)
      (setf *algorithm* algorithm
            *tree-or-graph* 'tree
            *depth-cutoff* depth-cutoff
            *solution-type* solution-type
            *threads* 0
            *randomize-search* nil
            *debug* 0
            *probe* nil)
      ;; Keep native CSP problems as CSP; otherwise default to planning.
      (unless (member *problem-type* '(planning csp))
        (setf *problem-type* 'planning))
      (refresh)
      (let ((start (get-internal-real-time)))
        (ww-solve)
        (let ((row (list :algorithm algorithm
                         :problem-type *problem-type*
                         :elapsed (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)
                         :cycles *program-cycles*
                         :states *total-states-processed*
                         :max-depth *max-depth-explored*
                         :solutions (length *solution-paths*)
                         :unique-solutions (length *unique-solution-states*))))
          (push row results)
          (format t "~%~A: elapsed=~,6F s cycles=~D states=~D max-depth=~D solutions=~D unique=~D type=~A~%"
                  algorithm
                  (getf row :elapsed)
                  (getf row :cycles)
                  (getf row :states)
                  (getf row :max-depth)
                  (getf row :solutions)
                  (getf row :unique-solutions)
                  (getf row :problem-type)))))
    (setf results (nreverse results))
    (let* ((dfs (find 'depth-first results :key (lambda (r) (getf r :algorithm))))
           (bt (find 'backtracking results :key (lambda (r) (getf r :algorithm))))
           (dfs-elapsed (getf dfs :elapsed))
           (bt-elapsed (getf bt :elapsed))
           (speedup (if (> bt-elapsed 0.0) (/ dfs-elapsed bt-elapsed) 0.0)))
      (format t "~%----------------------------------------~%")
      (format t "Summary: depth-first=~,6F s backtracking=~,6F s speedup(df/bt)=~,3Fx~%"
              dfs-elapsed bt-elapsed speedup)
      (format t "----------------------------------------~%"))
    results))
