;;; Filename: recorder.lisp

;;; Public recorder assembly.  Problems continue to write only (include-tech recorder).
;;; Identity and interaction isolation live in -recorder-core; each supported apparatus
;;; owns a private recording-shadow component.  Including this public assembly also installs
;;; recorder-specific prefix pruning, interleaving audit/pruning, candidate validation,
;;; solution reporting, and goal chaining after all implementations have been defined.
;;;
;;; Recorder start/stop/cancel transitions and their cycle count are planner-native and
;;; repeatable.
;;; The path parser accepts repeated setup/start/window/ending cycles and an optional
;;; final open window.  Every generated STOP or CANCEL successor validates its just-completed
;;; cycle and rejects a closed cycle without persistent progress before goal or duplicate
;;; processing.  At equal normalized boundaries, an equal-or-cheaper path with fewer cycles
;;; used dominates one with more cycles.  Final candidate validation checks all cycles plus
;;; the complete integrated path.  One ordinary SOLVE therefore optimizes across every
;;; permitted cycle.  The report reconstructs every cycle and its local metrics from the
;;; accepted path while retaining complete-solution totals.
;;;
;;; Recorder cycle chaining remains an explicitly guided convenience.  Each SOLVE-SUBGOAL
;;; is capped at and required to consume exactly one additional cycle, and the following
;;; SOLVE does the same for the final cycle.  The configured maximum limits the complete
;;; guided history.  Its retained history is locally optimized per search and makes no
;;; global claim.
;;;
;;; The component order preserves the established propagation seed:
;;; ordinary receiver state, recording plate state, recording receiver state, recording
;;; gate state, then recording wall-gears state.  The derived propagation driver still
;;; orders the final calls from their actual read/write graph.
;;;
;;; REQUIRES / PROVIDES VIA COMPONENTS:
;;;   -recorder-core               : RECORDING-COPY>, RECORDING-IN-PROGRESS, side identity,
;;;                                  object presence, and cross-layer interaction policy
;;;   -recorder-plate-shadow       : RECORDING-DEPRESSED / RECORDING-LATCHED
;;;   -recorder-receiver-shadow    : RECORDING-ACTIVE
;;;   -recorder-controls-shadow    : recording-side DNF controller evaluation
;;;   -recorder-jamming-shadow     : ghost-filtered RECORDING-JAMMED
;;;   -recorder-gate-shadow        : RECORDING-OPEN and gate-view hook
;;;   -recorder-wall-gears-shadow  : RECORDING-TURNING and gears-view hook
;;;   -recorder-solution           : multi-window parsing, mandatory ending validation,
;;;                                  optional open-prefix validation, interleaving
;;;                                  audit/pruning, candidate validation, and report
;;;   -recorder-session            : START-RECORDER / STOP-RECORDER / CANCEL-PLAYBACK
;;;                                  actions and the
;;;                                  live-to-ghost state fork; nests -recorder-solution,
;;;                                  so its own list position is a readability choice, not
;;;                                  a load-order requirement
;;;   -recorder-cycle-boundary     : closed-cycle goal and fresh-shadow preparation
;;;   -recorder-cycle-chaining     : one-cycle solve, commit, final solve, and undo history
;;;   -recorder-init-checks        : mapping, isolation, and supported-scope validation
;;;
;;; The supported behavior remains unchanged: plates, direct/relay-fed receivers, gates,
;;; wall gears, and gate/wall-gears jamming have recording views.  Initialization rejects
;;; beam crossings, floor and angled blowers, threats, receiver-controlled wall gears,
;;; movable wall-fan copies, and any other explicitly unsupported combination rather than
;;; approximating it at runtime.

(include-tech -recorder-core)
(include-tech -recorder-controls-shadow)
(include-tech -recorder-jamming-shadow)
(include-tech -recorder-gate-shadow)
(include-tech -recorder-wall-gears-shadow)
(include-tech -recorder-solution)
(include-tech -recorder-session)
(include-tech -recorder-cycle-boundary)
(include-tech -recorder-cycle-chaining)
(include-tech -recorder-init-checks)

(in-package :ww)


;; These registries are reset before every stage.  Keeping installation at the end of the
;; public assembly scopes the complete recorder policy to problems that include RECORDER and
;; guarantees that every registered implementation has already been defined.  Exact
;; live/ghost interleaving pruning is automatic on the supported serial search path.
(register-solution-validator 'validate-recorder-solution)
(register-search-prefix-validator
  'validate-recorder-cycle-boundary-prefix
  'recorder-cycle-boundary-validation-enabled-p
  'recorder-ending-prefix-trigger-p)
(register-search-prefix-validator
  'validate-recorder-recording-prefix
  'recorder-prefix-pruning-enabled-p)
(register-search-successor-pruner
  'prune-recorder-interleaving-successor-p
  'recorder-interleaving-pruning-enabled-p)
(register-search-successor-pruner
  'prune-recorder-boundary-dominated-successor-p
  'recorder-boundary-dominance-enabled-p
  'reset-recorder-boundary-dominance)
(register-solution-report-printer 'print-recorder-report)
(register-goal-chaining-policy
  'solve-recorder-subgoal-form
  'solve-recorder-final)
