;;; Filename: recorder.lisp

;;; Public recorder assembly.  Problems continue to write only (include-tech recorder).
;;; Identity and interaction isolation live in -recorder-core; each supported apparatus
;;; owns a private recording-shadow component.  Including this public assembly also installs
;;; recorder-specific prefix pruning, interleaving audit/pruning, candidate validation,
;;; solution reporting, and goal chaining after all implementations have been defined.
;;;
;;; Recorder cycle chaining is explicit rather than planner-native.  Each SOLVE-SUBGOAL
;;; searches and commits at most one intermediate recording cycle, and the following SOLVE
;;; searches and commits the final cycle.  An initial SOLVE remains an ordinary whole-problem
;;; search.  Every searched cycle physically returns its ghosts to a recorder before its
;;; integrated playback state can become a boundary.  An intermediate commit preserves that
;;; playback state, discards the completed program, and prepares a fresh capability-owned
;;; recording shadow for the next call.  Cycles are optimized only within their own searches;
;;; the retained history and cumulative metrics do not imply global optimality or completeness.
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
;;;   -recorder-solution           : prefix validation, interleaving audit/pruning,
;;;                                  candidate validation, and two-phase report
;;;   -recorder-session            : START-RECORDER / STOP-RECORDER actions and the
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
  'validate-recorder-recording-prefix
  'recorder-prefix-pruning-enabled-p)
(register-search-successor-pruner
  'prune-recorder-interleaving-successor-p
  'recorder-interleaving-pruning-enabled-p)
(register-solution-report-printer 'print-recorder-report)
(register-goal-chaining-policy
  'solve-recorder-subgoal-form
  'solve-recorder-final)
