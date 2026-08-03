;;; Filename: recorder.lisp

;;; Public recorder assembly.  Problems continue to write only (include-tech recorder).
;;; Identity and interaction isolation live in -recorder-core; each supported apparatus
;;; owns a private recording-shadow component; solution-time validation/reporting and the
;;; whole-problem support envelope remain separate services.
;;;
;;; Recorder cycle chaining is explicit rather than planner-native.  One call to
;;; SOLVE-RECORDER-SUBGOAL or SOLVE-RECORDER-FINAL searches at most one complete recording
;;; cycle.  Every searched cycle physically returns its ghosts to a recorder before its
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
;;;   -recorder-core               : RECORDING-COPY>, side identity, object presence,
;;;                                  and cross-layer interaction policy
;;;   -recorder-plate-shadow       : RECORDING-DEPRESSED / RECORDING-LATCHED
;;;   -recorder-receiver-shadow    : RECORDING-ACTIVE
;;;   -recorder-controls-shadow    : recording-side DNF controller evaluation
;;;   -recorder-jamming-shadow     : ghost-filtered RECORDING-JAMMED
;;;   -recorder-gate-shadow        : RECORDING-OPEN and gate-view hook
;;;   -recorder-wall-gears-shadow  : RECORDING-TURNING and gears-view hook
;;;   -recorder-solution           : candidate validation and two-phase report
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
(include-tech -recorder-cycle-boundary)
(include-tech -recorder-cycle-chaining)
(include-tech -recorder-init-checks)

(in-package :ww)
