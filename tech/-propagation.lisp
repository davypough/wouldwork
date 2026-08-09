;;; Filename: -propagation.lisp

;;; Propagation substrate: the MASTER PROPAGATION DRIVER itself, contributed by the
;;; technologies rather than authored by the problem.  Every technology that supplies a
;;; zero-argument update nests this file, so any problem including any such technology
;;; receives both driver functions without writing either one.
;;;
;;; PROPAGATE-CHANGES! is the fixpoint loop, and it is fixed.  It was byte-identical in
;;; CLAUSTRO-TOPO, CORNER-TOPO, PHOBIA, PROBLEM-FLOOR-GEARS-TEST,
;;; PROBLEM-WALL-BLOWER-TEST, PROBLEM-PROPAGATION-STRATA-TEST and
;;; PROBLEM-REACTION-ORDER-TEST -- seven independent transcriptions of one loop, which is
;;; exactly the thing a technology should own.
;;;
;;; PROPAGATE-CONSEQUENCES! is the ordered call sequence, and it is derived per problem:
;;; the loaded technologies decide which updates exist, and the read/write graph over those
;;; updates decides their order.  Neither is known when this file is spliced, because a
;;; technology may still override a peer's query afterward and change the graph.  So what
;;; is spliced here is a SENTINEL, and INIT overwrites it with the derived body once every
;;; definition is in.  See WW-PROPAGATION-ORDER's DERIVED DRIVER section.
;;;
;;; The sentinel signals rather than returning NIL.  A no-op PROPAGATE-CONSEQUENCES!
;;; surviving to DO-INIT-ACTION-UPDATES would yield an unpropagated start state and a
;;; failure bearing no relation to its cause; an error names the defect where it is.
;;; PROPAGATION-DRIVER-NOT-DERIVED is the marker both halves agree on --
;;; AUTHORED-PROPAGATION-DRIVER-BODY tests :RAW-BODY against it to tell a problem's own
;;; driver from this placeholder, so the body below must stay in step with
;;; *PROPAGATION-DRIVER-SENTINEL*.
;;;
;;; A problem may still author its own driver.  Its definitions are spliced below its
;;; (include-tech ...) block and so are installed after these, and the later definition
;;; wins -- the same rule the peer-substrate technologies already rely on.  An authored
;;; PROPAGATE-CONSEQUENCES! displaces the sentinel, and INIT then leaves it alone.
;;;
;;; REQUIRES (supplied by the planner, not by another technology):
;;;   specials  : *detect-propagated-changes*, *propagated-state-changed* (ww-settings)
;;;   functions : inconsistent-state, propagation-driver-not-derived
;;; PROVIDES:
;;;   update    : propagate-changes!  --  the fixpoint loop, final
;;;   update    : propagate-consequences!  --  sentinel only; INIT installs the real body

(in-package :ww)


(define-update propagate-changes! ()
  ;; Binds the change-detection gate so add-prop/del-prop flag *propagated-state-changed*
  ;; on real derived-fact mutations during the fixpoint.  Each pass runs to convergence (no
  ;; change) or, failing that, the cap declares the state inconsistent.
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 10
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil))))


(define-update propagate-consequences! ()
  ;; Sentinel.  INIT replaces this body with the derived call sequence, or a problem's own
  ;; driver overrides it at load.  Reaching it means neither happened.
  (propagation-driver-not-derived))
