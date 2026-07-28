;;; Filename: -threat.lisp

;;; Threat substrate: the shared interface for anything that makes a location lethal to
;;; occupy.  A threat instance (gun today; a future hazard technology joins the same
;;; union) authors the fixed set of locations it endangers via THREATENS; its own
;;; technology is the sole asserter of LETHAL when that threat is currently live -- gun's
;;; update-gun-status! for gun, mirroring how -gate.lisp's OPEN is asserted only by
;;; gate.lisp.  SAFE is the query any relocating action can consult at its own precondition
;;; to exclude an unsafe destination outright -- move, jump-to, and use-ladder all do.  But
;;; a location can also become unsafe with no relocation at all (a threat arming remotely
;;; while an agent already stands where it reaches), and a blower's forced physics has no
;;; precondition to gate in the first place, so ENFORCE-THREAT-SAFETY! is the general
;;; backstop: it runs every propagation pass and marks the whole state inconsistent -- see
;;; ww-planner.lisp's generate-children, which drops any successor carrying that marker --
;;; if any agent's current location isn't safe, however it got there.  This file is
;;; nested unconditionally by gun.lisp, so the backstop is present whenever a threat
;;; exists, regardless of which relocation technologies (accessibility, jump, ladder, the
;;; blowers) the problem also includes -- deliberately breaking the usual role convention
;;; of owning no driver logic, since only a file present under every combination
;;; guarantees the invariant actually holds.  A problem with no threats pays nothing: the
;;; THREAT type is empty, SAFE reduces to T, and the backstop's doall is vacuous.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   functions : inconsistent-state  --  supplied by the planner (ww-planner.lisp), not
;;;               by another technology
;;; PROVIDES:
;;;   types     : threat (either gun)  --  extensible; a future hazard technology adds its
;;;               own leaf type to this either form.  gun is declared optional here.
;;;   relations : (threatens threat $list)  --  static; the locations a threat instance
;;;               endangers while lethal
;;;               (lethal threat)  --  dynamic; asserted only by each threat technology's
;;;               own status update
;;;   query     : safe
;;;   update    : enforce-threat-safety!  --  the general inconsistent-state backstop

(include-tech -propagation)

(in-package :ww)


(define-optional-types gun)


(define-types
  threat (either gun))


(define-static-relations
  (threatens threat $list))  ;the locations a threat instance endangers while lethal


(define-dynamic-relations
  (lethal threat))  ;derived; asserted only by each threat technology's own status update


(define-query safe (?location location)
  ;; True unless some currently-lethal threat's authored THREATENS list includes ?location.
  (not (exists (?t threat)
         (and (lethal ?t)
              (bind (threatens ?t $locations))
              (member ?location $locations)))))


(define-update enforce-threat-safety! ()
  ;; The general backstop, run every propagation pass: if any agent's current location
  ;; isn't safe -- however it got there, whether by a precondition-gated move/jump-to/
  ;; use-ladder that somehow still landed unsafely, a blower's unconditional physics, or a
  ;; threat arming remotely while the agent already stood in its reach -- the whole state
  ;; is marked inconsistent and dropped from the search.  Even transient exposure during a
  ;; single fixpoint is fatal: nothing here retracts the marker once set.
  (doall (?agent agent)
    (if (and (bind (has-location ?agent $location))
             (not (safe $location)))
      (inconsistent-state))))
