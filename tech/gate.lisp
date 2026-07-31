;;; Filename: gate.lisp

;;; Gate technology: a gate's open-state is derived each propagation pass as
;;;   open  <=>  jammed  OR  control-on
;;; Controllers are stored in disjunctive normal form (an OR-list of AND-lists); a clause is
;;; on when all its members are energized (a receiver when active, a plate when depressed).
;;; Inverted mode negates the control aggregate; jamming always overrides toward open.  An
;;; uncontrolled gate reduces to open <=> jammed.  (Only normal and inverted are recognized,
;;; matching claustro4a; toggle is reserved.)
;;;
;;; REQUIRES (supplied by other techs):
;;;   types     : (none bare)  --  jammer is declared optional here (define-optional-types);
;;;               plate, mode, and receiver come from nested -controls; gate itself comes
;;;               from nested -gate
;;;   nested    : -controls ((controls ...), energized; nests -beam-substrate for
;;;               (active receiver))  --  shared with the blower techs' gears (gears-fan);
;;;               -gate (gate optional type, (open gate) relation) -- shared with
;;;               walkability (via -passability), reachability, visibility, beam-direct,
;;;               beam-crossing, and -passability, which all nest -gate instead of
;;;               hand-declaring it
;;;   conditional relations:
;;;               depressed (plate), guarded by plate
;;;               jamming (jammer), guarded by an exists over jammer
;;;               The owning tech is required only when the guarding optional type is nonempty;
;;;               translation removes the guarded reference when that type is empty.
;;;   driver    : the master propagate-consequences! must call update-gate-status!
;;; PROVIDES:
;;;   types     : jammer  --  declared optional here; a problem with no jammers need not
;;;               declare it.  Other techs (plate, jammer, box, beam-relay, -beam-substrate,
;;;               visibility, etc.) still declare their own jammer-alias for their own
;;;               pre-params; the bare and aliased forms resolve compatibly and do not conflict.
;;;   update    : update-gate-status!  --  the only file that ever asserts (open gate)

(include-tech -propagation)
(include-tech -controls)
(include-tech -gate)

(in-package :ww)


(define-optional-types jammer)


(define-update update-gate-status! ()
  ;; open <=> jammed OR control-on.  control-on (normal) iff some DNF clause has every member
  ;; energized; inverted negates that aggregate.  Jamming is the leading disjunct, so it
  ;; overrides an inverted force-close.  Uncontrolled gates reduce to open <=> jammed, which
  ;; is how jam-driven opening is realized.  Change detection is automatic, so an unchanged
  ;; re-assert is silent.
  (doall (?gate gate)
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
        (if (or (exists (?j jammer)
                  (jamming ?j ?gate))
                $control-on)
          (open ?gate)
          (not (open ?gate))))))
