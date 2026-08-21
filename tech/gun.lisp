;;; Filename: gun.lisp

;;; Gun technology: a stationary automated turret that makes its authored THREATENS
;;; locations lethal while armed.  Armed state is derived each propagation pass exactly
;;; like -gears-fan's turning: lethal <=> (uncontrolled OR control-on) AND NOT jammed --
;;; an uncontrolled gun is armed by default, matching the turret fiction (compare gate,
;;; which defaults closed); jamming always overrides toward safe, the same polarity as
;;; gears' jam-forces-stopped.  A gun may be wired to receivers/plates via -controls
;;; exactly like a gate or gears.  It has zero physical height and is positioned as a
;;; functional point fixture exactly like a transmitter or receiver
;;; (-beam-los-coordinates' APPARATUS-COORDS>) rather than with HAS-POSITION: nothing can ever
;;; occupy a gun's mounting point, since it isn't a location at all.  Jammed via
;;; jammer.lisp's target union; jam-target's LOS check reads it through visible/
;;; los-via, the same as a gate, not through has-position like gears -- this
;;; file doesn't need to nest -visibility/-beam-los-coordinates itself, since it never
;;; calls visible; that machinery arrives transitively through jammer.lisp (and, for real
;;; geometric derivation rather than hand-authored LOS facts, the public visibility tech).
;;;
;;; REQUIRES (supplied by other techs):
;;;   types     : (none bare)  --  gun itself comes from nested -threat; jammer is
;;;               declared optional here (define-optional-types), matching gate.lisp's
;;;               own pattern
;;;   nested    : -threat (gun optional type, threat union, (threatens ...), (lethal ...),
;;;               safe); -controls ((controls ...), energized; nests -beam-substrate for
;;;               (active receiver)) -- shared with gate and the blower techs' gears
;;;   soft      : APPARATUS-COORDS> (-beam-los-coordinates) and gun as a visibility target
;;;               leaf (-visibility) -- required only if a problem wants to jam this gun;
;;;               reached through jammer.lisp's own -visibility nest, and through public
;;;               visibility.lisp if LOS is to be derived from geometry rather than
;;;               hand-authored
;;;   conditional relations:
;;;               jamming (jammer), guarded by an exists over jammer -- jammer.lisp is
;;;               required only when the problem declares any jammers
;;;   driver    : the master propagate-consequences! must call update-gun-status!
;;; PROVIDES:
;;;   updates   : update-gun-status!  --  the only file that ever asserts (lethal gun)

(include-tech -propagation)
(include-tech -threat)
(include-tech -controls)

(in-package :ww)


(define-optional-types jammer)


(define-derived-relations
  lethal)


(define-update update-gun-status! ()
  ;; lethal <=> control-on AND NOT jammed, with -controls' shared CONTROL-ON supplying the
  ;; DNF aggregate.  The T uncontrolled default arms a bare turret, the same default gears
  ;; use, since a turret nothing controls is a threat until something disables it.  Change
  ;; detection is automatic, so an unchanged re-assert is silent.
  (doall (?gun gun)
    (if (and (control-on ?gun t)
             (not (exists (?j jammer)
                    (jamming ?j ?gun))))
      (lethal ?gun)
      (not (lethal ?gun)))))
