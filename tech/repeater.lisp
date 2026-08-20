;;; Filename: repeater.lisp

(in-package :ww)


;;; Fixed repeater technology: stationary relay apparatus that pass beam color through
;;; permanent directional couplings and pair with movable connectors.  A repeater behaves
;;; like a connector in the beam network -- it acquires exactly one incoming hue, remains
;;; unlit when conflicting hues reach it, and can relay its hue onward -- but it has no
;;; HAS-LOCATION, cannot be carried or placed, and imposes no pairing or coupling limit.
;;;
;;; Every repeater is classified by mounting orientation, and -vertical's BASE and TOP
;;; read that orientation out of *VERTICAL-TYPE-CONSTANTS*.  "Anchor" is retired: a
;;; repeater's beam tip is simply its TOP.
;;;
;;;   floor-repeater
;;;     Stands on the floor, so its base defaults to 0 and HAS-ELEVATION carries the
;;;     level of the floor under it.  Its axis is vertical, so its tip is that base plus
;;;     OBJECT-HEIGHT, which defaults to 1.  It is excluded from APPARATUS-COORDS>'s
;;;     third coordinate on purpose -- that coordinate defaults to the wall-mounting
;;;     level of 1, which would lift every floor repeater off the floor.
;;;
;;;   wall-repeater
;;;     Hangs on a wall at about chest height, so its base defaults to 1, authored as
;;;     APPARATUS-COORDS>'s third coordinate or as HAS-ELEVATION -- -apparatus-coordinates
;;;     cross-checks the two.  Its axis is horizontal, so its tip is its base and TOP
;;;     adds nothing.
;;;
;;; A wall-repeater's height is therefore DESCRIPTIVE ONLY: it records how far the rod
;;; projects from its wall, and nothing reads it.  TOP ignores it on a horizontal axis,
;;; and every other OBJECT-HEIGHT consumer -- beam occlusion, jammer aiming, vaulting,
;;; barrier crossing, stream sweeping -- takes an argument type that excludes repeaters.
;;; Phase 2 weighed cross-checking that height against the tip's offset from its wall and
;;; dropped the idea: no repeater names the wall it hangs on, and mounting has no walking,
;;; reachability, support, or wall-identity consequences, so there is no wall to measure
;;; from.  Reviving the check means first adding a relation that names one.
;;;
;;; APPARATUS-COORDS> gives the horizontal coordinates of the functional point -- the
;;; repeater's outer beam tip -- for either orientation, so an ordinary floor repeater and
;;; an ordinary wall repeater both put their tip at level 1.  Neither orientation names a
;;; Wouldwork location.
;;;
;;; Repeater identity, height, and mounting geometry live in the nested -height/-elevation
;;; roles.  The shared beam technologies own the network:
;;;
;;;   beam-relay
;;;     Owns RELAY (connector or repeater), TERMINUS, PAIRED, COLOR, conflict handling,
;;;     relay lighting, connector actions, receiver arrival, and relay crossing hooks.
;;;
;;;   beam-direct
;;;     Owns directional COUPLED and BEAM-VIA facts for fixed apparatus links and their
;;;     gate/location occlusion and crossing hooks.
;;;
;;; REQUIRES:
;;;   nested     : beam-direct, beam-relay, visibility
;;; PROVIDES:
;;;   types      : repeater (either floor-repeater wall-repeater), from -height
;;;   relations  : paired/color from beam-relay; coupled/beam-via from -beam-substrate;
;;;                apparatus-coords> and sightlines from visibility
;;;   actions    : none -- repeaters are fixed apparatus

(include-tech beam-direct)
(include-tech beam-relay)
(include-tech visibility)
