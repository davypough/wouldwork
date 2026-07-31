;;; Filename: repeater.lisp

(in-package :ww)


;;; Fixed repeater technology: stationary relay apparatus that pass beam color through
;;; permanent directional couplings and pair with movable connectors.  A repeater behaves
;;; like a connector in the beam network -- it acquires exactly one incoming hue, remains
;;; unlit when conflicting hues reach it, and can relay its hue onward -- but it has no
;;; HAS-LOCATION, cannot be carried or placed, and imposes no pairing or coupling limit.
;;;
;;; Every repeater is classified by mounting orientation:
;;;
;;;   floor-repeater
;;;     HAS-ELEVATION is the elevation of the floor under its base, defaulting to 0.
;;;     The beam anchor is that base elevation plus DECLARED-HEIGHT.
;;;
;;;   wall-repeater
;;;     HAS-ELEVATION is the wall mounting elevation and therefore also the beam-anchor
;;;     elevation, defaulting to 1.  The repeater extends horizontally, so its height does
;;;     not raise the beam anchor.
;;;
;;; DECLARED-HEIGHT defaults to 1 for either orientation.  APPARATUS-COORDS> always gives
;;; the horizontal coordinates of the functional point -- the repeater's outer beam tip.
;;; Thus an ordinary floor repeater and an ordinary wall repeater both default to beam-anchor
;;; elevation 1.  Neither orientation names a Wouldwork location or a particular wall:
;;; mounting has no walking, reachability, support, or wall-identity consequences.
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
