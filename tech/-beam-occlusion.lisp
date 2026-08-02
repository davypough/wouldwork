;;; Filename: -beam-occlusion.lisp

;;; Beam occlusion substrate: whether a beam-blocking object at a given location intersects
;;; a given beam elevation.  Factored out of beam-direct so the same elevation-aware test is
;;; available to any beam or sightline capability that needs it, not just the direct
;;; transmitter -> receiver corridor -- visibility becomes a second consumer once its own
;;; sightline occluder lists gain location entries alongside gates.
;;;
;;; REQUIRES:
;;;   types  : location, agent  --  box, jammer, connector are declared optional wherever
;;;            the including tech (beam-direct today) declares them; not redeclared here,
;;;            mirroring how -support-occupancy.lisp's own composite types work
;;;   nested : -support-elevation (occupant-elevation, chained through box/fan support;
;;;            plate support does not raise); -height (heighted-object, has-height,
;;;            declared-height); -location (mobile-object, (has-location ...));
;;;            -recording-shadow-policy (recording-side object presence)
;;; PROVIDES:
;;;   types    : beam-blocker (either agent box jammer connector)  --  what can occlude a
;;;              beam or sightline
;;;   queries  : beam-blocker-occludes-location,
;;;              beam-blocker-occludes-location-for-object, beam-blocker-spans-elevation

(include-tech -support-elevation)
(include-tech -height)
(include-tech -location)
(include-tech -recording-shadow-policy)

(in-package :ww)


(define-types
  beam-blocker (either agent box jammer connector))  ;what can block/occlude a beam or sightline


(define-query beam-blocker-occludes-location (?location location ?beam-elevation)
  ;; True iff some beam-blocker at ?location spans ?beam-elevation.
  (exists (?blocker beam-blocker)
    (and (has-location ?blocker ?location)
         (beam-blocker-spans-elevation ?blocker ?beam-elevation))))


(define-query beam-blocker-occludes-location-for-object
    (?view ?location location ?beam-elevation)
  ;; A recording view excludes mapped live blockers that were absent while the trajectory
  ;; was recorded.  Ordinary beam evaluation includes every physical playback blocker.
  (do ?view
      (exists (?blocker beam-blocker)
        (and (recording-shadow-object-present ?blocker)
             (has-location ?blocker ?location)
             (beam-blocker-spans-elevation ?blocker ?beam-elevation)))))


(define-query beam-blocker-spans-elevation (?blocker beam-blocker ?beam-elevation)
  ;; True iff ?blocker's own vertical span -- occupant-elevation up to occupant-elevation
  ;; plus its own declared-height -- covers ?beam-elevation.  Occupant-elevation (from
  ;; -support-elevation) chains through box/fan support; plate support does not raise it.
  (do (assign $base-level (occupant-elevation ?blocker))
      (and (<= $base-level ?beam-elevation)
           (<= ?beam-elevation (+ $base-level (declared-height ?blocker))))))
