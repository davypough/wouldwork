;;; Filename: -recorder-jamming-shadow.lisp

;;; Recording-side jamming view shared by gates and wall gears.  JAMMING remains the one
;;; action-owned relation; this component filters it to mapped ghost jammers rather than
;;; duplicating either the relation or its lifecycle.
;;;
;;; REQUIRES:
;;;   nested      : -recorder-core (ghost identity)
;;;   conditional : JAMMING from jammer.lisp, guarded by optional JAMMER
;;; PROVIDES:
;;;   query : recording-jammed

(include-tech -recorder-core)

(in-package :ww)


(define-optional-types gate wall-gears jammer)


(define-query recording-jammed (?target (either gate wall-gears))
  ;; Only a mapped ghost jammer existed during the recording, so live-only playback jams
  ;; do not leak into the recording shadow.
  (exists (?jammer jammer)
    (and (ghost-recording-object ?jammer)
         (jamming ?jammer ?target))))
