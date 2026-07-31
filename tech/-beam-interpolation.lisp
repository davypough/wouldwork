;;; Filename: -beam-interpolation.lisp

(in-package :ww)


;;; Beam interpolation substrate: include-order-independent hook for finding a sloped beam's
;;; elevation at an intervening location.  A horizontal beam needs no coordinates and keeps
;;; the default.  visibility overrides the hook with coordinate interpolation for endpoints
;;; at different elevations.  beam-direct consumes it for fixed coupled corridors.
;;;
;;; REQUIRES:
;;;   nested  : -beam-substrate (beam-node)
;;;   type    : location
;;; PROVIDES:
;;;   query   : beam-elevation-at-location -- horizontal default; visibility override

(include-tech -beam-substrate)


(define-query beam-elevation-at-location
    (?location location
     ?from beam-node
     ?near-elevation
     ?to beam-node
     ?far-elevation)
  (do ?location ?from ?to
      (if (= ?near-elevation ?far-elevation)
        ?near-elevation
        (error "A sloped fixed beam requires visibility's coordinate interpolation."))))
