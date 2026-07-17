;;; Filename: jump.lisp

;;; Jumping technology: change the agent's support at its current location or cross an
;;; authored jump edge.  Landings may be ground or a clear box top.  Level and downward
;;; landings are unrestricted; upward landings are limited by the agent's declared height.
;;; Open gates and passable screens impose no clearance requirement.  Closed gates,
;;; non-passable screens, fences, and walls must be within vaulting reach; a multi-feature
;;; jump must clear the highest feature that is not currently passable.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  box, fence, and wall are declared optional here
;;;   nested    : -support-elevation (support occupancy, location, height, elevation,
;;;               support-top-elevation, and occupant-elevation); -passability
;;;               (holding and obstacle-clear)
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : box, fence, wall  --  declared optional; jumping remains usable without them
;;;               jump-feature (either gate screen fence wall)
;;;   relations : (jump-via location $list location)
;;;               (jump-via> location $list location)
;;;   queries   : jump-elevation-reachable, jump-feature-passable, jump-barrier-height,
;;;               jump-barrier-top-elevation, jump-feature-list,
;;;               jump-required-clearance-height, jump-path-clear
;;;   action    : jump-to

(include-tech -support-elevation)
(include-tech -passability)

(in-package :ww)


(define-optional-types box fence wall)


(define-types
  jump-feature (either gate screen fence wall))


(define-static-relations
  (jump-via location $list location)  ;symmetric jump edge; $list = path features
  (jump-via> location $list location))  ;directed jump edge; $list = path features


(define-query jump-elevation-reachable (?agent agent ?target-elevation)
  ;; Downward and level jumps are unrestricted.  An upward landing may be no more than the
  ;; agent's declared height above its current standing elevation.
  (<= (- ?target-elevation (occupant-elevation ?agent))
      (declared-height ?agent)))


(define-query jump-feature-passable (?agent agent ?feature jump-feature)
  ;; Gates and screens may be crossed without vaulting when their ordinary passability rule
  ;; permits it.  Fences and walls always require clearance.
  (or (and (gate ?feature)
           (obstacle-clear ?agent ?feature))
      (and (screen ?feature)
           (obstacle-clear ?agent ?feature))))


(define-query jump-barrier-height (?feature jump-feature)
  ;; Explicit heights override the defaults: fences are height 2; gates, screens, and walls
  ;; are height 3.
  (if (bind (has-height ?feature $height))
    $height
    (if (fence ?feature)
      2
      3)))


(define-query jump-barrier-top-elevation (?feature jump-feature)
  (+ (object-elevation ?feature)
     (jump-barrier-height ?feature)))


(define-query jump-feature-list (?features)
  (ww-loop for $feature in ?features
           always (jump-feature $feature)))


(define-query jump-required-clearance-height (?agent agent ?features)
  ;; Passable features need no clearance.  Every remaining feature is physically vaulted, so
  ;; the required clearance is the highest of their top elevations.  NIL means all features
  ;; are currently passable or the edge has no features.
  (do (assign $required nil)
      (ww-loop for $feature in ?features
               do (if (not (jump-feature-passable ?agent $feature))
                    (do (assign $top (jump-barrier-top-elevation $feature))
                        (assign $required
                                (if $required
                                  (max $required $top)
                                  $top)))))
      $required))


(define-query jump-path-clear (?agent agent ?features)
  (and (jump-feature-list ?features)
       (assign $required
               (jump-required-clearance-height ?agent ?features))
       (or (not $required)
           (<= (- $required (occupant-elevation ?agent))
               (declared-height ?agent)))))


(define-action jump-to
  ;; Change the agent's support or location: climb onto a clear box at the current location,
  ;; step or drop to local ground, or cross an authored jump edge to ground or a clear box at
  ;; the adjacent location.  The agent may carry cargo throughout.
  1
  (?agent agent)
  (bind (has-location ?agent $a-location))
  (":" ?agent "at" $a-location "jumps onto" $place)
  (do (doall (?box box)
        (if (and (has-location ?box $a-location)
                 (cleartop ?box)
                 (not (on ?agent ?box))
                 (jump-elevation-reachable
                   ?agent (support-top-elevation ?box)))
          (assert (if (bind (on ?agent $current))
                    (not (on ?agent $current)))
                  (on ?agent ?box)
                  (assign $place ?box)
                  (finally (propagate-changes!)))))
      (if (bind (on ?agent $current-support))
        (assert (not (on ?agent $current-support))
                (assign $place 'ground)
                (finally (propagate-changes!))))
      (doall (?landing-box box)
        (if (and (bind (has-location ?landing-box $to-location))
                 (different $a-location $to-location)
                 (or (bind (jump-via $a-location $features $to-location))
                     (bind (jump-via> $a-location $features $to-location)))
                 (jump-path-clear ?agent $features)
                 (cleartop ?landing-box)
                 (jump-elevation-reachable
                   ?agent (support-top-elevation ?landing-box)))
          (assert (if (bind (on ?agent $prior-support))
                    (not (on ?agent $prior-support)))
                  (has-location ?agent $to-location)
                  (on ?agent ?landing-box)
                  (assign $place ?landing-box)
                  (finally (propagate-changes!)))))
      (doall (?to-location location)
        (if (and (or (bind (jump-via $a-location $features ?to-location))
                     (bind (jump-via> $a-location $features ?to-location)))
                 (jump-path-clear ?agent $features)
                 (jump-elevation-reachable
                   ?agent (location-elevation ?to-location)))
          (assert (if (bind (on ?agent $prior-support))
                    (not (on ?agent $prior-support)))
                  (has-location ?agent ?to-location)
                  (assign $place ?to-location)
                  (finally (propagate-changes!)))))))
