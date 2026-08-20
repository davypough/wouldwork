;;; Filename: problem-jammer-height-visibility-test.lisp

;;; Focused regression for elevation-aware jammer sight.  A default-height wall separates
;;; a jammer from a gun whose functional point is elevation 5.  Ground placement leaves the
;;; jammer top at 1 and blocks, while placement on a height-4 box raises its top to 5 and
;;; clears both the wall and a closed gate.  JAM-TARGET must therefore generate only the elevated placement.  Expected
;;; minimum path length: one action.

(in-package :ww)


(ww-set *problem-name* jammer-height-visibility-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(define-types
  agent (height-agent)
  location (height-site low-probe-site)
  jammer (height-jammer)
  box (height-box height-agent-platform)
  gun (height-gun)
  gate (sight-gate)
  wall (height-wall))


(include-tech jammer)
(include-tech gun)
(include-tech visibility)


(define-init
  (has-location height-agent height-site)
  (has-height height-agent 4)
  (holding height-agent height-jammer)
  (has-height height-jammer 1)

  ;; Stand one unit below HEIGHT-BOX's top so the fixed unit reach permits the
  ;; elevated placement without changing the visibility geometry under test.
  (has-location height-agent-platform height-site)
  (has-height height-agent-platform 3)
  (on height-agent height-agent-platform)

  (has-location height-box height-site)
  (has-height height-box 4)

  (has-elevation height-gun 5)

  (location-coords> height-site 0 0)
  (location-coords> low-probe-site 0 2)
  (apparatus-coords> height-gun 10 0)
  (wall-segment> height-wall 5 -1 5 3)
  (gate-segment> sight-gate 7 -1 7 3))


(define-query jammer-height-visibility-scenarios-valid ()
  (and
    (has-location height-agent height-site)
    (not (holding height-agent height-jammer))
    (has-location height-jammer height-site)
    (on height-jammer height-box)
    (jamming height-jammer height-gun)

    ;; Coordinate derivation retains the wall crossing for pairing but ordinary sight
    ;; remains opaque regardless of endpoint height.
    (potentially-visible height-site height-gun)
    (not (visible height-site height-gun))
    (bind (los-barrier-crossings>
            height-site $crossings height-gun))
    (equal $crossings
           '((:wall height-wall 1/2 5 -1 5 3)
             (:gate sight-gate 7/10 7 -1 7 3)))
    (= (object-height height-wall) 4)
    (= (object-height sight-gate) 4)
    (not (open sight-gate))

    ;; The actual elevated support clears strictly above the wall.  The ground option and
    ;; an independent low vantage remain blocked.
    (jammer-target-visible-from-placement
      nil height-site height-box height-jammer height-gun)
    (not (jammer-target-visible-from-placement
           nil height-site 'ground height-jammer height-gun))
    (not (jammer-target-visible-from-placement
           nil low-probe-site 'ground height-jammer height-gun))))


(define-goal
  (jammer-height-visibility-scenarios-valid))
