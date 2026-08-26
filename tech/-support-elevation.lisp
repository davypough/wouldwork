;;; Filename: -support-elevation.lisp

;;; Support-elevation policy: how far an agent can reach vertically.  This file no longer
;;; computes any geometry -- -vertical owns all of it.
;;;
;;; SUPPORT-TOP-ELEVATION and its held-tray special case TRAY-TOP-ELEVATION were exactly
;;; -vertical's TOP over the SUPPORT domain: a box's top is its base plus its height; a
;;; fan, tray, plate, or fixed blower is zero-thickness, so its top is its base; and a
;;; held tray's base is already its holder's top, which is what the special case computed
;;; by hand.  OCCUPANT-ELEVATION was BASE restricted to SUPPORT-OCCUPANT, minus the
;;; HOLDING branch: it resolved ON, then fell through to the occupant's own location.
;;; That branch is unreachable for every occupant it was called on, since held cargo
;;; loses its HAS-LOCATION -- and the one exception, a tray, is excluded from pickup while
;;; held and is zero-thickness anyway.  Callers use BASE and TOP directly.
;;;
;;; What remains is policy rather than geometry, and stays: the reach limit is a rule
;;; about what an agent may do, not a fact about where anything is.
;;;
;;; REQUIRES:
;;;   nested  : -vertical (base)
;;; PROVIDES:
;;;   parameter : *vertical-reach-limit*, default 1 -- the maximum elevation gap an agent
;;;               can act across vertically: lifting cargo above or below its own elevation,
;;;               raising cargo onto a higher resting place, or jumping up onto a higher
;;;               support or clearing a barrier (jump.lisp reuses this parameter rather than
;;;               defining its own).  Independent of the agent's own declared height.  A
;;;               problem may override it with DEFPARAMETER after including its parent tech.
;;;   queries   : within-agent-vertical-reach (symmetric, for lifting),
;;;               within-agent-placement-reach (one-sided, for setting down)
;;;   function  : vertical-reach-limit-relevant-p -- conservative diagnostic for whether
;;;               manipulation or jumping has a structurally possible nonzero comparison

(include-tech -vertical)

(in-package :ww)


(defparameter *vertical-reach-limit* 1
  "Maximum elevation gap across which an agent may manipulate, land, or clear a barrier.")


(define-optional-types box fan tray)


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; The lifting convention, used by cargo pickup and by any fixture an agent must reach to
  ;; manipulate: measure from the agent's standing elevation, capped by
  ;; *vertical-reach-limit* in either direction, independent of the agent's own declared
  ;; height.  Setting cargo down uses WITHIN-AGENT-PLACEMENT-REACH instead.
  (<= (abs (- ?target-elevation (base ?agent)))
      *vertical-reach-limit*))


(define-query within-agent-placement-reach (?agent agent ?target-elevation)
  ;; The setting-down convention, one-sided where lifting is symmetric: an agent can lower or
  ;; drop cargo any distance below its own standing elevation, since gravity does the work,
  ;; but can only raise a resting place *vertical-reach-limit* above itself.  Recovering what
  ;; it dropped is the symmetric WITHIN-AGENT-VERTICAL-REACH test, so a drop down a ledge is
  ;; deliberately not reversible from where the agent stands.
  (<= (- ?target-elevation (base ?agent))
      *vertical-reach-limit*))


;;;; PARAMETER RELEVANCE ;;;;


(define-problem-helper vertical-reach-technology-present-p (technology)
  (member technology *spliced-tech-names* :test #'string=))


(define-problem-helper vertical-reach-values-vary-p (values)
  (and values
       (some (lambda (value)
               (/= value (first values)))
             (rest values))))


(define-problem-helper vertical-reach-object-values (state objects query)
  (mapcar (lambda (object)
            (funcall (symbol-function query) state object))
          objects))


(define-problem-helper vertical-reach-offset-values
    (state base-values objects)
  (loop for object in objects
        append
          (let ((height
                  (funcall (symbol-function 'object-height) state object)))
            (mapcar (lambda (base-value)
                      (+ base-value height))
                    base-values))))


(define-problem-helper vertical-reach-fixed-supports ()
  (loop for type in '(pressure-plate toggle-plate floor-blower angled-blower)
        append (init-type-instances type)))


(define-problem-helper vertical-reach-other-cargo-p (object cargo)
  (some (lambda (candidate)
          (not (eq candidate object)))
        cargo))


(define-problem-helper vertical-reach-box-support-values
    (state location-values cargo)
  (loop for box in (init-type-instances 'box)
        when (vertical-reach-other-cargo-p box cargo)
          append
            (vertical-reach-offset-values state location-values (list box))))


(define-problem-helper vertical-reach-held-tray-values
    (state location-values agents cargo)
  (when (and (> (length agents) 1)
             (some (lambda (tray)
                     (vertical-reach-other-cargo-p tray cargo))
                   (init-type-instances 'tray)))
    (vertical-reach-offset-values state location-values agents)))


(define-problem-helper vertical-reach-fan-support-values
    (state location-values cargo)
  (when (or (init-type-instances 'floor-gears)
            (init-type-instances 'angled-gears))
    (loop for fan in (init-type-instances 'fan)
          when (vertical-reach-other-cargo-p fan cargo)
            append
              (vertical-reach-offset-values state location-values (list fan)))))


(define-problem-helper vertical-reach-gears-values (state)
  (when (and (vertical-reach-technology-present-p "-gears-fan")
             (init-type-instances 'fan))
    (vertical-reach-object-values state (init-type-instances 'gears)
                                  'blower-elevation)))


(define-problem-helper vertical-reach-manipulation-values (state)
  (let* ((locations (init-type-instances 'location))
         (agents (init-type-instances 'agent))
         (cargo (init-type-instances 'cargo))
         (location-values
           (vertical-reach-object-values state locations 'location-elevation))
         (values
           (append
             location-values
             (vertical-reach-object-values state agents 'base)
             (vertical-reach-object-values state cargo 'base)
             (vertical-reach-gears-values state))))
    (when (vertical-reach-technology-present-p "-placement")
      (setf values
            (append
              values
              (vertical-reach-object-values
                state (vertical-reach-fixed-supports) 'top)
              (vertical-reach-box-support-values
                state location-values cargo)
              (vertical-reach-fan-support-values
                state location-values cargo)
              (vertical-reach-held-tray-values
                state location-values agents cargo))))
    values))


(define-problem-helper vertical-reach-manipulation-relevant-p (state)
  (and (init-type-instances 'agent)
       (init-type-instances 'cargo)
       (vertical-reach-values-vary-p
         (vertical-reach-manipulation-values state))))


(define-problem-helper vertical-reach-symbols-in-tree (tree)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (append (vertical-reach-symbols-in-tree (first tree))
               (vertical-reach-symbols-in-tree (rest tree))))))


(define-problem-helper vertical-reach-jump-feature-relevant-p
    (state family source-values)
  (let ((vaultable-objects (init-type-instances 'vaultable-object)))
    (some
      (lambda (feature)
        (and (member feature vaultable-objects)
             (some
               (lambda (source-value)
                 (> (funcall (symbol-function 'top) state feature)
                    source-value))
               source-values)))
      (vertical-reach-symbols-in-tree family))))


(define-problem-helper vertical-reach-jump-fact-relevant-p (state fact)
  (let* ((relation (first fact))
         (source (third fact))
         (family (fourth fact))
         (destination (fifth fact))
         (source-value
           (funcall (symbol-function 'location-elevation) state source))
         (destination-value
           (funcall (symbol-function 'location-elevation) state destination))
         (source-values
           (if (eq relation 'traverse-via)
             (list source-value destination-value)
             (list source-value)))
         (landing-rises-p
           (if (eq relation 'traverse-via)
             (/= source-value destination-value)
             (> destination-value source-value))))
    (or landing-rises-p
        (vertical-reach-jump-feature-relevant-p
          state family source-values))))


(define-problem-helper vertical-reach-positive-height-box-p (state)
  (some (lambda (box)
          (> (funcall (symbol-function 'object-height) state box) 0))
        (init-type-instances 'box)))


(define-problem-helper vertical-reach-jumping-relevant-p (state)
  (and (vertical-reach-technology-present-p "jump")
       (init-type-instances 'agent)
       (or
         (vertical-reach-positive-height-box-p state)
         (some
           (lambda (fact)
             (and (member (first fact) '(traverse-via traverse-via>))
                  (eq (second fact) 'jumping)
                  (vertical-reach-jump-fact-relevant-p state fact)))
           (list-static-db)))))


(define-problem-helper vertical-reach-limit-relevant-p (state)
  "True when the staged model gives the limit a structurally possible nonzero comparison.
   This deliberately over-approximates reachability; deciding whether such a state is actually
   reachable would require planning."
  (or (vertical-reach-manipulation-relevant-p state)
      (vertical-reach-jumping-relevant-p state)))
