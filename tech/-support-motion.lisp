;;; Filename: -support-motion.lisp
;;; Shared support geometry, dependency traversal, relocation, and physical landing.
;;; ON and held-tray dependencies move together. Selection policy remains with callers:
;;; blower landings retain first-match/exact-height behavior; settling uses highest-below.
;;; MOUNTED-ON is a soft dependency guarded by optional FAN, as in -placement.

(include-tech -vertical)
(include-tech -location)
(include-tech -position)
(include-tech -holding)
(include-tech -support-occupancy)

(in-package :ww)

(define-optional-types fan floor-blower angled-blower tray)

(define-query physical-supports-at (?location location ?include-trays)
  (do (assign $surfaces nil)
      (doall (?plate plate)
        (if (has-position ?plate ?location) (push ?plate $surfaces)))
      (doall (?fan fan)
        (if (and (bind (mounted-on ?fan $gears))
                 (has-location ?fan ?location))
          (push ?fan $surfaces)))
      (doall (?fixed (either floor-blower angled-blower))
        (if (has-position ?fixed ?location) (push ?fixed $surfaces)))
      (doall (?box box)
        (if (has-location ?box ?location) (push ?box $surfaces)))
      (if ?include-trays
        (doall (?tray tray)
          (if (and (bind (holding $holder ?tray))
                   (has-location ?tray ?location))
            (push ?tray $surfaces))))
      (nreverse $surfaces)))

(define-query support-dependent-p (?current ?base ?seen)
  ;; Follow both sources of vertical dependence, before ever asking TOP to recurse.
  (cond
    ((eql ?current ?base) t)
    ((member ?current ?seen)
     (error "Support/holding cycle at ~S while checking ~S" ?current ?base))
    ((bind (on ?current $under))
     (support-dependent-p $under ?base (cons ?current ?seen)))
    ((bind (holding $holder ?current))
     (support-dependent-p $holder ?base (cons ?current ?seen)))
    (t nil)))

(define-query support-children (?object)
  (do (assign $children nil)
      (doall (?rider support-occupant)
        (if (on ?rider ?object) (push ?rider $children)))
      (if (and (agent ?object)
               (bind (holding ?object $held))
               (tray $held))
        (push $held $children))
      $children))

(define-query support-group (?base support-occupant)
  (do (assign $pending (list ?base))
      (assign $seen nil)
      (ww-loop while $pending
        do (assign $object (pop $pending))
           (if (member $object $seen)
             (error "Repeated support/holding dependency while relocating ~S: ~S"
                    ?base $object))
           (push $object $seen)
           (assign $children (support-children $object))
           (assign $pending (append $children $pending)))
      (nreverse $seen)))

(define-update relocate-stack! (?base support-occupant ?destination location)
  (do (assign $group (support-group ?base))
      (ww-loop for $object in $group
        do (has-location $object ?destination))))

(define-query landing-support (?location location ?self support-occupant ?required-elevation)
  ;; Preserve blower support kinds, category order, and exact-height contract.
  (do (assign $landing nil)
      (assign $surfaces (physical-supports-at ?location nil))
      (ww-loop for $surface in $surfaces
        do (if (and (not $landing)
                    (not (support-dependent-p $surface ?self nil))
                    (cleartop $surface ?self)
                    (support-use-allowed ?self $surface)
                    (or (not ?required-elevation)
                        (eql (top $surface) ?required-elevation)))
             (assign $landing $surface)))
      $landing))

(define-problem-helper normalize-support-rider-locations (state)
  "Predecessor-leaf normalizer: give every object resting ON a support, and every held
   tray, the location of the bottom of its stack, as a move would.  A guessed state then
   needs no separate guess for riders.  A stack with a cycle, or whose bottom has no
   location, is left unchanged for the placement screen to reject."
  (let ((idb (problem-state.idb state))
        (locations (make-hash-table :test #'eq))
        (carriers (make-hash-table :test #'eq))
        (trays (gethash 'tray *types*)))
    (dolist (proposition (list-database idb))
      (cond ((eql (car proposition) 'has-location)
             (setf (gethash (second proposition) locations) (third proposition)))
            ((eql (car proposition) 'on)
             (setf (gethash (second proposition) carriers) (third proposition)))
            ((and (equal (gethash (car proposition) *bijective-canonical*) '(holding . 1))
                  (member (third proposition) trays))
             (setf (gethash (third proposition) carriers) (second proposition)))))
    (loop for object being the hash-keys of carriers
          for bottom-location = (support-stack-bottom-location object carriers locations)
          when (and bottom-location
                    (not (eql bottom-location (gethash object locations))))
            do (add-proposition (list 'has-location object bottom-location) idb))
    (invalidate-problem-state-hash state)))


(define-problem-helper support-stack-bottom-location (object carriers locations)
  "Location of the bottom of OBJECT's stack, following CARRIERS (an ON support, or a
   held tray's holder) down to an object with no carrier.  NIL on a cycle or when the
   bottom has neither HAS-LOCATION nor a static HAS-POSITION."
  (let ((seen nil)
        (current object))
    (loop
      (when (member current seen)
        (return nil))
      (push current seen)
      (let ((carrier (gethash current carriers)))
        (unless carrier
          (return (or (gethash current locations)
                      (first (gethash (list 'has-position current) *static-db*)))))
        (setf current carrier)))))


(register-predecessor-leaf-normalizer 'normalize-support-rider-locations)


(define-update land-on-support!
    (?base support-occupant ?destination location ?required-elevation)
  ;; Rest ?base, already moved to ?destination by relocate-stack!, on the first
  ;; landing-support match there (excluding ?base itself), or leave it resting on bare
  ;; ground (relocate-stack!'s default) if none matches.
  (do (assign $support (landing-support ?destination ?base ?required-elevation))
      (if $support
        (on ?base $support))))
