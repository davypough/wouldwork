;;; Filename: -support-settling.lisp
;;; Event-driven, same-location tray-release settling. No global gravity sweep.
;;; Candidate geometry is the final tray placement with displaced roots detached.
;;; All landings are selected before any is applied; no iteration-order tie breaking.
;;; For backward search, RELEASED-TRAY-RIDER-VARIANTS (registered by tray.lisp as PUT-TRAY's
;;; predecessor-leaf expander) guesses riders back onto a set-down tray.

(include-tech -support-motion)

(in-package :ww)

(define-query tray-release-riders (?tray tray ?capture-height)
  (do (assign $riders nil)
      (doall (?rider support-occupant)
        (if (on ?rider ?tray)
          (do (assign $level nil)
              (if ?capture-height (assign $level (base ?rider)))
              (push (list ?rider $level) $riders))))
      $riders))

(define-query settling-support (?rider support-occupant ?location location ?ceiling ?excluded)
  (do (assign $best nil)
      (assign $height (location-elevation ?location))
      (if (> $height ?ceiling)
        (error "Settling floor ~S exceeds previous base ~S for ~S"
               $height ?ceiling ?rider))
      (assign $surfaces (physical-supports-at ?location t))
      (ww-loop for $surface in $surfaces
        do (if (and (not (member $surface ?excluded))
                    (support-use-allowed ?rider $surface)
                    (not (support-dependent-p $surface ?rider nil))
                    (cleartop $surface ?rider))
             (do (assign $level (top $surface))
                 (if (and (<= $level ?ceiling) (>= $level $height))
                   (do (if (> $level $height)
                         (assign $best nil))
                       (assign $height $level)
                       (push $surface $best))))))
      (if (rest $best)
        (error "Ambiguous settling for ~S at ~S, height ~S: ~S"
               ?rider ?location $height $best))
      (first $best)))

(define-query released-rider-group (?riders)
  (do (assign $excluded nil)
      (ww-loop for $entry in ?riders
        do (assign $group (support-group (first $entry)))
           (assign $excluded (append $group $excluded)))
      $excluded))

(define-query released-rider-landings (?riders ?location location)
  (do (assign $excluded (released-rider-group ?riders))
      (assign $landings nil)
      (ww-loop for $entry in ?riders
        do (assign $target
             (settling-support (first $entry) ?location (second $entry) $excluded))
           (ww-loop for $prior in $landings
             do (if (and $target (eql $target (second $prior))
                         (support-occupancy-conflict-p (first $entry) (first $prior)))
                  (error "Contending settling riders ~S and ~S on ~S"
                         (first $entry) (first $prior) $target)))
           (push (list (first $entry) $target) $landings))
      $landings))

(define-update settle-released-riders! (?riders ?location location)
  (do (assign $landings (released-rider-landings ?riders ?location))
      (ww-loop for $landing in $landings
        do (assign $rider (first $landing))
           (assign $support (second $landing))
           (if $support (on $rider $support)))))


(define-problem-helper released-tray-rider-variants (state)
  "Predecessor-leaf expander for PUT-TRAY.  STATE is a guessed predecessor; the rider
   it copies from the state after has already settled off the set-down tray, so put it
   back.  For each empty tray held at its holder's location, one variant per object
   there that could have ridden it, and one per pair of them that may share a top (a
   live object and its ghost), with each rider's ON replaced by the tray.  A rider's
   own riders stay on it.  Returns fresh copies; STATE is unchanged."
  (multiple-value-bind (locations on-map holders supports)
      (released-tray-state-tables state)
    (let ((variants nil))
      (maphash
        (lambda (tray holder)
          (when (and (member tray (gethash 'tray *types*))
                     (not (gethash tray supports)))
            (let ((riders (released-tray-rider-candidates
                            tray holder (gethash tray locations) locations on-map holders)))
              (dolist (rider riders)
                (push (released-tray-rider-variant state (list rider) tray) variants))
              (loop for (rider . others) on riders
                    do (dolist (other others)
                         (unless (funcall (symbol-function 'support-occupancy-conflict-p)
                                          state rider other)
                           (push (released-tray-rider-variant state (list rider other) tray)
                                 variants)))))))
        holders)
      (nreverse variants))))


(define-problem-helper released-tray-state-tables (state)
  "Four tables from STATE: object -> HAS-LOCATION, occupant -> ON support, held object
   -> holder, and support -> T when something rests on it."
  (let ((locations (make-hash-table :test #'eq))
        (on-map (make-hash-table :test #'eq))
        (holders (make-hash-table :test #'eq))
        (supports (make-hash-table :test #'eq)))
    (dolist (proposition (list-database (problem-state.idb state)))
      (cond ((eql (car proposition) 'has-location)
             (setf (gethash (second proposition) locations) (third proposition)))
            ((eql (car proposition) 'on)
             (setf (gethash (second proposition) on-map) (third proposition))
             (setf (gethash (third proposition) supports) t))
            ((equal (gethash (car proposition) *bijective-canonical*) '(holding . 1))
             (setf (gethash (third proposition) holders) (second proposition)))))
    (values locations on-map holders supports)))


(define-problem-helper released-tray-rider-candidates
    (tray holder location locations on-map holders)
  "Support occupants at LOCATION that could have ridden TRAY: not TRAY or its HOLDER,
   not held, and not under HOLDER's own ON chain."
  (remove-if-not
    (lambda (object)
      (and (eql (gethash object locations) location)
           (not (eql object tray))
           (not (eql object holder))
           (not (gethash object holders))
           (not (support-chain-member-p object holder on-map))))
    (gethash 'support-occupant *types*)))


(define-problem-helper support-chain-member-p (object start on-map)
  "True when OBJECT is below START in START's ON chain.  The placement screen rejects
   ON cycles, so the chain ends."
  (let ((support (gethash start on-map)))
    (loop while support
          when (eql support object)
            return t
          do (setf support (gethash support on-map)))))


(define-problem-helper released-tray-rider-variant (state riders tray)
  "A copy of STATE with each of RIDERS resting ON TRAY."
  (let ((variant (copy-problem-state state)))
    (dolist (rider riders)
      (add-proposition (list 'on rider tray) (problem-state.idb variant)))
    (invalidate-problem-state-hash variant)
    variant))
