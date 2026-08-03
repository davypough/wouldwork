;;; Filename: -controls.lisp

;;; Controls substrate: the shared DNF controller wiring for controlled devices (gates and
;;; gears) and the energized query that evaluates a single controller.  Owned in one
;;; place so gate.lisp and -gears-fan.lisp nest this file instead of each declaring
;;; controls/energized; both previously lived in gate.lisp.  CONTROL-ON evaluates the DNF
;;; aggregate once for every consumer -- gate, gears, and gun -- taking the uncontrolled
;;; default as an argument, since that is the only thing that varied between the copies it
;;; replaced (an uncontrolled gate reduces to open <=> jammed; uncontrolled gears and guns
;;; run until something disables them).
;;;
;;; Recorder's recording-side aggregate is deliberately a separate query in recorder.lisp
;;; rather than a view argument here.  WW-PROPAGATION-ORDER's walker recurses into query
;;; bodies to compute each update's read set, and prunes only tests STATIC-FORM-TRUTH can
;;; decide; a view passed as a query parameter is not one, so a single body branching on it
;;; would credit UPDATE-GATE-STATUS! with reading RECORDING-LATCHED and
;;; UPDATE-RECORDING-GATE-STATUS! with reading LATCHED.  Those invented edges cross the
;;; playback and recording strata in both directions and can close a cycle in the derived
;;; driver.  Two textually separate aggregates keep the two read sets disjoint, which is
;;; the property the derivation actually needs.
;;;
;;; REQUIRES:
;;;   nested    : -beam-substrate ((active receiver)) -- pulls in the full receiver machinery
;;;               even in a receiver-free problem (e.g. blower-only or gun-only, reached
;;;               through -gears-fan); harmless and expected, since update-receiver-status!
;;;               quantifies over an empty receiver type there and report-inert-techs names it
;;;   conditional relations, owned by plate.lisp:
;;;               depressed (pressure-plate), guarded by pressure-plate
;;;               latched (toggle-plate), guarded by toggle-plate
;;;               Translation removes either guarded reference when its leaf type is empty.
;;; PROVIDES:
;;;   types     : mode (normal inverted), owned here; plate comes from -plate-types;
;;;               gate, floor-gears, wall-gears, angled-gears, receiver, and gun are
;;;               declared optional here.  The gears leaf types appear directly (not via
;;;               the gears union) because this file splices before -gears-fan installs
;;;               the union; gun likewise appears directly since gun.lisp nests this file
;;;               rather than the other way around.
;;;   relations : (controls $list (either gate floor-gears wall-gears angled-gears gun)
;;;               $mode)  --  $list = DNF OR-list of AND-lists of controllers
;;;               (receiver/plate); mode: normal | inverted
;;;   queries   : energized, control-on
;;;
;;; DEFINE-INIT VALIDATION:
;;;   - the DNF value and every clause must be lists
;;;   - every clause member must be a receiver or plate
;;;   - a controlled device may have only one CONTROLS fact
;;;   - only NORMAL and INVERTED modes are supported
;;;   - () and (()) are both valid and intentionally distinct

(include-tech -plate-types)
(include-tech -beam-substrate)
(include-tech -controls-init-checks)

(in-package :ww)


(define-optional-types gate floor-gears wall-gears angled-gears receiver gun)


(define-types
  mode (normal inverted))


(define-static-relations
  ;; $list is a DNF OR-list of AND-lists.  The init validator checks its nested
  ;; controller types because a fluent list value cannot express them in this signature.
  (controls $list (either gate floor-gears wall-gears angled-gears gun) $mode))


(define-query energized (?controller (either receiver plate))
  ;; A receiver follows its beam state.  A pressure plate follows current physical pressure;
  ;; a toggle plate follows its remembered latch instead.
  (or (and (receiver ?controller)
           (active ?controller))
      (and (pressure-plate ?controller)
           (depressed ?controller))
      (and (toggle-plate ?controller)
           (latched ?controller))))


(define-query control-on (?device ?uncontrolled-default)
  ;; The DNF aggregate every controlled device shares: true (normal) iff some clause has
  ;; every member energized, negated under inverted, and ?UNCONTROLLED-DEFAULT when the
  ;; device has no CONTROLS fact at all -- NIL for a gate, whose uncontrolled reading is
  ;; jam-only, T for gears and guns, which run until something disables them.  What the
  ;; result then means is the caller's business: gate ORs it with jamming, gears and gun
  ;; AND it with not-jammed.
  (do (assign $control-on ?uncontrolled-default)
      (if (bind (controls $clauses ?device $mode))
        (do (assign $any-clause-on
              (ww-loop for $clause in $clauses
                       thereis (ww-loop for $controller in $clause
                                        always (energized $controller))))
            (if (eql $mode 'normal)
              (assign $control-on $any-clause-on)
              (if (eql $mode 'inverted)
                (assign $control-on (not $any-clause-on))))))
      $control-on))
