;;; Filename: recorder.lisp

;;; Recorder technology substrate: explicit identity between each live movable object and
;;; the ghost that replays its recording.  RECORDING-COPY> is directional and functional
;;; from live object to ghost.  Initialization validation additionally makes the relation
;;; one-to-one, keeps the two sides disjoint, and requires both members of a pair to share a
;;; MOBILE-OBJECT leaf category (agent-to-agent, connector-to-connector, and so on).
;;;
;;; The mapping is authoritative, not exhaustive over MOBILE-OBJECT.  MOBILE-OBJECT denotes
;;; a capability: an instance such as a welded wall fan can belong to a mobile-capable leaf
;;; type while remaining fixed in this problem.  An unmapped object is therefore neither a
;;; live recording object nor a ghost recording object.
;;;
;;; This file is the identity substrate and the recording-side state machine.  Everything
;;; that runs once per completed candidate path instead of once per propagation pass --
;;; solution validation and the two-phase report -- lives in nested -recorder-solution.lisp,
;;; which reads identity but none of the recording state derived here.  A problem still
;;; writes only (include-tech recorder) and gets both.
;;;
;;; REQUIRES:
;;;   nested : -location (mobile-object); -position (recorder has-position role and plate
;;;            types); -support-occupancy (on); -propagation; -interaction-policy
;;;            (neutral action hooks); -recording-shadow-policy (neutral state-view hooks);
;;;            -controls (controller wiring and receiver beam substrate); -gate
;;;            (actor-aware gate view); -recorder-solution (validation and report)
;;; PROVIDES:
;;;   type     : recorder (optional)
;;;   relation : recording-copy> (live mobile-object -> ghost mobile-object)
;;;              recording-depressed, recording-latched, recording-turning,
;;;              recording-active, recording-open
;;;   queries  : live-recording-object, ghost-recording-object, same-recording-side;
;;;              recording-control-on (recording-side twin of -controls' control-on);
;;;              recorder overrides object-manipulation-allowed, support-use-allowed,
;;;              connector-pairing-allowed, recording-shadow-object, and
;;;              recording-shadow-turning, recording-shadow-gate-open
;;;   updates  : update-recording-plate-status!, update-recording-receiver-status!,
;;;              update-recording-gate-status!, update-recording-gears-status!
;;;
;;; The recording shadow covers the Windtunnel controls: plates, wall gears, gates, and
;;; direct or relay-fed gate receivers.  Recording beam evaluation excludes mapped live
;;; objects and uses recording-side gate transparency.  There are no searched recorder
;;; controls.
;;;
;;; Two combinations lie outside the shadow, and both are refused at init rather than
;;; approximated at runtime, so an unsupported problem fails where it is authored instead
;;; of solving to a quietly wrong answer.  INIT-CHECK-RECORDING-WALL-GEARS-CONTROLS rejects
;;; a wall-gears device controlled by anything but a plate.  INIT-CHECK-RECORDING-JAMMERS
;;; rejects a problem that has jammers at all, because the recording gate and gears updates
;;; carry no jam term where their playback counterparts do.  Beam crossings are likewise
;;; unmodeled here.
;;;
;;; Recording-side jamming is wanted eventually, and is smaller than it looks.  JAMMING is
;;; asserted by JAM-TARGET rather than derived by an update, so it needs no parallel
;;; RECORDING-JAMMING relation and no parallel update: a query filtering JAMMING to ghost
;;; jammers -- the same ghost filter RECORDING-PLATE-OCCUPIED already applies to occupants
;;; -- supplies the recording-side reading, and the two updates gain the disjunct and the
;;; negated conjunct their playback counterparts carry.  The real work is upstream:
;;; JAM-TARGET tests its sightline with VISIBLE, the actor-blind form, so a ghost jamming
;;; through a gate would read playback openness.  That call has to route through the
;;; actor-aware view the beam queries already use before the shadow can be trusted.

(include-tech -location)
(include-tech -position)
(include-tech -support-occupancy)
(include-tech -propagation)
(include-tech -interaction-policy)
(include-tech -recording-shadow-policy)
(include-tech -controls)
(include-tech -gate)
(include-tech -recorder-solution)
(include-tech -recorder-init-checks)

(in-package :ww)


(define-optional-types recorder connector wall-gears gate receiver)


(define-static-relations
  (recording-copy> mobile-object $mobile-object))


(define-dynamic-relations
  (recording-depressed plate)
  (recording-latched toggle-plate)
  (recording-turning wall-gears)
  (recording-active receiver)
  (recording-open gate))


(define-derived-relations
  recording-depressed
  recording-latched
  recording-turning
  recording-active
  recording-open)


(define-query live-recording-object (?object mobile-object)
  (exists (?ghost mobile-object)
    (recording-copy> ?object ?ghost)))


(define-query ghost-recording-object (?object mobile-object)
  (exists (?live mobile-object)
    (recording-copy> ?live ?object)))


(define-query same-recording-side (?object1 mobile-object ?object2 mobile-object)
  (or (and (live-recording-object ?object1)
           (live-recording-object ?object2))
      (and (ghost-recording-object ?object1)
           (ghost-recording-object ?object2))))


(define-query recording-shadow-object (?object)
  (and (mobile-object ?object)
       (ghost-recording-object ?object)))


(define-query recording-shadow-object-present (?object)
  ;; Fixed apparatus and genuinely unmapped objects exist in both views.  Of each mapped
  ;; pair, only the ghost copy existed while the recording was made.
  (or (not (mobile-object ?object))
      (ghost-recording-object ?object)
      (and (not (live-recording-object ?object))
           (not (ghost-recording-object ?object)))))


(define-query recording-shadow-turning (?gears)
  (and (wall-gears ?gears)
       (recording-turning ?gears)))


(define-query recording-shadow-gate-open (?gate)
  (and (gate ?gate)
       (recording-open ?gate)))


(define-query object-manipulation-allowed (?actor ?object)
  ;; Recorder participants may manipulate only mapped objects on their own side.
  (and (mobile-object ?actor)
       (mobile-object ?object)
       (same-recording-side ?actor ?object)))


(define-query support-use-allowed (?occupant ?support)
  ;; Fixed supports such as plates are shared environmental apparatus.  A mobile support
  ;; (box or floor-mounted fan) is usable only by an occupant on the same recording side.
  (or (not (mobile-object ?support))
      (and (mobile-object ?occupant)
           (same-recording-side ?occupant ?support))))


(define-query connector-pairing-allowed (?actor ?connector ?terminus)
  ;; Fixed beam apparatus is shared.  During playback a live connector may use either
  ;; layer's connector as a terminus, while a ghost connector may depend only on another
  ;; ghost connector -- never on a live movable connector absent from its recording.
  (and (object-manipulation-allowed ?actor ?connector)
       (or (not (connector ?terminus))
           (and (live-recording-object ?actor)
                (or (live-recording-object ?terminus)
                    (ghost-recording-object ?terminus)))
           (and (ghost-recording-object ?actor)
                (ghost-recording-object ?terminus)))))


(define-query recording-plate-occupied (?plate plate)
  (exists (?occupant support-occupant)
    (and (ghost-recording-object ?occupant)
         (on ?occupant ?plate))))


(define-query recording-controller-energized (?controller (either receiver plate))
  (or (and (receiver ?controller)
           (recording-active ?controller))
      (and (pressure-plate ?controller)
           (recording-depressed ?controller))
      (and (toggle-plate ?controller)
           (recording-latched ?controller))))


(define-update update-recording-plate-status! ()
  ;; The recording view contains only mapped ghost occupants.  During initialization its
  ;; toggle latch starts from the authored playback latch; afterward it changes only on a
  ;; ghost-only clear-to-depressed transition.
  (doall (?plate plate)
    (do (if (and *applying-init-action*
                 (toggle-plate ?plate))
          (if (latched ?plate)
            (recording-latched ?plate)
            (not (recording-latched ?plate))))
        (if (recording-plate-occupied ?plate)
          (do (if (and (not *applying-init-action*)
                       (toggle-plate ?plate)
                       (not (recording-depressed ?plate)))
                (if (recording-latched ?plate)
                  (not (recording-latched ?plate))
                  (recording-latched ?plate)))
              (recording-depressed ?plate))
          (not (recording-depressed ?plate))))))


(define-update update-recording-receiver-status! ()
  (doall (?receiver receiver)
    (if (recording-shadow-beam-reaches-receiver ?receiver)
      (recording-active ?receiver)
      (not (recording-active ?receiver)))))


(define-query recording-control-on (?device ?uncontrolled-default)
  ;; The recording-side twin of -controls' CONTROL-ON: identical DNF polarity and the same
  ;; uncontrolled-default argument, reading ghost-only controller state.  It is a separate
  ;; query rather than a view argument to CONTROL-ON so that the two read sets stay
  ;; disjoint for WW-PROPAGATION-ORDER's walker -- see -controls.lisp's header.
  (do (assign $control-on ?uncontrolled-default)
      (if (bind (controls $clauses ?device $mode))
        (do (assign $any-clause-on
              (ww-loop for $clause in $clauses
                       thereis
                         (ww-loop for $controller in $clause
                                  always
                                    (recording-controller-energized $controller))))
            (if (eql $mode 'normal)
              (assign $control-on $any-clause-on)
              (if (eql $mode 'inverted)
                (assign $control-on (not $any-clause-on))))))
      $control-on))


(define-update update-recording-gate-status! ()
  ;; Recording gates use the ordinary DNF polarity, but their controllers read ghost-only
  ;; plates and receivers.  No jam disjunct here, matching the recording shadow's declared
  ;; scope -- see the header; INIT-CHECK-RECORDING-JAMMERS rejects the combination outright
  ;; rather than letting a ghost jammer be silently ignored.
  (doall (?gate gate)
    (if (recording-control-on ?gate nil)
      (recording-open ?gate)
      (not (recording-open ?gate)))))


(define-update update-recording-gears-status! ()
  ;; Uncontrolled wall gears turn; controlled wall gears evaluate their DNF against
  ;; recording-side plate state.  The plate-only restriction is authored wiring, not state,
  ;; so INIT-CHECK-RECORDING-WALL-GEARS-CONTROLS enforces it once at init rather than on
  ;; every propagation pass.
  (doall (?gears wall-gears)
    (if (recording-control-on ?gears t)
      (recording-turning ?gears)
      (not (recording-turning ?gears)))))
