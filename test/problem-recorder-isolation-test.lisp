;;; Filename: problem-recorder-isolation-test.lisp

;;; Zero-action characterization of recorder cross-layer isolation.  It exercises the
;;; installed generic pickup and connector actions in the initial state, inspects exact
;;; placement and physical-landing choices, and probes malformed initial HOLDING, ON, and
;;; PAIRED facts.  Recorder is included first to verify that nested-hook deduplication keeps
;;; its overrides in force when the shared action technologies are spliced later.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-isolation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (live-pickup-agent ghost-pickup-agent
         live-place-agent ghost-place-agent
         live-pair-agent ghost-pair-agent)
  box (live-pickup-box ghost-pickup-box
       live-support-box ghost-support-box
       live-landing-box ghost-landing-box)
  connector (live-place-connector ghost-place-connector
             live-pair-connector ghost-pair-connector
             live-target-connector ghost-target-connector)
  recorder (recorder1)
  pressure-plate (shared-plate)
  transmitter (shared-transmitter)
  location (pickup-site place-site pair-origin
            live-target-site ghost-target-site landing-site))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech recorder)
(include-tech plate)
(include-tech box)
(include-tech floor-blower)
(include-tech beam-relay)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Explicit recording identity.
  (recording-copy> live-pickup-agent ghost-pickup-agent)
  (recording-copy> live-place-agent ghost-place-agent)
  (recording-copy> live-pair-agent ghost-pair-agent)
  (recording-copy> live-pickup-box ghost-pickup-box)
  (recording-copy> live-support-box ghost-support-box)
  (recording-copy> live-landing-box ghost-landing-box)
  (recording-copy> live-place-connector ghost-place-connector)
  (recording-copy> live-pair-connector ghost-pair-connector)
  (recording-copy> live-target-connector ghost-target-connector)

  ;; Recorder and shared support apparatus.
  (has-position recorder1 place-site)
  (has-position shared-plate place-site)

  ;; Pickup matrix: both agents can reach both boxes, leaving layer policy as the
  ;; distinguishing precondition.
  (has-location live-pickup-agent pickup-site)
  (has-location ghost-pickup-agent pickup-site)
  (has-location live-pickup-box pickup-site)
  (has-location ghost-pickup-box pickup-site)

  ;; Placement matrix: each correctly held connector sees shared ground/plate and both
  ;; mobile support layers at one location.
  (has-location live-place-agent place-site)
  (has-location ghost-place-agent place-site)
  (holding live-place-agent live-place-connector)
  (holding ghost-place-agent ghost-place-connector)
  (has-location live-support-box place-site)
  (has-location ghost-support-box place-site)

  ;; Physical landing matrix used by gears-fan's shared landing-support query.
  (has-location live-landing-box landing-site)
  (has-location ghost-landing-box landing-site)

  ;; Pairing matrix: fixed apparatus is shared, live-target and ghost-target locations
  ;; are both structurally visible from the pairing origin.
  (has-location live-pair-agent pair-origin)
  (has-location ghost-pair-agent pair-origin)
  (holding live-pair-agent live-pair-connector)
  (holding ghost-pair-agent ghost-pair-connector)
  (has-location live-target-connector live-target-site)
  (has-location ghost-target-connector ghost-target-site)
  (los-to-location pair-origin () live-target-site)
  (los-to-location pair-origin () ghost-target-site)
  (los-to-apparatus pair-origin () shared-transmitter))


;;;; CHARACTERIZATION HELPERS ;;;;


(defun recorder-isolation-action-applicable-p (state action-name args)
  (let ((action (find action-name *actions* :key #'action.name)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


(defun recorder-isolation-error-contains-p (operation expected-text)
  (let ((condition
          (handler-case
              (progn
                (funcall operation)
                nil)
            (error (error-condition)
              error-condition))))
    (and condition
         (not (null (search expected-text (princ-to-string condition)))))))


(defun recorder-isolation-mappings ()
  '((recording-copy> live-pickup-agent ghost-pickup-agent)
    (recording-copy> live-place-agent ghost-place-agent)
    (recording-copy> live-pair-agent ghost-pair-agent)
    (recording-copy> live-pickup-box ghost-pickup-box)
    (recording-copy> live-support-box ghost-support-box)
    (recording-copy> live-landing-box ghost-landing-box)
    (recording-copy> live-place-connector ghost-place-connector)
    (recording-copy> live-pair-connector ghost-pair-connector)
    (recording-copy> live-target-connector ghost-target-connector)))


(defun recorder-isolation-validation-valid-p ()
  (and
    (recorder-isolation-error-contains-p
      (lambda ()
        (check-init-recorder-consistency
          (append (recorder-isolation-mappings)
                  '((holding live-pickup-agent ghost-pickup-box)))))
      "HOLDING crosses recording layers")
    (recorder-isolation-error-contains-p
      (lambda ()
        (check-init-recorder-consistency
          (append (recorder-isolation-mappings)
                  '((on live-pickup-box ghost-support-box)))))
      "ON crosses recording layers")
    (recorder-isolation-error-contains-p
      (lambda ()
        (check-init-recorder-consistency
          (append (recorder-isolation-mappings)
                  '((paired ghost-pair-connector live-target-connector)))))
      "PAIRED violates recorder connector isolation")
    (not
      (handler-case
          (progn
            (check-init-recorder-consistency
              (append (recorder-isolation-mappings)
                      '((paired live-pair-connector ghost-target-connector))))
            nil)
        (error () t)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query recorder-isolation-scenarios-valid ()
  (do (assign $live-places
        (placement-options live-place-agent place-site live-place-connector))
      (assign $ghost-places
        (placement-options ghost-place-agent place-site ghost-place-connector))
      (and
        ;; Generic pickup action: same-side legal, both cross-layer directions illegal.
        (recorder-isolation-action-applicable-p
          state 'pickup-box '(live-pickup-agent live-pickup-box))
        (not (recorder-isolation-action-applicable-p
               state 'pickup-box '(live-pickup-agent ghost-pickup-box)))
        (recorder-isolation-action-applicable-p
          state 'pickup-box '(ghost-pickup-agent ghost-pickup-box))
        (not (recorder-isolation-action-applicable-p
               state 'pickup-box '(ghost-pickup-agent live-pickup-box)))

        ;; Placement retains shared ground/plate but filters mobile supports by layer.
        (member 'ground $live-places)
        (member 'shared-plate $live-places)
        (member 'live-support-box $live-places)
        (not (member 'ghost-support-box $live-places))
        (member 'ground $ghost-places)
        (member 'shared-plate $ghost-places)
        (member 'ghost-support-box $ghost-places)
        (not (member 'live-support-box $ghost-places))

        ;; Environmental landings use the same mobile-support isolation.
        (eql (landing-support landing-site live-pickup-box nil)
             'live-landing-box)
        (eql (landing-support landing-site ghost-pickup-box nil)
             'ghost-landing-box)

        ;; Live playback may use either connector layer.  Ghost recording may use only
        ;; ghost movable connectors.  Fixed transmitter apparatus is shared.
        (connectable-terminus
          live-pair-agent '(pair-origin) pair-origin
          live-pair-connector live-target-connector)
        (connectable-terminus
          live-pair-agent '(pair-origin) pair-origin
          live-pair-connector ghost-target-connector)
        (connectable-terminus
          ghost-pair-agent '(pair-origin) pair-origin
          ghost-pair-connector ghost-target-connector)
        (not (connectable-terminus
               ghost-pair-agent '(pair-origin) pair-origin
               ghost-pair-connector live-target-connector))
        (connectable-terminus
          live-pair-agent '(pair-origin) pair-origin
          live-pair-connector shared-transmitter)
        (connectable-terminus
          ghost-pair-agent '(pair-origin) pair-origin
          ghost-pair-connector shared-transmitter)
        (recorder-isolation-action-applicable-p
          state 'connect-connector '(live-pair-agent pair-origin))
        (recorder-isolation-action-applicable-p
          state 'connect-connector '(ghost-pair-agent pair-origin))

        ;; Cross-fact validation rejects malformed authored starting states.
        (recorder-isolation-validation-valid-p))))


(define-goal
  (recorder-isolation-scenarios-valid))
