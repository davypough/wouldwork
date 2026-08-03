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

  ;; Physical landing matrix used by -gears-fan's shared landing-support query.
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


(define-test-helper recorder-isolation-action-applicable-p (state action-name args)
  (let ((action (find action-name *actions* :key #'action.name)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


(define-test-helper recorder-isolation-mappings ()
  '((recording-copy> live-pickup-agent ghost-pickup-agent)
    (recording-copy> live-place-agent ghost-place-agent)
    (recording-copy> live-pair-agent ghost-pair-agent)
    (recording-copy> live-pickup-box ghost-pickup-box)
    (recording-copy> live-support-box ghost-support-box)
    (recording-copy> live-landing-box ghost-landing-box)
    (recording-copy> live-place-connector ghost-place-connector)
    (recording-copy> live-pair-connector ghost-pair-connector)
    (recording-copy> live-target-connector ghost-target-connector)))


(define-test-claim recorder-isolation-validation
  (expect-condition
    (lambda ()
      (validate-init-literals
        (append (recorder-isolation-mappings)
                '((holding live-pickup-agent ghost-pickup-box)))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "HOLDING crosses recording layers"
    :check 'recorder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (append (recorder-isolation-mappings)
                '((on live-pickup-box ghost-support-box)))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "ON crosses recording layers"
    :check 'recorder-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (append (recorder-isolation-mappings)
                '((paired ghost-pair-connector live-target-connector)))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "PAIRED violates recorder connector isolation"
    :check 'recorder-init-check)
  (null
    (validate-init-literals
      (append (recorder-isolation-mappings)
              '((paired live-pair-connector ghost-target-connector)))
      :checks '(recorder-init-check))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


;;;; One named query per theme rather than a single conjunction, so a regression narrows to
;;;; a handful of clauses and each theme can be exercised on its own at the repl, eg
;;;; (funcall 'recorder-isolation-placement-valid *start-state*).


(define-query recorder-isolation-pickup-valid ()
  ;; Generic pickup action: same-side legal, both cross-layer directions illegal.
  (and (recorder-isolation-action-applicable-p
         state 'pickup-box '(live-pickup-agent live-pickup-box))
       (not (recorder-isolation-action-applicable-p
              state 'pickup-box '(live-pickup-agent ghost-pickup-box)))
       (recorder-isolation-action-applicable-p
         state 'pickup-box '(ghost-pickup-agent ghost-pickup-box))
       (not (recorder-isolation-action-applicable-p
              state 'pickup-box '(ghost-pickup-agent live-pickup-box)))))


(define-query recorder-isolation-placement-valid ()
  ;; Placement retains shared ground/plate but filters mobile supports by layer.
  (do (assign $live-places
        (placement-options live-place-agent place-site live-place-connector))
      (assign $ghost-places
        (placement-options ghost-place-agent place-site ghost-place-connector))
      (and (member 'ground $live-places)
           (member 'shared-plate $live-places)
           (member 'live-support-box $live-places)
           (not (member 'ghost-support-box $live-places))
           (member 'ground $ghost-places)
           (member 'shared-plate $ghost-places)
           (member 'ghost-support-box $ghost-places)
           (not (member 'live-support-box $ghost-places)))))


(define-query recorder-isolation-landing-valid ()
  ;; Environmental landings use the same mobile-support isolation.
  (and (eql (landing-support landing-site live-pickup-box nil)
            'live-landing-box)
       (eql (landing-support landing-site ghost-pickup-box nil)
            'ghost-landing-box)))


(define-query recorder-isolation-pairing-valid ()
  ;; Live playback may use either connector layer.  Ghost recording may use only ghost
  ;; movable connectors.  Fixed transmitter apparatus is shared.
  (and (connectable-terminus
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
         state 'connect-connector '(ghost-pair-agent pair-origin))))


(define-goal
  (and (recorder-isolation-pickup-valid)
       (recorder-isolation-placement-valid)
       (recorder-isolation-landing-valid)
       (recorder-isolation-pairing-valid)))
