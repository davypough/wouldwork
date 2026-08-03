;;; Filename: problem-recorder-scope-test.lisp

;;; Zero-action characterization of the recorder's supported recording-shadow boundary.
;;; The staged problem is a supported fixed-wall-fan configuration.  Direct validation
;;; probes then install one unsupported optional capability at a time and verify that each
;;; fails at recorder initialization rather than acquiring approximate playback physics.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-scope-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  fan (fixed-fan live-fan ghost-fan)
  recorder (recorder1)
  pressure-plate (plate1)
  wall-gears (wall-gears1)
  receiver (receiver1)
  location (source-site destination-site))


(include-tech recorder)
(include-tech plate)
(include-tech wall-blower)


(define-init
  (recording-copy> live-agent ghost-agent)
  (has-location live-agent source-site)
  (has-location ghost-agent source-site)
  (has-position recorder1 source-site)
  (has-position plate1 source-site)
  (has-position wall-gears1 source-site)
  (controls ((plate1)) wall-gears1 normal)
  (mounted-on fixed-fan wall-gears1)
  (welded fixed-fan wall-gears1)
  (aimed-at> wall-gears1 destination-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper recorder-scope-with-type-instances (type instances thunk)
  "Call THUNK with TYPE temporarily bound to INSTANCES in the staged type table."
  (multiple-value-bind (previous present-p)
      (gethash type *types*)
    (unwind-protect
        (progn
          (setf (gethash type *types*) instances)
          (funcall thunk))
      (if present-p
        (setf (gethash type *types*) previous)
        (remhash type *types*)))))


(define-test-helper recorder-scope-with-static-relation (relation signature thunk)
  "Call THUNK with RELATION temporarily installed in the staged static schema."
  (multiple-value-bind (previous present-p)
      (gethash relation *static-relations*)
    (unwind-protect
        (progn
          (setf (gethash relation *static-relations*) signature)
          (funcall thunk))
      (if present-p
        (setf (gethash relation *static-relations*) previous)
        (remhash relation *static-relations*)))))


(define-test-claim recorder-supported-scope-validation
  ;; The staged fixed-fan configuration and explicit plate control are supported.
  (null
    (validate-init-literals
      '((recording-copy> live-agent ghost-agent)
        (has-location live-agent source-site)
        (has-location ghost-agent source-site)
        (controls ((plate1)) wall-gears1 normal)
        (mounted-on fixed-fan wall-gears1)
        (welded fixed-fan wall-gears1))
      :checks '(recorder-init-check)))

  (expect-condition
    (lambda ()
      (recorder-scope-with-type-instances
        'floor-gears '(probe-floor-gears)
        (lambda ()
          (validate-init-literals nil :checks '(recorder-init-check)))))
    'init-check-failure
    :containing "recording-side floor blowers"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (recorder-scope-with-type-instances
        'angled-gears '(probe-angled-gears)
        (lambda ()
          (validate-init-literals nil :checks '(recorder-init-check)))))
    'init-check-failure
    :containing "recording-side angled blowers"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (recorder-scope-with-type-instances
        'gun '(probe-gun)
        (lambda ()
          (validate-init-literals nil :checks '(recorder-init-check)))))
    'init-check-failure
    :containing "recording-side threats"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (recorder-scope-with-type-instances
        'threat '(probe-future-threat)
        (lambda ()
          (validate-init-literals nil :checks '(recorder-init-check)))))
    'init-check-failure
    :containing "recording-side threats"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (recorder-scope-with-static-relation
        'crossings-along-beam> '(los-endpoint list los-endpoint)
        (lambda ()
          (validate-init-literals nil :checks '(recorder-init-check)))))
    'init-check-failure
    :containing "recording-side beam crossings"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls ((receiver1)) wall-gears1 normal))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "wall-gears controls support only plates"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (validate-init-literals
        '((recording-copy> live-fan ghost-fan))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "mapped wall-fan mounting"
    :check 'recorder-init-check)

  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-location fixed-fan source-site))
        :checks '(recorder-init-check)))
    'init-check-failure
    :containing "unmapped mobile object"
    :check 'recorder-init-check))


(define-goal
  (always-true))
