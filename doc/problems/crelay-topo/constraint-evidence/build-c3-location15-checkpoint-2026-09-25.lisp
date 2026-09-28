;;; Build the T10 location15 checkpoint (2026-09-25) from D's validated 80-action route
;;; (validate-c3-alt-2026-09-25.lisp): replay it from a fresh stage, wrap the endpoint as one
;;; accepted phase, and export it.  One action-sequence replay; NO search.
;;; Endpoint: agent1 at location15 holding tray1 with box1 and a paired connector1 on it,
;;; receiver1 active (gate8 open), agent1* on plate3, recorder cycle 3 open.
;;; Settings match the later search (threads 16) so the archive imports into that staging.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/build-c3-location15-checkpoint-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *c3-location15-actions*
  '(;; Cycle 1 -- setup: ghost holds plate1; live agent brings connector1 to plate1; cancel.
    (start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 nil location2)))
    (move > agent1* moves via ((step (location2 ground) nil (location2 plate1))))
    (move > agent1 moves via ((walk location1 (gate1) location7)))
    (move > agent1 moves via ((walk location7 (gate3) location9)))
    (pickup-connector > agent1 picks up connector1 at location9)
    (move > agent1 moves via ((walk location9 (gate3) location7)))
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (put-connector > agent1 puts connector1 on plate1 without pairings at location2)
    (move > agent1 moves via ((walk location2 nil location1)))
    (cancel-playback > agent1 cancels recorder playback)
    ;; Cycle 2 -- ghost holds tray1* at location5; live agent reaches location6 and the box.
    (start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 (gate1) location7)))
    (pickup-tray > agent1* picks up tray1* at location7 from location7)
    (move > agent1* moves via ((walk location7 nil location5)))
    (move > agent1 moves via ((walk location1 (gate1) location4)))
    (move > agent1 moves via ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch > agent1 toggles switch1)
    (move > agent1 moves via ((jump (location20 ground) nil (location5 tray1*))))
    (toggle-switch > agent1 toggles switch1)
    (move > agent1 moves via ((jump (location5 tray1*) (gate2) (location6 ground))))
    (pickup-box > agent1 picks up box1 at location6 from location6)
    (move > agent1 moves via ((jump location6 (gate2) location5)))
    ;; Cycle 2 continued (revised per D) -- box1 to plate2, tray1 left on the ground at
    ;; location7, connector1 stays on plate1.
    (move > agent1 moves via ((walk location5 nil location7)))
    (put-box > agent1 puts box1 on ground at location7)
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (put-tray > agent1 puts tray1 on ground at location7)
    (pickup-box > agent1 picks up box1 at location7 from location7)
    (put-box > agent1 puts box1 on plate2 at location7)
    (move > agent1 moves via ((walk location7 (gate1) location1)))
    (cancel-playback > agent1 cancels recorder playback)
    ;; Cycle 3 (D's alternative, 2026-09-25) -- D's actions 2-50.
    (START-RECORDER AGENT1)
    (MOVE AGENT1* ((WALK LOCATION1 (GATE1) LOCATION7)))
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION7 LOCATION7)
    (MOVE AGENT1* ((WALK LOCATION7 NIL LOCATION5)))
    (MOVE AGENT1 ((WALK LOCATION1 NIL LOCATION2)))
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
    (MOVE AGENT1 ((WALK LOCATION2 (GATE1) LOCATION7)))
    (CONNECT-CONNECTOR AGENT1 CONNECTOR1 GROUND (REPEATER1 RECEIVER1) LOCATION7)
    (PICKUP-BOX AGENT1 BOX1 LOCATION7 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 NIL LOCATION4)))
    (MOVE AGENT1 ((STEP (LOCATION4 GROUND) NIL (LOCATION4 BLOWER1))))
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (PUT-BOX AGENT1 BOX1 TRAY1* LOCATION5)
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION7)))
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 NIL LOCATION4)))
    (MOVE AGENT1 ((STEP (LOCATION4 GROUND) NIL (LOCATION4 BLOWER1))))
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (CONNECT-CONNECTOR AGENT1 CONNECTOR1 BOX1 (REPEATER1 RECEIVER1) LOCATION5)
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION7)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION7 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 (GATE3 GATE5) LOCATION13)))
    (PUT-TRAY AGENT1 TRAY1 PLATE4 LOCATION13)
    (MOVE AGENT1* ((WALK LOCATION5 (GATE3 GATE5) LOCATION12)))
    (MOVE AGENT1* ((STEP (LOCATION12 GROUND) NIL (LOCATION12 PLATE5))))
    (MOVE AGENT1 ((WALK LOCATION13 (GATE6 SCREEN1) LOCATION14)))
    (TOGGLE-SWITCH AGENT1 SWITCH2)
    (MOVE AGENT1 ((WALK LOCATION14 (GATE6 SCREEN1) LOCATION13)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION13 LOCATION13)
    (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION12)))
    (MOVE AGENT1 ((STEP (LOCATION12 GROUND) NIL (LOCATION12 PLATE5))))
    (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION12)
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION12 LOCATION12)
    (MOVE AGENT1* ((STEP (LOCATION12 PLATE5) NIL (LOCATION12 GROUND))))
    (MOVE AGENT1* ((WALK LOCATION12 (GATE1 GATE3 GATE5) LOCATION2)))
    (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
    (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
    (PUT-CONNECTOR AGENT1* CONNECTOR1* GROUND LOCATION2)
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION2 LOCATION2)
    (PUT-TRAY AGENT1* TRAY1* PLATE1 LOCATION2)
    (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
    (MOVE AGENT1* ((WALK LOCATION2 (GATE1 GATE3) LOCATION9)))
    (CONNECT-CONNECTOR AGENT1* CONNECTOR1* GROUND (REPEATER1 TRANSMITTER1) LOCATION9)
    (MOVE AGENT1* ((WALK LOCATION9 NIL LOCATION10)))
    (MOVE AGENT1* ((STEP (LOCATION10 GROUND) NIL (LOCATION10 PLATE3))))
    (MOVE AGENT1 ((STEP (LOCATION12 PLATE5) NIL (LOCATION12 GROUND))))
    (MOVE AGENT1 ((WALK LOCATION12 (GATE7) LOCATION15)))))


(defparameter *c3-location15-goal*
  '(and (has-location agent1 location15)
        (on box1 tray1)
        (on connector1 box1)
        (active receiver1)
        (recording-in-progress)))


(defparameter *c3-location15-archive*
  (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-c3-location15-checkpoint.txt"
                   (asdf:system-source-directory :wouldwork)))


(defun build-c3-location15-checkpoint ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (let* ((initial-checkpoint (capture-search-checkpoint))
         (source (search-checkpoint-state initial-checkpoint))
         (validation (validate-action-sequence source *c3-location15-actions*)))
    (unless (action-sequence-validation-success-p validation)
      (error "Location15 replay failed at action ~S: ~A"
             (action-sequence-validation-failure-index validation)
             (action-sequence-validation-failure-reason validation)))
    (let* ((endpoint (action-sequence-validation-final-state validation))
           (facts (database endpoint)))
      (unless (and (member '(has-location agent1 location15) facts :test #'equal)
                   (member '(on box1 tray1) facts :test #'equal)
                   (member '(on connector1 box1) facts :test #'equal)
                   (member '(active receiver1) facts :test #'equal)
                   (member '(recording-in-progress) facts :test #'equal))
        (error "Location15 replay reached an endpoint that does not satisfy ~S."
               *c3-location15-goal*))
      (let* ((request (make-goal-chain-request
                        :goal (copy-tree *c3-location15-goal*)
                        :final-p nil
                        :settings (capture-goal-chain-search-settings)))
             (solution (make-solution
                         :depth (length *c3-location15-actions*)
                         :time (problem-state.time endpoint)
                         :value (problem-state.value endpoint)
                         :path (copy-tree *c3-location15-actions*)
                         :goal (copy-problem-state endpoint)))
             (session (copy-goal-chain-session-deeply
                        (search-checkpoint-session initial-checkpoint)))
             (phase (make-goal-chain-phase-from-solution request source nil solution)))
        (setf (goal-chain-session-phases session) (list phase))
        (let ((checkpoint (search-checkpoint-from-session session)))
          (export-search-checkpoint checkpoint *c3-location15-archive*)
          (format t "~&LOCATION15 CHECKPOINT BUILT AND EXPORTED: ~S~%" checkpoint)
          checkpoint)))))


(build-c3-location15-checkpoint)
