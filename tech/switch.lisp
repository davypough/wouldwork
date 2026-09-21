;;; Filename: switch.lisp

;;; Wall-mounted switch technology.  A switch has no location of its own: an agent
;;; manipulates it from a location connected to the apparatus by REACH-VIA.  The switch's
;;; mounting level must also be within the agent's vertical reach.  Holding cargo does not
;;; prevent toggling.

(include-tech -switch)
(include-tech reachability)
(include-tech -support-elevation)
(include-tech -propagation)
(include-tech -recording-shadow-policy)

(in-package :ww)


(defun install-toggle-switch ()
  "Install TOGGLE-SWITCH after all technologies have been spliced, so recorder-aware
   problems can select their recording shadow without making ordinary switch problems
   depend on recorder.  Live and ghost toggles intentionally update separate switch
   layers: the ordinary state is the live playback baseline, while the recording state
   is the ghost's shadow memory."
  (let ((recording-switch-state-p
          (nth-value 1 (gethash 'recording-switched-on *relations*))))
    (install-action
      'toggle-switch
      1
      '(?agent agent ?switch switch)
      (if recording-switch-state-p
        '(and (or (live-recording-object ?agent)
                  (and (ghost-recording-object ?agent)
                       (recording-in-progress)))
              (bind (has-location ?agent $agent-location))
              (reachable ?switch $agent-location)
              (within-agent-vertical-reach ?agent (base ?switch)))
        '(and (bind (has-location ?agent $agent-location))
              (reachable ?switch $agent-location)
              (within-agent-vertical-reach ?agent (base ?switch))))
      '(">" ?agent "toggles" ?switch)
      (if recording-switch-state-p
        '(assert
           ;; Live toggles update ordinary state; ghost toggles update only the
           ;; recording shadow.  Cycle-boundary normalization later reseeds the
           ;; shadow from the committed live baseline.
           (if (recording-shadow-object ?agent)
             (if (recording-switched-on ?switch)
               (not (recording-switched-on ?switch))
               (recording-switched-on ?switch))
             (if (switched-on ?switch)
               (not (switched-on ?switch))
               (switched-on ?switch)))
           (finally (propagate-changes!)))
        '(assert
           (if (switched-on ?switch)
             (not (switched-on ?switch))
             (switched-on ?switch))
           (finally (propagate-changes!)))))))


(register-deferred-action-installer 'install-toggle-switch)
