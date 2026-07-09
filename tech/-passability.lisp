;;; Filename: -passability.lisp

;;; Passability substrate: whether an agent may pass each obstacle or enabling
;;; implement on a traversal edge.  Shared by walking, jumping, and ladder use.
;;;
;;; REQUIRES:
;;;   types     : agent
;;;   nested    : -holding (cargo, holding)
;;; PROVIDES:
;;;   types     : gate, screen, ladder  --  declared optional
;;;   relation  : (open gate)
;;;   queries   : obstacle-clear, all-clear

(include-tech -holding)

(in-package :ww)


(define-optional-types gate screen ladder)


(define-dynamic-relations
  (open gate))  ;only gate's update-gate-status! writes it


(define-query all-clear (?agent ?obstacles)
  ;; The empty list is clear.  Otherwise every listed obstacle or enabling
  ;; implement must be usable by the agent.
  (ww-loop for $obstacle in ?obstacles
           always (obstacle-clear ?agent $obstacle)))


(define-query obstacle-clear (?agent agent ?obstacle (either gate screen ladder))
  ;; An open gate passes.  A screen or ladder passes only when the agent is
  ;; empty-handed.
  (or (and (gate ?obstacle)
           (open ?obstacle))
      (and (screen ?obstacle)
           (not (bind (holding ?agent $any-held-object))))
      (and (ladder ?obstacle)
           (not (bind (holding ?agent $any-held-object))))))
