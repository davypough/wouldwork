;;; Filename: -passability.lisp

;;; Passability substrate: whether an agent may pass each obstacle or enabling
;;; implement on a traversal edge.  Shared by walking, jumping, and ladder use.
;;;
;;; Gate, screen, and ladder are owned outright here.  GEARS is reserved as a fourth
;;; obstacle kind through a null-object default hook, STREAM-OBSTACLE-CLEAR, so a
;;; technology like -stream-passability can override just that one slot instead of
;;; redefining OBSTACLE-CLEAR whole -- the same pattern -beam-substrate uses for its
;;; peers.  GEARS itself is declared optional here without nesting -gears-fan, so a
;;; walking-only problem never pulls in mounting machinery it doesn't use.
;;;
;;; REQUIRES:
;;;   types     : agent
;;;   nested    : -holding (cargo, holding); -gate (gate optional type, (open gate) relation)
;;; PROVIDES:
;;;   types     : screen, ladder, gears  --  declared optional; gears is a bare
;;;               reservation, populated for real only when -gears-fan is also loaded
;;;   queries   : obstacle-clear, all-clear, and an actor-aware null-object default for
;;;               stream-obstacle-clear (overridden by -stream-passability)

(include-tech -holding)
(include-tech -gate)

(in-package :ww)


(define-optional-types screen ladder gears)


(define-query all-clear (?agent agent ?obstacles)
  ;; The empty list is clear.  Otherwise every listed obstacle or enabling
  ;; implement must be usable by the agent. ?obstacles is computed Lisp list data.
  (ww-loop for $obstacle in ?obstacles
           always (obstacle-clear ?agent $obstacle)))


(define-query obstacle-clear (?agent agent ?obstacle (either gate screen ladder gears))
  ;; An open gate passes.  A screen or ladder passes only when the agent is
  ;; empty-handed.  A gears obstacle defers to stream-obstacle-clear, whose default
  ;; passes everything until a technology like -stream-passability overrides it with
  ;; real stream logic.
  (or (and (gate ?obstacle)
           (gate-open-for-object ?agent ?obstacle))
      (and (screen ?obstacle)
           (not (bind (holding ?agent $any-held-object))))
      (and (ladder ?obstacle)
           (not (bind (holding ?agent $any-held-object))))
      (and (gears ?obstacle)
           (stream-obstacle-clear ?agent ?obstacle))))


;;;; NULL-OBJECT DEFAULT HOOK ;;;;
;;;; Overridden by whichever technology owns a gears-typed obstacle's real clearance
;;;; rule.  Absent that technology, gears has no instances, so obstacle-clear's gears
;;;; branch never actually binds and this default is never called.


(define-query stream-obstacle-clear (?agent agent ?obstacle gears)
  (do ?agent ?obstacle t))
