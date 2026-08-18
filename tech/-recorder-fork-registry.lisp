;;; Filename: -recorder-fork-registry.lisp

;;; Registry of optional START-RECORDER fork clauses.  The recorder must copy every live
;;; object's current state onto its ghost, but three of those relations -- PAIRED, JAMMING,
;;; and MOUNTED-ON -- belong to technologies a recorder problem may or may not include
;;; (beam-relay, jammer, -gears-fan).  Rather than have -recorder-session test for each
;;; owner's presence when it is spliced, each owner registers its own fork clause here and
;;; -recorder-session collects them when it installs START-RECORDER.
;;;
;;; This inverts the dependency, which is the point: (include-tech ...) splices textually in
;;; the order a problem lists its directives, so a presence test written in
;;; -recorder-session sees whatever happens to have been spliced before it.  Nothing in the
;;; splicer computes or validates that order, and a recorder listed before beam-relay used
;;; to install a PAIRED fork whose (doall (?terminus terminus)) ranged over an empty domain
;;; -- silently forking no pairings at all.  A clause registered by its own owner cannot be
;;; out of order with respect to that owner, and START-RECORDER's installation is deferred
;;; to INIT, by which point every technology has been spliced.
;;;
;;; This file deliberately nests nothing.  Both sides -- the recorder session and each
;;; relation owner -- include it, so whichever is spliced first brings it in and the
;;; splicer's deduplication gives the second one the same registry.
;;;
;;; PROVIDES:
;;;   variable  : *recorder-fork-clauses*
;;;   function  : register-recorder-fork-clause

(in-package :ww)


(defvar *recorder-fork-clauses* nil
  "Registered (RELATION . CLAUSE) fork contributions, in registration order.  Each CLAUSE
   is an unevaluated effect form spliced into START-RECORDER's assert.")


;; A staged problem re-splices and reloads this file, so clear the preceding problem's
;; registrations here rather than relying on a caller to do it.
(setf *recorder-fork-clauses* nil)


(defun register-recorder-fork-clause (relation clause)
  "Register CLAUSE as RELATION's contribution to START-RECORDER's ghost fork."
  (when (assoc relation *recorder-fork-clauses*)
    (error "Recorder fork clause registered twice for ~S." relation))
  (setf *recorder-fork-clauses*
        (append *recorder-fork-clauses* (list (cons relation clause))))
  relation)
