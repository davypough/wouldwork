;;; Filename: -configuration-transition.lisp

;;; Explicit agent-configuration transitions.  Providers describe one state-changing
;;; transition at a time as
;;;
;;;   (mode source-configuration witness destination-configuration)
;;;
;;; where each configuration is (location ground-or-support).  Unlike mobility, these
;;; transitions are never transitively closed: changing support is a planning boundary
;;; because it can clear or occupy supports and trigger propagation.  This is also the
;;; sole place any agent's has-location is ever asserted, so it is the single insertion
;;; point for a held tray to follow its holder: apply-agent-configuration! relocates a
;;; held tray, and everything riding on it, to the agent's destination.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy; -location; -propagation; -holding (cargo, holding;
;;;               tray declared optional here so agent movement never requires a problem
;;;               to declare tray objects)
;;; PROVIDES:
;;;   query     : agent-configuration, configuration-transition-results
;;;   functions : register-configuration-transition-provider and canonical selection helpers
;;;   update    : apply-agent-configuration!  --  also relocates a held tray and its
;;;               riders, keeping the tray's has-location synced to its holder's
;;;               relocate-tray-and-riders!  --  breadth-first (on ...)-chain relocation,
;;;               modeled on -gears-fan's relocate-stack!; kept local so agent movement
;;;               never depends on optional blower technology.  -placement nests this file
;;;               to reuse it when a held tray is released away from where it was picked up

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -propagation)
(include-tech -holding)

(in-package :ww)


(define-optional-types tray)


(defparameter *configuration-transition-providers* nil
  "Problem-local query names that return explicit agent-configuration transitions.")


(define-problem-helper register-configuration-transition-provider (provider)
  "Register a pure configuration-transition provider query for the staged problem."
  (unless (and (symbolp provider)
               (member provider *query-names* :test #'eq))
    (error "Configuration-transition provider must name an installed query: ~S"
           provider))
  (pushnew provider *configuration-transition-providers* :test #'eq)
  provider)


(define-problem-helper configuration-transition-key (transition)
  "Return the stable lexical key for a normalized semantic transition."
  (let ((*print-case* :upcase)
        (*print-pretty* nil)
        (*package* (find-package :ww)))
    (prin1-to-string transition)))


(define-problem-helper configuration-transition-precedes-p
    (transition1 transition2)
  "Order transitions by their normalized semantic representation."
  (string< (configuration-transition-key transition1)
           (configuration-transition-key transition2)))


(define-problem-helper valid-agent-configuration-p (configuration)
  "Whether CONFIGURATION is a normalized (location ground-or-support) pair."
  (and (listp configuration)
       (= (length configuration) 2)
       (member (first configuration) (gethash 'location *types*) :test #'eq)
       (or (eql (second configuration) 'ground)
           (member (second configuration) (gethash 'support *types*) :test #'eq))))


(define-problem-helper validate-configuration-transition (transition source)
  "Signal an error unless TRANSITION obeys the normalized provider contract."
  (unless (and (listp transition)
               (= (length transition) 4)
               (symbolp (first transition))
               (equal (second transition) source)
               (valid-agent-configuration-p (second transition))
               (valid-agent-configuration-p (fourth transition))
               (not (equal (second transition) (fourth transition))))
    (error "Invalid configuration transition from ~S: ~S" source transition))
  transition)


(define-problem-helper configuration-provider-transitions
    (state agent source)
  "Collect, validate, deduplicate, and canonically order provider transitions."
  (let ((transitions nil))
    (dolist (provider *configuration-transition-providers*)
      (dolist (transition (funcall (symbol-function provider) state agent source))
        (push (validate-configuration-transition transition source)
              transitions)))
    (sort (remove-duplicates transitions :test #'equal)
          #'configuration-transition-precedes-p)))


(define-problem-helper canonical-configuration-transitions (transitions)
  "Retain the first canonical transition to each distinct destination configuration."
  (let ((destinations (make-hash-table :test #'equal))
        (selected nil))
    (dolist (transition transitions (nreverse selected))
      (let ((destination (fourth transition)))
        (unless (gethash destination destinations)
          (setf (gethash destination destinations) t)
          (push transition selected))))))


(define-query agent-configuration (?agent agent)
  (do (bind (has-location ?agent $location))
      (assign $place
              (if (bind (on ?agent $support))
                $support
                'ground))
      (list $location $place)))


(define-query configuration-transition-results (?agent agent)
  (do (assign $source (agent-configuration ?agent))
      (assign $transitions
              (configuration-provider-transitions state ?agent $source))
      (canonical-configuration-transitions $transitions)))


(define-update relocate-tray-and-riders! (?tray tray ?destination location)
  ;; Move ?tray and, transitively, every occupant riding on it to ?destination, keeping a
  ;; held tray's has-location synced to its holder's as the holder moves, and carrying the
  ;; same stack to the release location when -placement puts the tray down.  Breadth-first
  ;; over the (on ...) links, modeled on -gears-fan's relocate-stack!, so arbitrary stack
  ;; depth needs no recursion.  Kept local rather than calling -gears-fan's version so
  ;; agent movement never depends on optional blower technology.
  (do (assign $moving (list ?tray))
      (ww-loop while $moving
               do (assign $next nil)
                  (ww-loop for $object in $moving
                           do (has-location $object ?destination)
                              (doall (?y support-occupant)
                                (if (on ?y $object)
                                  (push ?y $next))))
                  (assign $moving $next))))


(define-update apply-agent-configuration!
    (?agent agent ?destination-configuration)
  (do (if (bind (on ?agent $source-support))
        (not (on ?agent $source-support)))
      (has-location ?agent (first ?destination-configuration))
      (if (not (eql (second ?destination-configuration) 'ground))
        (on ?agent (second ?destination-configuration)))
      (if (and (bind (holding ?agent $held))
               (tray $held))
        (relocate-tray-and-riders! $held (first ?destination-configuration)))))
