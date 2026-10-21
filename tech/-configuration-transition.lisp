;;; Filename: -configuration-transition.lisp

;;; Explicit agent-configuration transitions.  Providers describe one state-changing
;;; transition at a time as
;;;
;;;   (mode source-configuration witness destination-configuration)
;;;
;;; where each configuration is (location ground-or-support).  Unlike mobility, these
;;; transitions are never transitively closed: changing support is a planning boundary
;;; because it can clear or occupy supports and trigger propagation.  This is also the
;;; entry point for voluntary agent movement. Shared -support-motion also relocates
;;; supported agents and their held trays during physical transport; both use the
;;; same dependency traversal.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy; -location; -propagation; -holding (cargo, holding;
;;;               tray declared optional here so agent movement never requires a problem
;;;               to declare tray objects)
;;; PROVIDES:
;;;   query     : agent-configuration, configuration-provider-transitions (installed at INIT
;;;               from the registered providers), configuration-transition-results
;;;   functions : register-configuration-transition-provider,
;;;               install-configuration-provider-transitions, configuration-transition-key

;;;   update    : apply-agent-configuration!  --  also relocates a held tray and its
;;;               riders, keeping the tray's has-location synced to its holder's
;;;               relocate-tray-and-riders! -- typed entry to -support-motion's shared
;;;               relocation, including nested held trays and their riders.

(include-tech -support-motion)
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
  (reject-worker-read-write 'register-configuration-transition-provider)
  (unless (and (symbolp provider)
               (member provider *query-names* :test #'eq))
    (error "Configuration-transition provider must name an installed query: ~S"
           provider))
  (pushnew provider *configuration-transition-providers* :test #'eq)
  provider)


(define-problem-helper configuration-transition-key (transition)
  "Return the stable lexical key for a normalized semantic transition, or for any part of
   one built from fixed objects alone."
  (let ((*print-case* :upcase)
        (*print-pretty* nil)
        (*package* (find-package :ww)))
    (prin1-to-string transition)))


(define-problem-helper install-configuration-provider-transitions ()
  "Install CONFIGURATION-PROVIDER-TRANSITIONS as spec code that calls every registered
   provider by name, in registration order.  Collecting the transitions in spec code keeps
   the supports they name out of plain Lisp, so the symmetry safety check follows them
   through each provider.  Deferred to INIT because providers register from technologies
   spliced after this one."
  (install-query 'configuration-provider-transitions
                 '(?agent agent ?source-configuration)
                 `(validated-configuration-transitions
                    (append ,@(mapcar (lambda (provider)
                                        (list provider '?agent '?source-configuration))
                                      (reverse *configuration-transition-providers*)))
                    ?source-configuration)))


(define-query valid-agent-configuration (?configuration)
  ;; Whether ?CONFIGURATION is a normalized (location ground-or-support) pair.
  (and (listp ?configuration)
       (= (length ?configuration) 2)
       (assign $location (first ?configuration))
       (assign $place (second ?configuration))
       (location $location)
       (or (eql $place 'ground)
           (support $place))))


(define-query validated-configuration-transitions (?transitions ?source-configuration)
  ;; Signal an error unless every transition obeys the normalized provider contract.
  (do (ww-loop for $transition in ?transitions
               do (if (not (and (listp $transition)
                                (= (length $transition) 4)
                                (symbolp (first $transition))
                                (equal (second $transition) ?source-configuration)
                                (valid-agent-configuration (second $transition))
                                (valid-agent-configuration (fourth $transition))
                                (not (equal (second $transition) (fourth $transition)))))
                    (error "Invalid configuration transition from ~S: ~S"
                           ?source-configuration $transition)))
      ?transitions))


(define-query configuration-provider-transitions (?agent agent ?source-configuration)
  ;; Placeholder.  INSTALL-CONFIGURATION-PROVIDER-TRANSITIONS replaces this body at INIT.
  (error "CONFIGURATION-PROVIDER-TRANSITIONS was not installed from the registered providers."))


(define-query agent-configuration (?agent agent)
  (do (bind (has-location ?agent $location))
      (assign $place
              (if (bind (on ?agent $support))
                $support
                'ground))
      (list $location $place)))


(define-query configuration-transition-results (?agent agent)
  ;; The first transition to each distinct destination configuration, in provider order.
  ;; Transitions to one destination lead to the same state and differ only in the route
  ;; label.  Each provider lists its transitions in the position's own visit order, so the
  ;; choice never depends on support names.
  (do (assign $source (agent-configuration ?agent))
      (assign $selected nil)
      (assign $destinations nil)
      (ww-loop for $transition in (configuration-provider-transitions ?agent $source)
               do (if (not (member (fourth $transition) $destinations :test #'equal))
                    (do (assign $destinations (cons (fourth $transition) $destinations))
                        (assign $selected (cons $transition $selected)))))
      (reverse $selected)))


(define-update relocate-tray-and-riders! (?tray tray ?destination location)
  (relocate-stack! ?tray ?destination))


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


(register-deferred-action-installer 'install-configuration-provider-transitions)

(register-worker-read-configuration '*configuration-transition-providers*)
