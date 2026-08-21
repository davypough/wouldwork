;;; Filename: -traversal.lisp

;;; Traversal substrate: one topology relation for every way an agent crosses between two
;;; locations, replacing the five near-identical relation pairs -- WALK-VIA/WALK-VIA>,
;;; STAIRS-VIA/STAIRS-VIA>, JUMP-VIA/JUMP-VIA>, CLIMB-VIA> -- and the four near-identical
;;; provider queries that read them.  What genuinely differed between those technologies
;;; was never the relation or the iteration; it was one predicate each, and that is all
;;; each of them registers here now.
;;;
;;;   (traverse-via  <mode> <source> <dnf> <destination>)   symmetric
;;;   (traverse-via> <mode> <source> <dnf> <destination>)   directed, source first
;;;
;;; Directionality stays in the name, as everywhere else in this domain: the engine
;;; mirrors a relation whose argument types repeat and whose name does not end in ">", and
;;; the repeated type here is LOCATION, so prepending the mode leaves the two location
;;; positions as the mirrored pair.  The mode cannot be bound out of a fact -- a fluentless
;;; storage key needs every non-fluent argument ground -- so TRAVERSAL-SEGMENTS iterates
;;; the mode type rather than reading it off the edge.
;;;
;;; The payload is DNF everywhere, which is the change that closes the comprehension
;;; hazard behind the old shape: WALK-VIA read its list as OR-over-clauses while
;;; STAIRS-VIA, JUMP-VIA and CLIMB-VIA> read theirs as a flat conjunction, and both
;;; readings accept (), the common case, so the divergence almost never bit.  Now () is
;;; direct and unguarded, and anything else is a list of clauses: OR over clauses, AND
;;; within one -- the same convention CONTROLS uses.  A mode picks the first clause its
;;; own predicate accepts, in canonical order, so an edge can offer alternative routes
;;; whatever the mode.
;;;
;;; REACH-VIA is deliberately not a mode here.  Reaching across a barrier authorizes
;;; manipulation, not movement: REACHABLE is no mobility provider, applies no elevation or
;;; distance test, and its payload means "these gates must be open" rather than "these
;;; obstacles must be passable for the mover".  Folding it in would put a relation that
;;; moves nobody into the relation that moves everybody.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -mobility (the provider registry) alone.  This file calls no obstacle,
;;;               threat or elevation rule of its own -- every one of those lives in a
;;;               mode's builder, and that mode's technology nests what it needs.  The
;;;               clause types the init check validates against likewise come from
;;;               whichever technology registered the mode
;;; PROVIDES:
;;;   types     : traversal-mode (walking stairway jumping climbing)
;;;   relations : (traverse-via traversal-mode location $list location),
;;;               (traverse-via> traversal-mode location $list location)
;;;   queries   : traversal-segments  --  the single mobility provider
;;;   init      : traversal-init-check
;;;   functions : register-traversal-mode, and the canonical DNF family algebra the
;;;               coordinate zone-graph derivation in -walkability-coordinates uses

(include-tech -mobility)

(in-package :ww)


(define-types
  traversal-mode (walking stairway jumping climbing))


(define-static-relations
  (traverse-via traversal-mode location $list location)  ;symmetric traversal edge; $list = DNF clauses: () direct, else OR over clauses, AND within
  (traverse-via> traversal-mode location $list location))  ;directed traversal edge, source first, same $list convention


;;;; MODE REGISTRY ;;;;
;;;; A mode's technology registers the one predicate that distinguishes it, so this file
;;;; names no gate, ladder, box or elevation rule of its own, and a problem including only
;;;; some of the technologies simply has fewer modes registered.


(defparameter *traversal-modes* nil
  "Registered (MODE BUILDER OBSTACLE-TYPES) entries for the staged problem.  DEFPARAMETER
   rather than DEFVAR so the list resets each time a problem is respliced and loaded.")


(define-problem-helper register-traversal-mode (mode builder obstacle-types)
  "Register MODE's segment builder and the object types its payload clauses may name.
   BUILDER is called as (BUILDER state agent source destination clause) and returns a
   normalized (label source witness destination) segment, or NIL when that clause does not
   permit the crossing.  Registering a mode twice, or one outside TRAVERSAL-MODE, is an
   authoring error rather than a silent overwrite."
  (unless (member mode (gethash 'traversal-mode *types*))
    (error "Traversal mode must be an instance of TRAVERSAL-MODE: ~S" mode))
  (when (assoc mode *traversal-modes*)
    (error "Traversal mode is registered more than once: ~S" mode))
  (setf *traversal-modes*
        (append *traversal-modes* (list (list mode builder obstacle-types))))
  mode)


(define-problem-helper traversal-mode-entry (mode)
  "MODE's registry entry, or an error naming the technology the problem is missing."
  (or (assoc mode *traversal-modes*)
      (error "~%No technology registers the traversal mode ~S.~%~
              Registered modes: ~S~%~
              A traverse-via fact naming a mode means including the technology that owns ~
              it -- walkability, stairs, jump, or ladder."
             mode (mapcar #'first *traversal-modes*))))


;;;; CANONICAL DNF FAMILY ALGEBRA ;;;;
;;;; A family is an antichain of obstacle clauses: OR over clauses, AND within each.
;;;; Shared by the clause selection below and by -walkability-coordinates' zone
;;;; graph, which builds families by extension and union rather than by authoring.


(defparameter *traversal-canonical-families*
  (make-hash-table :test #'equal)
  "Canonical forms of static traversal families encountered in the staged problem.")


(defun traversal-family-union (family1 family2)
  ;; Alternative routes: all clauses of both, minimized and canonicalized.
  (traversal-minimize-family (append family1 family2)))


(defun traversal-family-add-obstacle (family obstacle)
  ;; Path extension by one obstacle, used by the coordinate zone graph.
  (traversal-minimize-family
    (mapcar (lambda (clause) (cons obstacle clause)) family)))


(defun traversal-minimize-family (family)
  ;; Canonical clauses, duplicates removed, and every nonminimal superset discarded.
  (let* ((clauses (remove-duplicates
                    (mapcar #'traversal-canonical-clause family)
                    :test #'equal))
         (minimal (remove-if (lambda (clause)
                               (some (lambda (other)
                                       (and (not (equal other clause))
                                            (subsetp other clause)))
                                     clauses))
                             clauses)))
    (sort (copy-list minimal) #'traversal-clause-precedes-p)))


(define-problem-helper traversal-canonical-family (family)
  "Return FAMILY's canonical form, computing it once per staged static value."
  (multiple-value-bind (canonical present)
      (gethash family *traversal-canonical-families*)
    (if present
      canonical
      (setf (gethash family *traversal-canonical-families*)
            (traversal-minimize-family family)))))


(defun traversal-canonical-clause (clause)
  (sort (copy-list (remove-duplicates clause)) #'string< :key #'symbol-name))


(defun traversal-clause-precedes-p (clause1 clause2)
  (cond ((/= (length clause1) (length clause2))
         (< (length clause1) (length clause2)))
        (t (loop for obstacle1 in clause1
                 for obstacle2 in clause2
                 unless (eq obstacle1 obstacle2)
                   return (string< (symbol-name obstacle1)
                                   (symbol-name obstacle2))
                 finally (return nil)))))


(defun traversal-normalize-family (family)
  ;; A family containing one empty clause is stored as NIL, the direct/unguarded value.
  (if (equal family '(nil))
    nil
    family))


(define-problem-helper traversal-segment-for-family
    (state agent mode source destination family)
  "The first segment MODE's builder accepts over FAMILY's clauses, in canonical order, or
   NIL.  An empty family is the direct case and offers the single empty clause, so a mode
   whose crossing needs no obstacle still gets exactly one attempt -- and the builders read
   an empty clause as trivially clear, exactly as ALL-CLEAR does."
  (let ((builder (second (traversal-mode-entry mode))))
    (loop for clause in (if family
                          (traversal-canonical-family family)
                          (list nil))
          for segment = (funcall (symbol-function builder)
                                 state agent source destination clause)
          when segment
            return segment)))


;;;; SEGMENT PRODUCTION ;;;;


(define-query traversal-segments (?agent agent ?from location)
  ;; The single mobility provider.  Every mode's symmetric and directed edges out of ?FROM,
  ;; each reduced to at most one segment.  The mode is iterated rather than bound because a
  ;; fluentless key needs it ground; four binds per location pair replaces the eight the
  ;; four separate providers used to make.
  (do (assign $segments nil)
      (doall (?mode traversal-mode)
        (doall (?to location)
          (do (assign $symmetric nil)
              (assign $directed nil)
              (if (bind (traverse-via ?mode ?from $symmetric-family ?to))
                (assign $symmetric
                        (traversal-segment-for-family
                          state ?agent ?mode ?from ?to $symmetric-family)))
              (if (bind (traverse-via> ?mode ?from $directed-family ?to))
                (assign $directed
                        (traversal-segment-for-family
                          state ?agent ?mode ?from ?to $directed-family)))
              (if $symmetric
                (assign $segments (cons $symmetric $segments)))
              (if $directed
                (assign $segments (cons $directed $segments))))))
      $segments))


(register-mobility-provider 'traversal-segments)


;;;; INITIALIZATION VALIDATION ;;;;


(define-init-check traversal-init-check (literals)
  (:consumes gate screen ladder wall gears
             floor-gears wall-gears angled-gears
             floor-blower wall-blower angled-blower)
  (check-init-traversal-endpoints literals)
  (check-init-traversal-payloads literals))


(define-init-check-helper check-init-traversal-endpoints (literals)
  "Reject positive traversal self-loops.  Mobility is already reflexive at every location,
   so such a fact can add no route and would otherwise disappear silently in the visited
   set of the closure."
  (dolist (relation '(traverse-via traverse-via>))
    (dolist (literal (positive-init-literals-with-relation relation literals))
      (destructuring-bind (mode source clauses destination)
          (rest (init-literal-proposition literal))
        (declare (ignore mode clauses))
        (when (eql source destination)
          (fail-init-check literal
            "Traversal source and destination are the same location: ~S.  Mobility is already reflexive; remove the self-loop or correct an endpoint."
            source))))))


(define-init-check-helper check-init-traversal-payloads (literals)
  "Every traversal payload is DNF, and each clause item must belong to a type the mode's
   own technology registered -- a wall is vaultable on a jumping edge but means nothing on
   a walking one, so the permitted set is per mode rather than shared.  A fact naming an
   unregistered mode fails here, which is what catches a JUMPING edge in a problem that
   never included jump."
  (dolist (relation '(traverse-via traverse-via>))
    (dolist (literal (init-literals-with-relation relation literals))
      (destructuring-bind (mode source clauses destination)
          (rest (init-literal-proposition literal))
        (declare (ignore source destination))
        (init-check-dnf-list-items-have-types
          literal clauses (third (traversal-mode-entry mode)))))))
