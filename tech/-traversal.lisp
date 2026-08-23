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
;;;   queries   : traversal-segments  --  the single mobility provider, cached;
;;;               traversal-segments-for-source  --  the computation behind it
;;;   init      : traversal-init-check
;;;   functions : register-traversal-mode, register-traversal-cache-parameter, and the
;;;               canonical DNF family algebra the coordinate zone-graph derivation in
;;;               -walkability-coordinates uses

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


;;;; SEGMENT CACHE ;;;;
;;;; TRAVERSAL-SEGMENTS is a pure function of the agent, the source location, the value of
;;;; every parameter a registered builder reads, and the state's projection onto a short
;;;; list of dynamic relations.  So it is cached by CONTENT: two states projecting alike
;;;; share an entry, and nothing ever needs invalidating.  Measured on RUMIN-TOPO at
;;;; depth 8, 831,175 calls collapse to 10 entries.
;;;;
;;;; *TRAVERSAL-STATE-DEPENDENCIES* is narrower than the transitive read set, deliberately,
;;;; and that is the one thing here that could go silently wrong.  A raw-body scan of the
;;;; builders' call graph also reports HAS-LOCATION and ON, which are reached only through
;;;; BASE -- of a location in LOCATION-LEVEL, and of a gate, screen or wall in
;;;; JUMP-BARRIER-TOP-ELEVATION.  None of those object kinds is ever ON anything, held, or
;;;; given a HAS-LOCATION, so every one of those binds fails and the read is vacuous.  That
;;;; is an argument about which types reach BASE, not something any static analysis can see,
;;;; and including the two relations anyway would key the cache on facts that change every
;;;; move and destroy the hit rate.  *TRAVERSAL-CACHE-PARANOID* exists to hold the argument
;;;; to account: see the file's companion note in claude/traversal-caching-plan.md.
;;;;
;;;; Adding a mode, an obstacle kind, or an override that reads a dynamic relation means
;;;; adding that relation here.  Run a full suite under the paranoid special afterwards.

(defvar *traversal-cache-enabled* t
  "Whether TRAVERSAL-SEGMENTS serves cached results.  Set to NIL to compare a run against
   the uncached computation without editing anything.  DEFVAR, not DEFPARAMETER, unlike the
   caches below: a switch the user sets once at the REPL must survive the resplice that each
   later STAGE performs, or it would silently revert partway through a suite run.")


(defvar *traversal-cache-paranoid* nil
  "When true, every cache hit recomputes the value and signals if it differs.  This is the
   check on *TRAVERSAL-STATE-DEPENDENCIES* being complete, and on the returned lists being
   treated as read-only by their callers; a run under it is roughly three times slower.
   DEFVAR for the same reason as *TRAVERSAL-CACHE-ENABLED*, and it matters more here --
   (TEST-TALOS) stages 102 problems, and a DEFPARAMETER would have been reset to NIL by the
   first of them, leaving the whole suite silently unchecked.")


(defparameter *traversal-state-dependencies*
  '(open recording-open      ;-gate / gate.lisp, through GATE-OPEN-FOR-OBJECT
    holding                  ;-holding, through OBSTACLE-CLEAR's screen and ladder arms
    mounted-on               ;-gears-fan, through BLOWER-PRESENT
    turning recording-turning ;-gears-fan / -recorder-wall-gears-shadow, through
                              ;BLOWER-TURNING-FOR-OBJECT
    lethal)                  ;-threat, through SAFE
  "The dynamic relations a traversal builder can read.  Each is commented with the
   technology that owns it and the query that reaches it.  A problem lacking that
   technology simply never stores facts under the relation, so an entry costs nothing.")


(defparameter *traversal-cache-parameters* nil
  "Special variables whose values a registered builder reads, and which therefore belong in
   the cache key -- a mid-session WW-SET of one must not be served a stale segment list.
   Registered by the owning technology, since -TRAVERSAL nests none of them.")


(defparameter *traversal-segment-cache*
  (make-hash-table :test #'equal :synchronized t)
  "Maps a traversal cache key to its segment list.  DEFPARAMETER so the cache empties every
   time this file is respliced for a different problem.  Synchronized unconditionally rather
   than on *THREADS*, because *THREADS* is routinely set at the REPL after staging, by which
   time this table already exists.")


(defparameter *traversal-dependency-key-cache*
  (make-hash-table :test #'eql :synchronized t)
  "Maps an idb storage key to whether its relation is in *TRAVERSAL-STATE-DEPENDENCIES*.
   Classifying a key costs a CONVERT-TO-PROPOSITION, so it is done once per distinct key
   rather than once per state.")


(define-problem-helper register-traversal-cache-parameter (symbol)
  "Declare that a builder reads SYMBOL's value, so the cache key carries it.  A separate
   registrar rather than a fifth argument to REGISTER-TRAVERSAL-MODE: the parameter belongs
   to the technology that reads it, and not every mode has one."
  (pushnew symbol *traversal-cache-parameters* :test #'eq)
  symbol)


(defun traversal-dependency-key-p (key)
  "Whether the idb entry stored under KEY belongs to a relation the builders read.  A
   bijective relation is stored under its two generated index names rather than its own, so
   the name is resolved back through *BIJECTIVE-CANONICAL* first -- without that step
   HOLDING, stored as HOLDING1 and HOLDING2, would never match and the cache would ignore
   what an agent is carrying."
  (multiple-value-bind (cached present) (gethash key *traversal-dependency-key-cache*)
    (if present
      cached
      (setf (gethash key *traversal-dependency-key-cache*)
            (let ((name (first (convert-to-proposition key))))
              (and (member (or (car (gethash name *bijective-canonical*)) name)
                           *traversal-state-dependencies*)
                   t))))))


(defun traversal-cache-key (state agent source)
  "STATE's projection onto the declared dependencies, tagged with AGENT, SOURCE, and every
   registered parameter value.  The projection carries each entry's stored value, not just
   its presence, so a fluent relation discriminates correctly.  Sorting by storage key makes
   the list canonical for EQUAL."
  (let ((projection nil))
    (maphash (lambda (key value)
               (when (traversal-dependency-key-p key)
                 (push (cons key value) projection)))
             (problem-state.idb state))
    (list agent source
          (mapcar #'symbol-value *traversal-cache-parameters*)
          (sort projection #'< :key #'car))))


(defun traversal-segments-value (state agent source)
  "TRAVERSAL-SEGMENTS-FOR-SOURCE's result for STATE, computed once per distinct cache key.
   The list and the segments in it are shared between every caller that keys alike and must
   be treated as read-only; -MOBILITY already COPY-TREEs a segment before extending a route
   with it, and *TRAVERSAL-CACHE-PARANOID* would catch a caller that stopped doing so."
  (if (not *traversal-cache-enabled*)
    (funcall (symbol-function 'traversal-segments-for-source) state agent source)
    (let ((key (traversal-cache-key state agent source)))
      (multiple-value-bind (cached present) (gethash key *traversal-segment-cache*)
        (if present
          (progn
            (when *traversal-cache-paranoid*
              (let ((fresh (funcall (symbol-function 'traversal-segments-for-source)
                                    state agent source)))
                (unless (equal fresh cached)
                  (error "~%Traversal cache returned a stale result.~%~
                          Agent:  ~S~%Source: ~S~%Cached: ~S~%Fresh:  ~S~%~
                          Some state the builders read is missing from ~
                          *TRAVERSAL-STATE-DEPENDENCIES* or *TRAVERSAL-CACHE-PARAMETERS*, ~
                          or a caller mutated a returned segment."
                         agent source cached fresh))))
            cached)
          (setf (gethash key *traversal-segment-cache*)
                (funcall (symbol-function 'traversal-segments-for-source)
                         state agent source)))))))


;;;; SEGMENT PRODUCTION ;;;;


(define-query traversal-segments (?agent agent ?from location)
  ;; The single mobility provider, and the one place the result is cached.  The computation
  ;; lives in TRAVERSAL-SEGMENTS-FOR-SOURCE so that every caller -- the mobility closure,
  ;; ONE-STEP-WALKABLE -- goes through the cache without knowing it exists.
  (traversal-segments-value state ?agent ?from))


(define-query traversal-segments-for-source (?agent agent ?from location)
  ;; Every mode's symmetric and directed edges out of ?FROM, each reduced to at most one
  ;; segment.  The mode is iterated rather than bound because a fluentless key needs it
  ;; ground; four binds per location pair replaces the eight the four separate providers
  ;; used to make.
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
