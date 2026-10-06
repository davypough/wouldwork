;;; Filename: -traversal.lisp

;;; Traversal substrate: one topology relation for every way an agent crosses between two
;;; locations.  A fact states what separates the two locations, and nothing else:
;;;
;;;   (traverse-via  <source> <dnf> <destination>)   symmetric
;;;   (traverse-via> <source> <dnf> <destination>)   directed, source first
;;;
;;; The payload is DNF: () means nothing separates the pair; anything else is a list of
;;; clauses, OR over clauses and AND within one -- the same convention CONTROLS uses.  Every
;;; way to cross between one pair sits in one fact, so a spec reads like its diagram:
;;; whatever is drawn between two locations goes in the brackets.
;;;
;;; The kind of move is inferred per clause, never authored.  Each kind's technology
;;; registers its MARKER types, and a clause naming a marker is of that kind -- a ladder
;;; makes a climb, a staircase a stairway, an edge or a wall a jump -- while a clause naming
;;; no marker is a walk.  A clause naming markers of two kinds is an authoring error.
;;; Gates, screens and gears are companions any kind may carry.  Kind is a function of a
;;; clause's types alone, so the DNF family algebra uses it without a state: a clause
;;; subsumes another only when both are of the same kind, or a derived walking () would
;;; erase an authored jump clause and the support transitions that read it.
;;;
;;; One exception is applied at segment time only.  In a problem whose locations carry bare
;;; levels rather than LOCATION-COORDS> geometry, a walk-kind clause across a level
;;; difference is read as a jump -- what an unnamed jumping edge used to mean.  With
;;; geometry on both endpoints, TRAVERSAL-INIT-CHECK rejects such a clause instead.
;;;
;;; STATIC types (staircase, edge) are separators with no state.  They stay in a segment's
;;; witness, so a printed route shows what it crossed, but TRAVERSAL-CLAUSE-PROFILE removes
;;; them from the clause's MEANS, which is all a builder hands to a clearance test --
;;; OBSTACLE-CLEAR and the jump clearance rules know nothing of them.
;;;
;;; A fact yields at most one grounded segment: the kinds are tried in
;;; *TRAVERSAL-KIND-PREFERENCE* order, cheapest first, and within a kind the family's
;;; clauses in canonical order.  Support transitions, which are not segments, read every
;;; clause of their own kind through TRAVERSAL-FAMILY-KIND-CLAUSES.
;;;
;;; Directionality stays in the name, as everywhere else in this domain: the engine mirrors
;;; a relation whose argument types repeat and whose name does not end in ">", and the
;;; repeated type here is LOCATION.
;;;
;;; REACH-VIA is deliberately not a traversal.  Reaching across a barrier authorizes
;;; manipulation, not movement: REACHABLE is no mobility provider, applies no elevation or
;;; distance test, and its payload means "these gates must be open" rather than "these
;;; obstacles must be passable for the mover".  Folding it in would put a relation that
;;; moves nobody into the relation that moves everybody.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -mobility (the provider registry) alone.  This file calls no obstacle,
;;;               threat or elevation rule of its own -- every one of those lives in a
;;;               kind's builder, and that kind's technology nests what it needs.  The one
;;;               elevation read, for the bare-level jump reading, is made only when jump
;;;               is registered, and jump nests -vertical
;;; PROVIDES:
;;;   relations : (traverse-via location $list location),
;;;               (traverse-via> location $list location)
;;;   queries   : traversal-segments  --  the single mobility provider, cached;
;;;               traversal-segments-for-source  --  the computation behind it;
;;;               traversal-pair-families
;;;   acceptor  : traversal-segment-replayable-p, registered with -mobility for replay
;;;   init      : traversal-init-check
;;;   functions : register-traversal-kind, register-traversal-cache-parameter,
;;;               traversal-clause-profile, traversal-family-kind-clauses, and the
;;;               canonical DNF family algebra the coordinate zone-graph derivation in
;;;               -walkability-coordinates uses

(include-tech -mobility)

(in-package :ww)


(define-static-relations
  (traverse-via location $list location)  ;symmetric; $list = DNF clauses of separators: () direct, else OR over clauses, AND within
  (traverse-via> location $list location))  ;directed, source first, same $list convention


;;;; KIND REGISTRY ;;;;
;;;; A kind's technology registers its builder and the types its clauses may name, so this
;;;; file names no gate, ladder, staircase, edge or elevation rule of its own, and a
;;;; problem including only some of the technologies simply has fewer kinds registered.


(defparameter *traversal-kind-preference* '(walk stairs climb jump)
  "Every traversal kind, in the order a fact's grounded segment prefers them: cheapest
   first.  Registration order is irrelevant; this list alone decides.")


(defparameter *traversal-kinds* nil
  "Registered (KIND BUILDER MARKER-TYPES STATIC-TYPES PERMITTED-TYPES) entries for the
   staged problem.  PERMITTED-TYPES is the union of the markers, the static types and the
   companions.  DEFPARAMETER rather than DEFVAR so the list resets each time a problem is
   respliced and loaded.")


(define-problem-helper register-traversal-kind
    (kind builder marker-types static-types companion-types)
  "Register KIND's segment builder and the object types its clauses may name.  A clause
   naming a member of MARKER-TYPES is of KIND; STATIC-TYPES are stateless separators kept
   in the witness but removed from the means; COMPANION-TYPES may accompany the markers.
   BUILDER is called as (BUILDER state agent source destination clause) and returns a
   normalized (label source witness destination) segment, or NIL when that clause does not
   permit the crossing.  Registering a kind twice, or one outside
   *TRAVERSAL-KIND-PREFERENCE*, is an authoring error rather than a silent overwrite."
  (reject-worker-read-write 'register-traversal-kind)
  (unless (member kind *traversal-kind-preference*)
    (error "Traversal kind must be one of ~S: ~S" *traversal-kind-preference* kind))
  (when (assoc kind *traversal-kinds*)
    (error "Traversal kind is registered more than once: ~S" kind))
  (setf *traversal-kinds*
        (append *traversal-kinds*
                (list (list kind builder marker-types static-types
                            (union marker-types (union static-types companion-types))))))
  kind)


;;;; CLAUSE KINDS ;;;;


(defparameter *traversal-clause-profiles*
  (make-hash-table :test #'equal)
  "Maps a clause to its (KIND MEANS) profile.  Both depend only on the clause's types, so
   each distinct clause is classified once per staged problem.")


(defun traversal-clause-marker-kinds (clause)
  "The registered kinds, in preference order, whose marker types some member of CLAUSE
   belongs to.  WALK registers no markers, so it never appears here."
  (loop for kind in *traversal-kind-preference*
        for entry = (assoc kind *traversal-kinds*)
        when (and entry
                  (some (lambda (item)
                          (init-member-of-any-type-p item (third entry)))
                        clause))
          collect kind))


(define-problem-helper traversal-clause-profile (clause)
  "CLAUSE's (KIND MEANS): KIND by its marker types, WALK when it names none; MEANS the
   clause without its static separators, in the clause's own order.  A clause mixing two
   kinds' markers is rejected by TRAVERSAL-INIT-CHECK before any profile is asked for."
  (multiple-value-bind (profile present) (gethash clause *traversal-clause-profiles*)
    (if present
      profile
      (setf (gethash clause *traversal-clause-profiles*)
            (let ((kinds (traversal-clause-marker-kinds clause))
                  (statics (loop for entry in *traversal-kinds* append (fourth entry))))
              (when (rest kinds)
                (error "Traversal clause ~S names the separators of ~S." clause kinds))
              (list (or (first kinds) 'walk)
                    (remove-if (lambda (item)
                                 (init-member-of-any-type-p item statics))
                               clause)))))))


(define-problem-helper traversal-clause-segment-kind (state source destination clause)
  "CLAUSE's kind for a crossing from SOURCE to DESTINATION: its profile kind, except that a
   walk across a level difference is read as a jump when jump is registered.  Only a
   bare-level problem can reach that exception: with LOCATION-COORDS> on both endpoints
   TRAVERSAL-INIT-CHECK rejects the authored clause, and the coordinate derivation never
   emits one."
  (let ((kind (first (traversal-clause-profile clause))))
    (if (and (eq kind 'walk)
             (assoc 'jump *traversal-kinds*)
             (/= (funcall (symbol-function 'location-elevation) state source)
                 (funcall (symbol-function 'location-elevation) state destination)))
      'jump
      kind)))


(define-problem-helper traversal-family-kind-clauses
    (state source destination family kind)
  "FAMILY's canonical clauses whose segment kind from SOURCE to DESTINATION is KIND.  An
   empty family is the direct case and offers the single empty clause."
  (remove-if-not (lambda (clause)
                   (eq (traversal-clause-segment-kind state source destination clause)
                       kind))
                 (if family
                   (traversal-canonical-family family)
                   (list nil))))


;;;; CANONICAL DNF FAMILY ALGEBRA ;;;;
;;;; A family is an antichain of separator clauses within each kind: OR over clauses, AND
;;;; within each.  Shared by the segment choice below and by -walkability-coordinates' zone
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


(defun traversal-family-add-obstacles (family obstacles)
  ;; Path extension by one compound crossing.  Every member of OBSTACLES belongs to the
  ;; same conjunctive clause; this is distinct from TRAVERSAL-FAMILY-UNION, which joins
  ;; alternative routes.
  (traversal-minimize-family
    (mapcar (lambda (clause) (append obstacles clause)) family)))


(defun traversal-minimize-family (family)
  ;; Canonical clauses, duplicates removed, and every nonminimal superset of the same kind
  ;; discarded.  A subset of another kind subsumes nothing: () walks, (edge2) jumps, and
  ;; both crossings must survive.
  (let* ((clauses (remove-duplicates
                    (mapcar #'traversal-canonical-clause family)
                    :test #'equal))
         (minimal (remove-if (lambda (clause)
                               (some (lambda (other)
                                       (and (not (equal other clause))
                                            (subsetp other clause)
                                            (eq (first (traversal-clause-profile other))
                                                (first (traversal-clause-profile clause)))))
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
    (state agent source destination family)
  "The one grounded segment FAMILY offers from SOURCE to DESTINATION, or NIL: the first
   segment a registered builder accepts, trying kinds in *TRAVERSAL-KIND-PREFERENCE* order
   and each kind's clauses in canonical order."
  (loop for kind in *traversal-kind-preference*
        for entry = (assoc kind *traversal-kinds*)
        for segment = (and entry
                           (loop for clause in (traversal-family-kind-clauses
                                                 state source destination family kind)
                                 for candidate = (funcall (symbol-function (second entry))
                                                          state agent source destination
                                                          clause)
                                 when candidate
                                   return candidate))
        when segment
          return segment))


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
;;;; Adding a kind, a separator type, or an override that reads a dynamic relation means
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
   (TEST-TALOS) stages over a hundred problems, and a DEFPARAMETER would have been reset to
   NIL by the first of them, leaving the whole suite silently unchecked.")


(defparameter *traversal-state-dependencies*
  '(open recording-open      ;-gate / gate.lisp, through GATE-OPEN-FOR-OBJECT
    holding                  ;-holding, through OBSTACLE-CLEAR's screen and ladder arms
    mounted-on               ;-gears-fan, through BLOWER-PRESENT
    turning recording-turning ;-gears-fan / -recorder-blower-shadow, through
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
   registrar rather than a sixth argument to REGISTER-TRAVERSAL-KIND: the parameter belongs
   to the technology that reads it, and not every kind has one."
  (reject-worker-read-write 'register-traversal-cache-parameter)
  (register-worker-read-configuration symbol)
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
  ;; The symmetric and directed facts out of ?FROM, each reduced to at most one segment.
  ;; Two binds per location pair, where the mode loop this replaces made eight.
  (do (assign $segments nil)
      (doall (?to location)
        (do (assign $symmetric nil)
            (assign $directed nil)
            (if (bind (traverse-via ?from $symmetric-family ?to))
              (assign $symmetric
                      (traversal-segment-for-family
                        state ?agent ?from ?to $symmetric-family)))
            (if (bind (traverse-via> ?from $directed-family ?to))
              (assign $directed
                      (traversal-segment-for-family
                        state ?agent ?from ?to $directed-family)))
            (if $symmetric
              (assign $segments (cons $symmetric $segments)))
            (if $directed
              (assign $segments (cons $directed $segments)))))
      $segments))


(register-mobility-provider 'traversal-segments)


;;;; REPLAY ACCEPTANCE ;;;;
;;;; A fact yields one grounded segment in search, but a replayed MOVE may cross by any
;;;; clause of the pair's fact that succeeds in that state (decision D6): a hand-written
;;;; (jump location13 (edge3) location17) is legal even though search would take the stairs.


(define-problem-helper traversal-segment-replayable-p (state agent source segment)
  "True when some clause of a traversal fact from SOURCE to SEGMENT's destination, run
   through the builder of its own kind, produces exactly SEGMENT -- the same label and the
   same witness.  A clause whose kind no technology registered offers nothing."
  (let ((destination (fourth segment)))
    (loop for family in (funcall (symbol-function 'traversal-pair-families)
                                 state source destination)
          thereis (loop for clause in (if family
                                        (traversal-canonical-family family)
                                        (list nil))
                        for entry = (assoc (traversal-clause-segment-kind
                                             state source destination clause)
                                           *traversal-kinds*)
                        thereis (and entry
                                     (equal segment
                                            (funcall (symbol-function (second entry))
                                                     state agent source destination
                                                     clause)))))))


(define-query traversal-pair-families (?from location ?to location)
  ;; The families of the symmetric and directed facts from ?FROM to ?TO, one entry per fact
  ;; present.  A fact whose family is () contributes NIL, which the caller reads as the
  ;; single empty clause.
  (do (assign $families nil)
      (if (bind (traverse-via ?from $symmetric-family ?to))
        (push $symmetric-family $families))
      (if (bind (traverse-via> ?from $directed-family ?to))
        (push $directed-family $families))
      $families))


(register-mobility-replay-acceptor 'traversal-segment-replayable-p)


;;;; INITIALIZATION VALIDATION ;;;;


(define-init-check traversal-init-check (literals)
  (:consumes gate screen ladder wall edge staircase gears
             floor-gears wall-gears angled-gears
             floor-blower wall-blower angled-blower)
  (check-init-traversal-endpoints literals)
  (check-init-traversal-payloads literals)
  (check-init-traversal-duplicates literals)
  (check-init-traversal-levels literals))


(define-init-check-helper check-init-traversal-endpoints (literals)
  "Reject positive traversal self-loops.  Mobility is already reflexive at every location,
   so such a fact can add no route and would otherwise disappear silently in the visited
   set of the closure."
  (dolist (relation '(traverse-via traverse-via>))
    (dolist (literal (positive-init-literals-with-relation relation literals))
      (destructuring-bind (source family destination)
          (rest (init-literal-proposition literal))
        (declare (ignore family))
        (when (eql source destination)
          (fail-init-check literal
            "Traversal source and destination are the same location: ~S.  Mobility is already reflexive; remove the self-loop or correct an endpoint."
            source))))))


(define-init-check-helper check-init-traversal-payloads (literals)
  "Every traversal payload is DNF over the separator types some registered kind permits,
   and each clause is checked against its own kind by CHECK-INIT-TRAVERSAL-CLAUSE."
  (let ((permitted (remove-duplicates
                     (loop for entry in *traversal-kinds* append (fifth entry)))))
    (dolist (relation '(traverse-via traverse-via>))
      (dolist (literal (init-literals-with-relation relation literals))
        (let ((family (third (init-literal-proposition literal))))
          (init-check-dnf-list-items-have-types literal family permitted)
          (dolist (clause (or family (list nil)))
            (check-init-traversal-clause literal clause)))))))


(define-init-check-helper check-init-traversal-clause (literal clause)
  "One clause is one way across, so it may name the markers of one kind only, and every
   member must be a type that kind permits -- a wall is vaultable in a jump but means
   nothing beside a staircase.  A walk-kind clause needs walking, or jump for the
   bare-level reading, to be registered."
  (let* ((kinds (traversal-clause-marker-kinds clause))
         (entry (assoc (or (first kinds) 'walk) *traversal-kinds*)))
    (when (rest kinds)
      (fail-init-check literal
        "Clause ~S mixes the separators of ~{~(~A~)~^ and ~} crossings.  One clause is one way across; give each its own alternative clause."
        clause kinds))
    (if entry
      (init-check-list-items-have-types literal clause (fifth entry))
      (unless (assoc 'jump *traversal-kinds*)
        (fail-init-check literal
          "Clause ~S is a walk, but no included technology registers walking.  Include walkability, or name the staircase, edge or ladder that makes it another kind."
          clause)))))


(define-init-check-helper check-init-traversal-duplicates (literals)
  "Each location pair is authored at most once per relation, holding every way across in
   one family.  A symmetric pair is the same pair in either order.  Merging silently would
   hide an authoring slip, so the rejection shows the family to write instead."
  (dolist (relation '(traverse-via traverse-via>))
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (literal (positive-init-literals-with-relation relation literals))
        (destructuring-bind (source family destination)
            (rest (init-literal-proposition literal))
          (let* ((key (if (and (eq relation 'traverse-via)
                               (string< (symbol-name destination) (symbol-name source)))
                        (list destination source)
                        (list source destination)))
                 (prior (gethash key seen)))
            (when prior
              (fail-init-check literal
                "Traversal pair ~S ~S is authored twice under ~S.  Author it once, with the combined family ~S.~%First literal: ~S"
                source destination relation
                (traversal-normalize-family
                  (traversal-minimize-family
                    (append (or (third (init-literal-proposition prior)) (list nil))
                            (or family (list nil)))))
                prior))
            (setf (gethash key seen) literal)))))))


(define-init-check-helper check-init-traversal-levels (literals)
  "A walk-kind clause keeps its endpoints on one level.  When both endpoints take their
   level from LOCATION-COORDS> and the levels differ, the clause must name the staircase,
   edge or ladder that separates them.  Without coordinates on both ends the clause is left
   to the bare-level jump reading of TRAVERSAL-CLAUSE-SEGMENT-KIND."
  (let ((levels (init-literal-map 'location-coords> literals 1 4)))
    (dolist (relation '(traverse-via traverse-via>))
      (dolist (literal (positive-init-literals-with-relation relation literals))
        (destructuring-bind (source family destination)
            (rest (init-literal-proposition literal))
          (multiple-value-bind (source-level source-present) (gethash source levels)
            (multiple-value-bind (destination-level destination-present)
                (gethash destination levels)
              (when (and source-present
                         destination-present
                         (/= source-level destination-level))
                (dolist (clause (or family (list nil)))
                  (unless (traversal-clause-marker-kinds clause)
                    (fail-init-check literal
                      "Walk clause ~S joins ~S at level ~S to ~S at level ~S.  Name the staircase, edge or ladder that separates them."
                      clause source source-level destination destination-level)))))))))))


(register-worker-read-memo '*traversal-canonical-families* :empty-table)
(register-worker-read-memo '*traversal-clause-profiles* :empty-table)
(register-worker-read-memo '*traversal-dependency-key-cache* :empty-table)
(register-worker-read-memo '*traversal-segment-cache* :empty-table)
(register-worker-read-configuration '*traversal-state-dependencies*
                                    '*traversal-cache-parameters* '*traversal-kinds*)
