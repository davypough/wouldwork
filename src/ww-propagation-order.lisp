;;; Filename: ww-propagation-order.lisp

;;; Init-time static analysis of a problem's hand-authored MASTER PROPAGATION DRIVER,
;;; plus two related coverage checks: propagation-order violations, inert object types,
;;; and inert technologies.
;;;
;;; Exactly one finding is fatal -- a reaction violation, which halts INIT.  It names a
;;; defect no later pass can repair, so admitting it would trade an accurate error here
;;; for a mystifying one later, or for a plan built on a state that silently diverged
;;; from the model.  Everything else prints and continues, being either a convergence
;;; cost or advice.  The distinction was calibrated against CLAUSTRO-TOPO, CORNER-TOPO,
;;; and PHOBIA, all of which report clean, and confirmed by relocating
;;; UPDATE-GEARS-STATUS! below the blowers in PHOBIA, which produces exactly the two
;;; expected reaction violations and nothing else.
;;;
;;; Why this runs at init rather than at install time.  -BEAM-SUBSTRATE installs
;;; null-object defaults for BEAM-CUT, CURRENT-CROSSING-SET, COMPUTE-CONNECTOR-LIGHTING,
;;; and RELAY-BEAM-REACHES-RECEIVER, whose bodies read nothing; BEAM-CROSSING and
;;; BEAM-RELAY then reinstall those same names with real bodies, and INSTALL-QUERY
;;; overwrites :RAW-BODY.  Only a single end-of-load pass sees the overriding
;;; definitions.  An incremental analysis would derive an empty graph.
;;;
;;; A consequence: the graph is per-problem, not per-tech.  The same
;;; UPDATE-RECEIVER-STATUS! has a different read set depending on which beam peers are
;;; loaded, so nothing here can be cached per tech file.
;;;
;;; What an ordering violation does and does not mean.  The dependency graph is
;;; genuinely cyclic -- gate and receiver depend on each other through OPEN and ACTIVE --
;;; so no topological order exists and correctness comes from PROPAGATE-CHANGES!'s
;;; fixpoint, not from the authored sequence.  Order matters two different ways, and the
;;; report separates them:
;;;   - a consumer that is a pure DERIVATION matters only across a component boundary,
;;;     and only as convergence speed.  Reading a value the driver computes later costs
;;;     the fixpoint an extra pass and nothing else.  Inside a component the cycle
;;;     guarantees recomputation, so nothing is even worth saying.
;;;   - a consumer that is a REACTION -- an update writing a base fact that ordinary
;;;     actions also write, as PHOBIA's blowers write HAS-LOCATION and ON through
;;;     RELOCATE-STACK! -- matters whether or not it shares a component with its
;;;     producer.  It acts on the stale value irreversibly, and no later pass can retract
;;;     a side effect.  A component-boundary test alone would say nothing about PHOBIA at
;;;     all: HAS-LOCATION couples the blowers to everything they read, collapsing all six
;;;     updates into one component, so the ordering FLOOR-BLOWER's header requires --
;;;     after UPDATE-GEARS-STATUS!, whose derived BLOWING the blowers act on -- would go
;;;     unchecked precisely where it is load-bearing.
;;; The reaction rule is narrowed to a derivation producer.  A reaction reading another
;;; reaction's base-fact write is mutual rather than misordered: both orders are equally
;;; defensible, the analysis has no better sequence to propose, and reporting it would
;;; bury the case that does have a right answer.
;;; An update with its own interior fixpoint, such as
;;; UPDATE-CROSSING-STATUS!, needs no special handling; its reads and writes are computed
;;; normally, and the only claim made is that nothing may reorder across its interior.
;;;
;;; Read/write contexts follow WW-TRANSLATOR's actual FLAG and *PROPOSITION-READ-MODE*
;;; behavior rather than an idealized reading of it.  Notably AND, OR, NOT and the
;;; quantifiers preserve the surrounding mode -- TRANSLATE-CONNECTIVE and
;;; TRANSLATE-EXISTENTIAL both do -- so they are not treated as forcing read context.
;;; What does force read context: IF/COND/CASE tests, BIND, EQUIVALENT, PRINT, the value
;;; position of ASSIGN and MV-ASSIGN, LET initial values, ordinary Lisp call arguments,
;;; and any call to a query.
;;;
;;; Only dynamic relations are tracked.  A static relation is immutable, so reading one
;;; can never create an ordering dependency, and including the implied unary type
;;; predicates would bury the real edges in noise.
;;;
;;; The walker reads a :RAW-BODY, but the edges it claims are about compiled code, so it
;;; must prune wherever TRANSLATE prunes.  -CONTROLS' ENERGIZED is
;;; (or (and (receiver ?c) (active ?c)) (and (plate ?c) (depressed ?c))), and in a problem
;;; with no plates TRANSLATE-SIMPLIFIED-CONNECTIVE drops the second disjunct outright, so
;;; the installed query never touches DEPRESSED.  Recording it anyway invents an edge,
;;; which either merges two components -- hiding a real violation -- or supplies a
;;; backward edge that was never there.  The three pruning sites are mirrored separately
;;; because TRANSLATE gates them differently:
;;;   - AND, OR and NOT are simplified only when *FORMULA-CONTEXT-P* holds and
;;;     STATIC-READ-CONTEXT-P holds, so the walker prunes them only in :READ mode and only
;;;     with *PROPAGATION-FORMULA-CONTEXT-P* live.
;;;   - TRANSLATE-CONDITIONAL, TRANSLATE-COND and TRANSLATE-CASE decide a test statically
;;;     under either FLAG, so the walker prunes conditionals in both modes.
;;;   - TRANSLATE-EXISTENTIAL, TRANSLATE-UNIVERSAL and TRANSLATE-DOALL all reach
;;;     TRANSLATE-EMPTY-STATIC-QUANTIFIER under either FLAG, so an empty static domain
;;;     suppresses the body in both modes.
;;; STATIC-FORM-TRUTH, EMPTY-STATIC-TYPE-PREDICATE-P and STATIC-EMPTY-QUANTIFIER-TRUTH are
;;; called rather than reimplemented; a second copy of the emptiness rules would drift
;;; from the translator, which is the one thing this analysis cannot afford.
;;;
;;; Pruning where TRANSLATE does not is the damaging direction, since it discards a read
;;; the compiled code really performs.  *FORMULA-CONTEXT-P* goes NIL for a whole subtree
;;; under a LET binding value, an ASSIGN or MV-ASSIGN value, and an ordinary Lisp call
;;; argument, and never comes back -- -GEARS-FAN's UPDATE-GEARS-STATUS! puts a WW-LOOP
;;; full of connectives inside (ASSIGN $ANY-CLAUSE-ON ...), where nothing is simplified.
;;; *PROPAGATION-FORMULA-CONTEXT-P* tracks that, so the walker's connective pruning stays
;;; coextensive with the translator's.  Where a verdict cannot be reached -- most often
;;; because STATIC-EMPTY-QUANTIFIER-TRUTH's HANDLER-CASE answers :UNKNOWN for a domain the
;;; walker cannot evaluate without the translator's *VAR-TYPE-ENV* -- nothing is pruned,
;;; which errs toward the recoverable phantom read rather than the lost edge.


(in-package :ww)


(defparameter *propagation-formula-context-p* t
  "The walker's counterpart to WW-TRANSLATOR's *FORMULA-CONTEXT-P*.  NIL inside a value
   position, where TRANSLATE routes AND, OR and NOT to TRANSLATE-BODY-FORM and simplifies
   nothing, so the walker must record every read those connectives contain.")


(defun report-propagation-diagnostics ()
  "Runs the four init-time checks in sequence.  Called from INIT before INIT-START-STATE
   and therefore before DO-INIT-ACTION-UPDATES, so a bad propagation order is reported as
   a bad propagation order rather than surfacing later as an unexplained
   INCONSISTENT-STATE.  A reaction violation signals an error here; the inert-type,
   inert-tech and driver-caller checks never do, so they run after the order check and only
   report."
  (report-propagation-order)
  (report-inert-types)
  (report-inert-techs)
  (report-driver-update-callers))


;;;; PROPAGATION ORDER ;;;;


(defparameter *propagation-driver-sentinel* '(propagation-driver-not-derived)
  "The body tech/-propagation.lisp installs for PROPAGATE-CONSEQUENCES!, standing in for a
   driver not yet derived.  Kept here rather than in the tech file because every consumer
   of the distinction lives in this file; the tech file's body must match it literally.")


(defun propagation-driver-not-derived ()
  "Called only from the spliced PROPAGATE-CONSEQUENCES! sentinel, which INIT should have
   replaced with the derived call sequence.  Signals rather than returning NIL: a driver
   that propagates nothing produces an unpropagated start state, and every symptom of that
   appears far from the cause."
  (error "~2%PROPAGATE-CONSEQUENCES! was called while still holding ~
          tech/-propagation.lisp's sentinel body.  No propagation order was derived and ~
          the problem authored no driver of its own, so nothing would have been ~
          propagated.~2%"))


(defun authored-propagation-driver-body ()
  "PROPAGATE-CONSEQUENCES!'s :RAW-BODY when the current problem authored a driver, else NIL.

   Two ways to be no driver of this problem's.  Membership in *UPDATE-NAMES*, which the
   per-problem reset clears, rules out a body left on the symbol plist by a previous
   problem -- :RAW-BODY survives a reload into a problem that defines no driver.  The
   sentinel test rules out tech/-propagation.lisp's placeholder, which every technology
   contributing a driver update now nests, so membership alone no longer discriminates:
   without this test a driverless tech-using problem would have the placeholder analyzed as
   though the author had written it."
  (let ((raw-body (and (member 'propagate-consequences! *update-names*)
                       (get 'propagate-consequences! :raw-body))))
    (and raw-body
         (not (equal raw-body *propagation-driver-sentinel*))
         raw-body)))


(defun report-propagation-order ()
  "Analyzes the authored order in PROPAGATE-CONSEQUENCES!, if the problem has one.
   Most problems define no driver at all; those are skipped silently.  See
   AUTHORED-PROPAGATION-DRIVER-BODY for what establishes that a driver is this problem's."
  (let ((raw-body (authored-propagation-driver-body)))
    (when raw-body
      (let ((order (authored-propagation-order raw-body)))
        (when (rest order)
          (report-propagation-order-findings order))))))


(defun authored-propagation-order (raw-body)
  "The update functions PROPAGATE-CONSEQUENCES! calls, in order of first appearance."
  (nreverse (collect-propagation-order-names raw-body nil)))


(defun collect-propagation-order-names (form accumulated)
  "Cons-tree walk of FORM returning ACCUMULATED extended with every propagation update
   name encountered, most recent first.  A name counts in head position, (UPDATE-X!), or
   behind a function reference, #'UPDATE-X! -- the FUNCALL-over-a-LIST idiom the older
   problems use.  A bare symbol anywhere else does not count.

   A DEFINE- form is skipped whole.  The walk descends the CDR generically, so the name
   a definition form declares sits in head position of that CDR and would be collected as
   though the driver called it: a (DEFINE-UPDATE UPDATE-PLATE-STATUS! () ...) spliced into
   the driver body yields a propagation order containing UPDATE-PLATE-STATUS!.  That input
   is malformed, but the order it produces is silently wrong rather than obviously so."
  (cond ((atom form)
         accumulated)
        ((eq (car form) 'quote)
         accumulated)
        ((and (symbolp (car form))
              (eql 0 (search "DEFINE-" (symbol-name (car form)))))
         accumulated)
        ((and (eq (car form) 'function)
              (symbolp (second form))
              (propagation-candidate-p (second form)))
         (if (member (second form) accumulated)
           accumulated
           (cons (second form) accumulated)))
        ((and (symbolp (car form))
              (propagation-candidate-p (car form)))
         (collect-propagation-order-names (cdr form)
                                          (if (member (car form) accumulated)
                                            accumulated
                                            (cons (car form) accumulated))))
        (t
         (collect-propagation-order-names
           (cdr form)
           (collect-propagation-order-names (car form) accumulated)))))


(defun propagation-candidate-p (name)
  "True for an update function eligible to appear in the propagation order: any user
   update except the two driver functions themselves."
  (and (member name *update-names*)
       (not (member name '(propagate-changes! propagate-consequences!)))
       t))


(defun report-propagation-order-findings (order)
  "Builds the dependency graph over ORDER and prints any violations."
  (multiple-value-bind (reads writes base-facts adjacency components)
      (propagation-graph order)
    (report-propagation-violations
      (propagation-order-violations order adjacency components reads writes base-facts)
      order base-facts)))


(defun propagation-graph (order)
  "The whole static picture of ORDER, as five values parallel to it: each update's read
   set, its write set, the base facts it writes, the adjacency matrix, and the
   strongly-connected-component id of each position.

   One walk per update, so the caller pays for the walker once however many of the five it
   uses.  Every analysis in this file starts here -- the authored-order check, the
   derivation strata, and the Phase 3 derivation -- which is the point: three callers
   computing this separately is three chances for them to disagree about what the graph
   is."
  (let* ((size (length order))
         (reads (make-array size))
         (writes (make-array size))
         (base (action-base-fact-set))
         (base-facts (make-array size)))
    (loop for name in order
          for index from 0
          do (multiple-value-bind (update-reads update-writes)
                 (propagation-relation-sets name)
               (setf (aref reads index) update-reads)
               (setf (aref writes index) update-writes)))
    (dotimes (index size)
      (setf (aref base-facts index)
            (propagation-base-facts-written (aref writes index) base)))
    (let ((adjacency (propagation-adjacency-matrix reads writes)))
      (values reads writes base-facts adjacency
              (propagation-component-ids adjacency)))))


(defun propagation-relation-sets (name)
  "Returns two EQ hash tables for update NAME: the dynamic relations its body reads, and
   those it writes.  One walk suffices; an update body starts in effect context, so the
   walk starts in :WRITE mode and the forced-read contexts inside it do the rest."
  (let ((reads (make-hash-table :test #'eq))
        (writes (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'equal)))
    (walk-propagation-form (get name :raw-body) :write reads writes visited)
    (values reads writes)))


(defun walk-propagation-form (form mode reads writes visited)
  "Records into READS and WRITES every dynamic relation FORM touches, under the read or
   write context each subform occupies.  The decisive difference from
   EXTRACT-EFFECT-MODIFIED-RELATIONS: a relation symbol counts only in head position of
   a form, never as a bare symbol somewhere in the tree.  That function's bare-symbol
   collection is what makes it too coarse to distinguish a read from a write.

   The connective clause discards a form TRANSLATE-SIMPLIFIED-CONNECTIVE would delete.
   Its guards reproduce the two the translator applies: *PROPAGATION-FORMULA-CONTEXT-P*
   for *FORMULA-CONTEXT-P*, and :READ mode for STATIC-READ-CONTEXT-P.  Testing the whole
   form covers both of the translator's cases -- an operand it removes has a decided truth
   of its own and is reached by recursion, while an AND collapsed by a false operand or an
   OR collapsed by a true one is precisely a form whose own truth is decided.

   ASSIGN, MV-ASSIGN, LET binding values, and the ordinary Lisp call at the end are the
   positions where TRANSLATE binds *FORMULA-CONTEXT-P* to NIL, and it never restores it,
   so the binding covers the whole subtree beneath them."
  (cond
    ((atom form)
     nil)
    ((member (car form) '(quote declare))
     nil)
    ((and *propagation-formula-context-p*
          (eq mode :read)
          (member (car form) *connectives*)
          (member (static-form-truth form) '(:true :false)))
     nil)
    ((gethash (car form) *relations*)
     (if (eq mode :write)
       (setf (gethash (car form) writes) t)
       (setf (gethash (car form) reads) t)))
    ((eq (car form) 'not)
     (walk-propagation-form (second form) mode reads writes visited))
    ((eq (car form) 'if)
     (walk-propagation-if form mode reads writes visited))
    ((eq (car form) 'cond)
     (walk-propagation-cond form mode reads writes visited))
    ((eq (car form) 'case)
     (walk-propagation-case form mode reads writes visited))
    ((eq (car form) 'ww-loop)
     (walk-propagation-loop form mode reads writes visited))
    ((member (car form) '(bind equivalent print))
     (walk-propagation-arguments (rest form) :read reads writes visited))
    ((member (car form) '(assign mv-assign))
     (let ((*propagation-formula-context-p* nil))
       (walk-propagation-form (third form) :read reads writes visited)))
    ((member (car form) '(assert finally next))
     (walk-propagation-arguments (rest form) :write reads writes visited))
    ((member (car form) '(exists exist forsome forall forevery doall))
     (unless (member (static-empty-quantifier-truth form) '(:true :false))
       (walk-propagation-form (third form) mode reads writes visited)))
    ((member (car form) '(let let*))
     (let ((*propagation-formula-context-p* nil))
       (walk-propagation-arguments (mapcar #'second (remove-if #'atom (second form)))
                                   :read reads writes visited))
     (walk-propagation-arguments (cddr form) mode reads writes visited))
    ((member (car form) '(do progn and or))
     (walk-propagation-arguments (rest form) mode reads writes visited))
    ((member (car form) *query-names*)
     (walk-propagation-arguments (rest form) :read reads writes visited)
     (walk-propagation-call (car form) :read reads writes visited))
    ((member (car form) *update-names*)
     (walk-propagation-arguments (rest form) :read reads writes visited)
     (walk-propagation-call (car form) mode reads writes visited))
    (t
     (let ((*propagation-formula-context-p* nil))
       (walk-propagation-arguments (rest form) :read reads writes visited)))))


(defun walk-propagation-if (form mode reads writes visited)
  "IF, pruned as TRANSLATE-CONDITIONAL prunes it.  A test STATIC-FORM-TRUTH decides is
   never emitted, and only the surviving branch is translated, so neither the test nor the
   unreachable branch may contribute a read.  An undecided test is evaluated in read
   context, mirroring the translator's forced *PROPOSITION-READ-MODE*, and both branches
   inherit MODE.  No mode guard here: TRANSLATE-CONDITIONAL consults STATIC-FORM-TRUTH
   under either FLAG."
  (case (static-form-truth (second form))
    (:true
      (walk-propagation-form (third form) mode reads writes visited))
    (:false
      (walk-propagation-form (fourth form) mode reads writes visited))
    (otherwise
      (walk-propagation-form (second form) :read reads writes visited)
      (walk-propagation-form (third form) mode reads writes visited)
      (walk-propagation-form (fourth form) mode reads writes visited))))


(defun walk-propagation-cond (form mode reads writes visited)
  "COND, pruned as TRANSLATE-COND prunes it.  That function rewrites the clauses into
   nested IFs and hands each to TRANSLATE-CONDITIONAL, so a statically false clause
   vanishes entirely and a statically true one -- (T ...) among them -- discards every
   clause after it."
  (dolist (clause (rest form))
    (case (static-form-truth (first clause))
      (:false)
      (:true
        (walk-propagation-arguments (rest clause) mode reads writes visited)
        (return))
      (otherwise
        (walk-propagation-form (first clause) :read reads writes visited)
        (walk-propagation-arguments (rest clause) mode reads writes visited)))))


(defun walk-propagation-case (form mode reads writes visited)
  "CASE, pruned as TRANSLATE-CASE prunes it.  A key STATIC-LITERAL-VALUE resolves selects
   one clause and deletes the rest along with the key expression itself; anything else
   leaves the whole form standing."
  (multiple-value-bind (key knownp) (static-literal-value (second form))
    (if knownp
      (multiple-value-bind (clause foundp) (static-case-selected-clause key (cddr form))
        (when foundp
          (walk-propagation-arguments (rest clause) mode reads writes visited)))
      (progn
        (walk-propagation-form (second form) :read reads writes visited)
        (dolist (clause (cddr form))
          (walk-propagation-arguments (rest clause) mode reads writes visited))))))


(defun walk-propagation-loop (form mode reads writes visited)
  "WW-LOOP.  Operands of the iteration and termination clauses -- FOR ... IN, ALWAYS,
   THEREIS, WHILE, UNTIL, REPEAT -- are read.  Operands of DO, COLLECT, FINALLY,
   INITIALLY, RETURN and the conditional clauses inherit MODE."
  (let ((clause-mode :read))
    (dolist (item (rest form))
      (cond ((member item '(do doing collect collecting append appending nconc
                            finally initially return when unless if else))
             (setf clause-mode mode))
            ((symbolp item)
             (setf clause-mode :read))
            (t
             (walk-propagation-form item clause-mode reads writes visited))))))


(defun walk-propagation-arguments (forms mode reads writes visited)
  "Walks each form of FORMS in MODE."
  (dolist (form forms)
    (walk-propagation-form form mode reads writes visited)))


(defun walk-propagation-call (name mode reads writes visited)
  "Recurses into NAME's stored raw body, once per (NAME . MODE) pair so mutually
   recursive updates terminate.  A query is always entered in :READ mode by its caller,
   since a query never writes; an update inherits the caller's mode, which is what
   carries RELOCATE-STACK!'s and SWEEP-OCCUPANTS-AWAY!'s writes up to the propagation
   update that invoked them."
  (let ((key (cons name mode)))
    (unless (gethash key visited)
      (setf (gethash key visited) t)
      (walk-propagation-form (get name :raw-body) mode reads writes visited))))


(defun action-base-fact-set ()
  "The dynamic relations ordinary actions write, as an EQ hash table.  Assembled from
   ACTION.EFFECT-ADDS, which CREATE-ACTION already populates -- via the coarse
   bare-symbol walker, so this set may be over-broad.  The summary prints the base facts
   each reaction writes, which is where an over-broad set would show up first."
  (let ((base (make-hash-table :test #'eq)))
    (dolist (action *actions* base)
      (dolist (relation (action.effect-adds action))
        (setf (gethash relation base) t)))))


(defun propagation-base-facts-written (update-writes base)
  "The base facts an update writes.  Non-empty makes the update a REACTION, whose
   position in the driver is semantically significant rather than merely a matter of
   convergence; empty makes it a pure DERIVATION."
  (let ((shared nil))
    (maphash (lambda (relation present)
               (declare (ignore present))
               (when (gethash relation base)
                 (push relation shared)))
             update-writes)
    (sort shared #'string< :key #'symbol-name)))


(defun propagation-adjacency-matrix (reads writes)
  "Bit matrix carrying a 1 at [i][j] when update i writes a relation update j reads."
  (let* ((size (length reads))
         (matrix (make-array (list size size) :element-type 'bit :initial-element 0)))
    (dotimes (i size matrix)
      (dotimes (j size)
        (unless (= i j)
          (maphash (lambda (relation present)
                     (declare (ignore present))
                     (when (gethash relation (aref reads j))
                       (setf (aref matrix i j) 1)))
                   (aref writes i)))))))


(defun propagation-component-ids (adjacency)
  "Vector mapping each update index to its strongly-connected-component id.  Computed by
   boolean reachability closure over the adjacency matrix, which at this scale -- around
   ten nodes -- costs nothing and avoids Tarjan's recursion, which would want LABELS."
  (let* ((size (array-dimension adjacency 0))
         (closure (make-array (list size size) :element-type 'bit))
         (ids (make-array size :initial-element nil))
         (next-id 0))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref closure i j) (aref adjacency i j))))
    (dotimes (k size)
      (dotimes (i size)
        (dotimes (j size)
          (when (and (= 1 (aref closure i k))
                     (= 1 (aref closure k j)))
            (setf (aref closure i j) 1)))))
    (dotimes (i size ids)
      (unless (aref ids i)
        (setf (aref ids i) next-id)
        (loop for j from (1+ i) below size
              when (and (= 1 (aref closure i j))
                        (= 1 (aref closure j i)))
                do (setf (aref ids j) next-id))
        (incf next-id)))))


(defun propagation-order-violations (order adjacency components reads writes base-facts)
  "Every edge producer -> consumer whose consumer is authored first.  Two rules, in
   priority order:

   A consumer that is a REACTION reading from a DERIVATION producer is reported whatever
   the components, because it acts irreversibly on a value the driver has not computed
   yet.  Restricting this to a derivation producer is deliberate -- see the file header.

   Any other consumer is reported only across a component boundary, where it costs the
   fixpoint a pass; inside a component the cycle already guarantees recomputation.

   Each violation records the producer, the consumer, the relations carrying the edge,
   and the base facts the CONSUMER writes -- empty for a convergence-only violation, and
   what makes the other kind semantic."
  (let ((size (length order))
        (violations nil))
    (dotimes (i size (nreverse violations))
      (dotimes (j size)
        (when (and (= 1 (aref adjacency i j))
                   (< j i))
          (cond ((and (aref base-facts j)
                      (null (aref base-facts i)))
                 (push (list (nth i order)
                             (nth j order)
                             (propagation-edge-relations (aref writes i) (aref reads j))
                             (aref base-facts j))
                       violations))
                ((/= (aref components i) (aref components j))
                 (push (list (nth i order)
                             (nth j order)
                             (propagation-edge-relations (aref writes i) (aref reads j))
                             nil)
                       violations))))))))


(defun propagation-edge-relations (producer-writes consumer-reads)
  "The relations the producer writes and the consumer reads, sorted for stable output."
  (let ((shared nil))
    (maphash (lambda (relation present)
               (declare (ignore present))
               (when (gethash relation consumer-reads)
                 (push relation shared)))
             producer-writes)
    (sort shared #'string< :key #'symbol-name)))


(defun report-propagation-violations (violations order base-facts)
  "Prints the convergence-only violations, then signals an error naming every reaction
   violation and giving a sequence that would satisfy them.  A derivation violation never
   errors: reading a value the driver computes later costs the fixpoint a pass and
   converges to the same state, so there is nothing to fail.  A reaction violation halts
   INIT, because the side effect it commits from stale state is exactly what no later
   pass can undo."
  (let ((reactions (remove-if-not #'fourth violations))
        (derivations (remove-if #'fourth violations)))
    (when derivations
      (format t "~&  Note: order violations among pure derivations.  These cost the ~
                 fixpoint an extra pass and are not incorrect:~%")
      (dolist (violation derivations)
        (format t "~&    ~A reads ~{~A~^ ~} from ~A, which the driver calls later.~%"
                (second violation) (third violation) (first violation))))
    (when reactions
      (error "~%Propagation order error in PROPAGATE-CONSEQUENCES!.~2%~
              ~{  ~A~%~}~%~
              A reaction writes base facts that ordinary actions also write, so no later ~
              pass of the fixpoint can retract what it did.  Acting on a derived value ~
              the driver has not computed yet therefore commits an irreversible change ~
              from stale state.~2%~
              This body satisfies every constraint of that kind, keeping your relative ~
              order within each group:~2%~
              ~{    (~(~A~))~%~}~%~
              Every derivation precedes every reaction, which discharges all of them at ~
              once.  It is a valid sequence rather than the smallest possible edit, so ~
              a narrower rearrangement may also do."
             (mapcar #'propagation-violation-description reactions)
             (propagation-repair-order order base-facts)))))


(defun propagation-repair-order (order base-facts)
  "A sequence satisfying every reaction constraint, built by stably partitioning ORDER
   into its derivations followed by its reactions.  Valid by construction: the only hard
   constraint is that a derivation producing a value precede the reaction consuming it,
   and putting every derivation ahead of every reaction discharges all of them at once.
   No constraint runs the other way, so the partition can never fail.

   Off the derivation path since Phase 3.  DERIVED-PROPAGATION-ORDER separates derivations
   from reactions itself, so the only caller left is REPORT-PROPAGATION-VIOLATIONS, which
   uses this to print a working body alongside the error -- advice to whoever authored the
   driver, in the shrinking set of problems that still author one.

   Stability is what makes this a repair rather than a rewrite.  Relative order survives
   within each group, so the suggestion differs from what the author wrote only where it
   had to -- for PHOBIA with UPDATE-GEARS-STATUS! misplaced below the blowers, it
   reconstructs precisely the order PHOBIA actually ships."
  (append (loop for name in order
                for index from 0
                unless (aref base-facts index)
                  collect name)
          (loop for name in order
                for index from 0
                when (aref base-facts index)
                  collect name)))


(defun propagation-violation-description (violation)
  "One line naming a reaction violation's consumer, the relations it reads too early, the
   producer that supplies them, and the base facts it writes in response."
  (format nil "~A reads ~{~A~^ ~} from ~A, which the driver calls later, and writes ~
               ~{~A~^ ~} in response."
          (second violation) (third violation) (first violation) (fourth violation)))


;;;; INERT TYPES ;;;;


(defun report-inert-types ()
  "Reports every declared object type that has instances but is named by no relation
   signature, no query or update parameter, and no action parameter.  The typical cause
   is a DEFINE-TYPES entry whose matching (INCLUDE-TECH ...) line was forgotten.

   Candidates are drawn from *TYPE-SIGNATURES*, not *TYPES*, because only a real
   INSTALL-TYPES call populates it.  *TYPES* additionally holds the combo types
   DISSECT-PRE-PARAMS synthesizes for an inline (EITHER ...) quantifier spec -- AGENT+CARGO
   from (DOALL (?OBJ (EITHER CARGO AGENT)) ...) -- which are named nowhere but inside the
   body that produced them, and would otherwise all be reported."
  (let ((consumed (consumed-type-names))
        (inert nil))
    (maphash (lambda (type instances)
               (when (and instances
                          (not (equal instances '(nil)))
                          (not (gethash type consumed)))
                 (push type inert)))
             *type-signatures*)
    (when inert
      (format t "~2&Note: declared object types with instances that nothing consumes:~%")
      (format t "~&  ~{~A~^, ~}~%" (sort inert #'string< :key #'symbol-name)))))


(defun consumed-type-names ()
  "EQ hash table of every type name reachable from a relation signature, a query or
   update parameter list, an action parameter list, a quantifier domain inside any body,
   or the named-segment convention.  Expanded transitively through *TYPE-COMPONENTS* so
   a composite consumes its members -- AGENT counts as consumed via BEAM-BLOCKER =
   (EITHER AGENT BOX JAMMER CONNECTOR)."
  (let ((consumed (make-hash-table :test #'eq)))
    (maphash (lambda (relation signature)
               (declare (ignore relation))
               (mark-consumed-type-spec signature consumed))
             *relations*)
    (maphash (lambda (relation signature)
               (declare (ignore relation))
               (mark-consumed-type-spec signature consumed))
             *static-relations*)
    (dolist (name (append *query-names* *update-names*))
      (mark-consumed-type-spec (get name :param-types) consumed)
      (mark-consumed-quantifier-domains (get name :raw-body) consumed))
    (dolist (action (append *actions* *init-actions*))
      (mark-consumed-type-spec (action.precondition-types action) consumed)
      (mark-consumed-quantifier-domains (action.precondition-form action) consumed)
      (mark-consumed-quantifier-domains (action.effect-form action) consumed))
    (mark-consumed-quantifier-domains (get 'goal-fn :form) consumed)
    (mark-consumed-segment-types consumed)
    consumed))


(defun mark-consumed-quantifier-domains (form consumed)
  "Marks the type spec of every DOALL, EXISTS, or FORALL parameter list anywhere in FORM.
   A type used only as an iteration domain inside a body is consumed just as surely as
   one named in a relation signature, and nothing in any signature or parameter list says
   so -- BEAM-ENDPOINT reaches the problem solely through -BEAM-LOS-COORDINATES'
   (DOALL (?ENDPOINT BEAM-ENDPOINT) ...) and -BEAM-CROSSING-COORDINATES'
   (EXISTS (?E BEAM-ENDPOINT) ...)."
  (cond ((atom form)
         nil)
        ((eq (car form) 'quote)
         nil)
        (t
         (when (member (car form) '(exists exist forsome forall forevery doall))
           (mark-consumed-type-spec (second form) consumed))
         (mark-consumed-quantifier-domains (car form) consumed)
         (mark-consumed-quantifier-domains (cdr form) consumed))))


(defun mark-consumed-segment-types (consumed)
  "Marks the object type behind each named-segment relation the problem actually declares.
   WALL-SEGMENTS and its siblings carry an untyped $list, so nothing in the signature
   names WALL or WINDOW; the pairing lives in WW-INIT-VALIDATOR's
   INIT-SEGMENT-RELATION-TYPES, which validates every record name against its type and is
   therefore the consumer.  Marking is conditional on the relation being declared here, so
   a problem with no segment geometry still reports an unused WINDOW."
  (dolist (entry (init-segment-relation-types))
    (when (or (gethash (car entry) *relations*)
              (gethash (car entry) *static-relations*))
      (mark-consumed-type-spec (cdr entry) consumed))))


(defun mark-consumed-type-spec (spec consumed)
  "Marks in CONSUMED every declared type name SPEC mentions, descending through
   (EITHER ...) forms and through *TYPE-COMPONENTS* so that a composite consumes its
   intermediate aliases as well as its leaves.  Marking only the leaves, as
   TYPE-SPEC-LEAF-TYPES computes them, would miss exactly the case this check keeps
   getting wrong: FIXTURE = (EITHER GATE TRANSMITTER RECEIVER) is reached only through
   the synthesized FIXTURE+LOCATION, and bottoms out in GATE, TRANSMITTER, and RECEIVER,
   so FIXTURE itself would never be marked.  The already-marked test terminates the
   descent, which a mutually recursive alias pair would otherwise run forever."
  (cond ((consp spec)
         (dolist (item spec)
           (mark-consumed-type-spec item consumed)))
        ((and (symbolp spec)
              (nth-value 1 (gethash spec *types*))
              (not (gethash spec consumed)))
         (setf (gethash spec consumed) t)
         (dolist (component (gethash spec *type-components*))
           (mark-consumed-type-spec component consumed)))))


;;;; INERT TECHNOLOGIES ;;;;


(defun report-inert-techs ()
  "Advisory report of included technologies contributing nothing to this problem.  The
   most speculative of the three checks.

   Inertness cannot be defined by declared types: GATE.LISP declares only JAMMER as
   optional -- GATE itself arrives from nested -GATE -- so a jammer-free problem would
   flag GATE inert while UPDATE-GATE-STATUS! is doing real work.  It is instead defined
   by what the technology contributes: a tech is inert when every action it defines is
   already uninstantiable and every zero-argument update it defines quantifies only over
   empty types.  Parameterized updates are ignored, since they are invoked from other
   updates and their relevance is decided by their callers -- that is what keeps
   WALL-BLOWER's SWEEP-OCCUPANTS-AWAY! from masking a fan-free problem.

   Only the technologies the problem itself included are candidates: *INCLUDED-TECH-NAMES*
   holds no nested ones.  The single piece of advice this report can give is to drop an
   include, so naming a technology the author never wrote and cannot remove would be advice
   with nothing behind it.  A receiver-free blower problem is the case that settled it --
   -BEAM-SUBSTRATE contributes only UPDATE-RECEIVER-STATUS!, which quantifies solely over
   an empty RECEIVER type and is therefore genuinely inert, but it arrives beneath
   FLOOR-BLOWER and WALL-BLOWER through -CONTROLS and -GEARS-FAN.  The technology worth
   naming is always the one the problem included: BEAM-RELAY in a connector-free problem is
   itself inert, and is reported."
  (let ((inert (remove-if-not #'tech-inert-p (reverse *included-tech-names*))))
    (when inert
      (format t "~2&Note: included technologies that appear inert in this problem:~%")
      (format t "~&  ~{~A~^, ~}~%" inert))))


(defun tech-inert-p (tech-name)
  "True when TECH-NAME's file defines at least one action or zero-argument update and
   every one of them is a no-op in the current problem."
  (let ((actions nil)
        (updates nil))
    (dolist (form (tech-contributed-forms tech-name))
      (when (consp form)
        (case (car form)
          ((define-action define-init-action)
           (push form actions))
          (define-update
            (when (null (third form))
              (push (second form) updates))))))
    (and (or actions updates)
         (every #'tech-action-uninstantiable-p actions)
         (every #'update-quantifies-only-over-empty-types-p updates))))


(defun tech-contributed-forms (tech-name)
  "The top-level forms of tech/TECH-NAME.lisp, read with *PACKAGE* bound to :WW as
   PRESCAN-PROBLEM-FILE does.  Re-reading the file is the only way to attribute a
   definition to the technology that supplied it: nothing at runtime records which tech
   file contributed which definition, and by init the spliced problem.lisp has erased
   the boundaries."
  (let ((tech-file (tech-file-path tech-name)))
    (when tech-file
      (let ((*package* (find-package :ww)))
        (read-problem-forms tech-file)))))


(defun tech-action-uninstantiable-p (form)
  "True when the action FORM defines has a parameter type with no instances, so
   INSTALL-ACTION skipped it.  Recomputed here from the source form rather than looked
   up, because a skipped action never reaches *ACTIONS* and leaves nothing to consult."
  (let ((pre-params (fourth form)))
    (check-action-parameter-instantiability
      (second form)
      (nth-value 1 (dissect-pre-params
                     (if (member (first pre-params) *parameter-headers*)
                       pre-params
                       (cons 'standard pre-params)))))))


(defun update-quantifies-only-over-empty-types-p (name)
  "True when every top-level DOALL in NAME's body iterates over a type with no
   instances, so the body never runs.  An update with no top-level DOALL is not provably
   inert and answers NIL."
  (let ((parameter-lists (top-level-doall-parameters (get name :raw-body))))
    (and parameter-lists
         (every (lambda (parameters)
                  (check-action-parameter-instantiability
                    name
                    (nth-value 1 (dissect-pre-params
                                   (if (member (first parameters) *parameter-headers*)
                                     parameters
                                     (cons 'standard parameters))))))
                parameter-lists))))


(defun top-level-doall-parameters (form)
  "The parameter lists of the DOALL forms an update body executes unconditionally,
   looking through the DO, PROGN, and LET wrappers that commonly surround them.  A DOALL
   buried inside a conditional is deliberately not counted: whether it runs is not a
   question about declared types."
  (cond ((atom form)
         nil)
        ((eq (car form) 'doall)
         (list (second form)))
        ((member (car form) '(do progn))
         (mapcan #'top-level-doall-parameters (rest form)))
        ((member (car form) '(let let*))
         (mapcan #'top-level-doall-parameters (cddr form)))
        (t
         nil)))


;;;; COMPONENT CONDENSATION ;;;;

;;; Phase 3 stage 6 deleted REPORT-DERIVATION-STRATA from here.  It printed a problem's
;;; derivations in dependency strata with the reactions removed, to judge whether Phase 2's
;;; two-tier architecture -- an inner fixpoint over derivations alone -- would pay off.  It
;;; read the authored driver, and no technology-bearing problem authors one any more, so it
;;; answered "this problem has no driver" for the whole population it existed to measure.
;;; REPORT-DERIVED-DRIVER's "derivation strata:" line reports the same partition from the
;;; derived candidates and is what Phase 2 should consult.


(defun propagation-condensation-order (names adjacency components)
  "The component DAG of ADJACENCY in dependency order, each component given as its member
   names.  Between components the order is a real dependency; within one it is arbitrary,
   and only a fixpoint establishes correctness."
  (let ((remaining (remove-duplicates (coerce components 'list)))
        (placed nil))
    (loop while remaining
          do (let ((ready (find-if (lambda (component)
                                     (notany (lambda (other)
                                               (and (/= other component)
                                                    (propagation-component-edge-p
                                                      adjacency components other component)))
                                             remaining))
                                   remaining)))
               (unless ready
                 (return))
               (push ready placed)
               (setf remaining (remove ready remaining))))
    (loop for component in (nreverse placed)
          collect (loop for name in names
                        for index from 0
                        when (= (aref components index) component)
                          collect name))))


(defun propagation-component-edge-p (adjacency components from to)
  "True when some node in component FROM writes something some node in component TO reads."
  (let ((size (array-dimension adjacency 0)))
    (dotimes (i size nil)
      (dotimes (j size)
        (when (and (= (aref components i) from)
                   (= (aref components j) to)
                   (= 1 (aref adjacency i j)))
          (return-from propagation-component-edge-p t))))))


;;;; DRIVER UPDATE ARITY ;;;;


(defun report-driver-update-callers ()
  "Advisory report of driver updates invoked from somewhere other than the driver.

   The invariant Phase 3 rests on: a zero-argument update in a technology file is a driver
   update, and a parameterized one is a helper whose relevance its callers decide.  That
   began as a survey result -- 13 DEFINE-UPDATE forms in tech/, the eight zero-argument ones
   exactly the eight driver updates, no exceptions in either direction -- and
   DRIVER-CANDIDATE-UPDATES has since turned it into a rule.  A zero-argument update that
   something else calls breaks it in the damaging direction: the derived driver would splice
   it in and its caller would run it again, twice per pass, with no symptom beyond a state
   that converged differently.

   Reported rather than signaled.  The arity is a convention, and a technology might have a
   defensible reason to depart from it -- but not silently, and not while a machine is
   reading the convention as a specification."
  (let ((offenders (loop for name in (driver-candidate-updates)
                         for callers = (driver-update-callers name)
                         when callers
                           collect (cons name callers))))
    (when offenders
      (format t "~2&Note: zero-argument technology updates invoked outside the driver:~%")
      (dolist (entry offenders)
        (format t "~&  ~A, called from ~{~A~^, ~}~%" (car entry) (cdr entry)))
      (format t "~&  Each would run twice per pass in a derived driver.~%"))))


(defun driver-update-callers (name)
  "The updates and actions whose bodies call NAME.  The two driver functions are excluded:
   calling a driver update is exactly what PROPAGATE-CONSEQUENCES! is for, and an action
   reaches it only through PROPAGATE-CHANGES!."
  (append (loop for other in *update-names*
                when (and (not (eq other name))
                          (not (member other '(propagate-changes! propagate-consequences!)))
                          (form-calls-update-p (get other :raw-body) name))
                  collect other)
          (loop for action in *actions*
                when (form-calls-update-p (action.effect-form action) name)
                  collect (action.name action))))


(defun form-calls-update-p (form name)
  "True when FORM calls NAME in head position, or references it as #'NAME.  Shares
   COLLECT-PROPAGATION-ORDER-NAMES' one weakness -- the generic descent cannot distinguish
   a head position from the second element of a list whose head it already passed -- which
   costs nothing here, since a bare update symbol has no meaning in this language and so
   never appears."
  (cond ((atom form)
         nil)
        ((eq (car form) 'quote)
         nil)
        ((eq (car form) name)
         t)
        ((and (eq (car form) 'function)
              (eq (second form) name))
         t)
        (t
         (or (form-calls-update-p (car form) name)
             (form-calls-update-p (cdr form) name)))))


;;;; DERIVED DRIVER -- PHASE 3, MILESTONE 1 ;;;;

;;; Derives the propagation order a problem would get if it authored no driver, and prints
;;; it beside the order the problem actually wrote.  Invoked by hand; INIT never calls it,
;;; and nothing here installs anything.  The point is to clear section 8.7's acceptance bar
;;; on all seven reference problems before any behavior changes, so that a later switchover
;;; has nothing left to discover.
;;;
;;; The bar as amended: the derived order must carry no reaction violation, and must cost
;;; the fixpoint no more passes than the authored order.  The original bar compared
;;; positions -- equal orders, or differing only within a component -- and failed
;;; CLAUSTRO-TOPO for a difference that was not one, since the pair that swapped had no
;;; edge between them in either direction.  See REPORT-DERIVED-DRIVER-DIFFERENCES.
;;;
;;; The order within a component is where the derivation has a free choice, and milestone 1
;;; left it to splice order -- a depth-first walk of the include directives, which encodes
;;; nothing about dependency.  It systematically placed substrate-declared updates too
;;; early, because an update's splice position is fixed by where it is declared while its
;;; dependencies are created by queries other technologies override later:
;;; -BEAM-SUBSTRATE declares UPDATE-RECEIVER-STATUS! and installs COMPUTE-CONNECTOR-LIGHTING
;;; as a null object, and BEAM-RELAY, spliced much later, supplies the real one.  The same
;;; inversion appeared in CORNER-TOPO, PHOBIA and PROBLEM-PROPAGATION-STRATA-TEST, and in no
;;; problem lacking BEAM-RELAY.
;;;
;;; Milestone 2 replaced the seed with MINIMUM-FEEDBACK-ARC-ORDER, which recovers the
;;; ordering from the graph instead of from the file layout.  On CORNER-TOPO it does better
;;; than the driver it would replace -- see that function.


(defun report-derived-driver ()
  "Prints the derived propagation order beside the authored one for the current problem.

   Inert candidates are dropped, so the derived driver is tighter than a hand-authored one:
   in a receiver-free problem UPDATE-RECEIVER-STATUS! quantifies solely over an empty type,
   and every hand-authored driver in test/ carries it anyway with the comment \"no receivers
   here; kept to document the required ordering.\"  The unfiltered order is printed too,
   because dropping a node can merge or split a component, and a difference from the
   authored order has to be attributable to one cause or the other."
  (let* ((candidates (driver-candidate-updates))
         (inert (remove-if-not #'update-quantifies-only-over-empty-types-p candidates))
         (kept (remove-if #'update-quantifies-only-over-empty-types-p candidates))
         (raw-body (authored-propagation-driver-body)))
    (format t "~2&Derived propagation driver:~2%")
    (format t "~&  spliced technologies:  ~{~A~^ ~}~%" (reverse *spliced-tech-names*))
    (format t "~&  candidate updates:     ~{~A~^ ~}~%" candidates)
    (format t "~&  dropped as inert:      ~{~A~^ ~}~%" (or inert (list "none")))
    (if (null kept)
      (format t "~&  This problem has no driver updates.~2%")
      (progn (format t "~&  derived, unfiltered:   ~{~A~^ ~}~%"
                     (derived-propagation-order candidates))
             (print-derived-driver-comparison
               kept
               (and raw-body (authored-propagation-order raw-body)))))))


(defun driver-candidate-updates ()
  "The zero-argument update functions the spliced technologies contribute, in splice order.

   Zero-argument is the whole test, and it is exact rather than approximate: tech/ holds 13
   DEFINE-UPDATE forms, the eight taking no arguments are precisely the eight driver
   updates, and the five taking arguments -- SWEEP-OCCUPANTS-AWAY!, BLOW-OCCUPANTS-AWAY!,
   DROP-OCCUPANTS!, RELOCATE-STACK!, PLACE-HELD-OBJECT! -- are precisely the helpers an
   update or an action invokes.  The same distinction TECH-INERT-P already draws, for the
   same reason: a parameterized update's relevance is decided by its callers.

   Restricted to technology files rather than taken from *UPDATE-NAMES* wholesale.  The
   older problems define plenty of zero-argument updates of their own -- PROBLEM-CORNER's
   DERIVE-HOLDS! and CREATE-MISSING-BEAMS! among ten, PROBLEM-MATCH3's APPLY-GRAVITY! --
   and none belongs in a technology-assembled driver."
  (let ((candidates nil))
    (dolist (tech-name (reverse *spliced-tech-names*) (nreverse candidates))
      (dolist (form (tech-contributed-forms tech-name))
        (when (and (consp form)
                   (eq (car form) 'define-update)
                   (null (third form))
                   (propagation-candidate-p (second form))
                   (not (member (second form) candidates)))
          (push (second form) candidates))))))


(defun derived-propagation-order (candidates)
  "Three values for CANDIDATES: the derived driver order, an alist giving each candidate's
   component in the whole-candidate graph, and the derivation strata the order was built
   from.

   Three tiers, in the order section 8.3 states them.  The derivations are separated out and
   ordered among themselves -- condensation between components, minimum feedback arc within
   -- and the reactions are appended, their order among themselves free by resolved decision
   8.6.1.

   Separating first is what milestone 2 changed.  The earlier version condensed every
   candidate together and moved the reactions afterward, which let the partition undo
   ordering work the condensation had just done.  Ordering the derivations alone also drops
   the edges that route between derivations through a reaction, correctly: reactions run
   last, so such a path is broken in the emitted order regardless and is not worth
   optimizing for.

   No reaction can precede its derivation producer, since every derivation precedes every
   reaction, so the emitted order is free of reaction violations by construction --
   REPORT-DERIVED-DRIVER-SELF-CHECK tests that rather than trusting it."
  (multiple-value-bind (reads writes base-facts adjacency components)
      (propagation-graph candidates)
    (declare (ignore reads writes adjacency))
    (multiple-value-bind (ordered strata)
        (ordered-derivations (loop for name in candidates
                                   for index from 0
                                   unless (aref base-facts index)
                                     collect name))
      (values (append ordered
                      (loop for name in candidates
                            for index from 0
                            when (aref base-facts index)
                              collect name))
              (loop for name in candidates
                    for index from 0
                    collect (cons name (aref components index)))
              strata))))


(defun ordered-derivations (derivations)
  "DERIVATIONS in dependency order, and the strata that order is made of.  Each stratum is
   one component of the derivation-only graph, its members ordered to carry as few backward
   edges as possible."
  (multiple-value-bind (reads writes base-facts adjacency components)
      (propagation-graph derivations)
    (declare (ignore reads writes base-facts))
    (let ((strata (loop for component
                          in (propagation-condensation-order derivations adjacency components)
                        collect (minimum-feedback-arc-order component derivations adjacency))))
      (values (apply #'append strata) strata))))


(defparameter *feedback-arc-limit* 8
  "The largest component MINIMUM-FEEDBACK-ARC-ORDER will order exhaustively.  Every
   reference problem produces components of at most six, where the search is instant.
   Exceeding the limit signals rather than silently degrading, because the fallback -- a
   greedy ordering -- does not exist yet and writing one before a problem needs it would be
   guessing at what it should optimize.")


(defun minimum-feedback-arc-order (component names adjacency)
  "COMPONENT's members in the order carrying the fewest edges that run backward, meaning a
   consumer placed ahead of its producer.  NAMES and ADJACENCY index the whole derivation
   set; COMPONENT is the subset being ordered.

   Inside a cycle no order eliminates every backward edge, so this minimizes rather than
   discharges.  It is worth doing because the count is what the fixpoint pays: CORNER-TOPO's
   four updates admit orders costing 1, 3 and 4 backward edges, its authored driver spends
   3, and splice order spends 4.  The knowledge its comment records -- connectors before
   receivers, crossings before both -- turns out to be recoverable from the graph after all,
   and improvable: gate first pays one backward edge to make three OPEN reads fresh.

   Exhaustive rather than greedy.  At six nodes the permutations cost nothing, and an exact
   answer removes the question of whether a heuristic's output is the heuristic's fault.
   PERMUTATIONS yields the input order first and the comparison is strict, so ties break
   toward splice order and the result stays deterministic."
  (when (> (length component) *feedback-arc-limit*)
    (error "Propagation component of ~D updates exceeds *FEEDBACK-ARC-LIMIT* (~D):~%  ~
            ~{~A ~}~2%~
            MINIMUM-FEEDBACK-ARC-ORDER searches permutations exhaustively, which is instant ~
            at the sizes every reference problem produces and is not at this one.  A greedy ~
            ordering is needed here."
           (length component) *feedback-arc-limit* component))
  (let ((best nil)
        (best-count nil))
    (dolist (candidate (permutations component) best)
      (let ((count (backward-edge-count candidate names adjacency)))
        (when (or (null best-count) (< count best-count))
          (setf best candidate)
          (setf best-count count))))))


(defun permutations (items)
  "Every ordering of ITEMS, the given order first."
  (if (null (rest items))
    (list items)
    (loop for item in items
          append (loop for ordering in (permutations (remove item items))
                       collect (cons item ordering)))))


(defun backward-edge-count (order names adjacency)
  "How many edges among ORDER's members place a consumer ahead of its producer.  NAMES and
   ADJACENCY index the whole derivation set; ORDER is the subset being scored."
  (let ((count 0))
    (dolist (producer order count)
      (dolist (consumer order)
        (when (and (= 1 (aref adjacency (position producer names) (position consumer names)))
                   (< (position consumer order) (position producer order)))
          (incf count))))))


(defun print-derived-driver-comparison (candidates authored)
  "Prints the derived order for CANDIDATES, the AUTHORED order, the components and strata
   the derivation was built from, and the verdict."
  (multiple-value-bind (derived component-alist strata) (derived-propagation-order candidates)
    (format t "~&  derived order:         ~{~A~^ ~}~%" derived)
    (format t "~&  authored order:        ~{~A~^ ~}~%" (or authored (list "none")))
    (format t "~&  components:            ~{~A~^ ~}~%"
            (mapcar (lambda (component) (format nil "{~{~A~^ ~}}" component))
                    (propagation-component-groups component-alist)))
    (format t "~&  derivation strata:     ~{~A~^ then ~}~%"
            (mapcar (lambda (stratum) (format nil "{~{~A~^ ~}}" stratum)) strata))
    (report-derived-driver-self-check derived)
    (report-derived-driver-differences derived authored)))


(defun propagation-component-groups (component-alist)
  "The candidates grouped by component, one list per component, in first-appearance order."
  (loop for id in (remove-duplicates (mapcar #'cdr component-alist) :from-end t)
        collect (loop for entry in component-alist
                      when (eql id (cdr entry))
                        collect (car entry))))


(defun report-derived-driver-self-check (derived)
  "Feeds the derived order back through the same analysis that audits an authored one.

   A reaction violation here is impossible by the argument in DERIVED-PROPAGATION-ORDER,
   which is exactly why it is worth running: a construction argument is not a test, and this
   is the one check that would catch the derivation emitting an order INIT would then
   reject."
  (multiple-value-bind (reactions costs) (propagation-order-findings derived)
    (declare (ignore costs))
    (format t "~&  self-check:            ~A~%"
            (if reactions
              (format nil "DEFECT -- ~D reaction violation(s) in the derived order"
                      (length reactions))
              "clean"))
    (dolist (violation reactions)
      (format t "~&    ~A~%" (propagation-violation-description violation)))))


(defun propagation-order-findings (order)
  "What ORDER costs, as three values: its reaction violations, which are fatal; its
   cross-component convergence violations, each worth an extra pass of the fixpoint; and its
   backward edges inside a single component.

   The third value exists because the first two cannot see the case Phase 3 most needs
   measured.  PROPAGATION-ORDER-VIOLATIONS deliberately says nothing about a backward edge
   within a component -- the cycle guarantees recomputation, so there is no correctness
   claim to make -- but six of the seven reference problems are a single component, and
   CORNER-TOPO's whole authored ordering rationale lives inside one.  Counting those edges
   is what makes its convergence cost visible without running the search."
  (multiple-value-bind (reads writes base-facts adjacency components)
      (propagation-graph order)
    (let ((violations (propagation-order-violations order adjacency components
                                                    reads writes base-facts)))
      (values (remove-if-not #'fourth violations)
              (remove-if #'fourth violations)
              (within-component-backward-edges order adjacency components reads writes)))))


(defun within-component-backward-edges (order adjacency components reads writes)
  "Every edge whose consumer runs before its producer inside one component, as
   (PRODUCER CONSUMER RELATIONS).

   Each is a read of a value one pass stale.  Inside a cycle no order eliminates them all,
   so the count is not a defect but an objective: the fewer an order carries, the closer it
   is to the minimum feedback arc set, and the fewer passes the fixpoint needs.  Comparing
   two orders' counts is the whole use."
  (let ((size (length order))
        (edges nil))
    (dotimes (i size (nreverse edges))
      (dotimes (j size)
        (when (and (= 1 (aref adjacency i j))
                   (< j i)
                   (= (aref components i) (aref components j)))
          (push (list (nth i order)
                      (nth j order)
                      (propagation-edge-relations (aref writes i) (aref reads j)))
                edges))))))


(defun report-derived-driver-differences (derived authored)
  "Compares the derived order against the authored one on the updates they share.

   Membership first, since the inert filter deliberately drops updates a hand-authored
   driver carries.  Then both orders go through the violation analysis, restricted to the
   updates they share so that the dependency graph is identical and only the sequence
   differs -- which is what makes the two results comparable at all.

   Comparing violations rather than positions is the correction section 8.7 needed.  The
   original criterion asked whether any pair in different components had swapped, which
   presumes the derived order is the suspect one.  CLAUSTRO-TOPO is the case that broke
   it: PLATE is a source component feeding GATE, the derived order respects that and the
   authored order splits the RECEIVER/GATE component around PLATE, so the pair that swapped
   is one the graph never ordered -- there is no PLATE to RECEIVER edge, only PLATE to GATE,
   which both orders honor.  The question worth asking is not whether the orders differ but
   whether the derived one introduces a violation the authored one did not."
  (unless authored
    (format t "~&  comparison:            no authored driver to compare against~2%")
    (return-from report-derived-driver-differences))
  (let ((missing (set-difference authored derived))
        (extra (set-difference derived authored)))
    (when missing
      (format t "~&  authored, not derived: ~{~A~^ ~}~%" missing))
    (when extra
      (format t "~&  derived, not authored: ~{~A~^ ~}~%" extra))
    (report-shared-order-comparison
      (remove-if-not (lambda (name) (member name authored)) derived)
      (remove-if-not (lambda (name) (member name derived)) authored))))


(defun report-shared-order-comparison (derived authored)
  "Prints each order's stale-read edges, both kinds, and the verdict.  Both arguments name
   the same set of updates, in their respective orders."
  (multiple-value-bind (derived-reactions derived-costs derived-back)
      (propagation-order-findings derived)
    (multiple-value-bind (authored-reactions authored-costs authored-back)
        (propagation-order-findings authored)
      (declare (ignore authored-reactions))
      (format t "~&  cross-component cost:  derived ~D, authored ~D~%"
              (length derived-costs) (length authored-costs))
      (format t "~&  within-component cost: derived ~D, authored ~D~%"
              (length derived-back) (length authored-back))
      (print-stale-read-edges "derived" (append derived-costs derived-back))
      (print-stale-read-edges "authored" (append authored-costs authored-back))
      (format t "~&  comparison:            ~A~2%"
              (derived-driver-verdict derived authored derived-reactions
                                      (- (length derived-costs) (length authored-costs))
                                      (- (length derived-back) (length authored-back)))))))


(defun print-stale-read-edges (label edges)
  "One line per edge naming the consumer, what it reads too early, and the producer."
  (dolist (edge edges)
    (format t "~&    ~A pays: ~A reads ~{~A~^ ~} from ~A, called later.~%"
            label (second edge) (third edge) (first edge))))


(defun derived-driver-verdict (derived authored reactions cost-delta back-delta)
  "One line stating whether the derived order clears section 8.7 as amended: it must carry
   no reaction violation, and cost the fixpoint no more stale reads than the authored order.
   COST-DELTA and BACK-DELTA are the derived order's edge counts less the authored order's,
   cross-component and within-component respectively."
  (cond (reactions
         "DEFECT -- the derived order carries a reaction violation, which INIT would reject")
        ((plusp cost-delta)
         (format nil "REGRESSION -- ~D more cross-component stale read(s) than authored"
                 cost-delta))
        ((plusp back-delta)
         (format nil "REGRESSION -- ~D more within-component stale read(s) than authored"
                 back-delta))
        ((or (minusp cost-delta) (minusp back-delta))
         "acceptable -- fewer stale reads than authored")
        ((equal derived authored)
         "identical")
        (t
         "acceptable -- reordered, costing the fixpoint no more than authored")))


;;;; DERIVED DRIVER -- INSTALLATION ;;;;

;;; Where the derivation stops reporting and starts running.  INIT calls
;;; INSTALL-DERIVED-PROPAGATION-DRIVER once, immediately after
;;; REPORT-PROPAGATION-DIAGNOSTICS.
;;;
;;; That position is load-bearing at both ends.  Ahead of it, INIT has already reversed
;;; *UPDATE-NAMES* and *ACTIONS*, so ACTION-BASE-FACT-SET -- which decides which updates are
;;; reactions, and therefore the whole shape of the derived order -- sees every action the
;;; problem defines.  Behind it, DO-INTEGER-CONVERSION's COMPILE-ALL-FUNCTIONS still lies
;;; ahead, so the lambda list INSTALL-UPDATE leaves in the symbol value gets compiled like
;;; any other update, and DO-INIT-ACTION-UPDATES -- which is what actually runs
;;; (PROPAGATE-CHANGES!) -- lies further ahead still.  Moving this call, or reordering INIT
;;; around it, breaks one of the two.


(defun install-derived-propagation-driver ()
  "Replaces tech/-propagation.lisp's sentinel PROPAGATE-CONSEQUENCES! with the driver
   derived from the loaded technologies.

   Silent for a problem that authored its own driver: that definition displaced the
   sentinel at load, and the author's sequence is left exactly as written.

   The derived order is audited before it is installed, by the same
   REPORT-PROPAGATION-ORDER-FINDINGS that audits an authored one, so a reaction violation
   halts INIT here as it would there.  DERIVED-PROPAGATION-ORDER argues no such violation
   is constructible; running the check anyway is what would catch that argument being
   wrong, and a construction argument is not a test.

   An empty derived order leaves the sentinel in place rather than installing a driver that
   propagates nothing.  It arises when every candidate update is inert, which makes an empty
   driver correct and a silent no-op indistinguishable from a derivation that failed.  The
   sentinel keeps the second case loud: nothing breaks unless something actually calls
   PROPAGATE-CHANGES!, and then it says so."
  (unless (and (member 'propagate-consequences! *update-names*)
               (equal (get 'propagate-consequences! :raw-body) *propagation-driver-sentinel*))
    (return-from install-derived-propagation-driver))
  (let ((order (derived-propagation-order
                 (remove-if #'update-quantifies-only-over-empty-types-p
                            (driver-candidate-updates)))))
    (unless order
      (return-from install-derived-propagation-driver))
    (report-propagation-order-findings order)
    (format t "~&Deriving propagation driver: ~{~A~^ ~}~%" order)
    (install-update 'propagate-consequences! nil
                    (derived-propagation-driver-body order))))


(defun derived-propagation-driver-body (order)
  "The PROPAGATE-CONSEQUENCES! body that runs ORDER.

   The shape every hand-authored driver used, and for the same reason: binding
   *PROPAGATED-STATE-CHANGED* to NIL, calling each update, and returning that variable makes
   the driver report T exactly when some update changed stored state -- which is the signal
   PROPAGATE-CHANGES!'s fixpoint tests to decide whether another pass is warranted."
  `(let ((*propagated-state-changed* nil))
     ,@(mapcar #'list order)
     *propagated-state-changed*))
