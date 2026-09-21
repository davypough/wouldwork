;;; Filename: constraint-ledger.lisp

;;; The realization ledger for the constraint-led analysis method (T2 of
;;; doc/constraint-method/Constraint-Implementation-Plan.md).  It is the interactive phase's
;;; state: links, their status, the premises each depends on, and the evidence closing each.
;;; Its schema, status algebra, retraction semantics and exhaustion rules are specified in
;;; doc/constraint-method/Status-Algebra-and-Record-Schema.md, which this file implements
;;; without adding a design decision of its own.  Section numbers in the docstrings below
;;; refer to that file.
;;;
;;; THIS FILE IS A LOADABLE DIAGNOSTIC, on the same terms as constraint-profile.lisp: never
;;; named in an (include-tech ...) directive, not an ASDF component, plain Common Lisp in the
;;; :WW package, no define-query, define-types, define-dynamic-relations or any other DSL
;;; defining form.
;;;
;;;   (progn (ql:quickload :wouldwork) (in-package :ww))
;;;   (load (merge-pathnames "tech/constraint-ledger.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;;
;;; PLACEMENT, and why it is a separate file rather than a block inside
;;; constraint-profile.lisp.  That file is a pure function of a STAGED problem; every entry
;;; point in it reads databases staging built.  The ledger reads none: it is loadable and
;;; usable with no problem staged at all, it persists across sessions, and it is amended by
;;; hand between them.  Keeping it separate means a ledger session never has to stage
;;; anything, and the hash-locked S0-S4 generator is not touched by ledger work.  Nothing
;;; here calls constraint-profile.lisp and nothing there calls this, so the two load in
;;; either order.
;;;
;;; DEFINITION ORDER is callees-first, as in constraint-profile.lisp and for the same reason:
;;; the file is LOADed by hand and reloaded after every edit, and a forward reference costs a
;;; STYLE-WARNING on each load.
;;;
;;; DOMAIN GENERALITY (C3).  No problem object name appears in this file.  Problem terms
;;; enter only as data inside records, which is the pattern RO's caller-supplied scenario
;;; established.
;;;
;;; M2 AND THIS FILE'S OUTPUT.  The ledger is the one generated artifact of this method that
;;; M2 does not seal: it is program-written and deliberately user-amendable.  The reader
;;; keeps every key it does not recognise and the writer writes it back, so a hand
;;; annotation is never silently dropped.

(in-package :ww)


;;; ---------------------------------------------------------------------------
;;; Vocabulary.  Every table here is the specification's, transcribed.
;;; ---------------------------------------------------------------------------

(defparameter *ledger-kind-prefixes*
  '((:premise . "PR") (:link . "LK") (:bound . "BD") (:question . "QN"))
  "Section 2.  Ids are stable and append-only; the prefix names the kind and never changes.")


(defparameter *ledger-envelope-keys*
  '(:id :kind :statement :provenance :depends-on :premise-gaps :segment :status :sources
    :events)
  "Section 3.  All required on every record of every kind; an absent key is ill-formed, not
   defaulted.")


(defparameter *ledger-kind-keys*
  '((:premise :discharged-by)
    (:link :from :to :intent :closed-by :evidence :validated :attempts :refuted-by
     :segment-bridge :search-goal :search-start :search-cutoff :search-threads
     :search-settings :search-preamble :search-final :chain-order :recommendation :measured)
    (:bound :for-link :measured :interpretation-committed :segment-bridge)
    (:question :candidates :default :answer-kind :template :blocks :answer :answer-premise
     :gap-candidate))
  "Section 6.  :SEGMENT-BRIDGE is the explicit bridging premise WF16 requires of a record
   whose dependency crosses an incompatible segment.  The :SEARCH-* keys and :RECOMMENDATION
   are T3's: they hold the problem terms a runnable recommendation needs, as data, and the
   recommendation committed before the run.")


(defparameter *ledger-statuses*
  '((:premise :in-force :discharged :retracted :refuted)
    (:link :open :realized :closed :refuted :invalidated)
    (:bound :standing :orphaned :superseded)
    (:question :open :answered :withdrawn :invalidated))
  "Section 7.  The stored LIFECYCLE.  It is not the conditional/established distinction,
   which is computed by LEDGER-STANDING and never stored.")


(defparameter *ledger-dead-statuses* '(:retracted :refuted)
  "A disjunct is dead in these two statuses and in no other.  DISCHARGED is alive: the
   premise was replaced by a derivation, not withdrawn.")


(defparameter *ledger-default-search-threads* 16
  "Threads for a recommended search that states its own start.  D set this on 2026-09-20.
   IT CANNOT APPLY TO A CHAIN LINK: VALIDATE-CONTINUATION-PRECONDITIONS in
   src/ww-goal-chaining.lisp signals \"Goal chaining requires single-threaded mode\" unless
   *THREADS* is 0, so the one-argument SOLVE-SUBGOAL runs serially or not at all.  The
   two-argument form goes through SOLVE-SUBGOAL-FROM-FORM, which its own docstring says runs
   in any thread mode.  LEDGER-DEFAULT-THREADS applies the split; nothing is coerced
   silently.")


(defparameter *ledger-bound-lint-words*
  '("impossible" "cannot" "no solution" "refutes" "unreachable")
  "Section 10, guard X4.  A crude heuristic sitting behind three structural guarantees, and
   stated as crude.  It catches the one case the other three cannot: a correctly typed bound
   whose prose says the wrong thing and then gets quoted into a register.")


;;; ---------------------------------------------------------------------------
;;; Accessors and small arithmetic over a ledger
;;; ---------------------------------------------------------------------------

(defun ledger-today ()
  "Today as the ISO date string the records and the state files both use."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time
                                                             (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~D-~2,'0D-~2,'0D" year month day)))


(defun ledger-record (ledger id)
  "The record with ID, or NIL.  Ids are unique by WF1, so a linear FIND is the whole story."
  (find id (getf ledger :records) :key (lambda (record) (getf record :id))))


(defun ledger-provenance-value (provenance key)
  "Section 4.  A provenance is a species keyword followed by a plist, so the plist starts one
   past the head and a plain GETF over the whole form would read the pairs off by one."
  (getf (rest provenance) key))


(defun ledger-set-value (record key value)
  "Destructive update of a key that is already present.  A plain SETF GETF rebinds the local
   variable when the key is absent, which would leave the ledger's own copy untouched and the
   caller none the wiser; this signals instead."
  (let ((tail (member key record)))
    (unless tail
      (error "Ledger record ~S has no key ~S; it cannot be updated." (getf record :id) key))
    (setf (second tail) value)))


(defun ledger-add-event (record date event note)
  "Section 3.2.  Events are append-only; nothing already in the list is edited or removed."
  (let ((tail (member :events record)))
    (unless tail
      (error "Ledger record ~S has no :EVENTS key." (getf record :id)))
    (setf (second tail)
          (append (second tail)
                  (list (list :date date :event event :by "ledger" :note note))))))


(defun ledger-next-id (ledger prefix)
  "Section 2.  One above the highest number present under PREFIX.  Nothing is ever removed
   from a ledger, so this is safe and no counter has to be stored."
  (let ((highest 0)
        (width (length prefix)))
    (dolist (record (getf ledger :records))
      (let ((name (symbol-name (getf record :id))))
        (when (and (> (length name) width) (string-equal prefix name :end2 width))
          (let ((number (parse-integer name :start width :junk-allowed t)))
            (when (and number (> number highest))
              (setf highest number))))))
    (intern (format nil "~A~D" (string-upcase prefix) (1+ highest)) (find-package :ww))))


;;; ---------------------------------------------------------------------------
;;; The dependency graph.  Nothing below is cached: a stored closure goes stale on
;;; the first amendment and the staleness is invisible (section 5).
;;; ---------------------------------------------------------------------------

(defun ledger-dead-record-p (ledger id)
  "Whether the disjunct ID is dead.  A dangling id is not dead, it is ill-formed, and WF2
   reports it as such rather than letting it quietly kill a clause."
  (let ((record (ledger-record ledger id)))
    (and record (member (getf record :status) *ledger-dead-statuses*) t)))


(defun ledger-clause-dead-p (ledger clause)
  "A clause dies only when every disjunct in it dies.  Cascade is by empty clause and never by
   mention: a record resting on (PR3 PR7) survives the retraction of PR3, which is the whole
   reason :DEPENDS-ON is a conjunction of disjunctions."
  (and clause (every (lambda (id) (ledger-dead-record-p ledger id)) clause)))


(defun ledger-direct-dependencies (ledger id)
  "Every id this record rests on: the disjuncts of each clause, plus the record discharging
   it, which standing looks through."
  (let ((record (ledger-record ledger id))
        (ids nil))
    (dolist (clause (getf record :depends-on))
      (dolist (disjunct clause)
        (pushnew disjunct ids)))
    (when (getf record :discharged-by)
      (pushnew (getf record :discharged-by) ids))
    (nreverse ids)))


(defun ledger-closure (ledger id)
  "The transitive set of records ID rests on, ID itself excluded."
  (let ((seen nil)
        (pending (ledger-direct-dependencies ledger id)))
    (loop while pending
          do (let ((next (pop pending)))
               (unless (member next seen)
                 (push next seen)
                 (dolist (further (ledger-direct-dependencies ledger next))
                   (push further pending)))))
    (nreverse seen)))


(defun ledger-dependents (ledger id)
  "The transitive closure of the inverse :DEPENDS-ON edge: exactly the records a retraction of
   ID may touch.  Everything outside this set is left alone, which is the half of invariant I1
   that keeps a withdrawal from discarding results that never rested on the guess."
  (let ((found nil)
        (changed t))
    (loop while changed
          do (setf changed nil)
             (dolist (record (getf ledger :records))
               (let ((candidate (getf record :id)))
                 (unless (member candidate found)
                   (let ((edges (ledger-direct-dependencies ledger candidate)))
                     (when (or (member id edges) (intersection found edges))
                       (push candidate found)
                       (setf changed t)))))))
    (nreverse found)))


(defun ledger-unfounded-p (ledger id)
  "True when some clause of ID, or of anything beneath it, has no surviving disjunct."
  (dolist (each (cons id (ledger-closure ledger id)) nil)
    (let ((record (ledger-record ledger each)))
      (dolist (clause (getf record :depends-on))
        (when (ledger-clause-dead-p ledger clause)
          (return-from ledger-unfounded-p t))))))


(defun ledger-conditional-record-p (record)
  "A record that makes everything above it conditional: a live guess, a cost bound, or an
   exhausted measurement.  A DISCHARGED guess is not one, so standing looks through it to the
   derivation that replaced it."
  (let ((provenance (getf record :provenance)))
    (or (and (eq (getf record :kind) :premise)
             (eq (getf record :status) :in-force)
             (eq (first provenance) :user-asserted))
        (eq (getf record :kind) :bound)
        (and (eq (first provenance) :search-measured)
             (eq (ledger-provenance-value provenance :outcome) :exhausted)))))


(defun ledger-standing (ledger id)
  "Section 8.  :ESTABLISHED, :CONDITIONAL or :UNFOUNDED, recomputed from the dependency
   closure on every call and never stored.  This is invariant I1 mechanized: there is no field
   an optimistic caller could set to have a guess printed as a fact."
  (let ((record (ledger-record ledger id)))
    (cond ((ledger-unfounded-p ledger id) :unfounded)
          ((ledger-conditional-record-p record) :conditional)
          ((some (lambda (each) (ledger-conditional-record-p (ledger-record ledger each)))
                 (ledger-closure ledger id))
           :conditional)
          ((and (eq (getf record :kind) :link)
                (eq (first (getf record :closed-by)) :search-measured)
                (not (getf record :validated)))
           :conditional)
          (t :established))))


(defun ledger-live-guesses (ledger id)
  "The in-force user assertions ID rests on: what the reporter calls its blocking premises."
  (let ((guesses nil))
    (dolist (each (cons id (ledger-closure ledger id)) (nreverse guesses))
      (let ((record (ledger-record ledger each)))
        (when (and (eq (getf record :kind) :premise)
                   (eq (getf record :status) :in-force)
                   (eq (first (getf record :provenance)) :user-asserted))
          (push each guesses))))))


;;; ---------------------------------------------------------------------------
;;; Construction
;;; ---------------------------------------------------------------------------

(defun make-realization-ledger (problem &optional (date (ledger-today)))
  "An empty ledger for one problem.  Ids are ledger-local (section 2)."
  (list :version 1 :problem problem :written date :records nil))


(defun make-ledger-record (&key id kind statement provenance depends-on premise-gaps
                                segment status sources events extra)
  "Builds an envelope in canonical key order and appends the kind-specific keys in EXTRA, so a
   record built here cannot fail WF4.  EXTRA is a plist of this kind's own keys."
  (append (list :id id
                :kind kind
                :statement statement
                :provenance provenance
                :depends-on depends-on
                :premise-gaps premise-gaps
                :segment (or segment :none)
                :status status
                :sources sources
                :events (or events
                            (list (list :date (ledger-today) :event :opened :by "ledger"
                                        :note ""))))
          extra))


(defun make-ledger-premise (id statement provenance &key depends-on premise-gaps segment
                                                         sources)
  "Section 6.1.  A user assertion is the only kind of record a guess may be, and this is where
   one is made."
  (make-ledger-record :id id :kind :premise :statement statement :provenance provenance
                      :depends-on depends-on :premise-gaps premise-gaps :segment segment
                      :status :in-force :sources sources
                      :extra (list :discharged-by nil)))


(defun make-ledger-link (id statement provenance &key from to intent depends-on premise-gaps
                                                      segment sources search-goal search-start
                                                      search-cutoff search-threads
                                                      search-settings search-preamble
                                                      search-final chain-order)
  "Section 6.2.  A link opens :OPEN with nothing closing it; M4's split between :REALIZED and
   :CLOSED is enforced by WF14 rather than by the constructor."
  (make-ledger-record :id id :kind :link :statement statement :provenance provenance
                      :depends-on depends-on :premise-gaps premise-gaps :segment segment
                      :status :open :sources sources
                      :extra (list :from from :to to :intent intent :closed-by nil
                                   :evidence nil :validated nil :attempts nil
                                   :refuted-by nil :segment-bridge nil
                                   :search-goal search-goal :search-start search-start
                                   :search-cutoff search-cutoff
                                   :search-threads search-threads
                                   :search-settings search-settings
                                   :search-preamble (or search-preamble :stage)
                                   :search-final search-final :chain-order chain-order
                                   :recommendation nil :measured nil)))


(defun make-ledger-bound (id statement provenance &key for-link measured
                                                       interpretation-committed depends-on
                                                       premise-gaps segment sources)
  "Section 6.3.  There is no :REFUTES key here and the grammar has nowhere to write one; that
   is guard X1, and WF8 is its enforcement on the other side."
  (make-ledger-record :id id :kind :bound :statement statement :provenance provenance
                      :depends-on depends-on :premise-gaps premise-gaps :segment segment
                      :status :standing :sources sources
                      :extra (list :for-link for-link :measured measured
                                   :interpretation-committed interpretation-committed
                                   :segment-bridge nil)))


(defun make-ledger-question (id statement &key candidates blocks depends-on premise-gaps
                                               segment sources gap-candidate template
                                               (answer-kind :one-of))
  "Section 6.4.  :DEFAULT is always :UNKNOWN, and an answer of :UNKNOWN leaves the question
   open and writes no premise.  :ANSWER-KIND is :ONE-OF, :SUBSET-OF or :STATED -- some
   underdetermination is a choice, some is a subset of a pool, and some is a description
   nobody has written down; forcing all three into a single choice would be the convenient
   reading RO already refuses to take."
  (make-ledger-record :id id :kind :question :statement statement
                      :provenance (list :derived :grade 1 :by "underdetermination in the analysis")
                      :depends-on depends-on :premise-gaps premise-gaps :segment segment
                      :status :open :sources sources
                      :extra (list :candidates candidates :default :unknown
                                   :answer-kind answer-kind :template template :blocks blocks
                                   :answer nil :answer-premise nil
                                   :gap-candidate gap-candidate)))


(defun add-ledger-record (ledger record)
  "Appends RECORD and returns the ledger.  Order in the file follows order of creation, which
   keeps a hand-read ledger chronological."
  (when (ledger-record ledger (getf record :id))
    (error "Ledger already holds a record with id ~S; ids are never reused." (getf record :id)))
  (ledger-set-value ledger :records (append (getf ledger :records) (list record)))
  ledger)


;;; ---------------------------------------------------------------------------
;;; Amendment, retraction and discharge (sections 5, 7.1 and 9)
;;; ---------------------------------------------------------------------------

(defun amend-ledger-depends-on (ledger id clause note &optional (date (ledger-today)))
  "Appends one clause to ID's premise list.  The list is open-ended by construction, which is
   the plan's mitigation for building the ledger before the later extractors are known.  An
   amendment may LOWER standing from established to conditional; that is correct behaviour and
   the event records it."
  (let* ((record (ledger-record ledger id))
         (before (ledger-standing ledger id)))
    (ledger-set-value record :depends-on (append (getf record :depends-on) (list clause)))
    (ledger-add-event record date :amended note)
    (let ((after (ledger-standing ledger id)))
      (unless (eq before after)
        (ledger-add-event record date :standing-changed
                          (format nil "~(~A~) -> ~(~A~) on amendment" before after))))
    ledger))


(defun answer-ledger-question (ledger id answer statement by &optional (date (ledger-today)))
  "Section 6.4.  An answer creates a premise and never edits a standing by fiat: the premise is
   user-asserted, the question gains a clause resting on it, and everything downstream
   recomputes to CONDITIONAL on its own.  An answer of :UNKNOWN leaves the question open and
   writes nothing, which is what the standing default is for."
  (let ((record (ledger-record ledger id)))
    (unless (eq (getf record :kind) :question)
      (error "~S is a ~(~A~), not a question." id (getf record :kind)))
    (when (eq answer :unknown)
      (ledger-add-event record date :answered "answered :unknown; the question stays open")
      (return-from answer-ledger-question ledger))
    (ecase (or (getf record :answer-kind) :one-of)
      (:one-of
       (unless (member answer (getf record :candidates))
         (error "~S is not among the candidate answers of ~S." answer id)))
      (:subset-of
       (unless (and (listp answer) answer)
         (error "~S expects a subset of its candidates, as a non-empty list." id))
       (dolist (element answer)
         (unless (member element (getf record :candidates))
           (error "~S is not among the candidates of ~S." element id))))
      (:stated
       (unless (and (stringp answer) (plusp (length answer)))
         (error "~S expects a stated description, as a non-empty string." id))))
    (let ((premise-id (ledger-next-id ledger "PR")))
      (add-ledger-record ledger
        (make-ledger-premise premise-id statement
                             (list :user-asserted :by by :asked-as id :date date)
                             :segment (getf record :segment)))
      (ledger-set-value record :answer answer)
      (ledger-set-value record :answer-premise premise-id)
      (ledger-set-value record :status :answered)
      (ledger-set-value record :depends-on
                        (append (getf record :depends-on) (list (list premise-id))))
      (ledger-add-event record date :answered
                        (format nil "answered ~(~A~); recorded as premise ~(~A~)"
                                answer premise-id)))
    ledger))


(defun ledger-dependent-standings (ledger id)
  "The standings of ID's dependents as they are now, so a retraction can tell a real change
   from a recomputation that landed on the same answer."
  (let ((pairs nil))
    (dolist (each (ledger-dependents ledger id) (nreverse pairs))
      (push (cons each (ledger-standing ledger each)) pairs))))


(defun ledger-apply-retraction (ledger id cause date before)
  "What a retraction does to one dependent, decided by its RECOMPUTED standing and by nothing
   else.  A bound is never invalidated: the measurement happened, and what a retraction can
   change is whether the analysis still reaches the state it was measured from."
  (let ((record (ledger-record ledger id))
        (standing (ledger-standing ledger id))
        (was (cdr (assoc id before))))
    (cond ((eq (getf record :kind) :bound)
           (ledger-set-value record :status :orphaned)
           (ledger-add-event record date :orphaned
                             (format nil "~A retracted; start-state premise withdrawn" cause)))
          ((and (eq standing :unfounded) (member (getf record :kind) '(:link :question)))
           (ledger-set-value record :status :invalidated)
           (ledger-add-event record date :invalidated (format nil "~A retracted" cause)))
          ((and (eq standing :unfounded) (eq (getf record :kind) :premise))
           (ledger-set-value record :status :retracted)
           (ledger-add-event record date :retracted
                             (format nil "no surviving support after ~A was retracted" cause)))
          (t (ledger-add-event record date :standing-changed
                               (if (eq standing was)
                                 (format nil "~A retracted; a clause survived, standing still ~(~A~)"
                                         cause standing)
                                 (format nil "~A retracted; standing ~(~A~) -> ~(~A~)"
                                         cause was standing)))))))


(defun retract-ledger-premise (ledger id reason &optional (date (ledger-today)))
  "Section 9.  The premise is kept and marked, its dependents are recomputed, and nothing
   outside the dependents set is touched.  A retracted premise is never un-retracted:
   re-asserting the same proposition creates a new record with a new id, because reviving an
   id would silently restore dependents nobody re-examined."
  (let ((record (ledger-record ledger id)))
    (unless record
      (error "No ledger record ~S." id))
    (unless (eq (getf record :kind) :premise)
      (error "~S is a ~(~A~); only a premise is retracted." id (getf record :kind)))
    (let ((before (ledger-dependent-standings ledger id)))
      (ledger-set-value record :status :retracted)
      (ledger-add-event record date :retracted reason)
      (dolist (each (ledger-dependents ledger id))
        (ledger-apply-retraction ledger each id date before)))
    ledger))


(defun discharge-ledger-premise (ledger id by note &optional (date (ledger-today)))
  "Section 7.1, the upgrade path.  A guess later proved is marked :DISCHARGED and points at the
   derivation that replaced it; standing then looks through it, so dependents become
   ESTABLISHED without any of them being rewritten and the history of the guess survives."
  (let ((record (ledger-record ledger id))
        (derivation (ledger-record ledger by)))
    (unless (and record (eq (getf record :kind) :premise))
      (error "~S is not a premise." id))
    (unless derivation
      (error "No ledger record ~S to discharge ~S with." by id))
    (unless (eq (first (getf derivation :provenance)) :derived)
      (error "~S is not a derivation; a guess is discharged only by one." by))
    (ledger-set-value record :status :discharged)
    (ledger-set-value record :discharged-by by)
    (ledger-add-event record date :discharged note)
    ledger))


;;; ---------------------------------------------------------------------------
;;; Well-formedness (section 11).  Each check signals on the first violation.  An
;;; ill-formed ledger is a bug in whatever wrote it and should manifest at once.
;;; ---------------------------------------------------------------------------

(defun check-ledger-identity (ledger)
  "WF1, WF4, WF5: unique ids carrying their kind's prefix, every envelope key present, and a
   status in that kind's domain."
  (let ((seen nil))
    (dolist (record (getf ledger :records))
      (let* ((id (getf record :id))
             (kind (getf record :kind))
             (prefix (cdr (assoc kind *ledger-kind-prefixes*))))
        (unless prefix
          (error "WF5: ~S has unknown kind ~S." id kind))
        (when (member id seen)
          (error "WF1: id ~S appears twice; ids are unique and never reused." id))
        (push id seen)
        (unless (and (> (length (symbol-name id)) (length prefix))
                     (string-equal prefix (symbol-name id) :end2 (length prefix)))
          (error "WF1: id ~S does not carry the ~A prefix its kind requires." id prefix))
        (dolist (key *ledger-envelope-keys*)
          (unless (member key record)
            (error "WF4: ~S is missing the required key ~S." id key)))
        (unless (member (getf record :status) (rest (assoc kind *ledger-statuses*)))
          (error "WF5: ~S has status ~S, which is outside the ~(~A~) domain."
                 id (getf record :status) kind))))))


(defun check-ledger-references (ledger)
  "WF2: every id named anywhere in a record exists.  A dangling id must not be allowed to look
   like a dead disjunct and quietly kill a clause."
  (dolist (record (getf ledger :records))
    (let ((id (getf record :id))
          (named nil))
      (dolist (clause (getf record :depends-on))
        (setf named (append named clause)))
      (setf named (append named (getf record :blocks) (getf record :attempts)))
      (dolist (key '(:discharged-by :refuted-by :answer-premise :for-link))
        (when (getf record key)
          (push (getf record key) named)))
      (dolist (other named)
        (unless (ledger-record ledger other)
          (error "WF2: ~S names ~S, which is not in the ledger." id other))))))


(defun check-ledger-acyclic (ledger)
  "WF3: the :DEPENDS-ON graph is acyclic.  A cycle would make standing ill-defined and the
   closure walk non-terminating in any implementation less careful than this one."
  (dolist (record (getf ledger :records))
    (let ((id (getf record :id)))
      (when (member id (ledger-closure ledger id))
        (error "WF3: ~S depends on itself transitively." id)))))


(defun check-ledger-provenance (ledger)
  "WF6, WF7, WF11, WF12: the three species and their required fields, grade 3 confined to
   bounds, empty dependency lists confined to the three cases that may have one, and user
   assertions confined to premises."
  (dolist (record (getf ledger :records))
    (let* ((id (getf record :id))
           (kind (getf record :kind))
           (provenance (getf record :provenance))
           (species (first provenance))
           (grade (ledger-provenance-value provenance :grade)))
      (case species
        (:derived
         (unless (member grade '(1 2 3 4))
           (error "WF6: ~S is :DERIVED without a grade in 1-4." id))
         (when (and (eql grade 3) (not (eq kind :bound)))
           (error "WF7: ~S is grade 3 but is not a bound; grade 3 occurs nowhere else." id))
         (when (and (eq kind :bound) (not (eql grade 3)))
           (error "WF7: bound ~S is :DERIVED at grade ~S; a bound is grade 3." id grade)))
        (:user-asserted
         (unless (eq kind :premise)
           (error "WF12: ~S is user-asserted but is a ~(~A~); a guess is a premise." id kind)))
        (:search-measured
         (dolist (key '(:outcome :start-state :search-expression :cutoff :threads))
           (when (eq (getf (rest provenance) key :missing) :missing)
             (error "WF6/X2: ~S is :SEARCH-MEASURED without ~S; a measurement names its start state and its cutoff."
                    id key))))
        (t (error "WF6: ~S has provenance species ~S, which is not one of the three." id
                  species)))
      (when (and (null (getf record :depends-on))
                 (not (or (eq kind :bound)
                          (eq species :user-asserted)
                          (and (eq species :derived) (eql grade 1)))))
        (error "WF11: ~S rests on nothing; only a grade-1 derivation, a user assertion or a bound may."
               id)))))


(defun check-ledger-grades (ledger)
  "WF9, WF10, WF13: no grade-2 record resting on a search, every grade-4 record carrying its
   own proof obligation, and :SEGMENT :NONE confined to grade 1 and grade 2."
  (dolist (record (getf ledger :records))
    (let* ((id (getf record :id))
           (provenance (getf record :provenance))
           (grade (and (eq (first provenance) :derived)
                       (ledger-provenance-value provenance :grade))))
      (when (eql grade 2)
        (dolist (each (ledger-closure ledger id))
          (let ((beneath (ledger-record ledger each)))
            (when (or (eq (getf beneath :kind) :bound)
                      (eq (first (getf beneath :provenance)) :search-measured))
              (error "WF9: grade-2 ~S rests on search result ~S; no inductive proof may depend on a cutoff."
                     id each)))))
      (when (and (eql grade 4)
                 (null (getf record :premise-gaps))
                 (null (getf record :proof-obligation)))
        (error "WF10: grade-4 ~S states no outstanding obligation; a trace claim carries its own." id))
      (when (and (eq (getf record :segment) :none) (not (member grade '(1 2))))
        (error "WF13: ~S claims :SEGMENT :NONE, which needs a grade-1 or grade-2 derivation." id)))))


(defun check-ledger-closure-fields (ledger)
  "WF8, WF14, WF15, WF17: a bound closes nothing, a link is closed only when validated, a
   refutation names a derivation, and an answered question points at the premise it wrote."
  (dolist (record (getf ledger :records))
    (let ((id (getf record :id))
          (kind (getf record :kind)))
      (dolist (key '(:closed-by :refuted-by))
        (let ((value (getf record key)))
          (when (and value (symbolp value) (ledger-record ledger value)
                     (eq (getf (ledger-record ledger value) :kind) :bound))
            (error "WF8: ~S names bound ~S in ~S; an exhausted search closes nothing and refutes nothing."
                   id value key))))
      (when (eq kind :link)
        (when (and (eq (getf record :status) :closed)
                   (or (null (getf record :closed-by)) (not (getf record :validated))))
          (error "WF14: link ~S is :CLOSED without a validated closure; an unvalidated find is :REALIZED."
                 id))
        (when (eq (getf record :status) :refuted)
          (let ((by (ledger-record ledger (getf record :refuted-by))))
            (unless (and by
                         (eq (first (getf by :provenance)) :derived)
                         (member (ledger-provenance-value (getf by :provenance) :grade)
                                 '(1 2)))
              (error "WF15: link ~S is :REFUTED without a grade-1 or grade-2 derivation in :REFUTED-BY."
                     id)))))
      (when (and (eq kind :question) (eq (getf record :status) :answered))
        (let ((premise (ledger-record ledger (getf record :answer-premise))))
          (unless (and premise
                       (eq (first (getf premise :provenance)) :user-asserted)
                       (eq (ledger-provenance-value (getf premise :provenance) :asked-as) id))
            (error "WF17: answered question ~S does not point at a user-asserted premise naming it."
                   id)))))))


(defun ledger-segments-compatible-p (one other)
  "Two segments clash when they state different views, or one states ghosts absent against the
   other's present.  :NONE composes with anything, being a claim about every segment."
  (or (eq one :none) (eq other :none)
      (and (eq (getf one :view) (getf other :view))
           (not (and (eq (getf one :ghosts) :absent) (eq (getf other :ghosts) :present)))
           (not (and (eq (getf one :ghosts) :present) (eq (getf other :ghosts) :absent))))))


(defun check-ledger-segments (ledger)
  "WF16.  A ledger that silently composes a physical-view premise with a recording-view one
   would reproduce G14's defect one level up, where no reader would see it."
  (dolist (record (getf ledger :records))
    (let ((id (getf record :id))
          (segment (getf record :segment)))
      (dolist (each (ledger-direct-dependencies ledger id))
        (let ((beneath (ledger-record ledger each)))
          (unless (or (ledger-segments-compatible-p segment (getf beneath :segment))
                      (getf record :segment-bridge))
            (error "WF16: ~S depends on ~S across an incompatible segment with no :SEGMENT-BRIDGE."
                   id each)))))))


(defun check-ledger-bound-statements (ledger)
  "WF18, guard X4.  Refuses the word before it can be quoted out of the ledger and into a
   register as a verdict."
  (dolist (record (getf ledger :records))
    (when (eq (getf record :kind) :bound)
      (let ((statement (string-downcase (getf record :statement))))
        (dolist (word *ledger-bound-lint-words*)
          (when (search word statement)
            (error "WF18: bound ~S says ~S; a cost bound licenses no impossibility claim.  Restate it."
                   (getf record :id) word)))))))


(defun check-ledger-well-formed (ledger)
  "Section 11.  Signals on the first violation; it does not warn and continue."
  (check-ledger-identity ledger)
  (check-ledger-references ledger)
  (check-ledger-acyclic ledger)
  (check-ledger-provenance ledger)
  (check-ledger-grades ledger)
  (check-ledger-closure-fields ledger)
  (check-ledger-segments ledger)
  (check-ledger-bound-statements ledger)
  t)


;;; ---------------------------------------------------------------------------
;;; Reading and writing (section 12)
;;; ---------------------------------------------------------------------------

(defun ledger-canonical-record (record)
  "Envelope keys in specification order, then this kind's own keys, then every key the reader
   did not recognise, kept in the order it met them.  Unknown keys survive the round trip, so
   a hand annotation is never silently dropped."
  (let ((known (append *ledger-envelope-keys*
                       (rest (assoc (getf record :kind) *ledger-kind-keys*))))
        (ordered nil)
        (extra nil))
    (dolist (key known)
      (let ((tail (member key record)))
        (when tail
          (setf ordered (append ordered (list key (second tail)))))))
    (do ((tail record (cddr tail)))
        ((null tail))
      (unless (member (first tail) known)
        (setf extra (append extra (list (first tail) (second tail))))))
    (append ordered extra)))


(defun write-ledger-record (record stream)
  "One record per top-level form, one key per line, so a hand edit is easy and a diff is
   readable.  Dollar-prefixed wouldwork symbols in a statement or an action sequence are
   printed as symbols and never substituted into."
  (let ((canonical (ledger-canonical-record record))
        (firstp t))
    (format stream "(")
    (do ((tail canonical (cddr tail)))
        ((null tail))
      (if firstp
        (setf firstp nil)
        (format stream "~% "))
      (format stream "~S ~S" (first tail) (second tail)))
    (format stream ")~%~%")))


(defun read-realization-ledger (pathname)
  "Reads a ledger written by WRITE-REALIZATION-LEDGER, or hand-edited since.  *PACKAGE* is
   bound to :WW, where staged data already lives, and *READ-EVAL* is off: a ledger is data."
  (with-open-file (stream pathname :direction :input)
    (let ((*package* (find-package :ww))
          (*read-eval* nil)
          (header (read stream nil nil))
          (records nil))
      (unless (and (consp header) (eq (first header) :ledger-version))
        (error "~A does not begin with a (:ledger-version ...) form." pathname))
      (loop for form = (read stream nil nil)
            while form
            do (push form records))
      (list :version (second header)
            :problem (getf (cddr header) :problem)
            :written (getf (cddr header) :written)
            :records (nreverse records)))))


(defun write-realization-ledger (ledger pathname &optional (date (ledger-today)))
  "Generates into a sibling temporary and only then replaces the previous ledger, the same
   discipline WRITE-STATIC-CONSTRAINT-PROFILE uses, so a generator error preserves what was
   there.  As there, replacement is not fully atomic if the rename itself fails after the
   prior target is deleted; do not claim otherwise.  The rename target carries only NAME and
   TYPE because RENAME-FILE merges its argument against the file being renamed, and passing
   the directory again duplicates it whenever PATHNAME is relative."
  (check-ledger-well-formed ledger)
  (let* ((pathname (merge-pathnames pathname))
         (temporary (make-pathname :type "tmp" :defaults pathname)))
    (with-open-file (stream temporary :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*package* (find-package :ww))
            (*print-case* :downcase)
            (*print-pretty* t)
            (*print-right-margin* 96)
            (*print-length* nil)
            (*print-level* nil))
        (format stream ";;; Constraint-led method -- realization ledger for ~A.~%"
                (getf ledger :problem))
        (format stream ";;; Written by WRITE-REALIZATION-LEDGER.  Schema:~%")
        (format stream ";;;   doc/constraint-method/Status-Algebra-and-Record-Schema.md~%")
        (format stream ";;; This file is deliberately user-amendable.  Unrecognised keys~%")
        (format stream ";;; are preserved on the next write.~%~%")
        (format stream "(:ledger-version ~D :problem ~S :written ~S)~%~%"
                (getf ledger :version) (getf ledger :problem) date)
        (dolist (record (getf ledger :records))
          (write-ledger-record record stream))))
    (when (probe-file pathname)
      (delete-file pathname))
    (rename-file temporary (make-pathname :name (pathname-name pathname)
                                          :type (pathname-type pathname)))
    pathname))


;;; ---------------------------------------------------------------------------
;;; The reporter (section 13)
;;; ---------------------------------------------------------------------------

(defun report-ledger-premise-line (ledger id)
  (let ((record (ledger-record ledger id)))
    (format t "        ~(~A~)  [~(~A~), ~(~A~)]  ~A~%"
            id (first (getf record :provenance)) (getf record :status)
            (getf record :statement))))


(defun report-ledger-open-link (ledger record)
  "An open link, what blocks it, and what would close it.  The closing sentence is fixed
   rather than authored per link, so M4's requirement cannot drift from record to record."
  (let ((id (getf record :id)))
    (format t "    ~(~A~)  ~A~%" id (or (getf record :intent) (getf record :statement)))
    (format t "      from: ~A~%" (or (getf record :from) "UNSTATED"))
    (format t "      to:   ~A~%" (or (getf record :to) "UNSTATED"))
    (format t "      standing ~(~A~); status ~(~A~)~%"
            (ledger-standing ledger id) (getf record :status))
    (let ((guesses (ledger-live-guesses ledger id)))
      (format t "      blocking premises (~D):~%" (length guesses))
      (dolist (guess guesses)
        (report-ledger-premise-line ledger guess)))
    (let ((gaps (getf record :premise-gaps)))
      (when gaps
        (format t "      premise gaps (~D): ~{~A~^; ~}~%" (length gaps) gaps)))
    (dolist (attempt (getf record :attempts))
      (let ((bound (ledger-record ledger attempt)))
        (format t "      attempt ~(~A~): ~A -- a cost bound, not a refutation~%"
                attempt (getf bound :statement))))
    (format t "      would close: an action sequence found and validated under~%")
    (format t "        VALIDATE-ACTION-SEQUENCE, or a grade-1 or grade-2 derivation named~%")
    (format t "        in :CLOSED-BY.  No cost bound can close or refute it.~%")))


(defun report-ledger-bound-strength (bound)
  "What an exhaustion's own engine readings say about how strong it is.  A cost bound is grade 3
   either way; this says how much of the space it actually covered."
  (let ((provenance (getf bound :provenance)))
    (ecase (getf (rest provenance) :cutoff-truncated :unknown)
      (:unknown
       (format t "      cutoff coverage UNKNOWN: truncation was not reliably measured; ~
                  do not infer~%        complete reachable-space exhaustion.~%"))
      ((t)
      (format t "      the cutoff truncated the space: nodes with successors were cut off, so ~
                 this covers strictly less~%        than the reachable space from that start.~%"))
      ((nil)
       (format t "      no truncation observed at depth-cutoff nodes; this does not certify ~
                  unpruned~%        reachable-space exhaustion. Still a cost bound, and ~
                  still not an impossibility.~%")))
    (when (ledger-provenance-value provenance :pruning)
      (format t "      pruning in force: ~A.  An exhaustion under pruning excludes solutions in ~
                 pruned branches.~%" (ledger-provenance-value provenance :pruning)))))


(defun report-ledger-bounds (ledger)
  "Section 10, guard X3.  Bounds print in their own section, never interleaved with link
   verdicts, and the closing sentence is a template."
  (let ((bounds (remove-if-not (lambda (record) (eq (getf record :kind) :bound))
                               (getf ledger :records))))
    (format t "~%  COST BOUNDS  (GRADE 3 -- NOT IMPOSSIBILITY)  (~D)~%" (length bounds))
    (dolist (bound bounds)
      (let ((provenance (getf bound :provenance)))
        (format t "    ~(~A~)  attempted ~(~A~); status ~(~A~)~%"
                (getf bound :id) (or (getf bound :for-link) :none) (getf bound :status))
        (format t "      exhausted at cutoff ~S, threads ~S, from: ~A~%"
                (ledger-provenance-value provenance :cutoff)
                (ledger-provenance-value provenance :threads)
                (ledger-provenance-value provenance :start-state))
        (format t "      expression: ~A~%"
                (ledger-provenance-value provenance :search-expression))
        (format t "      grade 3: a cost bound relative to that start state.  It licenses no~%")
        (format t "        impossibility claim.~%")
        (when (getf bound :interpretation-committed)
          (format t "      committed before the run: ~A~%"
                  (getf bound :interpretation-committed)))
        (format t "      evidence: ~A~%"
                (or (ledger-provenance-value provenance :run) "NONE RECORDED"))
        (when (eq (ledger-provenance-value provenance :outcome) :exhausted)
          (report-ledger-bound-strength bound))))))


(defun report-ledger-kind (ledger kind label statuses)
  "One roll-up section: the records of KIND whose status is in STATUSES, one line each."
  (let ((rows (remove-if-not (lambda (record)
                               (and (eq (getf record :kind) kind)
                                    (member (getf record :status) statuses)))
                             (getf ledger :records))))
    (format t "~%  ~A (~D)~%" label (length rows))
    (dolist (record rows)
      (format t "    ~(~A~)  standing ~(~A~); status ~(~A~)  ~A~%"
              (getf record :id) (ledger-standing ledger (getf record :id))
              (getf record :status) (getf record :statement)))))


(defun report-realization-ledger (ledger)
  "The whole ledger, in the order a session reads it: what is open and what blocks it first,
   then the settled records, then the cost bounds under their own heading, then the questions,
   then what died.  Nothing is omitted: a retracted record is evidence."
  (format t "~%~%LEDGER  REALIZATION LEDGER  [interactive-phase state for ~A]~%"
          (getf ledger :problem))
  (format t "-------------------------------------------------------------~%")
  (format t "  SCOPE: a record of what is claimed and what it rests on.  It derives nothing.~%")
  (format t "  STANDING is computed from the dependency closure and never stored.  A record~%")
  (format t "  printed CONDITIONAL rests on at least one live guess or one cost bound.~%")
  (format t "  NOTE: RO prints \"status CONDITIONAL\" for what this reporter calls STANDING.~%")
  (format t "  This file's :STATUS is the stored lifecycle, which RO has no analogue for.~%")
  (let ((open (remove-if-not (lambda (record)
                               (and (eq (getf record :kind) :link)
                                    (member (getf record :status) '(:open :realized))))
                             (getf ledger :records))))
    (format t "~%  OPEN LINKS (~D)~%" (length open))
    (dolist (record open)
      (report-ledger-open-link ledger record)))
  (report-ledger-kind ledger :link "SETTLED LINKS" '(:closed :refuted))
  (report-ledger-kind ledger :premise "PREMISES IN FORCE" '(:in-force))
  (report-ledger-kind ledger :premise "PREMISES DISCHARGED" '(:discharged))
  (report-ledger-bounds ledger)
  (report-ledger-kind ledger :question "OPEN QUESTIONS" '(:open))
  (report-ledger-kind ledger :question "ANSWERED QUESTIONS" '(:answered))
  (report-ledger-kind ledger :premise "RETRACTED OR REFUTED PREMISES" '(:retracted :refuted))
  (report-ledger-kind ledger :link "INVALIDATED LINKS" '(:invalidated))
  (format t "~%  no obligation, no necessity and no plan witness is derived by this component.~%"))


;;; ---------------------------------------------------------------------------
;;; T3, the search recommender.  For each open link it emits a recommendation that
;;; is runnable as printed, and it writes the reading of each outcome BEFORE the run,
;;; so the interpretation of an exhaustion is never chosen after the outcome is known.
;;; It runs nothing: running a recommendation is search and needs its own approval.
;;;
;;; C3 HOLDS.  Every problem term -- the problem name, the start facts, the goal form --
;;; comes from the ledger as data.  The only names in this code are Wouldwork's own
;;; interface: STAGE, WW-SET, SOLVE-SUBGOAL, WW-UNDO, *THREADS*, *DEPTH-CUTOFF*.
;;; ---------------------------------------------------------------------------

(defun ledger-bound-cutoff (record)
  "The cutoff a bound was measured at, or NIL when it was uncapped or is not a bound."
  (let ((cutoff (ledger-provenance-value (getf record :provenance) :cutoff)))
    (and (integerp cutoff) cutoff)))


(defun ledger-deepest-attempt (ledger id)
  "The attempt against ID measured at the greatest finite cutoff, or NIL.  This is what a new
   recommendation has to be compared against before it may go deeper."
  (let ((deepest nil)
        (best nil))
    (dolist (attempt (getf (ledger-record ledger id) :attempts) deepest)
      (let ((cutoff (ledger-bound-cutoff (ledger-record ledger attempt))))
        (when (and cutoff (or (null best) (> cutoff best)))
          (setf best cutoff
                deepest attempt))))))


(defun ledger-default-threads (link)
  "Threads for LINK when it states none.  A chain link is 0 because the engine forbids anything
   else; a link with its own start takes the configured default."
  (if (eq (getf link :search-start) :chain)
    0
    *ledger-default-search-threads*))


(defun ledger-form-text (form)
  "FORM printed as Lisp source the user can paste.  *PRINT-CASE* rather than a ~( ~) directive,
   because that directive downcases string contents too and would corrupt a goal carrying one."
  (let ((*package* (find-package :ww))
        (*print-case* :downcase)
        (*print-pretty* nil)
        (*print-length* nil)
        (*print-level* nil)
        (*print-readably* nil))
    (format nil "~S" form)))


(defun ledger-search-commands (ledger link cutoff threads)
  "The command lines of a recommendation, in the order they must be entered.  Settings come
   after STAGE because staging resets problem settings and state, and *THREADS* comes before
   *DEPTH-CUTOFF* because crossing the serial/parallel boundary forces a system rebuild.
   A :CONTINUE link emits the SOLVE-SUBGOAL line alone: it continues an open goal chain, and
   printing the preamble would tell the reader to stage, which discards the very chain the
   milestone is continuing.  The first real use of this component printed that preamble for
   every chained milestone, which is the bug this branch fixes."
  (let ((start (getf link :search-start))
        (goal (getf link :search-goal))
        (lines nil))
    (unless (eq (getf link :search-preamble) :continue)
      (push "(progn (ql:quickload :wouldwork) (in-package :ww))" lines)
      (push (format nil "(stage ~(~A~))" (getf ledger :problem)) lines)
      (push (format nil "(ww-set *threads* ~D)" threads) lines)
      (push (format nil "(ww-set *depth-cutoff* ~D)" cutoff) lines)
      (dolist (setting (getf link :search-settings))
        (push (format nil "(ww-set ~(~A~) ~A)" (car setting) (ledger-form-text (cdr setting)))
              lines)))
    (push (cond ((eq start :chain)
                 (format nil "(solve-subgoal ~A)" (ledger-form-text goal)))
                ((stringp start)
                 (format nil "(solve-subgoal ~A ~A)" start (ledger-form-text goal)))
                (t (format nil "(solve-subgoal ~A ~A)"
                           (ledger-form-text start) (ledger-form-text goal))))
          lines)
    (nreverse lines)))


(defun ledger-search-cautions (link)
  "The cautions that belong beside the commands, selected by what this recommendation does.
   They are templates, not prose authored per link, so none of them can drift."
  (let ((cautions
          (list "staging resets settings and state, so set the parameters after (stage ...)."
                "crossing the serial/parallel boundary with (ww-set *threads* ...) rebuilds the system."
                "the goal is unquoted by design; a quoted goal installs (quote ...), read as trivially true."
                "*depth-cutoff* 0 or negative means no cutoff at all, which is why it is set explicitly.")))
    (when (eq (getf link :search-preamble) :continue)
      (setf cautions
            (list "this milestone CONTINUES an open goal chain; enter it in the session that opened it."
                  "do NOT re-stage: staging discards the chain, and every earlier milestone would have to be re-run."
                  "crossing the serial/parallel boundary with (ww-set *threads* ...) discards it too.")))
    (if (eq (getf link :search-start) :chain)
      (setf cautions
            (append cautions
                    (list "the one-argument form is goal chaining, which the engine runs at *threads* 0 only.")))
      (setf cautions
            (append cautions
                    (list "the two-argument form DISCARDS any active goal chain; one (ww-undo) restores it."
                          "the two-argument form runs in any thread mode, which is why this one may be parallel."))))
    cautions))


(defun ledger-evidence-retrieval (link)
  "Where a find leaves its action sequence, which is not the same place in all three cases and
   cost this method a failed command the first time it mattered.  A MID-CHAIN milestone sets
   *SOLUTION-PATHS* to NIL deliberately -- there is no solution until the chain finishes -- and
   its own path lives in the session's phase record.  Only the FINAL milestone publishes a
   cumulative path, and the engine restores *START-STATE* to the chain's origin when it does."
  (cond ((and (eq (getf link :search-start) :chain) (not (getf link :search-final)))
         (list "this is a MID-CHAIN milestone, so *solution-paths* is NIL by design."
               "  (defparameter *phase* (car (last (goal-chain-session-phases *goal-chain-session*))))"
               "  (defparameter *path* (solution.path (goal-chain-phase-solution *phase*)))"
               "  (validate-action-sequence (goal-chain-phase-source-state *phase*) *path*"
               "                            :goal-test (symbol-function 'goal-fn) :verbose t)"
               "validate from the PHASE'S OWN source state, not from the chain's origin: only the"
               "first phase starts at the origin, and every later one starts where its predecessor"
               "ended.  Validating does not modify the state it is given, so the chain survives it."
               "  (format t \"~S~%\" *path*)   prints the raw action forms, for the record."))
        ((eq (getf link :search-start) :chain)
         (list "this is the FINAL milestone: finish with (solve), which composes the chain."
               "the engine then puts ONE cumulative path in *solution-paths* and restores"
               "*start-state* to the chain's origin, so the composition check is one call:"
               "  (validate-action-sequence *start-state* (solution.path (first *solution-paths*))"
               "                            :goal-test (symbol-function 'goal-fn) :verbose t)"))
        (t
         (list "this milestone searches from a stated start, so its path is published directly:"
               "  (validate-action-sequence *start-state* (solution.path (first *solution-paths*))"
               "                            :goal-test (symbol-function 'goal-fn) :verbose t)"))))


(defun ledger-success-reading (ledger id cutoff)
  "What a find would establish, written before the run.  M4 is in the sentence: a find makes a
   link :REALIZED and closes it only once the composition validates."
  (let ((guesses (ledger-live-guesses ledger id)))
    (format nil "a found action sequence realizes ~(~A~) from the stated start within ~D ~
                 actions.  It makes ~(~A~) :REALIZED.  It CLOSES ~(~A~) only once ~
                 VALIDATE-ACTION-SEQUENCE accepts the composition, and the closure then still ~
                 stands on ~D live premise(s)~@[: ~{~(~A~)~^, ~}~].  It establishes nothing ~
                 about any other link."
            id cutoff id id (length guesses) guesses)))


(defun ledger-exhaustion-reading (id cutoff)
  "What an exhaustion would establish, written before the run.  This is the sentence the whole
   schema exists to make unrevisable after the fact."
  (format nil "an exhaustion is a GRADE-3 COST BOUND relative to the stated start state at ~
               cutoff ~D.  It establishes that no realization of ~(~A~) exists within ~D ~
               actions from that state, and nothing further: not that none exists deeper, not ~
               that ~(~A~) is impossible, and not a refutation of any premise.  It is filed ~
               as a :BOUND record, which cannot close or refute a link."
          cutoff id cutoff id))


(defun ledger-search-ready-p (link)
  "Whether a runnable command can be printed at all.  A half-command is worse than a refusal:
   the user would have to edit it, and an edited recommendation is not the one that was
   committed to."
  (and (getf link :search-goal)
       (getf link :search-start)
       (integerp (getf link :search-cutoff))
       (> (getf link :search-cutoff) 0)))


(defun recommend-ledger-search (ledger id &key cutoff threads deepen (date (ledger-today)))
  "Writes the recommendation for link ID and stores it on the record, so T4 can file the bound
   against the reading that was committed rather than against a memory of it.
   NO SILENT DEEPENING: a cutoff greater than a previous attempt's signals unless DEEPEN is
   passed, and the recommendation then carries which bound it is going past."
  (let* ((link (ledger-record ledger id))
         (cutoff (or cutoff (getf link :search-cutoff)))
         (threads (or threads (getf link :search-threads) (ledger-default-threads link)))
         (previous (ledger-deepest-attempt ledger id))
         (measured (and previous (ledger-bound-cutoff (ledger-record ledger previous)))))
    (unless (eq (getf link :kind) :link)
      (error "~S is a ~(~A~); a search is recommended for a link." id (getf link :kind)))
    (when (and (eq (getf link :search-start) :chain) (not (eql threads 0)))
      (error "~S searches the active goal chain, which the engine runs single-threaded only. ~
              Give it its own :SEARCH-START to run at ~D threads, or set :SEARCH-THREADS 0."
             id threads))
    (unless (and (integerp cutoff) (> cutoff 0))
      (error "~S has no positive depth cutoff.  *DEPTH-CUTOFF* 0 means no cutoff at all, and ~
              an uncapped search is not a bounded recommendation." id))
    (when (and measured (> cutoff measured) (not deepen))
      (error "~S was already measured to cutoff ~D by ~S.  Recommending ~D would deepen the ~
              search; pass :DEEPEN T to say so on the record." id measured previous cutoff))
    (ledger-set-value link :search-cutoff cutoff)
    (ledger-set-value link :search-threads threads)
    (ledger-set-value link :recommendation
                      (list :date date
                            :cutoff cutoff
                            :threads threads
                            :start (getf link :search-start)
                            :premises (ledger-closure ledger id)
                            :guesses (ledger-live-guesses ledger id)
                            :standing (ledger-standing ledger id)
                            :deepens (and measured (> cutoff measured)
                                          (list previous measured))
                            :success (ledger-success-reading ledger id cutoff)
                            :exhaustion (ledger-exhaustion-reading id cutoff)))
    (ledger-add-event link date :amended
                      (format nil "search recommended at cutoff ~D, threads ~D" cutoff threads))
    ledger))


(defun report-ledger-recommendation (ledger link)
  "One link's recommendation: what it rests on, the commands verbatim, the cautions, and the
   two readings as they were committed."
  (let* ((id (getf link :id))
         (recommendation (getf link :recommendation)))
    (format t "    ~(~A~)  ~A~%" id (or (getf link :intent) (getf link :statement)))
    (format t "      standing ~(~A~); rests on ~:[nothing~;~:*~{~(~A~)~^, ~}~]~%"
            (ledger-standing ledger id) (ledger-closure ledger id))
    (cond ((null recommendation)
           (format t "      NO RECOMMENDATION COMMITTED.  Call RECOMMEND-LEDGER-SEARCH on ~
                      ~(~A~) first.~%" id))
          ((not (ledger-search-ready-p link))
           (format t "      NOT YET RUNNABLE.  Missing:~%")
           (unless (getf link :search-goal)
             (format t "        :SEARCH-GOAL, the unquoted goal form for this link.~%"))
           (unless (getf link :search-start)
             (format t "        :SEARCH-START, either :CHAIN or the start facts to search ~
                        from.~%"))
           (unless (and (integerp (getf link :search-cutoff))
                        (> (getf link :search-cutoff) 0))
             (format t "        :SEARCH-CUTOFF, a positive depth.~%")))
          (t
           (format t "      run, in order:~%")
           (dolist (line (ledger-search-commands ledger link (getf recommendation :cutoff)
                                                 (getf recommendation :threads)))
             (format t "        ~A~%" line))
           (format t "      cautions:~%")
           (dolist (caution (ledger-search-cautions link))
             (format t "        ~A~%" caution))
           (format t "      committed ~A, before the run:~%" (getf recommendation :date))
           (format t "        success:    ~A~%" (getf recommendation :success))
           (format t "        exhaustion: ~A~%" (getf recommendation :exhaustion))
           (format t "      on a find, the action sequence is here:~%")
           (dolist (line (ledger-evidence-retrieval link))
             (format t "        ~A~%" line))
           (when (getf recommendation :deepens)
             (format t "      DEEPENING: ~(~A~) measured this link to cutoff ~D; this run ~
                        raises it to ~D.~%"
                     (first (getf recommendation :deepens))
                     (second (getf recommendation :deepens))
                     (getf recommendation :cutoff))
             (format t "        The earlier bound is not superseded until a new bound is filed ~
                        against the same start state.~%"))))))


(defun ledger-chain-links (ledger)
  "The links that ride the goal chain, in CHAIN ORDER -- which is not file order once a
   decomposition has been refined.  A link inserted to split an earlier one is appended to the
   file but belongs before it in the chain, and the replay must follow the chain."
  (let ((links (remove-if-not (lambda (record)
                                (and (eq (getf record :kind) :link)
                                     (eq (getf record :search-start) :chain)
                                     (getf record :search-goal)))
                              (getf ledger :records))))
    (stable-sort (copy-list links) #'<
                 :key (lambda (record) (or (getf record :chain-order) most-positive-fixnum)))))


(defun report-chain-replay (ledger)
  "Everything needed to re-establish the chain in a COLD REPL, in order.  A goal chain lives in
   session state and does not survive a restart: the milestones already settled must be re-run
   to get back to where the next one starts.  Printing only the next line, as the per-link
   recommendation does, is right inside a live session and useless after a restart."
  (let ((links (ledger-chain-links ledger))
        (settled 0))
    (format t "~%~%CHAIN REPLAY  [for ~A]~%" (getf ledger :problem))
    (format t "--------------------------------------------------------------~%")
    (format t "  A goal chain is SESSION STATE and does not survive a REPL restart.  To resume~%")
    (format t "  from a cold image, enter these in order.  The settled milestones are re-run,~%")
    (format t "  not re-derived: their outcomes are already filed and are not filed again.~%~%")
    (format t "        (progn (ql:quickload :wouldwork) (in-package :ww))~%")
    (format t "        (stage ~(~A~))~%" (getf ledger :problem))
    (format t "        (ww-set *threads* 0)~%")
    (format t "        (ww-set *depth-cutoff* ~D)~%"
            (or (getf (first links) :search-cutoff) 8))
    (dolist (link links)
      (when (member (getf link :status) '(:closed :realized))
        (incf settled))
      (format t "        (solve-subgoal ~A)~@[   ; ~(~A~)~]~%"
              (ledger-form-text (getf link :search-goal))
              (and (member (getf link :status) '(:closed :realized))
                   (getf link :status))))
    (format t "~%  ~D of ~D milestones are already settled; the rest are the work.~%"
            settled (length links))
    (format t "  A milestone whose cutoff was raised must be re-run at the raised cutoff, which~%")
    (format t "  its own recommendation states; this block prints the chain's opening cutoff.~%")))


(defun report-search-recommendations (ledger)
  "T3's entry point.  Every open link, with its recommendation or with what it still needs.
   Nothing here runs a search: a run needs its own approval."
  (format t "~%~%RECOMMENDED SEARCHES  [for ~A]~%" (getf ledger :problem))
  (format t "--------------------------------------------------------------~%")
  (format t "  SCOPE: what to run and what each outcome would establish.  It runs nothing.~%")
  (format t "  Every reading below was written BEFORE its run, which is what stops an~%")
  (format t "  exhaustion from being reinterpreted once its outcome is known.~%")
  (let ((open (remove-if-not (lambda (record)
                               (and (eq (getf record :kind) :link)
                                    (member (getf record :status) '(:open :realized))))
                             (getf ledger :records))))
    (format t "~%  OPEN LINKS (~D)~%" (length open))
    (dolist (link open)
      (report-ledger-recommendation ledger link)))
  (format t "~%  a recommendation is not an approval to run it.~%"))


;;; ---------------------------------------------------------------------------
;;; T4, the result ingester.  It takes back what a local run produced and files it at
;;; its correct grade, AGAINST THE READING COMMITTED BEFORE THE RUN.  It refuses an
;;; outcome for a link carrying no recommendation, because filing one would mean the
;;; reading was chosen after the outcome was known, which is the error the whole
;;; schema exists to prevent.
;;;
;;; It touches the ledger and nothing else.  M2 holds: no generated output is edited.
;;; ---------------------------------------------------------------------------

(defun ledger-committed-recommendation (link)
  "LINK's committed recommendation, or a signal.  An outcome filed without one would be an
   outcome interpreted after the fact."
  (let ((recommendation (getf link :recommendation)))
    (unless recommendation
      (error "~S carries no committed recommendation.  Run RECOMMEND-LEDGER-SEARCH before the ~
              search, not after it: the reading of an outcome is committed beforehand or it is ~
              not committed at all." (getf link :id)))
    recommendation))


(defun ledger-run-provenance (recommendation outcome run truncated pruning)
  "The :SEARCH-MEASURED provenance of one run, carrying every field X2 makes mandatory plus the
   two the engine reports about the quality of an exhaustion."
  (unless (member truncated '(t nil :unknown))
    (error "Cutoff truncation must be T, NIL, or :UNKNOWN, not ~S." truncated))
  (list :search-measured
        :outcome outcome
        :start-state (format nil "~A" (getf recommendation :start))
        :search-expression (format nil "cutoff ~D, threads ~D, as recommended ~A"
                                   (getf recommendation :cutoff)
                                   (getf recommendation :threads)
                                   (getf recommendation :date))
        :cutoff (getf recommendation :cutoff)
        :threads (getf recommendation :threads)
        :cutoff-truncated truncated
        :pruning pruning
        :run run))


(defun ledger-premise-clauses (ids)
  "A closure of ids as single-disjunct clauses, which is what a filed record rests on."
  (let ((clauses nil))
    (dolist (id ids (nreverse clauses))
      (push (list id) clauses))))


(defun ingest-ledger-exhaustion (ledger id &key run (truncated :unknown) pruning nodes seconds
                                                (date (ledger-today)))
  "File an exhausted run as a GRADE-3 COST BOUND.  It is a :BOUND record, so WF8 makes it
   incapable of closing or refuting anything, and its statement is generated from a template
   so it cannot say in prose what its type forbids it to mean.
   TRUNCATED is T for confirmed truncation, NIL for a reliable negative observation
   at cutoff nodes, or :UNKNOWN (the default) for absent/unreliable instrumentation.
   NIL does not certify unpruned exhaustion. PRUNING is what
   the run reported pruned -- symmetry or repeated states -- because an exhaustion under
   pruning is weaker again."
  (let* ((link (ledger-record ledger id))
         (recommendation (ledger-committed-recommendation link))
         (bound-id (ledger-next-id ledger "BD")))
    (add-ledger-record ledger
      (make-ledger-bound bound-id
                         (format nil "no realization of ~(~A~) was found within ~D actions from ~
                                      the stated start state"
                                 id (getf recommendation :cutoff))
                         (ledger-run-provenance recommendation :exhausted run truncated pruning)
                         :for-link id
                         :measured (list :nodes nodes :seconds seconds)
                         :interpretation-committed (getf recommendation :exhaustion)
                         :depends-on (ledger-premise-clauses (getf recommendation :premises))
                         :segment (getf link :segment)
                         :sources (list (format nil "run evidence ~A" run))))
    (ledger-set-value link :attempts (append (getf link :attempts) (list bound-id)))
    (ledger-add-event link date :amended
                      (format nil "exhausted at cutoff ~D; filed as ~(~A~), a cost bound"
                              (getf recommendation :cutoff) bound-id))
    bound-id))


(defun ingest-ledger-find (ledger id actions &key validated run nodes seconds
                                                  (date (ledger-today)))
  "File a found segment.  The link becomes :REALIZED, and :CLOSED only when VALIDATED is true,
   which is M4 in the code rather than in a resolution: a find that has not composed under
   VALIDATE-ACTION-SEQUENCE is an intermediate result, not a plan witness.  Standing stays
   CONDITIONAL until it validates, whatever its premises say."
  (let* ((link (ledger-record ledger id))
         (recommendation (ledger-committed-recommendation link)))
    (unless actions
      (error "A find files its action sequence as evidence; none was given for ~S." id))
    (ledger-set-value link :closed-by
                      (append (ledger-run-provenance recommendation :found run nil nil)
                              (list :interpretation-committed (getf recommendation :success))))
    (ledger-set-value link :evidence actions)
    (ledger-set-value link :validated (and validated t))
    (ledger-set-value link :status (if validated :closed :realized))
    (ledger-set-value link :measured (list :nodes nodes :seconds seconds))
    (ledger-add-event link date (if validated :closed :realized)
                      (format nil "~D action~:P found at cutoff ~D~:[; NOT yet validated~;, ~
                                   validated under VALIDATE-ACTION-SEQUENCE~]"
                              (length actions) (getf recommendation :cutoff) validated))
    id))


(defun file-ledger-surprise (ledger id question candidates &optional (date (ledger-today)))
  "M5.  A surprising outcome is not a result to be absorbed; it is a question the schema failed
   to ask.  It is recorded here as a question marked :GAP-CANDIDATE, and
   REPORT-LEDGER-GAP-CANDIDATES prints it for the problem's Constraint-Schema-Gaps.txt.  That
   file is hand-maintained, so nothing writes to it from here."
  (let ((question-id (ledger-next-id ledger "QN")))
    (add-ledger-record ledger
      (make-ledger-question question-id question
                            :candidates candidates
                            :blocks (list id)
                            :segment (getf (ledger-record ledger id) :segment)
                            :gap-candidate t
                            :sources (list (format nil "surprise while ingesting a run for ~
                                                        ~(~A~)" id))))
    (ledger-add-event (ledger-record ledger id) date :amended
                      (format nil "surprise filed as ~(~A~), a schema-gap candidate"
                              question-id))
    question-id))


(defun ingest-ledger-result (ledger id outcome &key actions validated run (truncated :unknown) pruning
                                                    nodes seconds surprise surprise-candidates
                                                    (date (ledger-today)))
  "The one entry point a session uses after a local run.  OUTCOME is :FOUND or :EXHAUSTED.
   A surprise is filed alongside either, never instead of one."
  (let ((filed (ecase outcome
                 (:found (ingest-ledger-find ledger id actions :validated validated :run run
                                             :nodes nodes :seconds seconds :date date))
                 (:exhausted (ingest-ledger-exhaustion ledger id :run run :truncated truncated
                                                       :pruning pruning :nodes nodes
                                                       :seconds seconds :date date)))))
    (when surprise
      (file-ledger-surprise ledger id surprise surprise-candidates date))
    filed))


(defun report-ledger-gap-candidates (ledger)
  "The surprises this ledger has collected, as text for the problem's Constraint-Schema-Gaps.txt.
   Printed, never written: that file is hand-maintained and M2's regeneration rule does not
   cover it."
  (let ((candidates (remove-if-not (lambda (record)
                                     (and (eq (getf record :kind) :question)
                                          (getf record :gap-candidate)))
                                   (getf ledger :records))))
    (format t "~%~%SCHEMA-GAP CANDIDATES  [for ~A]  (~D)~%" (getf ledger :problem)
            (length candidates))
    (format t "--------------------------------------------------------------~%")
    (format t "  M5: a surprising outcome is a question the schema failed to ask.  Append these~%")
    (format t "  to the problem's Constraint-Schema-Gaps.txt BY HAND, stated domain-generally.~%")
    (format t "  Nothing here writes to that file.~%")
    (dolist (candidate candidates)
      (format t "~%    ~(~A~)  blocks ~:[nothing~;~:*~{~(~A~)~^, ~}~]~%"
              (getf candidate :id) (getf candidate :blocks))
      (format t "      question: ~A~%" (getf candidate :statement))
      (format t "      candidate answers: ~:[none enumerated~;~:*~{~(~S~)~^, ~}~]~%"
              (getf candidate :candidates))
      (format t "      raised by: ~{~A~^; ~}~%" (getf candidate :sources)))))


;;; ---------------------------------------------------------------------------
;;; T5, the question generator.  RO narrates its unresolved premises as prose a reader
;;; has to interpret.  This turns each one into an enumerated question with a candidate
;;; answer set and a default of unknown, so the interactive phase asks for intuition
;;; instead of asking for a reading.
;;;
;;; ANSWERING NEVER UPGRADES ANYTHING.  An answer goes through ANSWER-LEDGER-QUESTION,
;;; which writes a :USER-ASSERTED premise and makes the question rest on it.  A record
;;; above it recomputes to CONDITIONAL, by section 8, and no status is touched.
;;;
;;; C3 HOLDS.  Every template below is domain-general.  The segment description, the
;;; support names, the witness pool and the device names arrive as data in the caller's
;;; scenario, exactly as RO's own scenario does.
;;;
;;; ANTI-DRIFT.  Each template records the fragment of RO's printed text it stands for,
;;; in :NARRATED-AS.  The acceptance checks assert that every one of those fragments is
;;; still present in tech/constraint-profile.lisp, so a change to RO's narration that
;;; this table has not followed fails the checks rather than going unnoticed.
;;; ---------------------------------------------------------------------------

(defparameter *ledger-question-templates*
  '((:key :segment-necessity
     :narrated-as "necessity of this segment"
     :kind :one-of
     :slots (:segment)
     :candidates (:occurs-in-every-solution :occurs-in-some-solution-only)
     :statement "Does the segment described as ~A occur in every solution, or only in some?"
     :note "RO allocates inside a stated segment; it never establishes that the segment
            occurs.  An answer here is a premise, not a finding.")
    (:key :ghost-absence
     :narrated-as "ghost absence"
     :kind :one-of
     :slots (:segment)
     :candidates (:no-ghost-occupants-exist :ghost-occupants-exist)
     :statement "In the segment described as ~A, do any ghost occupants exist?"
     :note "S2 makes occupancy layer-blind, so a ghost depresses a support exactly as a
            live body does.  This changes the size of the pool, not the matching rule.")
    (:key :agent-occupancy
     :narrated-as "agent occupancy"
     :kind :one-of
     :slots (:segment :supports)
     :candidates (:agent-occupies-none-of-them :agent-occupies-one-of-them)
     :statement "In the segment described as ~A, does the agent occupy any of the required
                 supports ~(~S~)?"
     :note "The agent is one of the occupants.  Whether it is spent on a support decides
            whether the remaining pool can cover the rest.")
    (:key :replacement-witnesses
     :narrated-as "replacement witnesses"
     :kind :one-of
     :slots (:segment)
     :candidates (:a-replacement-witness-exists :no-replacement-witness-exists)
     :statement "In the segment described as ~A, can a witness removed from a required
                 support be replaced by another before the aggregate is read?"
     :note "Removing the last eligible witness closes the device in the same propagation.
            Whether a replacement exists is what separates a momentary loss from a lasting
            one, and RO does not decide it.")
    (:key :recorder-transitions
     :narrated-as "recorder transitions"
     :kind :one-of
     :slots (:segment)
     :candidates (:a-cycle-boundary-can-occur :no-cycle-boundary-occurs-here)
     :statement "Can a recorder cycle boundary occur within the segment described as ~A?"
     :note "A boundary removes ghost state and normalizes the shadow, so it changes the
            available pool mid-segment.  No extractor reads the cycle structure; that is
            G8, and this question is the hand reading it asks for.")
    (:key :per-support-reach
     :narrated-as "per-support restriction beyond availability UNRESOLVED"
     :kind :one-of
     :slots (:supports)
     :candidates (:every-available-witness-can-reach-every-support
                  :some-witness-support-pair-is-out-of-reach)
     :statement "For the required supports ~(~S~), can every available witness reach every
                 one of them?"
     :note "RO's eligibility restricts by availability alone and says so at the claim site.
            Reach is the first of the three restrictions it leaves open.")
    (:key :per-support-elevation
     :narrated-as "reach, elevation, history"
     :kind :one-of
     :slots (:supports)
     :candidates (:elevation-admits-every-pairing :some-pairing-is-blocked-by-elevation)
     :statement "For the required supports ~(~S~), does elevation admit every
                 witness-to-support pairing?"
     :note "The second restriction RO leaves open.  S5 is not written, so this is a hand
            reading until it is.")
    (:key :per-support-history
     :narrated-as "occupancy history"
     :kind :one-of
     :slots (:supports)
     :candidates (:occupancy-history-admits-every-pairing
                  :some-pairing-is-excluded-by-history)
     :statement "For the required supports ~(~S~), does occupancy history admit every
                 witness-to-support pairing?"
     :note "The third restriction RO leaves open.  A body that was never where it would
            have to start from is not eligible, however available it is.")
    (:key :availability
     :narrated-as "availability UNKNOWN; obligations report demand only."
     :kind :subset-of
     :slots (:segment :pool)
     :candidates nil
     :statement "In the segment described as ~A, which of the present occupants ~(~S~) are
                 actually available as witnesses?"
     :note "Presence is not availability.  A declared type extent states who exists, not
            who is free; an absent answer leaves RO reporting demand only.")
    (:key :segment-provenance
     :narrated-as "NONE STATED"
     :kind :stated
     :slots nil
     :candidates nil
     :statement "What segment is this obligation stated in?  Describe the view, the cycle
                 and the occupant presence in one sentence."
     :note "A label rather than a premise, but RO prints NONE STATED for it and an
            unlabelled obligation cannot be cited later.")
    (:key :undeclared-control
     :narrated-as "UNRESOLVED; no control aggregate is declared for it."
     :kind :one-of
     :slots (:device)
     :candidates (:controlled-outside-the-control-algebra :not-a-controlled-device)
     :statement "No control aggregate is declared for ~(~A~).  Is it controlled by
                 something outside the control algebra, or not a controlled device at all?"
     :note "RO refuses to invent an aggregate.  Either answer is a premise about the
            instance, not a reading of the control table."))
  "One entry per unresolved premise RO narrates.  :NARRATED-AS is the fragment of RO's
   printed text the entry stands for, and the acceptance checks assert it is still there.")


(defun ledger-collapse-whitespace (text)
  "TEXT on one line, every run of whitespace reduced to a single space.  The templates are
   written across lines for readability and must not print that way."
  (let ((out (make-string-output-stream))
        (pending nil)
        (started nil))
    (dotimes (index (length text) (get-output-stream-string out))
      (let ((character (char text index)))
        (if (member character '(#\Space #\Newline #\Tab #\Return))
          (setf pending started)
          (progn (when pending
                   (write-char #\Space out)
                   (setf pending nil))
                 (write-char character out)
                 (setf started t)))))))


(defun ledger-question-template (key)
  "The template with KEY, or a signal.  A missing key is a bug in the caller, not a case to
   default around."
  (let ((template (find key *ledger-question-templates* :key (lambda (entry) (getf entry :key)))))
    (unless template
      (error "No question template ~S." key))
    template))


(defun ledger-question-slot-value (slot context)
  "One slot of a template statement, taken from CONTEXT.  An absent slot prints as a stated
   placeholder rather than as NIL, so a half-filled question reads as underspecified instead
   of as a claim about nothing."
  (let ((value (getf context slot)))
    (cond (value value)
          ((eq slot :segment) "an unnamed segment")
          ((eq slot :supports) '(the-required-supports))
          ((eq slot :pool) '(the-present-occupants))
          ((eq slot :device) 'the-device)
          (t "unstated"))))


(defun ledger-question-statement (template context)
  "TEMPLATE's statement with its slots filled from CONTEXT, on one line."
  (let ((arguments nil))
    (dolist (slot (getf template :slots))
      (push (ledger-question-slot-value slot context) arguments))
    (ledger-collapse-whitespace
      (apply #'format nil (getf template :statement) (nreverse arguments)))))


(defun ledger-question-candidates (template context)
  "TEMPLATE's candidate set.  A :SUBSET-OF question takes its candidates from the pool the
   caller stated, because the answerable set is data and not vocabulary."
  (if (eq (getf template :kind) :subset-of)
    (copy-list (ledger-question-slot-value :pool context))
    (copy-list (getf template :candidates))))


(defun ledger-question-present-p (ledger key blocks)
  "Whether this question has already been generated for these blocked records.  Generation is
   idempotent: running it twice must not fill the ledger with duplicates, and ids are
   append-only so a duplicate could never be cleaned up afterwards."
  (find-if (lambda (record)
             (and (eq (getf record :kind) :question)
                  (eq (getf record :template) key)
                  (equal (getf record :blocks) blocks)))
           (getf ledger :records)))


(defun generate-ledger-question (ledger key context blocks segment date)
  "Generate one question from its template, or return the one already there."
  (or (ledger-question-present-p ledger key blocks)
      (let* ((template (ledger-question-template key))
             (id (ledger-next-id ledger "QN"))
             (record (make-ledger-question id (ledger-question-statement template context)
                                           :candidates (ledger-question-candidates template
                                                                                   context)
                                           :answer-kind (getf template :kind)
                                           :template key
                                           :blocks blocks
                                           :segment segment
                                           :sources (list (ledger-collapse-whitespace
                                                            (getf template :note))))))
        (add-ledger-record ledger record)
        (ledger-add-event record date :opened
                          (format nil "generated from template ~(~S~)" key))
        record)))


(defun generate-ledger-questions (ledger scenario &key blocks supports pool undeclared
                                                       (date (ledger-today)))
  "Every unresolved premise RO narrates, emitted as an enumerated question.  SCENARIO is the
   same plist RO takes; SUPPORTS, POOL and UNDECLARED are the problem terms RO computes and
   this component is not given a staged problem to compute, so the caller states them.
   Returns the questions, new and existing alike."
  (let* ((context (list :segment (getf scenario :provenance)
                        :supports supports
                        :pool pool))
         (segment (list :view (getf scenario :view)
                        :cycle (getf scenario :cycle)
                        :ghosts (getf scenario :ghosts)))
         (questions nil))
    (dolist (key '(:segment-necessity :ghost-absence :agent-occupancy :replacement-witnesses
                   :recorder-transitions :per-support-reach :per-support-elevation
                   :per-support-history))
      (push (generate-ledger-question ledger key context blocks segment date) questions))
    (unless (consp (getf scenario :available-witnesses))
      (push (generate-ledger-question ledger :availability context blocks segment date)
            questions))
    (unless (getf scenario :provenance)
      (push (generate-ledger-question ledger :segment-provenance context blocks segment date)
            questions))
    (dolist (device undeclared)
      (push (generate-ledger-question ledger :undeclared-control
                                      (append (list :device device) context)
                                      blocks segment date)
            questions))
    (nreverse questions)))


(defun report-ledger-question (ledger record)
  "One question, its candidates numbered, and the exact call that answers it.  A question the
   reader has to turn back into a command is prose again."
  (let ((id (getf record :id))
        (index 0))
    (format t "~%    ~(~A~)  [~(~A~)]~%" id (or (getf record :answer-kind) :one-of))
    (when (getf record :blocks)
      (format t "      blocks ~{~(~A~)~^, ~}~%" (getf record :blocks)))
    (format t "      ~A~%" (getf record :statement))
    (if (getf record :candidates)
      (dolist (candidate (getf record :candidates))
        (incf index)
        (format t "        ~D. ~(~S~)~%" index candidate))
      (format t "        (a stated description; there is nothing to choose from)~%"))
    (format t "        ~D. :unknown  -- the default; it writes no premise and leaves this open~%"
            (1+ index))
    (dolist (note (getf record :sources))
      (format t "      why it matters: ~A~%" note))
    (format t "      answer with: (answer-ledger-question ledger '~(~A~) <answer> ~
                    \"<the premise in your words>\" \"D\")~%" id)
    (when (getf record :answer-premise)
      (format t "      ANSWERED ~(~A~), recorded as premise ~(~A~); standing ~(~A~)~%"
              (getf record :answer) (getf record :answer-premise)
              (ledger-standing ledger id)))))


(defun report-ledger-questionnaire (ledger)
  "T5's entry point.  Every open question, enumerated, with the call that answers it."
  (format t "~%~%QUESTIONS  [for ~A]~%" (getf ledger :problem))
  (format t "--------------------------------------------------------------~%")
  (format t "  Each of these is a premise the analysis could not settle.  Answer from~%")
  (format t "  intuition; the default is :unknown and costs nothing.~%")
  (format t "  AN ANSWER IS A PREMISE, NOT A FINDING.  It is filed as user-asserted, and~%")
  (format t "  everything resting on it reads CONDITIONAL for as long as it stands.~%")
  (let ((open (remove-if-not (lambda (record)
                               (and (eq (getf record :kind) :question)
                                    (eq (getf record :status) :open)))
                             (getf ledger :records))))
    (format t "~%  OPEN (~D)~%" (length open))
    (dolist (record open)
      (report-ledger-question ledger record)))
  (let ((answered (remove-if-not (lambda (record)
                                   (and (eq (getf record :kind) :question)
                                        (eq (getf record :status) :answered)))
                                 (getf ledger :records))))
    (format t "~%  ANSWERED (~D)~%" (length answered))
    (dolist (record answered)
      (format t "    ~(~A~)  ~(~A~) -> premise ~(~A~); standing ~(~A~)~%"
              (getf record :id) (getf record :answer) (getf record :answer-premise)
              (ledger-standing ledger (getf record :id)))))
  (format t "~%  answering settles what rests on the answer, never that the answer is true.~%"))
