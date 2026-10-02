# Status Algebra and Record Schema — T1

Written 2026-09-20 under the approval recorded in
`doc/constraint-led-solving/archive/Implementation-Plan-2026-09-25.md` for **T1, the written
specification only**. Implementing it is T2 and needs its own approval; nothing
here authorizes code.

This is the design T2–T5 are built from. Archived plans retain development
history; this file is authoritative for the shape of a ledger record and for
what happens to one when a premise is withdrawn.

**Revised by T27 (2026-09-26), before code**, under T27's approved acceptance
criteria, scope (a). The revision makes the stage the ledger's unit and the
ledger file its own record: the `:stage` kind (§6.5, §7.5), stage supersession
(§9.1), ledger version 2 and the file-of-record discipline (§12, §12.1), rules
WF19–WF25 (§11), the entry points T27 adds (§13), and worked example C (§15a).
Links, bounds, questions and version-1 ledgers keep their meaning unchanged.
The change is listed as item 12 of §16.

**Status, 2026-09-30.** The ledger is not used for new problems (Problem-Solving
Guide, Optional tools); this schema is kept for crelay-topo's version-1 ledger,
now at `doc/constraint-led-solving/archive/crelay-topo-experiment/`, together with
the `constraint-evidence/` files cited below. Per-problem paths in this document
(`doc/problems/<problem>/Constraint-Realization-Ledger.txt`,
`constraint-evidence/...`) describe the layout it was written for.

**Contamination.** Nothing sealed was opened to write this. It was written
against RO's existing output shape
(`doc/problems/crelay-topo/Constraint-Role-Obligations.txt` and
`constraint-evidence/ro-first-run-2026-09-20.txt`), the grade vocabulary in
`Constraint-Continuation-Prompt.txt`, and the abstract model's AM1–AM9.

---

## 0  What this settles, and what it does not

**Settles**, as T1's entry requires:

- the provenance vocabulary and its grades (§4);
- how a record names the premises it rests on, open-endedly (§5);
- retraction semantics — what dies, what survives (§9);
- how an exhaustion result is stored so it cannot be read as impossibility (§10);
- how a conditional obligation stays distinct from an established one (§8).

**Does not settle**, deliberately:

- *Placement.* Left open when this was written; settled during T2 as
  `tech/constraint-ledger.lisp`, its own loadable diagnostic. See §17. The
  schema itself is placement-neutral; nothing below depends on the answer.
- *The recommender's content.* What a search recommendation says is T3.
- *Question wording.* How underdetermination becomes an enumerated question is
  T5; §6.4 fixes only the record it writes.

---

## 1  The two invariants, mechanized

The plan states two invariants. A schema that merely restates them in prose has
not enforced them. Each is discharged by a structural feature, named here so T2
can be checked against it.

**I1 — A user guess is a premise, not a fact.**
Discharged by §8: standing is *computed from the dependency closure at print
time and never stored*. No record can be printed ESTABLISHED while an in-force
user-asserted premise sits anywhere beneath it. There is no field an optimistic
caller could set to claim otherwise.

**I2 — An exhausted search is a cost bound, not a refutation.**
Discharged by §10 and by an asymmetry in the grammar: a *found* segment is
evidence attached to the link it closes, while an *exhaustion* is its own
record attached to nothing, because it closes nothing. `:bound` is not an
admissible value anywhere a closure or a refutation is named. The error that
produced T22 and the pre-test-12 alcove verdict is not available to write down.

---

## 2  Ids

Ids are stable, append-only and never renumbered, the same discipline as G1–G14
and register §7.x. Five prefixes, one per kind:

    PR<n>   premise
    LK<n>   link
    BD<n>   bound
    QN<n>   question
    ST<n>   stage          (T27; version-2 ledgers only)

A record is never deleted. A record that dies is given a terminal status with a
reason. The next free id for a prefix is one above the highest `<n>` present;
because nothing is ever removed, that is safe and needs no stored counter.

Ids are ledger-local. A ledger belongs to one problem and lives at
`doc/problems/<problem>/Constraint-Realization-Ledger.txt`. Cross-problem
references, if they are ever wanted, are out of scope here.

---

## 3  The record envelope

Every record, of every kind, is a plist carrying these keys. All are required;
an absent key is an ill-formed record, not a defaulted one.

    :id            PR3 | LK2 | BD1 | QN4 | ST2
    :kind          :premise | :link | :bound | :question | :stage
    :statement     a string; what the record asserts, in one sentence
    :provenance    §4
    :depends-on    §5, a list of clauses, possibly empty
    :premise-gaps  a list of strings: premises known to be missing but not
                   yet given ids.  RO's "unresolved premises" line maps here.
    :segment       §3.1, or :none
    :status        §7, kind-specific
    :sources       a list of strings naming files, sections or relations
    :events        §3.2, append-only

Optional, kind-specific keys are listed per kind in §6.

**Unknown keys are preserved.** The ledger is program-written and
user-amendable, which makes it the one generated artifact of this method that
M2 does not seal. A reader that meets a key it does not know keeps it and the
writer writes it back unchanged. A hand annotation is therefore never silently
dropped, and a later component can add a key without a reader change.

### 3.1  Segment

Obligations in this method are segment-relative, and the segment axis has no
extractor (G8). A record carries the segment it was derived in, in the shape
RO's scenario already uses:

    :segment (:view :physical | :recording
              :cycle :none | :open | :closed
              :ghosts :absent | :present | :unknown)

`:none` means the record is segment-independent and must be true in every
segment — a claim strong enough that WF13 requires a grade-1 or grade-2
derivation to make it.

A stage spans several of the plan's segments (§6.5). Its `:segment` is the
segment in force **at its endpoint**, the state the next stage starts from. The
plan's segments themselves stay in the plan-data file and are not copied into
the ledger.

### 3.2  Events

    :events ((:date "2026-09-20" :event :opened :by "T2" :note "")
             (:date "2026-09-21" :event :retracted :by "D" :note "...")
             ...)

`:event` is one of `:opened`, `:amended`, `:answered`, `:discharged`,
`:retracted`, `:refuted`, `:invalidated`, `:orphaned`, `:superseded`,
`:realized`, `:closed`, `:standing-changed`. Append-only; an event is never
edited or removed. This is where the audit trail lives, so no other field needs
to carry history.

---

## 4  Provenance vocabulary

Exactly three species, mutually exclusive. `:provenance` holds one of:

    (:derived :grade 1|2|3|4 :by "<what produced it>")
    (:user-asserted :by "<who>" :asked-as QN<n> | nil :date "<date>")
    (:search-measured :outcome :found | :exhausted
                      :start-state "<description>"
                      :search-expression "<the expression as run>"
                      :cutoff <depth or :none>
                      :threads <n>
                      :run "<evidence file>")

The grades are the method's own, unchanged from the continuation prompt:

| Grade | Meaning | Where it may appear |
|---|---|---|
| 1 | DEFINITIONAL: follows from predicate definitions and static facts | any kind |
| 2 | INDUCTIVE: initial case and preservation by every applicable action | any kind |
| 3 | COST BOUND: exhaustion at a cutoff, relative to its initial state | **`:bound` only** |
| 4 | TRACE/ORDERING: a statement about plans, with its own proof obligation | `:link`, `:premise`, `:stage` |

Three rules follow, and they carry most of the weight of §10:

- **Grade 3 occurs only on a `:bound` record, and a `:bound` record closes
  nothing.** One rule, stated twice in the machinery: WF7 and WF8.
- **No grade-2 record may depend on a search cutoff.** WF9: a grade-2 record's
  dependency closure contains no `:bound` and no `:search-measured` record.
  This is the continuation prompt's rule, mechanized.
- **A grade-4 record carries its own proof obligation.** WF10: its
  `:premise-gaps` is non-empty, or it names the obligation in
  `:proof-obligation`. A trace claim with nothing outstanding is either a
  mislabelled grade-2 or an overclaim.

`:user-asserted` never carries a grade. That is the point of I1: a guess is not
weakly derived, it is not derived.

A stage is always grade 4 (WF20): it is a claim about the plan's order — this
stage, from that start, reaches this intent — and its obligation is discharged
only by a validated endpoint.

**Shape note.** A provenance is a species keyword followed by a plist, so the
plist begins one past the head and a `GETF` over the whole form reads the pairs
off by one. `LEDGER-PROVENANCE-VALUE` is the only correct accessor.

---

## 5  Dependency shape

    :depends-on ((PR1) (PR3 PR7) (PR9))

A **conjunction of clauses**, each clause a **disjunction of record ids**. The
example reads: PR1 **and** (PR3 **or** PR7) **and** PR9.

Why not a flat list. RO keeps control alternatives separate and refuses to sum
mutually exclusive ones; AM1–AM4 are of the form "not all of these". A flat
premise list flattens that distinction on the first retraction: withdrawing one
of two alternatives would kill a record that still has the other. The clause
form is the smallest shape that preserves what RO already reports.

**Open-endedness.** The list is open by construction — a component may append a
clause to an existing record at any time, recording an `:amended` event. This is
the plan's mitigation for building the ledger before T6–T9 are known. Appending
a clause may *lower* a record's standing from ESTABLISHED to CONDITIONAL; that
is correct behaviour, not an error, and the writer records a
`:standing-changed` event when it happens.

**Nothing is cached.** Only the direct edges above are stored. The transitive
closure and the dependents set are recomputed on every query. A stored closure
would go stale on the first amendment and the staleness would be invisible.

**An empty `:depends-on`** claims the record rests on nothing. WF11 permits it
only for `:derived` grade 1, for `:user-asserted`, and for `:bound` — a
measurement rests on its start state, which is its own field, not a premise.

---

## 6  The five kinds

### 6.1  `:premise`

A proposition the analysis uses and has not necessarily established.

    :statement     the proposition
    :discharged-by PR<n> | nil     §7.1

A premise is the only kind a user assertion may be. An answer to a question
becomes a premise (§6.4); it never becomes a fact.

### 6.2  `:link`

A required realization step: get from one described state to another.

    :from             "<state description>"
    :to               "<state description>"
    :intent           "<what this link is for>"
    :closed-by        nil | (:derived ...) | (:search-measured :outcome :found ...)
    :evidence         nil | (<action> ...)      the action sequence found
    :validated        t | nil                   VALIDATE-ACTION-SEQUENCE result
    :attempts         (BD<n> ...)               bounds measured against this link
    :refuted-by       nil | LK/PR id of a grade-1 or grade-2 derivation

`:attempts` is informational. It records that a search was run and exhausted; it
is read by the reporter and by T3 when it proposes a deeper bound. It is **not**
a dependency edge and it is **not** a closure. WF8 forbids a `BD` id in
`:closed-by` or `:refuted-by`.

`:evidence` holds the action sequence exactly as the run produced it, including
dollar-prefixed symbols, which the writer must not transform.

T3 adds six more link keys, holding the problem terms a runnable recommendation
needs — as data, so C3 still holds of the code:

    :search-goal      the goal form, UNQUOTED.  A quoted goal installs (quote ...),
                      which the translator reads as trivially true.
    :search-start     :chain (the next milestone of the active chain), a list of
                      dynamic facts, or a string naming a form to evaluate.
                      Since T14 (2026-09-24) a string names the variable holding a
                      standalone search checkpoint
    :search-archive   T14: optional; the saved checkpoint archive a string start
                      imports, relative to the repository root.  Required unless
                      :search-preamble is :continue
    :search-cutoff    a positive integer.  *depth-cutoff* 0 or negative means no
                      cutoff at all, so it is never a default
    :search-threads   0 unless the link states otherwise
    :search-settings  further (parameter . value) pairs this link needs, since
                      staging restores everything else from the problem spec
    :recommendation   the recommendation as committed: its date, cutoff, threads,
                      start (and since T14 its archive), the premise closure and live guesses at the time, the
                      bound it deepens past if any, and the success and exhaustion
                      readings.  T4 files a bound against this text rather than
                      against a memory of it.
    :measured         a find's node and second counts

**Threads are decided by the form, not by preference.** `*threads*` is 16 for a link
that states its own start and 0 for a `:chain` link, because
`VALIDATE-CONTINUATION-PRECONDITIONS` signals unless `*threads*` is 0 and the
one-argument `SOLVE-SUBGOAL` is goal chaining. Asking for a parallel chain search
signals; nothing is coerced.

Links remain valid in both ledger versions. The stage procedure (§6.5) does not
create them; they are kept for version-1 ledgers and for any component that
still records a sub-step as a link.

### 6.3  `:bound`

A cost measurement. See §10; its mandatory fields are the ones that make it
unreadable as an impossibility.

    :for-link       LK<n> | nil     which link was being attempted
    :for-stage      ST<n> | nil     which stage was being attempted (T27)
    :measured       plist: nodes, seconds, depth reached — optional detail

At most one of `:for-link` and `:for-stage` is non-nil (WF23). A bound filed
against a stage lists, in its `:depends-on`, the stage its search started
from, or nothing when it started from the initial state, so that a
supersession upstream orphans it (§9.1).

There is no `:refutes` key. The grammar has nowhere to write one.

### 6.4  `:question`

Underdetermination converted into something the user can answer from intuition.

    :candidates      (<answer> ...)   an enumerated set
    :default         :unknown         always present, always :unknown
    :blocks          (<record id> ...)
    :answer          nil | <one of :candidates> | :unknown
    :answer-premise  nil | PR<n>

**Answering creates a premise; it never edits a record's standing.** T5's
acceptance criterion — "answering never silently upgrades a conditional
obligation to an established one" — is discharged here: the only thing an answer
may write is a new `:premise` record with `(:user-asserted ... :asked-as QN<n>)`
provenance, whose id goes in `:answer-premise`. Everything downstream recomputes
its standing from that premise and stays CONDITIONAL, because §8 says an
in-force user assertion in the closure forces CONDITIONAL.

Answering also appends the clause `(PR<n>)` to the question's own
`:depends-on`, so the question reads CONDITIONAL for as long as its answer is a
guess rather than ESTABLISHED because the question itself was well posed.
`ANSWER-LEDGER-QUESTION` performs the whole operation; nothing else should write
these four fields by hand.

T5 adds three more keys, and a third answer shape:

    :answer-kind   :one-of (the default), :subset-of, or :stated
    :template      the question template this was generated from, or nil
    :gap-candidate t when T4 filed this from a surprise, per M5

`:answer-kind` exists because not all underdetermination is a choice. Some of it
is a subset of a stated pool — which of the present occupants are actually
available — and some is a description nobody has written down, such as the segment
RO prints as NONE STATED. Forcing all three into a single choice would be the
convenient reading RO already refuses to take. `:unknown` is accepted in every
kind and writes no premise.

An answer of `:unknown` leaves the question `:open` and writes no premise.

### 6.5  `:stage` (T27)

One stage of D's approved stage plan (Problem-Solving Guide, Phase 2): the unit
the Phase 3 loop realizes. The plan's content — intent, segments, required
devices — lives in the plan-data file (Extractor Specifications §13.2); the
stage record carries what the ledger adds to it: which plan stage this is, what
it rests on, how it was realized, and where it ended.

    :plan-file         "<path of the plan-data file, relative to the repository root>"
    :plan-stage        "<the plan stage's :id string in that file>"
    :intent            "<the stage's intent, as the plan states it>"
    :starts-from       :initial | ST<n>    the stage whose endpoint this starts from
    :check             nil | (:label :pass | :conditional | :conflict
                              :date "<date>" :run "<CP evidence file>")
    :realization       nil | :hand | :search | :mixed
    :endpoint          nil | (:checkpoint "<archive path>" :actions <n>
                              :sha256 "<hex>")
    :closed-by         nil | (:derived ...) | (:search-measured :outcome :found ...)
    :validated         t | nil     the endpoint's validation from the initial state
    :attempts          (BD<n> ...)     bounds measured against this stage
    :refuted-by        nil | PR/ST id of a grade-1 or grade-2 derivation
    :superseded-by     nil | ST<n>     the stage that replaces this one (§9.1)
    :proof-obligation  "<what the endpoint must show>"
    :segment-bridge    as for links and bounds (WF16)

**Provenance.** Always `(:derived :grade 4 :by "<plan file> stage <plan-stage>")`
with a non-empty `:proof-obligation` (WF20). The plan is D's, so the stage's
`:depends-on` carries a clause on the user-asserted premise recording D's
approval of the plan, one clause per trick of D's the stage uses, and, unless
it starts from `:initial`, a clause naming its `:starts-from` stage (WF19). A
stage therefore stays CONDITIONAL while any of D's premises beneath it is a
guess, even after its endpoint validates: validation establishes that the
endpoint is reached, not that the plan was the right one. That is I1, applied
to plans.

**`:check`** records the CP run (Extractor Specifications §13) the stage passed
before it entered the ledger; `nil` means it has not been checked. A stage whose
check label is `:conflict` cannot be realized or closed (WF25).

**`:endpoint`** names the checkpoint archive exported at the stage's end, with
its action count from the initial state and its SHA-256, the same hash the
Handoff records. The archive holds the action sequence, so a stage has no
`:evidence` key.

`:actions` may be **less** than the archive's own action count. A stage that
ends inside a longer validated sequence names that sequence's archive and the
number of its actions that end the stage; its endpoint is the state after those
first actions. Import replays and validates every action of the archive, so it
passes through that state. This is how a plan whose early stages were validated
as a prefix of a later sequence, without their own export, is recorded
honestly: the stage names the archive that contains its endpoint, not an
archive nobody made.

**`:attempts`** is informational, exactly as for a link: never a dependency
edge, never a closure (WF8).

---

## 7  The status algebra

`:status` is the stored **lifecycle**. It is not the conditional/established
distinction; that is §8, computed. Keeping them apart is what makes I1
unbreakable, and it is the one place where this schema is richer than RO's
printed line rather than merely a transcription of it.

### 7.1  Premise

| Status | Meaning | Entered by |
|---|---|---|
| `:in-force` | currently relied upon | creation |
| `:discharged` | later derived from established records | a derivation recorded in `:discharged-by` |
| `:retracted` | withdrawn by the user, or by the component that asserted it | §9 |
| `:refuted` | shown false | a grade-1 or grade-2 derivation of its negation |

`:discharged` is the upgrade path. When a guess is later proved, a **new**
derived premise record is created and the guess is marked
`:discharged :discharged-by PR<n>`. Standing then treats the discharged premise
as transparent: it forwards to its discharging record. Dependents become
ESTABLISHED without being touched, and the history of the guess survives.

**A retracted premise is never un-retracted.** Re-asserting the same
proposition creates a new record with a new id. Reviving an id would silently
restore dependents that were invalidated for a reason nobody re-examined.

### 7.2  Link

| Status | Meaning | Entered by |
|---|---|---|
| `:open` | not realized | creation |
| `:realized` | a segment was found; composition not yet validated | T4 ingesting a `:found` result |
| `:closed` | realized **and** `:validated t` | independent validation |
| `:refuted` | shown impossible | a grade-1 or grade-2 derivation named in `:refuted-by` |
| `:invalidated` | a clause it rested on went empty | §9 |

The split between `:realized` and `:closed` is M4 mechanized: a forced skeleton
or an unvalidated find is an intermediate result, not a plan witness. WF14
forbids `:closed` with `:validated nil`.

### 7.3  Bound

| Status | Meaning |
|---|---|
| `:standing` | the measurement holds of its start state |
| `:orphaned` | a premise its start state assumed was retracted |
| `:superseded` | a later, deeper bound measured the same start state |

A bound is **never** `:invalidated`. The measurement happened; what a retraction
can change is whether the analysis still reaches that start state, and that is
what `:orphaned` says. An orphaned bound keeps its number and its evidence file
and still prints, under its own heading.

### 7.4  Question

`:open` → `:answered` (an answer was given and a premise written) | `:withdrawn`
(no longer blocks anything) | `:invalidated` (every record it blocked was
invalidated).

### 7.5  Stage (T27)

| Status | Meaning | Entered by |
|---|---|---|
| `:open` | in the approved plan, not yet realized | creation |
| `:realized` | an endpoint was exported; not yet validated from the initial state | a search find, or a hand sequence not yet validated |
| `:closed` | realized **and** `:validated t` | validation of the endpoint from the initial state |
| `:refuted` | shown impossible | a grade-1 or grade-2 derivation named in `:refuted-by` |
| `:invalidated` | a clause it rested on went empty | §9, §9.1 |
| `:superseded` | D revised the plan and this stage was replaced | §9.1 |

A search-found stage stays `:realized` with `:validated nil` unless D asks for a
replay, as the guide's checkpoint rules already say of search-found phases; a
hand-derived stage is validated before it is closed. A `:superseded` stage keeps
its endpoint, attempts and events, and is never revived: re-adopting it means a
new stage record with a new id, for the reason §7.1 gives for premises.

---

## 8  Standing — computed, never stored

Every record has a standing, recomputed on each query and on each print:

    ESTABLISHED   nothing in its dependency closure is a live guess or a bound
    CONDITIONAL   something in its closure is
    UNFOUNDED     some clause of some record in its closure has no surviving
                  disjunct

The rule, in order:

1. If any clause in the record's own `:depends-on`, or in any record reachable
   through those clauses, has every disjunct in status `:retracted`,
   `:refuted` or `:superseded` → **UNFOUNDED**. (`:superseded` is a stage's
   status; a bound's `:superseded` is never a disjunct, because a bound is
   never depended on.)
2. Otherwise, if the closure contains an `:in-force` premise whose provenance is
   `:user-asserted`, or any `:bound` record, or a `:search-measured` record with
   `:outcome :exhausted` → **CONDITIONAL**.
3. Otherwise, if the record is a `:link` or a `:stage` whose `:closed-by` is a
   `:found` result with `:validated nil` → **CONDITIONAL**, regardless of the
   rest.
4. Otherwise → **ESTABLISHED**.

A `:discharged` premise is transparent: step 2 looks through it to its
`:discharged-by` record.

Supersession changes the standing of a stage's **dependents**, never of the
superseded stage itself: its own clauses are as alive as before, so it keeps
the standing its closure gives it. Its status says it left the plan.

**On vocabulary.** RO prints `grade 1; status CONDITIONAL`. That printed word is
this schema's *standing*, not its `:status`. The ledger's reporter prints
`grade 1; standing CONDITIONAL` and a NOTE saying so, so the two are never
confused when a ledger line and an RO line sit in the same session's output.
This is the "preserve rather than flatten" requirement in T1's entry: RO's line
is kept exactly, and the lifecycle it had no room for is added beside it rather
than folded into it.

---

## 9  Retraction

`RETRACT-LEDGER-PREMISE` takes a ledger, a `PR` id, a reason and a date. It does
exactly this and nothing else:

1. Set that premise's `:status` to `:retracted`; append a `:retracted` event
   carrying the reason and who gave it. **The record is kept.**
2. Compute the dependents set: the transitive closure of the inverse
   `:depends-on` edge.
3. For each dependent, recompute standing (§8) and act on the result only:
   - standing became **UNFOUNDED** and the record is a `:link`, `:stage` or
     `:question` → status `:invalidated`, with an `:invalidated` event naming
     the retracted id;
   - standing became UNFOUNDED and the record is a `:premise` → status
     `:retracted`, event naming the cause. A premise with no surviving support
     is withdrawn, not merely marked;
   - standing is still CONDITIONAL — some clause kept a surviving disjunct →
     **no status changes; a `:standing-changed` event is appended either way**,
     saying whether the standing moved or the clause simply survived. Every
     dependent a retraction touched carries a trace of it, so a later reader
     never has to infer from silence that nothing happened;
   - the record is a `:bound` whose start state assumed the retracted premise
     → status `:orphaned`. Its number is preserved.
4. Records outside the dependents set are not touched, not re-derived and not
   re-printed as changed. This is the second half of I1: a retraction must not
   discard results that did not depend on the guess.

Cascade is by *empty clause*, not by *mention*. A record listing
`(PR3 PR7)` survives the retraction of PR3. That is the whole reason for §5's
shape, and it is T2's sharpest acceptance test.

A stage already `:superseded` keeps that status when a retraction reaches it:
it gets the event, not a second terminal status.

### 9.1  Supersession (T27)

`SUPERSEDE-LEDGER-STAGE` takes a ledger, the `ST` id being replaced, the id of
its successor (already added, WF22), a reason and a date. It sets the stage's
`:status` to `:superseded`, writes `:superseded-by`, appends a `:superseded`
event, and then runs steps 2–4 of §9 with the superseded stage in place of the
retracted premise. Because §8 step 1 counts `:superseded` as dead, a stage that
started from the replaced stage's endpoint, and names it alone in a clause, is
invalidated; a bound measured from that endpoint is orphaned. That is the
honest reading: its start state is no longer in the plan. Its endpoint and
evidence survive in its record.

A dependent that should survive the revision is expressed in the clause shape,
not by an exception: a stage that can start from either of two endpoints lists
both in one clause, `(ST2 ST5)`, and then survives the supersession of one.

---

## 10  Exhaustion

An exhausted search produces a `:bound` record and nothing else. Four
independent guards keep it from being read as impossibility; they are
independent on purpose, because this is the error the method exists to prevent.
They are numbered X1-X4 rather than G1-G4 to keep them clear of the
schema-gap ids.

**X1 — Type.** `:bound` is not admissible in `:closed-by`, `:refuted-by`, or
any other field that terminates a link or a stage. WF8. A bound id may appear
only in `:attempts`, which the reporter prints under the link or stage as an
attempt.

**X2 — Mandatory context.** A bound is ill-formed without `:start-state`,
`:search-expression`, `:cutoff` and `:threads`, all four inside its
`:provenance`. There is no way to write a bare "no solution": every bound names
the state it was measured from and the cutoff it stopped at. The writer signals
an error on a missing field rather than defaulting one.

**X3 — Fixed rendering.** The reporter prints bounds in their own section,
never interleaved with link or stage verdicts:

    COST BOUNDS  (GRADE 3 -- NOT IMPOSSIBILITY)
    ------------------------------------------
      BD1  attempted LK4
        exhausted at depth 6, threads 0, from: <start state>
        expression: <as run>
        grade 3: a cost bound relative to that start state.  It licenses no
        impossibility claim.  LK4 remains open.
        evidence: constraint-evidence/<file>

The closing sentence is a fixed template, not authored per record. For a bound
filed against a stage the template names the stage id in place of the link id.
The stage section lists the stage's attempts by id only.

**X4 — Statement lint.** The writer refuses a bound whose `:statement` contains
*impossible*, *cannot*, *no solution*, *refutes*, or *unreachable*, and signals
an error naming the word. This is a crude heuristic sitting behind three
structural guarantees, and it is stated as crude. It catches the case the other
three cannot: a correctly-typed bound whose prose says the wrong thing and then
gets quoted into the register.

**Interpretation is written before the run, not after.** T3's recommendation
states what a success and what an exhaustion would each establish. T4 files the
outcome against that statement. The bound record carries
`:interpretation-committed "<the T3 text>"` so the two can be compared without
trusting memory. For a stage the committed text is the expected reading the
guide's Phase 3 step 8 writes into the stage's evidence before the run, and the
bound quotes it the same way.

---

## 11  Well-formedness

T2 implements `CHECK-LEDGER-WELL-FORMED`, which signals an error on the first
violation. It does not warn and continue: an ill-formed ledger is a bug in the
component that wrote it, and it should manifest immediately.

    WF1   every :id is unique and matches its :kind's prefix
    WF2   every id in :depends-on, :blocks, :attempts, :closed-by, :refuted-by,
          :discharged-by, :answer-premise, :starts-from (other than :initial),
          :superseded-by and :for-stage exists in the ledger
    WF3   the :depends-on graph is acyclic
    WF4   every required envelope key (§3) is present
    WF5   :status is in its kind's domain (§7)
    WF6   :provenance is one of the three species (§4), with that species'
          required fields
    WF7   grade 3 appears only on a :bound record, and every :bound record whose
          provenance is :derived has grade 3
    WF8   no BD id appears in :closed-by or :refuted-by
    WF9   no grade-2 record's closure contains a :bound or a :search-measured
          record
    WF10  every grade-4 record has a non-empty :premise-gaps or
          :proof-obligation
    WF11  an empty :depends-on occurs only on :derived grade 1, :user-asserted,
          or :bound records
    WF12  :user-asserted provenance occurs only on a :premise record
    WF13  :segment :none occurs only on a :derived grade 1 or grade 2 record
    WF14  a :closed link has :validated t and a non-nil :closed-by
    WF15  a :refuted link or stage names a grade-1 or grade-2 record in
          :refuted-by
    WF16  no dependency edge crosses incompatible segments — differing :view,
          or :ghosts :absent against :ghosts :present — without an explicit
          bridging premise in the depending record's :segment-bridge
    WF17  an :answered question has a non-nil :answer-premise whose provenance
          is :user-asserted with :asked-as that question's id
    WF18  a :bound's :statement passes the §10 X4 lint

T27 adds, for version-2 ledgers:

    WF19  a :stage record occurs only in a version-2 ledger.  Its :starts-from
          is :initial or the id of another :stage, and a stage id there also
          appears in some clause of its :depends-on
    WF20  a :stage's :provenance is :derived grade 4, with a non-empty
          :proof-obligation, and its :depends-on is non-empty
    WF21  a :realized stage has an :endpoint; a :closed stage has :validated t,
          a non-nil :closed-by, and an :endpoint carrying :checkpoint, :actions
          and :sha256
    WF22  a :superseded stage names in :superseded-by another stage, and the
          :superseded-by chain is acyclic
    WF23  a :bound has at most one of :for-link and :for-stage non-nil; a
          :for-stage names a :stage, and the bound's id is in that stage's
          :attempts
    WF24  no two stages other than :superseded ones share both :plan-file and
          :plan-stage
    WF25  :check is nil or (:label L :date D :run R) with L in :pass,
          :conditional, :conflict; a :realized or :closed stage has a non-nil
          :check whose label is not :conflict

WF16 exists because G12 is open and G8 has no extractor. A ledger that silently
composes a physical-view premise with a recording-view one would reproduce
G14's defect one level up, where no reader would see it. It applies to stages
unchanged: a stage whose endpoint segment differs in view or ghosts from its
predecessor's needs a `:segment-bridge` naming the premise that bridges them —
usually the trick that opens or closes the recorder cycle.

---

## 12  File format and round-trip

The ledger is a text file of top-level Common Lisp forms, readable by `READ`.

    ;; header comment lines, regenerated on each write
    (:ledger-version 2 :problem "<name>" :written "<date>")
    (:id PR1 :kind :premise ...)
    (:id ST1 :kind :stage ...)
    ...

**Versions.** Version 1 is the pre-T27 format; version 2 admits `:stage`
records and nothing else changes. `MAKE-STAGE-LEDGER` creates version 2, and a
new problem's ledger is made with it. `MAKE-REALIZATION-LEDGER` still creates
version 1, unchanged, because T2's acceptance checks pin that
(`evidence/ledger-checks-2026-09-20.lisp`, case 21) and T27's A4 requires them
to pass unchanged. The reader accepts both and the writer writes back the
version it read, so a version-1 ledger (crelay-topo's) reads, checks and
reports exactly as before T27. There is no upgrade from version 1 to version 2
(T27 scope (a)).

Reading and writing rules, so round-trip is lossless:

- The reader binds `*package*` to the `:WW` package and `*read-eval*` to `nil`.
  Problem object names are read as symbols in `:WW`, which is where staged data
  already lives.
- Dollar-prefixed wouldwork symbols appear in `:evidence` and in statements.
  They are read and printed as symbols; the writer never applies substitution to
  record text. The plan's editing hazard applies to hand edits of this file too:
  use a whole-file write or a quoted heredoc.
- The writer prints with `*print-readably*` behaviour sufficient for `READ` to
  return an `EQUAL` structure, one record per top-level form, keys in a fixed
  order with unknown keys last (§3).
- Write is generate-to-sibling-temporary then replace, the same discipline
  `WRITE-STATIC-CONSTRAINT-PROFILE` already uses, so a generator error preserves
  the previous ledger. As there, replacement is not fully atomic if the rename
  itself fails; do not claim otherwise.
- **C3 holds.** No problem object name appears in the code. They appear in the
  ledger as data, which is what RO's caller-supplied scenario already
  established as the permitted pattern.

### 12.1  The file is the record (T27)

A ledger file is its own authority. It is not rebuilt, and no list of scripts
defines it. Every change goes through `LEDGER-FILE-APPLY` (§13), which:

1. reads the file and runs `CHECK-LEDGER-WELL-FORMED` on what it read, so a bad
   hand edit fails at the next change, not silently later;
2. applies exactly one ledger operation (any operation of §13 that takes the
   ledger as its first argument);
3. runs `CHECK-LEDGER-WELL-FORMED` on the result;
4. writes the file only if both checks passed. A signal at any step leaves the
   file byte-identical.

The operation's own event carries its date, who made the change and, in
`:note` or the record's `:sources`, the evidence file it rests on. That event
trail replaces the ingest-script chain: a later reader learns what happened
from the file, and the problem's Handoff records the file's SHA-256 at the end
of each session instead of a rebuild order. A data-only load file may still
call `LEDGER-FILE-APPLY` several times, for a stage's worth of changes; it is
evidence of how the change was made, not a part of the record.

Hand edits remain allowed (§3) and are checked at the next `LEDGER-FILE-APPLY`.
Version-1 ledgers already built by script (crelay-topo) keep their recorded
rebuild order as history; they are not re-derived.

---

## 13  What T2 implements

Named here so T2's proposal is about placement and testing, not design. Bodies
follow the conventions of whichever file receives them: callees-first in
`tech/constraint-profile.lisp`, high-level-first elsewhere; no `LABELS` or
`FLET`; no one-line functions.

    MAKE-REALIZATION-LEDGER      (problem &optional date)
    MAKE-LEDGER-PREMISE          (id statement provenance &key ...)     §6.1
    MAKE-LEDGER-LINK             (id statement provenance &key ...)     §6.2
    MAKE-LEDGER-BOUND            (id statement provenance &key ...)     §6.3
    MAKE-LEDGER-QUESTION         (id statement &key ...)                §6.4
    ADD-LEDGER-RECORD            (ledger record)
    READ-REALIZATION-LEDGER      (path)                    -> ledger    §12
    WRITE-REALIZATION-LEDGER     (ledger path &optional date)           §12
    REPORT-REALIZATION-LEDGER    (ledger)
    CHECK-LEDGER-WELL-FORMED     (ledger)                               §11
    LEDGER-RECORD                (ledger id)
    LEDGER-NEXT-ID               (ledger prefix)                        §2
    LEDGER-PROVENANCE-VALUE      (provenance key)                       §4
    LEDGER-SET-VALUE             (record key value)   destructive, present keys only
    LEDGER-CLOSURE               (ledger id)               transitive premises
    LEDGER-DEPENDENTS            (ledger id)               transitive dependents
    LEDGER-STANDING              (ledger id)                            §8
    LEDGER-LIVE-GUESSES          (ledger id)   the blocking premises the reporter names
    AMEND-LEDGER-DEPENDS-ON      (ledger id clause note &optional date) §5
    ANSWER-LEDGER-QUESTION       (ledger id answer statement by &optional date)  §6.4
    RETRACT-LEDGER-PREMISE       (ledger id reason &optional date)      §9
    DISCHARGE-LEDGER-PREMISE     (ledger id by note &optional date)     §7.1

T3 adds, in the same file:

    RECOMMEND-LEDGER-SEARCH      (ledger id &key cutoff threads deepen date)
    REPORT-SEARCH-RECOMMENDATIONS (ledger)
    LEDGER-SEARCH-COMMANDS       (ledger link cutoff threads)
    LEDGER-SEARCH-CAUTIONS       (link)
    LEDGER-SUCCESS-READING       (ledger id cutoff)
    LEDGER-EXHAUSTION-READING    (id cutoff)
    LEDGER-DEEPEST-ATTEMPT       (ledger id)
    LEDGER-BOUND-CUTOFF          (record)
    LEDGER-SEARCH-READY-P        (link)
    LEDGER-DEFAULT-THREADS       (link)
    LEDGER-FORM-TEXT             (form)

T4 adds, in the same file:

    INGEST-LEDGER-RESULT         (ledger id outcome &key actions validated run
                                  truncated pruning nodes seconds surprise
                                  surprise-candidates date)
    INGEST-LEDGER-EXHAUSTION     (ledger id &key run truncated pruning nodes seconds date)
    INGEST-LEDGER-FIND           (ledger id actions &key validated run nodes seconds date)
    FILE-LEDGER-SURPRISE         (ledger id question candidates &optional date)   M5
    REPORT-LEDGER-GAP-CANDIDATES (ledger)

T5 adds, in the same file:

    GENERATE-LEDGER-QUESTIONS    (ledger scenario &key blocks supports pool
                                  undeclared date)
    GENERATE-LEDGER-QUESTION     (ledger key context blocks segment date)
    REPORT-LEDGER-QUESTIONNAIRE  (ledger)
    REPORT-LEDGER-QUESTION       (ledger record)
    LEDGER-QUESTION-TEMPLATE     (key)
    LEDGER-QUESTION-STATEMENT    (template context)
    LEDGER-QUESTION-CANDIDATES   (template context)
    LEDGER-QUESTION-PRESENT-P    (ledger key blocks)
    LEDGER-QUESTION-SLOT-VALUE   (slot context)
    LEDGER-COLLAPSE-WHITESPACE   (text)

plus `*LEDGER-QUESTION-TEMPLATES*`, one entry per unresolved premise RO narrates.
Each entry records in `:narrated-as` the fragment of RO's printed text it stands
for, and the acceptance checks assert that fragment is still in
`tech/constraint-profile.lisp`. A change to RO's narration this table has not
followed fails the checks instead of going unnoticed.
    REPORT-LEDGER-BOUND-STRENGTH (bound)
    LEDGER-COMMITTED-RECOMMENDATION (link)
    LEDGER-RUN-PROVENANCE        (recommendation outcome run truncated pruning)
    LEDGER-PREMISE-CLAUSES       (ids)

T27 adds, in the same file:

    MAKE-STAGE-LEDGER            (problem &optional date)   version 2   §12
    MAKE-LEDGER-STAGE            (id statement plan-file plan-stage intent
                                  starts-from depends-on proof-obligation
                                  &key ...)                              §6.5
    SET-LEDGER-STAGE-CHECK       (ledger id label run &optional date)   §6.5
    REALIZE-LEDGER-STAGE         (ledger id realization endpoint closed-by
                                  &key validated nodes seconds date)     §7.5
    CLOSE-LEDGER-STAGE           (ledger id validation-run &optional date)  §7.5
    FILE-LEDGER-STAGE-BOUND      (ledger id &key statement start-state
                                  search-expression cutoff threads run
                                  truncated pruning interpretation
                                  depends-on nodes seconds date)         §6.3, §10
    SUPERSEDE-LEDGER-STAGE       (ledger id successor reason &optional date)  §9.1
    LEDGER-FILE-APPLY            (path operation &rest arguments)       §12.1
    LEDGER-STAGE-ORDER           (ledger)   stages in plan order
    REPORT-LEDGER-STAGES         (ledger)

`REALIZE-LEDGER-STAGE` enters `:realized`; it also closes the stage when called
with `:validated t`, so a hand-derived stage validated in one step needs one
call. `CLOSE-LEDGER-STAGE` closes a stage already realized, after a separate
validation run. `FILE-LEDGER-STAGE-BOUND` writes the bound with `:for-stage`
and adds its id to the stage's `:attempts`; like T4's ingester it requires the
committed interpretation and runs the X2 and X4 checks.

**Plan order.** `LEDGER-STAGE-ORDER` is breadth-first from `:initial`: first
the stages whose `:starts-from` is `:initial`, then the stages starting from
any of those, and so on level by level; within a level, by id number. A
superseded stage keeps its place in that order, and its successor is printed
with it.

The reporter prints, in this order: open links with their blocking premises and
what would close each; conditional records with the premises making them
conditional; established records; the cost-bound section of §10 X3; open
questions; then retracted and invalidated records under their own heading, never
omitted.

For a version-2 ledger, `REPORT-REALIZATION-LEDGER` first prints a STAGES
section from `REPORT-LEDGER-STAGES`, then the sections above, unchanged. Per
stage in plan order it prints: id, plan stage, status and standing; the
successor, if superseded; plan file; intent; starting point; check; realization;
endpoint (archive, actions, SHA-256) or none; live guesses in id order; and
attempts by id. The exact line format is fixed by T27's A2 evidence
(`evidence/t27-stage-ledger-2026-09-26.txt`, part 1). A version-1 ledger prints
exactly as before.

T2's acceptance criteria are in the plan and are not restated here. Two of them
are tests of this document rather than of the code, and are worth naming:
round-trip losslessness exercises §12's unknown-key rule, and "invalidates
exactly its dependents" exercises §9's empty-clause cascade.

---

## 14  Worked example A — a retraction

The scenario mirrors RO1/RO2 without depending on them: a device whose normal
aggregate requires three distinct supports, three eligible witnesses, and two
user premises narrowing the pool. Statements are abbreviated.

**Before.**

    (:id PR1 :kind :premise
     :statement "the device's normal aggregate requires three distinct
                 supports depressed, and occupancy is keyed by occupant, so
                 three distinct witnesses are necessary"
     :provenance (:derived :grade 1 :by "S1 control table; S2 injectivity")
     :depends-on () :premise-gaps ()
     :segment (:view :physical :cycle :none :ghosts :absent)
     :status :in-force
     :sources ("CONTROLS clauses and polarity" "ON's functional keying")
     :events ((:date "2026-09-20" :event :opened :by "T2" :note "")))

    (:id PR3 :kind :premise
     :statement "no ghost occupants exist in the segment under analysis"
     :provenance (:user-asserted :by "D" :asked-as QN1 :date "2026-09-20")
     :depends-on () :premise-gaps ("necessity of this segment")
     :segment (:view :physical :cycle :none :ghosts :absent)
     :status :in-force :sources ()
     :events ((:date "2026-09-20" :event :opened :by "T5" :note "answer to QN1")))

    (:id PR4 :kind :premise
     :statement "the agent occupies none of the three required supports"
     :provenance (:user-asserted :by "D" :asked-as QN2 :date "2026-09-20")
     :depends-on () :premise-gaps () :segment (...) :status :in-force
     :sources () :events (...))

    (:id PR7 :kind :premise
     :statement "the agent is committed elsewhere for the whole segment"
     :provenance (:user-asserted :by "D" :asked-as QN2 :date "2026-09-20")
     :depends-on () :premise-gaps () :segment (...) :status :in-force
     :sources () :events (...))

    (:id LK2 :kind :link
     :from "the three supports unoccupied"
     :to   "the three supports simultaneously occupied"
     :intent "hold the aggregate open across the segment"
     :depends-on ((PR1) (PR3) (PR4 PR7))
     :provenance (:derived :grade 1 :by "injective matching over the stated pool")
     :premise-gaps ("ghost absence" "replacement witnesses" "recorder transitions")
     :segment (...) :status :open :closed-by nil :evidence nil :validated nil
     :attempts () :refuted-by nil :sources (...) :events (...))

    (:id LK5 :kind :link
     :from "..." :to "..." :intent "..."
     :depends-on ((PR1))
     :provenance (:derived :grade 1 :by "...")
     ... :status :open ...)

Standings before: PR1 ESTABLISHED. LK2 CONDITIONAL — its closure holds PR3 and
PR4, both in force and both user-asserted. LK5 CONDITIONAL only if its own
closure says so; here its closure is `{PR1}` alone, so LK5 is **ESTABLISHED**.

**`(RETRACT-LEDGER-PREMISE ledger 'PR3 "D withdrew the ghost-absence guess" "2026-09-21")`**

    PR3   :status :retracted
          + (:date "2026-09-21" :event :retracted :by "D"
             :note "D withdrew the ghost-absence guess")

    LK2   clause (PR3) has no surviving disjunct  ->  standing UNFOUNDED
          :status :invalidated
          + (:date "2026-09-21" :event :invalidated :by "T2" :note "PR3 retracted")

    LK5   not a dependent of PR3.  Untouched.  Still ESTABLISHED.
    PR1   not a dependent.  Untouched.
    PR4   not a dependent.  Still :in-force.
    PR7   not a dependent.  Still :in-force.

**Now retract PR4 instead, on the original ledger.** PR4 sits in the clause
`(PR4 PR7)`, which keeps PR7:

    PR4   :status :retracted
    LK2   clause (PR4 PR7) still has PR7  ->  standing still CONDITIONAL
          :status unchanged, :open
          + (:date "2026-09-21" :event :standing-changed :by "T2"
             :note "PR4 retracted; clause survives on PR7")

That is the distinction §5 exists for. A flat premise list would have
invalidated LK2 in both cases, and the alternative RO was careful to keep
separate would have been lost at the ledger boundary.

**And the upgrade path.** Suppose PR7 is later derived from established records:

    (:id PR9 :kind :premise
     :statement "the agent is committed elsewhere for the whole segment"
     :provenance (:derived :grade 2 :by "<the derivation>")
     :depends-on ((PR1)) ...)

    PR7   :status :discharged  :discharged-by PR9

LK2's closure now looks through PR7 to PR9. With PR3 still retracted LK2 is
UNFOUNDED; with PR3 in force it is CONDITIONAL on PR3 alone; with PR3 also
discharged it becomes ESTABLISHED — and no record in the chain was rewritten to
make that happen.

---

## 15  Worked example B — an exhaustion result

A search is recommended for LK4, run locally, and exhausted at the cap. T4
ingests it.

**What is written.**

    (:id BD1 :kind :bound
     :statement "no realization of LK4 was found within depth 6 from the
                 stated start state"
     :provenance (:search-measured
                  :outcome :exhausted
                  :start-state "<the staged state, described>"
                  :search-expression "<the SOLVE-SUBGOAL chain as run>"
                  :cutoff 6
                  :threads 0
                  :run "constraint-evidence/lk4-bound-2026-09-21.txt")
     :depends-on ((PR1) (PR3))
     :premise-gaps ("whether a deeper cap changes the result")
     :segment (:view :physical :cycle :none :ghosts :absent)
     :status :standing
     :for-link LK4
     :measured (:nodes 148203 :seconds 91)
     :interpretation-committed
       "success would close LK4 with its action sequence, subject to
        validation; exhaustion would establish a depth-6 cost bound relative
        to this start state and nothing more"
     :sources ("T3 recommendation for LK4")
     :events ((:date "2026-09-21" :event :opened :by "T4" :note "")))

**What changes on LK4.**

    LK4   :attempts (BD1)
          :status unchanged, :open
          + (:date "2026-09-21" :event :amended :by "T4" :note "BD1 attempted")

**What is not written, and cannot be.** LK4 does not become `:refuted`: WF15
requires a grade-1 or grade-2 derivation in `:refuted-by`, and WF8 forbids a
`BD` id there. LK4 does not become `:closed`: `:closed-by` is still nil. No
record anywhere gains a statement that the link is impossible — BD1's own
statement would fail the X4 lint if it tried, and BD1 is the only record the
exhaustion produced.

**What it prints.**

    COST BOUNDS  (GRADE 3 -- NOT IMPOSSIBILITY)
    ------------------------------------------
      BD1  attempted LK4
        exhausted at depth 6, threads 0, from: <start state>
        expression: <as run>
        grade 3: a cost bound relative to that start state.  It licenses no
        impossibility claim.  LK4 remains open.
        committed before the run: success would close LK4 with its action
        sequence, subject to validation; exhaustion would establish a depth-6
        cost bound relative to this start state and nothing more.
        evidence: constraint-evidence/lk4-bound-2026-09-21.txt

**And if PR3 is later retracted**, BD1 becomes `:orphaned`, not `:invalidated`.
The 148,203 nodes were searched; that fact does not stop being true. What
changed is that the analysis may no longer reach the state they were searched
from, and `:orphaned` says exactly that and no more.

---

## 15a  Worked example C — stages (T27)

A plan of three stages, `c1`, `c2` and `c3`, in plan-data file
`constraint-evidence/stage-plan-2026-10-01.lisp`, checked by CP and approved by
D. Statements and events are abbreviated.

**The premises.**

    PR1  "D approved the stage plan stage-plan-2026-10-01.lisp"
         (:user-asserted :by "D" :date "2026-10-01")
    PR2  "a ghost holds a tray as a step"          (:user-asserted ...)  a trick
    PR3  "the lift's landing is the ghost's tray"  (:user-asserted ...)  a trick

**The stages, as first entered.**

    (:id ST1 :kind :stage
     :statement "c1 realizes its intent from the initial state"
     :provenance (:derived :grade 4 :by "stage-plan-2026-10-01.lisp stage c1")
     :depends-on ((PR1))
     :plan-file "doc/problems/<p>/constraint-evidence/stage-plan-2026-10-01.lisp"
     :plan-stage "c1" :intent "..." :starts-from :initial
     :check (:label :pass :date "2026-10-01" :run "constraint-evidence/stage-01-c1/cp.txt")
     :proof-obligation "the endpoint validates from the initial state and meets c1's intent"
     :segment (:view :physical :cycle :closed :ghosts :absent)
     :status :open ...)

    ST2  plan-stage "c2", :starts-from ST1, :depends-on ((PR1) (ST1) (PR2))
    ST3  plan-stage "c3", :starts-from ST2, :depends-on ((PR1) (ST2) (PR3))

**ST1 hand-derived and validated in one step.**

    (REALIZE-LEDGER-STAGE ledger 'ST1 :hand
      '(:checkpoint "constraint-evidence/stage-01-c1/c1-checkpoint.txt"
        :actions 11 :sha256 "<hex>")
      '(:derived :grade 1 :by "VALIDATE-ACTION-SEQUENCE from the initial state")
      :validated t)

    ST1   :status :closed, :validated t, :endpoint as given
          + :realized and :closed events

ST1's standing is CONDITIONAL: PR1 is a guess. Its endpoint is established by
the validation; whether c1 belongs in the plan rests on D.

**ST2 by search, exhausted once.** The expected reading was written into
`stage-02-c2/` before the run.

    (FILE-LEDGER-STAGE-BOUND ledger 'ST2 :start-state "ST1's endpoint" ...
      :cutoff 10 :threads 16 :depends-on '((ST1)) :interpretation "<the committed text>")

    BD1   :for-stage ST2, :depends-on ((ST1)), :status :standing
    ST2   :attempts (BD1), :status unchanged, :open

**D revises c2.** The plan file gains stage `c2b`; ST4 is added for it, starting
from ST1, and ST2 is superseded:

    ST4  plan-stage "c2b", :starts-from ST1, :depends-on ((PR1) (ST1) (PR2))
    (SUPERSEDE-LEDGER-STAGE ledger 'ST2 'ST4 "D revised c2 after BD1" "2026-10-02")

    ST2   :status :superseded, :superseded-by ST4
    ST3   clause (ST2) has no surviving disjunct -> UNFOUNDED -> :invalidated
    BD1   depends on ST1 only; not a dependent of ST2.  Still :standing.
    ST1   not a dependent.  Untouched.

BD1 stays standing because it measured ST1's endpoint, which the revision did
not touch. ST3 is invalidated because it would have started from ST2's
endpoint; D's revised c3 enters as a new stage starting from ST4. Had ST3 been
written with the clause `(ST2 ST4)`, it would have survived. ST2 itself stays
CONDITIONAL (§8): its own clauses are alive, and its status says it left the
plan.

**Every change above is one `LEDGER-FILE-APPLY` call** (§12.1), for example

    (LEDGER-FILE-APPLY path #'SUPERSEDE-LEDGER-STAGE 'ST2 'ST4
                       "D revised c2 after BD1" "2026-10-02")

and the file after each call is the record. The Handoff records its SHA-256.

**What it prints**, the STAGES section in plan order (abbreviated; the exact
line format is in T27's A2 evidence):

    STAGES (4)  [plan order; standing computed, never stored]
      st1  c1  closed; standing conditional
        ...
        endpoint: constraint-evidence/stage-01-c1/c1-checkpoint.txt, 11 actions, sha256 <hex>
        live guesses (1): pr1
        attempts: none
      st2  c2  superseded; standing conditional
        superseded by: st4
        ...
        endpoint: none
        live guesses (2): pr1 pr2
        attempts: bd1
      st4  c2b  open; standing conditional
        ...
      st3  c3  invalidated; standing unfounded
        ...

The order is breadth-first (§13): st1; then st2 and st4, which start from it;
then st3, which starts from st2.

---

## 16  Amended during implementation

T2 was implemented against this document on 2026-09-20 and five things were
found underspecified. Each was settled as a technical choice, the document was
corrected in the same turn, and the change is listed here rather than left to be
noticed by diff.

1. **`:segment-bridge`** — WF16 required "an explicit bridging premise recorded
   on the depending record" without naming the key. It is `:segment-bridge`, a
   kind key of `:link` and `:bound` (§6, §11).
2. **Provenance access** — a species keyword followed by a plist is not a plist,
   and a plain `GETF` reads it off by one. `LEDGER-PROVENANCE-VALUE` is now the
   named accessor (§4).
3. **Answering a question** adds a clause on the answer premise, so an answered
   question reads CONDITIONAL rather than ESTABLISHED while its answer is a
   guess. Without it the reporter printed a well-posed question as established
   beside the conditional premise it had just created, which invited exactly the
   misreading §8 exists to prevent (§6.4).
4. **Retraction events** — a surviving dependent now always gets an event, not
   only when its standing moved. Silence is not a record.
5. **The rename target** in the writer carries only name and type, because
   `RENAME-FILE` merges its argument against the file being renamed and the
   directory would otherwise be duplicated whenever the path is relative (§12).

T3, on 2026-09-20, added the six `:search-*` and `:recommendation` link keys of
§6.2 and the functions listed in §13. Two points of substance came out of it:

6. **`*depth-cutoff*` 0 or negative means no cutoff at all** in this engine, so a
   missing or zero cutoff can never be a default. The recommender signals rather
   than emitting an uncapped search dressed as a bounded one.
7. **"Must not silently deepen" is enforced by a signal, not a warning.** A cutoff
   greater than the deepest existing attempt's requires `:deepen t`, and the
   recommendation then records which bound it goes past. The earlier bound is not
   superseded by the deeper run; it is superseded only when a new bound is filed
   against the same start state.

T4, on 2026-09-20, added the ingester and two fields to a bound's provenance. The
substantive finding:

8. **An exhaustion has a quality, and the engine reports it.**
   `*DEPTH-CUTOFF-TRUNCATED*` records whether the cutoff cut off a node that still
   had successors. A reliably measured NIL means no such direct cutoff witness
   was observed; it does not certify unpruned reachable-space exhaustion. An
   exhaustion under symmetry or repeated-state pruning is weaker again, since it
   excludes solutions in pruned branches. Both are stored on the bound as
   `:cutoff-truncated` and `:pruning` and printed beside it, because inferring
   either later from the cutoff alone would be guessing. Neither changes the grade:
   a cost bound is grade 3 in every case.

   **Correction, approved 2026-09-21:** parallel workers previously counted cutoff
   hits without measuring truncation, so historical parallel NIL/COMPLETE reports
   cannot be treated as reliable negative observations. Bound provenance accepts
   `:cutoff-truncated T`, `NIL`, or `:UNKNOWN`. The ingester defaults to `:UNKNOWN`
   when coverage is not supplied; the reporter handles all three explicitly and
   also treats an absent historical field as unknown. Invalid values signal an
   error. Preserve raw engine fields separately when correcting their interpretation.
   Parallel workers now test successors at cutoff nodes, retaining one positive
   witness per worker and aggregating the result after workers complete. This
   change does not retroactively determine coverage for an older run.

T5, on 2026-09-20, added the question templates, the three answer kinds and the
questionnaire. One point of substance:

9. **Not all underdetermination is a choice.** §6.4 as written assumed a single
   candidate set and a single answer. Two of the premises RO narrates do not fit:
   availability is a subset of a stated pool, and the segment RO prints as NONE
   STATED is a description. Both are now first-class answer kinds rather than
   being squeezed into a choice or left out of the generator, which would have
   made the coverage claim false in the one place it is easiest not to notice.

T14, on 2026-09-24, brought the recommender up to date with standalone search
checkpoints. Two points of substance:

10. **An exhaustion reading says what was found, not what exists.** The template
    now records that the search found no realization within the cutoff under
    the settings and pruning in force, and states how the measured truncation
    (T, NIL or UNKNOWN) bounds its coverage. Readings committed before T14 stay
    on their records as written.
11. **A checkpoint start is its own kind of start.** A string `:search-start`
    names a checkpoint variable; `:search-archive` names the archive it is
    imported from. Its commands import, set the cutoff, assign the search's
    returned checkpoint and print the run metadata T4 needs; its advice on a
    find is to export to a new archive and file REALIZED with validated NIL.

T27, on 2026-09-26, revised this document before code (header note):

12. **The stage is the ledger's unit, and the file is the record.** crelay-topo's
    ledger was organized by spine link while its plan came from D's stages, and it
    was defined by replaying 13 ingest scripts in order (post-mortem 4.1). The
    `:stage` kind (§6.5, §7.5) records one stage of D's approved plan by
    reference to the plan-data file; supersession (§9.1) handles D's revisions
    through the existing empty-clause cascade, with `:superseded` counted dead in
    §8; `LEDGER-FILE-APPLY` (§12.1) makes every change a checked read-apply-write
    of the file, so no rebuild order exists. Version 2 carries the change;
    version-1 ledgers are unchanged and not upgraded. Two points were settled
    while writing T27's A2 fixture, before code: an endpoint may end inside a
    longer archive (`:actions` below the archive's count, §6.5), since
    crelay-topo's early stages were validated only as prefixes of later
    sequences; and a superseded stage keeps its own standing (§8), which the
    first draft of example C had printed as UNFOUNDED. A third, found reading
    T2's checks before code: case 21 of `evidence/ledger-checks-2026-09-20.lisp`
    pins version 1 for `MAKE-REALIZATION-LEDGER`, so version 2 is made by a new
    `MAKE-STAGE-LEDGER` instead (§12, §13).

## 17  Deliberately not settled

- **Placement.** Settled, not deferred: `tech/constraint-ledger.lisp`, its own
  loadable diagnostic. The reason is in that file's header — the ledger reads no
  staged data, persists across sessions and is hand-amended between them, while
  every entry point in `constraint-profile.lisp` reads what staging built. What
  remains unsettled is nothing about the ledger's API packaging.
- **Whether a bound may be compared across start states.** Two bounds measured
  from different start states are not comparable, and this schema gives no way
  to relate them. If T3 wants to deepen a search it writes a new bound; nothing
  here defines a partial order on bounds.
- **A necessity calculus.** The ledger records that a link is required by
  something; it does not derive which links are required. That is T6–T9 and the
  abstract model's unfinished mandatory role set.
- **Multi-problem ledgers.** Ids are ledger-local (§2).
- **Operators.** The abstract model's section 4 block is a decision of D's, not
  a gap this schema fills. Nothing above imports an operator vocabulary.
- **A per-stage search recommender.** Declined with T27 scope (c). The T3
  recommender works per link and is not used by the stage procedure; a stage's
  expected reading is written into its evidence by hand (guide, Phase 3 step 8)
  and quoted by `FILE-LEDGER-STAGE-BOUND`.
- **Upgrading a version-1 ledger.** Declined with T27 scope (b).
