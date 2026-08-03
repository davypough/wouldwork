# Session prompt: the boundary between Wouldwork and Talos technology

Paste this whole file as the opening message of a new session.

---

## What I want to talk about

Wouldwork is a Common Lisp planner. Underneath it sits `src/` — search, translation,
propagation ordering, initialization, validation. On top of it sits a Talos Principle
technology library in `tech/`, a set of problem specifications in `probs/`, and a set of
technology characterization problems in `test/`.

The intent is that these are two different things. `src/` is a planner. `tech/` is a
description of a fictional world's gadgets, written in the planner's own design language.
I want to examine how well that separation actually holds, decide where it should be
tightened, and agree on what the design language ought to cover.

This is a design discussion first. I don't want an implementation plan until we've agreed
on the shape of the thing.

## Statement of purpose — what I want as a user, not only as a developer

I am the developer of Wouldwork, but when I sit down to work on Talos problems I am its
user, and that is the perspective I care about here.

As a user I want to describe a world — locations, walls, gates, fans, connectors, beams,
plates, recorders — and have the planner find the action sequence that solves it. When
the game presents a new gadget, I want to add it by writing one file in `tech/` using
Wouldwork's design language, and nothing else. I should not have to open `src/`. I should
not have to understand how the propagation driver is derived, how the translator prunes
static formulas, or how the read/write graph is walked, in order to say "a fan blows
things at the location it faces."

I want to test a gadget in the language I wrote it in. A technology characterization
should read like a small world with a claim about it, not like a Lisp program poking at
the planner's hash tables.

And I want the boundary to be legible. When something breaks, I want to know immediately
whether I described the world wrong or the planner has a bug. Right now that question is
sometimes genuinely hard to answer, and that is the symptom that prompted this.

## Where the line currently is, measured

These counts are from the current tree, not from memory.

**`tech/` is mostly clean.** 50 files, 260 `define-*` forms, 58 plain-Lisp `defun`s.
The Lisp is concentrated in 5 files and falls into exactly two categories:

| File | defuns | What the Lisp does |
|---|---|---|
| `-walkability-coordinates.lisp` | 23 | Segment arrangement, zone flood fill, door-set antichains |
| `-beam-los-coordinates.lisp` | 9 | Sightline occluder derivation from segments |
| `-beam-crossing-coordinates.lisp` | 8 | Beam crossing geometry |
| `-walkability.lisp` | 6 | Canonical minimal-family algebra |
| `-recorder-solution.lisp` | 12 | Solution path classification, validation, two-phase report |

The first four are geometry. The fifth operates on a solution path. Both share one
property: **the data is not state.** A segment arrangement is authored geometry compiled
once at init; a solution path is a Lisp list handed over after search ends. The design
language addresses propositions in a database, so it has no vocabulary for either. The
containment is decent — the geometry Lisp is reached from exactly one `define-init-action`
per file and never from a query or update inside the search loop.

**`src/ww-init-validator.lisp` is the largest breach.** 1329 lines, 95 check functions, of
which 54 name Talos concepts outright — `check-init-recorder-consistency`,
`init-check-recording-wall-gears-controls`, `init-check-recording-jammers`,
`check-init-repeater-consistency`, `check-init-paired-connector-graph-acyclic`,
`check-init-coupled-beam-consistency`, `check-init-stream-consistency`,
`check-init-walk-via-clauses`, `check-init-beam-crossing-endpoints`, and so on. This is
technology semantics living inside the planner. Adding a gadget today means editing `src/`
after all, which directly contradicts the purpose above.

**`test/` is where the mixing is worst.** 18 of 68 test problems reach into engine
internals — `*types*`, `*type-components*`, `*relations*`, `*static-relations*`,
`*fluent-relation-indices*`, `*solution-report-printers*`, `check-init-*`,
`check-proposition`. The heaviest: `problem-controls-substrate-test` (15 sites),
`problem-recorder-test` (9), `problem-threat-test` (9), `problem-position-test` (8),
`problem-gate-substrate-test` (7), `problem-visibility-substrate-test` (7). Every one of
them invents the same `(setf (symbol-function 'foo) (lambda () ...))` idiom
independently, because there is no shared vocabulary for saying "this relation has this
signature" or "this malformed input should be rejected."

**Engine globals reached from `tech/`:** `*types*` (4 files), `*applying-init-action*`
(2), `*beam-occlusion-tolerance*` (2, and it is *declared* in a tech file),
`*max-pairings*` (1, a parameter the problem sets and a tech file reads),
`*solution-paths*` (1, as a default argument).

**Hook registration is asymmetric.** `register-solution-validator` is called by the
*problem*; `register-solution-report-printer` was called by the *tech*. Because
`printout-solution` runs every registered printer on every solution, the recorder's report
printer fired on characterization problems whose harness actions take no agent parameter,
and errored. Fixed for now by having the printer establish that it applies, but the
underlying question — who registers a hook, and against which problems does it run —
is unresolved.

## Specific places to look at, in rough priority order

1. **`src/ww-init-validator.lisp`.** Should a technology carry its own init-time
   validation? What would `define-init-check` have to look like for
   `init-check-recording-wall-gears-controls` to live in `recorder.lisp`? Note that these
   checks operate on raw `define-init` literals before the database exists, which is
   exactly why they ended up in Lisp in `src/`.

2. **The three coordinate derivation files.** Is geometry a legitimate permanent exception,
   or should the design language grow enough vocabulary — arrays, rationals, iteration over
   computed sequences — to express an arrangement? My instinct is that this is the one place
   plain Lisp genuinely belongs, but I want that decided rather than assumed.

3. **`test/`'s 18 engine-introspecting problems.** What should a substrate test be able to
   say, and in what language? Candidate answer: a small shared vocabulary for schema and
   registration claims, so no test file writes a `symbol-function` lambda again. This
   subsumes the failure-attribution problem — those characterizations are single booleans,
   so a regression reports only "completed without a solution."

4. **`src/ww-problem-tests.lisp`.** `test-talos` and `*mutation-cases*` name specific tech
   queries (`obstacle-clear`, `safe`) and specific test filenames. The engine's test
   harness knows the technology library by name.

5. **`-recorder-solution.lisp`.** Solution-path processing — the two-phase report and
   candidate validation. Is post-search path analysis a technology concern at all, or an
   engine service the technology should parameterize?

6. **The `-` prefix convention.** `-foo.lisp` currently means both "hook substrate a public
   tech overrides" (`-walkability`, `-visibility`, `-recording-shadow-policy`) and "payload
   nested for packaging" (`-walkability-coordinates`, `-recorder-solution`). Two different
   ideas share one marker.

## Constraints to respect

- Single developer, single-user codebase. Backward compatibility is not required.
- **Prefer Wouldwork clauses over Common Lisp except in exceptional circumstances.** The
  exceptions are what this session is partly about.
- No defensive programming — errors should manifest immediately, at the place where the
  mistake was made.
- No wrapper chains, no `labels`/`flet`, no single-line helper functions, no nesting past
  three levels.
- High-level to low-level ordering within a file; two blank lines between top-level forms;
  no blank lines inside a function.
- Present whole functions with changes marked; deal with one issue at a time; analyze a
  proposal a second time before presenting it.
- I reload with `(progn (ql:quickload :wouldwork) (in-package :ww))` after file changes.
- Tests are `(test-talos)`, `(test-talos :validate t)`, `(test-bt)`,
  `(test-start-is-goal)`, `(test-solution-validator)`,
  `(test-recorder-playback-validation)`, `(test-characterization)`. I run them; tell me
  which to run.

## Context: what the previous session did

Six cleanup items on the recorder technology, all landed and passing:

1. Deleted the unreachable deferred-walk subsystem — `walk-playback-validation-required`
   had no override anywhere, so `walk`'s deferred branch, ~90 lines across
   `walkability.lisp`, `-stream-passability.lisp`, and `-walk-recording-policy.lisp`
   (deleted), were dead.
2. Collapsed five copies of the controls DNF aggregate into two — `control-on` in
   `-controls.lisp` and `recording-control-on` in `recorder.lisp`. They stay separate
   because `ww-propagation-order`'s walker recurses into query bodies and prunes only
   statically decidable tests; a view flag passed as a parameter would credit playback
   updates with reading recording relations and could close a cycle in the derived driver.
3. Made both recorder scope limits fail at init instead of one erroring at runtime and one
   diverging silently. Added `init-check-recording-jammers` — **note this is a deferred
   feature, not a permanent limit: recording-side jamming is wanted.** `jamming` is asserted
   by `jam-target` rather than derived, so it needs no parallel relation — a query filtering
   `jamming` to ghost jammers plus a term in each of the two recording updates. The real
   obstacle is that `jam-target` tests its sightline with `visible`, the actor-blind form.
4. Split `recorder.lisp` (491 lines) into identity plus state machine (241) and
   `-recorder-solution.lisp` (277) for everything that runs once per candidate path.
5. Split the giant characterization conjunctions into named per-theme queries, and replaced
   an exact `*type-components*` roster assertion with the recorder's own membership claim.
6. Gave the report's return-walk append actual coverage, which required real walkability in
   the report test — under the identity default the append was unreachable, not just
   untested.

Also earlier in that session: diagnosed why `problem-windtunnel-topo` found no solution
(`location4` sat within `*beam-occlusion-tolerance*` of the `location3`→`receiver1`
sightline, so the agent waiting at `location4` extinguished the beam that opened the gate
it was waiting on), and reworked recorder termination so the default search stops at the
problem's own goal with `ghost-stops-recorder` available as an opt-in goal conjunct.

## What I'd like out of this session

An agreed position on where the boundary belongs, which of the six places above move and
which stay, and what the design language needs to grow — if anything — to make the moves
possible. Concrete enough that the follow-up work can be sequenced one issue at a time,
but I don't want that sequencing until the shape is settled.

## Implementation status

Stages 1 and 2 established attributed, technology-registered initialization checks and kept the
complete initial derivation inside the existing staging/initialization lifecycle. Stage 3 moved
Talos-specific raw-literal semantics out of `src/ww-init-validator.lisp`: the engine now owns only
generic literal access, schema access, duplicate-fluent and derived-fact rejection, registration,
failure attribution, and lifecycle cleanup. Technology files own their validation directly or
through focused `-*-init-checks.lisp` companions. `define-init-check-helper` prevents helper
functions from leaking across staged problems, while `(:consumes ...)` metadata replaces the
engine's former hard-coded knowledge of named segment types.

Stage 4 established a separate test-characterization boundary without changing initialization.
`test-talos` now runs technology-registered `define-test-claim` checks after staging has produced
the complete start state and before search begins. Each failed clause reports its claim and source
form, while ordinary Lisp errors remain fatal. A shared vocabulary covers type, relation, fluent,
registration, and expected-condition assertions; state behavior remains in Wouldwork queries and
goals. `define-test-helper` and `define-init-check-helper` share one problem-function lifecycle, so
test and validation helpers are removed together at restaging. Test files no longer define helpers
through `symbol-function` lambdas or unmanaged `defun`s. The remaining `symbol-function` calls
were direct invocations of staged functions and were removed in Stage 5.

Stage 5 moved all mutation specifications out of `src/ww-problem-tests.lisp` and into the
characterization problems that own them. `define-query-mutation`, `define-update-mutation`, and
`define-action-precondition-mutation` express the three mutation categories in the test language.
`test-talos :validate t` discovers those declarations during its ordinary sweep, then restages the
owning problem with one named mutation active before compilation and initial derivation. The engine
harness now knows only generic mutation metadata and reports surviving mutation names; it contains
no Talos function names or mutation test filenames. Test-side calls to dynamically installed
functions use ordinary function designators rather than `symbol-function` access.

Stage 6 made recorder solution-path processing a technology-owned policy over generic engine
services. `(include-tech recorder)` installs recorder mechanics but no solution hooks; an integrated
recorder problem declares `(enable-recorder-solution)`, which registers recorder candidate
validation and two-phase reporting together. Problems no longer call the engine's generic recorder
validator hook directly, and the report printer is no longer registered unconditionally. Because
activation now establishes that the recorder solution contract applies, report construction is
strict and the former path-shape applicability heuristic has been removed.

Stage 7 resolved the dash-prefix ambiguity by making it a visibility marker only. A bare technology
filename is public and problem-facing; a dash-prefixed filename is a private component normally
nested by another technology. Hook interfaces, roles, coordinate derivations, initialization-check
companions, solution services, and composite substrates remain separate structural kinds described
by their names and headers. Characterization problems may include private components directly to
test their contracts. The sole visibility exception, `gears-fan.lisp`, was renamed
`-gears-fan.lisp`; no splicer rule or additional design-language form was needed.
