# WouldWork Working-Reference Builder — Prompt

Task is to build a WouldWork (WW) Working Reference given a problem spec (and, optionally, an environment diagram).
It instructs you to build one markdown **working reference** that co-locates and normalizes the
spec's scattered facts, so they can be read off directly during analysis instead of reconstructed
from memory.

---

## Inputs

- **Required:** the problem spec `.lisp` file. Read it **fresh from disk every time**. Never rely
  on memory of a prior version, a prior session's reference, or a remembered conclusion. If a
  remembered answer surfaces, set it aside and derive from the current artifact.
- **Optional:** a hand drawing / diagram of the environment.

---

## Non-negotiable discipline

1. **Transcribe, don't recall.** Every structural fact in Sections 1–7 comes verbatim from the
   current file. Cite line numbers where it aids the transcription check.
2. **Spec stays authoritative.** The reference is a *derived* aid, not ground truth. On any
   conflict, the spec wins — fix the reference, never the other way around.
3. **Keep the capability families separate.** Movement (accessibility), sight (visibility), and
   reach (reachability) are distinct graphs with distinct passability rules. Never merge them; a
   fact true in one is routinely false in the others.
4. **Quarantine geometry.** Anything the spec cannot state — which side of a wall a spot is on,
   reach distance, sightline geometry — goes **only** in Section 8, labeled as a reading of the
   diagram and flagged for confirmation. Never assert it as spec fact. If no diagram is supplied,
   list these as open questions instead.
5. **Compute the start state once.** Hand-run the derivation cascade (the `define-update`
   functions, in their `propagate-consequences!` order) over the init facts and record the actual
   initial derived vector (open gates, active receivers, etc.). Don't leave it implicit.
6. **Words, not symbols, in legends.** Use tokens like `clear` / `(occluders…)` / `none`. Bare
   symbols (—, ·) render inconsistently and invite transcription drift.
7. **Include only what's declared.** Omit sections, branches, and obstacle kinds the spec doesn't
   actually use. A problem with no reach edges has no reach section.
8. **Present for verification before relying on it.** Show the finished reference, ask for a
   transcription check (call out the most error-prone table explicitly), and only then reason off
   the verified version.
9. **Regenerate per problem.** Never reuse a stale reference; rebuild from the current file.

---

## Output: the markdown working reference

Open with a **header block**: source filename; the `ww-set` config (`*problem-type*`,
`*solution-type*`, `*tree-or-graph*`, `*depth-cutoff*`); and any **type members declared in
`define-types` but absent from every init relation** — the "missing pieces" the analysis must place.

Then the sections below. Treat them as a **template**: drop any the problem doesn't exercise, and
add problem-specific ones where a derived layer doesn't fit the headings.

### 1. Accessibility (Movement) network
From `in-area`, `interface`, `traversable>`, and `accessible-clear`:
- Each area's free-access clique (intra-area moves are unobstructed).
- Each inter-area interface with its guarding-obstacle list; state that **every** listed obstacle
  must be passable to cross (per `one-step-accessible`).
- Each one-way edge (`traversable>` suppresses symmetry); give its direction and obstacles.
- The per-kind passability rule for each obstacle kind the spec declares (gate→open;
  screen/ladder→agent not carrying; etc.), read from `accessible-clear`.
- Flag any location not in any area.

### 2. Reachability (getting/putting) network
From `reachable-via` and `reachable-clear`:
- Every reach edge with its barrier list. Restate the algebra: **symmetric, NOT transitive, NOT
  implied by shared area or los-group, true for identical endpoints** (self-clause).
- The barrier-clearing rule (typically: clears only for an open gate; any non-gate barrier and any
  closed gate block reach).
- Note which locations no reach edge touches.

### 3. Visibility (Line of Sight) network
From `in-los-group`, `los-via`, and `visible-clear`:
- The group membership (which locations share each sightline profile).
- A **group × target** table of occluder lists. Use the word-token legend (rule 6):
  `clear` (empty list), `(occluders…)` (sightline exists but occluded), `none` (no `los-via` entry
  at all — no sightline). **This table is the historically error-prone one — transcribe it exactly
  and call it out in the verification request.**
- The per-occluder transparency rule from `visible-clear`.
- Flag any location/group with no sight data.

### 4. Object / role inventory
Every movable (cargo) object: start location, kind, the roles it can play (mover, carrier, jammer/placer,
beam-blocker, …), and **what world state each can independently force**. Name any scarce,
load-bearing resource (e.g., a single jammer) explicitly — overloaded resources drive the puzzle.

### 5. Derived-state dependency chain
Read the truth condition of each derived fluent off the `define-query` helpers and `define-update`
combine rules, and write the chain `base fact → derived fluent`. Capture exactly:
- Each controller's drive condition (`energized`, beam-reaches conditions, corridor-clear rules).
- The per-output combine rule and its modes (e.g., normal = energized, inverted = not energized,
  toggle if present), including override precedence (e.g., jamming over an inverted force-close).
- Which outputs are uncontrolled and therefore only forceable by a direct base action (jamming).

### 6. World-mode enumeration
- Identify the **minimal set of agent-controllable base toggles** that fix all derived state
  (e.g., "is the jammer on gate X?", "is corridor location L occupied?").
- Enumerate the reachable combinations as named modes. For each mode give the **full derived
  vector** (every gate/fluent) plus which **movement, reach, and sight** passages it opens/closes.
- List the **transitions**: which action flips which toggle, and hence which mode pairs are adjacent.
- Watch for mutually exclusive modes (a passage needed for step A only opens in a mode that closes
  the passage needed for step B) — that tension is usually the puzzle's core.

### 7. Goal reduction
- State the goal fluent(s).
- Find the **forced terminal action**: the `define-action` whose effect can assert the goal fluent.
  If only one action/branch can, it is forced. Recurse onto its preconditions.
- Pin the **mode** the terminal action must fire in (holding/occupancy preconditions often force a
  specific mode), and read that mode's row in Section 6 to see what vantages/edges are then live.

### 8. Extra-spec assumptions / to-confirm
List **only** facts a candidate new/changed edge's plausibility would depend on that the spec
cannot supply: adjacencies, which side of a wall a spot lies on, reach distance, sightline
geometry. Phrase each as a reading of the diagram (or an open question if none was given), and ask
for confirmation. Nothing here is a spec fact.

---

## Self-check before presenting
- Does every Section 1–7 claim trace to a line in the current file?
- Are movement, reach, and sight kept strictly separate?
- Is the computed start state internally consistent (re-run the cascade to a fixpoint)?
- Is all geometry confined to Section 8 and clearly marked unconfirmed?
- Are unused relation/obstacle kinds omitted rather than invented?
- Have I named the missing type members and the scarce resources?
