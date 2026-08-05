# Wouldwork Working-Reference Builder — Prompt

Task is to build a Wouldwork Working Reference given a problem spec (and, optionally, an environment diagram). It instructs you to build one markdown **working reference** that co-locates and normalizes the spec's scattered facts, so they can be read off directly during analysis instead of reconstructed from memory.

**Where this fits.** The working reference is the input to analysis, not the analysis itself. Once verified, it feeds `inferring-missing-relations.md`, which uses it to locate an omitted relation in an otherwise-correct spec. For writing a spec in the first place, see `wouldwork-problem-template.md`. For relation signatures, `tech/Talos Technology  Relations.txt` is authoritative.

> **Status:** Sections 1–3 were rewritten against the current `tech/` relation vocabulary. The previous version named relations (`in-area`, `interface`, `traversable>`, `los-via`, `in-los-group`, `reachable-via`) that exist in no current problem file. Discipline rule 4 and Section 8 were revised: geometry is spec fact in coordinate-derived problems and is no longer quarantined by default.

---

## Inputs

- **Required:** the problem spec `.lisp` file. Read it **fresh from disk every time**. Never rely on memory of a prior version, a prior session's reference, or a remembered conclusion. If a remembered answer surfaces, set it aside and derive from the current artifact.
- **Optional:** a hand drawing / diagram of the environment.
- **Optional but preferred:** the load printout from staging the problem. For coordinate-derived specs this is the only place the derived edge tables can actually be read.

---

## Step 0 — Determine the authoring path

Settle this before transcribing anything; it changes what Sections 1–3 can contain.

**Does the spec assert any `wall-segment>` facts?**

- **Yes — coordinate-derived.** The movement and sightline tables are *computed at initialization* from raw 2D segment geometry: `-walkability-coordinates` derives `walk-via` / `walk-via>`, and `-beam-los-coordinates` derives the `los-to-*` tables. **These facts are not in the file and cannot be transcribed from it.** Record the geometry inputs — `location-coords>`, `wall-segment>`, `gate-segment>`, `window-segment>`, `screen-segment>`, `boundary-wall` — as the authoritative source, and take the derived edges from the load printout, marked as derived output.
- **No — hand-authored.** The spec states its edges directly. Transcribe them.

Legacy specs may be mixed: `problem-corner.lisp` asserts segment lists *and* hand-authored sightline relations in its own older vocabulary (`los0`/`los1`, `visible0`/`visible1`, `accessible0`/`accessible1`). Transcribe whatever that file actually declares; do not translate it into `tech/` names.

**If a derived table comes back empty or short**, suspect a load-ordering fault before suspecting the geometry. Two failure modes are silent: an init-action skipped because one of its parameter types has no instances, and a type declared *below* the `include-tech` block, which collapses every `doall` over it into a no-op. See `../load-ordering/ordering-of-operations.md`, Traps 3 and 4.

---

## Non-negotiable discipline

1. **Transcribe, don't recall.** Every structural fact in Sections 1–7 comes verbatim from the current file, or from the load printout where the fact is derived. Cite line numbers where it aids the transcription check.
2. **Spec stays authoritative.** The reference is a *derived* aid, not ground truth. On any conflict, the spec wins — fix the reference, never the other way around.
3. **Keep the capability families separate.** Walking (walkability), sight (visibility), and reach (reachability) are distinct graphs with distinct passability rules. Never merge them; a fact true in one is routinely false in the others. In particular: walking is agent-dependent, reach and sight are not; and a screen or ladder that a walking agent can pass empty-handed blocks reach absolutely.
4. **Separate what the spec states from what it cannot.** In a coordinate-derived spec, geometry *is* spec fact — record positions and segments in Sections 1–3 like any other declaration. Reserve Section 8 for what genuinely cannot be read off the spec: an unstated adjacency, a diagram-only feature, an intended-but-unmodeled relationship. If the spec has no geometry at all, then wall sides and sightline plausibility do belong in Section 8, as readings of the diagram flagged for confirmation.
5. **Compute the start state once.** Hand-run the derivation cascade (the `define-update` functions, in `propagate-consequences!` order) over the init facts and record the actual initial derived vector — open gates, active receivers, crossing states. Don't leave it implicit. Note that on a tech-based spec this driver is usually *derived from splice order* rather than authored, so read the technology include order to get the sequence right.
6. **Words, not symbols, in legends.** Use tokens like `clear` / `(occluders…)` / `none`. Bare symbols (—, ·) render inconsistently and invite transcription drift.
7. **Include only what's declared.** Omit sections, branches, and obstacle kinds the spec doesn't actually use. A problem with no reach edges has no reach section.
8. **Present for verification before relying on it.** Show the finished reference, ask for a transcription check (call out the most error-prone table explicitly), and only then reason off the verified version.
9. **Regenerate per problem.** Never reuse a stale reference; rebuild from the current file.

---

## Output: the markdown working reference

Open with a **header block**: source filename; authoring path (coordinate-derived or hand-authored); the technologies included, if any; the `ww-set` config (`*problem-type*`, `*solution-type*`, `*tree-or-graph*`, `*depth-cutoff*`); and any **type members declared in `define-types` but absent from every init relation** — the "missing pieces" the analysis must place.

Then the sections below. Treat them as a **template**: drop any the problem doesn't exercise, and add problem-specific ones where a derived layer doesn't fit the headings.

### 1. Walkability network

From `walk-via`, `walk-via>`, and the passability queries `obstacle-clear` / `all-clear` (in `tech/-passability.lisp`), consumed by `walkable-locations`, `walkable`, and `one-step-walkable`. Keep the separate `jump-via`, `jump-via>`, and `climb-via>` locomotion edges outside this walking network.

- **The clause convention, stated once.** These `$list` values are **DNF door-clause lists**: `()` means direct and unguarded; a nonempty value is **OR over clauses, AND within a clause**. `((gate1) (gate2 gate3))` reads *gate1 open, or else both gate2 and gate3 open*. Record each edge's clause list exactly — collapsing alternatives into one flat list changes the meaning.
- **Each walking edge** with its clause list. Mark direction: `walk-via` is symmetric; `walk-via>` is directional, and the reverse direction may have a different clause list or none.
- **Separate locomotion edges.** Record `jump-via` / `jump-via>` and `climb-via>` separately; they are not members of the walking closure.
- **The per-kind passability rule** for each obstacle kind the spec actually uses, read from `obstacle-clear`: a **gate** passes when open; a **screen** or **ladder** passes only when the agent is empty-handed; a **gears** item is an air-stream crossing, passable unless a blowing fan is mounted.
- **Air streams, if present.** They are derived, not authored: each wall-gears' band runs from the solid backstop behind its fan, through its `has-position` swept location, to its `aimed-at` destination, 3 units wide unless `stream-width` overrides. The swept location is standable exactly while the stream is off.
- **Flag any location with no walk edge at all**, and any location reachable only by a directional edge.

For a coordinate-derived spec, record the geometry inputs and the derived edge count, and note that the edge table comes from the load printout.

### 2. Reachability (getting/putting) network

From `reach-via` and `reachable-clear` (`tech/reachability.lisp`).

- **Every reach edge with its barrier list.** Restate the algebra explicitly, because it differs from movement at three points: the list is a **flat conjunction** — every gate in it must be open, with no alternative clauses; the relation is **symmetric** but **not transitive**; and it is **agent-independent**, so carrying does not matter. `reachable` is also trivially true for identical endpoints.
- **The barrier-clearing rule.** `reachable-clear` admits a barrier only if it is a gate and it is open. A closed gate blocks, and **any non-gate barrier blocks absolutely** — there is no empty-handed exemption as there is on a walk edge.
- **Note which locations no reach edge touches.**

### 3. Visibility (line of sight) network

From `los-to-location`, `los-to-target`, `los-to-apparatus`, and `visible-clear` (`tech/visibility.lisp`).

There are no sightline groups in the current representation — entries are per location, or per location pair.

- **Split the tables by consuming role**, since that is how the relations are split, and using the wrong one is a common error:
  - `los-to-target` — a jammer's target. **Gates only.** A gears jam target instead resolves through its `has-position` location's `los-to-location` entry.
  - `los-to-apparatus` — beam pairing with a transmitter or receiver.
  - `los-to-location` — everything else, including connector-to-connector pairing.
- **A location × target table of occluder lists** for each role in use. Use the word-token legend (rule 6): `clear` for an empty list, `(occluders…)` where a sightline exists but is occluded, `none` where there is no entry at all and therefore no sightline. **This table is the historically error-prone one — transcribe it exactly and call it out in the verification request.**
- **The transparency rule** from `visible-clear`: a sightline must exist in the tables, and is clear iff every occluder is an open gate.
- **Flag any location with no sight data.**

### 4. Object / role inventory

Every movable (cargo) object: start location, kind, the roles it can play (mover, carrier, jammer/placer, beam-blocker, …), and **what world state each can independently force**. Name any scarce, load-bearing resource — a single jammer, one connector short of the pairings needed — explicitly. Overloaded resources drive the puzzle.

### 5. Derived-state dependency chain

Read the truth condition of each derived fluent off the `define-query` helpers and `define-update` combine rules, and write the chain `base fact → derived fluent`. Capture exactly:

- Each controller's drive condition (`energized`, beam-reaches conditions, corridor-clear rules).
- The per-output combine rule and its modes — `normal` = open when energized, `inverted` = open when not, `toggle` if present — including override precedence. Jamming overrides: a jammed gate is forced open, jammed gears forced stopped.
- Note that `controls` takes a **DNF clause list** of controllers, same convention as the movement edges.
- Which outputs are uncontrolled and therefore forceable only by a direct base action such as jamming. These usually pin the terminal action.
- If beams are in play, the crossing bookkeeping: `beam-via`, `crossings-along-beam>`, `beam-crossings-before-gate>`, and the dynamic `crossing-active`.

### 6. World-mode enumeration

- Identify the **minimal set of agent-controllable base toggles** that fix all derived state (e.g. "is the jammer on gate X?", "is corridor location L occupied?").
- Enumerate the reachable combinations as named modes. For each mode give the **full derived vector** — every gate and fluent — plus which **movement, reach, and sight** passages it opens or closes.
- List the **transitions**: which action flips which toggle, and hence which mode pairs are adjacent.
- Watch for mutually exclusive modes — a passage needed for step A opens only in a mode that closes the passage needed for step B. That tension is usually the puzzle's core, and it is what the deadlock patterns in `inferring-missing-relations.md` §5 name.

### 7. Goal reduction

- State the goal fluent(s).
- Find the **forced terminal action**: the `define-action` whose effect can assert the goal fluent. If only one action or branch can, it is forced. Recurse onto its preconditions.
- Pin the **mode** the terminal action must fire in — holding and occupancy preconditions often force a specific one — and read that mode's row in Section 6 to see what vantages and edges are then live.

### 8. Extra-spec assumptions / to-confirm

List **only** what the spec genuinely cannot supply and the analysis nonetheless depends on. In a coordinate-derived spec this section is often empty or near-empty, because positions and segments are declared; do not manufacture entries by re-listing geometry that Section 1 already recorded.

Legitimate entries: an adjacency the diagram shows but no relation encodes; a feature drawn but not modeled; an intended relationship the spec appears to be missing. Phrase each as a reading of the diagram, or as an open question if none was given, and ask for confirmation. Nothing here is a spec fact.

---

## Self-check before presenting

- Was the authoring path determined first, and do Sections 1–3 reflect it?
- Does every Section 1–7 claim trace to a line in the current file, or to the load printout for a derived fact?
- Are movement, reach, and sight kept strictly separate — including their differing barrier rules?
- Are DNF clause lists recorded as clauses, not flattened?
- Is `reach-via`'s list recorded as a flat conjunction rather than as alternatives?
- Is the computed start state internally consistent (re-run the cascade to a fixpoint)?
- Is Section 8 confined to what the spec cannot state, and clearly marked unconfirmed?
- Are unused relation and obstacle kinds omitted rather than invented?
- Have I named the missing type members and the scarce resources?
