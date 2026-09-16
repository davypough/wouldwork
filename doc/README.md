# doc/

Supporting documentation for Wouldwork. The *Wouldwork User Manual* in the project root is the primary reference for using the system; these are supplementary — engine internals, authoring and analysis procedures, applicability guidance, and per-problem working notes.

Two other documentation homes sit outside this directory: `tech/README.html` is authoritative for the Talos technology library, and `tech/Talos Technology  Summary.txt` is the current relation inventory.

---

## Directories

### `load-ordering/` — how the engine loads a problem

| File | Answers |
|---|---|
| `ordering-of-operations.md` | *When* does each thing happen, from ASDF bootstrap to the end of `init()`? Five stages, what each freezes, and ten ordering traps. |
| `parameter-precedence.md` | *Where did this parameter's value come from, and who wins?* The four value sources, what `vals.lisp` persists, and what `stage` / `refresh` / `ww-reset` each do. |

Read these when a problem loads but behaves as though a form didn't take effect, when a derived table comes back empty, or when a setting keeps reverting.

### `problem-analysis/` — writing and diagnosing specs

| File | Use when |
|---|---|
| `wouldwork-problem-template.md` | Writing a new spec. Opens with the Talos/`tech`-based vs. hand-authored fork, then the DSL reference and an interview template. |
| `working-reference-builder.md` | A spec exists and needs analysis. Normalizes its scattered facts into one verified working reference. |
| `inferring-missing-relations.md` | A spec is correct but unsolvable because one relation instance is missing. Assumes a working reference already exists. |
| `action-phrases.md` | Reading action reports and pasting phrase or plain forms into replay validation. |

These form a sequence: write → normalize → diagnose.

### `search-strategies/` — making a hard problem tractable

`heuristics.md` and `relaxation.md` cover two strategies that are easily confused, with
applicability criteria and worked examples. A heuristic changes exploration *order*; a
relaxation changes which states are *legal* and requires goal post-validation.
`novelty.md` retains the analysis of a retired incomplete pruning experiment.

### `problems/` — per-problem working notes

Raw analysis material for individual problems: goal deductions, solution traces, enumerator output, diagrams. Notes rather than guidance, and not maintained as reference documentation.

---

## Parallel-search investigation

Start with [parallel-search-defaults.md](parallel-search-defaults.md): completed integration, default operation and validation. Earlier investigation documents below retain historical results.
The maintained design and evidence are:

- [Snapshot audit](parallel-snapshot-audit.md): ownership and lifecycle protections.
- [Traversal cache results](parallel-traversal-cache-results.md): completed bounded diagnostic; its recommended integration is now installed.
- [Corner support and generalization](parallel-snapshot-generalization.md): historical extension and user-reported result; superseded by the default closeout.
- [Selector review](parallel-selector-review.md) and [timing](parallel-selector-timing.md): retained patch and validation.
- [Baseline](parallel-baseline.md) and [handoff](parallel-search-handoff.md): detailed evidence and investigation context; earlier status/approval entries are chronological, not current instructions.

The original snapshot proposal, completed serial-overhead plan and old selector
continuation were removed during the 2026-09-14 documentation cleanup because
the retained documents supersede them. Raw reports, diagnostics, caches and
reference worktrees remain preserved.

## Conventions

- **Markdown for anything maintained as reference.** Generated artifacts meant to be read rather than edited go to `artifacts/` as HTML.
- **Cite files and function names, not line numbers.** Line references go stale on the first edit above them and give no signal when they do. The exception is a within-session transcription check against a file open in front of you.
- **Name the authoritative source rather than duplicating it.** Where `tech/README.html` or the Manual covers something, point at it.
- **Say when a document has been superseded.** Several files here were written against representations that no longer exist; where reasoning was worth keeping, it is retained under a header saying so rather than silently left to look current.
