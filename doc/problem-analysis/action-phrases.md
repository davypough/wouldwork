# Action phrases and replay

User-facing action reports use the staged action's phrase template. For example:

```lisp
(PICKUP-TRAY > AGENT1 picks up TRAY1 at LOCATION21 from LOCATION21)
```

The first location belongs to the tray; the second belongs to the agent. The two
values can differ for a pickup within reach.

`validate-solution` accepts both this printed form and the plain form:

```lisp
(PICKUP-TRAY AGENT1 TRAY1 LOCATION21 LOCATION21)
```

Copy the complete parenthesized action, including `>` and the phrase words. Words
are checked against the currently staged template, without regard to case. Missing,
incorrect, or extra phrase words produce a malformed-action-phrase diagnostic.
Only phrase words are unquoted: string-valued arguments retain quotes, and nested
routes and connector pairing lists retain their parentheses. This is template-based
Lisp syntax, not a free-form natural-language parser.

The shared formatter in `src/ww-action-format.lisp` serves solution paths, recorder
phase reports, replay diagnostics, action traces, action tests, backward-search
reports, and state displays. Internal action lists and saved checkpoint data stay
plain. Actions without phrase templates, `WAIT` durations, and report markers such
as `(PAUSE)` remain in their existing readable form. Invalid input is shown verbatim
in error reports so its mistake remains visible.

After reloading Wouldwork, load `test/action-phrases.lisp` and call
`(test-action-phrases)` for focused syntax, print/read, and replay-lookahead checks.
After staging `crelay-topo`, `(test-crelay-action-phrase-replay)` checks the maintained
94-action solution in both forms, compares their final states, checks the goal,
and independently validates recorder execution. Neither test launches a search.

Validation status: the user ran both tests successfully:

```text
ACTION-PHRASE-TESTS: PASS
CRELAY-ACTION-PHRASES: PASS (94 actions, both forms, goal and recorder)
```

The full check confirms both forms execute all 94 actions, satisfy the goal, and
produce matching final databases and time; independent recorder validation also
passes. No solve/search was run. The test resolves the generated goal and recorder
functions at runtime because staging uninterns and recreates the goal symbol.
