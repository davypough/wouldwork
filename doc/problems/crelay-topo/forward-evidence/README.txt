Forward pilot evidence
======================
E001-output.txt contains the completed user-run replay qualification.
E001-interface-check.lisp is the durable replay-only qualification script.
E001-source-hashes.txt records relevant source identities before the user run.
E002-output.txt records the completed depth-1 search and accepted C003 prefix.
E002-one-off-check.lisp is its reproducible diagnostic; reruns need new log names.
C003-checkpoint.sexp holds actions and endpoint extracted from accepted E002 output.
E003-one-off-check.lisp reconstructs C003 and runs the approved depth-2 search.
E003-output.txt records the completed run and accepted C004 (17-action prefix).
C004-checkpoint.sexp contains its actions and endpoint extracted from E003 output.

For each Ennn retain readable action data, selected output, exact settings,
termination reason, endpoint signature, ordinary and technology validation,
and relevant source/specification hashes plus git revision/dirty-state identity.
Name files with the stable experiment or checkpoint ID. These are durable
records, not temporary scripts to delete when a feature is finished.

Store plain Lisp action forms where possible; replay supplies their execution
semantics. If timestamps are retained, distinguish cumulative times from local
segment times. Do not serialize opaque in-memory planner objects as checkpoints.
Record parent links and immutable segment data so cumulative prefixes can be
reconstructed without duplicating large transcripts in every planning document.

Do not place credentials, unrelated logs, or sealed solution copies here.

E004-E006 validation logs and candidate archives preserve accepted C005-C008.
E007-search-output.txt is search evidence only. E007-pending.sexp preserves the
printed display-form action plus its accepted parent reference for restart;
replay and cumulative recorder validation are still required before acceptance.
No generic combined search/validation/save runner has been implemented yet.
