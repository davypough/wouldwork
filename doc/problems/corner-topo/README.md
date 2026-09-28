# Corner problem notes

This directory is an archive of analysis and solution output for the older,
hand-authored `probs/problem-corner.lisp` family. Its examples use that model's `loc`,
`current-beams`, and `connect-to-N-terminus` vocabulary. They are useful as historical
reasoning and performance evidence, but they are not executable examples for the current
technology-based `probs/problem-corner-topo.lisp`.

The current model uses `has-location`/`on`, `connect-connector`, the route-bearing `move`
action, `los-via`, and `traversal-via`; its beam and movement topology is derived from the
coordinates and segments in the problem file. See `tech/README.html` for the authoritative
technology behavior and `tech/Talos Technology  Summary.txt` for current relation
signatures.

`problem-corner-topo-plus.jpg` is the diagram associated with the topology version. The
`goal analysis/`, `solutions/`, and `enumerator/` subdirectories otherwise retain legacy
material and should not be copied into a new tech-based problem without translating its
representations.
