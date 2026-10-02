# Tiles1e: a working feature characterization

Date: 2026-10-01. Status: first draft for discussion and refinement.

Spec: [problem-tiles1e-heuristic.lisp](../../probs/problem-tiles1e-heuristic.lisp).
Related discussion document: [Problem Classification Guide](problem-classification.md).

## Purpose and boundaries

Use this concrete example to develop a vocabulary for characterizing search problems before assigning classes, choosing strategies, or selecting Wouldwork parameters. The groups below are provisional organizing headings, not a finalized taxonomy. A feature may later move to another group without changing its meaning.

This draft comes from reading the spec and reasoning about its rules. No solver, state enumeration, or replay was run for this analysis. It describes the modeled puzzle; fidelity to the original Islands of Insight puzzle has not been independently checked.

## How to read an entry

Each entry records a feature, its value, its source, and supporting evidence or qualification.

- **Given:** Explicit in the spec.
- **User:** Supplied by the user, including intended requirements or clarifications beyond the spec.
- **Derived:** Established by reasoning from available facts.
- **Discovered:** Established through examination of concrete situations or exploration, such as an observed path or measured count.

Multiple sources may support an entry. Reading an explicit rule counts as Given, not Discovered. Source is separate from certainty: user suggestions and exploration results can be provisional. An observation about some states does not automatically describe all states.

Unless stated otherwise, established entries apply to this entire modeled instance. Unknown means not established here; not applicable means the feature has no role in this model. Future entries can use conditional values and narrower scopes, such as the initial state or a particular checkpoint.

**User-supplied scope:** Characterize this example and use it to refine the feature scheme. Classification and strategy selection are later steps. No additional puzzle facts have yet been supplied by the user.

## The puzzle in ordinary terms

Five rigid tiles occupy eleven cells of a 4 by 4 board, leaving five empty cells. One tile is a single square, two are straight two-cell tiles, and two are three-cell L shapes. One L shape, YL2, is the yellow target tile. A move slides one tile one cell up, down, left, or right, provided the newly occupied cells are empty and inside the board. Tiles cannot rotate.

The yellow tile must reach a specified position at the upper right. The other tiles have no specified final positions.

The following board is derived from the initial coordinates and the shapes encoded in the action rules. Rows run downward and columns rightward, both numbered 0 through 3.

```text
          column
          0   1   2   3
row 0     .   L   .   .
row 1     L   L   H   H
row 2     .   Y   .   V
row 3     Y   Y   S   V

. = empty   L = L1   Y = YL2   H = HOR   V = VER   S = SQ
```

For an L tile, the recorded coordinate is its upper cell. Its other cells are one row below, in the same column and the column to the left. Thus the yellow goal coordinate (0, 3) means occupying cells (0, 3), (1, 2), and (1, 3).

## 1. Available information

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| I1 | Starting situation | One fully specified arrangement | Given: `define-init` lists all five tile coordinates and all empty cells. |
| I2 | Hidden information | None in the model | Given: no concealed properties or observation actions are specified. |
| I3 | Information revealed by actions | None required | Derived: an action changes the arrangement; it does not reveal an unknown puzzle fact. |

## 2. Control and predictability

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| C1 | Result of a chosen move | Predictable | Derived: choosing a tile and a legal direction fixes its new coordinate and the new empty cells. |
| C2 | Outside events | None specified | Given: the spec contains tile moves and no outside-event rules. |
| C3 | Opponents or independent actors | None | Given: no other actor makes decisions in this model. |

## 3. Choices and change

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| A1 | Kind of choice | Select a tile and slide direction | Given: the four action definitions cover the four tile types. |
| A2 | Size of one move | One cell, one tile | Given: coordinate changes are one row or one column. |
| A3 | Changes of orientation | Not allowed | Given: no action rotates or reflects a tile. |
| A4 | Object creation or destruction | None | Given: moves only relocate existing tiles and update empty cells. |
| A5 | Nature of the answer | A sequence of moves | Derived: the required destination must be reached through legal intermediate arrangements. A final arrangement alone does not supply that sequence. |

## 4. Order and prerequisites

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| P1 | Immediate move prerequisites | Space at the tile's entering edge | Given: action rules check the new cells for emptiness and board bounds. |
| P2 | Dependence on action order | Present | Derived: moving a tile changes which later moves have room. |
| P3 | Permanent unlocks | None specified | Given: there are no keys, switches, or earned permissions. Availability depends on the current arrangement. |
| P4 | Necessary intermediate arrangements | Unknown | No mandatory passage or arrangement has been established in this analysis. |

## 5. Reversibility and commitment

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| R1 | Can a move be undone immediately? | Yes, for every legal move from a valid arrangement | Derived: each directional rule has its opposite; the forward move vacates exactly the cells needed for the reverse move. |
| R2 | Recovery cost for the most recent move | One move | Derived from R1. Recovering an older arrangement may require reversing several moves. |
| R3 | Permanent loss of resources or opportunities | None through legal movement | Derived: any finite move sequence can be reversed to restore its starting arrangement. |
| R4 | Can a reachable move destroy solvability? | No, if the starting arrangement is solvable | Derived: reverse the path to the start, then take a solution. Solvability of the start is not proved here. |
| R5 | Can useful intermediate progress be undone? | Yes | Derived: reversibility also allows moving a useful tile out of place. Whether a solution must undo particular progress is unknown. |

Reversibility does not establish that the goal is reachable, that detours are short, or that a checkpoint can be preserved while solving the remainder. A state can also fail under a remaining move budget even when unrestricted recovery is possible.

## 6. Resources and capacity

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| Q1 | Shared capacity | Board space | Derived: tiles compete for cells and for room to move. |
| Q2 | Empty space available | Five cells, conserved | Given + Derived: five initial empty cells; each move replaces entering empty cells with the same number of vacated cells. |
| Q3 | Does resource location matter? | Yes | Derived: five empty cells scattered elsewhere cannot substitute for empty cells at a tile's entering edge. |
| Q4 | Consumable resources | None | Given: no fuel, expendable items, or move allowance appears in the puzzle rules. The configured search cutoff is separate. |

## 7. Interaction and separability

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| X1 | Do objects interact? | Yes, through space | Derived: one tile's occupancy can prevent another tile's move. |
| X2 | Can the target be considered independently? | Not for determining legal paths | Derived: its legal moves depend on cells occupied or vacated by other tiles. |
| X3 | Independent subproblems | None established | Unknown: spatial interaction alone does not prove that every part is strongly coupled at every stage. |
| X4 | Changes beyond the moved object | Empty-cell availability changes | Given: every action updates both the moved tile and the empty-cell list. No chain of device updates is specified. |

## 8. Time and concurrency

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| T1 | Dependence on elapsed time | None in the puzzle rules | Given: no clocks, deadlines, or time-dependent legality conditions appear. |
| T2 | Simultaneous movement required | No | Given: each move concerns a single tile. |
| T3 | Effect of waiting | No modeled effect | Given: no wait action or independently changing world is specified. |

The numeric action field is 1 in every action definition. This observation alone is not used to infer physical timing requirements.

## 9. Goals and ongoing requirements

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| G1 | Target condition | Yellow tile coordinate (0, 3) | Given: `define-goal` requires `(loc YL2 0 3)`. |
| G2 | Completeness of final arrangement | Partially specified | Given: no final coordinates are required for the other four tiles. How many goal arrangements are reachable is unknown. |
| G3 | Conditions on the journey | Stay inside the board and avoid overlap | Given + Derived: entry-cell checks enforce these conditions starting from the valid initial arrangement. |
| G4 | Required visits or action history | None | Given: the goal asks only for the yellow tile's final coordinate. |
| G5 | Desired quality of answer | Any solution in the current configuration; user's eventual preference not yet established | Given: `*solution-type*` is `first`. This setting is not an intrinsic property of the puzzle. |

## 10. Regularity and equivalence

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| E1 | Repeated local movement rules | Present | Given: the same shape-specific sliding rules apply wherever their boundary and emptiness checks pass. |
| E2 | Different paths to the same arrangement | Present | Derived: a move followed by its reverse returns to the same arrangement. |
| E3 | Interchangeable objects for the complete task | No simple exchange established | Derived: L1 and YL2 share movement rules, but the goal singles out YL2; the other tiles have distinct shapes or orientations. |
| E4 | Whole-problem geometric symmetry | Unknown; not established by the square board alone | Any candidate must preserve tile orientations, the initial arrangement, and the distinguished goal. |
| E5 | Explicit size-parameter family | Absent from this spec | Given: the board bounds and objects are fixed. Larger variants would be separate specifications or a later generalization. |

## 11. Progress and failure

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| H1 | Available numerical progress measure | Target's row distance plus column distance to its goal | Given: `heuristic?` defines this measure. |
| H2 | What that measure omits | Work needed to reposition other tiles | Derived: the formula uses only the target coordinate and goal coordinate. A non-target move leaves it unchanged. |
| H3 | Lower bound on remaining moves | The distance measure is a lower bound | Derived: one move can reduce that distance by at most one. Initially it is 4; this is not a claim that four moves suffice. |
| H4 | Does decreasing distance reliably select useful moves? | Unknown | The formula's existence does not establish its effectiveness as guidance. |
| H5 | Must the target sometimes move away from its goal? | Unknown | Not established by the rules alone in this analysis. |
| H6 | Recognition of illegal next moves | Immediate | Given: the rules test space and boundaries before permitting a move. |
| H7 | Recognition of an unreachable goal | Not established | No reachability proof or exhaustive exploration was performed. Illegal moves and unsolvable arrangements are different questions. |

## 12. Scale and bounds

| ID | Feature | Value | Source and evidence |
|---|---|---|---|
| B1 | Number of distinct arrangements | Finite; exact reachable count not established here | Derived: five fixed-orientation tiles on sixteen cells have finitely many placements. |
| B2 | Number of immediate choices | At most 20; actual count depends on arrangement | Derived: five tiles times four directions, with illegal moves excluded. |
| B3 | Fixed solution length | No length forced by a steadily consumed quantity | Derived: nothing is consumed; reversible detours can lengthen paths. Existence and shortest length remain unproved here. |
| B4 | Possible sequence length | Unbounded if repeated states are permitted | Derived: legal forward/reverse pairs can repeat. For example, SQ can initially move up into (2, 2) and back down. |
| B5 | Shortest solution length | Unknown in this analysis | The configured cutoff of 40 is a search setting, not evidence of a shortest solution. |
| B6 | Change of difficulty by phase | Unknown | No solution trajectory or exploration was examined for this draft. |
| B7 | Cost of evaluating moves | Simple local checks in the spec; runtime unmeasured | Given: rules check entering cells and update coordinates and a short empty-cell list. No performance claim is made. |

## Representation and settings: separate from puzzle features

The spec represents the situation with tile coordinates and a sorted list of empty cells. For valid arrangements, empty cells can also be derived from tile occupancy. Keeping both is a representation choice, not an additional physical feature.

Current explicit settings are planning, first solution, graph search, and a depth cutoff of 40. A distance heuristic is defined. Defining it does not by itself establish which configured search mechanism uses it; engine behavior and inherited settings were not audited here. These settings are recorded as context, not recommendations.

The existing classification guide reports 866 reachable states and a shortest solution of 40 from earlier work. Those are prior reported findings, not newly verified results of this characterization. Before promoting them to Discovered entries, consult their supporting enumeration or replay evidence and confirm the same spec and move-count convention.

## What this example suggests about the feature scheme

1. Separate a problem fact from its source, certainty, scope, and evidence.
2. Allow quantities and conditional values, not only yes/no choices.
3. Distinguish a goal's requirements from the user's requested solution quality and the solver's configured limits.
4. Distinguish reversible actions from preservation of useful progress.
5. Distinguish a measure that exists from a measure that actually guides search well.
6. Distinguish physical properties from bookkeeping introduced by the representation.
7. Allow unknowns without requiring a solve to complete the first characterization.

## Next discussion

Review the feature definitions and grouping before filling unknowns through exploration. Are entries sufficiently individual, are any duplicates, and what important aspect of this example is missing? After refining this example, use a contrasting problem to test the vocabulary's breadth. Classification labels, strategy pairings, and parameter recommendations remain deferred.
