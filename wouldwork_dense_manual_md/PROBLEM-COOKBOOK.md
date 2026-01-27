# Problem Cookbook (Appendix Rewritten as Recipes)

Each entry contains:
- the original specification and run instructions (source extract)
- plus a short “what this teaches you” wrapper.

## Blocks World Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Blocks World Problem (source extract)</strong></summary>

Develop a plan to stack three blocks on a table.

```lisp
Blocks Problem Specification:

;;; Filename: problem-blocks3.lisp

;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.

(in-package :ww) ;required

(ww-set *problem-name* blocks3)

(ww-set *problem-type* planning) ;use planning (vs constraint satisfaction) search strategy

(ww-set *solution-type* every) ;find every possible solution

(ww-set *tree-or-graph* tree)

(define-types
 block (A B C)
 table (T)
 support (either block table)) ;a block or table can be a support (for a block)

(define-dynamic-relations
 (on block support)) ;a block can be on a support

(define-query cleartop? (?block)
 (not (exists (?b block) ;?block has cleartop if there is no block on it
 (on ?b ?block))))

(define-action put
 1
 (standard ?block block (?block-support ?target) support) ;standard (optional) means ?block /= ?support /= ?target
 (and (cleartop? ?block) ;there is no other block on ?block
 (on ?block ?block-support) ;?block is on some ?support
 (or (and (block ?target) (cleartop? ?target)) ;there is no block on the ?target block
 (table ?target))) ;or the ?target is the table
 (?block ?target) ;the action description will be (put ?block ?target)
 (assert (on ?block ?target) ;new assertion added to state
 (not (on ?block ?block-support)))) ;previous assertion removed from state

(define-init
 (on A T)
 (on B T)
 (on C T))

(define-goal
 (or (and (on C T) (on B C) (on A B)) ;A -> B -> C -> T
 (and (on A T) (on B A) (on C B)))) ;C -> B -> A -> T

Blocks Problem Solution:

working...

New path to goal found at depth = 2

New path to goal found at depth = 3

New path to goal found at depth = 2

New path to goal found at depth = 3

In problem BLOCKS3, performed TREE search for EVERY solution.

DEPTH-FIRST search process completed normally.

Exhaustive search for every solution finished (up to the depth cutoff, if any).

Depth cutoff = 0

Maximum depth explored = 4

Program cycles = 21

Total states processed = 51

Average branching factor = 2.4

Start state:
((ON A T) (ON B T) (ON C T))

Goal:
(OR (AND (ON C T) (ON B C) (ON A B)) (AND (ON A T) (ON B A) (ON C B)))

Total solution paths recorded = 4, of which 2 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in a minimum path length solution = 2

A minimum length solution path from start state to goal state:
(1.0 (PUT B C))
(2.0 (PUT A B))

Final state:
((ON C T) (ON B C) (ON A B))

A shortest path solution is also a minimum duration solution.

Evaluation took:
 0.002 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 8,609,458 processor cycles
 327,536 bytes consed
```

</details>

## Boxes Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Boxes Problem (source extract)</strong></summary>

Move from area1 to area4 by placing boxes on pressure plates, which open the gates.

```lisp
Boxes Problem Specification:

;;; Filename: problem-boxes.lisp

;;; Problem specification for using boxes to move to an
;;; area through a sequence of gates controlled by
;;; pressure plates.

(in-package :ww) ;required

(ww-set *problem-name* boxes)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 15)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(define-types
 myself (me)
 box (box1 box2)
 gate (gate1 gate2 gate3)
 plate (plate1 plate2 plate3)
 area (area1 area2 area3 area4)
 object (either myself box plate))

(define-dynamic-relations
 (holding myself box)
 (loc (either myself box plate) area)
 (on box plate))

(define-static-relations
 (controls plate gate)
 (separates gate area area))

(define-query free? (?me)
 (not (exists (?b box)
 (holding ?me ?b))))

(define-query cleartop? (?plate)
 (not (exists (?b box)
 (on ?b ?plate))))

(define-query open? (?gate ?area1 ?area2)
 (and (separates ?gate ?area1 ?area2)
 (exists (?p plate)
 (and (controls ?p ?gate)
 (exists (?b box)
 (on ?b ?p))))))
(define-action move
 1
 ((?area1 ?area2) area)
 (and (loc me ?area1)
 (exists (?g gate)
 (open? ?g ?area1 ?area2)))
 (?area1 ?area2)
 (assert (not (loc me ?area1))
 (loc me ?area2)))

(define-action pickup
 1
 (?box box ?area area)
 (and (loc me ?area)
 (loc ?box ?area)
 (free? me))
 (?box ?area)
 (assert (not (loc ?box ?area))
 (holding me ?box)
 (exists (?p plate)
 (if (on ?box ?p)
 (not (on ?box ?p))))))

(define-action drop
 1
 (?box box ?area area)
 (and (loc me ?area)
 (holding me ?box))
 (?box ?area)
 (assert (loc ?box ?area)
 (not (holding me ?box))))

(define-action put
 1
 (?box box ?plate plate ?area area)
 (and (loc me ?area)
 (holding me ?box)
 (loc ?plate ?area)
 (cleartop? ?plate))
 (?box ?plate ?area)
 (assert (loc ?box ?area)
 (not (holding me ?box))
 (on ?box ?plate)))

(define-init
 ;dynamic
 (loc me area1)
 (loc box1 area1)
 (loc box2 area2)
 ;static
 (loc plate1 area1)
 (loc plate2 area1)
 (loc plate3 area3)
 (controls plate1 gate1)
 (controls plate2 gate2)
 (controls plate3 gate3)
 (separates gate1 area1 area2)
 (separates gate2 area1 area3)
 (separates gate3 area3 area4))

(define-goal
 (loc me area4))

Boxes Problem Solution:

working...

New path to goal found at depth = 14

New path to goal found at depth = 12

New path to goal found at depth = 10

In problem BOXES, performed GRAPH search for MIN-LENGTH solution.

DEPTH-FIRST search process completed normally.

Depth cutoff = 15

Maximum depth explored = 15

Program cycles = 57

Total states processed = 123

Repeated states = 63, ie, 51.2 percent

Average branching factor = 2.1

Start state:
((LOC ME AREA1) (LOC BOX1 AREA1) (LOC BOX2 AREA2) (LOC PLATE1 AREA1) (LOC PLATE2 AREA1) (LOC PLATE3 AREA3))

Goal:
(LOC ME AREA4)
Total solution paths recorded = 3, of which 2 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in a minimum path length solution = 10

A minimum length solution path from start state to goal state:
(1.0 (PICKUP BOX1 AREA1))
(2.0 (PUT BOX1 PLATE1 AREA1))
(3.0 (MOVE AREA1 AREA2))
(4.0 (PICKUP BOX2 AREA2))
(5.0 (MOVE AREA2 AREA1))
(6.0 (PUT BOX2 PLATE2 AREA1))
(7.0 (PICKUP BOX1 AREA1))
(8.0 (MOVE AREA1 AREA3))
(9.0 (PUT BOX1 PLATE3 AREA3))
(10.0 (MOVE AREA3 AREA4))

Final state:
((LOC ME AREA4) (LOC PLATE1 AREA1) (LOC PLATE2 AREA1) (LOC PLATE3 AREA3) (LOC BOX2 AREA1) (LOC BOX1 AREA3) (ON BOX2 PLATE2)
 (ON BOX1 PLATE3))

Evaluation took:
 0.013 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 [ Real times consist of 0.005 seconds GC time, and 0.008 seconds non-GC time. ]
 0.00% CPU
 42,565,021 processor cycles
 6,765,216 bytes consed
```

</details>

## 2-Jugs Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>2-Jugs Problem (source extract)</strong></summary>

Fill jugs at reservoir, pour water between jugs until exactly 1 gallon remains. This is set up as a min-time problem, requiring 6 steps, rather than a min-length problem, which requires only 4 steps . Note that it takes a long time to empty a jug (10 time units), since you must walk some distance to do it.

```lisp
2-Jugs Problem Specification:

;;; Filename: problem-jugs2.lisp

;;; Fluent problem specification for pouring between jugs
;;; to achieve 1 gal given 2-gal jug & 5-gal jug.

(in-package :ww) ;required

(ww-set *problem-name* jugs2)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-time)

(ww-set *tree-or-graph* graph)

(define-types
 jug (jug1 jug2))

(define-dynamic-relations
 (contents jug $integer))

(define-static-relations
 (capacity jug $integer))

(define-action fill
 2
 (?jug jug)
 (and (bind (contents ?jug $amt))
 (bind (capacity ?jug $cap))
 (< $amt $cap))
 (?jug $cap)
 (assert (contents ?jug $cap)))

(define-action empty
 10
 (?jug jug)
 (and (bind (contents ?jug $amt))
 (> $amt 0))
 (?jug)
 (assert (contents ?jug 0)))

(define-action pour ;A into B
 3
 ((?jugA ?jugB) jug)
 (and (bind (contents ?jugA $amtA))
 (> $amtA 0)
 (bind (contents ?jugB $amtB))
 (bind (capacity ?jugB $capB))
 (< $amtB $capB))
 (?jugA $amtA ?jugB $amtB $capB)
 (if (<= $amtA (- $capB $amtB))
 (assert (contents ?jugA 0)
 (contents ?jugB (+ $amtB $amtA)))
 (assert (contents ?jugA (- (+ $amtA $amtB) $capB))
 (contents ?jugB $capB))))

(define-init
 (contents jug1 0)
 (contents jug2 0)
 (capacity jug1 2)
 (capacity jug2 5))

(define-goal
 (or (contents jug1 1)
 (contents jug2 1)))

2-Jugs Problem Solution:

working...

New path to goal found at depth = 6
Time = 15.0

In problem JUGS2, performed GRAPH search for MIN-TIME solution.

DEPTH-FIRST search process completed normally.

Depth cutoff = 0

Maximum depth explored = 6

Program cycles = 9

Total states processed = 31

Repeated states = 17, ie, 54.8 percent

Average branching factor = 3.3

Start state:
((CONTENTS JUG1 0) (CONTENTS JUG2 0))

Goal:
(OR (CONTENTS JUG1 1) (CONTENTS JUG2 1))

Total solution paths recorded = 1, of which 1 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Duration of a minimum time solution = 15.0

A minimum time solution path from start state to goal state:
(2.0 (FILL JUG1 2))
(5.0 (POUR JUG1 2 JUG2 0 5))
(7.0 (FILL JUG1 2))
(10.0 (POUR JUG1 2 JUG2 2 5))
(12.0 (FILL JUG1 2))
(15.0 (POUR JUG1 2 JUG2 4 5))

Final state:
((CONTENTS JUG1 1) (CONTENTS JUG2 5))

Evaluation took:
 0.003 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 10,527,759 processor cycles
 5,979,536 bytes consed
```

</details>

## Sentry Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Sentry Problem (source extract)</strong></summary>

Move through an area guarded by an automatic laser gun, so as to jam an automated patrolling sentry, and move to the goal area. Gun1 sweeps area2. Switch1 turns gun1 on/off. Movable jammer1 can jam gun1 or sentry1. Sentry1 repeatedly patrols area5, area6, area7.

```lisp
Sentry Problem Specification:

;;; Filename: problem-sentry.lisp

;;; Problem specification for getting by an automated
;;; sentry by jamming it.

(in-package :ww) ;required

(ww-set *problem-name* sentry)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 16)

(define-types
 myself (me)
 box (box1)
 jammer (jammer1)
 gun (gun1)
 sentry (sentry1)
 switch (switch1)
 area (area1 area2 area3 area4 area5 area6 area7
 area8)
 cargo (either jammer box)
 threat (either gun sentry)
 target (either threat))

(define-dynamic-relations
 (holding myself cargo)
 (loc (either myself cargo threat target switch) area)
 (red switch)
 (green switch)
 (jamming jammer target))

(define-static-relations
 (adjacent area area)
 (los area target) ;line-of-sight exists
 (visible area area) ;area is visible from another area
 (controls switch gun)
 (watches gun area))

(define-query free? (?myself)
 (not (exists (?c cargo)
 (holding ?myself ?c))))

(define-query passable? (?area1 ?area2)
 (adjacent ?area1 ?area2))

(define-query active? (?threat)
 (not (or (exists (?j jammer)
 (jamming ?j ?threat))
 (forall (?s switch)
 (and (controls ?s ?threat)
 (green ?s))))))

(define-query safe? (?area)
 (not (exists (?g gun)
 (and (watches ?g ?area)
 (active? ?g)))))

(define-happening sentry1
 :inits ((loc sentry1 area6)) ;what's true at t=0
 :events ;events happening at t>0
 ((1 (not (loc sentry1 area6)) (loc sentry1 area7))
 (2 (not (loc sentry1 area7)) (loc sentry1 area6))
 (3 (not (loc sentry1 area6)) (loc sentry1 area5))
 (4 (not (loc sentry1 area5)) (loc sentry1 area6)))
 :repeat t
 :interrupt (exists (?j jammer)
 (jamming ?j sentry1)))

(define-constraint
 ;Constraints only needed for happening events that can
 ;kill or delay an action. Global constraints included
 ;here. Return t if constraint satisfied, nil if
 ;violated.
 (not (exists (?s sentry ?a area)
 (and (loc me ?a)
 (loc ?s ?a)
 (active? ?s)))))

(define-action jam
 1
 (?target target ?area2 area ?jammer jammer ?area1 area)
 (and (holding me ?jammer)
 (loc me ?area1)
 (loc ?target ?area2)
 (visible ?area1 ?area2))
 (?target ?jammer ?area1)
 (assert (not (holding me ?jammer))
 (loc ?jammer ?area1)
 (jamming ?jammer ?target)))

(define-action throw
 1
 (?switch switch ?area area)
 (and (free? me)
 (loc me ?area)
 (loc ?switch ?area))
 (?switch)
 (assert (if (red ?switch)
 (do (not (red ?switch))
 (green ?switch))
 (do (not (green ?switch))
 (red ?switch)))))

(define-action pickup
 1
 (?cargo cargo ?area area)
 (and (loc me ?area)
 (loc ?cargo ?area)
 (free? me))
 (?cargo ?area)
 (assert (not (loc ?cargo ?area))
 (holding me ?cargo)
 (exists (?t target)
 (if (and (jammer ?cargo)
 (jamming ?cargo ?t))
 (not (jamming ?cargo ?t))))))

(define-action drop
 1
 (?cargo cargo ?area area)
 (and (loc me ?area)
 (holding me ?cargo))
 (?cargo ?area)
 (assert (not (holding me ?cargo))
 (loc ?cargo ?area)))

(define-action move
 1
 ((?area1 ?area2) area)
 (and (loc me ?area1)
 (passable? ?area1 ?area2)
 (safe? ?area2))
 (?area1 ?area2)
 (assert (not (loc me ?area1))
 (loc me ?area2)))

(define-action wait
 0 ;always 0, wait for next exogenous event
 (?area area)
 (loc me ?area)
 ()
 (assert (waiting)))
(define-init
 ;dynamic
 (loc me area1)
 (loc jammer1 area1)
 (loc gun1 area2)
 (loc switch1 area3)
 (loc box1 area4)
 (red switch1)
 ;static
 (always-true)
 (watches gun1 area2)
 (controls switch1 gun1)
 (los area1 gun1)
 (los area2 gun1)
 (los area3 gun1)
 (los area4 gun1)
 (visible area5 area6)
 (visible area5 area7)
 (visible area5 area8)
 (visible area6 area7)
 (visible area6 area8)
 (visible area7 area8)
 (adjacent area1 area2)
 (adjacent area2 area3)
 (adjacent area2 area4)
 (adjacent area4 area5)
 (adjacent area5 area6)
 (adjacent area6 area7)
 (adjacent area7 area8))

(define-init-action derived-visibility
 0
 ((?area1 ?area2) area)
 (adjacent ?area1 ?area2)
 ()
 (assert (visible ?area1 ?area2)))

(define-goal
 (loc me area8))

Sentry Problem Solution:

working...

New path to goal found at depth = 16

In problem SENTRY, performed TREE search for FIRST solution.

Search ended with first solution found.

Depth cutoff = 16

Maximum depth explored = 16

Program cycles = 1,086

Total states processed = 1,624

Average branching factor = 1.5

Start state:
((LOC ME AREA1) (LOC JAMMER1 AREA1) (LOC GUN1 AREA2) (LOC SWITCH1 AREA3) (LOC BOX1 AREA4) (RED SWITCH1))

Goal:
(LOC ME AREA8)

Total solution paths recorded = 1, of which 1 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in first solution found: = 16

Duration of first solution found = 16.0

Solution path of first solution found from start state to goal state:
(1.0 (PICKUP JAMMER1 AREA1))
(2.0 (JAM GUN1 JAMMER1 AREA1))
(3.0 (MOVE AREA1 AREA2))
(4.0 (MOVE AREA2 AREA3))
(5.0 (THROW SWITCH1))
(6.0 (MOVE AREA3 AREA2))
(7.0 (MOVE AREA2 AREA1))
(8.0 (PICKUP JAMMER1 AREA1))
(9.0 (MOVE AREA1 AREA2))
(10.0 (MOVE AREA2 AREA4))
(11.0 (WAIT 1.0))
(12.0 (JAM SENTRY1 JAMMER1 AREA4))
(13.0 (MOVE AREA4 AREA5))
(14.0 (MOVE AREA5 AREA6))
(15.0 (MOVE AREA6 AREA7))
(16.0 (MOVE AREA7 AREA8))

Final state:
((GREEN SWITCH1) (JAMMING JAMMER1 SENTRY1) (LOC ME AREA8) (LOC JAMMER1 AREA4) (LOC GUN1 AREA2) (LOC SWITCH1 AREA3) (LOC BOX1 AREA4))

Evaluation took:
 0.047 seconds of real time
 0.031250 seconds of total run time (0.031250 user, 0.000000 system)
 [ Real times consist of 0.012 seconds GC time, and 0.035 seconds non-GC time. ]
 65.96% CPU
 150,013,329 processor cycles
 102,400,208 bytes consed

```

</details>

## 4-Queens Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>4-Queens Problem (source extract)</strong></summary>

rows: 1, 2, 3, 4
columns: 1, 2, 3, 4

 queen1 queen2 queen3 queen4

Place four queens on the board, so that no two queens are attacking each other. Place the first queen on row 1, the second on row 2, etc, until all queens are properly placed.

```lisp
4-Queens Problem Specification:

;;;; Filename: problem-queens4.lisp

;;; Problem specification for 4-queens.

(in-package :ww) ;required

(ww-set *problem-name* queens4)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)

(define-types
 queen (queen1 queen2 queen3 queen4)
 column (1 2 3 4))

(define-dynamic-relations
 (loc queen $fixnum $fixnum) ;row column of a queen
 (placed queen)
 (next-row $fixnum))

(define-action put
 1
 (?queen queen ?column column)
 (and (not (placed ?queen))
 (bind (next-row $row))
 (not (exists (?q queen)
 (and (placed ?q)
 (bind (loc ?q $r $c))
 (or ;(= $r $row) superfluous, always considering next row
 (= $c ?column)
 (= (- $r $row) (- $c ?column))
 (= (- $r $row) (- ?column $c)))))))
 (?queen $row ?column)
 (assert (loc ?queen $row ?column)
 (placed ?queen)
 (next-row (1+ $row))))

(define-init
 (next-row 1))

(define-goal
 (next-row 5))

4-Queens Problem Solution:

```
There are exactly 48 unique solutions to the 4-queens problem taking into account all possible successful arrangements of four distinct queens labeled queen1, queen2, queen3, queen4. Considering only the arrangements of unlabeled queens on the board, however, there are only two distinct successful arrangements. The shortest solution path listed below gives one of the 48 possible solutions. Each step in the solution below corresponds to placing a queen in successive rows 1-4. Thus, the first action labeled (PUT QUEEN4 1 3) means put queen4 in the 1st row of the 3rd column. For larger versions of this problem (such as with eight queens), making sure *tree-or-graph* = tree, should provide the only symmetrical 8-queens solution in under 1 second.

working...

New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4
New path to goal found at depth = 4

In problem QUEENS4, performed TREE search for EVERY solution.

DEPTH-FIRST search process completed normally.

Exhaustive search for every solution finished (up to the depth cutoff, if any).

Depth cutoff = 0

Maximum depth explored = 4

Program cycles = 185

Total states processed = 233

Average branching factor = 1.3

Start state:
((NEXT-ROW 1))
Goal:
(NEXT-ROW 5)

Total solution paths recorded = 48, of which 48 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in a minimum path length solution = 4

A minimum length solution path from start state to goal state:
(1.0 (PUT QUEEN4 1 3))
(2.0 (PUT QUEEN3 2 1))
(3.0 (PUT QUEEN2 3 4))
(4.0 (PUT QUEEN1 4 2))

Final state:
((LOC QUEEN4 1 3) (LOC QUEEN3 2 1) (LOC QUEEN2 3 4) (LOC QUEEN1 4 2) (NEXT-ROW 5) (PLACED QUEEN4) (PLACED QUEEN3) (PLACED QUEEN2)
 (PLACED QUEEN1))

A shortest path solution is also a minimum duration solution.

Evaluation took:
 0.009 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 31,051,751 processor cycles
```lisp
 2,030,576 bytes consed
```

</details>

## Smallspace Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Smallspace Problem (source extract)</strong></summary>

This is an example of a rather complex and lengthy specification that illustrates the integration of many Wouldwork planner features. It has served as a useful testbed for new features. It is a partial solution to a problem situation given in The Road to Gehenna, an add-on module for the Talos Principle game. The objective is to position a set of connectors such that they relay a laser beam from a transmitter source to a receiver of the same color which controls a gate. Once the receiver detects a beam of the proper color, it opens a gate. The goal is to move from area5 to area8.

```lisp
Smallspace Problem Specification:

;;; Filename: problem-smallspace.lisp

;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil
;;; dome. First leg to area8.
;;; Uses mixed fluent & non-fluent relations

(in-package :ww) ;required

(ww-set *problem-name* smallspace)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)

(define-types
 me (me1)
 gate (gate1 gate2)
 barrier (nil) ;cannot move cargo thru a barrier
 jammer (nil)
 gun (nil)
 connector (connector1 connector2)
 plate (nil)
 box (nil)
 fan (nil)
 gears (nil)
 switch (nil)
 ladder (nil)
 rostrum (nil)
 transmitter (transmitter1 transmitter2)
 receiver (receiver1 receiver2)
 hue (blue red) ;the color of a transmitter, receiver, or active connector
 area (area1 area2 area3 area4 area5 area6 area7 area8)
 cargo (either connector jammer box fan) ;what an agent (me) can pickup & carry
 target (either gate gears gun) ;what a jammer can jam
 divider (either barrier gate)
 terminus (either transmitter receiver connector) ;what a connector can connect to
 fixture (either transmitter receiver gears ladder rostrum)
 station (either fixture gate) ;useful for los determinations
 support (either box rostrum))

(define-dynamic-relations ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
 (holds me $cargo) ;fluent because we need to sometimes lookup what is currently being held
 ;(free me)
 (loc (either me cargo) $area)
 ;(on (either me cargo) $support)
 ;(attached fan gears)
 ;(jams jammer $target)
 (connects terminus terminus)
 (active (either connector receiver gate switch gun gears))
 (color terminus $hue))

(define-static-relations
 (adjacent area area) ;agent can always move to adjacent area unimpeded
 (locale fixture area) ;locale is a fixed location, loc is dynamic
 (barrier-separates barrier area area)
 (gate-separates gate area area)
 ;(separates divider area area)
 ;(climbable> ladder area area)
 ;(height support $real)
 (controls receiver gate)
 ;clear los from an area to a gate/fixture
 (los0 area (either gate fixture))
 (los1 area divider (either gate fixture))
 (los2 area divider divider (either gate fixture))
 ;could see a mobile object in an area from a given area
 (visible0 area area)
 (visible1 area divider area)
 (visible2 area divider divider area))

;;;; QUERY FUNCTIONS ;;;;

(define-query source? (?terminus)
 (or (transmitter ?terminus)
 (and (connector ?terminus)
 (active ?terminus))))

(define-query los-thru-2-dividers? (?area ?station)
 (exists ((?d1 ?d2) divider)
 (and (los2 ?area ?d1 ?d2 ?station)
 (or (and (barrier ?d1)
 (barrier ?d2))
 (and (barrier ?d1)
 (gate ?d2)
 (not (active ?d2)))
 (and (barrier ?d2)
 (gate ?d1)
 (not (active ?d1)))
 (and (gate ?d1)
 (active ?d1)
 (gate ?d2)
 (not (active ?d2)))))))
(define-query los-thru-1-divider? (?area ?station)
 (exists (?d divider)
 (and (los1 ?area ?d ?station)
 (or (barrier ?d)
 (and (gate ?d)
 (not (active ?d)))))))

(define-query los? (?area ?station)
 (or (los0 ?area ?station)
 (los-thru-1-divider? ?area ?station)
 (los-thru-2-dividers? ?area ?station)))

(define-query visible-thru-2-dividers? (?area1 ?area2)
 (exists ((?d1 ?d2) divider)
 (and (visible2 ?area1 ?d1 ?d2 ?area2)
 (or (and (barrier ?d1)
 (barrier ?d2))
 (and (barrier ?d1)
 (gate ?d2)
 (not (active ?d2)))
 (and (barrier ?d2)
 (gate ?d1)
 (not (active ?d1)))
 (and (gate ?d1)
 (active ?d1)
 (gate ?d2)
 (not (active ?d2)))))))

(define-query visible-thru-1-divider? (?area1 ?area2)
 (exists (?d divider)
 (and (visible1 ?area1 ?d ?area2)
 (or (barrier ?d)
 (and (gate ?d)
 (not (active ?d)))))))

(define-query visible? (?area1 ?area2)
 (or (visible0 ?area1 ?area2)
 (visible-thru-1-divider? ?area1 ?area2)
 (visible-thru-2-dividers? ?area1 ?area2)))

(define-query connector-has-valid-line-of-sight? (?connector ?hue)
 (or
 ;; Check direct transmitter connection with current line-of-sight
 (exists (?t transmitter)
 (and (connects ?connector ?t)
 (bind (color ?t $t-hue))
 (eql $t-hue ?hue)
 (bind (loc ?connector $c-area))
 (los? $c-area ?t)))
 ;; Check if connected to another active connector that has line-of-sight to transmitter
 (exists (?other-connector connector)
 (and (different ?other-connector ?connector)
 (connects ?connector ?other-connector)
 (active ?other-connector)
 (bind (color ?other-connector $other-hue))
 (eql $other-hue ?hue)
 ;; Verify the other connector has direct transmitter access
 (exists (?t transmitter)
 (and (connects ?other-connector ?t)
 (bind (color ?t $t2-hue))
 (eql $t2-hue ?hue)
 (bind (loc ?other-connector $other-area))
 (los? $other-area ?t)))))))

(define-query connectable? (?area ?terminus)
 (or (los? ?area ?terminus) ;from connector in area to terminus
 (and (connector ?terminus)
 (exists (?a area)
 (and (loc ?terminus ?a)
 (visible? ?area ?a))))))

(define-query passable? (?area1 ?area2)
 (or (adjacent ?area1 ?area2)
 (exists (?b barrier)
 (and (barrier-separates ?b ?area1 ?area2)
 (not (bind (holds me1 $cargo))))) ;must drop cargo first
 (exists (?g gate)
 (and (gate-separates ?g ?area1 ?area2)
 (not (active ?g))))))

;;;; UPDATE FUNCTIONS ;;;;

(define-update activate-connector! (?connector ?hue)
 (do (active ?connector)
 (color ?connector ?hue)))

(define-update deactivate-connector! (?connector ?hue)
 (do (not (active ?connector))
 (not (color ?connector ?hue))))

(define-update deactivate-receiver! (?receiver)
 (do (not (active ?receiver))
 (doall (?g gate)
 (if (controls ?receiver ?g)
 (active ?g)))))

(define-update chain-activate! (?terminus ?hue)
 (do
 ;; Step 1: Activate the terminus based on its type
 (if (connector ?terminus)
 (activate-connector! ?terminus ?hue)
 (if (receiver ?terminus)
 (do (active ?terminus)
 (doall (?g gate)
 (if (and (controls ?terminus ?g)
 (active ?g))
 (not (active ?g)))))))
 ;; Step 2: Handle cascading effects based on terminus type
 (if (connector ?terminus)
 (do
 ;; Connector activation: activate connected receivers of matching color
 (doall (?r receiver)
 (if (and (connects ?terminus ?r)
 (not (active ?r))
 (bind (color ?r $rhue))
 (eql $rhue ?hue))
 (chain-activate! ?r ?hue)))
 ;; Connector activation: activate connected connectors
 (doall (?c connector)
 (if (and (different ?c ?terminus)
 (connects ?terminus ?c)
 (not (active ?c)))
 (chain-activate! ?c ?hue))))
 ;; Handle receiver activation branch
 (if (receiver ?terminus)
 ;; Receiver activation: check for newly accessible connectors
 (doall (?c connector)
 (if (not (active ?c))
 (doall (?t transmitter)
 (if (and (connects ?c ?t)
 (bind (color ?t $t-hue))
 (eql $t-hue ?hue)
 (bind (loc ?c $c-area))
 (los? $c-area ?t))
 (chain-activate! ?c $t-hue)))))))))

(define-update chain-deactivate! (?connector ?hue)
 (do
 ;; Step 1: Deactivate this connector
 (deactivate-connector! ?connector ?hue)
 ;; Step 2: Deactivate receivers that lost power
 (doall (?r receiver)
 (if (and (connects ?connector ?r)
 (not (exists (?c connector)
 (and (different ?c ?connector)
 (connects ?c ?r)
 (active ?c)
 (bind (color ?c $c-hue))
 (eql $c-hue ?hue)))))
 (deactivate-receiver! ?r)))
 ;; Step 3: Connector revalidation
 (doall (?c connector)
 (if (and (active ?c)
 (bind (color ?c $c-hue))
 (eql $c-hue ?hue))
 ;; Check if this connector still has valid line-of-sight to power sources
 (if (not (connector-has-valid-line-of-sight? ?c ?hue))
 (chain-deactivate! ?c ?hue))))))

;;;; ACTIONS ;;;;

(define-action connect-to-1-terminus
 2
 (?terminus terminus)
 (and (bind (holds me1 $cargo))
 (connector $cargo)
 (bind (loc me1 $area))
 (connectable? $area ?terminus))
 ($cargo ?terminus $area $hue)
 (assert (not (holds me1 $cargo))
 (loc $cargo $area)
 (connects $cargo ?terminus)
 (if (and (source? ?terminus)
 (bind (color ?terminus $hue)))
 (activate-connector! $cargo $hue))))

(define-action connect-to-2-terminus
 3
 (combination (?terminus1 ?terminus2) terminus)
 (and (bind (holds me1 $cargo))
 (connector $cargo)
 (bind (loc me1 $area))
 (connectable? $area ?terminus1)
 (connectable? $area ?terminus2))
 ($cargo ?terminus1 ?terminus2 $area $hue)
 (assert (not (holds me1 $cargo))
 (loc $cargo $area)
 ;; Always establish physical connections
 (connects $cargo ?terminus1)
 (connects $cargo ?terminus2)
 ;; Extract source colors systematically
 (if (source? ?terminus1)
 (bind (color ?terminus1 $hue1))
 (setq $hue1 nil))
 (if (source? ?terminus2)
 (bind (color ?terminus2 $hue2))
 (setq $hue2 nil))
 ;; Determine activation color based on source consensus
 (if (and $hue1 $hue2) ; both sources active
 (if (eql $hue1 $hue2) ; same color
 (setq $hue $hue1)) ; activate with consensus color
 (if (or $hue1 $hue2) ; exactly one source active
 (setq $hue (or $hue1 $hue2)))) ; activate with available color
 (if $hue
 (chain-activate! $cargo $hue))))

(define-action connect-to-3-terminus
 4
 (combination (?terminus1 ?terminus2 ?terminus3) terminus)
 (and (bind (holds me1 $cargo))
 (connector $cargo)
 (bind (loc me1 $area))
 (connectable? $area ?terminus1)
 (connectable? $area ?terminus2)
 (connectable? $area ?terminus3))
 ($cargo ?terminus1 ?terminus2 ?terminus3 $area $hue)
 (assert (not (holds me1 $cargo))
 (loc $cargo $area)
 ;; Always establish physical connections
 (connects $cargo ?terminus1)
 (connects $cargo ?terminus2)
 (connects $cargo ?terminus3)
 ;; Extract source colors systematically
 (if (source? ?terminus1)
 (bind (color ?terminus1 $hue1))
 (setq $hue1 nil))
 (if (source? ?terminus2)
 (bind (color ?terminus2 $hue2))
 (setq $hue2 nil))
 (if (source? ?terminus3)
 (bind (color ?terminus3 $hue3))
 (setq $hue3 nil))
 ;; Systematic consensus determination
 (if (and $hue1 $hue2 $hue3) ; all three sources active
 (if (and (eql $hue1 $hue2) (eql $hue2 $hue3)) ; universal consensus
 (setq $hue $hue1))
 (if (and $hue1 $hue2) ; exactly two sources: 1 and 2
 (if (eql $hue1 $hue2) ; consensus between active pair
 (setq $hue $hue1))
 (if (and $hue1 $hue3) ; exactly two sources: 1 and 3
 (if (eql $hue1 $hue3) ; consensus between active pair
 (setq $hue $hue1))
 (if (and $hue2 $hue3) ; exactly two sources: 2 and 3
 (if (eql $hue2 $hue3) ; consensus between active pair
 (setq $hue $hue2))
 ;; exactly one source active
 (setq $hue (or $hue1 $hue2 $hue3))))))
 (if $hue
 (chain-activate! $cargo $hue))))

(define-action pickup-connector
 1
 (?connector connector)
 (and (not (bind (holds me1 $cargo)))
 (bind (loc me1 $area))
 (loc ?connector $area))
 (?connector $area)
 (assert (holds me1 ?connector)
 (not (loc ?connector $area))
 (if (bind (color ?connector $hue))
 (chain-deactivate! ?connector $hue))
 ;; Finally disconnect this picked up connector from everything
 (doall (?t terminus)
 (if (connects ?connector ?t)
 (not (connects ?connector ?t))))))

(define-action drop
 1
 ()
 (and (bind (loc me1 $area)) ;me1 is always located somewhere
 (bind (holds me1 $cargo))) ;if not holding, then bind statement returns nil, otherwise binds $cargo
 ($cargo $area)
 (assert (not (holds me1 $cargo))
 (loc $cargo $area)))

(define-action move
 1
 (?area2 area)
 (and (bind (loc me1 $area1))
 (different $area1 ?area2)
 (passable? $area1 ?area2))
 ($area1 ?area2)
 (assert (loc me1 ?area2)))

;;;; INITIALIZATION ;;;;

(define-init
 ;dynamic
 (loc me1 area5)
 (loc connector1 area5)
 (loc connector2 area7)
 ;(free me1)
 (active gate1)
 (active gate2)
 ;static
 (adjacent area1 area2)
 (adjacent area2 area3)
 (adjacent area3 area4)
 (adjacent area4 area5)
 (adjacent area6 area7)
 (locale transmitter1 area4)
 (locale transmitter2 area6)
 (locale receiver1 area4)
 (locale receiver2 area8)
 (color transmitter1 blue)
 (color transmitter2 red)
 (color receiver1 blue)
 (color receiver2 red)
 (controls receiver1 gate1)
 (controls receiver2 gate2)
 (gate-separates gate1 area4 area7)
 (gate-separates gate2 area7 area8)

 ;los is from an area to a fixed station
 ;(los0 area2 transmitter1)
 (los0 area3 transmitter1)
 (los0 area3 receiver1)
 (los0 area5 transmitter1)
 (los0 area5 receiver1)
 (los0 area5 receiver2)
 (los0 area6 transmitter1)
 ;(los0 area6 transmitter2)
 (los0 area7 transmitter2)
 (los0 area8 transmitter1)
 (los1 area7 gate1 transmitter1)
 (los1 area7 gate2 receiver2)
 (los1 area8 gate2 transmitter2)
 (los2 area3 gate1 gate2 receiver2)
 (los2 area4 gate1 gate2 receiver2)

 ;visibility is from an area to an area
 ;potentially containing a movable target or terminus
 (visible0 area1 area3)
 (visible0 area1 area4)
 ;(visible0 area1 area5)
 (visible0 area2 area4)
 (visible0 area2 area5)
 (visible0 area2 area6)
 (visible0 area3 area5)
 (visible0 area3 area6)
 (visible0 area3 area7)
 (visible0 area3 area8)
 (visible0 area4 area6)
 (visible0 area4 area8)
 (visible0 area5 area6)
 (visible0 area5 area8)
 (visible1 area1 gate1 area7)
 (visible1 area2 gate1 area7)
 (visible1 area3 gate1 area7)
 ;(visible1 area4 gate1 area7)
 (visible1 area4 gate1 area6)
 (visible1 area5 gate1 area6)
 (visible1 area5 gate1 area7)
 (visible1 area6 gate2 area8)
 ;(visible1 area7 gate2 area8)
 (visible2 area2 gate1 gate2 area8)
 (visible2 area3 gate1 gate2 area8)
 (visible2 area4 gate1 gate2 area8)
)

;;;; INITIALIZATION ACTIONS ;;;;

;init-actions save listing systematic facts

 (define-init-action init-los0
 ;los exists to any station within its local area
 0
 (?station station (?area1 ?area2) area)
 (or (locale ?station ?area1) ;for fixtures
 (gate-separates ?station ?area1 ?area2)) ;for gates
 ()
 (assert (los0 ?area1 ?station)))

 (define-init-action init-visible0-locally
 ;any object is visible from its own local area
 0
 (?area area)
 (always-true)
 ()
 (assert (visible0 ?area ?area)))

 (define-init-action init-visible0-via-adjacency
 ;any object is visible from an adjacent area
 0
 ((?area1 ?area2) area)
 (adjacent ?area1 ?area2)
 ()
 (assert (visible0 ?area1 ?area2)))

 (define-init-action init-visible1-thru-divider
 ;any object is visible thru a divider
 0
 (?divider divider (?area1 ?area2) area)
 (or (and (barrier ?divider)
 (barrier-separates ?divider ?area1 ?area2))
 (and (gate ?divider)
 (gate-separates ?divider ?area1 ?area2)))
 ()
 (assert (visible1 ?area1 ?divider ?area2)))

;;;; GOAL ;;;;

(define-goal ;always put this last
 (loc me1 area8))

;;;;;;;; Invariant Checks for Debugging ;;;;;;;;;;;;;;;

#+ignore (define-invariant active-connector-hue ()
 ;Connectors are active if and only if they have a hue
 (doall (?c connector)
 (equivalent (active ?c)
 (bind (color ?c $hue)))))

#+ignore (define-invariant holds-cargo-location ()
 ;Cargo cannot be both held and have a location simultaneously
 (not (and (bind (holds me1 $cargo))
 (bind (loc $cargo $area)))))

#+ignore (define-invariant receiver-activation ()
 ;A receiver is active if and only if there exists at least one
 ;connected, active connector of the same color.
 (doall (?r receiver)
 (if (bind (color ?r $rhue))
 (equivalent (active ?r)
 (exists (?c connector)
 (and (connects ?c ?r)
 (active ?c)
 (bind (color ?c $chue))
 (eql $chue $rhue)))))))

#+ignore (define-invariant receiver-gate-control ()
 ;A receiver is active if and only if all gates it controls are inactive
 (doall (?r receiver)
 (if (exists (?g gate)
 (controls ?r ?g))
 (equivalent (active ?r)
 (forall (?g gate)
 (if (controls ?r ?g)
 (not (active ?g))))))))

#+ignore (define-invariant colored-connector-connection ()
 ;Any colored connector must have a valid source with matching color,
 ;either a transmitter or another connector
 (doall (?c connector)
 (if (bind (color ?c $hue))
 (or (exists (?t transmitter)
 (and (connects ?c ?t)
 (bind (color ?t $t-hue))
 (eql $t-hue $hue)))
 (exists (?other connector)
 (and (different ?other ?c)
 (connects ?c ?other) ; Connected to it
 (bind (color ?other $other-hue))
 (eql $other-hue $hue)))))))

#+ignore (define-invariant connector-self-connection ()
 ;No connector is connected to itself
 (doall (?c connector)
 (not (connects ?c ?c))))

#+ignore (define-invariant me1-has-location ()
 ;Me1 is always located in some area
 (bind (loc me1 $area)))

#+ignore (define-invariant connector-transmitter-source ()
 ;Every active connector ultimately traces to transmitter sources without cycles
 (let (($valid-source t)) ; Assume valid until proven otherwise
 ;; For each active connector, verify it has a valid transmitter source
 (doall (?c connector)
 (if (active ?c)
 (do (setq $visited nil) ; Track visited connectors
 (setq $source-found nil) ; Flag if transmitter found
 (setq $stack nil) ; DFS stack
 (setq $current ?c) ; Current connector being examined
 (setq $color nil) ; Color we're tracing
 ;; Get the color of this connector
 (bind (color ?c $col))
 (setq $color $col)
 ;; Initialize stack with current connector
 (push $current $stack)
 ;; Perform depth-first search to find transmitter or detect cycle
 (ww-loop while $stack do
 ;; Pop current connector from stack
 (setq $current (pop $stack))
 ;; Only process this connector if we haven't seen it before
 (unless (member $current $visited)
 ;; Mark as visited
 (push $current $visited)
 ;; Check if current is directly connected to a transmitter of same color
 (if (exists (?t transmitter)
 (and (connects $current ?t)
 (bind (color ?t $t-color))
 (eql $t-color $color)))
 (setq $source-found t)
 ;; Otherwise, add connected connectors of same color to stack
 (doall (?other connector)
 (if (and (different ?other $current)
 (connects $current ?other)
 (active ?other)
 (bind (color ?other $other-color))
 (eql $other-color $color))
 (push ?other $stack))))))
 ;; If we've explored all paths and never found a transmitter source,
 ;; this connector has no valid source (either disconnected or in a cycle)
 (if (not $source-found)
 (setq $valid-source nil)))))
 ;; Return result
 $valid-source))

Smallspace Problem Solution:

working...

New path to goal found at depth = 19

In problem SMALLSPACE, performed GRAPH search for MIN-LENGTH solution.

DEPTH-FIRST search process completed normally.

Depth cutoff = 20

Maximum depth explored = 20

Program cycles = 34,745

Total states processed = 69,398

Repeated states = 51,583, ie, 74.3 percent

Average branching factor = 2.0

Start state:
((ACTIVE GATE1) (ACTIVE GATE2) (COLOR TRANSMITTER1 BLUE) (COLOR TRANSMITTER2 RED) (COLOR RECEIVER1 BLUE) (COLOR RECEIVER2 RED)
 (LOC ME1 AREA5) (LOC CONNECTOR1 AREA5) (LOC CONNECTOR2 AREA7))

Goal:
(LOC ME1 AREA8)

Total solution paths recorded = 1, of which 1 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in a minimum path length solution = 19

A minimum length solution path from start state to goal state:
(1.0 (PICKUP-CONNECTOR CONNECTOR1 AREA5))
(4.0 (CONNECT-TO-2-TERMINUS CONNECTOR1 RECEIVER1 TRANSMITTER1 AREA5 BLUE))
(5.0 (MOVE AREA5 AREA4))
(6.0 (MOVE AREA4 AREA7))
(7.0 (PICKUP-CONNECTOR CONNECTOR2 AREA7))
(8.0 (MOVE AREA7 AREA6))
(10.0 (CONNECT-TO-1-TERMINUS CONNECTOR2 TRANSMITTER1 AREA6 BLUE))
(11.0 (MOVE AREA6 AREA7))
(12.0 (MOVE AREA7 AREA4))
(13.0 (MOVE AREA4 AREA5))
(14.0 (PICKUP-CONNECTOR CONNECTOR1 AREA5))
(18.0 (CONNECT-TO-3-TERMINUS CONNECTOR1 CONNECTOR2 RECEIVER2 RECEIVER1 AREA5 BLUE))
(19.0 (MOVE AREA5 AREA4))
(20.0 (MOVE AREA4 AREA7))
(21.0 (MOVE AREA7 AREA6))
(22.0 (PICKUP-CONNECTOR CONNECTOR2 AREA6))
(25.0 (CONNECT-TO-2-TERMINUS CONNECTOR2 CONNECTOR1 TRANSMITTER2 AREA6 RED))
(26.0 (MOVE AREA6 AREA7))
(27.0 (MOVE AREA7 AREA8))

Final state:
((ACTIVE GATE1) (ACTIVE CONNECTOR2) (ACTIVE CONNECTOR1) (ACTIVE RECEIVER2) (COLOR TRANSMITTER1 BLUE) (COLOR TRANSMITTER2 RED)
 (COLOR RECEIVER1 BLUE) (COLOR RECEIVER2 RED) (COLOR CONNECTOR2 RED) (COLOR CONNECTOR1 RED) (CONNECTS RECEIVER2 CONNECTOR1)
 (CONNECTS CONNECTOR1 RECEIVER2) (CONNECTS RECEIVER1 CONNECTOR1) (CONNECTS CONNECTOR1 RECEIVER1) (CONNECTS CONNECTOR1 CONNECTOR2)
 (CONNECTS CONNECTOR2 CONNECTOR1) (CONNECTS TRANSMITTER2 CONNECTOR2) (CONNECTS CONNECTOR2 TRANSMITTER2) (LOC ME1 AREA8)
 (LOC CONNECTOR1 AREA5) (LOC CONNECTOR2 AREA6))

Evaluation took:
 1.120 seconds of real time
 0.515625 seconds of total run time (0.500000 user, 0.015625 system)
 [ Real times consist of 0.112 seconds GC time, and 1.008 seconds non-GC time. ]
 [ Run times consist of 0.046 seconds GC time, and 0.470 seconds non-GC time. ]
 46.07% CPU
 3,570,337,217 processor cycles
 1,197,836,560 bytes consed
```

</details>

## Capt John’s Journey Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Capt John’s Journey Problem (source extract)</strong></summary>

The following is a logic problem from braingle.com called Captain John’s Journey (Part 1), submitted by cdrock. It illustrates how to solve a Constraint Satisfaction Problem (CSP) with the Wouldwork planner, since a CSP is not normally regarded as a planning problem. The basic approach is to write a single action specification that progressively generates values for each constrained variable, and then checks those values against a goal. The problem is solved when the set of variable values satisfies the goal.

Captain John is the captain of a pirate ship called the Wasp. He just heard about a lost treasure on a far-away island. He needs to get his two crew mates, and lead them to a ship, but there are guards around and he needs to do this without passing them, or they will throw him in the brig. Can you help him get his two crew mates to the ship without getting sent to the brig?

The positions of everything are in a 3-by-3 grid (1 John, 1 ship, 2 crew mates, 2 guards, and 3 grass areas). John may only move 1 space at a time, either vertically, horizontally, or diagonally. He can only go to each space once.

But before he can figure out the right way to go, he must figure out where everything is to start with. This is what he knew:

1. The Wasp is not in the same row or column as John.
2. John is not in the same row or column as either guard.
3. Neither guard is in the third column.
4. Both guards are vertically next to grass.
5. The ship is in the same row as one guard, and the same column as the other guard.
6. One of the grass spaces is diagonally next to both crew mates.
7. One of the grass spaces is in the 2nd column, in the first row.
8. The two guards are not in the same row or column.
These statements are enough for Capt John to figure out where all nine tokens are initially located on the 3-by-3 grid (part 1).

```lisp
Capt John Problem Specification

;;; Filename: problem-captjohn-csp.lisp

;;; Brain Teaser logic problem,
;;; Capt John's Journey (part 1)

(in-package :ww)

(ww-set *problem-name* captjohn)

(ww-set *problem-type* csp)

(define-types
 captain (john)
 ship (wasp)
 crew (crew1 crew2)
 guard (guard1 guard2)
 grass (grass1 grass2 grass3)
 object (either captain ship crew guard grass))

(define-dynamic-relations
 (remaining object $list)) ;list of coords--eg, ((0 2) ...)

(define-query get-remaining (?obj)
 (do (bind (remaining ?obj $coords))
 $coords))

(define-query in-same-row (?coord1 ?coord2)
 (= (first ?coord1) (first ?coord2)))

(define-query in-same-col (?coord1 ?coord2)
 (= (second ?coord1) (second ?coord2)))

(define-query in-row (?coord ?row)
 (= (first ?coord) ?row))

(define-query in-col (?coord ?col)
 (= (second ?coord) ?col))

(define-query vert-next-to (?coord1 ?coord2)
 (and (= (second ?coord1) (second ?coord2))
 (or (= (first ?coord1) (1+ (first ?coord2)))
 (= (first ?coord1) (1- (first ?coord2))))))

(define-query diag-next-to (?coord1 ?coord2)
 (or (and (= (1+ (first ?coord1)) (first ?coord2))
 (= (1- (second ?coord1)) (second ?coord2)))
 (and (= (1+ (first ?coord1)) (first ?coord2))
 (= (1+ (second ?coord1)) (second ?coord2)))
 (and (= (1- (first ?coord1)) (first ?coord2))
 (= (1- (second ?coord1)) (second ?coord2)))
 (and (= (1- (first ?coord1)) (first ?coord2))
 (= (1+ (second ?coord1)) (second ?coord2)))))

(define-update make-assignment (?assigned-obj ?assigned-coord)
 (doall (?obj object)
 (do (bind (remaining ?obj $initial-coords))
 (if (eql ?obj ?assigned-obj)
 (remaining ?obj (list ?assigned-coord))
 (remaining ?obj (remove ?assigned-coord $initial-coords :test #'equal))))))

; Make one rule for each object assignment with constraints
; Later actions can depend on prior assignments

(define-action assign-grass2
 ; One of the grass spaces is in the 2nd column, in the first row.
 1
 (?grass2-coord (get-remaining grass2))
 (equal ?grass2-coord '(0 1))
 (?grass2-coord)
 (assert (make-assignment grass2 ?grass2-coord)))

(define-action assign-guard1
 ; Neither guard is in the third column.
 1
 (?guard1-coord (get-remaining guard1))
 (not (in-col ?guard1-coord 2))
 (?guard1-coord)
 (assert (make-assignment guard1 ?guard1-coord)))

(define-action assign-guard2
 ; Neither guard is in the third column.
 ; The two guards are not in the same row or column.
 1
 (?guard2-coord (get-remaining guard2) ?guard1-coord (get-remaining guard1)) ;?guard1-coord previously assigned
 (and (not (in-col ?guard2-coord 2))
 (not (in-same-row ?guard2-coord ?guard1-coord))
 (not (in-same-col ?guard2-coord ?guard1-coord)))
 (?guard2-coord)
 (assert (make-assignment guard2 ?guard2-coord)))

(define-action assign-john
 ; John is not in the same row or column as either guard.
 1
 (?john-coord (get-remaining john) ?guard1-coord (get-remaining guard1)
 ?guard2-coord (get-remaining guard2))
 (and (not (in-same-row ?john-coord ?guard1-coord))
 (not (in-same-col ?john-coord ?guard1-coord))
 (not (in-same-row ?john-coord ?guard2-coord))
 (not (in-same-col ?john-coord ?guard2-coord)))
 (?john-coord)
 (assert (make-assignment john ?john-coord)))

(define-action assign-wasp
 ; The Wasp is not in the same row or column as John.
 ; The ship is in the same row as one guard, and the same column as the other guard.
 1
 (?wasp-coord (get-remaining wasp) ?john-coord (get-remaining john)
 ?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2))
 (and (not (in-same-row ?wasp-coord ?john-coord))
 (not (in-same-col ?wasp-coord ?john-coord))
 (or (and (in-same-row ?wasp-coord ?guard1-coord)
 (in-same-col ?wasp-coord ?guard2-coord))
 (and (in-same-row ?wasp-coord ?guard2-coord)
 (in-same-col ?wasp-coord ?guard1-coord))))
 (?wasp-coord)
 (assert (make-assignment wasp ?wasp-coord)))

(define-action assign-grass1
 ; Both guards are vertically next to grass.
 1
 (?grass1-coord (get-remaining grass1) ?guard1-coord (get-remaining guard1)
 ?guard2-coord (get-remaining guard2) ?grass2-coord (get-remaining grass2))
 (and (or (vert-next-to ?guard1-coord ?grass1-coord)
 (vert-next-to ?guard1-coord ?grass2-coord)
 (ww-loop for ?possible-grass3-coord in (get-remaining grass3)
 thereis (vert-next-to ?guard1-coord ?possible-grass3-coord)))
 (or (vert-next-to ?guard2-coord ?grass1-coord)
 (vert-next-to ?guard2-coord ?grass2-coord)
 (ww-loop for ?possible-grass3-coord in (get-remaining grass3)
 thereis (vert-next-to ?guard2-coord ?possible-grass3-coord))))
 (?grass1-coord)
 (assert (make-assignment grass1 ?grass1-coord)))

(define-action assign-grass3
 ; Both guards are vertically next to grass.
 1
 (?grass3-coord (get-remaining grass3) ?grass1-coord (get-remaining grass1)
 ?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2)
 ?grass2-coord (get-remaining grass2))
 (and (or (vert-next-to ?guard1-coord ?grass1-coord)
 (vert-next-to ?guard1-coord ?grass2-coord)
 (vert-next-to ?guard1-coord ?grass3-coord))
 (or (vert-next-to ?guard2-coord ?grass1-coord)
 (vert-next-to ?guard2-coord ?grass2-coord)
 (vert-next-to ?guard2-coord ?grass3-coord)))
 (?grass3-coord)
 (assert (make-assignment grass3 ?grass3-coord)))

(define-action assign-crew1
 ; One of the grass spaces is diagonally next to both crew mates.
 1
 (?crew1-coord (get-remaining crew1) ?grass1-coord (get-remaining grass1)
 ?grass2-coord (get-remaining grass2) ?grass3-coord (get-remaining grass3))
 (and (or (ww-loop for ?possible-crew2-coord in (get-remaining crew2)
 thereis (and (diag-next-to ?grass1-coord ?crew1-coord)
 (diag-next-to ?grass1-coord ?possible-crew2-coord)))
 (ww-loop for ?possible-crew2-coord in (get-remaining crew2)
 thereis (and (diag-next-to ?grass2-coord ?crew1-coord)
 (diag-next-to ?grass2-coord ?possible-crew2-coord)))
 (ww-loop for ?possible-crew2-coord in (get-remaining crew2)
 thereis (and (diag-next-to ?grass3-coord ?crew1-coord)
 (diag-next-to ?grass3-coord ?possible-crew2-coord)))))
 (?crew1-coord)
 (assert (make-assignment crew1 ?crew1-coord)))

(define-action assign-crew2
 ; One of the grass spaces is diagonally next to both crew mates.
 1
 (?crew2-coord (get-remaining crew2) ?crew1-coord (get-remaining crew1)
 ?grass1-coord (get-remaining grass1) ?grass2-coord (get-remaining grass2)
 ?grass3-coord (get-remaining grass3))
 (or (and (diag-next-to ?grass1-coord ?crew1-coord)
 (diag-next-to ?grass1-coord ?crew2-coord))
 (and (diag-next-to ?grass2-coord ?crew1-coord)
 (diag-next-to ?grass2-coord ?crew2-coord))
 (and (diag-next-to ?grass3-coord ?crew1-coord)
 (diag-next-to ?grass3-coord ?crew2-coord)))
 (?crew2-coord)
 (assert (make-assignment crew2 ?crew2-coord)))

(define-init
 (remaining john ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining wasp ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining crew1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining crew2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining guard1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining guard2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining grass1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining grass2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (remaining grass3 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

(define-goal ;when the last variable has been assigned
 (eq (problem-state.name state) (action.name (car (last *actions*)))))

Capt John Problem Solution

working...

New path to goal found at depth = 9

In problem CAPTJOHN, performed GRAPH search for FIRST solution.

Search ended with first solution found.

Depth cutoff = 0

Maximum depth explored = 9

Program cycles = 12

Total states processed = 21

Repeated states = 0, ie, 0.0 percent

Average branching factor = 1.6

Start state:
((REMAINING JOHN ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING WASP ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING CREW1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING CREW2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING GUARD1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING GUARD2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING GRASS1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING GRASS2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
 (REMAINING GRASS3 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

Goal:
(EQ (PROBLEM-STATE.NAME STATE) (ACTION.NAME (CAR (LAST *ACTIONS*))))

Total solution paths recorded = 1, of which 1 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in first solution found: = 9

Duration of first solution found = 9.0

Solution path of first solution found from start state to goal state:
(1.0 (ASSIGN-GRASS2 (0 1)))
(2.0 (ASSIGN-GUARD1 (0 0)))
(3.0 (ASSIGN-GUARD2 (2 1)))
(4.0 (ASSIGN-JOHN (1 2)))
(5.0 (ASSIGN-WASP (2 0)))
(6.0 (ASSIGN-GRASS1 (1 0)))
(7.0 (ASSIGN-GRASS3 (1 1)))
(8.0 (ASSIGN-CREW1 (0 2)))
(9.0 (ASSIGN-CREW2 (2 2)))

Final state:
((REMAINING JOHN ((1 2))) (REMAINING WASP ((2 0))) (REMAINING CREW1 ((0 2))) (REMAINING CREW2 ((2 2))) (REMAINING GUARD1 ((0 0)))
 (REMAINING GUARD2 ((2 1))) (REMAINING GRASS1 ((1 0))) (REMAINING GRASS2 ((0 1))) (REMAINING GRASS3 ((1 1))))

Evaluation took:
 0.004 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 14,249,965 processor cycles
 6,143,120 bytes consed
```

</details>

## Triangle Peg Puzzle

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Triangle Peg Puzzle (source extract)</strong></summary>

The Triangle Peg Puzzle (aka Cracker Barrel Puzzle, Conqueror Puzzle) consists of a triangular peg board with 15 holes and 14 pegs, initially filling all holes except one. The objective is to jump over a peg on each move, as in checkers, continuing to jump with different pegs on each move, until there is only one peg left. Any solution therefore will require 13 jumps total.

This puzzle illustrates how Common Lisp code can be added to a problem specification, in this case to setup the initial board configuration, when using a standard planning initialization action would be awkward and inefficient.

```lisp
Triangle Peg Problem Specification

;;; Filename: problem-triangle-xy.lisp

;;; Basic problem specification for triangle peg problem with peg-count.

;;; The peg board positions have coordinates measured
;;; from the triangle's right diagonal (/) and left diagonal (\)
;;; side lengths from 1 to *N* with pegs in all positions except 11.
;;; Jumps are by / up or down, \ up or down, - (horiz) right or left.
;;; 11
;;; 12 21
;;; 13 22 31
;;; 14 23 32 41
;;; 15 24 33 42 51

(in-package :ww) ;required

(ww-set *problem-name* triangle-xy)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)

(defparameter *N* 5) ;the number of pegs on a side

(defparameter *holes* '((1 1))) ;coordinates of the initial holes

(define-types
 peg (compute (loop for i from 1 ;peg1, peg2, ...
 to (- (/ (* *N* (1+ *N*)) 2) (length *holes*))
 collect (intern (format nil "PEG~D" i))))
 row (compute (loop for i from 1 to *N*
 collect i))
 col (compute (loop for j from 1 to *N*
 collect j)))

(define-dynamic-relations
 (loc peg $fixnum $fixnum) ;location of a peg
 (contents row col $peg) ;peg contents at a location
 (remaining-pegs $list) ;list of remaining pegs
 (peg-count $integer)) ;pegs remaining on the board

(define-query get-remaining-pegs? ()
 (do (bind (remaining-pegs $pegs))
 $pegs))

(define-action jump-left-down ;jump downward in the / diagonal direction
 1
 (?peg (get-remaining-pegs?)) ;function type
 (and (bind (loc ?peg $r $c))
 (<= (+ $r $c) (- *N* 1))
 (setq $c+1 (1+ $c))
 (bind (contents $r $c+1 $adj-peg))
 (setq $c+2 (+ $c 2))
 (not (bind (contents $r $c+2 $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg)) ;from
 (loc ?peg $r $c+2) ;to
 (contents $r $c+2 ?peg) ;update to
 (not (loc $adj-peg $r $c+1)) ;remove adj peg
 (not (contents $r $c+1 $adj-peg)) ;remove adj peg
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(define-action jump-right-up ;jump upward in the / diagonal direction
 1
 (?peg (get-remaining-pegs?)) ;named type
 (and (bind (loc ?peg $r $c))
 (>= $c 3)
 (setq $c-1 (1- $c))
 (bind (contents $r $c-1 $adj-peg))
 (setq $c-2 (- $c 2))
 (not (bind (contents $r $c-2 $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg))
 (loc ?peg $r $c-2)
 (contents $r $c-2 ?peg)
 (not (loc $adj-peg $r $c-1))
 (not (contents $r $c-1 $adj-peg))
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(define-action jump-right-down ;jump downward in the \ diagonal direction
 1
 (?peg (get-remaining-pegs?))
 (and (bind (loc ?peg $r $c))
 (<= (+ $r $c) (- *N* 1))
 (setq $r+1 (+ $r 1))
 (bind (contents $r+1 $c $adj-peg))
 (setq $r+2 (+ $r 2))
 (not (bind (contents $r+2 $c $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg))
 (loc ?peg $r+2 $c)
 (contents $r+2 $c ?peg)
 (not (loc $adj-peg $r+1 $c))
 (not (contents $r+1 $c $adj-peg))
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(define-action jump-left-up ;jump upward in the \ diagonal direction
 1
 (?peg (get-remaining-pegs?))
 (and (bind (loc ?peg $r $c))
 (>= $r 3)
 (setq $r-1 (- $r 1))
 (bind (contents $r-1 $c $adj-peg))
 (setq $r-2 (- $r 2))
 (not (bind (contents $r-2 $c $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg))
 (loc ?peg $r-2 $c)
 (contents $r-2 $c ?peg)
 (not (loc $adj-peg $r-1 $c))
 (not (contents $r-1 $c $adj-peg))
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(define-action jump-right-horiz ;jump rightward in the horizontal direction
 1
 (?peg (get-remaining-pegs?))
 (and (bind (loc ?peg $r $c))
 (>= $c 3)
 (setq $r+1 (+ $r 1))
 (setq $c-1 (- $c 1))
 (bind (contents $r+1 $c-1 $adj-peg))
 (setq $r+2 (+ $r 2))
 (setq $c-2 (- $c 2))
 (not (bind (contents $r+2 $c-2 $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg))
 (loc ?peg $r+2 $c-2)
 (contents $r+2 $c-2 ?peg)
 (not (loc $adj-peg $r+1 $c-1))
 (not (contents $r+1 $c-1 $adj-peg))
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(define-action jump-left-horiz ;jump leftward in the horizontal direction
 1
 (?peg (get-remaining-pegs?))
 (and (bind (loc ?peg $r $c))
 (>= $r 3)
 (setq $r-1 (- $r 1))
 (setq $c+1 (+ $c 1))
 (bind (contents $r-1 $c+1 $adj-peg))
 (setq $r-2 (- $r 2))
 (setq $c+2 (+ $c 2))
 (not (bind (contents $r-2 $c+2 $any-peg)))
 (bind (peg-count $peg-count))
 (bind (remaining-pegs $pegs)))
 ($r $c)
 (assert (not (contents $r $c ?peg))
 (loc ?peg $r-2 $c+2)
 (contents $r-2 $c+2 ?peg)
 (not (loc $adj-peg $r-1 $c+1))
 (not (contents $r-1 $c+1 $adj-peg))
 (peg-count (1- $peg-count))
 (remaining-pegs (remove $adj-peg $pegs))))

(progn (format t "~&Initializing database...~%")
 (loop with pegs = (gethash 'peg *types*)
 ;*db* is the name of the initial database
 ;update is the function that asserts a proposition
 ;into the database
 initially (update *db* `(peg-count ,(length pegs)))
 (update *db* `(remaining-pegs ,pegs))
 for row from 1 to *N*
 do (loop for col from 1 to (- (1+ *N*) row)
 unless (member (list row col) *holes* :test #'equal)
 do (let ((peg (pop pegs)))
 (update *db* `(loc ,peg ,row ,col))
 (update *db* `(contents ,row ,col ,peg))))))

(define-goal ;only one peg left
 (peg-count 1))

Triangle Peg Problem Solution:

working...

New path to goal found at depth = 13

In problem TRIANGLE-XY, performed TREE search for FIRST solution.

Search ended with first solution found.

Depth cutoff = 0

Maximum depth explored = 13

Program cycles = 115

Total states processed = 137

Average branching factor = 1.2

Start state:
((CONTENTS 1 2 PEG1) (CONTENTS 1 3 PEG2) (CONTENTS 1 4 PEG3) (CONTENTS 1 5 PEG4) (CONTENTS 2 1 PEG5) (CONTENTS 2 2 PEG6)
 (CONTENTS 2 3 PEG7) (CONTENTS 2 4 PEG8) (CONTENTS 3 1 PEG9) (CONTENTS 3 2 PEG10) (CONTENTS 3 3 PEG11) (CONTENTS 4 1 PEG12)
 (CONTENTS 4 2 PEG13) (CONTENTS 5 1 PEG14) (LOC PEG1 1 2) (LOC PEG2 1 3) (LOC PEG3 1 4) (LOC PEG4 1 5) (LOC PEG5 2 1) (LOC PEG6 2 2)
 (LOC PEG7 2 3) (LOC PEG8 2 4) (LOC PEG9 3 1) (LOC PEG10 3 2) (LOC PEG11 3 3) (LOC PEG12 4 1) (LOC PEG13 4 2) (LOC PEG14 5 1)
 (PEG-COUNT 14) (REMAINING-PEGS (PEG1 PEG2 PEG3 PEG4 PEG5 PEG6 PEG7 PEG8 PEG9 PEG10 PEG11 PEG12 PEG13 PEG14)))

Goal:
(PEG-COUNT 1)

Total solution paths recorded = 1, of which 1 is/are unique solution paths
Check *solutions* and *unique-solutions* for solution records.

Number of steps in first solution found: = 13

Duration of first solution found = 13.0

Solution path of first solution found from start state to goal state:
(1.0 (JUMP-RIGHT-UP 1 3))
(2.0 (JUMP-RIGHT-UP 1 5))
(3.0 (JUMP-LEFT-UP 3 2))
(4.0 (JUMP-LEFT-DOWN 1 2))
(5.0 (JUMP-RIGHT-UP 2 4))
(6.0 (JUMP-LEFT-DOWN 2 1))
(7.0 (JUMP-LEFT-UP 4 1))
(8.0 (JUMP-RIGHT-DOWN 1 1))
(9.0 (JUMP-RIGHT-HORIZ 1 4))
(10.0 (JUMP-LEFT-HORIZ 4 2))
(11.0 (JUMP-LEFT-DOWN 3 1))
(12.0 (JUMP-RIGHT-HORIZ 2 4))
(13.0 (JUMP-LEFT-HORIZ 5 1))

Final state:
((CONTENTS 3 3 PEG14) (LOC PEG14 3 3) (PEG-COUNT 1) (REMAINING-PEGS (PEG14)))

Evaluation took:
 0.005 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 16,456,360 processor cycles
 1,701,824 bytes consed
```

</details>

## Knapsack Problem

**What it teaches:**
- How to express the domain in objects/relations/actions
- How to interpret a returned plan

**Try this variation (after it works):**
- Change the goal slightly (or add an extra constraint) and compare the plan length/search time.

<details>
<summary><strong>Knapsack Problem (source extract)</strong></summary>

Given a knapsack which can carry a maximum weight W, and a number of items with weights wi and values vi, how can you pack the knapsack so as to fit in items with the maximum total value V, without exceeding the knapsack's capacity W?

This is an optimization problem, in this case find a goal (ie, a knapsack filling) that has the maximum total value. You must specify what is to be optimized, namely the value in the knapsack ($objective-value), computed in the put rule below.

The query function get-best-relaxed-value? computes a maximum possible value to fill the remaining capacity in the knapsack from any given state, allowing a fractional object as the last object (with its fractional weight and fractional value). Since the items have been initially ordered by their value/weight ratios, the last fractional value added is the best total value that can be achieved in that state. This relaxed value is used by the planner to prune any subsequent states whose value is worse than the best value so far.

```lisp
Knapsack Problem Specification

;;;; Filename: problem-knap19.lisp

;;; Problem specification for a 19-item knapsack problem.

(in-package :ww) ;required

(ww-set *problem-name* knap19)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* graph)

(ww-set *solution-type* max-value)

(defvar item nil) ;prevent compiler warnings
(defvar create-item-structures nil)

#.(defstruct item ;an item template
 (name nil :type symbol) ;item name--eg, item3
 (id -1 :type fixnum) ;item# in sorted list
 (value -1 :type fixnum) ;value of that item
 (weight -1 :type fixnum) ;weight of that item
 (value/weight -1 :type double-float)) ;the ratio of the item's value/weight

#.(defparameter *item-structures* nil) ;the list of data item structures
#.(defparameter *num-items* -1) ;total number of items, update from data-knap19.lisp
#.(defparameter *max-weight* -1) ;max weight allowed, update from data-knap19.lisp

#.(defun create-item-structures (data-file)
 ;Read in data from a file.
 (with-open-file (infile data-file :direction :input :if-does-not-exist nil)
 (when (not (streamp infile)) (error "File does not exist!"))
 (setf *num-items* (read infile))
 (setf *max-weight* (read infile))
 (loop for i from 1 upto *num-items* do
 (let ((value (read infile))
 (weight (read infile)))
 (push (make-item :name (intern (format nil "ITEM~D" i) :ww)
 :value value
 :weight weight
 :value/weight (coerce (/ value weight)
 'double-float))
 *item-structures*)))))

#.(create-item-structures (in-src "data-knap19.lisp")) ;path to the data file in the src directory

(defparameter *sorted-item-structures*
 (loop with sorted-item-structures = (sort (copy-list *item-structures*) #'>
 :key #'item-value/weight)
 for item-structure in sorted-item-structures
 for index from 1
 do (setf (item-id item-structure) index)
 collect item-structure))

(define-types
 item-id (compute (mapcar #'item-id *sorted-item-structures*))) ;item IDs

(define-dynamic-relations
 (in item-id) ;an item-id in the knapsack
 (contents $list) ;the item-ids in the knapsack
 (load $fixnum) ;the net weight of the knapsack
 (worth $fixnum)) ;the net worth of item-ids in the knapsack

(define-static-relations
 (capacity $fixnum) ;weight capacity of the knapsack
 (value item-id $fixnum) ;value of an item-id
 (weight item-id $fixnum)) ;weight of an item-id

(define-query compute-bounds? ($item-ids)
 ;Computes cost and upper bounds for a state
 (do (bind (capacity $capacity)) ;(ut::prt state)
 (setf $max-item-id (or (car (last $item-ids)) 0))
 (if (= (length $item-ids) $max-item-id)
 (setf $missing-item-ids nil)
 (do (setf $initial-item-ids (cdr (alexandria:iota (1+ $max-item-id))))
 (setf $missing-item-ids (set-difference $initial-item-ids $item-ids)))) ;(ut::prt $all-item-ids $missing-item-ids)
 (setf $all-item-ids (gethash 'item-id *types*))
 (setf $wt 0 $cost 0 $upper 0)
 (ww-loop for $item-id in $all-item-ids do ;run thru all item-ids until capacity exceeded
 (if (and (not (member $item-id $missing-item-ids)) ;except those missing
 (bind (weight $item-id $item-weight))
 (bind (value $item-id $item-value)) ;(ut::prt $item-id $item-weight $item-value)
 (if (<= (+ $wt $item-weight) $capacity)
 (do (incf $wt $item-weight) ;(ut::prt $wt)
 (incf $cost $item-value)
 (incf $upper $item-value))
 (do (setq $fraction (/ (- $capacity $wt) $item-weight))
 (incf $cost (* $fraction $item-value)) ;(ut::prt $fraction (- $cost) (- $upper)) (break)
 (return-from compute-bounds? (values (- $cost) (- $upper)))))))) ;(ut::prt (- $cost) (- $upper)) (break)
 (return-from compute-bounds? (values (- $cost) (- $upper)))))

(defun successors-p (lst)
 "Tests for a succession of integers (ie, item-ids)."
 (iter (for item-id in lst)
 (for prev-item-id previous item-id)
 (when prev-item-id
 (always (= item-id (1+ prev-item-id))))))

(define-query bounding-function? ()
 (do (bind (contents $item-ids))
 (if (successors-p $item-ids)
 (if (= *cost* *upper* 0)
 (do (multiple-value-setq (*cost* *upper*)
 (compute-bounds? $item-ids))
 (values *cost* *upper*))
 (values *cost* *upper*))
 (do (setf *cost* 0 *upper* 0)
 (compute-bounds? $item-ids)))))

(define-action put
 1
 (?item-id item-id)
 (and (not (in ?item-id))
 (bind (weight ?item-id $item-weight))
 (bind (load $load))
 (setq $new-load (+ $load $item-weight))
 (bind (capacity $capacity))
 (<= $new-load $capacity))
 (?item-id)
 (assert (in ?item-id)
 (bind (contents $item-ids))
 (setq $new-item-ids
 (merge 'list (list ?item-id) (copy-list $item-ids) #'<))
 (contents $new-item-ids)
 (load $new-load)
 (bind (worth $worth))
 (bind (value ?item-id $item-value))
 (setq $new-worth (+ $worth $item-value))
 (worth $new-worth)
 (setq $objective-value $new-worth)))

(define-init-action init-item-weights&values
 0
 (?item-id item-id)
 (always-true)
 ()
 (assert ;(ut::prt item-structure)
 (weight ?item-id (item-weight (find ?item-id *sorted-item-structures*
 :key #'item-id)))
 (value ?item-id (item-value (find ?item-id *sorted-item-structures*
 :key #'item-id)))))

(define-init
 (capacity #.*max-weight*)
 (contents nil)
 (load 0)
 (worth 0))

Knapsack Problem Solution:

working...

Higher value state found: 321 in thread NIL
Higher value state found: 667 in thread NIL
Higher value state found: 689 in thread NIL
Higher value state found: 805 in thread NIL
Higher value state found: 1060 in thread NIL
Higher value state found: 1101 in thread NIL
Higher value state found: 1107 in thread NIL
Higher value state found: 1833 in thread NIL
Higher value state found: 1865 in thread NIL
Higher value state found: 1945 in thread NIL
Higher value state found: 2890 in thread NIL
Higher value state found: 2945 in thread NIL
Higher value state found: 12248 in thread NIL

In problem KNAP19, performed GRAPH search for MAX-VALUE solution.

DEPTH-FIRST search process completed normally.

Depth cutoff = 0

Maximum depth explored = 5

Program cycles = 432

Total states processed = 456

Repeated states = 24, ie, 5.3 percent

Average branching factor = 1.1

Start state:
((CONTENTS NIL) (LOAD 0) (WORTH 0))

Goal:
NIL

No goal specified, but best results follow:

Total number of results recorded = 50.
Check *best-states* for all result records.

The maximum objective value found = 12,248

A maximum value state:
<PUT (11) NIL 5.0 12248 0.0
 ((CONTENTS (3 5 6 9 11)) (IN 3) (IN 5) (IN 6) (IN 9) (IN 11) (LOAD 30996) (WORTH 12248))
 NIL>

Evaluation took:
 0.027 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 88,545,217 processor cycles
 8,795,216 bytes consed
```

</details>
