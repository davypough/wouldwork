# Reference

This is the **dense lookup** file: settings, outputs, and conceptual categories.

If you’re new: read `TUTORIAL.md` first, then come back here when you need exact controls.

<details>
<summary><strong>Solution Types</strong></summary>

Wouldwork can adjust its search strategy depending on what kind of solution the user wants. The choice is specified with the solution-type parameter—eg, (ww-set *solution-type* first). All Wouldwork settings conform to the Common Lisp “earmuff” notation with asterisk pre- & post-fixes. The options are first (find the first encountered solution that satisfies the goal), min-length (find the solution with the minimum number of steps/actions from start to finish), min-time (find the solution taking the least amount of time to complete, given the time to complete each action), min-value (find the solution that minimizes an objective value function), max-value (find the solution that maximizes an objective value function), and every (find and record every solution). The default is first.

Note that for every, the number of solutions found may differ, depending on whether a tree search or graph search is performed. Tree search aims to find all paths to legitimate goals without pruning (but without retracing steps), which may take a long time. Standard graph search will prune states that have been previously visited, and therefore will not find alternative paths to goals through those pruned states. However, when graph search for every solution is combined with a positive depth-cutoff, Wouldwork automatically activates a hybrid search mode that finds all solution paths at or below the cutoff depth. This hybrid mode maintains the memory efficiency of graph search while enumerating every distinct action sequence leading to a goal, including paths that differ only in the ordering of actions. The hybrid mode is recommended for problems where exhaustive solution enumeration is required but tree search would be prohibitively expensive due to state space explosion.

Each of min-length, min-time, min-value, and max-value also employ pruning to avoid exploring infeasible paths. During the initial phase of debugging a plan specification however, it may be useful to leave the solution type at its default value of first, in order to verify quickly that some/any plan is achievable. Then gradually increase *depth-cutoff* to see if there is any solution within a reasonable depth bound, within a reasonable amount of time.

Note that it is possible to find a min-value or max-value state even if there is no goal specified (ie, the goal is set to nil). For example, see the simple knapsack problem in problem-knap4a.lisp. In this case the search will return the min- or max-value of all states examined. If a goal is specified, then the min- or max-value satisfying the goal will be returned.

Wouldwork stores in the variable *solutions* all solution paths to all goals reached. But note that many paths may lead to the same goal state. The variable *unique-solutions* will contain the paths to each unique goal state reached. After a planning search completes, enter either variable name at the SBCL prompt to see its contents.

</details>

<details>
<summary><strong>Problem Types</strong></summary>

There are two basic types of problem that Wouldwork can be configured to solve—namely, planning problems and constraint satisfaction problems. The user must tell Wouldwork which type (*problem-type* = planning or csp) to use to solve the current problem specification, since each type expects a slightly different organization of actions. Planning is the default, where the actions represent possible changes to the current problem state. But CSP actions represent the sequential assignments of values to variables. (See problem-captjohn.lisp for an example of a CSP problem.) CSP problems can be solved as planning problems, but the CSP specializations will greatly increase efficiency for large CSP problems. In particular, CSP problems are usually paired with a backtracking algorithm for maximal efficiency.

</details>

<details>
<summary><strong>Algorithm Types</strong></summary>

The user can also choose one of two algorithms to use for solving a problem (*algorithm* = depth-first or backtracking). Normally, depth-first is best for problems with repeated states whose solution involves searching a graph. Depth-first avoids redundantly analyzing such repeated states or paths. Alternately, backtracking is best suited for tree search where no or very few states repeat, and no circular paths exist in the state space. Most planning problems are best solved with depth-first + graph, while CSP problems are ideal with the backtracking + tree strategy.

</details>

<details>
<summary><strong>Program Control Settings</strong></summary>

There are a number of other program settings, like *solution-type* mentioned in the prior paragraphs, that the user has control over. All of these settings have default values, but the user can change them as needed. Simply add a statement like (ww-set <setting> <value>) to the problem specification file. For example, (ww-set *solution-type* min-length). Note that the parameters can also be changed at the REPL. The setting names and their initial default values (in the package :ww) are as follows:

```lisp
*problem-name* = unspecified (any symbol, string, or number)
```
Specifies the name of the problem to be solved. The user will normally include a statement like (ww-set *problem* problem-name) in a problem specification file to identify which problem the specification is about. The statement however is optional.

```lisp
*problem-type* = planning (planning or csp)
```
Specifies whether the problem is a planning problem or a constraint satisfaction problem (csp). Wouldwork uses different search strategies for each.

```lisp
*algorithm* = depth-first (depth-first or backtracking)
```
Specifies whether the problem is to be solved using a depth-first or backtracking strategy.

```lisp
*depth-cutoff* = 0 (n >= 0)
```
Specifies the max search depth; 0 means no cutoff. Searching deeper usually requires more time. For a tree search, if no cutoff is specified, the search along each path will continue until a state repeats, indicating a cyclical (infinitely repeating) path, thereby terminating the path. Searching with a fixed *depth-cutoff* > 0 may be somewhat faster, since then there is no need to check for cyclical paths. Cyclical paths are not an issue for graph search, since any repeated state (on the current path or not) is automatically pruned.

```lisp
*tree-or-graph* = graph (tree or graph)
```
Specifies whether the search space is expected to be a tree (with no repeated states) or a graph (with repeated states, such that the same state can be reached in more than one way). A paradigmatic example of a problem with a graph search space is the 15-puzzle, since there are many ways to arrive at the same tile configuration by different paths. The 8-queens puzzle, however, exemplifies a problem with a tree search space, if the queens are placed on successive rows one after the other. A previous board configuration of non-attacking queens will never be repeated, because there will always be more queens on the board with each new placement. But neither search strategy will get caught in an infinite loop. Tree search may find more solutions than graph search, since different paths of different lengths to the same goal state are allowed in tree search. Graph search is the default, but if there are few repeated states, tree search will likely be faster. Try both if unsure.

```lisp
*solution-type* = first (first, min-length, min-time, min-value, max-value, every)
```
Specifies what kind of solution is required from the planner: first means stop after the first solution is found; every means find all solutions by examining the entire search space, where the search space is determined by the *tree-or-graph* setting; min-length means find the path containing the least number of steps (ie, actions); min-time means find the solution taking the least amount of time. And min-value or max-value finds the solution with the minimum or maximum value according to a user-specified optimization value.

```lisp
*progress-reporting-interval* = 200000 (n > 0)
```
Specifies how often to report progress to the terminal during search; reports after each multiple n of states examined; useful for long searches.

```lisp
*debug* 0 (0 <= n <= 5)
```
This global variable determines how much debugging information is printed to the terminal during or following a search. The default, 0, means no debugging. A value of 1 prints out the search tree after the search is complete, while higher values print out progressively more information. Setting the value to 5 will interrupt the search for inspection after each processing cycle. This type of debugging is only available in serial (not parallel) search, so first verify that *threads* = 0. See the file ww-preliminaries.lisp for additional details. Also see the discussion in section entitled Plan Monitoring & Debugging in Part 2. Set *debug* only at the REPL, not in the problem specification file.

```lisp
*probe* nil (any action step)
```
Setting this global variable will temporarily interrupt processing whenever Wouldwork takes a particular action. It is often useful for debugging, when you know that a particular step is wrong in the current state. It allows inspection of the current processing cycle and optional actions to help diagnose what is going wrong at a particular point in time. The argument to *probe* is a list consisting of an action name, the instantiation for the action, and the depth. For example, (move (area1 area2) 5) will interrupt processing after Wouldwork considers moving from area1 to area5 at a depth of 5. The resulting state can then be analyzed for errors. Note an exception, however, that setting *probe* must occur at the terminal prompt (REPL), not in the problem specification file.

In addition to the program control settings above, there is a global variable, called *threads*, that controls the degree of parallelism. The value of this variable can be changed, and is located at the top of the ww-preliminaries.lisp file. While the previous program control variables (eg, *depth-cutoff*) can be changed after the Wouldwork files are loaded, the parallel global variable must be set before loading SBCL, since it affects the Lisp image. To change to or from serial computation (non-parallelism), edit ww-preliminaries.lisp, exit SBCL altogether, and reload. Also note that parallelism is only available in SBCL, not in other Common Lisp implementations.

```lisp
*threads* 0 (0 <= n <= N-1)
```
This global variable specifies how many parallel threads to use in the search for a solution. The default, 0, means no parallel threads (ie, standard serial search). A value of 1 means use one parallel thread (which effectively amounts to a serial search, but uses the parallel processing mechanisms of Wouldwork—potentially useful for parallel debugging). Assuming the platform CPU can handle up to N threads, it can be useful to choose any number up to N-1, since managing parallelism will consume one thread. Choosing a number greater than N threads is feasible, but probably less efficient.

```lisp
*branch* -1 (-1 or 1 <= n <= N)
```
The variable *branch* refers to any one of the possible N actions taken from the start state. Specifying one of these branches tells Wouldwork to concentrate all processing on this one branch from the start state, and ignore the others. This might be useful for very large problems, since each branch can be searched for a solution individually in separate runs. A value of -1 searches all branches.

```lisp
*randomize-search* nil (nil or t)
```
This global variable specifies whether or not the search through the problem state space will be randomized. (Specifically, whether the order of the child states of a given state is randomized before being placed on the stack of states to be expanded next.) Multiple randomized searches may luckily hit on a solution when a standard (repeatable) search does not. Randomized search is incompatible with a heuristic search, since a heuristic will sort child states in a best-first order.

```lisp
CTRL-C
```
To interrupt the search at any time, enter CTRL-C from the keyboard, which throws SBCL into the debugger, or safely shutsdown all threads if running in parallel mode. In the debugger, entering 0 will continue the search, while entering 1 will stop the search, returning user control to the SBCL prompt.

</details>

<details>
<summary><strong>Program Output</strong></summary>

Sample program output for the 3 blocks stacking problem

```lisp
(run “blocks3”)

Current parameter settings:
 *PROBLEM-NAME* => BLOCKS3
 *PROBLEM-TYPE* => PLANNING
 *TREE-OR-GRAPH* => TREE
 *SOLUTION-TYPE* => EVERY
 *DEPTH-CUTOFF* => 0
 *PROGRESS-REPORTING-INTERVAL* => 100000
 *THREADS* => 0
 *RANDOMIZE-SEARCH* => NIL
 *DEBUG* => 0
 *PROBE* => NIL
 BRANCH TO EXPLORE => ALL
 HEURISTIC? => NIL
 EXOGENOUS HAPPENINGS => NIL
 BOUNDING FUNCTION? => NIL

working...

New path to goal found at depth = 2

New path to goal found at depth = 3

New path to goal found at depth = 2

New path to goal found at depth = 3

In problem BLOCKS3, performed TREE search for EVERY solution.

Search process completed normally.

Exhaustive search for every solution (up to the depth cutoff, if any).

Depth cutoff = 0

Maximum depth explored = 3

Program cycles = 21

Total states processed = 31

Average branching factor = 1.4

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
 0.001 seconds of real time
 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
 0.00% CPU
 3,294,240 processor cycles
 262,192 bytes consed

```
The program first outputs progressive improvements to solutions as planning proceeds (if the user has requested every or an optimal solution). In this case there are four solutions. Next is a statement about the kind of search conducted, whether the program finished normally, and the depth cutoff (ie, consideration of all possible plans less than or equal to the cutoff in path length), where *depth-cutoff* = 0 means no cutoff. The total number of states encountered during planning is also listed. The number of program cycles reports how many sweeps through all possible actions there were. The average branching factor indicates how many new states were generated (on average) from any given state during the search. The number of solution plans found is then reported, with reference to where all the successful plans are stored, if needed.

The sequence of actions in the plan is displayed next as the final planning result. The first number in each plan step indicates the time at which that action occurred, if the actions are specified to take time to complete. Each step contains the action taken along with its arguments. The order of the arguments displayed is the same as the order of parameter variables in the action effect specification, so a put action parameter specification like (?block ?target) would then display as (put A B) meaning “put block A on target B”. Lastly, the final state and a summary of computational resources expended wraps up the report.

</details>

## Extracted tables from the source `.docx`

The `.docx` contains tables that don’t reliably round-trip through paragraph extraction. They are included here to avoid any loss of information.

### Table 1

| Header | Operation | Result for block = (B1 B2) |
| --- | --- | --- |
| dot-product | Positional zip | ((B1 B1) (B2 B2)) |
| product | Full cross-product | ((B1 B1) (B1 B2)
 (B2 B1) (B2 B2)) |
| standard | Cross-product, distinct only | ((B1 B2) (B2 B1)) |
| combination | Cross-product, distinct, deduplicated by set-equality | ((B1 B2)) or
 (B2 B1)) — one representative |

### Table 2

|  |  |  |  |
| --- | --- | --- | --- |
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |
