# Strategies: How to Solve Different Problem Classes

Use this when you have a model but search is slow or fails. The core idea: **change the shape of the search** (heuristics, decomposition, pruning) before you crank compute.

## Quick diagnosis
- Too slow immediately: you likely have **too many legal actions**; add constraints or reduce symmetry.
- Finds wrong/long plans: change solution type, bounds, or add better constraints.
- No solution: either impossible goal or missing action effects/preconditions.

## Source extracts (Part 3)

<details>
<summary><strong>Brute-Force Search</strong></summary>

Implementing a brute-force search is the simplest and easiest problem specification to write. In fact, all problem solving strategies depend on an underlying depth-first brute-force search, so this should always be the first problem specification to develop and validate on a simple version of the main problem.

The principle drawback is that the time required to find a solution is generally exponential in the depth of a solution. Exploring to greater depths, therefore, may be infeasible. A rough estimate of the total size of the search space is available by running a brute-force search on the main problem, and noting the branching factor, b. Combining b with an estimate of the number of steps or depth, d, required to find a solution gives an estimate of the total search space size: bd. If the size is greater than, say 1 billion, and the number of possible paths to solutions are relatively sparse, then brute-force will likely take too long to be of use.

</details>

<details>
<summary><strong>Parallel Search</strong></summary>

Probably the simplest improvement to implement is to use some of the available core processors in your CPU. The search uses only one core by default, but this can be increased as discussed below. (But note that recent updates to the parallel algorithm to increase thread safety have significantly reduced its performance.)

Parallel processing probably works best when using a tree (as opposed to a graph) search, because many problem states may be redundantly processed in different threads, thereby reducing the effectiveness of graph search. Also, parallel search will be less efficient than serial search for shallow search trees, because of the constant thread switching overhead. But actual performance tradeoffs will depend on the problem. Parallel search can be combined with any of the other strategies.

During the initial phase of problem development, testing, and debugging, it is prudent to validate simple solutions with serial processing. But for problems with a large search space, parallel processing may be the only way to find any solution. The global variable *threads*, located at the beginning of the file ww-preliminaries.lisp, controls how many threads will be allocated to the search. (See Part 1: Program Control Settings for details.)

Wouldwork uses a pool of *threads* to manage parallelism, where each thread in the pool can perform a depth-first search over a subproblem of the originally specified problem. Initially, the main problem is given to the first thread. Then, if there are any idle threads, a subproblem is split off from the current problem, to be picked up by any idle thread. Processing continues on the current problem, with periodic checks for any more idle threads and subsequent splits, until no threads are idle. Each thread may split off a subproblem whenever an idle thread is detected. A thread becomes idle when its search tree is exhausted. Solutions are recorded globally when a thread detects a goal state.

There are a number of different techniques (discussed in the literature on parallel depth-first search) for deciding how to split off a subproblem. Wouldwork always splits off (copies into a subproblem) the top-most state in a search tree. This top state constitutes the shortest unexplored linear path back to the start state. Subsequent state expansions in the subproblem will continue from the last state in this path. This strategy (ie, top linear split) seems to work well for most problems, although massively parallel simulations have shown that a more complex multi-level strategy (ie, stack split) is generally more efficient overall. In Wouldwork the best parallel speedups seem to correlate with tree (as opposed to graph) search. But note that too many threads, or even any multiple threads, can slow down the search for some problems, since there is an overhead for managing parallelism. Experiment on a small version of a problem to determine the optimal number of threads. The following diagram illustrates the parallel speedup for the 8-Queens problem, which maxes out at 4 threads.

</details>

<details>
<summary><strong>Goal Chaining (Sequential Subgoals)</strong></summary>

For problems that cannot be solved in a single search due to depth or complexity constraints, Wouldwork supports goal chaining - the ability to solve a sequence of subgoals where each search continues from the final state of the previous search.

The typical workflow involves:
Specify the first subgoal in your problem specification file using (define-goal ...)
Load and solve: (stage problem-name) followed by (solve)
Continue from the solution state with a new goal: (ww-continue <new-goal-form>)
Make any necessary parameter adjustments
Solve for the new goal: (solve)
Repeat steps 3-5 as needed for additional subgoals

Each (solve) call automatically resets all search counters and statistics, presenting results for that search phase independently. Users can capture the REPL output after each solve to maintain a complete history of the solution sequence. Enter (dribble “file-name”) to begin recording, and (dribble) to stop recording to the file.

</details>

<details>
<summary><strong>Heuristic Search</strong></summary>

A heuristic function is a user-defined function that can focus the search for a solution. Adding a heuristic function to Wouldwork may be useful, if the baseline unfocused depth-first search fails to find a solution within a reasonable amount of time. The heuristic function essentially tells Wouldwork to explore the next possible problem states in a best-first, rather than arbitrary, order. Given a current state and the set of all possible states reachable from the current state, the lower-valued next-states will be explored before the higher-valued ones. An effective heuristic function may lead the search to the first solution quickly, although it may not be an optimal solution. But sometimes even a relatively poor heuristic may be enough to make an intractable problem solvable. Note that Wouldwork’s heuristic strategy results in a so-called “beam” search, and not an optimal A* search.

The heuristic function analyses a state, and returns a real number indicating how promising that state is for leading to a solution. A lower value indicates a more promising state than a higher value. In general, a lower value may correspond to a state which is closer to a goal. But not always. It is probably better to think of the heuristic value as an indicator of how likely a state is to eventually lead to a solution—ie, its promise.

A heuristic is specified in Wouldwork as a normal query function with no arguments, but with the reserved name “heuristic?”. (The current state is automatically filled in by Wouldwork.) The typical heuristic query function then looks like the following:

(define-query heuristic? ()
 . . . ;compute heuristic $value
 (return-from heuristic? $value))

See the file problem-triangle-heuristic.lisp for an example of a heuristic function.

Note that a heuristic function currently only works with serial, as opposed to parallel, processing.

</details>

<details>
<summary><strong>Optimization Problems</strong></summary>

Many problems can be solved using straightforward search techniques, namely, look for the first solution satisfying a goal, or look for every possible solution. However, an optimization problem requires finding a "best" solution; for example, a solution minimizing the number of steps or time taken to achieve a goal, or maximizing or minimizing some user-defined value (eg, net worth or net cost).

To optimize on the number of steps, set solution-type to min-length by including (ww-set *solution-type* min-length) in the problem specification file. To optimize on time, set solution-type to min-time, and specify for each action rule the time taken to complete that action.

To optimize on some other user-defined value (either minimizing or maximizing the value), set solution-type to either min-value or max-value, calculate the current value in the relevant action rule, and assign it to the special fluent variable named $objective-value in each assert statement. Once the current value (any real number value) is assigned to $objective-value, the planner will automatically optimize over all planning states. See the knapsack problem in the Appendix for an example of value optimization.

If the search space for a value optimization problem (ie, one involving a solution-type of min-value or max-value) turns out to be very large, it may become necessary to write a special user-defined "bounding" function to effectively prune suboptimal states. The function is optional, but may considerably shorten the time required for the planner to find the best solution. It should be specified in the problem.lisp file as a define-query, and given the name get-best-relaxed-value? like so:

(define-query get-best-relaxed-value? ()
 ;calculate the best relaxed value here
 ;eg, (setq $relaxed-value (+ $current-value $added-value))
 ;and return the new value

The current state is implicitly passed to this function, and the $relaxed-value will be associated with this state. As the name suggests, the function body should efficiently compute a so-called "relaxed" best estimated value for the state, for example using a greedy algorithm. To gain any advantage from using a relaxed value estimate, the function's computational complexity should be less than exponential (preferably linear), since the normal unrelaxed search is typically exponential.

</details>

<details>
<summary><strong>Constraint Satisfaction Problems</strong></summary>

A constraint satisfaction problem (CSP) concerns finding a value for each of a number of variables that together satisfy a number of constraints. One popular type of CSP are logic problems, as illustrated in the file problem-captjohn.lisp, where there are multiple constraints on the relative positions of objects. The n-queens problem (eg, problem-8-queens.lisp) is also a CSP, although it is readily formulated here as a simple planning problem with only one constraint.

Wouldwork was originally designed to solve planning problems, in which an agent typically can perform any number of actions in the current situation. The various actions available in various situations are encoded in the action rules, all of which are analyzed by Wouldwork in the current situation. However in a constraint satisfaction problem, it is usually possible to select the next best constraint (ie, action) to consider next, without considering all other constraints, thereby simplifying the search for a solution. CSP problems are thus distinguished from pure planning problems in Wouldwork according to how the constraints are analyzed. But all the constraints are still specified in individual action rules. Setting problem-type to csp—ie, (ww-set *problem-type* csp) — tells Wouldwork to apply this simpler stragegy, as opposed to the default (ww-set *problem-type* planning). Also (ww-set *tree-or-graph* tree) since states are not repeated in constraint satisfaction problems; and leave *depth-cutoff* at 0, since search to a depth equal to the number of rules is required.

</details>

<details>
<summary><strong>Symmetry Pruning</strong></summary>

Many planning problems involve multiple objects of the same type that are functionally interchangeable. For example, four identical blocks on a table, or six equivalent connectors to be placed. In such cases, the search may explore many equivalent solution paths that differ only in which specific objects are used, but not in the structure of the solution. Symmetry pruning detects these interchangeable objects and avoids exploring redundant paths, potentially reducing search time significantly. To enable symmetry pruning, include the following in your problem specification file:

```lisp
(ww-set *symmetry-pruning* t)

```
When enabled, Wouldwork automatically analyzes your problem before search begins and identifies symmetry groups—sets of objects that are interchangeable based on their type, static relations, and whether they appear in the goal condition. Objects explicitly named in the goal are excluded from symmetry groups to ensure correctness.
During problem initialization, Wouldwork reports detected symmetry groups:

```lisp
Symmetry groups detected: 1
 (C1 C2 C3 C4) [4 potentially interchangeable objects of type CONNECTOR]
 Search will prune equivalent paths that simply swap interchangeable objects.
 Strategy: Global — symmetric states are detected as duplicates in closed list and pruned.

```
Wouldwork automatically selects the appropriate pruning strategy based on the search mode. For graph search (*tree-or-graph* = graph), symmetric states are detected as duplicates in the closed list. For tree search, symmetric action instantiations are filtered at generation time.
At the end of a search, symmetry statistics appear in the output:

```lisp
Symmetry: 847 canonical duplicates pruned (23.4% of repeated states)

```
Symmetry pruning carries significant overhead checking, but is most beneficial when:
Your problem has multiple objects of the same type with identical initial properties
The goal does not reference specific objects (e.g., "stack any block on any other block" rather than "put block A on block B")
The search space is large enough that pruning redundant paths provides a measurable benefit

If no symmetry groups are detected, Wouldwork will report this and suggest disabling symmetry pruning for greater efficiency.

When using *solution-type* = every, be aware that symmetry pruning will eliminate solutions that differ only by which symmetric objects are used. If you need to enumerate all such variations, disable symmetry pruning for that search.

</details>

<details>
<summary><strong>Searching with Macro Operators</strong></summary>

A macro operator is an action rule that combines two (or more) individual rules into one rule. For example, instead of taking two separate steps in two separate actions, take one big step to end up at the same place. The objective is to reduce the depth and time of the search, since a macro action essentially compresses multiple actions into one.

Macro actions should be added to the individual actions, not replace them. Place all macro actions before any of the baseline individual actions, so they will be considered first during the planning process. Macro actions should be added judiciously, however, since each new action rule adds to the processing overhead for each state. It is something of an art to select useful macros that will quickly lead to a solution, from the myriad potentially useful macros for a problem. (After all, it is even possible to envision a direct solution from the start state to the goal in one step, given prescient knowledge of the search space.)

Examples are provided in the files labeled problem-triangle-macros.lisp and problem-triangle-macros-one.lisp. The six macros included in these files were added to the baseline triangle peg problems labeled problem-triangle-xyz.lisp and problem-triangle-xyz-one.lisp, respectively. Each macro jumps two pegs, rather than one at a time. The time savings is achieved by interspersing double jumps among the usual single jumps whenever possible.
A utility program, named freq, is provided for analyzing the possible macros associated with a problem. If the problem specification file is set up to find every solution to a simple version of the (difficult) target problem, then the utility program can extract all possible macros that lead to those solutions. The arguments to freq are the macros of the desired length. For example, (freq 2 3) will return all macros of length 2 and length 3, ordered by frequency. The most frequently found macro actions are the ones most likely to be of use in the final problem specification. But intuitions can be deceptive.

</details>

<details>
<summary><strong>Bi-Directional Search</strong></summary>

The size of (and time to search) a problem’s search space is typically an exponential function of the depth-cutoff (d) and the problem’s average branching factor (b): O(bd). Reducing either b or d can significantly decrease the search space. One way to reduce d is to perform two searches, one forward from a starting state, and one backward from a goal state. If the searches meet in the middle, each search will have searched to a depth of d/2. This is a significant savings, since 2bd/2 << bd.

But performing a bi-directional search involves extra programming effort. In addition to developing a specification file for the normal forward search, a second specification is required for performing a backward search. Much of the two specifications will be the same, but the action rules will be different, since each of the possible forward actions are now reversed. (However, note that for some problems it may be impossible to reverse the actions.) The depth-cutoff for each direction may also be different. That is, the forward and backward searches will each examine a different portion of the total search space, say to depths d1 (forward from a starting state) and d2 (backward from a goal state), but they must meet in the middle somewhere, so dtotal = d1 + d2.

In Wouldwork the two problem specifications are run independently, but are coordinated via a user-defined function for efficiently encoding problem states. The general procedure involves 1) running a backward search to some depth d2 to collect all possible states at that depth reachable from a goal state, 2) running a user-defined function to efficiently encode all of those states, and 3) running a forward search to a depth d1 = dtotal – d2 that hopefully matches one (or more) of the encoded backward states. Wouldwork then joins together the solutions from the forward and backward directions to derive the net solutions.

The following outlines in more detail the procedure for setting up and running a bi-directional search:
First, see if the main (difficult) problem is amenable to solving with a simpler strategy than bi-directional search. Some of the simpler methods Wouldwork supports include one or more combinations of: brute-force search, sub-goaling, bounded search, macro operators, heuristic search, parallel search, and backwards search.
The bi-directional approach in Wouldwork begins by developing a basic specification file for the usual forward search. Validate the program on one or more simple (ie, reduced) versions of the main problem to make sure the forward search is working correctly and finding all solutions to a specified goal state. (See problem-triangle-forward6.lisp for an example.)
Next, develop the basic specification file for a backward search, where the action rules reverse the actions in the forward action rules. Again, test the backward search from a goal state to a start state on the simple versions of the main problem. The solutions should correspond (in reverse order) to those found in the forward search. (See problem-triangle-backward6.lisp for an example.)
Setup the backwards search specification for the main problem, setting the solution-type to every. But set the *depth-cutoff* to a small value, and perform a backwards search only to that depth. Modify the goal to detect a solution whenever that depth is reached. Then steadily increase the depth-cutoff until all available memory is used up with every solution recorded at the depth-cutoff. You want to set the final depth-cutoff at the maximum value, d2, before running out of memory. Run the backward search with this maximum depth-cutoff to collect all states at that depth (recorded in *solutions*).
Add a standard Lisp function called encode-state to the backward search specification—ie, (defun encode-state (propositions) …), which takes a list of propositions and returns a coded representation of those propositions. The propositions are the list of current (dynamic) propositions defining any state. The encode-state function must return a unique code for each state. The most efficient code will be an integer representing the state that distinguishes it from any other state. (See problem-triangle-backward6.lisp for an example.)
Run the built-in function called get-state-codes—ie, execute (get-state-codes). This function will use your encode-state function to generate a code for every state recorded as a solution during the previous backward search at the maximum depth-cutoff. The code and the path to that state are stored in a hash table. (The subsequent forward search will consult the hash table to determine if a forward state matches a backward state leading directly to a goal.)
Setup the forward search specification for the main problem. Set the *depth-cutoff*, d1, so that the forward search will meet any states recorded in the hash table. Given the total depth to achieve a solution is dtotal, then set d1 = dtotal – d2. Also set the goal to record a solution when the forward search reaches d2 and a backward path exists in the hash table. For the latter test, use the function call (backward-state-exists state), which will return T (ie, true) when there is such a backward state. (Whether or not the forward search matches a backward search state, the forward search will not go deeper than d1.) Wouldwork will check the current state at depth d1 to see if it is in the hash table, and paste together the forward path with the backward path as a solution if it is.
The forward search may benefit from a heuristic or other optimizing strategy to better guide the search toward a solution--but not in the backward search, since every state at the given depth must be visited.

</details>

<details>
<summary><strong>The Wouldwork Depth-first Search Algorithm</strong></summary>

This final section provides a description of the particular algorithm that Wouldwork uses to perform a depth-first state-space search. It is a variation on the “ordered-search algorithm” developed by Nills Nilsson in “Problem Solving Methods in Artificial Intelligence” (1971), p.55. It offers more options for variations than some of the more modern algorithms:

Put the start node s on a list called OPEN and compute f(s) [the value of a node].
If OPEN is empty, exit with failure, otherwise continue.
Remove from OPEN that node whose f value is smallest and put it on a list called CLOSED. Call this node n. Resolve ties for minimal f values arbitrarily, but always in favor of any goal node.
If n is a goal node, exit with the solution path obtained by tracing back through the pointers, otherwise continue.
Expand node n, generating all of its successors. If there are no successors, go immediately to (2). For each successor ni, compute f(ni).
Associate with the successors not already on either OPEN or CLOSED, the f values just computed. Put these nodes on OPEN and direct pointers from them back to n.
Associate with those successors that were already on OPEN or CLOSED, the smaller of the f values just computed and their previous f values. Put on OPEN those successors on CLOSED whose f values were thus lowered and redirect to n, the pointers from all nodes whose f values were lowered.
Go to (2).

Note that Wouldwork can perform either a depth-first or backtracking search using the same problem specification. Backtracking search is more efficient when working with trees since it doesn’t copy states or maintain OPEN or CLOSED tables of states. However, backtracking often will get lost if there are repeated states, so use depth-first with graphs.

</details>
