Wouldwork’s parallel search **divides the search into branches, lets workers explore those branches independently, and shares information that helps them avoid wasted work.** Each worker runs in a thread within the same Lisp process.

This explanation is based on the current source code.

**1. The architecture in functional terms**

There are five main parts:

| Part | What it does |
|---|---|
| **Coordinator** | Prepares the work, starts the workers, waits for them, and combines their results. |
| **Shared task queue** | Holds starting points for branches that need exploring. Workers take tasks as they become available. |
| **Worker’s private stack** | Holds that worker’s pending states. Most searching happens here without coordinating with other workers. |
| **Shared visited-state record** | In graph mode, prevents workers from unnecessarily exploring the same state reached through different paths. |
| **Shared solutions and best bound** | Records solutions and, for optimization searches, tells workers what result they must improve upon. |

A **task** contains a search node: its state, its depth, and its connection to the path leading there. It means “continue searching from this point,” rather than “perform one action.”

The visited-state record is divided into separately locked sections called *shards*. Two workers can check different sections simultaneously. If they reach the same state, checking and updating its record happens under the same lock. A better route can reopen a previously encountered state.

In tree mode, workers instead check for cycles along their own current paths.

The main orchestration is in [ww-parallel.lisp](/D:/quicklisp/local-projects/wouldwork/src/ww-parallel.lisp:462); the queue and shared records are in [ww-parallel-infrastructure.lisp](/D:/quicklisp/local-projects/wouldwork/src/ww-parallel-infrastructure.lisp:21).

**2. How parallel depth-first search operates**

1. **Create an initial supply of branches.**  
   Before starting workers, Wouldwork expands the initial state level by level. It stops when it has enough tasks, exhausts the available branches, or reaches the splitting-depth safety cap. It also checks for solutions during this preparation.

   The source defaults target `max(256, 8 × number of workers)` tasks, with a splitting-depth cap of 20. These are defaults, not a measurement of your current REPL settings.

2. **Give each worker a starting node.**  
   Each worker takes a task from the shared queue and places it on its private stack.

3. **Explore that branch depth first.**  
   The worker repeatedly:
   - Takes the top node off its stack.
   - Checks depth limits and pruning rules.
   - Generates successor states by applying available actions.
   - Checks those successors for validity, goals, and repeated states.
   - Places surviving successors on top of its stack.

   Because new successors go on top, the worker explores descendants before returning to older pending alternatives. Heuristics or randomization can influence which successor comes next.

   For example, suppose its stack is `[A, B, C]`, with `A` next. Expanding `A` might produce:

   ```text
   [A1, A2, B, C]
   ```

   Expanding `A1` might then produce:

   ```text
   [A1a, A1b, A2, B, C]
   ```

   When a branch ends or is pruned, the next pending alternative is already on the stack. This DFS uses stored successor states; it does not depend on undoing actions in one mutable state.

4. **Take another task or donate pending work.**  
   When its stack empties, a worker takes another queued task. If the queue is empty while other workers remain active, it waits.

   Busy workers periodically check whether they can donate pending nodes. With the source defaults, donation is checked every 10,000 loop iterations, requires more than 256 pending nodes, and transfers about 20% when the shared queue is empty. It preferentially gives away shallower nodes, which may represent larger remaining branches.

   This is **work donation**: the busy worker hands work to the queue; idle workers do not reach into its private stack.

5. **Share discoveries and finish.**  
   In `first` mode, a discovered solution sets a shared flag that workers check to stop searching. In optimization modes, an improved solution updates the shared best bound, allowing other workers to prune branches that cannot improve it. Workers periodically refresh their cached bound.

   For exhaustive completion, an empty queue alone is insufficient: workers may still have private work to explore or donate. Completion is detected when the queue is empty and no workers remain active. The coordinator then gathers statistics and finalizes the solutions.

These operations are implemented in [worker-local-dfs](/D:/quicklisp/local-projects/wouldwork/src/ww-parallel.lisp:171) and [work donation](/D:/quicklisp/local-projects/wouldwork/src/ww-parallel-infrastructure.lisp:198).

**The important distinction is that depth-first describes each worker’s local exploration.** The whole search has several branches advancing simultaneously, following an initial level-by-level split. Donation can also reorder pending work. Consequently, parallel execution does not preserve serial DFS’s overall visitation order, and its first solution need not be the same solution—or the shortest one.