# Wouldwork Planning Software — Dense Manual (Unofficial Rewrite)

> Source: *Wouldwork Planning Software User Manual (26.7), 5th edition (2025)* by David Alan Brown.

This rewrite aims to be **hands-on and information-dense**. It keeps **all examples** from the original manual, but reorganizes the learning path:

- Start here: **README.md** (what it is + when to use it)
- Then: **QUICKSTART.md** (first successful plan run)
- Then: **TUTORIAL.md** (how to model problems)
- Then: **REFERENCE.md** (all knobs/options)
- Then: **FEATURES.md** (optional features)
- Then: **STRATEGIES.md** (how to solve different problem classes)
- Then: **PROBLEM-COOKBOOK.md** (appendix problems as recipes)

## What Wouldwork is

Wouldwork is a **classical planner / general problem solver**. You describe a world using:

- **Objects & types**
- **Relations (facts) about objects**
- **Actions** with preconditions and effects
- **Initial conditions**
- **Goal conditions** (and optional constraints)

The planner searches for a **plan** (a sequence of actions) that transforms the initial state into a goal state.

## When to use Wouldwork

Use it when your problem is mostly:

- **Discrete and symbolic** (states can be represented by facts/relations)
- **Deterministic** (actions have reliable effects)
- **Constraint-heavy** (you want explainable, rule-based constraints)
- **Explainability matters** (you want the actual plan and trace)

Typical fits:
- Puzzle-like domains (blocks world, jugs, peg solitaire)
- Scheduling with explicit constraints
- Search problems where you can clearly define operators (actions)

## When *not* to use Wouldwork (or when to be cautious)

Be cautious when your problem is dominated by:

- **Continuous control** (robotics control, PID-like problems)
- **Uncertainty / noise** (stochastic outcomes, perception errors)
- **Huge branching without strong structure** (state explosion)
- **Learning-from-data** is the main requirement (then you want ML/optimization)

A quick heuristic:

- If you can write down **actions** and **effects** clearly, and you want a plan: ✅
- If you mostly need to **estimate** unknown quantities from data: ❌ (use Bayesian/ML)
- If you have both: use Wouldwork for the **discrete decision skeleton**, and ML for scores/estimates.

## How to succeed (classical planning realities)

Most failures are modeling failures, not algorithm failures:

- **State explosion**: too many objects/relations/actions => search blows up
- **Underspecified actions**: missing effects/preconditions => planner can’t reach goal
- **Overconstrained goals**: impossible constraints => no solution exists

Debugging loop:
1. Start tiny: smallest objects, fewest actions, easiest goal
2. Validate one action at a time
3. Add constraints gradually (invariants, global constraints)
4. Only then scale up object counts

## Original manual sections (extracted)

The following sections are included verbatim-ish (formatting normalized) as expandable references.
The tutorial/docs above point into these when you need precise wording.

<details>
<summary><strong>Introduction (source extract)</strong></summary>

This is a user manual for the Wouldwork Planner (a.k.a. The I’d Be Pleased If You Would Work Planner, inspired by my grandson Elias). It covers how to download and install the software, how to write a problem specification, how to run the program, and how to interpret the program’s output. This manual is available in booklet or Kindle form for nominal cost at Amazon.com under the same name.

</details>

<details>
<summary><strong>Classical Planning (source extract)</strong></summary>

The typical classical planning problem takes place in a fully specified environment, in which an agent is attempting to plan out a sequence of actions to achieve a complex goal. The planning program, acting on behalf of the agent, analyzes numerous possible paths to the goal, and, if successful, presents a complete action plan from start to finish. Given the potentially large number of actions, environmental objects, and situations, classical planners may discover surprising solutions that elude even careful human analysis.

From a somewhat different perspective, a classical planner can also be viewed as a general problem solver. So it is generally applicable to many state-space search problems not normally regarded as planning problems. For example, Wouldwork can also solve constraint satisfaction problems (CSP) as a special case. The main advantage of using a planner for general problem solving is that the user is relieved of the task of developing a specialized state representation for a problem. A problem state is simply a list of propositions that are true in that state, and the planner reasons about states using predicate logic. Therefore, as long as the user can express potential problem-solving steps in predicate logic, the planner will independently search for a sequence of steps leading to a solution. However, this generality and convenience in specifying a problem comes with a cost. Unlike a specialized, hand-crafted problem solver, a planner cannot take advantage of sophisticated data representations. It can, however, incorporate problem domain specific features to improve efficiency.

The “classic” classical planning problem, called blocks-world, illustrates the basic operation of a planner. There is really no point in using a planner for such a simple problem, but it is suitable for introducing the basic concepts. Three blocks labeled A, B, and C are each resting on a table T. The goal is to stack the blocks so that A is on B is on C is on T. One possible action is ‘put x on y’, where x can be a block and y can be another block or the table. The shortest successful plan then contains only two steps: 1) put B on C, and 2) put A on B.

Background information about the problem is provided to the planner by the user in a problem specification file. In general, the problem specification will include a list of possible actions (eg, put x on y), a list of environmental objects (eg, A, B, and C are blocks, while T is a table), relevant properties and relations between objects (eg, x is on y), a state description for recording individual states between actions (typically a collection of facts holding at a particular time), a starting state (eg, A is on T, B is on T, and C is on T), and a goal condition (eg, C is on T, B is on C, and A is on B).

The Wouldwork Planner is designed to efficiently find any (or every) possible solution to a goal, within bounds provided by the user. Within those bounds the search is potentially exhaustive, if run to completion. This approach to planning makes it possible to find a needle in a haystack, if it exists, but is not feasible for extremely complex or large problems, which may require an inordinate amount of search time. However, since computer memory use grows only gradually, the main limitation for large problems is simply user patience.

</details>

<details>
<summary><strong>Planner Features (source extract)</strong></summary>

The Wouldwork Planner enables users to solve planning and constraint satisfaction problems without extensive programming experience. It is yet one more in a long line of classical planners. A brief listing of some other well-known classical planners would include Fast Downward, LPG, MIPS-XXL, SATPLAN, SGPLAN,
Metric-FF Planner, Optop, SHOP3, and PDDL4j. All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms. But each has its own limitations in its ability to specify and deal with certain kinds of problems.

In contrast, the Wouldwork Planner was developed not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems. (Please see the author’s medium.com article for a gentle introduction to planning problems at: https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226 .) Wouldwork focuses on the problem templates and program interfaces that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution. The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands, or potentially millions, of states per second, optionally in parallel. The program attempts to combine many of the interface capabilities of the other planners into one package. Some of the basic features of the user interface include:

General conformance with the expressive capabilities of the PDDL language for problem specification, with extensions
Arbitrary object type hierarchies
Mixing of object types to allow efficient selection of objects during search
Action rules with preconditions and effects, based on predicate logic
Full nested predicate logic expressiveness in action rules with quantifiers and equality
Specification of initial conditions
Goal specification
Fluents (ie, continuous and discrete variable quantities & qualities)
Durative actions taking time to complete
Exogenous events (ie, happenings occurring independently of the planning agent’s actions)
Temporal plan generation (ie, possible action schedules)
Global constraint specification, independent of action preconditions
User function specification for on-the-fly, possibly recursive, embedded computations
Specification of complementary relations to simplify action rules
Inclusion of arbitrary Common Lisp code in action rules, constraints, and functions
User control over search depth
Identification of shortest length, shortest time, or min/max value plans
Optional parallel processing to speed up search
Output/debugging diagnostics describing details of the search

</details>

<details>
<summary><strong>Disclaimer (source extract)</strong></summary>

The Wouldwork Planner is open-source software. It can be freely copied, distributed, or modified as needed. But there is no warrant or guarantee attached to its use. The user therefore assumes all risk in its use. Furthermore, the software was developed for experimental purposes, and has not undergone formal unit testing. Run-time error checking is piecemeal, and latent software bugs surely remain. The software runs optimally in the Steel Bank Common Lisp (SBCL) programming environment. It is designed to be portable for other common lisp implementations, but only tested with CCL. Search is compute-intensive, so expect significantly longer run times if not using SBCL. Users may send bug reports, suggestions, or other useful comments to Dave Brown at davypough@gmail.com.

</details>
# Quickstart

Goal: **run Wouldwork once** and understand what “success” looks like.

## 0) Prereqs
- You need a Common Lisp implementation (the original quickstart uses SBCL).
- You need the Wouldwork sources available and loadable in your Lisp.

## 1) Run SBCL the way the manual recommends

Use the same launch flags as in the manual (important for larger searches).

```lisp
> sbcl --dynamic-space-size 2048
```

If you hit memory pressure later, this is the first knob to revisit.

## 2) Load Wouldwork and run a sample problem

Pick *one* of the appendix problems as your first run (Blocks World is usually ideal).

1. Open `PROBLEM-COOKBOOK.md`
2. Copy the full problem specification into a file (or use the provided sample file if present)
3. Start the planner run as described in that recipe

## 3) What success looks like
- The planner prints a **plan** (sequence of actions), or
- It tells you there is **no solution** within the given bounds/settings.

## 4) If you get stuck
- Reduce problem size (fewer objects).
- Relax constraints.
- Check that action preconditions/effects actually allow progress.
- Use trace/debug output (see `REFERENCE.md` and `FEATURES.md`).

## Source extract

<details>
<summary><strong>Quickstart with SBCL (source extract)</strong></summary>

Install the SBCL Common Lisp release for your computer from http://www.sbcl.org/platform-table.html
Install Quicklisp from https://www.quicklisp.org/beta/
Clone or download the Wouldwork repository to your computer from https://github.com/davypough/quick-wouldwork placing it in your ~/quicklisp/local-projects/ directory (the path to the project files should then be ~/quicklisp/local-projects/quick-wouldwork/)
Start SBCL from your terminal command prompt.
At the SBCL prompt, enter (ql:quickload :wouldwork :force t)
Enter (in-package :ww) to switch the current package from cl-user to wouldwork.
Enter (run-test-problems), or just (test), to verify everything is loaded and running properly.
Review the printout from any of the test problems to see the format of solutions.
Look at the sample problem specifications (eg, problem-blocks3.lisp) in the src directory to become acquainted with how problems are defined.

Once you have the SBCL Common Lisp environment installed (I like SBCL because it is open-source and compiles into speedy code), you can add your own problem specifications or modify the source code. Note that the environment will need Quicklisp and ASDF pre-installed as a baseline, plus the :alexandria, :iterate, and :lparallel Quicklisp libraries (automatically installed when loading Wouldwork). Check the file wouldwork.asd for detailed information about what Wouldwork loads.

Also note that SBCL loads with 1024MB = 1GB of memory by default. If a planning problem exits with an out-of-memory condition (heap exhausted), you will need to increase the default by loading SBCL at the command prompt with additional memory, for example:

```lisp
> sbcl –dynamic-space-size 2048

```
which doubles the default (or 4096, 8192, etc, depending how much memory you have).

Planner parameters (*depth-cutoff*, *solution-type*, etc) are either set in the problem specification, at the REPL after loading Wouldwork, or simply left at their default values. Finally, executing (run <problem-name>) runs the planner on the specification file. To see a list of all currently defined problems in the /src directory, enter (list-problem-names). Enter (help) at the REPL to see a list of all available commands.

</details>
# Tutorial: Model a Problem in Wouldwork

This tutorial teaches the **core interface** by walking through the modeling pipeline. Use the appendix cookbook for complete end-to-end examples.

## Mental model

You provide a *problem specification* describing:

1. **Objects & types** (what exists)
2. **Relations** (facts that can be true/false)
3. **Actions** (how facts change)
4. **Initial state** (which facts are initially true)
5. **Goal** (facts you want to make true), plus optional constraints

A planner run is then: **search** over action sequences.

## Step-by-step checklist (printable)

- [ ] Can I name all objects? (or generate them systematically?)
- [ ] Can I express state as boolean facts/relations? (avoid hidden state)
- [ ] For each action: do I have complete preconditions and effects?
- [ ] Do I have a goal that is actually reachable?
- [ ] Do I need optional features (quantifiers, fluents, exogenous events)?

## Core sections (source extracts)

<details>
<summary><strong>Problem Specification</strong></summary>

Most classical planners take as input, a text-like specification of a planning problem provided by the user, and this planner is no exception. The standard problem specification language for classical planning is PDDL, which offers the user a straightforward way to define various types of environmental objects, properties and relations, as well as possible actions, constraints, goals, and initial conditions.

The format of a problem specification file for the Wouldwork Planner is a variation on PDDL to allow some further simplifications and elaborations, but at the expense of being able to represent some very complex planning scenarios falling outside the scope of this planner. The following paragraphs outline the essential sections comprising a problem specification file. The blocks-world problem mentioned previously will serve as a running example. Additional specification sections for more complex problems are left for Part 2 and the Appendix. Advanced planning applications may include arbitrary Common Lisp code along with the standard PDDL sections.

</details>

<details>
<summary><strong>Specifying Environmental Objects & Types</strong></summary>

The first requirement is to specify the various objects and object types
relevant to the problem domain. In the blocks world, there are three blocks (named A, B, C) and a table (named T). The following definition establishes this background information for the planner:

```lisp
(define-types
 block (A B C)
 table (T)
 support (either block table)) ;where a block can be placed

```
To the left are the object types, and to the right, the particular objects of that type appearing in the problem. The last type (namely, support) is a sometimes useful catch-all type signifying a generic entity of some kind, in this case either a block or table, both of which can support blocks. So A, B, C, and T are all supports. Generic types are often useful for simplifying action rules, to be discussed shortly. The ‘either’ construct simply forms the union of its argument types. In this example the object names correspond to actual objects in the blocks world, but in general objects can also be values like 1, 2, or 3 or any other Common Lisp programmatic object. Value objects provide a convenient way to represent some discrete properties, such as location coordinates or scaled discrete quantities, which then allows straightforward enumeration of the values in action rules.

</details>

<details>
<summary><strong>Specifying Object Relations</strong></summary>

The second requirement is to specify the relevant relations (includes properties) which may hold for and between the various object types. Primary relations are used by the planner to generate and analyze various states of the environment during planning. In the blocks world there is only one relation that needs to be considered, namely whether a block is, or is not, on some support, specified as: (on block support). In other words, in this problem it is possible for a block (A, B, or C) to be on some kind of support location (A, B, C, or T), and particular instantiations of this relation will be present in various states during planning. The full relational specification is then:

```lisp
(define-dynamic-relations
 (on block support))

```
The dynamic relation specification indicates that the situation of a block being on a support may change from state to state during the course of planning. (Static—ie, unchanging—relations are discussed later in Part 2. Optionally, all relations can be specified as dynamic, but it is computationally more efficient to separate the two.)

</details>

<details>
<summary><strong>Specifying Possible Actions</strong></summary>

The third specification is for the individual actions that the planning agent can take. In this case the agent can take a block and put it either on the table or on another block (in a stack). Actions are always composed at least of a precondition, specifying the conditions that must be met before the action can be taken, and an effect, specifying changes in the state of the environment after the action is taken. Since the action here is one of putting a block (labeled by a variable like ?block) on a support (like ?support), the precondition must specify that there is not another block on top of ?block, and in addition, that there is not another block on top of ?support, unless ?support is the table. It is conventional, and required in Wouldwork, that typed variables have a question mark (?) prefix. The following precondition expresses these conditions in predicate logic, where ?block signifies the block to be put somewhere, ?b signifies some arbitrary block, ?block-support is the support the block happens to be on currently, and ?support signifies the support on which ?block will be put:

```lisp
(and (not (exists (?b block) ;no block is on ?block
 (on ?b ?block)))
 (on ?block ?block-support) ;?block is on some support
 (not (exists (?b block) ;no block is on the support
 (on ?b ?support)))

```
In plain English, the first condition says that there does not exist a block which is on the block to be moved--ie, that it has a clear top. Later it will be explained how to define auxilliary functions, like cleartop?, which can be used to simplify such action conditions. The second condition verifies that ?block is on some kind of support, labeled ?block-support (either the table or another block); and the third condition checks that that the support place, labeled ?support, itself has a clear top.

It is worth noting that the above action can be expressed much more straightforwardly if fluents and functions are used:

```lisp
(and (cleartop? ?block)))
 (bind (on ?block $block-support))
 (cleartop? $support))

```
These kinds of enhancements to PDDL are discussed in Part 2, and illustrated in the specification for problem-blocks3a.lisp.

Once the action precondition is established, the action effect then must specify what happens if the action is taken by the planning agent. The effect (in this case after an instance of ?block is put on some ?support), is that the block will now be on that support; and also that the block will no longer be on what was previously supporting it. This effect is concisely expressed as:

```lisp
(assert (on ?block ?support)
 (not (on ?block ?block-support)))

```
In other words, ?block now will be on ?support, and ?block is no longer on ?block-support. The assert operator groups multiple changes to the current state together, and automatically arranges and executes them in the proper order to maintain database consistency. Like the precondition, the effect necessarily consists of only one (possibly complex) statement.

The assert operator establishes a scope for propositional updates to the current state. An assert statement, therefore, can include any number of sub-statements (except another assert statement). All of the asserted propositions within its scope will contribute to the next state.

Bringing the precondition and the effect together defines a complete action rule, consisting of six parts. First is the name of the rule (put), second is the duration of the action (where 1 means one unit of time), third is the list of free parameters appearing in the precondition (where a variable, or list of variables alternates with its type, as discussed later), fourth is the precondition, fifth is the free parameters related to the effect, and sixth is the effect itself:

```lisp
(define-action put
 1
 (?block block (?block-support ?support) support)
 (and (not (exists (?b block)
 (on ?b ?block)))
 (on ?block ?block-support)
 (not (exists (?b block)
 (on ?b ?support))))
 (?block ?block-support ?support)
 (assert (on ?block ?support)
 (not (on ?block ?block-support))))

```
On each planning cycle, each trial action is evaluated by the planner in the order presented in the problem specification. For each action during evaluation, all possible nonredundant arrangements of the precondition parameters are considered. For the above rule, the possible instantiations of ?block are A, B, and C, while the possible ?block-support and ?support values are A, B, C, and T. This leads to a set of 18 (?block ?block-support ?support) arrangement triples generated by the precondition parameters: ((A B C) (A B T) (A C B)
(A C T) (A T B) (A T C) (B A C) (B A T) (B C A) (B C T) (B T A) (B T C) (C A B) (C A T) (C B A) (C B T) (C T A) (C T B)). After removing duplicate variable assignments, each is tested against the action preconditions in the current state. If one or more instantiations satisfies the precondition, then the action’s effect is executed with those instantiations, producing an update to the current state. (See Part 2: Parameter Lists, for additional details.)

Each successful instantiation represents a possible next step in the planner’s path to the goal. As discussed in Part 2, the instantiation procedure is the same for local parameters appearing in quantified expressions (ie, expressions headed by exists or forall in the precondition or effect), except then the local parameter combinations are instantiated in the context of the current action parameter instantiations.

In general, logical expressions in an action can be headed by any of the usual logical labels—exists, forall, and, or, not, if, <relation name>, <function name>, <lisp function> (see Part 2 for a full list). An argument in a logical expression can generally be any <typed variable>, <fluent variable> (see Part 2), integer (allows iteration over values in the same way as with typed variables), real number (eg, the value of a fluent), or a <lisp lambda-expression> (which is first evaluated to produce the argument in the logical expression in which it occurs).

It is not unreasonable to complain that writing action rules is often difficult. We normally do not reason with predicate logic, and keeping all the variables straight requires some amount of bookkeeping (not to mention trial and error). However, predicate logic has been shown to be a highly expressive language for representing all manner of technical problems, one of which is planning. Its precision is often particularly useful in highlighting subtle errors in thinking. To better handle complex logical operations, the logical statements can often be usefully grouped into functions or independent actions. For example, another option for expressing the action put above is to break it into two actions, say put-block-on-table and put-block-on-block. This would simplify the logic for each rule, but at the potential cost of added debugging, maintenance, and planner processing.

When writing action rules, it may be helpful to remember that when an action runs, all of the parameters for the precondition first will be instantiated for all possible combinations of the variables. Then, the body of the precondition is run for each combination. Every time an instantiation meets the precondition, that instantiation becomes the context for instantiating the effect. The body of the effect then runs for each precondition instantiation, along with any further effect instantiations, thereby registering the specific effects of the action in the next state. In this way, executing a single action rule may give rise to multiple effects, each of which generates a successor state to the current state.

</details>

<details>
<summary><strong>Specifying Initial Conditions</strong></summary>

The next required specification characterizes the initial state of the environment, from which planning commences. The initial conditions are simply a list of all facts which are true at time = 0. Below are the initial conditions for the blocks world, indicating that all three blocks are on the table:

```lisp
(define-init
 (on A T)
 (on B T)
 (on C T))

```
Note also that the planner bases its analysis of all environmental states, including the initial state, on the usual closed-world assumption. This means every fact is represented as being either true or false, and never unknown. Therefore, if a fact is true, it will appear in a state representation at a particular time—for example, as (on B T) does in the initial state above. If this statement did not appear in the initial state, it would mean that B was not on the table—that is, that (on B T) was false. This way of representing facts (as either present or absent in a state) makes it easy to check for true and false conditions for a state in the action rules—for example, (not (on B T)) expresses the condition in an action rule that B is not on the table. But
(not (on B T)) does not appear in the state itself.

</details>

<details>
<summary><strong>Specifying the Goal Condition</strong></summary>

The last requirement is for a goal condition, which is also expressed in terms of a single predicate logic statement. When the planner encounters a state in which the goal is true, the planner records that state, and the path to it from the initial state, as a solution. Depending on whether the user has requested the shortest possible path (in number of steps or duration), or merely the first solution path encountered, the program either continues searching, or exits, respectively. In the blocks world, the goal is to appropriately stack three blocks, which is satisfied when the following fact is true:

```lisp
(define-goal
 (and (on C T)
 (on B C)
 (on A B)))

```

</details>

<details>
<summary><strong>Operational Considerations</strong></summary>

This completes the basic specification for the blocks world problem, except to note that the user can tell the planner how deeply to search for a solution. Since the shortest solution involves only two steps—namely, (put B C), and (put A B)—the user could set the *depth-cutoff* at 2 steps. Setting the *depth-cutoff* at 2 will end up generating the minimal number of intermediate states, but it is typically not known in advance how many steps will be required to solve complex problems. Choosing a large value will increase the search time due to greater search depth, but the planner can still find the shortest path to the goal among all paths shorter than or equal to that value. But choosing a value too small may lead to finding no solutions. Some experimentation with different depth cutoffs and partial solutions may be needed to solve complex problems. For the blocks-world problem it is not necessary to specify a depth cutoff, since Wouldwork automatically determines when to stop going deeper (ie, when all possibilities are exhausted).

Another viable option for problems where the total number of possible states is not too large (say under 1,000,000) is to leave the depth cutoff at its default value of 10, and stipulate a graph (rather than tree) search. A graph search avoids reanalyzing previously visited states, thereby potentially shortening the search, but incurs additional overhead that a tree search avoids. The depth cutoff can be gradually increased for subsequent runs. For the blocks problem the total number of possible states is relatively small (ie, all combinations of three blocks arranged in different ways), so setting the *depth-cutoff* to 0 (meaning go as deep as necessary) is fine, potentially examining all combinations in the worst case.

From the user’s perspective, interacting with Wouldwork can be thought of as first staging (ie, loading) a problem, and then solving it. When Wouldwork first loads, it automatically stages the blocks3 problem or whatever problem you were last working on. Once staged, a problem can be solved by entering (solve) at the REPL. Entering (stage problem-name) at any time will stage a new problem. Staging and solving will occur together if (run problem-name) is entered. Other commands can also be entered at the REPL—enter (help) to see a list. Note that if you edit and save a problem specification file, you will need to enter (refresh) to let Wouldwork know about the changes before solving again.

</details>
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
# Optional Features

These features extend the basic planner model. Don’t use them unless the core model is working; optional features can multiply the state space.

## Source extracts (Part 2)

<details>
<summary><strong>Object Types</strong></summary>

Every object constant (eg, A or block1) must have a user-specified type (eg, block), in the define-types specification. The type is listed first, followed by a list of object constants of that type. The object constants must all be Common Lisp objects, usually symbols. In general, objects can have more than one type, and types can have subtypes, as a convenience for specifying action rules. For example in the blocks world, A is both a block and a support since it can support other blocks; and support includes both table and block as subtypes. By default, block A is also an instance of the supertype called something, since every object is a something, and every type is a subtype of something. Thus, a generic statement like (something ?obj) is valid for determining if some object is known to Wouldwork.

Specifications of subtypes are distinguished from object constants by the keyword either. For example, support has subtypes (either block table). But block has objects (A B C) or, perhaps in a more perspicuous specification, (block1 block2 block3), where the type is included in the name of the object. To include an object type with no object instances, specify the instances as () or NIL. Instead of listing a large number of objects for a type, you can also use the operator compute, followed by Common Lisp code, to automatically compute the object list (see Problem 8: Triangle Peg Puzzle in the Appendix for an example).

It is also possible to specify dynamic types—ie, types whose instances can vary from state to state. For example in the Triangle Peg Puzzle, the number of pegs left on the board in any state after a jump is one less than in the prior state. If each peg on the board is being checked to see if it can jump any other peg, it makes sense to only check those pegs which are currently on the board in any state, instead of all pegs (including those which have already been removed from the board). To specify such a dynamic type, create a query function to compute the current board pegs (see the following section entitled Query, Update, & Lisp Functions for how to write a function). For the Triangle Peg Puzzle, the dynamic type could look like:

```lisp
(define-types
 peg (peg1 peg2 peg3 …) ;static type (all pegs)
 current-peg (get-current-pegs?) ;dynamic type
 …)

```
where the function get-current-pegs? retrieves the current pegs from the current state. The type current-peg can then be used in action rules to generate pegs in the same way as the type peg will generate all pegs. Alternately, the precondition of the action rule could simply use the dynamic type directly in the parameter list as in (?peg (get-current-pegs?)). The savings in processing time may be significant, since the set of current pegs will always be less than or equal to the set of all pegs.

</details>

<details>
<summary><strong>Object Relations</strong></summary>

A fundamental description of any object necessarily includes the properties it has and the relations it has to other objects. Accordingly, each relevant relation (or property) of every object must be included either in a define-dynamic-relations or a define-static-relations specification. To illustrate the dynamic/static difference, consider a dynamic relation like (on block support), which could be instantiated by a proposition like (on A T). During the course of planning, the planner maintains a local database of propositions that are true for each state. When the status of A subsequently changes from (on A T) to (on A B), the current database is updated for the next state. However, static propositions, like (block A) indicating that A is a block, never change, and do not need to be maintained in every state. Static propositions are more efficiently stored in a separate database, which the planner can take advantage of if the user defines dynamic and static relations separately. Note, in passing, that in the proposition (block A), the term block is being used as a predicate. However, block is also a type. Predicates and types are distinguished by their context of use.

The relations of or between objects are specified according to object type, and serve as a template for the propositions that instantiate them. Binary relations are probably the most common, specifying a relation between two object types. For instance, (on block support) expresses a binary relation between blocks and supports. A unary relation expressing a property like (red block), says that blocks can be red. A trinary relation like (separates gate area area) indicates that gates separate two areas, and so on for other higher order relations specifying relations among an arbitrary number of types. Relations can even have no arguments, such as (raining), simply indicating the proposition that it is raining. The type arguments to a relation can also include the either construct, as in (color (either block table) hue), indicating that both blocks and tables can have a color, assuming the type hue includes object constants like red, blue, green, etc. The current limit on relation arity is four arguments. Thus, a relation like (connected node node node node node) incorporates one argument too many. All relations are fixed arity.

When a relation includes duplicate types like (separates gate area area), it is interpreted by Wouldwork as a symmetric relation. This is a convenience, since the user then does not need to worry about how the symmetric types (ie, the 2nd and 3rd area arguments) are instantiated during planning. The user can simply include (separates ?gate ?area1 ?area2) in an action, constraint, function, etc; leaving out the reverse symmetric test for (separates ?gate ?area2 ?area1), since Wouldwork automatically includes both in any database. Whenever one proposition like (separates gate1 area1 area2) becomes true or false, the reciprocal proposition (separates gate1 area2 area1) also becomes true or false. In those special cases where the relation is not symmetric, the default symmetric assumption can be cancelled by marking the relation with a “>” directional suffix—eg, (separates> ?gate ?area1 ?area2). Now the statement will only match the ?area1 and ?area2 variables in the given order.

Relations may also include fluent types, which act as variable types, most commonly for numbers, but also for other defined user types. For example, a relation like (height block $real) might specify that blocks have a height that is a real number, possibly useful for evaluating the net height of a stack of blocks. Fluent variables are identified by their dollar-sign ($) prefix, much as logical variables like ?support are identified by their question mark (?) prefix. The type label (following the $) must be a valid user-defined or Common Lisp type.

Fluent variables, however, operate differently from their logical variable counterparts in actions. While logical variables generate object instantiations, one at a time, for testing in actions; fluent variables are instantiated by looking in the propositional database of the current state for a matching proposition. In the previous example, the relevant database is queried for a proposition matching the pattern (height block $height). If it finds a proposition like (height A 3.2), it instantiates the variable $height with the value 3.2. This instantiation is then available for further evaluation in the action as the value of the variable $height. Note that a relation with fluents is more usefully characterized as a function (being a special kind of relation). For this reason there should be only one proposition in the database that can match the fluent pattern. Otherwise, the fluent instantiation would be ambiguous among the possible choices.

The returned value of a fluent is not limited to numerical quantities. Any user-defined type (eg, $hue) can also be a fluent (assuming the type hue includes object constants like red, green, blue, etc., and objects can have only one color at a time). A statement like (color block $color) illustrates this way of using a fluent variable. Looking at the actual problem specifications in the Appendix may help clarify the different fluent options and capabilities.

Sometimes, usually for efficiency reasons, a user may specify a relation that includes a lisp container fluent—ie, a list, vector, array, or hash-table. For example, the set of used words in the file problem-crossword5-11.lisp is specified as (used-words $hash-table), which records the words in a set that have been tried so far in the search. In these cases, the usual choice to represent a set will be to use a hash-table rather than a list or vector, because the containers are always tested for equality with the predicate #’equalp. The other choices, in general, will not be #’equalp even if they have the same elements. Only use one of the other choices if the order of elements in containers with the same number of elements is always the same—eg, a lexicographic ordering.

</details>

<details>
<summary><strong>Bijective Relations</strong></summary>

In some domains, a relation may be functional in both directions—ie, the mapping from either argument uniquely determines the other. For example, consider a puzzle where each item occupies exactly one position, and each position holds exactly one item. The relation (at item position) exhibits this one-to-one correspondence: given an item, there is exactly one position it occupies; given a position, there is exactly one item there.

Normally, to support efficient lookup in both directions, the user would need to define two separate relations:

```lisp
(define-dynamic-relations
 (at-by-item item $position) ; lookup position given item
 (at-by-position position $item)) ; lookup item given position

```
This is awkward because the user must then maintain both relations consistently—asserting and retracting both whenever the state changes. Wouldwork provides the :bijective annotation to handle this automatically:

```lisp
(define-dynamic-relations
 (at $item $position :bijective))

```
When a relation is declared bijective:
Both arguments must be fluent (prefixed with $), and there must be exactly two arguments.
Assertions and retractions are automatically mirrored. When you (assert (at box1 pos3)), Wouldwork internally maintains indices for lookup in both directions.
Bind operations work in either direction. At runtime, exactly one of the two $variables should already be bound (non-nil). The other will receive the looked-up value:

```lisp
(and (bind (loc agent1 $area)) ;$area gets bound, say to AREA1
 (bind (at $item $area)) ;$area is bound → lookup $item
 ...)

```
Important restrictions for bijective relations:
The relation must be genuinely one-to-one in both directions. If multiple items can occupy the same position, or an item can be at multiple positions, do not use :bijective.
Do not use NIL as a valid type instance for arguments of bijective relations. Wouldwork uses null-checking at runtime to determine which variable is bound. If you need to represent "no value," omit the proposition from the database rather than storing a NIL value; the bind will return NIL (false) if no matching proposition exists.
Within a single action rule, if you need to rebind the same $variable to a different value via a second bijective bind, first reset it to NIL using (setq $var nil), or use a different variable name.

Example:

```lisp
(define-types
 box (box1 box2 box3)
 slot (slot1 slot2 slot3))

(define-dynamic-relations
 (in $box $slot :bijective)) ; each box in exactly one slot,
 ; each slot holds one box

(define-action swap
 1
 (?slot1 slot ?slot2 slot)
 (and (bind (in $box1 ?slot1)) ; find what's in slot1
 (bind (in $box2 ?slot2))) ; find what's in slot2
 (?slot1 ?slot2 $box1 $box2)
 (assert (in $box1 ?slot2) ; move box1 to slot2
 (in $box2 ?slot1))) ; move box2 to slot1

```

</details>

<details>
<summary><strong>Complementary Relations</strong></summary>

For a given relation R, the complement of R is the relation expressing the negation of R (ie, not R). Complementary relations come in pairs, such that if any propositional instance of one is true, the corresponding propositional instance other is false. For example, (on switch) and (off switch) are complements. That is, for any instantiation of switch, say switch1, whenever (on switch1) is true, then (off switch1) is false, and vice versa.

If the user stipulates which relations are complements by using the define-complementary-relations specification, the planner will automatically keep track of which complementary propositions are true and which are false. Then, if an action rule concludes that switch1 is on by asserting (on switch1), the planner will automatically remove (off switch1) from the current database. Otherwise, the user must also assert (not (off switch1)) to maintain database consistency.

The previous switch example illustrates the simplest kind of complementary relation, namely one in which the arguments of both relations are the same (ie, switch). For simple complements like on/off, the planner can always work out how to deal with the assertion of any associated proposition, since (on switch1) implies (not (off switch1)), (not (on switch1)) implies (off switch1), (off switch1) implies (not (on switch1)), and (not (off switch1)) implies (on switch1).

However, some complements share only some, or even no, arguments. In the expanded blocks world problem, there is a gripper arm that moves blocks around. In this world, it is useful to know when the gripper is holding a block (in which case it cannot pickup another block) and when the gripper is free. The corresponding relations (holding block) and (not (free)) are therefore complements, since (holding block) implies (not (free)), and (not (holding block) implies (free). But the reverse direction is problematic. While (free) does imply (not (holding ?)), where ? matches anything that the gripper is currently holding, if (not (free)) were asserted, it is indeterminate what the gripper would then be holding. For this reason, all complement specifications are limited to the forward direction only. If the reverse direction is warranted, it must be included separately.

Putting all of the above examples into a specification of complementary relations would look like:

```lisp
(define-complementary-relations
 (on switch) -> (not (off switch))
 (off switch) -> (not (on switch))
 (holding block) -> (not (free)))

```

</details>

<details>
<summary><strong>Logical Statements, Quantifiers, & Doall</strong></summary>

Actions, goals, constraints, functions, and other user-defined constructs are composed of logical statements that the planner uses to analyze the current active planning state. All statements must adhere to the previous type and relation definitions as specified. Many statements simply return a true or false value when executed, but others may return a user-defined value (like query function calls), or add and delete propositions from a state (like assert statements) . Simple examples of logical statements appear in Part 1. The complete list of admissible logical statement forms in Wouldwork is included in the later section: The Variety of Logical Statements.

Of particular note are the quantifier statements forsome or exists (existential quantification) and forall or forevery (universal quantification). An existentially quantified statement generates all possible values of the local variables in its parameter list, and executes the statements in its body until some instantiation returns true, at which time the entire statement immediately becomes true. If the body is not true for any instantiation, then the statement returns false. Likewise, a universally quantified statement generates all possible instantiations, but returns true only if every instantiation is true. It returns false immediately if any instantiation is false. In general, quantifier statements, can be nested to an arbitrary level. Additional details about a quantifier’s parameter list are discussed in the subsequent section on Parameter Lists.

Experience has shown that a third “generating” logical statement form is also sometimes useful, in that it guarantees all instantiations will be processed. This is the doall statement, which follows the form of the quantifier statements, for example:

```lisp
(doall (?g gate ?s switch)
 (if (and (controls ?s ?g)
 (off ?s))
 (assert (inactive ?g))
 (assert (active ?g))))

```
This statement tests to see if the switch control for a gate is on or off in the current state, and updates the state accordingly. It always returns true.

Note, however, that in this case, generating and testing all possible values of gate and switch can be expensive. If there are four gates, each of which is controlled by a switch, there are a total of 4x4 = 16 separate instantiations (<gate1,switch1>, <gate1,switch2>, …), each of which is tested in the if statement. Since each gate is controlled by only one switch, and a switch can only be on or off, it would be much more efficient to write a statement like:

```lisp
(doall (?g gate)
 (if (and (controls $s ?g)
 (off $s))
 (assert (inactive ?g))
 (assert (active ?g))))

```
which uses a fluent variable ($s) as discussed later. Here, the fluent variable is bound to whatever switch controls the current generated gate ?g. Now there is only one test for each gate, for a total of 4 instantiations.

</details>

<details>
<summary><strong>Durative Actions</strong></summary>

For many planning problems, in which the required solution only involves the sequence of planning actions, the time to complete each action is irrelevant. In these cases the second argument in an action specification (after the name) can be 0, indicating instantaneous execution. For other problems where the duration of actions is relevant, the second argument will be a positive real number, signifying the time taken to complete the action. The duration can be specified in any arbitrary time units the user chooses, as long as the units are consistent in all of the action rules (and in any other requirements which are part of the problem specification).

One simple durative action might involve the planning agent’s movement from one area to another, where the agent is named me below. Assuming movement only occurs between adjacent areas, and the time to move to any new adjacent area is the same, the move action could be expressed as:

```lisp
(define-action move
 2.5 ;moving takes 2.5 time units
 ((?area1 ?area2) area)
 (and (loc me ?area1)
 (adjacent ?area1 ?area2))
 (?area1 ?area2)
 (assert (not (loc me ?area1))
 (loc me ?area2)))

```
As an aside, note that this action involves two variables which are both areas. Since the Wouldwork Planner always interprets variables as names for unique individuals, two distinct variables of the same type will always be instantiated with different objects. This convention means it is not necessary to check for equality with a statement like (not (eql ?area1 ?area2)) in the precondition above. Although this convention does violate a basic tenet of predicate logic, it is consistent with the intuitive understanding that we usually give different names to different objects in a given context to avoid confusion. It is straightforward to override this behavior, however.

</details>

<details>
<summary><strong>Fluent Variables & Variable Binding</strong></summary>

A fluent variable, indicated by the prefix $ (as in $support), contrasts with a generated variable, indicated by the prefix ? (as in ?support). While generated variables are instantiated in logical statements by generating all possible instances of the variable (eg, all possible supports for the variable ?support), fluent variables are instantiated by looking up a matching proposition in the current database. For example, in a statement like (on ?block $support), a previously generated value for ?block is first consulted (eg, A), and then the current state database is consulted to determine if A is on something. If it is, that something becomes the value of the fluent $support.

Fluent variables, then, provide a very efficient way of evaluating the truth of a statement, since there is no need to generate and test all possible values of a variable—a simple lookup suffices. However, to avoid ambiguity there should be only one, or no, matching proposition in the database. This limits fluents to appearing in functional relations like on—a block can only be on one support at a time. When there is a choice between using a generated or fluent variable, the fluent representation is much more efficient.

The object variables in the blocks world problem (namely, ?block and ?support) are discrete variables, in that they can only take on discrete values, such as A, B, C, or T. However, for other problems it is useful to allow continuous variables as well, perhaps representing the height and weight of a block. As discussed previously, in addition to relations like (on ?block $support), relations like (weight ?block $w) or
(height ?block-support $h) could also be used in action preconditions, say to account for limitations in the agent’s ability to move certain blocks that weigh too much or are stacked too high. The continuous variables $w and $h, distinguished by their required $ prefixes, are technically known as numerical fluents, and in this case can take on positive real number values. Numerical fluents are instantiated by the same lookup procedure as for discrete fluents.

Sometimes there is a choice of whether to represent a variable as a fluent ($) or as a discrete generated (logical) variable. Generally speaking, fluents are most useful for representing data (eg, the height of a block), while logical variables represent symbolic values (eg, individual blocks and supports). But data can also be made discrete if there are a modest number of possible values, say <100. In this case the discrete values must be defined as a type—eg, (define-type height (1 2 3 … 100)) so the action rules can generate the values in parameter lists. Note that processing will slow down somewhat if equality between the values cannot be tested with #’eql (eg, if the values are lists or vectors). Often it is better to simply use a fluent for such values—eg, (define-dynamic-relation (locations $list))—where $list represents a list of coordinates like ((0 0) (3 1) …). Coordinates can then be dynamically added to or removed from the current locations list in the action rules.

As a more complete example, consider the specification of the classic “jugs” problem. Two jugs of different size are used to measure out a specific amount of water taken from a large reservoir. There are no markings on the jugs to indicate graded amounts. A jug can be filled or emptied completely at the reservoir, or emptied into the other jug to the point where the other jug is full. The goal is to get some reservoir water and pour it back and forth between jugs until a specific target amount of water remains. The first part of the problem specification might look like:

```lisp
(define-types
 jug (jug1 jug2))

(define-dynamic-relations
 (contents jug $integer))

(define-static-relations
 (capacity jug $integer))

```
Here, the relations contents (representing the current amount of water in a jug) and capacity (representing the maximum amount of water a jug can hold) take a discrete argument (one of the jugs) and a fluent argument (an integer, indicated by the required $ prefix). And both relations are clearly functional, since a jug has only one contents and capacity at any time. The action of filling a jug with water from the reservoir then takes the form of the following rule:

```lisp
(define-action fill
 1
 (?jug jug)
 (and (bind (contents ?jug $amt))
 (bind (capacity ?jug $cap))
 (< $amt $cap))
 (?jug $cap)
 (assert (contents ?jug $cap)))

```
The action uses the fluents $amt and $cap to represent the current amount in a jug and its capacity, respectively. The rule says that if a jug presently has some amount of water in it (possibly 0), and the current amount is less than its capacity (otherwise it is already full), then after filling, its current amount will be its capacity. Other actions such as emptying a jug completely, or pouring the water in one jug into the other jug until it is either empty or the other jug is full (leaving some remaining in the first jug) are left for the complete Appendix jug problem example. The bind operator basically tells the planner to look up and assign the current values to all fluent variables in a statement. Those values are then available for use in subsequent statements. If you need to lookup (ie, bind) the value of a variable in an action rule, then you must specify the relation with the appropriate fluent variable type. Also, there is no need to use fluent variables when specifying background knowledge with define-static-relations, since they don’t change during search.

Note that any statement which involves database lookup—eg, (bind (loc ?obj $x $y)) in a precondition, query function, if statement, etc—must not include in-line computations: eg, (bind (loc ?obj (1+ $x) $y)). In the former statement the location of an object is being looked up in the database and its retrieved location coordinates assigned to the variables $x and $y. The expression (1+ $x) is not a variable, and thus cannot receive an assignment. But a statement like (assert (loc ?obj (1+ $x) $y)) is fine, since the x coordinate is being computed before adding the proposition to the database.

Other local variables can be introduced in any Wouldwork function using the usual let form from Common Lisp. If they are special variables (accessible from subfunctions), then simply declare them special as discussed later. Note that local variables appearing in pure Common Lisp expressions are not required to have the prefix $, even though it is advisable. Such prefixed variables are definitely required in mixed Common Lisp & Wouldwork statements, and there is no penalty for always using $-prefixed variables. As a general rule, assign fluent variables using the $ prefix notation—for example, with a setq (setq $var 3) or bind (bind (blocks $blocks) or let (let ($counter 0) …) form.

</details>

<details>
<summary><strong>Global Variables</strong></summary>

On some occasions it may be useful to define one or more global variables at the beginning of a problem specification file. Global variables are often convenient, because they are visible from anywhere in the problem specification, and in all threads. Use the normal Lisp operator defparameter for single-threaded programs and for programs in which the value of the parameter does not change. Otherwise, to define a global variable which may change or to enable multi-threaded program runs, put in a declaration like the following:

```lisp
(sb-ext:defglobal var-name val-form doc-string)

```
The global var-name can then be safely updated by using the built in SBCL atomic parallel operations such as

```lisp
(sb-ext:atomic-incf var-name delta) for fixnums, or
(sb-ext:atomic-push object var-name), for lists
(sb-ext:atomic-pop var-name)

```
See the SBCL manual for further details.

</details>

<details>
<summary><strong>Dynamic (aka Special) Variables</strong></summary>

Along with global variables, Common Lisp has a built-in facility for handling dynamic variables. Typically, the variables appearing in a user’s problem specification will be local (ie, non-dynamic) to each action rule or function, and therefore have no meaning to Wouldwork outside of that rule or function. However, user variables declared as dynamic are not so restricted. Wouldwork can use dynamic variables to relay variable values between a parent function and any of its subfunctions. For example, if one function initializes a counter that several other functions subsequently increment depending on various conditions, the incremented values will be available to the initializing function for further processing. Here is a simple example:

```lisp
(define-query bag-item ($item)
 (let ()
 (declare (special $net-price))
 (incf $net-price
 (case $item
 (peas 1.80)
 (carrots 2.25)
 (mangos 3.00)))
 (do-some-other-stuff)))

(define-query purchase-price? ()
 (let (($net-price 0))
 (declare (special $net-price))
 (bag-item ‘peas)
 (bag-item ‘carrots)
 $net-price)) ;returns the total price

```
Declaring $net-price as dynamic (using the Common Lisp keyword special) allows its current value to be shared among the functions that use it. Any special variable must be so declared inside of a let or do statement.

</details>

<details>
<summary><strong>Parameter Lists</strong></summary>

Each action precondition and effect, as well as every quantified logical statement has a parameter list which tells the planner how to process the variables appearing in those statements. In the jugs example above, the fill action has a parameter list for the precondition, namely (?jug jug), and for the effect (?jug $cap). The presentation format in a precondition parameter list is always zero or more variables followed by their type. The presentation format in an effect parameter list is always a simple list of zero or more variables. An empty precondition parameter list means execute the body of the action only once, if the precondition is simply (always-true). The order of variable-type presentations in a precondition or quantified statement is arbitrary, but the order of variables in an effect determines the order of the variable instantiations printed out in the steps of a solution. The printout of a step like (fill jug1 5) means fill jug1 to capacity 5, because that is the user-specified order and meaning of variables in the effect parameter list above. Note that the variables appearing in an effect parameter list can come from either the precondition or the effect. Complex types expressed via the either construct are also allowed in precondition parameter lists—eg, ((?pet1 ?pet2) (either dog cat pig) ?owner owner).

As a general rule, every generated variable (?) must be parameterized before it is subsequently used in a logical statement. Thus, a precondition parameter list establishes a scope in which its variables can appear in an action precondition, effect, or quantified formula. Fluent variables ($) will never appear in a precondition or quantified formula parameter list, since their values are not generated. Their binding is always established through the use of the bind operator. However, fluent variables can appear in action effect parameter lists to control how solution steps are printed, as mentioned above.

In the case of overlapping types, as in a blocks world parameter list like (?block block ?support support), some supports are blocks (all supports except the table T) and all blocks are supports. For this situation, the default is to generate only relevant combinations of a block and a support—eg, (A B) or (A T), but not (T A) or (T T) or (A A)—for checking in an action rule. For example, in the default case, there is no need to include a statement like (different ?block ?support) in the body of a rule.

Whenever the precondition of an action is satisfied for a particular instantiation of its variables, those instantiations are then available for use in the action’s effect. In the jugs example, all of the precondition variable instantiations for ?jug, $amt, and $cap are automatically passed to the effect, and therefore do not need to appear in the effect parameter list, unless needed to appropriately print out solution steps.

Wouldwork has several available strategies for generating variables, depending on the requirements of the problem. The default strategy, previously summarized in Part 1: Specifying Possible Actions, effectively generates a list of all possible instantiations of each variable, but culls the list to include only unique instantiations. That is, no two variables will be instantiated with the same value. This comports with the common intuition that we give different names (in this case variable names) to different objects (as values). For example if the parameter list is (?block block (?block-support ?support) support), there are three generating variables—?block, ?block-support, and ?support—each of which can be instantiated with any of the instances from their respective types—block (A B C), block-support (A B C T), and support (A B C T). Therefore by default, instantiations for the three variables are generated from the full cross-product of (block X block-support X support)minus any duplication of values = ((A B C) (A B T) (A C B) (A C T) (A T B) (A T C) (B A C) (B A T) (B C A) (B C T) (B T A) (B T C) (C A B) (C A T) (C B A) (C B T) (C T A) (C T B)). The action rule or quantified formula is then checked for each such instantiation of the three (?) variables.

For some problems it may be possible to reduce the number of instantiations to be checked by considering only different combinations of the variable types. To generate combinations, insert combination as a keyword at the front of the parameter list like so: (combination ?block1 block ?block2 block). Now only different combinations of instances will be generated—namely, ((A B) (A C) (B C), greatly reducing the number of instantiations to be checked in a rule. Specifying combinations usually only makes sense if there are at least two variables of the same type.

Alternately, the full cross-product of all variables can always be specified with the keyword product at the front of the parameter list like so: (product ?digit1 digit ?digit2 digit). Now, all possible combinations of two digits, including when ?digit1 = ?digit2, will be generated and checked in the rule.

On other occasions, it is useful to simply generate dot-products from the variable types, such that one instance is selected progressively from each type. Progressive generation is often useful for associating items with attributes. Use the keyword dot-product to specify this generation strategy: (dot-product ?obj object ?pos position). Here, each object will be paired with a corresponding position—eg, ((OBJ1 (3 1)) (OBJ2 (1 1) (OBJ3 (2 3)). Note that to generate dot-products the number of objects must be the same as the number of positions, since they are paired up one-to-one.

Here is a summary of the four operators using the basic template for two items (standard (?block1 ?block2) block):

Finally, the user can specify that one or more variables are to be generated dynamically—ie, every time an action rule is invoked. For this case, specify a variable type as a query function call that returns the list of values for that variable. For example, instead of specifying a parameter list as (?field field ?word word), specify it as (?field (get-remaining-fields?) ?word (get-remaining-words?)). Then write define-query functions named get-remaining-fields? and get-remaining-words? which return the remaining fields and words for instantiation in the action rule. This may progressively reduce the number of instantiations for multiple invocations of the rule, but is ineffective if the rule is only executed once.

</details>

<details>
<summary><strong>Goals</strong></summary>

A planning goal is a logical statement that evaluates to true or false (ie, T or NIL) when applied to a state. Often it will simply consist of a conjunction of literals, all of which must be true to satisfy the goal. Alternately, it can be a complex logical statement or query function which expresses the conditions for recognizing when a situation is true. Every state explored by the planner is checked against the goal condition to see if it satisfies the goal. If it does, the planner stores that final state along with the path to that state as a solution. Depending on whether the user is looking for the first solution or additional solutions, planning will either exit or continue the search, respectively.

When solving an optimization problem that searches for a min-value or max-value state, a goal specification may be superfluous, since progressively better solutions are produced whenever a new state’s $objective-value is better than that of any older state. Leave out the goal specification to tell Wouldwork there is no goal in such a case. But if a goal is specified, min or max values will only be stored for goal states. Solutions and best states are recorded separately, and progressively. While solutions are recorded in *solutions*, best states are recorded in *best-states*.

</details>

<details>
<summary><strong>Global Constraint</strong></summary>

The user may optionally specify a global constraint (or multiple constraints AND-ed or OR-ed together). A global constraint places unconditional restrictions (serving as a kill switch) on planning actions. If a global constraint is ever violated in a state encountered during the planning process, it means that state cannot be on a path to a solution, and the planner must keep looking for some other path to the goal.

A constraint violation occurs when the constraint condition evaluates to NIL (false). In other words, define the constraint to evaluate to T (true), when the constraint is not violated. For example, to have an agent avoid any area of toxic gas, the user could include a constraint like:

```lisp
(define-constraint
 (not (exists (?gas gas ?area area)
 (and (loc me ?area)
 (atmosphere ?gas ?area)
 (toxic ?gas)))))

```
Constraints thus use the same format for expressions as goals. Since constraints are evaluated independently of context, any variables in the constraint must be bound (ie, no free variables). In general, it is more efficient to place constraints locally in the preconditions of individual action rules, since a global constraint is checked in every trial state generated by the planner. However, global constraints can avoid excessive redundancy, and are usually simpler to specify and debug than action preconditions.

</details>

<details>
<summary><strong>Query, Update, and Lisp Functions</strong></summary>

One very flexible kind of statement that can appear pretty much anywhere is a function call, used to query or update the current planning state. In the simplest cases, arbitrary built-in Common Lisp function calls may appear along with other Wouldwork logical statements. For example, an expression in a precondition like (< $height1 $height2) evaluates to T (true) if the previously bound value of $height1 is less than $height2. User-defined Common Lisp functions are specified with the usual defun expression. In standard programming style, function calls pass the current values of their arguments on to built-in Common Lisp or user-defined functions, which perform some computation before passing the result back to the calling statement. This result then is available for subsequent use in the action rule.

A Wouldwork query or update function provides a convenient way to encapsulate and hide the details of a complex analysis, thus making the problem specification more readable. And since functions are called from action rules or other functions at run time, they can incorporate recursive calls. As usual, function calls support an arbitrary number of arguments, the values of which are passed to the function in the same ordered sequence. Functions must be defined before they are used in a problem specification file.

Query function calls can be used to assign computed values to fluent arguments like $height1 and $height2. For example, the following blocks world query function takes a support as an argument, and computes the elevation of that support. Since blocks can be stacked, the elevation of a block is recursively computable from the height of that block plus the elevation of the block beneath it. If the expression (setf $elev (elevation? state $support)) appears in the precondition of an action, where $support is already instantiated, then the resulting value of $elev after evaluation will be the elevation of that support. The setf operator simply assigns the fluent variable to the value returned by the function elevation?. This value might then be used subsequently in an expression like (< $elev 10) to check whether the total elevation of that support is less than 10.

```lisp
(define-query elevation? ($support)
 (do (bind (height $support $h))
 (bind (on ?support $s))
 (if (not (support $s))
 $h
 (+ $h (elevation? $s)))))

```
The above function first gets the height ($h) of the support ($support). Next, if there is some other support, $s, which $support is on, then it returns $h plus the elevation of $s as the net elevation of $support. Otherwise, it just returns $h as the elevation of $support. The calculation of elevation thus recurses down a stack of blocks, adding in the height of each, until finally the table’s height is added. The bind statement is used to bind $s to the support that $support is on, if it is currently on a support, or to NIL (meaning the statement is false), if it is not on any support. The do operator simply collects the statements into a sequence. The do operator is required here, because the body of a Wouldwork function must consist of only one statement (in distinction from a Common Lisp function).

The syntax for a function definition must include a name for the function (eg, elevation?), an argument list of $ and ? variables which are passed into the function (eg, $support), and a body consisting of a single logical statement (possibly composed of multiple sub statements) that returns a value when the function is executed.

As alluded to, there are two basic kinds of user function specifications, depending on whether the function call appears in the precondition or the effect part of an action. The define-query specification (eg, elevation?) is for use in preconditions and conditional if statements, and returns a value that can be assigned to a fluent variable. The setf statement above illustrates this kind of use. However, a more common query use is simply for returning a boolean true or false value, which does not get explicitly assigned. A statement like

```lisp
(if (stable? ?support)
 (assert (on ?block ?support)))

```
uses the query function stable? to check if a ?support is stable before putting a block on it, where stable? is defined by the user with define-query.

Alternately, a define-update specification is for functions appearing in an effect, and can be used for analysis leading to changes to the current database. For example, an effect statement like

```lisp
(deactivate-receiver! ?receiver)

```
might consolidate a number of database conditions and updates associated with deactivation. (While query functions are optionally distinguished by the ? suffix, update functions are optionally distinguished by the ! suffix.) Update functions return user-specified values from their body expression just like query functions. The function body's final value becomes the return value, which can be captured using setq or mvsetq for single or multiple values, respectively. Wouldwork automatically collects the implicit changes, and returns them to the update function’s caller. It is up to the action rule to assert all changes. In the following update function, the changes appear in the then part of the if-then statements:

```lisp
(define-update deactivate-receiver! ($receiver)
 (if (active $receiver)
 (do (not (active $receiver)))
 (doall (?g gate)
 (if (controls $receiver ?g)
 (active ?g))))))

```
The do statement simply groups several statements together as the then part of the if-then statement. The propositions (not (active $receiver)) and (active ?g) are updates to the current state.

</details>

<details>
<summary><strong>Next & Finally</strong></summary>

All statements in the effect part of an action rule are executed in order in the context of the variable values established during precondition execution. The order of effect statements therefore normally does not matter, since the state changes resulting from those statements are effectively processed as a group. However, some occasions require a follow-up sequence of effect actions, for which the operators next and finally may be appropriate. A common use is for when the initial actions of a rule can automatically trigger additional follow-up actions. In the blocks world, an example might be triggering the collapse of a stack of blocks after a block has been placed on a stack exceeding a certain height. In this case, the last effect statement could look like (finally (assess-stack-stability! ?support)), whose execution is delayed until the preceding effect statements are completed.

Any number of follow-up actions are allowed, each of which will be executed in strict sequence. The convention for including multiple follow-up actions is to label the initial statements with next, reserving finally for the last statement, although next and finally are internally processed as synonyms. Also, next and finally can only take one argument, which must be an update function call, not a generic logic statement.

</details>

<details>
<summary><strong>Assert Statements</strong></summary>

Propositions are added to (or retracted from) the current state using the assert operator. For example, (assert (on ?block ?support)) will, depending on the current values of ?block and ?support, add a proposition like (on block1 block2). Alternately, a statement like (assert (not (on ?block ?support))) will, depending again on current values, delete a proposition like (on block1 block2) from the current state.

An assert statement establishes a scope for other statements that update the current state. And often there are a number of statements that get asserted by an assert statement in the order specified by the user. Each assert statement generates its own update to the current state, which is then stored by Wouldwork for future follow-on processing. This means that the effect of an action rule can contain several assert statements, each with its own scope, and each of which will generate a follow-on state. Thus multiple follow-on states can be conveniently generated by different effects as well as different preconditions in action rules. See problem-triangle-xyz-one.lisp for an example of multiple effects all condensed into one large action rule (as opposed to many smaller rules). If the problem is one of optimization using either min-value or max-value, make sure the variable $objective-value is calculated in each assert statement.

</details>

<details>
<summary><strong>Exogenous Events</strong></summary>

Exogenous events are events that happen in the planning environment independent of the planning agent’s actions. Typically, the agent must react to or otherwise take into account such happenings along the way to achieving the goal. The user specifies exogenous events before planning begins in a program schedule, which becomes part of the problem specification. As these events are prespecified by the user, they are technically known as timed initial literals. The planner uses the schedule to automatically update the state of the environment at the appropriate time. For example, below is a schedule for an automated sentry, named sentry1, which is continuously patrolling three adjacent areas in sequence.

```lisp
(define-happening sentry1
 :events
 ((1 (not (loc sentry1 area6))
 (loc sentry1 area7))
 (2 (not (loc sentry1 area7))
 (loc sentry1 area6))
 (3 (not (loc sentry1 area6))
 (loc sentry1 area5))
 (4 (not (loc sentry1 area5))
 (loc sentry1 area6)))
 :repeat t)

```
Starting in area6 at time = 0 (which is specified separately in the initial state), the planner updates the sentry’s location to area7 at time 1. Subsequently, the sentry’s location is updated at each time index, to area6 (at time = 2), area5 (at time = 3), back to area6 (at time = 4), area7 (at time = 5), area6 (at time = 6), etc. The keyword :repeat (where t means true) indicates that the sequence is repeated indefinitely. If the keyword :repeat is not included (or is NIL), the exogenous events end with the last event listed. The happenings at each indicated time are simply a list of all state changes occurring at that time.

In this automated sentry problem example (presented fully in the Appendix as the Sentry Problem) the agent must avoid contact with the sentry, but can pass through a patrolled area as long as the sentry is in a different area at the time. Exogenous events often place global constraints on the planning agent, which can be added to the problem specification:

```lisp
(define-constraint ;avoid kill situation
 (not (exists (?s sentry ?a area)
 (and (loc me ?a)
 (loc ?s ?a)
 (not (disabled? ?s)))))

```
This constraint determines whether there is a sentry and an area, such that the agent ( labeled me) and the sentry are both located in that area, and the sentry is not disabled (a kill situation). If the constraint is ever not satisfied during planning (ie, evaluates to NIL in any state), then the constraint is violated, and the planner must backtrack and explore a different path.

There is also a means to temporarily interrupt scheduled happenings, and to dynamically change the sequence of happening events based on certain conditions. The previous constraint shows that whenever a sentry is disabled (eg, by jamming, destruction, or other deactivation), then the constraint is satisfied, and it will be safe to enter the sentry’s current area. But becoming disabled means the sentry’s program schedule is temporarily or permanently suspended during planning. The user can optionally indicate when such an interruption occurs by specifying interrupt conditions, signaled by the keyword :interrupt. Adding this specification to the happenings then yields the full definition:

```lisp
(define-happening sentry1
 :events ((1 (not (loc sentry1 area6))
 (loc sentry1 area7))
 (2 (not (loc sentry1 area7))
 (loc sentry1 area6))
 (3 (not (loc sentry1 area6))
 (loc sentry1 area5))
 (4 (not (loc sentry1 area5))
 (loc sentry1 area6)))
 :repeat t
 :interrupt (exists (?j jammer)
 (jamming ?j sentry1)))

```
Note that if exogenous events are included in a problem specification, then a graph search strategy cannot be used. This is because states cannot be permanently closed. Entering a previously dead state, may no longer be dead, if it is reached at a different time. Use tree search instead.

</details>

<details>
<summary><strong>Exogenous Patrolling Objects</strong></summary>

Wouldwork has a specialization for a common kind of object, called a patroller, programmed to follow a set path in the environment. A patroller uses exogenous events to move from one area to the next, similar to an object defined with define-happening, but allows some optional specifications unique to a patrolling object. A patroller is specified with define-patroller (instead of the more general define-happening). For example, problem-mine1.lisp is a specification patterned after problem-sentry.lisp, but with implicit events, which includes a patrolling mine traversing a repeating path.

```lisp
(define-patroller mine1
 path (area5 area6 area7)
 mode :reverse
 rebound (exists (?c cargo)
 (and (bind (loc mine1 $area))
 (loc ?c $area)))
 interrupt (exists (?j jammer)
 (jamming ?j mine1))
 kill (and (not (exists (?j jammer)
 (jamming ?j mine1)))
 (bind (loc mine1 $area))
 (loc agent1 $area)))

```
Here, path is the sequence of areas that the mine moves through; the mode indicates that the direction should be reversed upon reaching area7 (or area1) with the other option being :cycle (ie, start over when reaching area7); rebound indicates the mine should rebound off of an obstacle in its path, reversing direction; interrupt specifies the condition in which the motion can be arrested; and kill indicates the condition in which this state will be pruned from the search tree.

</details>

<details>
<summary><strong>Waiting</strong></summary>

When there are exogenous events happening in the planning environment, it may sometimes become expedient for an agent to simply wait for the situation to change. For example, in the patrolling sentry problem, the planning agent can wait for a time interval before moving to a patrolled area, to allow the automated sentry to move away from the area. In Wouldwork, waiting is handled automatically by enabling the auto-wait mechanism in the problem specification.

```lisp
(ww-set *tree-or-graph* tree)

(ww-set *auto-wait* t)

```
When auto-wait is enabled, the planner automatically inserts wait actions when needed, without requiring an explicit wait action rule in the problem specification. The wait duration is computed as the exact time until the next relevant exogenous event occurs, rather than iterating one time unit at a time. Note, however, that enabling auto-wait typically increases the size of the search tree, so it us usually best to try to solve a problem without waiting first.

Auto-wait operates through two complementary mechanisms. First, when the agent reaches a state where no actions are applicable (the agent is "stuck"), the planner simulates happenings forward in time until either an action becomes applicable or the goal is reached. Second, when all regular actions from a state have been explored and lead to dead ends, the planner attempts waiting as a "second chance" before the search retreats to explore alternative paths. This enables discovering solutions that require deliberate pauses for synchronization with exogenous events.

Auto-wait requires tree search mode and runs only in serial processing with the depth-first algorithm. This is because states cannot be permanently closed. A state that was a dead end earlier might now have viable successors because an exogenous event occurred. The planner validates these requirements when enabling auto-wait and reports error messages if any are not satisfied. If your problem requires graph search or parallel processing, auto-wait cannot be used.

</details>

<details>
<summary><strong>Initialization Actions</strong></summary>

As already discussed in Part 1, the database for the starting state is initialized via a straightforward listing of true propositions appearing in a define-init specification. Similarly, an initialization action, as defined in a define-init-action specification, is an action which is taken only once, also at initialization. An initialization action is useful for adding numerous default propositions to the starting state, in lieu of listing each one individually in a define-init specification. For example, the following rule specifies that all sentries are active at initialization:

```lisp
(define-init-action activate-sentries
 0
 (?sentry sentry)
 (always-true)
 ()
 (assert (active ?sentry)))

```
The duration in an initialization action is always set to 0 (actually, any other value is ignored), but otherwise it looks and functions like a normal action. Also, the effect parameter list in an initialization action is usually set to (), since Wouldwork does not printout initialization actions as steps in the planning process.

On rare occasions it also useful to execute a normal action rule one or more times, and then delete it. The action then will no longer take part in generating new states. This allows a form of conditional pre-processing—eg, to generate several possible initial states from the *start-state*—which the other actions can subsequently process. For example, assume there is such an action rule, named add-first-peg, that places a peg on a peg board at each possible initial position. (See problem-triangle-backwards.lisp, which solves a triangle peg puzzle backwards.) But after this initial move, the rule is superfluous, since another action takes over placing pegs adjacent to those already on the board. Adding an action rule to delete the now superfluous action will speed up the search:

```lisp
(define-action delete-add-first-peg
 0
 ()
 (always-true)
 ()
 (delete-actions 'add-first-peg 'delete-add-first-peg))

```
This rule will only run once, since it deletes itself as well as the target add-first-peg action. The function named delete-actions is reserved and pre-defined in Wouldwork to be available for this purpose. Its arguments are the names of action rules to be deleted.

</details>

<details>
<summary><strong>Multiple Action Effects</strong></summary>

A typical action rule has one precondition and one effect. That is, if the precondition is satisfied for some instantiation of its variables, the effect assertions are executed with that instantiation, giving rise to the next state. But it may be convenient, and more efficient, on some occasions to specify several possible effects for each instantiation in one action rule. Then if a precondition is satisfied, all of the intended effects will be independently executed with the precondition instantiation. Each of the multiple effects will thus produce a follow-on state.

Multiple effects are useful for consolidating several action rules into one action, when those several actions have the same precondition. The consolidated action specification is more complex, since it now specifies the previous effects as alternates in the consolidated action. But then the separate actions are redundant, and can be removed. Processing efficiency is also enhanced, since the precondition only needs to be checked once to allow all possible alternate effects.

The simplest way to specify the alternates is to list them with a do statement in the consolidated effect like so:

```lisp
(do (if …
 (assert …))
 (if …
 (assert …))
 …)

```
where each set of assertions specifies a state update (if the condition is satisfied). In other words, all update statements within the scope of an assert are processed together, separately from other sets of assertions. But only use assert in action rules, not in update functions called from action rules. An example of a multi-effect action rule is provided in the problem specification file named problem-triangle-heuristic.lisp.

</details>

<details>
<summary><strong>The Variety of Logical Statements</strong></summary>

This section outlines the kinds of logical statement that may appear in actions, goals, initialization actions, functions, exogenous interrupt conditions, and constraints. All planning analysis is done in terms of logic statements, each of which is ultimately translated into the implementation language Common Lisp. Logic statements typically can be nested within other logic statements to an arbitrary extent, although there are some limitations.

```lisp
(loc ?sentry ?area)
```
This is probably the most common form of statement, containing locally generated (?) variables. It deals with the location of a sentry. This basic statement will become instantiated with particular values for ?sentry and ?area during execution (eg, sentry3 and area2) forming a proposition. In a precondition the statement tests whether or not a proposition is present in a state database (returning T or NIL). If true, processing of subsequent precondition statements continues (typically because all the precondition statements are AND-ed together into a conjunction), but if false, the entire conjunction fails. In an effect, such an instantiated statement is instead interpreted as a proposition in the current state database.

```lisp
(assert (loc ?sentry ?area)), or
(assert (not (loc ?sentry ?area))
 (loc me ?area))
```
Assertions are used in action effects to add or retract propositions from the current database. If ?sentry and ?area have been previously instantiated with sentry3 and area2, respectively, then the proposition (loc sentry2 area2) will be either added to or removed from the database. All unconditional statements within the scope of an assert will be asserted.

```lisp
(loc sentry1 ?area)
```
A partially instantiated statement. The operation of partially instantiated statements is as described above. If a statement is fully instantiated, as in (loc sentry1 area3), it is already a proposition, and can be checked in the database directly.

```lisp
(not (loc ?sentry ?area))
```
The negation of a statement in a precondition tests whether an instantiation is explicitly not in the database. In an action effect it means delete the proposition, if present. In the latter case there is no change if the proposition is not present.

```lisp
(loc ?sentry $area)
```
A statement containing fluents (eg, $area), which can be matched with the current variable bindings is in the database. If not found in the database, the statement is false, otherwise true. Note that fluents should only appear in functional relations (ie, relations exhibiting only one instantiation in a database at a time). Location is one such relation, since an object normally cannot be located in more than one place at a time.

```lisp
(bind (loc ?sentry $area1))
(not (bind (loc ?sentry $area1)))
```
The bind operator on a fluent statement binds its fluent variables by looking up their values in the current database. The bindings are then available for subsequent use. If a corresponding proposition is not found in the database, the proposition returns false, otherwise true. It is normally used in a precondition or effect if statement.

```lisp
(assert (if (active ?sentry)
 (dangerous ?sentry)
 (benign ?sentry)))
```
A conditional if statement performs a test and depending on the true/false result selects one or more statements to assert. Its full format consists of three clauses (if <test> <then> <else>), although the <else> clause is optional. In an effect the test is performed on the current successor state, which is being updated. The judicious use of if statements in effects can substantially reduce the total number of action rules required to cover a problem domain.

```lisp
(cleartop? ?block)
```
A statement that calls a query function is indicated by an optional postfix question mark (?) on a predicate or other computation. The sequence of argument variables must correspond to the order of those in the query function’s definition. The statement’s value is the value returned by the function.

```lisp
(ww-loop for ($x $y) in $positions do …)
```
An iterative statement like loop in Common Lisp. Allows binding of local variables and respects all the normal loop keywords and constructs like do, collect, sum, etc. Using ww-loop as opposed to loop allows Wouldwork logical statements to be embedded within the ww-loop statement. Note that, in general, Wouldwork logical statements cannot be embedded in Common Lisp statements.

```lisp
(setf $elevation (elevation? ?support))
```
In this case the query function elevation? takes one argument, ?support, computes the elevation of the support, and returns a value which is stored in the variable $elevation. The setf operator performs fluent variable assignment. Subsequent uses of the variable $elevation will refer to this value.

```lisp
(count #.(1+ *n*))
```
This is an example of a statement with a reader macro, indicated by #. followed by an expression to evaluate. It offers a flexible way to evaluate any Lisp expression immediately upon loading, the results of which are inserted directly into the statement. For example, if the value of *n* has previously been set to 100, then the above statement translates to (count 101). See the define-init section of the problem-knap19.lisp file for an example that initializes the capacity of a knapsack. Note that the variable *n* has to be evaluated earlier with a statement like #.(defparameter *n* 100), so that the value of *n* is available.

```lisp
(climbable> ?ladder ?area1 ?area2)
```
When a statement contains two or more generated variables of the same type (viz, ?area1 and ?area2 are both types of area), the planner assumes by default that the predicate and its arguments express a symmetric relation. A symmetric interpretation of this statement means that you can climb from ?area1 to ?area2 using the ?ladder, and from ?area2 to ?area1 using the same ?ladder. (Here, the ladder goes over a wall separating the two areas.) But this default interpretation is probably incorrect in this case. To specify that climbable is not symmetric, attach the direction marker (>) to the predicate, climbable>. Now the agent can use the ladder to go only from ?area1 to ?area2.

```lisp
(exists (?sentry sentry)
 (and (bind (loc ?sentry $area))
 (active ?sentry)))
```
An existential statement operates much like a scaled down action rule, but stops executing after it finds the first instantiation satisfying its conditional part (ie, the and statement above). The parameter list can have one or more generator variables, and the condition is tested for each instantiation set. If any instantiation of the condition is true, the entire existential statement is true, otherwise it is false.

```lisp
(forall ((?sentry1 ?sentry2))
 (if (and (bind (loc ?sentry1 $area1))
 (bind (loc ?sentry2 $area2))
 (eql $area1 $area2))
 (assert (conflict ?sentry1 ?sentry2))))
```
A universal statement is the quantified counterpart to an existential statement, and stops executing as soon as any instantiation does not satisfy the conditional part. It returns true only if all instantiations of its conditional part are true.

```lisp
(doall (?g gate ?s switch)
 (if (and (controls ?s ?g)
 (off ?s))
 (assert (inactive ?g))
 (assert (active ?g))))
```
A doall statement executes its body for all instantiations of its parameter list. It simply returns true when finished. Note that it is unusual to use forall in the effect of an action rule to make assertions, because the assertions will only be made if the forall returns true. Use doall instead.

```lisp
(do (activate-transmitter! ?transmitter)
 (activate-receiver! ?receiver))
```
The do operator simply combines two or more statements into a single statement. This statement is common in functions, where the body must be expressed as a single statement.

```lisp
(different ?block ?support)
```
The keyword different is a built-in predicate for testing whether two variable instantiations are distinct. Since blocks are also supports, the instantiations for each could be the same—eg, ?block = block1, ?support = block1. In cases like this, it may be useful to test for distinct instantiations using different. Here, different is synonymous with (not (eql ?block ?support)).

```lisp
(setq $objective-value (+ $current-value $additional-value))
```
A statement like this informs the planner what the net value of a state is when the solution-type is either min-value or max-value. Wouldwork will then search for a state minimizing or maximizing that value. Put this statement inside an assert statement, so it becomes part of the next updated state.

```lisp
(delete-actions 'add-first-peg 'delete-add-first-peg))
```
This statement will delete the specified action rules from further consideration, once the statement is executed. The search then continues without those rules.

```lisp
(always-true)
```
This statement, as it indicates, is always true. It can be used to satisfy a precondition for any and all action rule parameters, which are then passed to the effect for producing assertions.

```lisp
(print ?area), (< $height1 $height2), etc.
```
Any valid Common Lisp function or special form can also appear as the relation in a statement. However, the operator’s arguments are limited to other Common Lisp expressions, or previously defined planning parameters.

</details>

<details>
<summary><strong>Plan Monitoring & General Debugging</strong></summary>

Even the best laid plans can go awry for unexpected reasons. And it can be difficult to write a logically valid problem specification free from error. Accordingly, Wouldwork offers several options for tracking down mistakes in logic or programming, or just monitoring the planning process.

The global variable named *debug* (in the :ww package) controls the level of information output to the terminal during planning, and for nonparallel search can take a value of 0, 1, 2, 3, 4, or 5 (initially 0 for no debugging output). Level 1 simply outputs the complete search tree of actions attempted, which may be useful for determining where a plan goes wrong. Level 2 outputs level 1 plus the state after each planning step is taken, which may help determine why a particular action failed. Level 3 outputs information about the sequence of steps. Level 4 outputs detailed information about each step. Level 5 outputs the same as Level 4, but temporarily halts program execution after each step, allowing the user to carefully examine each possible action before continuing to the next step. For parallel search there are only two valid values of *debug*, namely 0 (no output) and 1 (basic thread output). Debugging, naturally, slows down the search process as it gathers information about the individual planning states.

Debugging output is displayed in the SBCL REPL window, along with normal output. Since the output can be lengthy, you can capture it to a file for subsequent analysis in a text editor by executing
(dribble <filename>)—eg, (dribble "D:/temp.txt")—before executing (run problem-name). After the program completes, execute (dribble) to close the file.

To assist with debugging individual actions, constraints, goals, functions, etc, you can also insert arbitrary Common Lisp code among logic expressions—for example to print variable bindings as they are assigned during execution. The following action from the blocks world problem will print the bindings for two variables of interest using the utility (ut::prt <variable names>) during rule execution:

```lisp
(define-action put
 1
 (?block block ?support support)
 (and (cleartop? ?block)
 (cleartop? ?support)
 (ut::prt ?block ?support))
 (?block block ?support support)
 (assert (on ?block ?support)
 (if (bind (on ?block $s))
 (not (on ?block $s)))))

```
Note that a ut::prt statement always returns the value of its last argument, just like print in Common Lisp.

Here is a list of some other potentially useful SBCL commands to inspect the Wouldwork data structures:

```lisp
 (ut::show (first *actions*)) ;show first rule code

 (ut::show *types*)

 (ut::show *relations*)

 (ut::show *db*) ;the initial dynamic database

```
As a final resort, there is also a facility for interrupting the search at a particular step (ie, action) in the program. This allows checking whether that step is being performed correctly. For example, specifying (ww-set *probe* (move (area4 area5) 12)), will interrupt the search just after the action of moving from area4 to area5, where the current depth is 12. This step (ie, state) can then be analyzed for errors. To tell Wouldwork to interrupt after the third such move action encountered, specify (ww-set *probe* (move (area4 area5) 12 3).

</details>

<details>
<summary><strong>Solution Validation</strong></summary>

The Wouldwork Planner includes a solution validation facility that allows users to verify and troubleshoot action sequences. This is particularly useful when a user has obtained or developed an action sequence and wishes to confirm that the sequence is valid according to the problem specification. It also serves as a debugging aid when developing or refining problem specifications, allowing step-by-step verification that a sequence of actions produce the expected state transitions.

The main interface is the validate-solution macro. Given a sequence of actions, the validator executes each action in order starting from *start-state*, checking that preconditions are satisfied and applying effects to produce successive states. The macro accepts action forms using the same syntax as solution output—the action name followed by its effect arguments:

```lisp
(validate-solution
 (move robot area1 area2)
 (pickup robot connector1)
 (connect robot transmitter1 receiver3))

```
If any action fails, the validator reports the action name, the failed action form, the reason for failure, and the state immediately prior to the failure point. For more detailed diagnostic output during validation, include the :verbose keyword before the action list.

</details>

<details>
<summary><strong>Debugging with an Action Trace</strong></summary>

Wouldwork provides an action tracing facility to help debug and understand action specifications during development. This tool intercepts an action execution during normal search and displays exactly what that action does for inspection.

Basic Usage:
(ww-set *debug* 0.5) ; Enable minimal debugging (required)
(trace-action action-name) ; Trace the specified action

The tracer runs a normal search on the current problem, and pauses each time it encounters a unique instantiation of the specified action with a satisfied precondition. For each instantiation, it displays:
The action name with current variable bindings
The complete current state (all propositions)
The resultant state delta showing additions and deletions

After examining the result of the action to make sure it is correct, you can continue on to the next instantiation, if useful.

</details>

<details>
<summary><strong>Debugging with Runtime Invariants</strong></summary>

An invariant in programming is a statement that, when executed, will check that some condition is true, and signal an error if it is not. Including invariants in a program can often detect subtle mistakes in programming that logical or otherwise careful analysis would overlook. For example, if the program involves initializing a lot of data, a typo that inputs 7 instead of 6 may allow the program to run fine to completion, but perform a faulty analysis.

In Wouldwork, there are two ways to use invariants. In the first (action invariants), an invariant can check for its condition in every state generated by a particular action rule during planning. An action invariant is expressed in Wouldwork as an update function (included as a define-update specification in the problem-*.lisp file), and called in a next or finally clause in an action rule. As an example, consider the check-emptys invariant function in the problem-tiles7a-heuristic.lisp file:

```lisp
(define-update check-emptys ()
 (do (bind (emptys $emptys)) ;get the value of $emptys
 (unless (alexandria:setp $emptys :test #'equal)
 (troubleshoot "$emptys is not setp: ~A" $emptys))
 (unless (alexandria:sequence-of-length-p $emptys 9)
 (troubleshoot "$emptys length is not 9: ~A"
 (length $emptys)))))

```
Here, the variable $emptys is always expected to be a list with no duplicates (checked with alexandria:setp) and to have a length of 9 (checked with alexandria:sequence-of-length-p). If either invariant is ever violated, a troubleshooting environment is entered, to help the user determine what led to the error in that action rule. Note that the arguments to troubleshoot will be displayed to explain which error was encountered.

Subsequent to entering the troubleshooting environment, if the user selects option 0, Wouldwork displays the state information for the problematic action, along with the followon states. Seeing the exact error should provide a clue to what led to the specific error. Once all such errors have been eliminated, the next or finally clauses in the action can be removed to allow more efficient processing.

The second use of invariants (global invariants) is for checking an invariant against every stage generated by Wouldwork. The purpose of including a global invariant is to make sure no inconsistent states are ever generated. If Wouldwork ever detects a global invariant violation, the troubleshooting environment is entered, providing information about where the inconsistency was detected. A global invariant is expressed in Wouldwork with a define-invariant specification in the problem-*.lisp file. For example,

```lisp
(define-invariant holds-cargo-location ()
 ;Cargo cannot be both held and have a location simultaneously
 (not (and (bind (holds me1 $cargo))
 (bind (loc $cargo $area)))))

```
In this invariant Wouldwork looks up what the agent is currently holding, if any, and checks to see if it is also located in some area. If both are true, it is a domain inconsistency indicating the invariant does not hold for the current state. A troubleshooting option is then invoked on that state. Again, once all such errors have been eliminated, the invariant can be removed or commented out to allow more efficient processing.

</details>

<details>
<summary><strong>Some Potentially Useful User Commands</strong></summary>

```lisp
sbcl --dynamic-space-size 24000
```
Opens SBCL with more than the default 1K of memory.

```lisp
(run-test-problems) or (test)
```
Runs through a varied collection of problems and their solutions, useful for debugging any changes to Wouldwork itself.

```lisp
(list-all-problems)
```
Displays the names of all problem specifications in the src directory.

```lisp
(ut::show (first *actions*))
```
Prints the lisp code for one of the Wouldwork action rules (for debugging).

```lisp
(pprint (get '*goal* 'fn))
```
Prints the lisp code for a Wouldwork function (for debugging).

```lisp
(ut::show *types*)
```
Prints all current type structures as defined in problem.lisp.

```lisp
(ut::show *relations*)
```
Prints all current dynamic relation structures as defined in problem.lisp

```lisp
(ut::show *static-relations*)
```
Prints all current static relations.

```lisp
(ut::show *db*)
```
Prints the initial dynamic propositional database before solving begins.

```lisp
(ut::show *static-db*)
```
Prints the initial static propositional database.

```lisp
(ut::prt s-expr s-expr …)
```
Prints the values of any lisp variables or s-expressions.

```lisp
(profile)
```
Profiles the solution of a problem, to determine which functions are taking the most time. If the search is interrupted midway (eg, by entering CTRL-C) it will be necessary to execute (sb-profile::report) to retrieve the results.

```lisp
(reset)
```
Removes all previous problem solving history. Potentially useful if Wouldwork should enter an inconsistent state that keeps generating errors.

</details>

<details>
<summary><strong>Some Useful User Functions to Use in Rules</strong></summary>

There are also a few pre-defined Common Lisp functions that the user can include in rules and functions. These functions are simply provided as a convenience so the user doesn’t have to write them, if needed. For example, if you want to use a hash table as a representation for a set of elements, there are functional set operations provided such as union, difference, copy, etc. Others may be added later.

```lisp
(different sym1 sym2)
 "Determines whether two symbols represent different objects
 rather than the same object."

(delete-actions &rest names)
 "Deletes named actions from *actions* at run-time."

(get-state-codes)
 "User calls this after finding backwards *solutions* to a
 Problem in order to generate forward paths to those
 solutions."

(backward-path-exists state)
 "Use in a forward search goal to check for the existence of a
 pre-computed backward path."

(vectorize lists)
 "Turns a list of lists into vector of vectors."

(make-ht-set &rest args
 &key (initial-contents nil initial-contents-p)
 &allow-other-keys)
 "Makes a wouldwork hash-table that works as a set container
 for the user."

(copy-ht-set set-ht)
 "Copy a set hash table (with t values)."

(union-ht-set &rest set-hts)
 "Unions hash tables keys. Assumes values are all t and have
 the same :test function."

(set-difference-ht-set ht1 ht2)
 "Returns a new hash table that represents the set difference
 of ht1 and ht2."
```

</details>

## Extracted tables (for completeness)

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
